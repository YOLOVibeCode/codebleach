using System.Text.Json;
using System.Text.RegularExpressions;
using CodeBleach.Core.Models;
using Jint;

namespace CodeBleach.Core.Services;

/// <summary>
/// Loads custom rules from configuration files.
/// Supports three rule types: regex (inline), regexFile, and javascript.
/// </summary>
public static class CustomRuleLoader
{
    /// <summary>
    /// Loads custom rules from a configuration file.
    /// </summary>
    public static IEnumerable<SanitizationRule> LoadFromFile(string configPath)
    {
        if (!File.Exists(configPath))
        {
            return [];
        }
        
        try
        {
            var json = File.ReadAllText(configPath);
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true
            };
            var config = JsonSerializer.Deserialize<CustomRuleConfig>(json, options);
            
            if (config?.Rules == null)
            {
                return [];
            }
            
            var configDir = Path.GetDirectoryName(configPath) ?? ".";
            var rules = new List<SanitizationRule>();
            
            foreach (var r in config.Rules.Where(r => r.Enabled))
            {
                var pattern = ResolvePattern(r, configDir);
                if (string.IsNullOrEmpty(pattern))
                {
                    continue;
                }
                
                rules.Add(new SanitizationRule
                {
                    RuleId = r.RuleId,
                    Name = r.Name,
                    Description = r.Description ?? $"Custom rule: {r.Name}",
                    Pattern = pattern,
                    Prefix = r.Prefix,
                    Severity = r.Severity,
                    Enabled = r.Enabled,
                    Exceptions = r.Exceptions,
                    Order = r.Order
                });
            }
            
            return rules;
        }
        catch
        {
            // Return empty if file is invalid
            return [];
        }
    }
    
    /// <summary>
    /// Resolves the pattern based on rule type.
    /// </summary>
    private static string? ResolvePattern(CustomRuleDefinition rule, string configDir)
    {
        return rule.Type switch
        {
            RuleType.Regex => rule.Pattern,
            RuleType.RegexFile => LoadRegexFromFile(rule.PatternFile, configDir),
            RuleType.JavaScript => LoadPatternFromJavaScript(rule.ScriptFile, configDir),
            _ => rule.Pattern
        };
    }
    
    /// <summary>
    /// Loads a regex pattern from an external .regex file.
    /// Supports comments (lines starting with #) and blank lines.
    /// </summary>
    private static string? LoadRegexFromFile(string? patternFile, string configDir)
    {
        if (string.IsNullOrEmpty(patternFile))
        {
            return null;
        }
        
        var fullPath = Path.IsPathRooted(patternFile) 
            ? patternFile 
            : Path.Combine(configDir, patternFile);
            
        if (!File.Exists(fullPath))
        {
            return null;
        }
        
        try
        {
            var lines = File.ReadAllLines(fullPath);
            var patterns = lines
                .Select(line => line.Trim())
                .Where(line => !string.IsNullOrEmpty(line) && !line.StartsWith('#'))
                .ToList();
            
            if (patterns.Count == 0)
            {
                return null;
            }
            
            // If multiple patterns, combine with OR
            return patterns.Count == 1 
                ? patterns[0] 
                : $"({string.Join("|", patterns)})";
        }
        catch
        {
            return null;
        }
    }
    
    /// <summary>
    /// Loads and executes a JavaScript file that returns a regex pattern.
    /// The JS file should define a getPattern() function that returns a regex string.
    /// </summary>
    private static string? LoadPatternFromJavaScript(string? scriptFile, string configDir)
    {
        if (string.IsNullOrEmpty(scriptFile))
        {
            return null;
        }
        
        var fullPath = Path.IsPathRooted(scriptFile) 
            ? scriptFile 
            : Path.Combine(configDir, scriptFile);
            
        if (!File.Exists(fullPath))
        {
            return null;
        }
        
        try
        {
            var script = File.ReadAllText(fullPath);
            
            // Create sandboxed Jint engine
            var engine = new Engine(options =>
            {
                options.LimitRecursion(50);
                options.MaxStatements(1000);
            });
            
            // Execute the script
            engine.Execute(script);
            
            // Call getPattern() function
            var result = engine.Invoke("getPattern");
            
            return result.AsString();
        }
        catch
        {
            return null;
        }
    }
    
    /// <summary>
    /// Executes a JavaScript match function against content.
    /// For advanced use - returns matches from a JS function.
    /// </summary>
    public static IEnumerable<string> ExecuteJavaScriptMatcher(
        string scriptPath, 
        string content,
        string configDir)
    {
        var fullPath = Path.IsPathRooted(scriptPath) 
            ? scriptPath 
            : Path.Combine(configDir, scriptPath);
            
        if (!File.Exists(fullPath))
        {
            return [];
        }
        
        try
        {
            var script = File.ReadAllText(fullPath);
            
            var engine = new Engine(options =>
            {
                options.LimitRecursion(50);
                options.MaxStatements(10000);
            });
            
            engine.Execute(script);
            
            // Call match(content) function
            var result = engine.Invoke("match", content);
            
            if (result.IsNull() || result.IsUndefined())
            {
                return [];
            }
            
            // Convert JS array to C# list
            if (result.IsArray())
            {
                var array = result.AsArray();
                var matches = new List<string>();
                foreach (var item in array)
                {
                    if (item.IsString())
                    {
                        matches.Add(item.AsString());
                    }
                }
                return matches;
            }
            
            return [];
        }
        catch
        {
            return [];
        }
    }
    
    /// <summary>
    /// Searches for custom rule files in the directory hierarchy.
    /// </summary>
    public static string? FindConfigFile(string startPath)
    {
        var current = new DirectoryInfo(startPath);
        
        while (current != null)
        {
            var configPath = Path.Combine(current.FullName, CustomRuleConfig.DefaultFileName);
            if (File.Exists(configPath))
            {
                return configPath;
            }
            
            current = current.Parent;
        }
        
        return null;
    }
}

