using System.Text.Json;
using CodeBleach.Core.Models;

namespace CodeBleach.Core.Services;

/// <summary>
/// Loads custom rules from configuration files.
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
            
            return config.Rules
                .Where(r => r.Enabled)
                .Select(r => new SanitizationRule
                {
                    RuleId = r.RuleId,
                    Name = r.Name,
                    Description = r.Description ?? $"Custom rule: {r.Name}",
                    Pattern = r.Pattern,
                    Prefix = r.Prefix,
                    Severity = r.Severity,
                    Enabled = r.Enabled,
                    Exceptions = r.Exceptions,
                    Order = r.Order
                });
        }
        catch
        {
            // Return empty if file is invalid
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

