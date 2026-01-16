using System.Text;
using System.Text.RegularExpressions;
using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using Microsoft.Extensions.Logging;

namespace CodeBleach.Core.Services;

/// <summary>
/// Sanitizes content by replacing sensitive values with aliases.
/// </summary>
public sealed class Sanitizer : ISanitizer
{
    private readonly IRuleRegistry _ruleRegistry;
    private readonly ILogger<Sanitizer>? _logger;
    
    public Sanitizer(IRuleRegistry ruleRegistry, ILogger<Sanitizer>? logger = null)
    {
        _ruleRegistry = ruleRegistry;
        _logger = logger;
    }
    
    public SanitizationResult Sanitize(string content, MappingTable mappings)
    {
        if (string.IsNullOrEmpty(content))
        {
            return new SanitizationResult
            {
                Content = content,
                WasSanitized = false,
                Matches = []
            };
        }
        
        var matches = new List<SanitizationMatch>();
        var activeRules = _ruleRegistry.GetActiveRules();
        var contentBuilder = new StringBuilder(content);
        
        // Process each rule
        foreach (var rule in activeRules)
        {
            var regex = new Regex(rule.Pattern, RegexOptions.Compiled);
            var ruleMatches = regex.Matches(content);
            
            foreach (Match match in ruleMatches)
            {
                var originalValue = match.Value;
                
                // Check exceptions
                if (rule.Exceptions.Contains(originalValue, StringComparer.OrdinalIgnoreCase))
                {
                    continue;
                }
                
                // Get or create alias
                var alias = mappings.GetOrCreateAlias(originalValue, rule.Prefix);
                
                // Calculate line number (1-based)
                var lineNumber = content.Substring(0, match.Index).Split('\n').Length;
                
                matches.Add(new SanitizationMatch
                {
                    OriginalValue = originalValue,
                    Alias = alias,
                    RuleId = rule.RuleId,
                    LineNumber = lineNumber,
                    StartIndex = match.Index,
                    Length = match.Length
                });
            }
        }
        
        // Sort matches: by position first, then longer matches take priority
        // This ensures "Server=PRODSRV01" is preferred over just "PRODSRV01"
        var sortedMatches = matches
            .OrderBy(m => m.StartIndex)
            .ThenByDescending(m => m.Length)
            .ToList();
        
        // Filter out overlapping matches (keep the first/longest one)
        var nonOverlappingMatches = new List<SanitizationMatch>();
        var lastEndIndex = 0;
        
        foreach (var match in sortedMatches)
        {
            // Skip if this match overlaps with a previously accepted match
            if (match.StartIndex < lastEndIndex)
            {
                continue;
            }
            
            nonOverlappingMatches.Add(match);
            lastEndIndex = match.StartIndex + match.Length;
        }
        
        // Build result by replacing non-overlapping matches
        var resultBuilder = new StringBuilder();
        var currentIndex = 0;
        
        foreach (var match in nonOverlappingMatches)
        {
            // Add content before this match
            if (match.StartIndex > currentIndex)
            {
                resultBuilder.Append(content, currentIndex, match.StartIndex - currentIndex);
            }
            
            // Add the alias
            resultBuilder.Append(match.Alias);
            currentIndex = match.StartIndex + match.Length;
        }
        
        // Add remaining content
        if (currentIndex < content.Length)
        {
            resultBuilder.Append(content, currentIndex, content.Length - currentIndex);
        }
        
        return new SanitizationResult
        {
            Content = resultBuilder.ToString(),
            WasSanitized = nonOverlappingMatches.Count > 0,
            Matches = nonOverlappingMatches
        };
    }
}

