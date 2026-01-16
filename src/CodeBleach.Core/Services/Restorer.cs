using System.Text.RegularExpressions;
using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;

namespace CodeBleach.Core.Services;

/// <summary>
/// Restores sanitized content by replacing aliases with original values.
/// </summary>
public sealed class Restorer : IRestorer
{
    public RestoreResult Restore(string content, MappingTable mappings)
    {
        if (string.IsNullOrEmpty(content))
        {
            return new RestoreResult
            {
                Content = content,
                WasRestored = false,
                ReplacementCount = 0,
                UnmatchedAliases = []
            };
        }
        
        if (mappings.Reverse.Count == 0)
        {
            return new RestoreResult
            {
                Content = content,
                WasRestored = false,
                ReplacementCount = 0,
                UnmatchedAliases = []
            };
        }
        
        var result = content;
        var replacementCount = 0;
        var unmatchedAliases = new List<string>();
        
        // Sort aliases by length (descending) to avoid partial matches
        // e.g., SERVER_10 should be replaced before SERVER_1
        var sortedAliases = mappings.Reverse.Keys
            .OrderByDescending(a => a.Length)
            .ThenByDescending(a => a)
            .ToList();
        
        foreach (var alias in sortedAliases)
        {
            if (mappings.Reverse.TryGetValue(alias, out var original))
            {
                // Use word boundary to avoid partial matches
                var pattern = $@"\b{Regex.Escape(alias)}\b";
                var regex = new Regex(pattern, RegexOptions.Compiled);
                
                var matches = regex.Matches(result);
                if (matches.Count > 0)
                {
                    result = regex.Replace(result, original);
                    replacementCount += matches.Count;
                }
            }
        }
        
        // Check for unmatched aliases (pattern: PREFIX_N)
        var aliasPattern = new Regex(@"\b[A-Z]+_\d+\b", RegexOptions.Compiled);
        var potentialAliases = aliasPattern.Matches(result);
        foreach (Match match in potentialAliases)
        {
            var potentialAlias = match.Value;
            if (!mappings.Reverse.ContainsKey(potentialAlias))
            {
                unmatchedAliases.Add(potentialAlias);
            }
        }
        
        return new RestoreResult
        {
            Content = result,
            WasRestored = replacementCount > 0,
            ReplacementCount = replacementCount,
            UnmatchedAliases = unmatchedAliases.Distinct().ToList()
        };
    }
}

