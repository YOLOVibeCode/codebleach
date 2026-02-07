namespace CodeBleach.Core.Models;

/// <summary>
/// Bidirectional mapping between original values and aliases.
/// </summary>
public sealed class MappingTable
{
    /// <summary>Original value → Alias (e.g., "ProductionDB" → "SERVER_0")</summary>
    public Dictionary<string, string> Forward { get; init; } = new();
    
    /// <summary>Alias → Original value (e.g., "SERVER_0" → "ProductionDB")</summary>
    public Dictionary<string, string> Reverse { get; init; } = new();
    
    /// <summary>Counter per prefix for alias generation</summary>
    public Dictionary<string, int> Counters { get; init; } = new();

    /// <summary>Original relative file path → obfuscated relative file path (e.g., "src/Services/PayrollService.cs" → "src/DIR_0/FILE_0.cs")</summary>
    public Dictionary<string, string> FilePathForward { get; init; } = new();

    /// <summary>Obfuscated relative file path → original relative file path</summary>
    public Dictionary<string, string> FilePathReverse { get; init; } = new();
    
    /// <summary>Gets or creates an alias for the given value.</summary>
    public string GetOrCreateAlias(string originalValue, string prefix)
    {
        if (Forward.TryGetValue(originalValue, out var existingAlias))
        {
            return existingAlias;
        }
        
        var counter = Counters.GetValueOrDefault(prefix, 0);
        var alias = $"{prefix}_{counter}";
        
        Forward[originalValue] = alias;
        Reverse[alias] = originalValue;
        Counters[prefix] = counter + 1;
        
        return alias;
    }
    
    /// <summary>Checks if a mapping exists for the original value.</summary>
    public bool HasMapping(string originalValue)
    {
        return Forward.ContainsKey(originalValue);
    }
    
    /// <summary>Gets the original value for an alias, or null if not found.</summary>
    public string? GetOriginal(string alias)
    {
        return Reverse.TryGetValue(alias, out var original) ? original : null;
    }
}

