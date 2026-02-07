namespace CodeBleach.Core.Models;

/// <summary>
/// Enhanced source map with per-entry semantic categories, per-file tracking,
/// and cross-language references. Extends the basic MappingTable for Level 2 obfuscation.
/// </summary>
public sealed class EnhancedSourceMap
{
    /// <summary>All mapping entries with full metadata, keyed by original value.</summary>
    public Dictionary<string, SourceMapEntry> Entries { get; init; } = new();

    /// <summary>Per-file mapping summaries.</summary>
    public Dictionary<string, FileSourceMap> FileMap { get; init; } = new();

    /// <summary>Cross-language references (e.g., COBOL data item used as SQL host variable).</summary>
    public List<CrossLanguageReference> CrossReferences { get; init; } = [];

    /// <summary>
    /// Gets an existing entry or adds a new one for the given original value.
    /// </summary>
    public SourceMapEntry GetOrAddEntry(string originalValue, string alias, SemanticCategory category)
    {
        if (Entries.TryGetValue(originalValue, out var existing))
        {
            return existing;
        }

        var entry = new SourceMapEntry
        {
            OriginalValue = originalValue,
            Alias = alias,
            Category = category
        };
        Entries[originalValue] = entry;
        return entry;
    }
}

/// <summary>
/// A single mapping entry with semantic metadata and location tracking.
/// </summary>
public sealed class SourceMapEntry
{
    public required string OriginalValue { get; init; }
    public required string Alias { get; init; }
    public required SemanticCategory Category { get; init; }

    /// <summary>All locations where this mapping was applied.</summary>
    public List<SourceLocation> Locations { get; init; } = [];
}

/// <summary>
/// A specific location in a source file where a mapping was applied.
/// </summary>
public record SourceLocation
{
    public required string FilePath { get; init; }
    public required int LineNumber { get; init; }
    public required int ColumnStart { get; init; }
    public required int ColumnEnd { get; init; }
}

/// <summary>
/// Per-file summary of obfuscation processing.
/// </summary>
public record FileSourceMap
{
    public required string FilePath { get; init; }
    public required string ProcessorId { get; init; }
    public required ObfuscationLevel Level { get; init; }
    public required int ReplacementCount { get; init; }
}

/// <summary>
/// Tracks a cross-language reference where the same identifier appears
/// in different languages (e.g., COBOL host variable in EXEC SQL).
/// </summary>
public record CrossLanguageReference
{
    public required string Alias { get; init; }
    public required string SourceLanguage { get; init; }
    public required string TargetLanguage { get; init; }
    public string? SourceFile { get; init; }
    public string? TargetFile { get; init; }
    public required string Description { get; init; }
}
