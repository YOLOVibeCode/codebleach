using CodeBleach.Core.Interfaces;

namespace CodeBleach.Core.Models;

/// <summary>
/// Shared context across all files and all language processors during a single obfuscation run.
/// Ensures the same identifier gets the same alias everywhere (cross-file, cross-language).
/// Thread-safe for potential parallel file processing.
/// </summary>
public sealed class ObfuscationContext
{
    private readonly object _lock = new();

    /// <summary>The underlying MappingTable (backward compatible with v1).</summary>
    public MappingTable Mappings { get; }

    /// <summary>Enhanced source map with semantic categories and locations.</summary>
    public EnhancedSourceMap SourceMap { get; }

    /// <summary>Current obfuscation level.</summary>
    public ObfuscationLevel Level { get; }

    /// <summary>Naming strategy for alias generation.</summary>
    public NamingStrategy NamingStrategy { get; }

    /// <summary>
    /// Optional reference to the processor registry, enabling cross-language delegation
    /// (e.g., COBOL EXEC SQL blocks delegating to the SQL processor).
    /// Set by SanitizeCommand when Level 2 is active.
    /// </summary>
    public ILanguageProcessorRegistry? ProcessorRegistry { get; set; }

    public ObfuscationContext(ObfuscationLevel level, MappingTable? mappings = null, NamingStrategy? namingStrategy = null)
    {
        Level = level;
        Mappings = mappings ?? new MappingTable();
        NamingStrategy = namingStrategy ?? new NamingStrategy();
        SourceMap = new EnhancedSourceMap();
    }

    /// <summary>
    /// Gets or creates a semantically-categorized alias. This is the primary method
    /// language processors should call. Thread-safe.
    /// </summary>
    /// <param name="originalValue">The original identifier value.</param>
    /// <param name="category">Semantic category (Class, Table, Paragraph, etc.).</param>
    /// <param name="sourceFile">Optional source file path for location tracking.</param>
    /// <param name="lineNumber">Optional line number for location tracking.</param>
    /// <param name="columnStart">Optional column start for location tracking.</param>
    /// <param name="columnEnd">Optional column end for location tracking.</param>
    /// <returns>The alias (e.g., "CLS_0", "TBL_3", "PARA_1").</returns>
    public string GetOrCreateAlias(
        string originalValue,
        SemanticCategory category,
        string? sourceFile = null,
        int? lineNumber = null,
        int? columnStart = null,
        int? columnEnd = null)
    {
        lock (_lock)
        {
            var prefix = NamingStrategy.GetPrefix(category);
            var alias = Mappings.GetOrCreateAlias(originalValue, prefix);

            // Track in enhanced source map
            var entry = SourceMap.GetOrAddEntry(originalValue, alias, category);
            if (sourceFile != null && lineNumber != null)
            {
                entry.Locations.Add(new SourceLocation
                {
                    FilePath = sourceFile,
                    LineNumber = lineNumber.Value,
                    ColumnStart = columnStart ?? 0,
                    ColumnEnd = columnEnd ?? 0
                });
            }

            return alias;
        }
    }

    /// <summary>
    /// Records a cross-language reference (e.g., COBOL host variable used in EXEC SQL).
    /// </summary>
    public void AddCrossReference(CrossLanguageReference crossRef)
    {
        lock (_lock)
        {
            SourceMap.CrossReferences.Add(crossRef);
        }
    }

    /// <summary>
    /// Records which processor was used for a specific file.
    /// </summary>
    public void RecordFileProcessing(string filePath, string processorId, int replacementCount)
    {
        lock (_lock)
        {
            SourceMap.FileMap[filePath] = new FileSourceMap
            {
                FilePath = filePath,
                ProcessorId = processorId,
                Level = Level,
                ReplacementCount = replacementCount
            };
        }
    }
}
