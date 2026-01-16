namespace CodeBleach.Core.Models;

/// <summary>
/// Complete metadata for a sanitization run, stored in .codebleach/manifest.json
/// </summary>
public record Manifest
{
    public const string CurrentVersion = "1.0";
    public const string FileName = "manifest.json";
    public const string DirectoryName = ".codebleach";
    
    public required string Version { get; init; }
    public required string SourcePath { get; init; }
    public required string DestinationPath { get; init; }
    public required DateTime CreatedAt { get; init; }
    public DateTime? RestoredAt { get; init; }
    public required MappingTable Mappings { get; init; }
    public required IReadOnlyList<string> ProcessedFiles { get; init; }
    public required SanitizationStats Stats { get; init; }
}

/// <summary>
/// Statistics for a sanitization run.
/// </summary>
public record SanitizationStats
{
    public required int TotalFiles { get; init; }
    public required int ProcessedFiles { get; init; }
    public required int SkippedFiles { get; init; }
    public required int TotalReplacements { get; init; }
    public required int UniqueValuesReplaced { get; init; }
    public required long ProcessingTimeMs { get; init; }
}

