namespace CodeBleach.Core.Models;

/// <summary>
/// Result of processing a file.
/// </summary>
public record FileProcessingResult
{
    public required string FilePath { get; init; }
    public required bool WasProcessed { get; init; }
    public required int ReplacementCount { get; init; }
    public string? ErrorMessage { get; init; }
}

