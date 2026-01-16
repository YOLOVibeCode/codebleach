namespace CodeBleach.Core.Models;

/// <summary>
/// Result of restoring sanitized content.
/// </summary>
public record RestoreResult
{
    public required string Content { get; init; }
    public required bool WasRestored { get; init; }
    public required int ReplacementCount { get; init; }
    public required IReadOnlyList<string> UnmatchedAliases { get; init; }
}

