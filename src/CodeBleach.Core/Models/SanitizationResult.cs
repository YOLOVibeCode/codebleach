namespace CodeBleach.Core.Models;

/// <summary>
/// Result of sanitizing content.
/// </summary>
public record SanitizationResult
{
    public required string Content { get; init; }
    public required bool WasSanitized { get; init; }
    public required IReadOnlyList<SanitizationMatch> Matches { get; init; }
}

/// <summary>
/// Represents a single sanitization match/replacement.
/// </summary>
public record SanitizationMatch
{
    public required string OriginalValue { get; init; }
    public required string Alias { get; init; }
    public required string RuleId { get; init; }
    public required int LineNumber { get; init; }
    public required int StartIndex { get; init; }
    public required int Length { get; init; }
}

