namespace CodeBleach.Core.Models;

/// <summary>
/// Result of a language-aware obfuscation or deobfuscation operation.
/// </summary>
public record LanguageProcessingResult
{
    /// <summary>The transformed content.</summary>
    public required string Content { get; init; }

    /// <summary>Whether any transformations were applied.</summary>
    public required bool WasTransformed { get; init; }

    /// <summary>Number of identifiers replaced.</summary>
    public required int ReplacementCount { get; init; }

    /// <summary>The processor ID that performed the transformation.</summary>
    public required string ProcessorId { get; init; }

    /// <summary>Any warnings generated during processing (e.g., parse failures in subregions).</summary>
    public IReadOnlyList<string> Warnings { get; init; } = [];
}

/// <summary>
/// Result of validating obfuscated content for syntactic correctness.
/// </summary>
public record ValidationResult
{
    /// <summary>Whether the content is syntactically valid.</summary>
    public required bool IsValid { get; init; }

    /// <summary>Parse errors, if any.</summary>
    public IReadOnlyList<string> Errors { get; init; } = [];

    /// <summary>Parse warnings, if any.</summary>
    public IReadOnlyList<string> Warnings { get; init; } = [];

    public static ValidationResult Valid() => new() { IsValid = true };

    public static ValidationResult Invalid(IReadOnlyList<string> errors) => new()
    {
        IsValid = false,
        Errors = errors
    };
}
