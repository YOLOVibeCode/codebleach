using CodeBleach.Core.Models;

namespace CodeBleach.Core.Interfaces;

/// <summary>
/// Language-specific processor that performs AST-aware obfuscation.
/// Each processor handles one or more file extensions and understands
/// the language's syntax deeply enough to rename identifiers while
/// preserving syntactic validity.
/// </summary>
public interface ILanguageProcessor
{
    /// <summary>Unique identifier for this processor (e.g., "csharp", "tsql", "cobol").</summary>
    string ProcessorId { get; }

    /// <summary>Display name for CLI output (e.g., "C# (Roslyn)", "COBOL (Enterprise)").</summary>
    string DisplayName { get; }

    /// <summary>File extensions this processor handles (e.g., ".cs", ".sql", ".cbl").</summary>
    IReadOnlySet<string> SupportedExtensions { get; }

    /// <summary>
    /// Priority when multiple processors could handle the same extension.
    /// Lower = higher priority. Allows DB2 SQL processor to override generic SQL for ".db2" files.
    /// </summary>
    int Priority { get; }

    /// <summary>
    /// Whether this processor can handle the given file based on content heuristics
    /// (not just extension). For example, detecting EXEC SQL in COBOL files,
    /// or distinguishing VBScript from VBA.
    /// </summary>
    bool CanProcess(string filePath, string content);

    /// <summary>
    /// Optional: Prepare batch context from all files that will be processed.
    /// Called once before individual Obfuscate calls. Used by C#/VB.NET processors
    /// to build a Roslyn Compilation for semantic analysis across multiple files.
    /// Default implementation is a no-op.
    /// </summary>
    void PrepareBatch(IReadOnlyList<string> filePaths, ObfuscationContext context) { }

    /// <summary>
    /// Obfuscates the content using language-aware AST/token analysis.
    /// Must use the shared ObfuscationContext for cross-file/cross-language consistency.
    /// </summary>
    LanguageProcessingResult Obfuscate(string content, ObfuscationContext context, string? filePath = null);

    /// <summary>
    /// Restores obfuscated content back to original form using the context mappings.
    /// </summary>
    LanguageProcessingResult Deobfuscate(string content, ObfuscationContext context, string? filePath = null);

    /// <summary>
    /// Validates that the obfuscated output is syntactically valid for this language.
    /// Returns parse errors if any. Used for testing and quality assurance.
    /// </summary>
    ValidationResult Validate(string obfuscatedContent);
}
