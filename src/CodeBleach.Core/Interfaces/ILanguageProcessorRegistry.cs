namespace CodeBleach.Core.Interfaces;

/// <summary>
/// Registry for language-specific processors. Handles registration,
/// lookup by file extension/content, and priority-based selection.
/// </summary>
public interface ILanguageProcessorRegistry
{
    /// <summary>Registers a language processor.</summary>
    void Register(ILanguageProcessor processor);

    /// <summary>
    /// Gets the best processor for a given file, or null if no processor matches.
    /// Lookup order: extension match -> priority sort -> content heuristic (CanProcess).
    /// Returns null means the file should be handled by the Level 1 regex sanitizer.
    /// </summary>
    ILanguageProcessor? GetProcessor(string filePath, string? content = null);

    /// <summary>Gets all registered processors.</summary>
    IReadOnlyList<ILanguageProcessor> GetAll();
}
