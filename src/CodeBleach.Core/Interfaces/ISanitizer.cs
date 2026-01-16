using CodeBleach.Core.Models;

namespace CodeBleach.Core.Interfaces;

/// <summary>
/// Sanitizes content by replacing sensitive values with aliases.
/// </summary>
public interface ISanitizer
{
    /// <summary>
    /// Sanitizes the given content using active rules.
    /// </summary>
    /// <param name="content">Content to sanitize</param>
    /// <param name="mappings">Mapping table to use/update</param>
    /// <returns>Sanitization result with transformed content</returns>
    SanitizationResult Sanitize(string content, MappingTable mappings);
}

