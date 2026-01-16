using CodeBleach.Core.Models;

namespace CodeBleach.Core.Interfaces;

/// <summary>
/// Restores sanitized content by replacing aliases with original values.
/// </summary>
public interface IRestorer
{
    /// <summary>
    /// Restores the given content using the mapping table.
    /// </summary>
    /// <param name="content">Sanitized content</param>
    /// <param name="mappings">Mapping table with reverse mappings</param>
    /// <returns>Restored content with original values</returns>
    RestoreResult Restore(string content, MappingTable mappings);
}

