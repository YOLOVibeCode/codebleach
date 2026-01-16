using CodeBleach.Core.Models;

namespace CodeBleach.Core.Interfaces;

/// <summary>
/// Manages manifest and cross-reference file operations.
/// </summary>
public interface IManifestManager
{
    /// <summary>Loads manifest from .codebleach directory.</summary>
    Task<Manifest?> LoadManifestAsync(string sanitizedPath, CancellationToken ct = default);
    
    /// <summary>Saves manifest to .codebleach directory.</summary>
    Task SaveManifestAsync(Manifest manifest, string sanitizedPath, CancellationToken ct = default);
    
    /// <summary>Generates and saves xref.md file.</summary>
    Task SaveXrefAsync(Manifest manifest, string sanitizedPath, CancellationToken ct = default);
    
    /// <summary>Checks if a path is a sanitized directory.</summary>
    bool IsSanitizedDirectory(string path);
}

