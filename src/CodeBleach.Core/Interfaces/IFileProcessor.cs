using CodeBleach.Core.Models;

namespace CodeBleach.Core.Interfaces;

/// <summary>
/// Handles file system operations for sanitization.
/// </summary>
public interface IFileProcessor
{
    /// <summary>Determines if a file should be processed based on extension and path.</summary>
    bool ShouldProcess(string filePath);
    
    /// <summary>Determines if a path should be ignored (binary, .git, node_modules, etc.).</summary>
    bool ShouldIgnore(string path);
    
    /// <summary>Gets all files to process in a directory.</summary>
    IEnumerable<string> GetFilesToProcess(string directoryPath);
    
    /// <summary>Copies directory structure, skipping ignored paths.</summary>
    Task CopyDirectoryAsync(
        string sourcePath, 
        string destinationPath, 
        CancellationToken ct = default);
    
    /// <summary>Processes a single file (sanitize or restore).</summary>
    Task<FileProcessingResult> ProcessFileAsync(
        string sourcePath,
        string destinationPath,
        Func<string, string> transform,
        CancellationToken ct = default);
}

