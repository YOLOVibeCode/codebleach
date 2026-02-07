using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;

namespace CodeBleach.Core.Services;

/// <summary>
/// Handles file system operations for sanitization.
/// </summary>
public sealed class FileProcessor : IFileProcessor
{
    private static readonly HashSet<string> SupportedExtensions = new(StringComparer.OrdinalIgnoreCase)
    {
        // .NET languages
        ".cs", ".csx", ".fs", ".fsx", ".fsi", ".vb",
        // JavaScript / TypeScript
        ".js", ".ts", ".jsx", ".tsx", ".mjs", ".cjs",
        // VBScript / VBA
        ".vbs", ".bas", ".cls", ".frm",
        // Other languages
        ".py", ".go", ".rs", ".java", ".kt", ".swift", ".rb", ".php",
        // Data / Config
        ".json", ".yaml", ".yml", ".xml", ".config", ".ini", ".toml", ".env",
        // Shell
        ".sh", ".bash", ".ps1", ".psm1", ".bat", ".cmd",
        // Web
        ".html", ".htm", ".css", ".scss", ".less",
        // SQL (T-SQL, DB2, Oracle, generic)
        ".sql", ".db2", ".pls", ".plb", ".pks", ".pkb", ".fnc", ".prc", ".trg", ".graphql",
        // Mainframe
        ".cbl", ".cob", ".cpy", ".jcl",
        // Docs
        ".md", ".txt", ".rst"
    };
    
    private static readonly string[] IgnoredDirectories = 
    {
        ".codebleach", ".git", ".svn", ".hg",
        "node_modules", "vendor", "packages", ".nuget",
        "bin", "obj", "dist", "build", "target", "out",
        ".vs", ".vscode", ".idea"
    };
    
    private static readonly string[] IgnoredExtensions = 
    {
        ".exe", ".dll", ".so", ".dylib", ".pdb",
        ".png", ".jpg", ".jpeg", ".gif", ".ico", ".svg",
        ".zip", ".tar", ".gz", ".rar", ".7z",
        ".pfx", ".p12", ".cer", ".crt", ".key"
    };
    
    public bool ShouldProcess(string filePath)
    {
        var extension = Path.GetExtension(filePath);
        return SupportedExtensions.Contains(extension);
    }
    
    public bool ShouldIgnore(string path)
    {
        var normalizedPath = path.Replace('\\', '/');
        
        // Check ignored directories
        foreach (var ignoredDir in IgnoredDirectories)
        {
            if (normalizedPath.Contains($"/{ignoredDir}/", StringComparison.OrdinalIgnoreCase) ||
                normalizedPath.StartsWith($"{ignoredDir}/", StringComparison.OrdinalIgnoreCase))
            {
                return true;
            }
        }
        
        // Check ignored extensions
        var extension = Path.GetExtension(path);
        if (IgnoredExtensions.Contains(extension, StringComparer.OrdinalIgnoreCase))
        {
            return true;
        }
        
        // Check file size (skip files > 10MB)
        try
        {
            if (File.Exists(path))
            {
                var fileInfo = new FileInfo(path);
                if (fileInfo.Length > 10 * 1024 * 1024) // 10MB
                {
                    return true;
                }
            }
        }
        catch
        {
            // If we can't check, assume we should process it
        }
        
        return false;
    }
    
    public IEnumerable<string> GetFilesToProcess(string directoryPath)
    {
        if (!Directory.Exists(directoryPath))
        {
            return [];
        }
        
        return Directory.EnumerateFiles(directoryPath, "*", SearchOption.AllDirectories)
            .Where(f => !ShouldIgnore(f) && ShouldProcess(f));
    }
    
    public async Task CopyDirectoryAsync(string sourcePath, string destinationPath, CancellationToken ct = default)
    {
        if (!Directory.Exists(sourcePath))
        {
            throw new DirectoryNotFoundException($"Source directory not found: {sourcePath}");
        }
        
        Directory.CreateDirectory(destinationPath);
        
        foreach (var file in Directory.EnumerateFiles(sourcePath, "*", SearchOption.AllDirectories))
        {
            ct.ThrowIfCancellationRequested();
            
            if (ShouldIgnore(file))
            {
                continue;
            }
            
            var relativePath = Path.GetRelativePath(sourcePath, file);
            var destFile = Path.Combine(destinationPath, relativePath);
            var destDir = Path.GetDirectoryName(destFile);
            
            if (destDir != null)
            {
                Directory.CreateDirectory(destDir);
            }
            
            File.Copy(file, destFile, overwrite: true);
        }
    }
    
    public async Task<FileProcessingResult> ProcessFileAsync(
        string sourcePath,
        string destinationPath,
        Func<string, string> transform,
        CancellationToken ct = default)
    {
        ct.ThrowIfCancellationRequested();
        
        try
        {
            var content = await File.ReadAllTextAsync(sourcePath, ct);
            var transformed = transform(content);
            var destDir = Path.GetDirectoryName(destinationPath);
            
            if (destDir != null)
            {
                Directory.CreateDirectory(destDir);
            }
            
            await File.WriteAllTextAsync(destinationPath, transformed, ct);
            
            // Count replacements (simple heuristic: count differences)
            var replacementCount = content != transformed ? 1 : 0;
            
            return new FileProcessingResult
            {
                FilePath = destinationPath,
                WasProcessed = true,
                ReplacementCount = replacementCount
            };
        }
        catch (Exception ex)
        {
            return new FileProcessingResult
            {
                FilePath = destinationPath,
                WasProcessed = false,
                ReplacementCount = 0,
                ErrorMessage = ex.Message
            };
        }
    }
}

