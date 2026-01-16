using System.Text.Json;
using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;

namespace CodeBleach.Core.Services;

/// <summary>
/// Manages manifest and cross-reference file operations.
/// </summary>
public sealed class ManifestManager : IManifestManager
{
    public bool IsSanitizedDirectory(string path)
    {
        var manifestPath = Path.Combine(path, Manifest.DirectoryName, Manifest.FileName);
        return File.Exists(manifestPath);
    }
    
    public async Task<Manifest?> LoadManifestAsync(string sanitizedPath, CancellationToken ct = default)
    {
        var manifestPath = Path.Combine(sanitizedPath, Manifest.DirectoryName, Manifest.FileName);
        
        if (!File.Exists(manifestPath))
        {
            return null;
        }
        
        try
        {
            var json = await File.ReadAllTextAsync(manifestPath, ct);
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true
            };
            return JsonSerializer.Deserialize<Manifest>(json, options);
        }
        catch
        {
            return null;
        }
    }
    
    public async Task SaveManifestAsync(Manifest manifest, string sanitizedPath, CancellationToken ct = default)
    {
        var codebleachDir = Path.Combine(sanitizedPath, Manifest.DirectoryName);
        Directory.CreateDirectory(codebleachDir);
        
        var manifestPath = Path.Combine(codebleachDir, Manifest.FileName);
        var options = new JsonSerializerOptions
        {
            WriteIndented = true
        };
        var json = JsonSerializer.Serialize(manifest, options);
        await File.WriteAllTextAsync(manifestPath, json, ct);
    }
    
    public async Task SaveXrefAsync(Manifest manifest, string sanitizedPath, CancellationToken ct = default)
    {
        var codebleachDir = Path.Combine(sanitizedPath, Manifest.DirectoryName);
        Directory.CreateDirectory(codebleachDir);
        
        var xrefPath = Path.Combine(codebleachDir, "xref.md");
        var xref = GenerateXref(manifest);
        await File.WriteAllTextAsync(xrefPath, xref, ct);
    }
    
    private static string GenerateXref(Manifest manifest)
    {
        var sb = new System.Text.StringBuilder();
        sb.AppendLine("# CodeBleach Cross-Reference");
        sb.AppendLine();
        sb.AppendLine($"> **Source:** `{manifest.SourcePath}`");
        sb.AppendLine($"> **Sanitized:** `{manifest.CreatedAt:yyyy-MM-dd HH:mm:ss}`");
        sb.AppendLine($"> **Files Processed:** {manifest.ProcessedFiles.Count}");
        sb.AppendLine();
        sb.AppendLine("## Substitution Table");
        sb.AppendLine();
        sb.AppendLine("| Original Value | Alias | Occurrences |");
        sb.AppendLine("|----------------|-------|-------------|");
        
        foreach (var kvp in manifest.Mappings.Forward.OrderBy(k => k.Value))
        {
            sb.AppendLine($"| `{kvp.Key}` | `{kvp.Value}` | - |");
        }
        
        sb.AppendLine();
        sb.AppendLine("## Files Modified");
        sb.AppendLine();
        sb.AppendLine("| File | Replacements |");
        sb.AppendLine("|------|--------------|");
        
        foreach (var file in manifest.ProcessedFiles)
        {
            sb.AppendLine($"| `{file}` | - |");
        }
        
        return sb.ToString();
    }
}

