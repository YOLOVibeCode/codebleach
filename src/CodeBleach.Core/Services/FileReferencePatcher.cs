using System.Text.RegularExpressions;
using System.Xml.Linq;
using CodeBleach.Core.Models;

namespace CodeBleach.Core.Services;

/// <summary>
/// Patches cross-file references in the output directory to match renamed file paths.
/// Must be called AFTER FileNameMapper.BuildMappings() but BEFORE actual file renames.
///
/// Handles:
///   - .fsproj: Compile Include + ProjectReference Include
///   - .csproj/.vbproj: ProjectReference Include
///   - .sln: Project path entries
///   - .slnx: Project Path attributes (XML format)
///   - JavaScript/TypeScript: import/require/export source paths
///   - package.json: main, module, types entry points
/// </summary>
public sealed class FileReferencePatcher
{
    private static readonly string[] JsExtensions = [".js", ".ts", ".jsx", ".tsx", ".mjs", ".cjs"];

    // Regex patterns for JS import/require/export source paths
    private static readonly Regex ImportFromRegex = new(
        @"(?:import|export)\s+.*?\s+from\s+['""]([^'""]+)['""]",
        RegexOptions.Compiled | RegexOptions.Singleline);

    private static readonly Regex RequireRegex = new(
        @"require\s*\(\s*['""]([^'""]+)['""]",
        RegexOptions.Compiled);

    private static readonly Regex DynamicImportRegex = new(
        @"import\s*\(\s*['""]([^'""]+)['""]",
        RegexOptions.Compiled);

    // .sln project line pattern: Project("{guid}") = "Name", "path\to\project.csproj", "{guid}"
    private static readonly Regex SlnProjectRegex = new(
        @"^Project\(""\{[^}]+\}""\)\s*=\s*""[^""]*"",\s*""([^""]+)"",\s*""\{[^}]+\}""",
        RegexOptions.Compiled | RegexOptions.Multiline);

    /// <summary>
    /// Patches all cross-file references in the output directory.
    /// Returns the number of files patched.
    /// </summary>
    public async Task<int> PatchReferencesAsync(
        string outputPath,
        MappingTable mappings,
        bool verbose = false)
    {
        var forward = mappings.FilePathForward;
        if (forward.Count == 0) return 0;

        var patchCount = 0;

        patchCount += await PatchProjectFilesAsync(outputPath, forward, verbose);
        patchCount += await PatchSolutionFilesAsync(outputPath, forward, verbose);
        patchCount += await PatchJavaScriptImportsAsync(outputPath, forward, verbose);

        return patchCount;
    }

    /// <summary>
    /// Patches .csproj, .fsproj, .vbproj files: ProjectReference + Compile Include.
    /// </summary>
    private async Task<int> PatchProjectFilesAsync(
        string outputPath,
        Dictionary<string, string> forward,
        bool verbose)
    {
        var patchCount = 0;
        var projFiles = Directory.EnumerateFiles(outputPath, "*.*proj", SearchOption.AllDirectories)
            .Where(f =>
            {
                var ext = Path.GetExtension(f).ToLowerInvariant();
                return ext is ".csproj" or ".fsproj" or ".vbproj";
            });

        foreach (var projFile in projFiles)
        {
            var projRelative = Path.GetRelativePath(outputPath, projFile).Replace('\\', '/');
            if (!forward.TryGetValue(projRelative, out var projNewPath))
                continue;

            var content = await File.ReadAllTextAsync(projFile);
            var doc = XDocument.Parse(content, LoadOptions.PreserveWhitespace);
            var changed = false;

            foreach (var element in doc.Descendants())
            {
                var includeAttr = element.Attribute("Include");
                if (includeAttr == null) continue;

                var elementName = element.Name.LocalName;
                if (elementName is not ("Compile" or "ProjectReference" or "None" or "Content" or "EmbeddedResource"))
                    continue;

                var originalRef = includeAttr.Value;
                var newRef = ResolveAndMapReference(projRelative, projNewPath, originalRef, forward);
                if (newRef != null && newRef != originalRef)
                {
                    includeAttr.Value = newRef;
                    changed = true;

                    if (verbose)
                    {
                        Console.WriteLine($"  Patched {elementName} in {projRelative}: {originalRef} → {newRef}");
                    }
                }
            }

            if (changed)
            {
                // Preserve original encoding and write settings
                using var writer = new StreamWriter(projFile, false, System.Text.Encoding.UTF8);
                await writer.WriteAsync(doc.Declaration?.ToString() ?? "");
                if (doc.Declaration != null) await writer.WriteLineAsync();
                await writer.WriteAsync(doc.ToString());
                patchCount++;
            }
        }

        return patchCount;
    }

    /// <summary>
    /// Patches .sln files: Project path entries.
    /// </summary>
    private async Task<int> PatchSolutionFilesAsync(
        string outputPath,
        Dictionary<string, string> forward,
        bool verbose)
    {
        var patchCount = 0;

        // Handle .sln files (text format)
        foreach (var slnFile in Directory.EnumerateFiles(outputPath, "*.sln", SearchOption.AllDirectories))
        {
            var slnRelative = Path.GetRelativePath(outputPath, slnFile).Replace('\\', '/');
            if (!forward.TryGetValue(slnRelative, out var slnNewPath))
                continue;

            var content = await File.ReadAllTextAsync(slnFile);
            var changed = false;

            content = SlnProjectRegex.Replace(content, match =>
            {
                var originalPath = match.Groups[1].Value;
                var newRef = ResolveAndMapReference(slnRelative, slnNewPath, originalPath, forward);

                if (newRef != null && newRef != originalPath)
                {
                    changed = true;
                    if (verbose)
                    {
                        Console.WriteLine($"  Patched project ref in {slnRelative}: {originalPath} → {newRef}");
                    }
                    // Replace only the path portion in the full match
                    return match.Value.Replace(originalPath, newRef);
                }
                return match.Value;
            });

            if (changed)
            {
                await File.WriteAllTextAsync(slnFile, content);
                patchCount++;
            }
        }

        // Handle .slnx files (XML format)
        foreach (var slnxFile in Directory.EnumerateFiles(outputPath, "*.slnx", SearchOption.AllDirectories))
        {
            var slnxRelative = Path.GetRelativePath(outputPath, slnxFile).Replace('\\', '/');
            if (!forward.TryGetValue(slnxRelative, out var slnxNewPath))
                continue;

            var content = await File.ReadAllTextAsync(slnxFile);
            var doc = XDocument.Parse(content, LoadOptions.PreserveWhitespace);
            var changed = false;

            foreach (var project in doc.Descendants("Project"))
            {
                var pathAttr = project.Attribute("Path");
                if (pathAttr == null) continue;

                var originalPath = pathAttr.Value;
                var newRef = ResolveAndMapReference(slnxRelative, slnxNewPath, originalPath, forward);
                if (newRef != null && newRef != originalPath)
                {
                    pathAttr.Value = newRef;
                    changed = true;

                    if (verbose)
                    {
                        Console.WriteLine($"  Patched project ref in {slnxRelative}: {originalPath} → {newRef}");
                    }
                }
            }

            if (changed)
            {
                using var writer = new StreamWriter(slnxFile, false, System.Text.Encoding.UTF8);
                if (doc.Declaration != null)
                {
                    await writer.WriteAsync(doc.Declaration.ToString());
                    await writer.WriteLineAsync();
                }
                await writer.WriteAsync(doc.ToString());
                patchCount++;
            }
        }

        return patchCount;
    }

    /// <summary>
    /// Patches JavaScript/TypeScript import, require, and export source paths.
    /// </summary>
    private async Task<int> PatchJavaScriptImportsAsync(
        string outputPath,
        Dictionary<string, string> forward,
        bool verbose)
    {
        var patchCount = 0;
        var jsFiles = Directory.EnumerateFiles(outputPath, "*.*", SearchOption.AllDirectories)
            .Where(f => JsExtensions.Contains(Path.GetExtension(f).ToLowerInvariant()));

        foreach (var jsFile in jsFiles)
        {
            var jsRelative = Path.GetRelativePath(outputPath, jsFile).Replace('\\', '/');
            if (!forward.TryGetValue(jsRelative, out var jsNewPath))
                continue;

            var content = await File.ReadAllTextAsync(jsFile);
            var newContent = content;
            var fileChanged = false;
            bool patched;

            (newContent, patched) = PatchJsPatterns(newContent, ImportFromRegex, jsRelative, jsNewPath, forward, verbose);
            fileChanged |= patched;
            (newContent, patched) = PatchJsPatterns(newContent, RequireRegex, jsRelative, jsNewPath, forward, verbose);
            fileChanged |= patched;
            (newContent, patched) = PatchJsPatterns(newContent, DynamicImportRegex, jsRelative, jsNewPath, forward, verbose);
            fileChanged |= patched;

            if (fileChanged)
            {
                await File.WriteAllTextAsync(jsFile, newContent);
                patchCount++;
            }
        }

        return patchCount;
    }

    private (string Content, bool Changed) PatchJsPatterns(
        string content,
        Regex pattern,
        string jsRelative,
        string jsNewPath,
        Dictionary<string, string> forward,
        bool verbose)
    {
        var anyChanged = false;
        var result = pattern.Replace(content, match =>
        {
            var importPath = match.Groups[1].Value;

            // Only patch relative imports (not npm packages)
            if (!importPath.StartsWith("./") && !importPath.StartsWith("../"))
                return match.Value;

            var newImportPath = ResolveAndMapJsImport(jsRelative, jsNewPath, importPath, forward);
            if (newImportPath != null && newImportPath != importPath)
            {
                anyChanged = true;
                if (verbose)
                {
                    Console.WriteLine($"  Patched import in {jsRelative}: '{importPath}' → '{newImportPath}'");
                }
                return match.Value.Replace(importPath, newImportPath);
            }
            return match.Value;
        });
        return (result, anyChanged);
    }

    /// <summary>
    /// Resolves a reference relative to a source file and maps it to the new path,
    /// then computes the new relative reference from the source file's new location.
    /// </summary>
    private string? ResolveAndMapReference(
        string sourceOriginalPath,
        string sourceNewPath,
        string reference,
        Dictionary<string, string> forward)
    {
        // Normalize the reference path
        var normalizedRef = reference.Replace('\\', '/');
        var usesBackslash = reference.Contains('\\');

        // Resolve the reference relative to the source file's original directory
        var sourceDir = GetDirectory(sourceOriginalPath);
        var resolvedTarget = NormalizePath(sourceDir + "/" + normalizedRef);

        // Look up in forward mapping
        if (!forward.TryGetValue(resolvedTarget, out var targetNewPath))
            return null;

        // Compute new relative path from source's new location to target's new location
        var sourceNewDir = GetDirectory(sourceNewPath);
        var newRelative = ComputeRelativePath(sourceNewDir, targetNewPath);

        // Restore original slash convention
        if (usesBackslash)
            newRelative = newRelative.Replace('/', '\\');

        return newRelative;
    }

    /// <summary>
    /// Resolves a JS import path (which may omit extensions) and maps it.
    /// </summary>
    private string? ResolveAndMapJsImport(
        string jsOriginalPath,
        string jsNewPath,
        string importPath,
        Dictionary<string, string> forward)
    {
        var jsDir = GetDirectory(jsOriginalPath);
        var resolvedBase = NormalizePath(jsDir + "/" + importPath);
        var hasExtension = Path.HasExtension(importPath);

        // Try exact match first
        if (forward.TryGetValue(resolvedBase, out var targetNew))
        {
            return ComputeJsImportPath(jsNewPath, targetNew, hasExtension);
        }

        // Try with common extensions
        foreach (var ext in JsExtensions)
        {
            if (forward.TryGetValue(resolvedBase + ext, out targetNew))
            {
                return ComputeJsImportPath(jsNewPath, targetNew, hasExtension);
            }
        }

        // Try index file (directory import)
        foreach (var ext in JsExtensions)
        {
            if (forward.TryGetValue(resolvedBase + "/index" + ext, out targetNew))
            {
                // For directory imports, compute path to the directory (not the index file)
                var targetDir = GetDirectory(targetNew);
                var jsNewDir = GetDirectory(jsNewPath);
                var rel = ComputeRelativePath(jsNewDir, targetDir);
                return rel.Length == 0 ? "." : rel;
            }
        }

        return null;
    }

    private string ComputeJsImportPath(string fromFile, string toFile, bool includeExtension)
    {
        var fromDir = GetDirectory(fromFile);
        var relativePath = ComputeRelativePath(fromDir, toFile);

        if (!includeExtension)
        {
            // Strip extension to match original convention
            var ext = Path.GetExtension(relativePath);
            if (JsExtensions.Contains(ext.ToLowerInvariant()))
            {
                relativePath = relativePath[..^ext.Length];
            }
        }

        // Ensure relative import starts with ./
        if (!relativePath.StartsWith("../"))
        {
            relativePath = "./" + relativePath;
        }

        return relativePath;
    }

    /// <summary>
    /// Normalizes a path by resolving . and .. segments.
    /// </summary>
    internal static string NormalizePath(string path)
    {
        var parts = path.Replace('\\', '/').Split('/');
        var result = new List<string>();

        foreach (var part in parts)
        {
            if (part == "." || part == "") continue;
            if (part == ".." && result.Count > 0 && result[^1] != "..")
            {
                result.RemoveAt(result.Count - 1);
            }
            else
            {
                result.Add(part);
            }
        }

        return string.Join("/", result);
    }

    /// <summary>
    /// Computes a relative path from a directory to a file.
    /// </summary>
    internal static string ComputeRelativePath(string fromDir, string toPath)
    {
        var fromParts = fromDir.Split('/', StringSplitOptions.RemoveEmptyEntries);
        var toParts = toPath.Split('/', StringSplitOptions.RemoveEmptyEntries);

        // Find common prefix length
        var common = 0;
        while (common < fromParts.Length && common < toParts.Length &&
               string.Equals(fromParts[common], toParts[common], StringComparison.OrdinalIgnoreCase))
        {
            common++;
        }

        var parts = new List<string>();
        for (var i = common; i < fromParts.Length; i++)
            parts.Add("..");
        for (var i = common; i < toParts.Length; i++)
            parts.Add(toParts[i]);

        return string.Join("/", parts);
    }

    private static string GetDirectory(string filePath)
    {
        var dir = Path.GetDirectoryName(filePath.Replace('\\', '/'))?.Replace('\\', '/');
        return string.IsNullOrEmpty(dir) ? "" : dir;
    }
}
