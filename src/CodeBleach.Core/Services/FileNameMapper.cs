using CodeBleach.Core.Models;

namespace CodeBleach.Core.Services;

/// <summary>
/// Builds file and directory name mappings for Level 2 obfuscation.
/// Renames files and directories to remove organizational fingerprints
/// while maintaining syntactical coherence with obfuscated content.
///
/// Two strategies:
///   A) Derive file name from primary type (C#, VB.NET, F#, COBOL, JCL)
///   B) Assign FILE_N alias (JS, TS, SQL, config, other)
///
/// Directories get DIR_N aliases; well-known structural dirs are preserved.
/// Project files (.csproj, .fsproj, .sln, etc.) get PROJ_N aliases.
/// </summary>
public sealed class FileNameMapper
{
    /// <summary>Well-known structural directories that don't leak business context.</summary>
    private static readonly HashSet<string> StructuralDirectories = new(StringComparer.OrdinalIgnoreCase)
    {
        "src", "lib", "test", "tests", "public", "wwwroot", "assets",
        "config", "scripts", "docs", "resources", "Properties"
    };

    /// <summary>Project/solution file extensions that get PROJ_N aliases.</summary>
    private static readonly HashSet<string> ProjectFileExtensions = new(StringComparer.OrdinalIgnoreCase)
    {
        ".csproj", ".fsproj", ".vbproj", ".sln", ".slnx"
    };

    /// <summary>Categories representing primary type declarations for Strategy A naming.</summary>
    private static readonly HashSet<SemanticCategory> PrimaryTypeCategories =
    [
        SemanticCategory.Class,
        SemanticCategory.Interface,
        SemanticCategory.Record,
        SemanticCategory.Enum,
        SemanticCategory.Delegate,
        SemanticCategory.Module,
        SemanticCategory.Program,
        SemanticCategory.Copybook,
        SemanticCategory.Job,
    ];

    /// <summary>Extensions where Strategy A (derive from primary type) is attempted first.</summary>
    private static readonly HashSet<string> StrategyAExtensions = new(StringComparer.OrdinalIgnoreCase)
    {
        ".cs", ".vb", ".fs", ".fsx", ".fsi",   // .NET
        ".cbl", ".cob", ".cpy",                  // COBOL
        ".jcl",                                   // JCL
    };

    private int _fileCounter;
    private int _dirCounter;
    private int _projCounter;
    private readonly Dictionary<string, string> _dirAliasCache = new(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// Builds file path mappings for all files in the output directory.
    /// Populates context.Mappings.FilePathForward and FilePathReverse.
    /// </summary>
    /// <param name="context">The obfuscation context with populated MappingTable and SourceMap.</param>
    /// <param name="allRelativeFiles">All files in the output directory, as relative paths (forward-slash normalized).</param>
    public void BuildMappings(ObfuscationContext context, IReadOnlyList<string> allRelativeFiles)
    {
        var mappings = context.Mappings;
        var usedPaths = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        // Phase 1: Collect all unique directory segment names and assign DIR_N aliases
        BuildDirectoryAliases(allRelativeFiles);

        // Phase 2: Map each file
        foreach (var file in allRelativeFiles)
        {
            var normalized = file.Replace('\\', '/');
            var extension = Path.GetExtension(normalized);
            var stem = Path.GetFileNameWithoutExtension(normalized);
            var dir = Path.GetDirectoryName(normalized)?.Replace('\\', '/') ?? "";

            string newStem;

            if (ProjectFileExtensions.Contains(extension))
            {
                newStem = $"PROJ_{_projCounter++}";
            }
            else if (StrategyAExtensions.Contains(extension))
            {
                // Strategy A: try to derive from primary type alias
                newStem = FindPrimaryTypeAlias(context, normalized, stem)
                          ?? $"FILE_{_fileCounter++}";
            }
            else if (string.IsNullOrEmpty(extension))
            {
                // Extensionless files: try Strategy A first (mainframe files often lack extensions)
                newStem = FindPrimaryTypeAlias(context, normalized, stem)
                          ?? $"FILE_{_fileCounter++}";
            }
            else
            {
                // Strategy B: FILE_N
                newStem = $"FILE_{_fileCounter++}";
            }

            // Map directory path (replace non-structural segments)
            var newDir = MapDirectoryPath(dir);
            var newFileName = $"{newStem}{extension}";
            var newPath = string.IsNullOrEmpty(newDir) ? newFileName : $"{newDir}/{newFileName}";

            // Collision avoidance: if path already used, fall back to FILE_N
            if (!usedPaths.Add(newPath))
            {
                newStem = $"FILE_{_fileCounter++}";
                newFileName = $"{newStem}{extension}";
                newPath = string.IsNullOrEmpty(newDir) ? newFileName : $"{newDir}/{newFileName}";
                usedPaths.Add(newPath);
            }

            mappings.FilePathForward[normalized] = newPath;
            mappings.FilePathReverse[newPath] = normalized;
        }
    }

    private void BuildDirectoryAliases(IReadOnlyList<string> files)
    {
        // Collect all unique directory segment names
        var segmentNames = new SortedSet<string>(StringComparer.OrdinalIgnoreCase);
        foreach (var file in files)
        {
            var dir = Path.GetDirectoryName(file.Replace('\\', '/'))?.Replace('\\', '/');
            if (string.IsNullOrEmpty(dir)) continue;

            foreach (var part in dir.Split('/'))
            {
                if (!StructuralDirectories.Contains(part))
                {
                    segmentNames.Add(part);
                }
            }
        }

        // Assign DIR_N aliases in sorted order for determinism
        foreach (var name in segmentNames)
        {
            if (!_dirAliasCache.ContainsKey(name))
            {
                _dirAliasCache[name] = $"DIR_{_dirCounter++}";
            }
        }
    }

    private string MapDirectoryPath(string dir)
    {
        if (string.IsNullOrEmpty(dir)) return dir;

        var parts = dir.Split('/');
        for (var i = 0; i < parts.Length; i++)
        {
            if (_dirAliasCache.TryGetValue(parts[i], out var alias))
            {
                parts[i] = alias;
            }
        }
        return string.Join("/", parts);
    }

    /// <summary>
    /// Strategy A: Find the alias of the primary type declaration in a file.
    /// First tries to match the file stem to a type name (convention), then falls back
    /// to any primary type in the file.
    /// </summary>
    private static string? FindPrimaryTypeAlias(ObfuscationContext context, string relativeFilePath, string fileStem)
    {
        string? anyTypeAlias = null;

        foreach (var entry in context.SourceMap.Entries.Values)
        {
            if (!PrimaryTypeCategories.Contains(entry.Category))
                continue;

            foreach (var loc in entry.Locations)
            {
                var normalizedLoc = loc.FilePath.Replace('\\', '/');
                if (!PathsMatch(normalizedLoc, relativeFilePath))
                    continue;

                // Prefer entries where the original name matches the file stem
                if (entry.OriginalValue.Equals(fileStem, StringComparison.OrdinalIgnoreCase) ||
                    entry.OriginalValue.Replace("-", "").Equals(fileStem.Replace("-", ""), StringComparison.OrdinalIgnoreCase))
                {
                    return entry.Alias;
                }

                // Track first match as fallback
                anyTypeAlias ??= entry.Alias;
            }
        }

        return anyTypeAlias;
    }

    private static bool PathsMatch(string path1, string path2)
    {
        return path1.Equals(path2, StringComparison.OrdinalIgnoreCase) ||
               path1.EndsWith($"/{path2}", StringComparison.OrdinalIgnoreCase);
    }
}
