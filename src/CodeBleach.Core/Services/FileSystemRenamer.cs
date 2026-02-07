using CodeBleach.Core.Models;

namespace CodeBleach.Core.Services;

/// <summary>
/// Renames files and directories in the output directory according to file path mappings.
/// Must be called AFTER FileReferencePatcher has patched all cross-file references.
/// </summary>
public sealed class FileSystemRenamer
{
    /// <summary>
    /// Renames all files and directories in the output directory according to the mappings.
    /// Creates new directory structure, moves files, then removes empty old directories.
    /// </summary>
    /// <param name="outputPath">Root output directory.</param>
    /// <param name="mappings">Mapping table with FilePathForward populated.</param>
    /// <param name="verbose">Whether to print details.</param>
    /// <returns>Number of files renamed.</returns>
    public int RenameFiles(string outputPath, MappingTable mappings, bool verbose = false)
    {
        var forward = mappings.FilePathForward;
        if (forward.Count == 0) return 0;

        var renameCount = 0;

        // Step 1: Create all new directories first
        var newDirs = forward.Values
            .Select(newPath => Path.GetDirectoryName(Path.Combine(outputPath, newPath)))
            .Where(d => d != null)
            .Distinct()
            .OrderBy(d => d);

        foreach (var dir in newDirs)
        {
            Directory.CreateDirectory(dir!);
        }

        // Step 2: Move files to new locations
        foreach (var (originalRelative, newRelative) in forward)
        {
            if (originalRelative == newRelative) continue;

            var originalFull = Path.Combine(outputPath, originalRelative);
            var newFull = Path.Combine(outputPath, newRelative);

            if (!File.Exists(originalFull)) continue;

            // Ensure the target directory exists
            var targetDir = Path.GetDirectoryName(newFull);
            if (targetDir != null) Directory.CreateDirectory(targetDir);

            File.Move(originalFull, newFull, overwrite: true);
            renameCount++;

            if (verbose)
            {
                Console.WriteLine($"  Renamed: {originalRelative} → {newRelative}");
            }
        }

        // Step 3: Clean up empty directories (bottom-up)
        CleanEmptyDirectories(outputPath);

        return renameCount;
    }

    /// <summary>
    /// Reverses file renames: moves files from obfuscated paths back to original paths.
    /// Used during restore before content de-obfuscation.
    /// </summary>
    /// <param name="directory">Directory containing obfuscated file names.</param>
    /// <param name="filePathMappings">Original → obfuscated path mappings from the manifest.</param>
    /// <param name="verbose">Whether to print details.</param>
    /// <returns>Number of files renamed.</returns>
    public int ReverseRenameFiles(string directory, Dictionary<string, string> filePathMappings, bool verbose = false)
    {
        if (filePathMappings.Count == 0) return 0;

        var renameCount = 0;

        // Build reverse: obfuscated → original
        var reverse = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        foreach (var (original, obfuscated) in filePathMappings)
        {
            reverse[obfuscated] = original;
        }

        // Step 1: Create all original directories first
        var originalDirs = reverse.Values
            .Select(origPath => Path.GetDirectoryName(Path.Combine(directory, origPath)))
            .Where(d => d != null)
            .Distinct()
            .OrderBy(d => d);

        foreach (var dir in originalDirs)
        {
            Directory.CreateDirectory(dir!);
        }

        // Step 2: Move files back to original locations
        foreach (var (obfuscatedRelative, originalRelative) in reverse)
        {
            if (obfuscatedRelative == originalRelative) continue;

            var obfuscatedFull = Path.Combine(directory, obfuscatedRelative);
            var originalFull = Path.Combine(directory, originalRelative);

            if (!File.Exists(obfuscatedFull)) continue;

            var targetDir = Path.GetDirectoryName(originalFull);
            if (targetDir != null) Directory.CreateDirectory(targetDir);

            File.Move(obfuscatedFull, originalFull, overwrite: true);
            renameCount++;

            if (verbose)
            {
                Console.WriteLine($"  Restored: {obfuscatedRelative} → {originalRelative}");
            }
        }

        // Step 3: Clean up empty directories
        CleanEmptyDirectories(directory);

        return renameCount;
    }

    private static void CleanEmptyDirectories(string rootPath)
    {
        // Process subdirectories bottom-up (deepest first)
        foreach (var dir in Directory.EnumerateDirectories(rootPath, "*", SearchOption.AllDirectories)
            .OrderByDescending(d => d.Length))
        {
            try
            {
                if (Directory.Exists(dir) && !Directory.EnumerateFileSystemEntries(dir).Any())
                {
                    // Don't delete .codebleach directory
                    if (!dir.Contains(".codebleach"))
                    {
                        Directory.Delete(dir);
                    }
                }
            }
            catch
            {
                // Ignore errors during cleanup
            }
        }
    }
}
