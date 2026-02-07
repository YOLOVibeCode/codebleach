using System.CommandLine;
using System.Diagnostics;
using CodeBleach.Core.Services;

namespace CodeBleach.Commands;

public static class RestoreCommand
{
    public static Command Create()
    {
        var writebackOption = new Option<bool>(
            new[] { "--writeback", "-w" },
            "Write restored files back to original location");
        var yesOption = new Option<bool>(
            new[] { "--yes", "-y" },
            "Skip confirmation prompts");
        var verboseOption = new Option<bool>(
            new[] { "--verbose", "-v" },
            "Show detailed output");
        var dryRunOption = new Option<bool>(
            new[] { "--dry-run", "-n" },
            "Show what would be done");
        var verifyOption = new Option<bool>(
            "--verify",
            "Build the output after restore to verify correctness");

        var command = new Command("restore", "Restore sanitized directory")
        {
            writebackOption,
            yesOption,
            verboseOption,
            dryRunOption,
            verifyOption
        };

        command.SetHandler(async (writeback, yes, verbose, dryRun, verify) =>
        {
            await HandleAsync(writeback, yes, verbose, dryRun, verify);
        }, writebackOption, yesOption, verboseOption, dryRunOption, verifyOption);
        
        return command;
    }
    
    private static async Task HandleAsync(bool writeback, bool yes, bool verbose, bool dryRun, bool verify)
    {
        var currentDir = Environment.CurrentDirectory;
        var manifestManager = new ManifestManager();
        
        if (!manifestManager.IsSanitizedDirectory(currentDir))
        {
            Console.Error.WriteLine("Error: Not a sanitized directory (no .codebleach/ directory found)");
            Console.Error.WriteLine("Run 'codebleach <path>' to sanitize a directory first.");
            Environment.Exit(1);
            return;
        }
        
        var manifest = await manifestManager.LoadManifestAsync(currentDir);
        if (manifest == null)
        {
            Console.Error.WriteLine("Error: Manifest file is corrupted or invalid");
            Environment.Exit(1);
            return;
        }
        
        if (dryRun)
        {
            Console.WriteLine("DRY RUN - No files will be modified");
            Console.WriteLine();
        }
        
        Console.WriteLine($"Restoring: {currentDir}");
        Console.WriteLine($"Source:    {manifest.SourcePath}");
        Console.WriteLine();
        
        var stopwatch = Stopwatch.StartNew();
        var restorer = new Restorer();
        var fileProcessor = new FileProcessor();
        var processedCount = 0;
        var totalReplacements = 0;

        // Phase 5: Reverse file renames (if file path mappings exist)
        if (manifest.FilePathMappings is { Count: > 0 } && !dryRun)
        {
            var renamer = new FileSystemRenamer();
            var reversedCount = renamer.ReverseRenameFiles(currentDir, manifest.FilePathMappings, verbose);
            Console.WriteLine($"  Files un-renamed: {reversedCount}");
        }

        foreach (var file in manifest.ProcessedFiles)
        {
            var filePath = Path.Combine(currentDir, file);
            if (!File.Exists(filePath))
            {
                continue;
            }
            
            var content = await File.ReadAllTextAsync(filePath);
            var result = restorer.Restore(content, manifest.Mappings);
            
            if (result.WasRestored)
            {
                processedCount++;
                totalReplacements += result.ReplacementCount;
                
                if (!dryRun)
                {
                    if (writeback)
                    {
                        var originalPath = Path.Combine(manifest.SourcePath, file);
                        var destDir = Path.GetDirectoryName(originalPath);
                        if (destDir != null)
                        {
                            Directory.CreateDirectory(destDir);
                        }
                        await File.WriteAllTextAsync(originalPath, result.Content);
                    }
                    else
                    {
                        await File.WriteAllTextAsync(filePath, result.Content);
                    }
                }
            }
        }
        
        stopwatch.Stop();
        
        Console.WriteLine("Summary:");
        Console.WriteLine($"  Files processed:  {processedCount}");
        Console.WriteLine($"  Replacements:     {totalReplacements}");
        Console.WriteLine();
        Console.WriteLine($"Done in {stopwatch.ElapsedMilliseconds}ms");

        // Build verification
        if (verify && !dryRun)
        {
            var verifyDir = writeback ? manifest.SourcePath : currentDir;
            Console.WriteLine();
            var verifier = new BuildVerifier();
            var buildSystem = verifier.DetectBuildSystem(verifyDir);

            if (buildSystem == BuildVerifier.BuildSystem.Unknown)
            {
                Console.WriteLine("Verify: SKIP - No recognized build system found.");
            }
            else
            {
                Console.WriteLine($"Verify: Building restored output ({VerifyCommand.FormatBuildSystem(buildSystem)})...");
                var buildResult = await verifier.VerifyAsync(verifyDir);

                if (verbose || !buildResult.Success)
                {
                    if (!string.IsNullOrWhiteSpace(buildResult.Output))
                        Console.WriteLine(buildResult.Output.TrimEnd());
                    if (!string.IsNullOrWhiteSpace(buildResult.ErrorOutput))
                        Console.Error.WriteLine(buildResult.ErrorOutput.TrimEnd());
                }

                Console.WriteLine();
                if (buildResult.Success)
                {
                    Console.WriteLine($"Verify: PASS - Build succeeded ({buildResult.ElapsedMs}ms)");
                }
                else
                {
                    Console.WriteLine($"Verify: FAIL - Build failed (exit code {buildResult.ExitCode}, {buildResult.ElapsedMs}ms)");
                    Environment.Exit(1);
                }
            }
        }
    }
}

