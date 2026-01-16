using System.CommandLine;
using System.Diagnostics;
using CodeBleach.Core.Interfaces;
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
        
        var command = new Command("restore", "Restore sanitized directory")
        {
            writebackOption,
            yesOption,
            verboseOption,
            dryRunOption
        };
        
        command.SetHandler(async (writeback, yes, verbose, dryRun) =>
        {
            await HandleAsync(writeback, yes, verbose, dryRun);
        }, writebackOption, yesOption, verboseOption, dryRunOption);
        
        return command;
    }
    
    private static async Task HandleAsync(bool writeback, bool yes, bool verbose, bool dryRun)
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
    }
}

