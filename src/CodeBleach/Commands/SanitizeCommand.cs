using System.CommandLine;
using System.Diagnostics;
using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using CodeBleach.Core.Rules;
using CodeBleach.Core.Services;
using Microsoft.Extensions.Logging;

namespace CodeBleach.Commands;

public static class SanitizeCommand
{
    public static Command Create()
    {
        var sourceArg = new Argument<DirectoryInfo>("source", "Directory to sanitize");
        var outputOption = new Option<DirectoryInfo?>(
            new[] { "--output", "-o" },
            "Output directory (default: <source>-sanitize)");
        var dryRunOption = new Option<bool>(
            new[] { "--dry-run", "-n" },
            "Preview changes without modifying files");
        var verboseOption = new Option<bool>(
            new[] { "--verbose", "-v" },
            "Show detailed output");
        var forceOption = new Option<bool>(
            new[] { "--force", "-f" },
            "Overwrite existing output directory");
        
        var command = new Command("sanitize", "Sanitize a directory")
        {
            sourceArg,
            outputOption,
            dryRunOption,
            verboseOption,
            forceOption
        };
        
        command.SetHandler(async (source, output, dryRun, verbose, force) =>
        {
            await HandleAsync(source, output, dryRun, verbose, force);
        }, sourceArg, outputOption, dryRunOption, verboseOption, forceOption);
        
        return command;
    }
    
    private static async Task HandleAsync(
        DirectoryInfo source,
        DirectoryInfo? output,
        bool dryRun,
        bool verbose,
        bool force)
    {
        if (!source.Exists)
        {
            Console.Error.WriteLine($"Error: Source directory not found: {source.FullName}");
            Environment.Exit(1);
            return;
        }
        
        var outputPath = output?.FullName ?? $"{source.FullName}-sanitize";
        var outputDir = new DirectoryInfo(outputPath);
        
        if (outputDir.Exists && !force)
        {
            Console.Error.WriteLine($"Error: Output directory already exists: {outputPath}");
            Console.Error.WriteLine("Use --force to overwrite.");
            Environment.Exit(1);
            return;
        }
        
        if (dryRun)
        {
            Console.WriteLine("DRY RUN - No files will be modified");
            Console.WriteLine();
        }
        
        Console.WriteLine($"Sanitizing: {source.FullName}");
        Console.WriteLine($"Output:     {outputPath}");
        Console.WriteLine();
        
        var stopwatch = Stopwatch.StartNew();
        
        // Setup services
        var ruleRegistry = new RuleRegistry();
        foreach (var rule in BuiltInRules.All)
            ruleRegistry.AddRule(rule);
        
        // Load custom rules if present
        var customRulesPath = CustomRuleLoader.FindConfigFile(source.FullName);
        if (customRulesPath != null)
        {
            var customRules = CustomRuleLoader.LoadFromFile(customRulesPath).ToList();
            foreach (var rule in customRules)
            {
                ruleRegistry.AddRule(rule);
            }
            if (verbose)
            {
                Console.WriteLine($"Loaded {customRules.Count} custom rules from: {customRulesPath}");
            }
        }
        
        var sanitizer = new Sanitizer(ruleRegistry);
        var fileProcessor = new FileProcessor();
        var manifestManager = new ManifestManager();
        
        var mappings = new MappingTable();
        var processedFiles = new List<string>();
        var totalFiles = 0;
        var skippedFiles = 0;
        var totalReplacements = 0;
        
        if (!dryRun)
        {
            if (outputDir.Exists)
            {
                outputDir.Delete(recursive: true);
            }
            
            await fileProcessor.CopyDirectoryAsync(source.FullName, outputPath);
        }
        
        var filesToProcess = fileProcessor.GetFilesToProcess(source.FullName).ToList();
        totalFiles = filesToProcess.Count;
        
        foreach (var file in filesToProcess)
        {
            var content = await File.ReadAllTextAsync(file);
            var result = sanitizer.Sanitize(content, mappings);
            
            if (result.WasSanitized)
            {
                totalReplacements += result.Matches.Count;
                var relativePath = Path.GetRelativePath(source.FullName, file);
                processedFiles.Add(relativePath);
                
                if (!dryRun)
                {
                    var destFile = Path.Combine(outputPath, relativePath);
                    await fileProcessor.ProcessFileAsync(file, destFile, _ => result.Content);
                }
            }
            else
            {
                skippedFiles++;
            }
        }
        
        stopwatch.Stop();
        
        if (!dryRun)
        {
            var manifest = new Manifest
            {
                Version = Manifest.CurrentVersion,
                SourcePath = source.FullName,
                DestinationPath = outputPath,
                CreatedAt = DateTime.UtcNow,
                RestoredAt = null,
                Mappings = mappings,
                ProcessedFiles = processedFiles,
                Stats = new SanitizationStats
                {
                    TotalFiles = totalFiles,
                    ProcessedFiles = processedFiles.Count,
                    SkippedFiles = skippedFiles,
                    TotalReplacements = totalReplacements,
                    UniqueValuesReplaced = mappings.Forward.Count,
                    ProcessingTimeMs = stopwatch.ElapsedMilliseconds
                }
            };
            
            await manifestManager.SaveManifestAsync(manifest, outputPath);
            await manifestManager.SaveXrefAsync(manifest, outputPath);
        }
        
        Console.WriteLine("Summary:");
        Console.WriteLine($"  Files copied:     {totalFiles}");
        Console.WriteLine($"  Files processed:  {processedFiles.Count}");
        Console.WriteLine($"  Files skipped:    {skippedFiles}");
        Console.WriteLine($"  Replacements:     {totalReplacements}");
        Console.WriteLine($"  Unique values:    {mappings.Forward.Count}");
        Console.WriteLine();
        Console.WriteLine($"Done in {stopwatch.ElapsedMilliseconds}ms");
        
        if (!dryRun)
        {
            Console.WriteLine($"Output: {outputPath}");
        }
    }
}

