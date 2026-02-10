using System.CommandLine;
using System.Diagnostics;
using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using CodeBleach.Core.Rules;
using CodeBleach.Core.Services;
using CodeBleach.Processors.Cobol;
using CodeBleach.Processors.CSharp;
using CodeBleach.Processors.FSharp;
using CodeBleach.Processors.JavaScript;
using CodeBleach.Processors.Jcl;
using CodeBleach.Processors.OracleSql;
using CodeBleach.Processors.Sql;
using CodeBleach.Processors.VbScript;
using CodeBleach.Processors.MainframeUtility;
using CodeBleach.Processors.VisualBasic;

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
        var rulesOption = new Option<FileInfo?>(
            new[] { "--rules", "-r" },
            "Path to custom rules file (overrides auto-discovery)");
        var levelOption = new Option<int>(
            new[] { "--level", "-l" },
            () => 2,
            "Obfuscation level: 1=sanitize patterns only, 2=full identifier obfuscation (default)");
        var verifyOption = new Option<bool>(
            "--verify",
            "Build the output after obfuscation to verify correctness");

        var command = new Command("sanitize", "Sanitize a directory")
        {
            sourceArg,
            outputOption,
            dryRunOption,
            verboseOption,
            forceOption,
            rulesOption,
            levelOption,
            verifyOption
        };

        command.SetHandler(async (source, output, dryRun, verbose, force, rulesFile, level, verify) =>
        {
            await HandleAsync(source, output, dryRun, verbose, force, rulesFile, level, verify);
        }, sourceArg, outputOption, dryRunOption, verboseOption, forceOption, rulesOption, levelOption, verifyOption);

        return command;
    }

    /// <summary>
    /// Creates and populates the language processor registry with all available processors.
    /// Called only when Level 2 obfuscation is requested.
    /// </summary>
    private static ILanguageProcessorRegistry CreateProcessorRegistry()
    {
        var registry = new LanguageProcessorRegistry();
        registry.Register(new SqlLanguageProcessor());
        registry.Register(new Db2SqlLanguageProcessor());
        registry.Register(new CSharpLanguageProcessor());
        registry.Register(new VisualBasicLanguageProcessor());
        registry.Register(new JavaScriptLanguageProcessor());
        registry.Register(new CobolLanguageProcessor());
        registry.Register(new JclLanguageProcessor());
        registry.Register(new VbScriptLanguageProcessor());
        registry.Register(new OracleSqlLanguageProcessor());
        registry.Register(new FSharpLanguageProcessor());
        registry.Register(new MainframeUtilityLanguageProcessor());
        return registry;
    }

    private static async Task HandleAsync(
        DirectoryInfo source,
        DirectoryInfo? output,
        bool dryRun,
        bool verbose,
        bool force,
        FileInfo? rulesFile,
        int level,
        bool verify)
    {
        if (!source.Exists)
        {
            Console.Error.WriteLine($"Error: Source directory not found: {source.FullName}");
            Environment.Exit(1);
            return;
        }

        var obfuscationLevel = level switch
        {
            1 => ObfuscationLevel.Sanitize,
            2 => ObfuscationLevel.Full,
            _ => ObfuscationLevel.Full
        };

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

        var levelLabel = obfuscationLevel == ObfuscationLevel.Full ? "Level 2 (Full Obfuscation)" : "Level 1 (Sanitize)";
        Console.WriteLine($"Sanitizing: {source.FullName}");
        Console.WriteLine($"Output:     {outputPath}");
        Console.WriteLine($"Level:      {levelLabel}");
        Console.WriteLine();

        var stopwatch = Stopwatch.StartNew();

        // Setup services
        var ruleRegistry = new RuleRegistry();

        // 1. Load built-in rules first (lowest priority)
        foreach (var rule in BuiltInRules.All)
            ruleRegistry.AddRule(rule);

        // 2. Load custom rules from hierarchy (global -> explicit -> project-local)
        var configLocator = new GlobalConfigLocator();
        var configPaths = configLocator.GetConfigFilePaths(
            source.FullName,
            rulesFile?.FullName).ToList();

        if (configPaths.Any())
        {
            var customRules = CustomRuleLoader.LoadFromMultipleFiles(configPaths).ToList();

            foreach (var rule in customRules)
            {
                ruleRegistry.AddRule(rule);
            }

            if (verbose)
            {
                Console.WriteLine($"Loaded {customRules.Count} custom rules from:");
                foreach (var path in configPaths)
                {
                    Console.WriteLine($"  - {path}");
                }
                Console.WriteLine();
            }
        }
        else if (verbose)
        {
            Console.WriteLine("No custom rules loaded (using built-in rules only)");
            Console.WriteLine();
        }

        var sanitizer = new Sanitizer(ruleRegistry);
        var fileProcessor = new FileProcessor();
        var manifestManager = new ManifestManager();

        // Setup obfuscation context (shared across all files and processors)
        var mappings = new MappingTable();
        var context = new ObfuscationContext(obfuscationLevel, mappings);
        var processedFiles = new List<string>();
        var processorsUsed = new HashSet<string>();
        var totalFiles = 0;
        var skippedFiles = 0;
        var totalReplacements = 0;

        // Setup language processor registry for Level 2
        ILanguageProcessorRegistry? processorRegistry = null;
        if (obfuscationLevel == ObfuscationLevel.Full)
        {
            processorRegistry = CreateProcessorRegistry();
            context.ProcessorRegistry = processorRegistry;

            // Prepare batch context for processors that need it (e.g., Roslyn compilation)
            // Include extensionless files for Level 2 (mainframe FTP downloads often lack extensions)
            var filesToBatch = fileProcessor.GetFilesToProcess(source.FullName,
                includeExtensionless: true).ToList();
            foreach (var processor in processorRegistry.GetAll())
            {
                var batchFiles = filesToBatch.Where(f => processor.SupportedExtensions.Contains(Path.GetExtension(f))).ToList();
                if (batchFiles.Count > 0)
                {
                    processor.PrepareBatch(batchFiles, context);
                }
            }
        }

        if (!dryRun)
        {
            if (outputDir.Exists)
            {
                outputDir.Delete(recursive: true);
            }

            await fileProcessor.CopyDirectoryAsync(source.FullName, outputPath);
        }

        var filesToProcess = fileProcessor.GetFilesToProcess(source.FullName,
            includeExtensionless: obfuscationLevel == ObfuscationLevel.Full).ToList();
        totalFiles = filesToProcess.Count;

        foreach (var file in filesToProcess)
        {
            var content = await File.ReadAllTextAsync(file);
            var relativePath = Path.GetRelativePath(source.FullName, file);
            string transformedContent;
            var fileWasProcessed = false;
            var fileReplacements = 0;

            // Level 2: Try language-aware processor first
            if (obfuscationLevel == ObfuscationLevel.Full && processorRegistry != null)
            {
                var processor = processorRegistry.GetProcessor(file, content);
                if (processor != null)
                {
                    var langResult = processor.Obfuscate(content, context, file);
                    transformedContent = langResult.Content;
                    fileReplacements += langResult.ReplacementCount;
                    processorsUsed.Add(processor.ProcessorId);

                    if (langResult.WasTransformed)
                    {
                        fileWasProcessed = true;
                        context.RecordFileProcessing(relativePath, processor.ProcessorId, langResult.ReplacementCount);

                        if (verbose && langResult.Warnings.Count > 0)
                        {
                            foreach (var warning in langResult.Warnings)
                            {
                                Console.WriteLine($"  Warning [{processor.DisplayName}] {relativePath}: {warning}");
                            }
                        }
                    }

                    // Defense-in-depth: run regex sanitizer on AST-processed output
                    var regexResult = sanitizer.Sanitize(transformedContent, mappings);
                    if (regexResult.WasSanitized)
                    {
                        transformedContent = regexResult.Content;
                        fileReplacements += regexResult.Matches.Count;
                        fileWasProcessed = true;
                    }
                }
                else
                {
                    // No language processor -- fall back to Level 1 regex
                    var regexResult = sanitizer.Sanitize(content, mappings);
                    transformedContent = regexResult.Content;
                    fileReplacements = regexResult.Matches.Count;
                    fileWasProcessed = regexResult.WasSanitized;
                }
            }
            else
            {
                // Level 1: regex-only
                var regexResult = sanitizer.Sanitize(content, mappings);
                transformedContent = regexResult.Content;
                fileReplacements = regexResult.Matches.Count;
                fileWasProcessed = regexResult.WasSanitized;
            }

            if (fileWasProcessed)
            {
                totalReplacements += fileReplacements;
                processedFiles.Add(relativePath);

                if (!dryRun)
                {
                    var destFile = Path.Combine(outputPath, relativePath);
                    await fileProcessor.ProcessFileAsync(file, destFile, _ => transformedContent);
                }
            }
            else
            {
                skippedFiles++;
            }
        }

        // Phase 2: Build file name mappings (Level 2 only)
        if (obfuscationLevel == ObfuscationLevel.Full && !dryRun)
        {
            var fileNameMapper = new FileNameMapper();

            // Get all non-ignored files in the output directory
            var allOutputFiles = Directory.EnumerateFiles(outputPath, "*", SearchOption.AllDirectories)
                .Where(f => !fileProcessor.ShouldIgnore(f))
                .Select(f => Path.GetRelativePath(outputPath, f).Replace('\\', '/'))
                .OrderBy(f => f)
                .ToList();

            fileNameMapper.BuildMappings(context, allOutputFiles);

            if (verbose)
            {
                Console.WriteLine($"  File path mappings: {mappings.FilePathForward.Count}");
            }

            // Phase 3: Patch cross-file references (before renaming)
            var patcher = new FileReferencePatcher();
            var patchedFiles = await patcher.PatchReferencesAsync(outputPath, mappings, verbose);

            if (verbose && patchedFiles > 0)
            {
                Console.WriteLine($"  Files patched:      {patchedFiles}");
            }

            // Phase 3.5: Sanitize directory listing files (e.g., DIRECTORY_STRUCTURE.txt)
            // These contain original file/folder names that must be replaced with aliases.
            var txtFiles = Directory.EnumerateFiles(outputPath, "*.txt", SearchOption.AllDirectories)
                .Where(f => !fileProcessor.ShouldIgnore(f))
                .ToList();
            foreach (var txtFile in txtFiles)
            {
                var txtContent = await File.ReadAllTextAsync(txtFile);
                if (txtContent.Contains("Folder PATH listing") || txtContent.Contains("+---") || txtContent.Contains("\\---"))
                {
                    var patched = txtContent;
                    // Apply file path mappings (original name → alias name)
                    foreach (var (original, mapped) in mappings.FilePathForward.OrderByDescending(p => p.Key.Length))
                    {
                        var origName = Path.GetFileName(original);
                        var mappedName = Path.GetFileName(mapped);
                        if (!string.IsNullOrEmpty(origName) && origName != mappedName)
                        {
                            patched = patched.Replace(origName, mappedName, StringComparison.OrdinalIgnoreCase);
                        }
                    }
                    // Also replace directory names
                    foreach (var (original, mapped) in mappings.FilePathForward.OrderByDescending(p => p.Key.Length))
                    {
                        var parts = original.Replace('\\', '/').Split('/');
                        var mappedParts = mapped.Replace('\\', '/').Split('/');
                        for (int idx = 0; idx < parts.Length && idx < mappedParts.Length; idx++)
                        {
                            if (parts[idx] != mappedParts[idx] && !string.IsNullOrEmpty(parts[idx]))
                            {
                                patched = patched.Replace(parts[idx], mappedParts[idx], StringComparison.OrdinalIgnoreCase);
                            }
                        }
                    }
                    if (patched != txtContent)
                    {
                        await File.WriteAllTextAsync(txtFile, patched);
                    }
                }
            }

            // Phase 4: Rename files and directories on disk
            var renamer = new FileSystemRenamer();
            var renamedFiles = renamer.RenameFiles(outputPath, mappings, verbose);

            Console.WriteLine($"  Files renamed:    {renamedFiles}");
        }

        stopwatch.Stop();

        if (!dryRun)
        {
            if (processorsUsed.Count == 0)
            {
                processorsUsed.Add("regex");
            }

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
                },
                Level = obfuscationLevel,
                ProcessorsUsed = processorsUsed.Order().ToList(),
                EnhancedMap = obfuscationLevel == ObfuscationLevel.Full ? context.SourceMap : null,
                FilePathMappings = mappings.FilePathForward.Count > 0 ? new Dictionary<string, string>(mappings.FilePathForward) : null
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
        if (obfuscationLevel == ObfuscationLevel.Full)
        {
            Console.WriteLine($"  Processors used:  {string.Join(", ", processorsUsed.Order())}");
        }
        Console.WriteLine();
        Console.WriteLine($"Done in {stopwatch.ElapsedMilliseconds}ms");

        if (!dryRun)
        {
            Console.WriteLine($"Output: {outputPath}");
        }

        // Build verification
        if (verify && !dryRun)
        {
            Console.WriteLine();
            var verifier = new BuildVerifier();
            var buildSystem = verifier.DetectBuildSystem(outputPath);

            if (buildSystem == BuildVerifier.BuildSystem.Unknown)
            {
                Console.WriteLine("Verify: SKIP - No recognized build system found in output.");
            }
            else
            {
                Console.WriteLine($"Verify: Building output ({VerifyCommand.FormatBuildSystem(buildSystem)})...");
                var buildResult = await verifier.VerifyAsync(outputPath);

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
