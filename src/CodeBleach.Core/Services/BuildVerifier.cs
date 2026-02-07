using System.Diagnostics;

namespace CodeBleach.Core.Services;

/// <summary>
/// Detects build systems and executes builds to verify obfuscated/restored code compiles correctly.
/// </summary>
public sealed class BuildVerifier
{
    /// <summary>
    /// Detected build system type.
    /// </summary>
    public enum BuildSystem
    {
        Unknown,
        DotNetSolution,
        DotNetProject,
        TypeScript,
        JavaScript,
        Make
    }

    /// <summary>
    /// Result of a build verification attempt.
    /// </summary>
    public record BuildVerificationResult
    {
        public required bool Success { get; init; }
        public required BuildSystem DetectedBuildSystem { get; init; }
        public required string BuildCommand { get; init; }
        public required string Output { get; init; }
        public required string ErrorOutput { get; init; }
        public required int ExitCode { get; init; }
        public required long ElapsedMs { get; init; }
    }

    /// <summary>
    /// Detects the build system used in the given directory.
    /// </summary>
    public BuildSystem DetectBuildSystem(string directoryPath)
    {
        // .NET solution file (highest priority for .NET projects)
        if (Directory.EnumerateFiles(directoryPath, "*.sln", SearchOption.TopDirectoryOnly).Any() ||
            Directory.EnumerateFiles(directoryPath, "*.slnx", SearchOption.TopDirectoryOnly).Any())
        {
            return BuildSystem.DotNetSolution;
        }

        // .NET project file (no solution)
        if (Directory.EnumerateFiles(directoryPath, "*.csproj", SearchOption.AllDirectories).Any() ||
            Directory.EnumerateFiles(directoryPath, "*.fsproj", SearchOption.AllDirectories).Any() ||
            Directory.EnumerateFiles(directoryPath, "*.vbproj", SearchOption.AllDirectories).Any())
        {
            return BuildSystem.DotNetProject;
        }

        // TypeScript (package.json + tsconfig.json)
        if (File.Exists(Path.Combine(directoryPath, "tsconfig.json")))
        {
            return BuildSystem.TypeScript;
        }

        // JavaScript (package.json without tsconfig)
        if (File.Exists(Path.Combine(directoryPath, "package.json")))
        {
            return BuildSystem.JavaScript;
        }

        // Makefile
        if (File.Exists(Path.Combine(directoryPath, "Makefile")) ||
            File.Exists(Path.Combine(directoryPath, "makefile")))
        {
            return BuildSystem.Make;
        }

        return BuildSystem.Unknown;
    }

    /// <summary>
    /// Gets the build command for the detected build system.
    /// </summary>
    public (string Command, string Arguments) GetBuildCommand(BuildSystem buildSystem, string directoryPath)
    {
        return buildSystem switch
        {
            BuildSystem.DotNetSolution => ("dotnet", "build --no-restore --nologo"),
            BuildSystem.DotNetProject => ("dotnet", "build --no-restore --nologo"),
            BuildSystem.TypeScript => ("npx", "tsc --noEmit"),
            BuildSystem.JavaScript => ("node", "--check ."),
            BuildSystem.Make => ("make", ""),
            _ => ("", "")
        };
    }

    /// <summary>
    /// Runs the build verification against the given directory.
    /// </summary>
    public async Task<BuildVerificationResult> VerifyAsync(string directoryPath, CancellationToken ct = default)
    {
        var buildSystem = DetectBuildSystem(directoryPath);

        if (buildSystem == BuildSystem.Unknown)
        {
            return new BuildVerificationResult
            {
                Success = false,
                DetectedBuildSystem = BuildSystem.Unknown,
                BuildCommand = "",
                Output = "",
                ErrorOutput = "No recognized build system found. Supported: .NET (sln/csproj/fsproj/vbproj), TypeScript (tsconfig.json), Makefile.",
                ExitCode = -1,
                ElapsedMs = 0
            };
        }

        var (command, arguments) = GetBuildCommand(buildSystem, directoryPath);
        var stopwatch = Stopwatch.StartNew();

        try
        {
            using var process = new Process();
            process.StartInfo = new ProcessStartInfo
            {
                FileName = command,
                Arguments = arguments,
                WorkingDirectory = directoryPath,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                CreateNoWindow = true
            };

            process.Start();

            var outputTask = process.StandardOutput.ReadToEndAsync(ct);
            var errorTask = process.StandardError.ReadToEndAsync(ct);

            await process.WaitForExitAsync(ct);
            stopwatch.Stop();

            var output = await outputTask;
            var errorOutput = await errorTask;

            return new BuildVerificationResult
            {
                Success = process.ExitCode == 0,
                DetectedBuildSystem = buildSystem,
                BuildCommand = $"{command} {arguments}",
                Output = output,
                ErrorOutput = errorOutput,
                ExitCode = process.ExitCode,
                ElapsedMs = stopwatch.ElapsedMilliseconds
            };
        }
        catch (Exception ex)
        {
            stopwatch.Stop();
            return new BuildVerificationResult
            {
                Success = false,
                DetectedBuildSystem = buildSystem,
                BuildCommand = $"{command} {arguments}",
                Output = "",
                ErrorOutput = $"Failed to execute build command: {ex.Message}",
                ExitCode = -1,
                ElapsedMs = stopwatch.ElapsedMilliseconds
            };
        }
    }
}
