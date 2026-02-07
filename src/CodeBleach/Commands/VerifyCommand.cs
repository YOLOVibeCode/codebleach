using System.CommandLine;
using CodeBleach.Core.Services;

namespace CodeBleach.Commands;

public static class VerifyCommand
{
    public static Command Create()
    {
        var directoryArg = new Argument<DirectoryInfo>(
            "directory",
            () => new DirectoryInfo(Environment.CurrentDirectory),
            "Directory to verify (default: current directory)");
        var verboseOption = new Option<bool>(
            new[] { "--verbose", "-v" },
            "Show full build output");

        var command = new Command("verify", "Verify that a directory compiles successfully")
        {
            directoryArg,
            verboseOption
        };

        command.SetHandler(async (directory, verbose) =>
        {
            await HandleAsync(directory, verbose);
        }, directoryArg, verboseOption);

        return command;
    }

    private static async Task HandleAsync(DirectoryInfo directory, bool verbose)
    {
        if (!directory.Exists)
        {
            Console.Error.WriteLine($"Error: Directory not found: {directory.FullName}");
            Environment.Exit(1);
            return;
        }

        var verifier = new BuildVerifier();
        var buildSystem = verifier.DetectBuildSystem(directory.FullName);

        Console.WriteLine($"Directory: {directory.FullName}");
        Console.WriteLine($"Build system: {FormatBuildSystem(buildSystem)}");

        if (buildSystem == BuildVerifier.BuildSystem.Unknown)
        {
            Console.Error.WriteLine("Error: No recognized build system found.");
            Console.Error.WriteLine("Supported: .NET (sln/csproj/fsproj/vbproj), TypeScript (tsconfig.json), Makefile.");
            Environment.Exit(1);
            return;
        }

        Console.WriteLine();
        Console.WriteLine("Building...");

        var result = await verifier.VerifyAsync(directory.FullName);

        if (verbose || !result.Success)
        {
            if (!string.IsNullOrWhiteSpace(result.Output))
            {
                Console.WriteLine(result.Output.TrimEnd());
            }

            if (!string.IsNullOrWhiteSpace(result.ErrorOutput))
            {
                Console.Error.WriteLine(result.ErrorOutput.TrimEnd());
            }
        }

        Console.WriteLine();
        if (result.Success)
        {
            Console.WriteLine($"PASS - Build succeeded ({result.ElapsedMs}ms)");
        }
        else
        {
            Console.WriteLine($"FAIL - Build failed (exit code {result.ExitCode}, {result.ElapsedMs}ms)");
            Environment.Exit(1);
        }
    }

    internal static string FormatBuildSystem(BuildVerifier.BuildSystem buildSystem)
    {
        return buildSystem switch
        {
            BuildVerifier.BuildSystem.DotNetSolution => ".NET Solution (dotnet build)",
            BuildVerifier.BuildSystem.DotNetProject => ".NET Project (dotnet build)",
            BuildVerifier.BuildSystem.TypeScript => "TypeScript (tsc --noEmit)",
            BuildVerifier.BuildSystem.JavaScript => "JavaScript (node --check)",
            BuildVerifier.BuildSystem.Make => "Make",
            _ => "Unknown"
        };
    }
}
