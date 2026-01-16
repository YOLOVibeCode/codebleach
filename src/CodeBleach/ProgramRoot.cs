using System.CommandLine;
using CodeBleach.Commands;

namespace CodeBleach;

/// <summary>
/// Root entry point for CodeBleach CLI.
/// </summary>
public static class ProgramRoot
{
    public static async Task<int> InvokeAsync(string[] args)
    {
        var rootCommand = new RootCommand("Sanitize code before sharing with AI assistants")
        {
            SanitizeCommand.Create(),
            RestoreCommand.Create(),
            StatusCommand.Create()
        };
        
        return await rootCommand.InvokeAsync(args);
    }
}
