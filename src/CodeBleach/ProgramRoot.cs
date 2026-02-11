using System.CommandLine;
using CodeBleach.Commands;

namespace CodeBleach;

/// <summary>
/// Root entry point for CodeBleach CLI.
/// </summary>
public static class ProgramRoot
{
    private static readonly HashSet<string> KnownCommands = new(StringComparer.OrdinalIgnoreCase)
    {
        "sanitize", "restore", "verify", "status", "init", "config"
    };

    public static async Task<int> InvokeAsync(string[] args)
    {
        // If first arg looks like a path (not a subcommand or flag), default to "sanitize"
        args = PreprocessArgs(args);

        var rootCommand = new RootCommand(
            "CodeBleach — Industrial-grade code obfuscation for AI-safe sharing.\n\n" +
            "Quick Recipes:\n" +
            "  codebleach .                                 Obfuscate current directory (Level 2)\n" +
            "  codebleach sanitize ./src                    Obfuscate ./src directory\n" +
            "  codebleach sanitize ./src --scope database   Only obfuscate SQL/database code\n" +
            "  codebleach sanitize ./src --scope mainframe  Only obfuscate COBOL/JCL code\n" +
            "  codebleach sanitize ./src --level 1          Regex-only sanitization (fast)\n" +
            "  codebleach sanitize ./src --dry-run          Preview without modifying files\n" +
            "  codebleach sanitize ./src --verify           Build after obfuscation to verify\n" +
            "  codebleach restore                           Restore from manifest in current dir\n\n" +
            "Scope Groups:\n" +
            "  database   = tsql, db2sql, oraclesql\n" +
            "  mainframe  = cobol, jcl, mainframe-utility\n" +
            "  dotnet     = csharp, vbnet, fsharp\n" +
            "  web        = javascript\n" +
            "  scripting  = vbscript")
        {
            SanitizeCommand.Create(),
            RestoreCommand.Create(),
            VerifyCommand.Create(),
            StatusCommand.Create(),
            InitCommand.Create(),
            ConfigCommand.Create()
        };

        return await rootCommand.InvokeAsync(args);
    }

    /// <summary>
    /// If the first argument looks like a directory path rather than a known subcommand,
    /// prepend "sanitize" so that <c>codebleach .</c> works as <c>codebleach sanitize .</c>.
    /// </summary>
    private static string[] PreprocessArgs(string[] args)
    {
        if (args.Length == 0)
            return args;

        var firstArg = args[0];

        // Already a known subcommand or a flag — leave as-is
        if (KnownCommands.Contains(firstArg) || firstArg.StartsWith('-'))
            return args;

        // Looks like a path — prepend "sanitize"
        return ["sanitize", .. args];
    }
}
