using System.CommandLine;
using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Services;

namespace CodeBleach.Commands;

public static class StatusCommand
{
    public static Command Create()
    {
        var directoryOption = new Option<DirectoryInfo?>(
            new[] { "--directory", "-d" },
            "Directory to check (default: current directory)");
        
        var command = new Command("status", "Show sanitization status")
        {
            directoryOption
        };
        
        command.SetHandler(async (directory) =>
        {
            await HandleAsync(directory);
        }, directoryOption);
        
        return command;
    }
    
    private static async Task HandleAsync(DirectoryInfo? directory)
    {
        var path = directory?.FullName ?? Environment.CurrentDirectory;
        var manifestManager = new ManifestManager();
        
        if (!manifestManager.IsSanitizedDirectory(path))
        {
            Console.WriteLine("CodeBleach Status");
            Console.WriteLine("-----------------");
            Console.WriteLine($"Directory:  {path}");
            Console.WriteLine("Type:       Regular directory (not sanitized)");
            Console.WriteLine();
            Console.WriteLine("Run 'codebleach .' to create a sanitized copy.");
            return;
        }
        
        var manifest = await manifestManager.LoadManifestAsync(path);
        if (manifest == null)
        {
            Console.Error.WriteLine("Error: Manifest file is corrupted or invalid");
            Environment.Exit(1);
            return;
        }
        
        Console.WriteLine("CodeBleach Status");
        Console.WriteLine("-----------------");
        Console.WriteLine($"Directory:  {path}");
        Console.WriteLine("Type:       Sanitized copy");
        Console.WriteLine($"Source:     {manifest.SourcePath}");
        Console.WriteLine($"Created:    {manifest.CreatedAt:yyyy-MM-dd HH:mm:ss}");
        Console.WriteLine($"Restored:   {(manifest.RestoredAt?.ToString("yyyy-MM-dd HH:mm:ss") ?? "Never")}");
        Console.WriteLine();
        Console.WriteLine($"Mappings ({manifest.Mappings.Forward.Count}):");
        
        foreach (var kvp in manifest.Mappings.Forward.OrderBy(k => k.Value).Take(10))
        {
            Console.WriteLine($"  {kvp.Value,-12} <-> {kvp.Key}");
        }
        
        if (manifest.Mappings.Forward.Count > 10)
        {
            Console.WriteLine($"  ... and {manifest.Mappings.Forward.Count - 10} more");
        }
        
        Console.WriteLine();
        Console.WriteLine($"Files processed: {manifest.ProcessedFiles.Count}");
    }
}

