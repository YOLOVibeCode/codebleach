using System.CommandLine;
using CodeBleach.Core.Services;

namespace CodeBleach.Commands;

public static class ConfigCommand
{
    public static Command Create()
    {
        var listOption = new Option<bool>(
            new[] { "--list", "-l" },
            "List all configuration file locations");
        
        var pathOption = new Option<bool>(
            new[] { "--path", "-p" },
            "Show global configuration directory path");
        
        var command = new Command("config", "Manage CodeBleach configuration")
        {
            listOption,
            pathOption
        };
        
        command.SetHandler((list, path) =>
        {
            if (list)
            {
                ShowList();
            }
            else if (path)
            {
                ShowPath();
            }
            else
            {
                // Default: show path
                ShowPath();
            }
        }, listOption, pathOption);
        
        return command;
    }
    
    private static void ShowPath()
    {
        var locator = new GlobalConfigLocator();
        var configDir = locator.GetGlobalConfigDirectory();
        var rulesFile = locator.GetGlobalRulesFilePath();
        
        Console.WriteLine("Global Configuration");
        Console.WriteLine("====================");
        Console.WriteLine();
        Console.WriteLine($"Config directory: {configDir}");
        Console.WriteLine($"Rules file:       {rulesFile}");
        Console.WriteLine();
        Console.WriteLine($"Exists: {(locator.GlobalRulesFileExists() ? "Yes" : "No")}");
        
        if (!locator.GlobalRulesFileExists())
        {
            Console.WriteLine();
            Console.WriteLine("To create a global configuration:");
            Console.WriteLine("  codebleach init --global");
            Console.WriteLine();
            Console.WriteLine("To create with SQL-focused rules:");
            Console.WriteLine("  codebleach init --global --sql");
        }
    }
    
    private static void ShowList()
    {
        var locator = new GlobalConfigLocator();
        var currentDir = Directory.GetCurrentDirectory();
        var configPaths = locator.GetConfigFilePaths(currentDir).ToList();
        
        Console.WriteLine("Configuration Files");
        Console.WriteLine("===================");
        Console.WriteLine();
        Console.WriteLine("Files are loaded in priority order (lowest to highest):");
        Console.WriteLine();
        
        if (!configPaths.Any())
        {
            Console.WriteLine("  (none found)");
            Console.WriteLine();
            Console.WriteLine("Using built-in rules only.");
            Console.WriteLine();
            Console.WriteLine("To create configuration:");
            Console.WriteLine("  codebleach init           # Create project-local config");
            Console.WriteLine("  codebleach init --global  # Create global user config");
            return;
        }
        
        for (int i = 0; i < configPaths.Count; i++)
        {
            var path = configPaths[i];
            var type = DetermineConfigType(path, locator, currentDir);
            Console.WriteLine($"  {i + 1}. {path}");
            Console.WriteLine($"     ({type})");
            Console.WriteLine();
        }
        
        Console.WriteLine("Note: Later files override earlier files for rules with the same ruleId.");
    }
    
    private static string DetermineConfigType(string path, GlobalConfigLocator locator, string currentDir)
    {
        var globalPath = locator.GetGlobalRulesFilePath();
        
        if (Path.GetFullPath(path) == Path.GetFullPath(globalPath))
        {
            return "global user config";
        }
        
        if (Path.GetDirectoryName(path) == currentDir)
        {
            return "project-local config";
        }
        
        return "inherited from parent directory";
    }
}

