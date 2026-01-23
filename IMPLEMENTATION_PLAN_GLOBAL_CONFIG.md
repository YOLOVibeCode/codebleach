# Implementation Plan: Multi-Level Configuration System

**Feature:** Global and hierarchical rule configuration  
**Author:** Software Architect  
**Approved:** 2026-01-15  
**Target Version:** 1.3.0

---

## Overview

Implement a multi-level configuration system that loads rules from multiple sources in priority order:

```
Built-in → Global User Config → CLI Option → Project Local
(lowest)                                        (highest)
```

---

## Phase 1: CLI Option for Explicit Rules File

### 1.1 Update SanitizeCommand

**File:** `src/CodeBleach/Commands/SanitizeCommand.cs`

**Changes:**
```csharp
// Add option
var rulesOption = new Option<FileInfo?>(
    new[] { "--rules", "-r" },
    "Path to custom rules file (overrides auto-discovery)");

// Add to command
command.Add(rulesOption);

// Update handler signature
command.SetHandler(async (source, output, dryRun, verbose, force, rulesFile) =>
{
    await HandleAsync(source, output, dryRun, verbose, force, rulesFile);
}, sourceArg, outputOption, dryRunOption, verboseOption, forceOption, rulesOption);

// In HandleAsync:
private static async Task HandleAsync(
    DirectoryInfo source,
    DirectoryInfo? output,
    bool dryRun,
    bool verbose,
    bool force,
    FileInfo? rulesFile)  // NEW parameter
{
    // ... existing setup ...
    
    // REPLACE rule loading logic:
    string? customRulesPath = rulesFile?.FullName 
        ?? CustomRuleLoader.FindConfigFile(source.FullName);
    
    if (customRulesPath != null)
    {
        if (!File.Exists(customRulesPath))
        {
            Console.Error.WriteLine($"Error: Rules file not found: {customRulesPath}");
            Environment.Exit(1);
            return;
        }
        
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
}
```

**Tests:**
- [ ] `SanitizeCommandTests.cs`: Test `--rules` option works
- [ ] Verify error when file doesn't exist
- [ ] Verify verbose output shows rules file path

---

## Phase 2: Global Configuration Discovery

### 2.1 Create GlobalConfigLocator Interface

**File:** `src/CodeBleach.Core/Interfaces/IGlobalConfigLocator.cs`

```csharp
namespace CodeBleach.Core.Interfaces;

/// <summary>
/// Locates global and user-level configuration files.
/// </summary>
public interface IGlobalConfigLocator
{
    /// <summary>
    /// Gets the primary global configuration directory path for the current platform.
    /// </summary>
    /// <returns>Full path to global config directory (e.g., ~/.config/codebleach)</returns>
    string GetGlobalConfigDirectory();
    
    /// <summary>
    /// Gets the full path to the global rules file.
    /// </summary>
    /// <returns>Full path to global rules.json</returns>
    string GetGlobalRulesFilePath();
    
    /// <summary>
    /// Checks if a global rules file exists.
    /// </summary>
    bool GlobalRulesFileExists();
    
    /// <summary>
    /// Gets all configuration file paths in priority order (lowest to highest).
    /// Returns paths that exist.
    /// </summary>
    /// <param name="projectPath">Starting project path for local config discovery</param>
    /// <param name="explicitRulesPath">Optional explicit rules file from --rules option</param>
    /// <returns>List of config file paths to load, in order</returns>
    IEnumerable<string> GetConfigFilePaths(string projectPath, string? explicitRulesPath = null);
}
```

### 2.2 Implement GlobalConfigLocator Service

**File:** `src/CodeBleach.Core/Services/GlobalConfigLocator.cs`

```csharp
namespace CodeBleach.Core.Services;

/// <summary>
/// Cross-platform configuration file locator.
/// </summary>
public sealed class GlobalConfigLocator : IGlobalConfigLocator
{
    private const string ConfigDirName = "codebleach";
    private const string RulesFileName = "rules.json";
    
    public string GetGlobalConfigDirectory()
    {
        // Check environment variable first
        var envConfigDir = Environment.GetEnvironmentVariable("CODEBLEACH_CONFIG_DIR");
        if (!string.IsNullOrEmpty(envConfigDir))
        {
            return envConfigDir;
        }
        
        // Platform-specific defaults
        if (OperatingSystem.IsWindows())
        {
            var appData = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData);
            return Path.Combine(appData, ConfigDirName);
        }
        else // Linux/macOS
        {
            var home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
            return Path.Combine(home, ".config", ConfigDirName);
        }
    }
    
    public string GetGlobalRulesFilePath()
    {
        return Path.Combine(GetGlobalConfigDirectory(), RulesFileName);
    }
    
    public bool GlobalRulesFileExists()
    {
        return File.Exists(GetGlobalRulesFilePath());
    }
    
    public IEnumerable<string> GetConfigFilePaths(string projectPath, string? explicitRulesPath = null)
    {
        var paths = new List<string>();
        
        // 1. Global user config (if exists)
        var globalPath = GetGlobalRulesFilePath();
        if (File.Exists(globalPath))
        {
            paths.Add(globalPath);
        }
        
        // 2. Explicit --rules option (if provided and exists)
        if (!string.IsNullOrEmpty(explicitRulesPath) && File.Exists(explicitRulesPath))
        {
            paths.Add(explicitRulesPath);
        }
        
        // 3. Project-local config (if exists)
        var projectLocalPath = CustomRuleLoader.FindConfigFile(projectPath);
        if (projectLocalPath != null && File.Exists(projectLocalPath))
        {
            // Only add if not already in list (avoid duplicates)
            if (!paths.Contains(projectLocalPath, StringComparer.OrdinalIgnoreCase))
            {
                paths.Add(projectLocalPath);
            }
        }
        
        return paths;
    }
}
```

**Tests:**
- [ ] `GlobalConfigLocatorTests.cs`: Test Windows path resolution
- [ ] Test Linux/macOS path resolution
- [ ] Test environment variable override
- [ ] Test GetConfigFilePaths returns correct order

---

### 2.3 Update CustomRuleLoader to Support Multiple Files

**File:** `src/CodeBleach.Core/Services/CustomRuleLoader.cs`

Add new method:

```csharp
/// <summary>
/// Loads and merges rules from multiple configuration files.
/// Later files override earlier files if same ruleId.
/// </summary>
public static IEnumerable<SanitizationRule> LoadFromMultipleFiles(IEnumerable<string> configPaths)
{
    var rulesById = new Dictionary<string, SanitizationRule>(StringComparer.OrdinalIgnoreCase);
    
    foreach (var configPath in configPaths)
    {
        var rules = LoadFromFile(configPath);
        
        foreach (var rule in rules)
        {
            // Later files override earlier files (by ruleId)
            rulesById[rule.RuleId] = rule;
        }
    }
    
    return rulesById.Values.Where(r => r.Enabled);
}
```

**Tests:**
- [ ] `CustomRuleLoaderTests.cs`: Test loading from multiple files
- [ ] Test rule override by ruleId
- [ ] Test disabled rules are filtered out
- [ ] Test empty list returns empty

---

### 2.4 Update SanitizeCommand to Use New System

**File:** `src/CodeBleach/Commands/SanitizeCommand.cs`

```csharp
private static async Task HandleAsync(
    DirectoryInfo source,
    DirectoryInfo? output,
    bool dryRun,
    bool verbose,
    bool force,
    FileInfo? rulesFile)
{
    // ... existing setup ...
    
    // Setup services
    var ruleRegistry = new RuleRegistry();
    
    // 1. Load built-in rules first
    foreach (var rule in BuiltInRules.All)
        ruleRegistry.AddRule(rule);
    
    // 2. Load custom rules from hierarchy
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
        }
    }
    else if (verbose)
    {
        Console.WriteLine("No custom rules loaded (using built-in rules only)");
    }
    
    // ... rest of implementation ...
}
```

---

## Phase 3: Config Management Commands

### 3.1 Create InitCommand

**File:** `src/CodeBleach/Commands/InitCommand.cs`

```csharp
using System.CommandLine;
using CodeBleach.Core.Services;

namespace CodeBleach.Commands;

public static class InitCommand
{
    public static Command Create()
    {
        var globalOption = new Option<bool>(
            new[] { "--global", "-g" },
            "Create global user configuration");
        
        var sqlOption = new Option<bool>(
            "--sql",
            "Include SQL-focused rules (databases, tables, schemas)");
        
        var forceOption = new Option<bool>(
            new[] { "--force", "-f" },
            "Overwrite existing configuration file");
        
        var command = new Command("init", "Initialize CodeBleach configuration")
        {
            globalOption,
            sqlOption,
            forceOption
        };
        
        command.SetHandler(async (global, sql, force) =>
        {
            await HandleAsync(global, sql, force);
        }, globalOption, sqlOption, forceOption);
        
        return command;
    }
    
    private static async Task HandleAsync(bool global, bool sql, bool force)
    {
        string targetPath;
        
        if (global)
        {
            var locator = new GlobalConfigLocator();
            var configDir = locator.GetGlobalConfigDirectory();
            
            // Ensure directory exists
            Directory.CreateDirectory(configDir);
            
            targetPath = locator.GetGlobalRulesFilePath();
        }
        else
        {
            targetPath = Path.Combine(Directory.GetCurrentDirectory(), ".codebleach-rules.json");
        }
        
        if (File.Exists(targetPath) && !force)
        {
            Console.Error.WriteLine($"Error: Configuration file already exists: {targetPath}");
            Console.Error.WriteLine("Use --force to overwrite.");
            Environment.Exit(1);
            return;
        }
        
        // Create example configuration
        var exampleConfig = sql 
            ? GetSqlExampleConfig() 
            : GetBasicExampleConfig();
        
        await File.WriteAllTextAsync(targetPath, exampleConfig);
        
        Console.WriteLine($"Created configuration file: {targetPath}");
        Console.WriteLine();
        Console.WriteLine("Edit this file to customize your sanitization rules.");
        Console.WriteLine($"Documentation: https://github.com/YOLOVibeCode/codebleach#custom-rules");
    }
    
    private static string GetBasicExampleConfig()
    {
        return """
        {
          "rules": [
            {
              "ruleId": "custom_example",
              "name": "Custom Example Rule",
              "description": "Example custom rule - replace with your patterns",
              "type": "regex",
              "pattern": "\\bEXAMPLE_PATTERN\\b",
              "prefix": "CUSTOM",
              "severity": "Medium",
              "enabled": false,
              "order": 100
            }
          ]
        }
        """;
    }
    
    private static string GetSqlExampleConfig()
    {
        return """
        {
          "rules": [
            {
              "ruleId": "sql_database_names",
              "name": "SQL Database Names",
              "description": "Detects database names in FROM clauses",
              "type": "regex",
              "pattern": "(?i)(?<=FROM\\s+)[A-Za-z_][A-Za-z0-9_]*(?=\\.)",
              "prefix": "DATABASE",
              "severity": "High",
              "order": 5
            },
            {
              "ruleId": "sql_schema_qualified",
              "name": "Schema-Qualified Tables",
              "description": "Detects schema.table patterns (dbo.TableName)",
              "type": "regex",
              "pattern": "(?i)\\b(dbo|staging|archive|etl)\\.[A-Za-z_][A-Za-z0-9_]*\\b",
              "prefix": "TABLE",
              "severity": "High",
              "order": 6
            },
            {
              "ruleId": "sql_linked_servers",
              "name": "Linked Server References",
              "description": "Detects [Server].[Database] patterns",
              "type": "regex",
              "pattern": "\\[[A-Za-z_][A-Za-z0-9_]*\\]\\.\\[[A-Za-z_][A-Za-z0-9_]*\\]",
              "prefix": "SERVER",
              "severity": "Critical",
              "order": 1
            }
          ]
        }
        """;
    }
}
```

### 3.2 Create ConfigCommand

**File:** `src/CodeBleach/Commands/ConfigCommand.cs`

```csharp
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
            if (path)
            {
                ShowPath();
            }
            else if (list)
            {
                ShowList();
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
        
        Console.WriteLine($"Global config directory: {configDir}");
        Console.WriteLine($"Global rules file:       {rulesFile}");
        Console.WriteLine();
        Console.WriteLine($"Exists: {(locator.GlobalRulesFileExists() ? "Yes" : "No")}");
        
        if (!locator.GlobalRulesFileExists())
        {
            Console.WriteLine();
            Console.WriteLine("To create a global configuration:");
            Console.WriteLine("  codebleach init --global");
        }
    }
    
    private static void ShowList()
    {
        var locator = new GlobalConfigLocator();
        var currentDir = Directory.GetCurrentDirectory();
        var configPaths = locator.GetConfigFilePaths(currentDir).ToList();
        
        Console.WriteLine("Configuration files (in load order):");
        Console.WriteLine();
        
        if (!configPaths.Any())
        {
            Console.WriteLine("  (none found)");
            Console.WriteLine();
            Console.WriteLine("Using built-in rules only.");
            return;
        }
        
        for (int i = 0; i < configPaths.Count; i++)
        {
            Console.WriteLine($"  {i + 1}. {configPaths[i]}");
        }
        
        Console.WriteLine();
        Console.WriteLine("Later files override earlier files for rules with the same ruleId.");
    }
}
```

### 3.3 Register New Commands

**File:** `src/CodeBleach/Program.cs` (or `ProgramRoot.cs`)

```csharp
// Add to root command
rootCommand.AddCommand(SanitizeCommand.Create());
rootCommand.AddCommand(RestoreCommand.Create());
rootCommand.AddCommand(StatusCommand.Create());
rootCommand.AddCommand(InitCommand.Create());      // NEW
rootCommand.AddCommand(ConfigCommand.Create());     // NEW
```

---

## Phase 4: Testing Strategy

### 4.1 Unit Tests

**New Test Files:**
- [ ] `GlobalConfigLocatorTests.cs`
  - Test Windows path resolution
  - Test Linux/macOS path resolution
  - Test environment variable override
  - Test GetConfigFilePaths returns correct priority order
  
- [ ] `CustomRuleLoaderTests.cs` (additions)
  - Test `LoadFromMultipleFiles()`
  - Test rule override by ruleId
  - Test merging behavior

### 4.2 Integration Tests

- [ ] `InitCommandTests.cs`
  - Test `codebleach init` creates local file
  - Test `codebleach init --global` creates global file
  - Test `--sql` option includes SQL rules
  
- [ ] `ConfigCommandTests.cs`
  - Test `codebleach config --path` shows correct path
  - Test `codebleach config --list` shows hierarchy

- [ ] `SanitizeCommandTests.cs` (additions)
  - Test `--rules` option overrides auto-discovery
  - Test global rules are loaded automatically
  - Test project rules override global rules

### 4.3 End-to-End Tests

- [ ] Create global config with SQL rules
- [ ] Sanitize a project with SQL content
- [ ] Verify database names, schemas, tables are masked
- [ ] Verify 4-roundtrip restore works correctly

---

## Phase 5: Documentation Updates

### 5.1 README.md

Add sections:
- [ ] "Global Configuration"
- [ ] "Configuration Hierarchy"
- [ ] "Setting Up SQL Rules"
- [ ] Examples of `init` and `config` commands

### 5.2 Man Pages / Help Text

- [ ] Update `codebleach --help` to show new commands
- [ ] Update `codebleach sanitize --help` to show `--rules` option

---

## Rollout Plan

### Version 1.3.0 Release

**Included:**
- ✅ Phase 1: `--rules` CLI option
- ✅ Phase 2: Global config discovery
- ✅ Phase 3: `init` and `config` commands
- ✅ Phase 4: Comprehensive tests
- ✅ Phase 5: Documentation

**Breaking Changes:**
- None (fully backward compatible)

**Migration Path:**
- Existing projects continue to work as-is
- Users can opt-in to global config with `codebleach init --global`

---

## Success Criteria

- [ ] Users can run `codebleach init --global --sql` once and have SQL rules apply to all projects
- [ ] Users can override global rules with project-local `.codebleach-rules.json`
- [ ] Users can use `--rules` for one-off custom rule files
- [ ] All tests pass (unit, integration, E2E)
- [ ] Documentation complete
- [ ] 4-roundtrip test passes with SQL rules

---

## Estimated Effort

| Phase | Effort | Risk |
|-------|--------|------|
| Phase 1 | 2 hours | Low |
| Phase 2 | 4 hours | Medium (cross-platform paths) |
| Phase 3 | 3 hours | Low |
| Phase 4 | 4 hours | Medium (comprehensive tests) |
| Phase 5 | 2 hours | Low |
| **Total** | **~15 hours** | **Medium** |

---

**Ready for ENGINEER role to begin implementation.**

