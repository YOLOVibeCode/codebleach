using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;

namespace CodeBleach.Core.Services;

/// <summary>
/// Cross-platform configuration file locator for global and user-level rules.
/// </summary>
public sealed class GlobalConfigLocator : IGlobalConfigLocator
{
    private const string ConfigDirName = "codebleach";
    private const string RulesFileName = "rules.json";
    private const string EnvVarName = "CODEBLEACH_CONFIG_DIR";

    public string GetGlobalConfigDirectory()
    {
        // Check environment variable first (highest priority)
        var envConfigDir = Environment.GetEnvironmentVariable(EnvVarName);
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
        var seenPaths = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        // 1. Global user config (lowest priority)
        var globalPath = GetGlobalRulesFilePath();
        if (File.Exists(globalPath))
        {
            paths.Add(globalPath);
            seenPaths.Add(globalPath);
        }

        // 2. Explicit --rules option (medium priority)
        if (!string.IsNullOrEmpty(explicitRulesPath) && File.Exists(explicitRulesPath))
        {
            var normalizedExplicit = Path.GetFullPath(explicitRulesPath);
            if (!seenPaths.Contains(normalizedExplicit))
            {
                paths.Add(normalizedExplicit);
                seenPaths.Add(normalizedExplicit);
            }
        }

        // 3. Project-local config (highest priority)
        var projectLocalPath = CustomRuleLoader.FindConfigFile(projectPath);
        if (projectLocalPath != null && File.Exists(projectLocalPath))
        {
            var normalizedLocal = Path.GetFullPath(projectLocalPath);
            if (!seenPaths.Contains(normalizedLocal))
            {
                paths.Add(normalizedLocal);
                seenPaths.Add(normalizedLocal);
            }
        }

        return paths;
    }
}

