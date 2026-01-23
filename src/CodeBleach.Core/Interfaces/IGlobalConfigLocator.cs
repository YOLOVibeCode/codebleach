namespace CodeBleach.Core.Interfaces;

/// <summary>
/// Locates global and user-level configuration files across platforms.
/// </summary>
public interface IGlobalConfigLocator
{
    /// <summary>
    /// Gets the primary global configuration directory path for the current platform.
    /// </summary>
    /// <returns>Full path to global config directory (e.g., ~/.config/codebleach on Linux/macOS)</returns>
    string GetGlobalConfigDirectory();
    
    /// <summary>
    /// Gets the full path to the global rules file.
    /// </summary>
    /// <returns>Full path to global rules.json</returns>
    string GetGlobalRulesFilePath();
    
    /// <summary>
    /// Checks if a global rules file exists.
    /// </summary>
    /// <returns>True if global rules file exists, false otherwise</returns>
    bool GlobalRulesFileExists();
    
    /// <summary>
    /// Gets all configuration file paths in priority order (lowest to highest).
    /// Only returns paths that actually exist on the file system.
    /// </summary>
    /// <param name="projectPath">Starting project path for local config discovery</param>
    /// <param name="explicitRulesPath">Optional explicit rules file from --rules CLI option</param>
    /// <returns>List of config file paths to load, in priority order</returns>
    IEnumerable<string> GetConfigFilePaths(string projectPath, string? explicitRulesPath = null);
}

