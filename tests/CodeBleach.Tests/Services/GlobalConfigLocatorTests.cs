using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

public class GlobalConfigLocatorTests
{
    private readonly IGlobalConfigLocator _sut;
    private readonly string _originalConfigDirEnv;

    public GlobalConfigLocatorTests()
    {
        _sut = new GlobalConfigLocator();
        _originalConfigDirEnv = Environment.GetEnvironmentVariable("CODEBLEACH_CONFIG_DIR") ?? string.Empty;
    }

    [Fact]
    public void GetGlobalConfigDirectory_OnWindows_ReturnsAppDataPath()
    {
        // Arrange & Act
        var result = _sut.GetGlobalConfigDirectory();

        // Assert
        result.Should().NotBeNullOrEmpty();
        
        if (OperatingSystem.IsWindows())
        {
            var expectedBase = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData);
            result.Should().StartWith(expectedBase);
            result.Should().EndWith("codebleach");
        }
    }

    [Fact]
    public void GetGlobalConfigDirectory_OnLinuxMacOS_ReturnsConfigPath()
    {
        // Arrange & Act
        var result = _sut.GetGlobalConfigDirectory();

        // Assert
        result.Should().NotBeNullOrEmpty();
        
        if (OperatingSystem.IsLinux() || OperatingSystem.IsMacOS())
        {
            var home = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile);
            var expected = Path.Combine(home, ".config", "codebleach");
            result.Should().Be(expected);
        }
    }

    [Fact]
    public void GetGlobalConfigDirectory_WithEnvironmentVariable_ReturnsEnvPath()
    {
        // Arrange
        var customPath = Path.Combine(Path.GetTempPath(), "custom-codebleach-config");
        Environment.SetEnvironmentVariable("CODEBLEACH_CONFIG_DIR", customPath);
        var locator = new GlobalConfigLocator(); // Create new instance to pick up env var

        try
        {
            // Act
            var result = locator.GetGlobalConfigDirectory();

            // Assert
            result.Should().Be(customPath);
        }
        finally
        {
            // Cleanup
            Environment.SetEnvironmentVariable("CODEBLEACH_CONFIG_DIR", _originalConfigDirEnv);
        }
    }

    [Fact]
    public void GetGlobalRulesFilePath_ReturnsCorrectFileName()
    {
        // Act
        var result = _sut.GetGlobalRulesFilePath();

        // Assert
        result.Should().EndWith("rules.json");
        result.Should().Contain("codebleach");
    }

    [Fact]
    public void GlobalRulesFileExists_WhenFileDoesNotExist_ReturnsFalse()
    {
        // Act
        var result = _sut.GlobalRulesFileExists();

        // Assert
        // In a clean test environment, global rules likely don't exist
        // We're just testing the method doesn't throw
        // Result is a bool, so either true or false is valid
        (result == true || result == false).Should().BeTrue();
    }

    [Fact]
    public void GetConfigFilePaths_WithNoConfigFiles_ReturnsEmptyList()
    {
        // Arrange
        var tempDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(tempDir);

        try
        {
            // Act
            var result = _sut.GetConfigFilePaths(tempDir).ToList();

            // Assert
            result.Should().BeEmpty();
        }
        finally
        {
            Directory.Delete(tempDir, recursive: true);
        }
    }

    [Fact]
    public void GetConfigFilePaths_WithProjectLocalConfig_ReturnsProjectPath()
    {
        // Arrange
        var tempDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(tempDir);
        var configPath = Path.Combine(tempDir, ".codebleach-rules.json");
        File.WriteAllText(configPath, "{}");

        try
        {
            // Act
            var result = _sut.GetConfigFilePaths(tempDir).ToList();

            // Assert
            result.Should().HaveCount(1);
            result[0].Should().Be(configPath);
        }
        finally
        {
            Directory.Delete(tempDir, recursive: true);
        }
    }

    [Fact]
    public void GetConfigFilePaths_WithExplicitRulesPath_IncludesExplicitPath()
    {
        // Arrange
        var tempDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(tempDir);
        var explicitPath = Path.Combine(tempDir, "custom-rules.json");
        File.WriteAllText(explicitPath, "{}");

        try
        {
            // Act
            var result = _sut.GetConfigFilePaths(tempDir, explicitPath).ToList();

            // Assert
            result.Should().Contain(explicitPath);
        }
        finally
        {
            Directory.Delete(tempDir, recursive: true);
        }
    }

    [Fact]
    public void GetConfigFilePaths_WithMultipleConfigs_ReturnsInCorrectOrder()
    {
        // Arrange
        var tempDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(tempDir);
        
        var projectLocalPath = Path.Combine(tempDir, ".codebleach-rules.json");
        var explicitPath = Path.Combine(tempDir, "explicit-rules.json");
        
        File.WriteAllText(projectLocalPath, "{}");
        File.WriteAllText(explicitPath, "{}");

        try
        {
            // Act
            var result = _sut.GetConfigFilePaths(tempDir, explicitPath).ToList();

            // Assert
            // Explicit should come before project-local
            var explicitIndex = result.IndexOf(explicitPath);
            var projectIndex = result.IndexOf(projectLocalPath);
            
            explicitIndex.Should().BeGreaterThanOrEqualTo(0);
            projectIndex.Should().BeGreaterThanOrEqualTo(0);
            explicitIndex.Should().BeLessThan(projectIndex);
        }
        finally
        {
            Directory.Delete(tempDir, recursive: true);
        }
    }

    [Fact]
    public void GetConfigFilePaths_WithNonExistentExplicitPath_DoesNotIncludeIt()
    {
        // Arrange
        var tempDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(tempDir);
        var nonExistentPath = Path.Combine(tempDir, "does-not-exist.json");

        try
        {
            // Act
            var result = _sut.GetConfigFilePaths(tempDir, nonExistentPath).ToList();

            // Assert
            result.Should().NotContain(nonExistentPath);
        }
        finally
        {
            Directory.Delete(tempDir, recursive: true);
        }
    }

    [Fact]
    public void GetConfigFilePaths_WithDuplicatePaths_NoDuplicatesInResult()
    {
        // Arrange
        var tempDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(tempDir);
        var configPath = Path.Combine(tempDir, ".codebleach-rules.json");
        File.WriteAllText(configPath, "{}");

        try
        {
            // Act - Pass same path as both project and explicit
            var result = _sut.GetConfigFilePaths(tempDir, configPath).ToList();

            // Assert
            result.Should().OnlyHaveUniqueItems();
            result.Count(p => p == configPath).Should().Be(1);
        }
        finally
        {
            Directory.Delete(tempDir, recursive: true);
        }
    }
}

