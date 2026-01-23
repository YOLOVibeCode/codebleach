using CodeBleach.Core.Models;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

public class CustomRuleLoaderTests
{
    [Fact]
    public void LoadFromFile_WithValidJson_ReturnsRules()
    {
        // Arrange
        var tempFile = Path.GetTempFileName();
        var json = """
        {
          "rules": [
            {
              "ruleId": "test_rule",
              "name": "Test Rule",
              "type": "regex",
              "pattern": "\\bTEST\\b",
              "prefix": "TST",
              "severity": "Medium"
            }
          ]
        }
        """;
        File.WriteAllText(tempFile, json);

        try
        {
            // Act
            var result = CustomRuleLoader.LoadFromFile(tempFile).ToList();

            // Assert
            result.Should().HaveCount(1);
            result[0].RuleId.Should().Be("test_rule");
            result[0].Name.Should().Be("Test Rule");
            result[0].Pattern.Should().Be("\\bTEST\\b");
            result[0].Prefix.Should().Be("TST");
        }
        finally
        {
            File.Delete(tempFile);
        }
    }

    [Fact]
    public void LoadFromFile_WithNonExistentFile_ReturnsEmpty()
    {
        // Act
        var result = CustomRuleLoader.LoadFromFile("/path/that/does/not/exist.json");

        // Assert
        result.Should().BeEmpty();
    }

    [Fact]
    public void LoadFromFile_WithDisabledRule_DoesNotReturnIt()
    {
        // Arrange
        var tempFile = Path.GetTempFileName();
        var json = """
        {
          "rules": [
            {
              "ruleId": "disabled_rule",
              "name": "Disabled Rule",
              "type": "regex",
              "pattern": "\\bDISABLED\\b",
              "prefix": "DIS",
              "enabled": false
            }
          ]
        }
        """;
        File.WriteAllText(tempFile, json);

        try
        {
            // Act
            var result = CustomRuleLoader.LoadFromFile(tempFile).ToList();

            // Assert
            result.Should().BeEmpty();
        }
        finally
        {
            File.Delete(tempFile);
        }
    }

    [Fact]
    public void LoadFromMultipleFiles_WithNoFiles_ReturnsEmpty()
    {
        // Act
        var result = CustomRuleLoader.LoadFromMultipleFiles([]);

        // Assert
        result.Should().BeEmpty();
    }

    [Fact]
    public void LoadFromMultipleFiles_WithSingleFile_ReturnsRules()
    {
        // Arrange
        var tempFile = Path.GetTempFileName();
        var json = """
        {
          "rules": [
            {
              "ruleId": "rule1",
              "name": "Rule 1",
              "type": "regex",
              "pattern": "\\bRULE1\\b",
              "prefix": "R1"
            }
          ]
        }
        """;
        File.WriteAllText(tempFile, json);

        try
        {
            // Act
            var result = CustomRuleLoader.LoadFromMultipleFiles([tempFile]).ToList();

            // Assert
            result.Should().HaveCount(1);
            result[0].RuleId.Should().Be("rule1");
        }
        finally
        {
            File.Delete(tempFile);
        }
    }

    [Fact]
    public void LoadFromMultipleFiles_WithMultipleFiles_MergesRules()
    {
        // Arrange
        var file1 = Path.GetTempFileName();
        var file2 = Path.GetTempFileName();
        
        var json1 = """
        {
          "rules": [
            {
              "ruleId": "rule1",
              "name": "Rule 1",
              "type": "regex",
              "pattern": "\\bRULE1\\b",
              "prefix": "R1"
            }
          ]
        }
        """;
        
        var json2 = """
        {
          "rules": [
            {
              "ruleId": "rule2",
              "name": "Rule 2",
              "type": "regex",
              "pattern": "\\bRULE2\\b",
              "prefix": "R2"
            }
          ]
        }
        """;
        
        File.WriteAllText(file1, json1);
        File.WriteAllText(file2, json2);

        try
        {
            // Act
            var result = CustomRuleLoader.LoadFromMultipleFiles([file1, file2]).ToList();

            // Assert
            result.Should().HaveCount(2);
            result.Should().Contain(r => r.RuleId == "rule1");
            result.Should().Contain(r => r.RuleId == "rule2");
        }
        finally
        {
            File.Delete(file1);
            File.Delete(file2);
        }
    }

    [Fact]
    public void LoadFromMultipleFiles_WithSameRuleIdInMultipleFiles_LaterFileOverrides()
    {
        // Arrange
        var file1 = Path.GetTempFileName();
        var file2 = Path.GetTempFileName();
        
        var json1 = """
        {
          "rules": [
            {
              "ruleId": "shared_rule",
              "name": "Original Name",
              "type": "regex",
              "pattern": "\\bORIGINAL\\b",
              "prefix": "ORIG"
            }
          ]
        }
        """;
        
        var json2 = """
        {
          "rules": [
            {
              "ruleId": "shared_rule",
              "name": "Overridden Name",
              "type": "regex",
              "pattern": "\\bOVERRIDDEN\\b",
              "prefix": "OVER"
            }
          ]
        }
        """;
        
        File.WriteAllText(file1, json1);
        File.WriteAllText(file2, json2);

        try
        {
            // Act
            var result = CustomRuleLoader.LoadFromMultipleFiles([file1, file2]).ToList();

            // Assert
            result.Should().HaveCount(1);
            result[0].RuleId.Should().Be("shared_rule");
            result[0].Name.Should().Be("Overridden Name");
            result[0].Pattern.Should().Be("\\bOVERRIDDEN\\b");
            result[0].Prefix.Should().Be("OVER");
        }
        finally
        {
            File.Delete(file1);
            File.Delete(file2);
        }
    }

    [Fact]
    public void LoadFromMultipleFiles_WithDisabledRuleInLaterFile_DisablesRule()
    {
        // Arrange
        var file1 = Path.GetTempFileName();
        var file2 = Path.GetTempFileName();
        
        var json1 = """
        {
          "rules": [
            {
              "ruleId": "to_disable",
              "name": "Will Be Disabled",
              "type": "regex",
              "pattern": "\\bENABLED\\b",
              "prefix": "EN",
              "enabled": true
            }
          ]
        }
        """;
        
        var json2 = """
        {
          "rules": [
            {
              "ruleId": "to_disable",
              "name": "Now Disabled",
              "type": "regex",
              "pattern": "\\bDISABLED\\b",
              "prefix": "DIS",
              "enabled": false
            }
          ]
        }
        """;
        
        File.WriteAllText(file1, json1);
        File.WriteAllText(file2, json2);

        try
        {
            // Act
            var result = CustomRuleLoader.LoadFromMultipleFiles([file1, file2]).ToList();

            // Assert
            result.Should().BeEmpty(); // Disabled rules are filtered out
        }
        finally
        {
            File.Delete(file1);
            File.Delete(file2);
        }
    }

    [Fact]
    public void LoadFromMultipleFiles_WithInvalidFile_SkipsInvalidFile()
    {
        // Arrange
        var validFile = Path.GetTempFileName();
        var invalidFile = Path.GetTempFileName();
        
        var validJson = """
        {
          "rules": [
            {
              "ruleId": "valid_rule",
              "name": "Valid Rule",
              "type": "regex",
              "pattern": "\\bVALID\\b",
              "prefix": "VAL"
            }
          ]
        }
        """;
        
        File.WriteAllText(validFile, validJson);
        File.WriteAllText(invalidFile, "{ invalid json }");

        try
        {
            // Act
            var result = CustomRuleLoader.LoadFromMultipleFiles([invalidFile, validFile]).ToList();

            // Assert
            // Should load the valid file and skip the invalid one
            result.Should().HaveCount(1);
            result[0].RuleId.Should().Be("valid_rule");
        }
        finally
        {
            File.Delete(validFile);
            File.Delete(invalidFile);
        }
    }

    [Fact]
    public void FindConfigFile_FromCurrentDirectory_FindsFile()
    {
        // Arrange
        var tempDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(tempDir);
        var configPath = Path.Combine(tempDir, CustomRuleConfig.DefaultFileName);
        File.WriteAllText(configPath, "{}");

        try
        {
            // Act
            var result = CustomRuleLoader.FindConfigFile(tempDir);

            // Assert
            result.Should().Be(configPath);
        }
        finally
        {
            Directory.Delete(tempDir, recursive: true);
        }
    }

    [Fact]
    public void FindConfigFile_FromParentDirectory_FindsFile()
    {
        // Arrange
        var tempDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        var subDir = Path.Combine(tempDir, "subdir");
        Directory.CreateDirectory(subDir);
        
        var configPath = Path.Combine(tempDir, CustomRuleConfig.DefaultFileName);
        File.WriteAllText(configPath, "{}");

        try
        {
            // Act
            var result = CustomRuleLoader.FindConfigFile(subDir);

            // Assert
            result.Should().Be(configPath);
        }
        finally
        {
            Directory.Delete(tempDir, recursive: true);
        }
    }

    [Fact]
    public void FindConfigFile_WhenNotFound_ReturnsNull()
    {
        // Arrange
        var tempDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(tempDir);

        try
        {
            // Act
            var result = CustomRuleLoader.FindConfigFile(tempDir);

            // Assert
            result.Should().BeNull();
        }
        finally
        {
            Directory.Delete(tempDir, recursive: true);
        }
    }
}
