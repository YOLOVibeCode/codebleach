using CodeBleach.Core.Models;
using CodeBleach.Core.Services;
using FluentAssertions;

namespace CodeBleach.Tests.Services;

public class CustomRuleLoaderTests : IDisposable
{
    private readonly string _tempDir;

    public CustomRuleLoaderTests()
    {
        _tempDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString());
        Directory.CreateDirectory(_tempDir);
    }

    public void Dispose()
    {
        if (Directory.Exists(_tempDir))
        {
            Directory.Delete(_tempDir, recursive: true);
        }
    }

    [Fact]
    public void LoadFromFile_WithInlineRegex_ReturnsRule()
    {
        // Arrange
        var configPath = Path.Combine(_tempDir, ".codebleach-rules.json");
        File.WriteAllText(configPath, """
        {
          "rules": [
            {
              "ruleId": "test_tables",
              "name": "Test Tables",
              "type": "regex",
              "pattern": "\\bTB\\d{5}\\b",
              "prefix": "TBL"
            }
          ]
        }
        """);

        // Act
        var rules = CustomRuleLoader.LoadFromFile(configPath).ToList();

        // Assert
        rules.Should().HaveCount(1);
        rules[0].RuleId.Should().Be("test_tables");
        rules[0].Pattern.Should().Be(@"\bTB\d{5}\b");
        rules[0].Prefix.Should().Be("TBL");
    }

    [Fact]
    public void LoadFromFile_WithRegexFile_LoadsPatternFromFile()
    {
        // Arrange
        var rulesDir = Path.Combine(_tempDir, "rules");
        Directory.CreateDirectory(rulesDir);
        
        var regexFile = Path.Combine(rulesDir, "servers.regex");
        File.WriteAllText(regexFile, """
        # Server patterns
        \b[A-Z]+SRV\d{1,3}\b
        \b[A-Z]+SVR\d{1,3}\b
        """);

        var configPath = Path.Combine(_tempDir, ".codebleach-rules.json");
        File.WriteAllText(configPath, """
        {
          "rules": [
            {
              "ruleId": "file_servers",
              "name": "Servers from File",
              "type": "regexFile",
              "patternFile": "rules/servers.regex",
              "prefix": "SRV"
            }
          ]
        }
        """);

        // Act
        var rules = CustomRuleLoader.LoadFromFile(configPath).ToList();

        // Assert
        rules.Should().HaveCount(1);
        rules[0].RuleId.Should().Be("file_servers");
        rules[0].Pattern.Should().Contain(@"\b[A-Z]+SRV\d{1,3}\b");
        rules[0].Pattern.Should().Contain(@"\b[A-Z]+SVR\d{1,3}\b");
        rules[0].Pattern.Should().StartWith("("); // Combined with OR
    }

    [Fact]
    public void LoadFromFile_WithJavaScript_ExecutesGetPattern()
    {
        // Arrange
        var rulesDir = Path.Combine(_tempDir, "rules");
        Directory.CreateDirectory(rulesDir);
        
        var jsFile = Path.Combine(rulesDir, "databases.js");
        File.WriteAllText(jsFile, """
        function getPattern() {
            return '\\b[A-Z]+DB\\b';
        }
        """);

        var configPath = Path.Combine(_tempDir, ".codebleach-rules.json");
        File.WriteAllText(configPath, """
        {
          "rules": [
            {
              "ruleId": "js_databases",
              "name": "JS Databases",
              "type": "javascript",
              "scriptFile": "rules/databases.js",
              "prefix": "DB"
            }
          ]
        }
        """);

        // Act
        var rules = CustomRuleLoader.LoadFromFile(configPath).ToList();

        // Assert
        rules.Should().HaveCount(1);
        rules[0].RuleId.Should().Be("js_databases");
        rules[0].Pattern.Should().Be(@"\b[A-Z]+DB\b");
    }

    [Fact]
    public void LoadFromFile_WithMultipleRuleTypes_LoadsAll()
    {
        // Arrange
        var rulesDir = Path.Combine(_tempDir, "rules");
        Directory.CreateDirectory(rulesDir);
        
        File.WriteAllText(Path.Combine(rulesDir, "servers.regex"), @"\b[A-Z]+SRV\d+\b");
        File.WriteAllText(Path.Combine(rulesDir, "db.js"), "function getPattern() { return '\\\\bDB[0-9]+\\\\b'; }");

        var configPath = Path.Combine(_tempDir, ".codebleach-rules.json");
        File.WriteAllText(configPath, """
        {
          "rules": [
            {
              "ruleId": "inline_rule",
              "name": "Inline",
              "type": "regex",
              "pattern": "\\bTBL\\d+\\b",
              "prefix": "TBL"
            },
            {
              "ruleId": "file_rule",
              "name": "From File",
              "type": "regexFile",
              "patternFile": "rules/servers.regex",
              "prefix": "SRV"
            },
            {
              "ruleId": "js_rule",
              "name": "JavaScript",
              "type": "javascript",
              "scriptFile": "rules/db.js",
              "prefix": "DB"
            }
          ]
        }
        """);

        // Act
        var rules = CustomRuleLoader.LoadFromFile(configPath).ToList();

        // Assert
        rules.Should().HaveCount(3);
        rules.Should().Contain(r => r.RuleId == "inline_rule");
        rules.Should().Contain(r => r.RuleId == "file_rule");
        rules.Should().Contain(r => r.RuleId == "js_rule");
    }

    [Fact]
    public void LoadFromFile_WithDisabledRule_SkipsIt()
    {
        // Arrange
        var configPath = Path.Combine(_tempDir, ".codebleach-rules.json");
        File.WriteAllText(configPath, """
        {
          "rules": [
            {
              "ruleId": "enabled_rule",
              "name": "Enabled",
              "type": "regex",
              "pattern": "\\bTBL\\d+\\b",
              "prefix": "TBL",
              "enabled": true
            },
            {
              "ruleId": "disabled_rule",
              "name": "Disabled",
              "type": "regex",
              "pattern": "\\bXYZ\\d+\\b",
              "prefix": "XYZ",
              "enabled": false
            }
          ]
        }
        """);

        // Act
        var rules = CustomRuleLoader.LoadFromFile(configPath).ToList();

        // Assert
        rules.Should().HaveCount(1);
        rules[0].RuleId.Should().Be("enabled_rule");
    }

    [Fact]
    public void LoadFromFile_RegexFileWithComments_IgnoresComments()
    {
        // Arrange
        var rulesDir = Path.Combine(_tempDir, "rules");
        Directory.CreateDirectory(rulesDir);
        
        var regexFile = Path.Combine(rulesDir, "test.regex");
        File.WriteAllText(regexFile, """
        # This is a comment
        \bPATTERN1\b
        
        # Another comment
        \bPATTERN2\b
        # Trailing comment
        """);

        var configPath = Path.Combine(_tempDir, ".codebleach-rules.json");
        File.WriteAllText(configPath, """
        {
          "rules": [
            {
              "ruleId": "test",
              "name": "Test",
              "type": "regexFile",
              "patternFile": "rules/test.regex",
              "prefix": "TEST"
            }
          ]
        }
        """);

        // Act
        var rules = CustomRuleLoader.LoadFromFile(configPath).ToList();

        // Assert
        rules.Should().HaveCount(1);
        rules[0].Pattern.Should().Be(@"(\bPATTERN1\b|\bPATTERN2\b)");
    }

    [Fact]
    public void ExecuteJavaScriptMatcher_WithMatchFunction_ReturnsMatches()
    {
        // Arrange
        var rulesDir = Path.Combine(_tempDir, "rules");
        Directory.CreateDirectory(rulesDir);
        
        var jsFile = Path.Combine(rulesDir, "matcher.js");
        File.WriteAllText(jsFile, """
        function match(content) {
            var results = [];
            var regex = /\bSRV\d+\b/g;
            var m;
            while ((m = regex.exec(content)) !== null) {
                results.push(m[0]);
            }
            return results;
        }
        """);

        var content = "Connect to SRV01 and SRV02 servers";

        // Act
        var matches = CustomRuleLoader.ExecuteJavaScriptMatcher(
            jsFile, content, _tempDir).ToList();

        // Assert
        matches.Should().HaveCount(2);
        matches.Should().Contain("SRV01");
        matches.Should().Contain("SRV02");
    }

    [Fact]
    public void FindConfigFile_WhenExists_ReturnsPath()
    {
        // Arrange
        var configPath = Path.Combine(_tempDir, ".codebleach-rules.json");
        File.WriteAllText(configPath, "{}");
        var subDir = Path.Combine(_tempDir, "sub", "deep");
        Directory.CreateDirectory(subDir);

        // Act
        var found = CustomRuleLoader.FindConfigFile(subDir);

        // Assert
        found.Should().Be(configPath);
    }

    [Fact]
    public void FindConfigFile_WhenNotExists_ReturnsNull()
    {
        // Act
        var found = CustomRuleLoader.FindConfigFile(_tempDir);

        // Assert
        found.Should().BeNull();
    }
}

