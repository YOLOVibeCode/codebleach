using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using CodeBleach.Core.Rules;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

/// <summary>
/// Tests for round-trip sanitization and restoration.
/// Verifies that: Original → Sanitize → Restore = Original
/// </summary>
public class RoundTripTests
{
    private readonly IRuleRegistry _ruleRegistry;
    private readonly ISanitizer _sanitizer;
    private readonly IRestorer _restorer;

    public RoundTripTests()
    {
        _ruleRegistry = new RuleRegistry();
        foreach (var rule in BuiltInRules.All)
            _ruleRegistry.AddRule(rule);
        
        // Add enterprise rules
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "enterprise_server",
            Name = "Enterprise Server",
            Description = "Detects enterprise server names",
            Pattern = @"\b(PROD|STG|DEV)SRV\d{1,2}\b",
            Prefix = "SERVER",
            Severity = RuleSeverity.High,
            Order = 1
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "enterprise_db",
            Name = "Enterprise Database",
            Description = "Detects enterprise database names",
            Pattern = @"\bDB[A-Z][A-Z0-9]+\b",
            Prefix = "DB",
            Severity = RuleSeverity.High,
            Order = 2
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "sql_table_prefixed",
            Name = "SQL Prefixed Table",
            Description = "Detects prefixed table names",
            Pattern = @"\b(TB|TA)\d{5,6}\b",
            Prefix = "TBL",
            Severity = RuleSeverity.Medium,
            Order = 3
        });
        
        _sanitizer = new Sanitizer(_ruleRegistry);
        _restorer = new Restorer();
    }

    // ========================================
    // Basic Round-Trip Tests
    // ========================================

    [Fact]
    public void RoundTrip_SimpleServerName_RestoresPerfectly()
    {
        // Arrange
        var original = "Connect to PRODSRV01 for production data.";
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().Be(original);
        sanitized.Content.Should().NotBe(original);
    }

    [Fact]
    public void RoundTrip_MultipleServers_RestoresPerfectly()
    {
        // Arrange
        var original = "Primary: PRODSRV01, Secondary: PRODSRV02, Staging: STGSRV01";
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().Be(original);
    }

    [Fact]
    public void RoundTrip_PrivateIPs_RestoresPerfectly()
    {
        // Arrange
        var original = "Database at 10.50.100.10, Redis at 192.168.1.100";
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().Be(original);
    }

    [Fact]
    public void RoundTrip_DatabaseNames_RestoresPerfectly()
    {
        // Arrange
        var original = "Query DBZMEW for data, DBZBHI for analytics.";
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().Be(original);
    }

    [Fact]
    public void RoundTrip_TableNames_RestoresPerfectly()
    {
        // Arrange
        var original = "Tables: TB00123, TB00456, TA09052";
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().Be(original);
    }

    // ========================================
    // Complex Content Round-Trip Tests
    // ========================================

    [Fact]
    public void RoundTrip_SqlQuery_RestoresPerfectly()
    {
        // Arrange
        var original = @"SELECT e.EMP_ID, e.NAME
FROM DBZMEW.dbo.TB00123 e
WHERE e.DEPT_ID IN (
    SELECT DEPT_ID FROM DBZMEW.dbo.TB00456
)";
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().Be(original);
    }

    [Fact]
    public void RoundTrip_MarkdownDocument_RestoresPerfectly()
    {
        // Arrange
        var original = @"# Database Guide

## Servers

| Environment | Server | IP |
|-------------|--------|-----|
| Production | PRODSRV01 | 10.50.100.10 |
| Staging | STGSRV01 | 192.168.1.100 |

## Connection

```sql
Server=PRODSRV01;Database=DBZMEW;
```

Connect to `PRODSRV01` using your credentials.";
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().Be(original);
    }

    [Fact]
    public void RoundTrip_JsonConfig_RestoresPerfectly()
    {
        // Arrange
        var original = @"{
  ""database"": {
    ""server"": ""PRODSRV01"",
    ""name"": ""DBZMEW"",
    ""port"": 1433
  },
  ""redis"": {
    ""host"": ""192.168.1.100"",
    ""port"": 6379
  }
}";
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().Be(original);
    }

    [Fact]
    public void RoundTrip_YamlConfig_RestoresPerfectly()
    {
        // Arrange
        var original = @"database:
  server: PRODSRV01
  name: DBZMEW
  
redis:
  host: 192.168.1.100
  port: 6379";
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().Be(original);
    }

    // ========================================
    // AI Modification Simulation Tests
    // ========================================

    [Fact]
    public void RoundTrip_AfterAiAddsCode_PreservesAdditions()
    {
        // Arrange
        var original = "var server = \"PRODSRV01\";";
        var mappings = new MappingTable();
        
        // Sanitize
        var sanitized = _sanitizer.Sanitize(original, mappings);
        
        // Simulate AI adding new code (that doesn't contain sensitive data)
        var aiModified = sanitized.Content + "\nvar port = 1433;";

        // Act - Restore
        var restored = _restorer.Restore(aiModified, mappings);

        // Assert
        restored.Content.Should().Contain("PRODSRV01");
        restored.Content.Should().Contain("var port = 1433;");
    }

    [Fact]
    public void RoundTrip_AfterAiModifiesLogic_PreservesChanges()
    {
        // Arrange
        var original = @"var server = ""PRODSRV01"";
var db = ""DBZMEW"";
Console.WriteLine(server);";
        var mappings = new MappingTable();
        
        // Sanitize
        var sanitized = _sanitizer.Sanitize(original, mappings);
        
        // Simulate AI modifying the code (change WriteLine to Log)
        var aiModified = sanitized.Content.Replace("Console.WriteLine", "Logger.Info");

        // Act - Restore
        var restored = _restorer.Restore(aiModified, mappings);

        // Assert
        restored.Content.Should().Contain("PRODSRV01");
        restored.Content.Should().Contain("DBZMEW");
        restored.Content.Should().Contain("Logger.Info"); // AI change preserved
        restored.Content.Should().NotContain("Console.WriteLine"); // Original removed
    }

    [Fact]
    public void RoundTrip_AfterAiReorganizesCode_RestoresCorrectly()
    {
        // Arrange
        var original = @"// Config
var server = ""PRODSRV01"";
var ip = ""10.50.100.10"";
var db = ""DBZMEW"";";
        var mappings = new MappingTable();
        
        // Sanitize
        var sanitized = _sanitizer.Sanitize(original, mappings);
        
        // Simulate AI reorganizing (swapping lines)
        var lines = sanitized.Content.Split('\n');
        var aiModified = $"{lines[0]}\n{lines[3]}\n{lines[2]}\n{lines[1]}";

        // Act - Restore
        var restored = _restorer.Restore(aiModified, mappings);

        // Assert
        // All values should be restored, even in different order
        restored.Content.Should().Contain("PRODSRV01");
        restored.Content.Should().Contain("10.50.100.10");
        restored.Content.Should().Contain("DBZMEW");
    }

    // ========================================
    // Multiple Sanitize-Restore Cycles
    // ========================================

    [Fact]
    public void RoundTrip_MultipleCycles_RemainsPerfect()
    {
        // Arrange
        var original = "Server: PRODSRV01, Database: DBZMEW, IP: 10.50.100.10";
        var current = original;

        // Act - Multiple sanitize/restore cycles
        for (int i = 0; i < 5; i++)
        {
            var mappings = new MappingTable();
            var sanitized = _sanitizer.Sanitize(current, mappings);
            var restored = _restorer.Restore(sanitized.Content, mappings);
            current = restored.Content;
        }

        // Assert
        current.Should().Be(original);
    }

    // ========================================
    // Edge Cases
    // ========================================

    [Fact]
    public void RoundTrip_NoSensitiveData_RemainsUnchanged()
    {
        // Arrange
        var original = "Hello, World! This has no sensitive data.";
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        sanitized.Content.Should().Be(original);
        restored.Content.Should().Be(original);
    }

    [Fact]
    public void RoundTrip_EmptyContent_RemainsEmpty()
    {
        // Arrange
        var original = "";
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().BeEmpty();
    }

    [Fact]
    public void RoundTrip_WhitespaceOnly_RemainsUnchanged()
    {
        // Arrange
        var original = "   \n\t\n   ";
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().Be(original);
    }

    [Fact]
    public void RoundTrip_LongDocument_RestoresPerfectly()
    {
        // Arrange - Create a long document with repeated patterns
        var lines = new List<string>();
        for (int i = 0; i < 100; i++)
        {
            lines.Add($"Line {i}: Server=PRODSRV01, DB=DBZMEW, IP=10.50.100.10");
        }
        var original = string.Join("\n", lines);
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().Be(original);
    }

    [Fact]
    public void RoundTrip_UnicodeContent_RestoresPerfectly()
    {
        // Arrange
        var original = "Server: PRODSRV01 🖥️, Database: DBZMEW 💾, Emoji: 🎉";
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().Be(original);
    }

    [Fact]
    public void RoundTrip_SpecialCharacters_RestoresPerfectly()
    {
        // Arrange
        var original = "Server=PRODSRV01;Password=P@$$w0rd!;Database=DBZMEW";
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().Be(original);
    }
}
