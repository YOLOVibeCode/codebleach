using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using CodeBleach.Core.Rules;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

/// <summary>
/// Tests for enterprise markdown sanitization scenarios.
/// Covers tables, code blocks, inline code, and links containing sensitive data.
/// </summary>
public class EnterpriseMarkdownSanitizerTests
{
    private readonly IRuleRegistry _ruleRegistry;
    private readonly ISanitizer _sut;

    public EnterpriseMarkdownSanitizerTests()
    {
        _ruleRegistry = new RuleRegistry();
        foreach (var rule in BuiltInRules.All)
            _ruleRegistry.AddRule(rule);
        
        // Add enterprise-specific rules
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "enterprise_server_fqdn",
            Name = "Enterprise Server FQDN",
            Description = "Detects fully qualified server names",
            Pattern = @"\b[A-Z][A-Z0-9-]+\.(acme-corp|internal|corp)\.(com|net)\b",
            Prefix = "HOST",
            Severity = RuleSeverity.High,
            Order = 1
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "enterprise_server_name",
            Name = "Enterprise Server Name",
            Description = "Detects enterprise server naming patterns",
            Pattern = @"\b(PROD|STG|DEV|SQL|DB|FILE|LINKED)S(RV|VR|ERVER)\d{1,2}\b",
            Prefix = "SERVER",
            Severity = RuleSeverity.High,
            Order = 2
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "enterprise_db_prefix",
            Name = "Enterprise DB Prefix",
            Description = "Detects database names with DB prefix",
            Pattern = @"\bDB[A-Z][A-Z0-9]+\b",
            Prefix = "DB",
            Severity = RuleSeverity.High,
            Order = 3
        });
        
        _sut = new Sanitizer(_ruleRegistry);
    }

    // ========================================
    // Markdown Table Tests
    // ========================================

    [Fact]
    public void Sanitize_MarkdownTable_SanitizesServerNamesInCells()
    {
        // Arrange
        var content = @"| Environment | Server |
|-------------|--------|
| Production | PRODSRV01 |
| Staging | STGSRV01 |";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("PRODSRV01");
        result.Content.Should().NotContain("STGSRV01");
        result.Content.Should().Contain("SERVER_");
        
        // Table structure should be preserved
        result.Content.Should().Contain("| Environment | Server |");
        result.Content.Should().Contain("|-------------|--------|");
    }

    [Fact]
    public void Sanitize_MarkdownTable_SanitizesIPsInCells()
    {
        // Arrange
        var content = @"| Server | IP Address |
|--------|------------|
| Primary | 10.50.100.10 |
| Secondary | 192.168.1.100 |";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("10.50.100.10");
        result.Content.Should().NotContain("192.168.1.100");
        result.Content.Should().Contain("IP_");
    }

    [Fact]
    public void Sanitize_MarkdownTable_PreservesTableStructure()
    {
        // Arrange
        var content = @"| Environment | Server | Database | IP |
|-------------|--------|----------|-----|
| Production | PRODSRV01 | DBZMEW | 10.50.100.10 |
| Staging | STGSRV01 | DBZSTG | 192.168.1.100 |";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        // Count pipe characters to verify table structure
        var originalPipes = content.Count(c => c == '|');
        var sanitizedPipes = result.Content.Count(c => c == '|');
        sanitizedPipes.Should().Be(originalPipes);
        
        // Count row separators
        result.Content.Should().Contain("|-------------|");
    }

    [Fact]
    public void Sanitize_MarkdownTable_ConsistentAliasesAcrossRows()
    {
        // Arrange - Same server appears in multiple rows
        var content = @"| Task | Server |
|------|--------|
| Backup | PRODSRV01 |
| Restore | PRODSRV01 |
| Monitor | PRODSRV01 |";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        // All PRODSRV01 should map to the same alias
        var aliasMatches = result.Matches
            .Where(m => m.OriginalValue == "PRODSRV01")
            .Select(m => m.Alias)
            .Distinct()
            .ToList();
        
        aliasMatches.Should().HaveCount(1, "same server should always get same alias");
    }

    // ========================================
    // Markdown Code Block Tests
    // ========================================

    [Fact]
    public void Sanitize_MarkdownCodeBlock_SanitizesSqlContent()
    {
        // Arrange
        var content = @"Connect to the database:

```sql
Server=PRODSRV01;Database=DBZMEW;Integrated Security=true
```";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("PRODSRV01");
        result.Content.Should().NotContain("DBZMEW");
        // Code block markers should be preserved
        result.Content.Should().Contain("```sql");
        result.Content.Should().EndWith("```");
    }

    [Fact]
    public void Sanitize_MarkdownCodeBlock_SanitizesBashContent()
    {
        // Arrange
        var content = @"SSH to the server:

```bash
ssh admin@PRODSRV01.acme-corp.com
```";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("PRODSRV01.acme-corp.com");
        result.Content.Should().Contain("```bash");
    }

    [Fact]
    public void Sanitize_MarkdownCodeBlock_SanitizesJsonContent()
    {
        // Arrange
        var content = @"Configuration:

```json
{
  ""server"": ""PRODSRV01"",
  ""database"": ""DBZMEW"",
  ""ip"": ""10.50.100.10""
}
```";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("PRODSRV01");
        result.Content.Should().NotContain("DBZMEW");
        result.Content.Should().NotContain("10.50.100.10");
    }

    [Fact]
    public void Sanitize_MultipleCodeBlocks_SanitizesAll()
    {
        // Arrange
        var content = @"## Step 1

```sql
SELECT * FROM DBZMEW.dbo.Users;
```

## Step 2

```bash
ping 10.50.100.10
```";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("DBZMEW");
        result.Content.Should().NotContain("10.50.100.10");
    }

    // ========================================
    // Markdown Inline Code Tests
    // ========================================

    [Fact]
    public void Sanitize_InlineCode_SanitizesServerName()
    {
        // Arrange
        var content = "The server `PRODSRV01` hosts the primary database.";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("PRODSRV01");
        // Backticks should still be present
        result.Content.Should().Contain("`SERVER_");
    }

    [Fact]
    public void Sanitize_InlineCode_SanitizesDatabase()
    {
        // Arrange
        var content = "Connect to `DBZMEW` for production data.";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("DBZMEW");
        result.Content.Should().Contain("`DB_");
    }

    [Fact]
    public void Sanitize_MultipleInlineCodes_SanitizesAll()
    {
        // Arrange
        var content = "Server `PRODSRV01` connects to `DBZMEW` at `10.50.100.10`.";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("PRODSRV01");
        result.Content.Should().NotContain("DBZMEW");
        result.Content.Should().NotContain("10.50.100.10");
    }

    // ========================================
    // Markdown Link Tests
    // ========================================

    [Fact]
    public void Sanitize_MarkdownLink_SanitizesInternalUrl()
    {
        // Arrange
        var content = "See [documentation](https://wiki.internal.acme.com/database)";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("wiki.internal.acme.com");
    }

    [Fact]
    public void Sanitize_MarkdownLink_PreservesLinkText()
    {
        // Arrange
        var content = "Visit [Grafana Dashboard](https://grafana.internal.acme.com/d/health)";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        // Link text should be preserved
        result.Content.Should().Contain("[Grafana Dashboard]");
        // URL should be sanitized
        result.Content.Should().NotContain("grafana.internal.acme.com");
    }

    // ========================================
    // Complex Markdown Document Tests
    // ========================================

    [Fact]
    public void Sanitize_ComplexMarkdownDocument_SanitizesAllSections()
    {
        // Arrange
        var content = @"# Deployment Guide

## Server Information

| Environment | Server | IP |
|-------------|--------|-----|
| Production | PRODSRV01 | 10.50.100.10 |

## Connection

Connect via: `PRODSRV01.acme-corp.com`

```sql
Server=PRODSRV01;Database=DBZMEW;
```

See [internal docs](https://wiki.internal.acme.com)";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        
        // All sensitive data should be sanitized
        result.Content.Should().NotContain("PRODSRV01");
        result.Content.Should().NotContain("10.50.100.10");
        result.Content.Should().NotContain("DBZMEW");
        result.Content.Should().NotContain("wiki.internal.acme.com");
        result.Content.Should().NotContain("acme-corp.com");
        
        // Structure should be preserved
        result.Content.Should().Contain("# Deployment Guide");
        result.Content.Should().Contain("## Server Information");
        result.Content.Should().Contain("```sql");
    }

    [Fact]
    public void Sanitize_MarkdownWithDiagram_SanitizesAsciiArt()
    {
        // Arrange
        var content = @"## Network Diagram

```
┌─────────────────┐     ┌─────────────────┐
│  PRODSRV01      │────▶│  DBZMEW         │
│  10.50.100.10   │     │  (SQL Server)   │
└─────────────────┘     └─────────────────┘
```";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("PRODSRV01");
        result.Content.Should().NotContain("10.50.100.10");
        result.Content.Should().NotContain("DBZMEW");
        // Box drawing characters should be preserved
        result.Content.Should().Contain("┌");
        result.Content.Should().Contain("└");
    }

    // ========================================
    // Edge Cases
    // ========================================

    [Fact]
    public void Sanitize_EmptyMarkdown_ReturnsEmpty()
    {
        // Arrange
        var content = "";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.Content.Should().BeEmpty();
        result.WasSanitized.Should().BeFalse();
    }

    [Fact]
    public void Sanitize_MarkdownWithNoSensitiveData_ReturnsUnchanged()
    {
        // Arrange
        var content = @"# Hello World

This is a simple document with no sensitive data.

```python
print('Hello, World!')
```";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.Content.Should().Be(content);
        result.WasSanitized.Should().BeFalse();
    }

    [Fact]
    public void Sanitize_MarkdownWithMixedCase_SanitizesCorrectly()
    {
        // Arrange - Our patterns are case-sensitive for server names
        var content = "Server: PRODSRV01, prodsrv01, ProdSrv01";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        // Only uppercase version should match our pattern
        result.Content.Should().NotContain("PRODSRV01");
    }
}
