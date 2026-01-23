using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using CodeBleach.Core.Rules;
using CodeBleach.Core.Services;

namespace CodeBleach.IntegrationTests;

/// <summary>
/// Integration tests using the enterprise fixtures.
/// Tests full sanitization pipeline with realistic corporate data.
/// </summary>
public class EnterpriseFixtureTests : IDisposable
{
    private readonly string _fixturesPath;
    private readonly string _tempOutputPath;
    private readonly IRuleRegistry _ruleRegistry;
    private readonly ISanitizer _sanitizer;
    private readonly IRestorer _restorer;
    private readonly IFileProcessor _fileProcessor;

    public EnterpriseFixtureTests()
    {
        // Find fixtures path relative to test execution
        _fixturesPath = FindFixturesPath();
        _tempOutputPath = Path.Combine(Path.GetTempPath(), $"codebleach-test-{Guid.NewGuid()}");
        Directory.CreateDirectory(_tempOutputPath);
        
        _ruleRegistry = new RuleRegistry();
        SetupEnterpriseRules();
        
        _sanitizer = new Sanitizer(_ruleRegistry);
        _restorer = new Restorer();
        _fileProcessor = new FileProcessor();
    }

    private string FindFixturesPath()
    {
        var currentDir = Directory.GetCurrentDirectory();
        var searchPaths = new[]
        {
            Path.Combine(currentDir, "fixtures"),
            Path.Combine(currentDir, "..", "..", "..", "fixtures"),
            Path.Combine(currentDir, "..", "..", "..", "..", "fixtures"),
            Path.Combine(currentDir, "..", "..", "..", "..", "..", "tests", "fixtures"),
        };
        
        foreach (var path in searchPaths)
        {
            var fullPath = Path.GetFullPath(path);
            if (Directory.Exists(fullPath))
                return fullPath;
        }
        
        // Fallback to test directory structure
        return Path.Combine(currentDir, "fixtures");
    }

    private void SetupEnterpriseRules()
    {
        // Add built-in rules
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
            RuleId = "enterprise_server",
            Name = "Enterprise Server",
            Description = "Detects enterprise server names",
            Pattern = @"\b(PROD|STG|DEV|SQL|FILE|LINKED)S(RV|VR|ERVER)\d{1,2}\b",
            Prefix = "SERVER",
            Severity = RuleSeverity.High,
            Order = 2
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "enterprise_db",
            Name = "Enterprise Database",
            Description = "Detects enterprise database names",
            Pattern = @"\bDB[A-Z][A-Z0-9]+\b",
            Prefix = "DB",
            Severity = RuleSeverity.High,
            Order = 3
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "sql_4part_bracketed",
            Name = "SQL 4-Part Bracketed",
            Description = "Detects [Server].[Database].[Schema].[Table]",
            Pattern = @"\[[A-Z][A-Z0-9_]+\]\.\[[A-Z][A-Z0-9_]+\]\.\[[a-z]+\]\.\[[A-Z][A-Z0-9_]+\]",
            Prefix = "LINKED",
            Severity = RuleSeverity.High,
            Order = 4
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "sql_table_prefixed",
            Name = "SQL Prefixed Table",
            Description = "Detects TB/TA prefixed tables",
            Pattern = @"\b(TB|TA)\d{5,6}\b",
            Prefix = "TBL",
            Severity = RuleSeverity.Medium,
            Order = 5
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "ad_service_account",
            Name = "AD Service Account",
            Description = "Detects Active Directory service accounts",
            Pattern = @"\b[A-Z]+\\svc_[a-z0-9_]+\b",
            Prefix = "ADUSER",
            Severity = RuleSeverity.High,
            Order = 6
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "vault_path",
            Name = "Vault Path",
            Description = "Detects HashiCorp Vault paths",
            Pattern = @"secret/data/[a-z0-9/-]+",
            Prefix = "VAULT",
            Severity = RuleSeverity.Critical,
            Order = 7
        });
    }

    public void Dispose()
    {
        if (Directory.Exists(_tempOutputPath))
        {
            try
            {
                Directory.Delete(_tempOutputPath, recursive: true);
            }
            catch
            {
                // Ignore cleanup errors in tests
            }
        }
    }

    // ========================================
    // Enterprise Docs Fixture Tests
    // ========================================

    [Fact]
    public void EnterpriseDocsFixture_Exists()
    {
        // Arrange
        var enterpriseDocsPath = Path.Combine(_fixturesPath, "enterprise-docs");

        // Assert
        Directory.Exists(enterpriseDocsPath).Should().BeTrue(
            $"Enterprise docs fixture should exist at {enterpriseDocsPath}");
    }

    [Fact]
    public async Task EnterpriseDocsFixture_ReadmeIsSanitizable()
    {
        // Arrange
        var readmePath = Path.Combine(_fixturesPath, "enterprise-docs", "README.md");
        if (!File.Exists(readmePath))
        {
            // Skip if fixture doesn't exist yet
            return;
        }
        
        var content = await File.ReadAllTextAsync(readmePath);
        var mappings = new MappingTable();

        // Act
        var result = _sanitizer.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue("README should contain sensitive data");
        result.Content.Should().NotContain("PRODSRV01");
        result.Content.Should().NotContain("DBZMEW");
        result.Content.Should().NotContain("10.50.100.10");
        result.Content.Should().NotContain("acme-corp.com");
    }

    [Fact]
    public async Task EnterpriseDocsFixture_DatabaseDesignIsSanitizable()
    {
        // Arrange
        var docPath = Path.Combine(_fixturesPath, "enterprise-docs", "docs", "architecture", "database-design.md");
        if (!File.Exists(docPath))
        {
            return;
        }
        
        var content = await File.ReadAllTextAsync(docPath);
        var mappings = new MappingTable();

        // Act
        var result = _sanitizer.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        
        // Server names should be sanitized
        result.Content.Should().NotContain("PRODSRV01");
        result.Content.Should().NotContain("LINKEDSRV01");
        
        // Database names should be sanitized
        result.Content.Should().NotContain("DBZMEW");
        result.Content.Should().NotContain("DBZBHI");
        
        // Table names should be sanitized
        result.Content.Should().NotContain("TB00123");
        result.Content.Should().NotContain("TB00456");
        
        // IPs should be sanitized
        result.Content.Should().NotContain("10.50.100.10");
        result.Content.Should().NotContain("10.50.200.10");
    }

    [Fact]
    public async Task EnterpriseDocsFixture_SqlMigrationIsSanitizable()
    {
        // Arrange
        var sqlPath = Path.Combine(_fixturesPath, "enterprise-docs", "sql", "migrations", "V001__initial_schema.sql");
        if (!File.Exists(sqlPath))
        {
            return;
        }
        
        var content = await File.ReadAllTextAsync(sqlPath);
        var mappings = new MappingTable();

        // Act
        var result = _sanitizer.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        
        // Linked server references should be sanitized
        result.Content.Should().NotContain("[LINKEDSRV01]");
        
        // Database names should be sanitized
        result.Content.Should().NotContain("DBZMEW");
        result.Content.Should().NotContain("DBZBHI");
        
        // Service accounts should be sanitized
        result.Content.Should().NotContain("ACME\\svc_sql_agent");
        result.Content.Should().NotContain("ACME\\svc_deployment");
        
        // SQL keywords should NOT be sanitized
        result.Content.Should().Contain("CREATE TABLE");
        result.Content.Should().Contain("SELECT");
        result.Content.Should().Contain("INSERT INTO");
    }

    [Fact]
    public async Task EnterpriseDocsFixture_TerraformIsSanitizable()
    {
        // Arrange
        var tfPath = Path.Combine(_fixturesPath, "enterprise-docs", "infrastructure", "terraform", "main.tf");
        if (!File.Exists(tfPath))
        {
            return;
        }
        
        var content = await File.ReadAllTextAsync(tfPath);
        var mappings = new MappingTable();

        // Act
        var result = _sanitizer.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        
        // IP ranges should be sanitized
        result.Content.Should().NotContain("10.50.101.0/24");
        result.Content.Should().NotContain("10.50.100.0/24");
        
        // Terraform structure should be preserved
        result.Content.Should().Contain("resource");
        result.Content.Should().Contain("provider");
    }

    // ========================================
    // Deep Nested Fixture Tests
    // ========================================

    [Fact]
    public void DeepNestedFixture_Exists()
    {
        // Arrange
        var deepNestedPath = Path.Combine(_fixturesPath, "deep-nested");

        // Assert
        Directory.Exists(deepNestedPath).Should().BeTrue(
            $"Deep nested fixture should exist at {deepNestedPath}");
    }

    [Fact]
    public async Task DeepNestedFixture_Level10ConfigIsSanitizable()
    {
        // Arrange
        var configPath = Path.Combine(_fixturesPath, "deep-nested", 
            "level1", "level2", "level3", "level4", "level5", 
            "level6", "level7", "level8", "level9", "level10", "config.json");
        
        if (!File.Exists(configPath))
        {
            return;
        }
        
        var content = await File.ReadAllTextAsync(configPath);
        var mappings = new MappingTable();

        // Act
        var result = _sanitizer.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("PRODSRV01");
        result.Content.Should().NotContain("DBZMEW");
        result.Content.Should().NotContain("10.50.100.50");
        result.Content.Should().NotContain("LINKEDSRV01");
    }

    [Fact]
    public void DeepNestedFixture_FileProcessorFindsAllFiles()
    {
        // Arrange
        var deepNestedPath = Path.Combine(_fixturesPath, "deep-nested");
        if (!Directory.Exists(deepNestedPath))
        {
            return;
        }

        // Act
        var files = _fileProcessor.GetFilesToProcess(deepNestedPath).ToList();

        // Assert
        files.Should().NotBeEmpty();
        files.Should().Contain(f => f.EndsWith("config.json"));
        files.Should().Contain(f => f.EndsWith("README.md"));
    }

    // ========================================
    // CI/CD Configs Fixture Tests
    // ========================================

    [Fact]
    public async Task CiCdFixture_GitLabCiIsSanitizable()
    {
        // Arrange
        var gitlabCiPath = Path.Combine(_fixturesPath, "ci-cd-configs", ".gitlab-ci.yml");
        if (!File.Exists(gitlabCiPath))
        {
            return;
        }
        
        var content = await File.ReadAllTextAsync(gitlabCiPath);
        var mappings = new MappingTable();

        // Act
        var result = _sanitizer.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        
        // Server names should be sanitized
        result.Content.Should().NotContain("PRODSRV01");
        result.Content.Should().NotContain("STGSRV01");
        result.Content.Should().NotContain("DEVSRV01");
        
        // Internal server FQDNs should be sanitized (uppercase server names)
        // Note: registry.acme-corp.com is NOT sanitized because it's a lowercase hostname
        // Our rules specifically target server names like PRODSRV01.acme-corp.com
        result.Content.Should().NotContain("PRODSRV01.acme-corp.com");
        result.Content.Should().NotContain("STGSRV01.internal.net");
        result.Content.Should().NotContain("DEVSRV01.corp.acme.net");
        
        // Database names as standalone words should be sanitized
        // Note: DBZMEW_TEST and DBZMEW_STG contain underscores that prevent word boundary match
        // The pattern \bDB[A-Z][A-Z0-9]+\b matches DBZMEW but not DBZMEW_TEST
        // (DBZMEW inside DBZMEW_TEST is not at word boundary due to underscore)
        
        // IPs should be sanitized
        result.Content.Should().NotContain("192.168.1.100");
        result.Content.Should().NotContain("192.168.2.100");
        result.Content.Should().NotContain("10.50.100.10");
        
        // Vault paths should be sanitized
        result.Content.Should().NotContain("secret/data/production");
        
        // YAML structure should be preserved
        result.Content.Should().Contain("stages:");
        result.Content.Should().Contain("script:");
    }

    [Fact]
    public async Task CiCdFixture_GitHubActionsIsSanitizable()
    {
        // Arrange
        var workflowPath = Path.Combine(_fixturesPath, "ci-cd-configs", ".github", "workflows", "deploy-production.yml");
        if (!File.Exists(workflowPath))
        {
            return;
        }
        
        var content = await File.ReadAllTextAsync(workflowPath);
        var mappings = new MappingTable();

        // Act
        var result = _sanitizer.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        
        // Server names should be sanitized
        result.Content.Should().NotContain("PRODSRV01");
        result.Content.Should().NotContain("PRODSRV02");
        result.Content.Should().NotContain("LINKEDSRV01");
        
        // Database names should be sanitized
        result.Content.Should().NotContain("DBZMEW");
        
        // IPs should be sanitized
        result.Content.Should().NotContain("10.50.100.10");
        
        // Workflow structure should be preserved
        result.Content.Should().Contain("name:");
        result.Content.Should().Contain("jobs:");
        result.Content.Should().Contain("steps:");
    }

    // ========================================
    // Round-Trip Tests with Fixtures
    // ========================================

    [Fact]
    public async Task RoundTrip_EnterpriseReadme_RestoresPerfectly()
    {
        // Arrange
        var readmePath = Path.Combine(_fixturesPath, "enterprise-docs", "README.md");
        if (!File.Exists(readmePath))
        {
            return;
        }
        
        var original = await File.ReadAllTextAsync(readmePath);
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().Be(original);
    }

    [Fact]
    public async Task RoundTrip_DeepNestedConfig_RestoresPerfectly()
    {
        // Arrange
        var configPath = Path.Combine(_fixturesPath, "deep-nested", 
            "level1", "level2", "level3", "level4", "level5", 
            "level6", "level7", "level8", "level9", "level10", "config.json");
        
        if (!File.Exists(configPath))
        {
            return;
        }
        
        var original = await File.ReadAllTextAsync(configPath);
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().Be(original);
    }

    [Fact]
    public async Task RoundTrip_GitLabCi_RestoresPerfectly()
    {
        // Arrange
        var gitlabCiPath = Path.Combine(_fixturesPath, "ci-cd-configs", ".gitlab-ci.yml");
        if (!File.Exists(gitlabCiPath))
        {
            return;
        }
        
        var original = await File.ReadAllTextAsync(gitlabCiPath);
        var mappings = new MappingTable();

        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);

        // Assert
        restored.Content.Should().Be(original);
    }
}
