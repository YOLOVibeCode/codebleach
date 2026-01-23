using System.Diagnostics;
using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using CodeBleach.Core.Rules;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

/// <summary>
/// Comprehensive tests for sanitization algorithm correctness.
/// Verifies determinism, completeness, precision, reversibility, and consistency.
/// </summary>
public class AlgorithmCorrectnessTests
{
    private readonly IRuleRegistry _ruleRegistry;
    private readonly ISanitizer _sanitizer;
    private readonly IRestorer _restorer;

    public AlgorithmCorrectnessTests()
    {
        _ruleRegistry = new RuleRegistry();
        SetupTestRules();
        _sanitizer = new Sanitizer(_ruleRegistry);
        _restorer = new Restorer();
    }

    private void SetupTestRules()
    {
        // Add rules in specific order to test priority
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "fqdn",
            Name = "Server FQDN",
            Description = "Fully qualified domain names",
            Pattern = @"\b[A-Z][A-Z0-9-]+\.(acme-corp|internal)\.(com|net)\b",
            Prefix = "HOST",
            Severity = RuleSeverity.High,
            Order = 1  // Highest priority (longest match)
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "server",
            Name = "Server Name",
            Description = "Server names",
            Pattern = @"\b(PROD|STG|DEV)SRV\d{1,2}\b",
            Prefix = "SERVER",
            Severity = RuleSeverity.High,
            Order = 2
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "ip_10",
            Name = "Private IP 10.x",
            Description = "10.x.x.x addresses",
            Pattern = @"\b10\.\d{1,3}\.\d{1,3}\.\d{1,3}\b",
            Prefix = "IP",
            Severity = RuleSeverity.High,
            Order = 3
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "db_name",
            Name = "Database Name",
            Description = "Database names",
            Pattern = @"\bDB[A-Z][A-Z0-9]+\b",
            Prefix = "DB",
            Severity = RuleSeverity.Medium,
            Order = 4
        });
    }

    // ================================================================
    // SECTION 1: DETERMINISM TESTS
    // ================================================================

    [Fact]
    public void Algorithm_IsDeterministic_SameInputSameOutput()
    {
        // Arrange
        var content = "Server PRODSRV01 at 10.0.0.1 hosts DBZMEW";
        
        // Act - Run multiple times
        var results = new List<string>();
        for (int i = 0; i < 10; i++)
        {
            var mappings = new MappingTable();
            var result = _sanitizer.Sanitize(content, mappings);
            results.Add(result.Content);
        }
        
        // Assert - All results identical
        results.Should().AllBe(results[0]);
    }

    [Fact]
    public void Algorithm_AliasGeneration_IsDeterministic()
    {
        // Arrange
        var content = "A: PRODSRV01, B: STGSRV01, C: DEVSRV01";
        
        // Act
        var mappings1 = new MappingTable();
        var result1 = _sanitizer.Sanitize(content, mappings1);
        
        var mappings2 = new MappingTable();
        var result2 = _sanitizer.Sanitize(content, mappings2);
        
        // Assert - Same aliases in same order
        mappings1.Forward.Should().BeEquivalentTo(mappings2.Forward);
        result1.Content.Should().Be(result2.Content);
    }

    // ================================================================
    // SECTION 2: OVERLAPPING PATTERN TESTS
    // ================================================================

    [Fact]
    public void Overlapping_LongerMatchWins()
    {
        // Arrange - FQDN pattern should win over server name pattern
        var content = "Connect to PRODSRV01.acme-corp.com";
        var mappings = new MappingTable();
        
        // Act
        var result = _sanitizer.Sanitize(content, mappings);
        
        // Assert - Should use HOST prefix (FQDN), not SERVER prefix
        result.Content.Should().Contain("HOST_");
        result.Content.Should().NotContain("SERVER_");
        
        // Only one match (the FQDN), not two overlapping matches
        result.Matches.Should().HaveCount(1);
    }

    [Fact]
    public void Overlapping_NonOverlappingBothMatch()
    {
        // Arrange - Server name and IP don't overlap
        var content = "PRODSRV01 at 10.0.0.1";
        var mappings = new MappingTable();
        
        // Act
        var result = _sanitizer.Sanitize(content, mappings);
        
        // Assert - Both should be sanitized
        result.Content.Should().Contain("SERVER_0");
        result.Content.Should().Contain("IP_0");
        result.Matches.Should().HaveCount(2);
    }

    [Fact]
    public void Overlapping_AdjacentPatternsNoConflict()
    {
        // Arrange - Adjacent but not overlapping
        var content = "PRODSRV01:1433";
        var mappings = new MappingTable();
        
        // Act
        var result = _sanitizer.Sanitize(content, mappings);
        
        // Assert - Server name sanitized, port number unchanged
        result.Content.Should().Be("SERVER_0:1433");
    }

    // ================================================================
    // SECTION 3: ALIAS CONSISTENCY TESTS
    // ================================================================

    [Fact]
    public void Consistency_SameValueSameAlias()
    {
        // Arrange
        var content = "First: PRODSRV01, Second: PRODSRV01, Third: PRODSRV01";
        var mappings = new MappingTable();
        
        // Act
        var result = _sanitizer.Sanitize(content, mappings);
        
        // Assert - All occurrences use same alias
        var aliasCount = result.Content.Split("SERVER_0").Length - 1;
        aliasCount.Should().Be(3);
        result.Content.Should().NotContain("SERVER_1");
    }

    [Fact]
    public void Consistency_DifferentValuesDifferentAliases()
    {
        // Arrange
        var content = "A: PRODSRV01, B: STGSRV01, C: DEVSRV01";
        var mappings = new MappingTable();
        
        // Act
        var result = _sanitizer.Sanitize(content, mappings);
        
        // Assert - Three different aliases
        result.Content.Should().Contain("SERVER_0");
        result.Content.Should().Contain("SERVER_1");
        result.Content.Should().Contain("SERVER_2");
        mappings.Forward.Should().HaveCount(3);
    }

    [Fact]
    public void Consistency_DifferentPrefixesIndependentCounters()
    {
        // Arrange
        var content = "Server: PRODSRV01, IP: 10.0.0.1, Server2: STGSRV01, IP2: 10.0.0.2";
        var mappings = new MappingTable();
        
        // Act
        var result = _sanitizer.Sanitize(content, mappings);
        
        // Assert - Each prefix has independent counter
        result.Content.Should().Contain("SERVER_0");
        result.Content.Should().Contain("SERVER_1");
        result.Content.Should().Contain("IP_0");
        result.Content.Should().Contain("IP_1");
        
        // Counters should be: SERVER=2, IP=2
        mappings.Counters["SERVER"].Should().Be(2);
        mappings.Counters["IP"].Should().Be(2);
    }

    [Fact]
    public void Consistency_PrePopulatedMappingsReused()
    {
        // Arrange - Pre-populate mapping
        var mappings = new MappingTable();
        mappings.GetOrCreateAlias("PRODSRV01", "SERVER");
        
        var content = "Connect to PRODSRV01";
        
        // Act
        var result = _sanitizer.Sanitize(content, mappings);
        
        // Assert - Uses existing alias
        result.Content.Should().Be("Connect to SERVER_0");
        mappings.Counters["SERVER"].Should().Be(1); // Not incremented
    }

    // ================================================================
    // SECTION 4: BIDIRECTIONAL MAPPING TESTS
    // ================================================================

    [Fact]
    public void Bidirectional_ForwardMappingCorrect()
    {
        // Arrange
        var mappings = new MappingTable();
        
        // Act
        var alias = mappings.GetOrCreateAlias("PRODSRV01", "SERVER");
        
        // Assert
        mappings.Forward["PRODSRV01"].Should().Be("SERVER_0");
    }

    [Fact]
    public void Bidirectional_ReverseMappingCorrect()
    {
        // Arrange
        var mappings = new MappingTable();
        
        // Act
        var alias = mappings.GetOrCreateAlias("PRODSRV01", "SERVER");
        
        // Assert
        mappings.Reverse["SERVER_0"].Should().Be("PRODSRV01");
    }

    [Fact]
    public void Bidirectional_ConsistencyHolds()
    {
        // Arrange
        var mappings = new MappingTable();
        var originalValues = new[] { "PRODSRV01", "STGSRV01", "DEVSRV01", "DBZMEW", "10.0.0.1" };
        var prefixes = new[] { "SERVER", "SERVER", "SERVER", "DB", "IP" };
        
        // Act
        for (int i = 0; i < originalValues.Length; i++)
        {
            mappings.GetOrCreateAlias(originalValues[i], prefixes[i]);
        }
        
        // Assert - Forward and Reverse are inverses
        foreach (var (original, alias) in mappings.Forward)
        {
            mappings.Reverse[alias].Should().Be(original);
        }
        
        foreach (var (alias, original) in mappings.Reverse)
        {
            mappings.Forward[original].Should().Be(alias);
        }
    }

    // ================================================================
    // SECTION 5: ROUND-TRIP INTEGRITY TESTS
    // ================================================================

    [Theory]
    [InlineData("Simple text with PRODSRV01")]
    [InlineData("Multiple: PRODSRV01, STGSRV01, DEVSRV01")]
    [InlineData("Mixed: PRODSRV01 at 10.0.0.1 with DBZMEW")]
    [InlineData("FQDN: PRODSRV01.acme-corp.com")]
    [InlineData("Repeated: PRODSRV01 PRODSRV01 PRODSRV01")]
    public void RoundTrip_IdentityPropertyHolds(string original)
    {
        // Arrange
        var mappings = new MappingTable();
        
        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);
        
        // Assert - Identity: Restore(Sanitize(x)) = x
        restored.Content.Should().Be(original);
    }

    [Fact]
    public void RoundTrip_MultipleCyclesPreserveIdentity()
    {
        // Arrange
        var original = "Server: PRODSRV01, IP: 10.0.0.1, DB: DBZMEW";
        var current = original;
        
        // Act - 10 sanitize/restore cycles
        for (int i = 0; i < 10; i++)
        {
            var mappings = new MappingTable();
            var sanitized = _sanitizer.Sanitize(current, mappings);
            var restored = _restorer.Restore(sanitized.Content, mappings);
            current = restored.Content;
        }
        
        // Assert
        current.Should().Be(original);
    }

    [Fact]
    public void RoundTrip_NoInformationLoss()
    {
        // Arrange
        var original = "Config: PRODSRV01, STGSRV01; IPs: 10.0.0.1, 10.0.0.2; DBs: DBZMEW, DBZBHI";
        var mappings = new MappingTable();
        
        // Act
        var sanitized = _sanitizer.Sanitize(original, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);
        
        // Assert
        restored.Content.Should().Be(original);
        
        // Verify all original values are in mappings
        mappings.Forward.Keys.Should().Contain("PRODSRV01");
        mappings.Forward.Keys.Should().Contain("STGSRV01");
        mappings.Forward.Keys.Should().Contain("10.0.0.1");
        mappings.Forward.Keys.Should().Contain("10.0.0.2");
        mappings.Forward.Keys.Should().Contain("DBZMEW");
        mappings.Forward.Keys.Should().Contain("DBZBHI");
    }

    // ================================================================
    // SECTION 6: BOUNDARY CONDITION TESTS
    // ================================================================

    [Theory]
    [InlineData("PRODSRV01", true, "Standalone word")]
    [InlineData("PRODSRV01:1433", true, "Colon boundary")]
    [InlineData("PRODSRV01.domain", true, "Dot boundary")]
    [InlineData("(PRODSRV01)", true, "Parentheses boundary")]
    [InlineData("[PRODSRV01]", true, "Brackets boundary")]
    [InlineData("\"PRODSRV01\"", true, "Quotes boundary")]
    public void Boundary_WordBoundariesRespected(string input, bool shouldMatch, string scenario)
    {
        // Arrange
        var mappings = new MappingTable();
        
        // Act
        var result = _sanitizer.Sanitize(input, mappings);
        
        // Assert
        if (shouldMatch)
        {
            result.WasSanitized.Should().BeTrue(scenario);
            result.Content.Should().NotContain("PRODSRV01", scenario);
        }
    }

    [Fact]
    public void Boundary_AtStartOfContent()
    {
        // Arrange
        var content = "PRODSRV01 is the primary server";
        var mappings = new MappingTable();
        
        // Act
        var result = _sanitizer.Sanitize(content, mappings);
        
        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().StartWith("SERVER_0");
    }

    [Fact]
    public void Boundary_AtEndOfContent()
    {
        // Arrange
        var content = "Primary server is PRODSRV01";
        var mappings = new MappingTable();
        
        // Act
        var result = _sanitizer.Sanitize(content, mappings);
        
        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().EndWith("SERVER_0");
    }

    [Theory]
    [InlineData("")]
    [InlineData("   ")]
    [InlineData("\n\n\n")]
    [InlineData("\t\t\t")]
    public void Boundary_EmptyOrWhitespaceReturnsUnchanged(string content)
    {
        // Arrange
        var mappings = new MappingTable();
        
        // Act
        var result = _sanitizer.Sanitize(content, mappings);
        
        // Assert
        result.Content.Should().Be(content);
        result.WasSanitized.Should().BeFalse();
    }

    // ================================================================
    // SECTION 7: SPECIAL CHARACTER TESTS
    // ================================================================

    [Theory]
    [InlineData("Server PRODSRV01 🖥️", "Emoji")]
    [InlineData("服务器: PRODSRV01", "Chinese")]
    [InlineData("Сервер: PRODSRV01", "Cyrillic")]
    [InlineData("שרת: PRODSRV01", "Hebrew")]
    public void SpecialChars_UnicodeHandledCorrectly(string content, string scenario)
    {
        // Arrange
        var mappings = new MappingTable();
        
        // Act
        var sanitized = _sanitizer.Sanitize(content, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);
        
        // Assert
        restored.Content.Should().Be(content, scenario);
    }

    [Theory]
    [InlineData("PRODSRV01 (primary)")]
    [InlineData("PRODSRV01 [main]")]
    [InlineData("PRODSRV01 {server}")]
    [InlineData("path/to/PRODSRV01")]
    [InlineData("PRODSRV01?query=1")]
    [InlineData("PRODSRV01#anchor")]
    public void SpecialChars_RegexSpecialCharsHandled(string content)
    {
        // Arrange
        var mappings = new MappingTable();
        
        // Act
        var sanitized = _sanitizer.Sanitize(content, mappings);
        
        // Assert
        sanitized.Content.Should().NotContain("PRODSRV01");
        sanitized.WasSanitized.Should().BeTrue();
    }

    [Fact]
    public void SpecialChars_NewlinesProcessedCorrectly()
    {
        // Arrange
        var content = "Line1: PRODSRV01\nLine2: STGSRV01\nLine3: DEVSRV01";
        var mappings = new MappingTable();
        
        // Act
        var result = _sanitizer.Sanitize(content, mappings);
        
        // Assert
        result.Matches.Should().HaveCount(3);
        
        // Verify line numbers
        result.Matches.Select(m => m.LineNumber).Should().Equal(1, 2, 3);
    }

    // ================================================================
    // SECTION 8: ALIAS FORMAT TESTS
    // ================================================================

    [Theory]
    [InlineData("SERVER", 0, "SERVER_0")]
    [InlineData("SERVER", 1, "SERVER_1")]
    [InlineData("IP", 0, "IP_0")]
    [InlineData("DB", 5, "DB_5")]
    [InlineData("HOST", 99, "HOST_99")]
    public void AliasFormat_CorrectFormat(string prefix, int index, string expectedAlias)
    {
        // Arrange
        var mappings = new MappingTable();
        
        // Set counter to expected index
        for (int i = 0; i < index; i++)
        {
            mappings.GetOrCreateAlias($"dummy{i}", prefix);
        }
        
        // Act
        var alias = mappings.GetOrCreateAlias($"value{index}", prefix);
        
        // Assert
        alias.Should().Be(expectedAlias);
    }

    [Fact]
    public void AliasFormat_AllAliasesUnique()
    {
        // Arrange
        var mappings = new MappingTable();
        
        // Act - Create 100 aliases
        for (int i = 0; i < 100; i++)
        {
            mappings.GetOrCreateAlias($"Server{i}", "SERVER");
        }
        
        // Assert
        mappings.Forward.Values.Should().OnlyHaveUniqueItems();
        mappings.Reverse.Keys.Should().OnlyHaveUniqueItems();
    }

    // ================================================================
    // SECTION 9: PERFORMANCE TESTS
    // ================================================================

    [Fact]
    public void Performance_LargeContentCompletesInTime()
    {
        // Arrange - 1,000 lines with sensitive data (realistic document size)
        var lines = Enumerable.Range(0, 1000)
            .Select(i => $"Line {i}: Server=PRODSRV01, IP=10.0.0.{i % 256}, DB=DBZMEW");
        var content = string.Join("\n", lines);
        var mappings = new MappingTable();
        
        // Act
        var sw = Stopwatch.StartNew();
        var result = _sanitizer.Sanitize(content, mappings);
        sw.Stop();
        
        // Assert - 1000 lines should complete within 3 seconds
        sw.ElapsedMilliseconds.Should().BeLessThan(3000, "1000 lines should process within 3 seconds");
        result.WasSanitized.Should().BeTrue();
        
        // Note: For very large files (10k+ lines), consider chunking or streaming
        // Current algorithm is O(n*m) where n=content length, m=number of rules
    }

    [Fact]
    public void Performance_ManyUniqueValuesHandled()
    {
        // Arrange - 1000 unique server names
        var lines = Enumerable.Range(0, 1000)
            .Select(i => $"Server: PRODSRV{i:D2}");
        var content = string.Join("\n", lines);
        var mappings = new MappingTable();
        
        // Note: Only PRODSRV01-PRODSRV99 will match our pattern
        
        // Act
        var sw = Stopwatch.StartNew();
        var result = _sanitizer.Sanitize(content, mappings);
        sw.Stop();
        
        // Assert
        sw.ElapsedMilliseconds.Should().BeLessThan(2000);
    }

    // ================================================================
    // SECTION 10: COMPLETENESS TESTS
    // ================================================================

    [Fact]
    public void Completeness_AllMatchesFound()
    {
        // Arrange - Content with known number of matches
        var content = @"
Server 1: PRODSRV01
Server 2: STGSRV01
Server 3: DEVSRV01
IP 1: 10.0.0.1
IP 2: 10.0.0.2
DB 1: DBZMEW
DB 2: DBZBHI
";
        var mappings = new MappingTable();
        
        // Act
        var result = _sanitizer.Sanitize(content, mappings);
        
        // Assert - All sensitive data removed
        result.Content.Should().NotContain("PRODSRV01");
        result.Content.Should().NotContain("STGSRV01");
        result.Content.Should().NotContain("DEVSRV01");
        result.Content.Should().NotContain("10.0.0.1");
        result.Content.Should().NotContain("10.0.0.2");
        result.Content.Should().NotContain("DBZMEW");
        result.Content.Should().NotContain("DBZBHI");
        
        // Verify match count
        result.Matches.Should().HaveCount(7);
    }

    [Fact]
    public void Completeness_NoFalsePositives()
    {
        // Arrange - Content that should NOT match
        var content = @"
Normal text without sensitive data.
Numbers like 123 and words like production.
Public IPs like 8.8.8.8 should not match.
localhost and 127.0.0.1 are fine.
";
        var mappings = new MappingTable();
        
        // Act
        var result = _sanitizer.Sanitize(content, mappings);
        
        // Assert - No changes
        result.Content.Should().Be(content);
        result.WasSanitized.Should().BeFalse();
        result.Matches.Should().BeEmpty();
    }
}
