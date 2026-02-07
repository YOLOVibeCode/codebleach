using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

/// <summary>
/// Tests for Restorer robustness in LLM round-trip scenarios.
/// Verifies that the word-boundary regex approach survives
/// common LLM reformatting patterns (whitespace changes, method reordering,
/// line break changes) and handles edge cases gracefully.
/// </summary>
public class RestorerRobustnessTests
{
    private readonly IRestorer _restorer = new Restorer();

    private static MappingTable CreateMappings(params (string alias, string original)[] pairs)
    {
        var mappings = new MappingTable();
        foreach (var (alias, original) in pairs)
        {
            mappings.Forward[original] = alias;
            mappings.Reverse[alias] = original;
        }
        return mappings;
    }

    // ═══════════════════════════════════════════════════════════════════
    // LLM Reformatting Scenarios
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Restore_AfterWhitespaceReformatting_StillRestores()
    {
        // Simulate LLM adding extra whitespace around aliases
        var content = "public   class   CLS_0\n{\n    public void   MTD_0()\n    {\n        var   VAR_0 = 42;\n    }\n}";
        var mappings = CreateMappings(
            ("CLS_0", "CustomerService"),
            ("MTD_0", "CalculateTotal"),
            ("VAR_0", "totalAmount"));

        var result = _restorer.Restore(content, mappings);

        result.WasRestored.Should().BeTrue();
        result.Content.Should().Contain("CustomerService");
        result.Content.Should().Contain("CalculateTotal");
        result.Content.Should().Contain("totalAmount");
        result.ReplacementCount.Should().Be(3);
    }

    [Fact]
    public void Restore_AfterMethodReordering_StillRestores()
    {
        // Simulate LLM reordering methods (MTD_1 before MTD_0)
        var content = @"public class CLS_0
{
    public void MTD_1() { }
    public void MTD_0() { }
}";
        var mappings = CreateMappings(
            ("CLS_0", "OrderManager"),
            ("MTD_0", "ProcessOrder"),
            ("MTD_1", "ValidateOrder"));

        var result = _restorer.Restore(content, mappings);

        result.WasRestored.Should().BeTrue();
        result.Content.Should().Contain("OrderManager");
        result.Content.Should().Contain("ProcessOrder");
        result.Content.Should().Contain("ValidateOrder");
        result.ReplacementCount.Should().Be(3);
    }

    [Fact]
    public void Restore_AfterLineBreakChanges_StillRestores()
    {
        // Simulate LLM changing \r\n to \n (or vice versa)
        var contentLf = "class CLS_0\n{\n    void MTD_0()\n    { }\n}";
        var contentCrLf = "class CLS_0\r\n{\r\n    void MTD_0()\r\n    { }\r\n}";
        var mappings = CreateMappings(
            ("CLS_0", "PaymentService"),
            ("MTD_0", "ChargeCard"));

        var resultLf = _restorer.Restore(contentLf, mappings);
        var resultCrLf = _restorer.Restore(contentCrLf, mappings);

        resultLf.Content.Should().Contain("PaymentService");
        resultLf.Content.Should().Contain("ChargeCard");
        resultCrLf.Content.Should().Contain("PaymentService");
        resultCrLf.Content.Should().Contain("ChargeCard");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Partial Restore
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Restore_WhenSomeAliasesMissing_RestoresRemainder()
    {
        // LLM removed MTD_0 but kept CLS_0 and VAR_0
        var content = "class CLS_0 { var VAR_0 = 42; }";
        var mappings = CreateMappings(
            ("CLS_0", "AccountService"),
            ("MTD_0", "Transfer"),
            ("VAR_0", "balance"));

        var result = _restorer.Restore(content, mappings);

        result.WasRestored.Should().BeTrue();
        result.Content.Should().Contain("AccountService");
        result.Content.Should().Contain("balance");
        result.Content.Should().NotContain("CLS_0");
        result.Content.Should().NotContain("VAR_0");
        result.ReplacementCount.Should().Be(2);
    }

    [Fact]
    public void Restore_AliasInCommentOrString_StillRestored()
    {
        // Aliases appearing in comments and string literals should still be restored
        var content = "// Uses CLS_0 for processing\nstring desc = \"See CLS_0 for details\";";
        var mappings = CreateMappings(("CLS_0", "EmployeeService"));

        var result = _restorer.Restore(content, mappings);

        result.WasRestored.Should().BeTrue();
        result.Content.Should().Contain("// Uses EmployeeService for processing");
        result.Content.Should().Contain("\"See EmployeeService for details\"");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Case Sensitivity
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Restore_LowercaseAlias_IsNotMatched()
    {
        // Lowercase "cls_0" should NOT match "CLS_0" (case-sensitive word-boundary)
        var content = "cls_0 is not the same as CLS_0";
        var mappings = CreateMappings(("CLS_0", "UserProfile"));

        var result = _restorer.Restore(content, mappings);

        result.Content.Should().Contain("cls_0 is not the same as UserProfile");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Edge Cases
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Restore_NullContent_DoesNotThrow()
    {
        var mappings = CreateMappings(("CLS_0", "Test"));

        var act = () => _restorer.Restore(null!, mappings);

        act.Should().NotThrow();
    }

    [Fact]
    public void Restore_VeryLargeContent_CompletesInTime()
    {
        // Generate 10,000 lines each containing an alias
        var lines = Enumerable.Range(0, 10_000)
            .Select(i => $"Line {i}: using CLS_0 and MTD_0 and VAR_0 here")
            .ToList();
        var content = string.Join("\n", lines);
        var mappings = CreateMappings(
            ("CLS_0", "LargeService"),
            ("MTD_0", "ProcessBatch"),
            ("VAR_0", "batchSize"));

        var sw = System.Diagnostics.Stopwatch.StartNew();
        var result = _restorer.Restore(content, mappings);
        sw.Stop();

        result.WasRestored.Should().BeTrue();
        result.ReplacementCount.Should().Be(30_000);
        sw.ElapsedMilliseconds.Should().BeLessThan(5000, "restore of 10K lines should complete in under 5 seconds");
    }

    [Fact]
    public void Restore_AliasAtBoundaries_CorrectlyMatched()
    {
        // Alias at start of line, end of line, after punctuation
        var content = "CLS_0\n{MTD_0}\n(VAR_0)";
        var mappings = CreateMappings(
            ("CLS_0", "BoundaryClass"),
            ("MTD_0", "BoundaryMethod"),
            ("VAR_0", "boundaryVar"));

        var result = _restorer.Restore(content, mappings);

        result.WasRestored.Should().BeTrue();
        result.Content.Should().Contain("BoundaryClass");
        result.Content.Should().Contain("{BoundaryMethod}");
        result.Content.Should().Contain("(boundaryVar)");
        result.ReplacementCount.Should().Be(3);
    }

    [Fact]
    public void Restore_OverlappingAliases_LongerWins()
    {
        // SERVER_10 must not be accidentally matched as SERVER_1 + "0"
        var content = "Connect to SERVER_1 and SERVER_10 and SERVER_100";
        var mappings = CreateMappings(
            ("SERVER_1", "StagingDB"),
            ("SERVER_10", "ArchiveDB"),
            ("SERVER_100", "BackupDB"));

        var result = _restorer.Restore(content, mappings);

        result.Content.Should().Be("Connect to StagingDB and ArchiveDB and BackupDB");
        result.ReplacementCount.Should().Be(3);
    }
}
