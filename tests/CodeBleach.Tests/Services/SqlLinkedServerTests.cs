using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using CodeBleach.Core.Rules;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

/// <summary>
/// Tests for SQL linked server and four-part naming sanitization.
/// Covers [Server].[Database].[Schema].[Table] patterns common in enterprise SQL.
/// </summary>
public class SqlLinkedServerTests
{
    private readonly IRuleRegistry _ruleRegistry;
    private readonly ISanitizer _sut;

    public SqlLinkedServerTests()
    {
        _ruleRegistry = new RuleRegistry();
        
        // Add SQL-specific rules in order of specificity (longest patterns first)
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "sql_4part_bracketed",
            Name = "SQL 4-Part Bracketed Name",
            Description = "Detects [Server].[Database].[Schema].[Table] patterns",
            Pattern = @"\[[A-Z][A-Z0-9_]+\]\.\[[A-Z][A-Z0-9_]+\]\.\[[a-z]+\]\.\[[A-Z][A-Z0-9_]+\]",
            Prefix = "LINKED",
            Severity = RuleSeverity.High,
            Order = 1
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "sql_4part_unbracketed",
            Name = "SQL 4-Part Unbracketed Name",
            Description = "Detects Server.Database.Schema.Table patterns",
            Pattern = @"\b[A-Z][A-Z0-9_]+\.[A-Z][A-Z0-9_]+\.dbo\.[A-Z][A-Z0-9_]+\b",
            Prefix = "LINKED",
            Severity = RuleSeverity.High,
            Order = 2
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "sql_3part_dbo",
            Name = "SQL 3-Part Name (dbo)",
            Description = "Detects Database.dbo.Table patterns",
            Pattern = @"\b[A-Z][A-Z0-9_]+\.dbo\.[A-Z][A-Z0-9_]+\b",
            Prefix = "SCHEMA",
            Severity = RuleSeverity.High,
            Order = 3
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "sql_2part",
            Name = "SQL 2-Part Name",
            Description = "Detects Database.Table patterns",
            Pattern = @"\b[A-Z][A-Z0-9_]+\.[A-Z][A-Z0-9_]+\b",
            Prefix = "SCHEMA",
            Severity = RuleSeverity.Medium,
            Order = 4
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "sql_table_prefixed",
            Name = "SQL Prefixed Table",
            Description = "Detects TB/TA prefixed table names",
            Pattern = @"\b(TB|TA)\d{5,6}\b",
            Prefix = "TBL",
            Severity = RuleSeverity.Medium,
            Order = 5
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "sql_server_name",
            Name = "SQL Server Name",
            Description = "Detects SQL server names",
            Pattern = @"\b(PROD|STG|DEV|LINKED)S(RV|VR)\d{1,2}\b",
            Prefix = "SERVER",
            Severity = RuleSeverity.High,
            Order = 6
        });
        
        _ruleRegistry.AddRule(new SanitizationRule
        {
            RuleId = "sql_db_name",
            Name = "SQL Database Name",
            Description = "Detects database names",
            Pattern = @"\bDB[A-Z][A-Z0-9]+\b",
            Prefix = "DB",
            Severity = RuleSeverity.High,
            Order = 7
        });
        
        _sut = new Sanitizer(_ruleRegistry);
    }

    // ========================================
    // Four-Part Bracketed Names
    // ========================================

    [Fact]
    public void Sanitize_FourPartBracketed_ReplacesEntirePattern()
    {
        // Arrange
        var content = "SELECT * FROM [LINKEDSRV01].[DBZMEW].[dbo].[TB00123]";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("[LINKEDSRV01]");
        result.Content.Should().NotContain("[DBZMEW]");
        result.Content.Should().NotContain("[TB00123]");
        result.Content.Should().Contain("LINKED_");
    }

    [Fact]
    public void Sanitize_MultipleFourPartBracketed_ReplacesAll()
    {
        // Arrange
        var content = @"SELECT a.*, b.*
FROM [LINKEDSRV01].[DBZMEW].[dbo].[TB00123] a
JOIN [LINKEDSRV02].[DBZBHI].[dbo].[TB00456] b ON a.ID = b.ID";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("LINKEDSRV01");
        result.Content.Should().NotContain("LINKEDSRV02");
        result.Content.Should().NotContain("DBZMEW");
        result.Content.Should().NotContain("DBZBHI");
    }

    // ========================================
    // Four-Part Unbracketed Names
    // ========================================

    [Fact]
    public void Sanitize_FourPartUnbracketed_ReplacesEntirePattern()
    {
        // Arrange
        var content = "SELECT * FROM LINKEDSRV01.DBZMEW.dbo.TB00123";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("LINKEDSRV01.DBZMEW.dbo.TB00123");
    }

    // ========================================
    // Three-Part Names (Database.dbo.Table)
    // ========================================

    [Fact]
    public void Sanitize_ThreePartDbo_ReplacesPattern()
    {
        // Arrange
        var content = "SELECT * FROM DBZMEW.dbo.TB00123";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("DBZMEW.dbo.TB00123");
        result.Content.Should().Contain("SCHEMA_");
    }

    [Fact]
    public void Sanitize_MultipleThreePart_ReplacesAll()
    {
        // Arrange
        var content = @"INSERT INTO DBZBHI.dbo.RESULTS
SELECT * FROM DBZMEW.dbo.SOURCE";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("DBZBHI.dbo.RESULTS");
        result.Content.Should().NotContain("DBZMEW.dbo.SOURCE");
    }

    // ========================================
    // Two-Part Names (Database.Table)
    // ========================================

    [Fact]
    public void Sanitize_TwoPart_ReplacesPattern()
    {
        // Arrange
        var content = "SELECT * FROM DBZMEW.EMPLOYEES";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("DBZMEW.EMPLOYEES");
    }

    // ========================================
    // Prefixed Table Names (TB00123, TA09052)
    // ========================================

    [Fact]
    public void Sanitize_PrefixedTable_ReplacesPattern()
    {
        // Arrange
        var content = "SELECT * FROM TB00123";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("TB00123");
        result.Content.Should().Contain("TBL_");
    }

    [Fact]
    public void Sanitize_MultiplePrefixedTables_ReplacesAll()
    {
        // Arrange
        var content = @"SELECT e.*, d.*, s.*
FROM TB00123 e
JOIN TB00456 d ON e.DEPT_ID = d.DEPT_ID
JOIN TB00999 s ON e.EMP_ID = s.EMP_ID";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("TB00123");
        result.Content.Should().NotContain("TB00456");
        result.Content.Should().NotContain("TB00999");
    }

    // ========================================
    // Complex SQL Queries
    // ========================================

    [Fact]
    public void Sanitize_ComplexJoinQuery_SanitizesAllReferences()
    {
        // Arrange
        var content = @"-- Cross-database financial report
SELECT 
    e.EMP_ID,
    e.FIRST_NM,
    e.LAST_NM,
    d.DEPT_NM,
    s.SALARY_AMT,
    bi.PERFORMANCE_SCORE
FROM DBZMEW.dbo.TB00123 e
JOIN DBZMEW.dbo.TB00456 d ON e.DEPT_ID = d.DEPT_ID
JOIN DBZMEW.dbo.TB00999 s ON e.EMP_ID = s.EMP_ID
LEFT JOIN [LINKEDSRV01].[DBZBHI].[dbo].[BI_METRICS] bi ON e.EMP_ID = bi.EMP_ID
WHERE e.ACTIVE_IND = 'Y'
  AND s.EFFECTIVE_DT = (
      SELECT MAX(s2.EFFECTIVE_DT) 
      FROM DBZMEW.dbo.TB00999 s2 
      WHERE s2.EMP_ID = e.EMP_ID
  );";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        
        // All database/table references should be sanitized
        result.Content.Should().NotContain("DBZMEW");
        result.Content.Should().NotContain("DBZBHI");
        result.Content.Should().NotContain("TB00123");
        result.Content.Should().NotContain("TB00456");
        result.Content.Should().NotContain("TB00999");
        result.Content.Should().NotContain("LINKEDSRV01");
        
        // SQL structure should be preserved
        result.Content.Should().Contain("SELECT");
        result.Content.Should().Contain("JOIN");
        result.Content.Should().Contain("WHERE");
        
        // Column names should NOT be sanitized
        result.Content.Should().Contain("EMP_ID");
        result.Content.Should().Contain("FIRST_NM");
        result.Content.Should().Contain("DEPT_NM");
    }

    [Fact]
    public void Sanitize_InsertSelectQuery_SanitizesSourceAndDest()
    {
        // Arrange
        var content = @"INSERT INTO [LINKEDSRV01].[DBZBHI].[dbo].[FINANCIAL_REPORT] (
    FISCAL_YEAR, DEPT_ID, TOTAL_SALARY
)
SELECT 
    YEAR(s.EFFECTIVE_DT),
    e.DEPT_ID,
    SUM(s.SALARY_AMT)
FROM DBZMEW.dbo.TB00123 e
JOIN DBZMEW.dbo.TB00999 s ON e.EMP_ID = s.EMP_ID
GROUP BY YEAR(s.EFFECTIVE_DT), e.DEPT_ID;";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("LINKEDSRV01");
        result.Content.Should().NotContain("DBZBHI");
        result.Content.Should().NotContain("DBZMEW");
    }

    [Fact]
    public void Sanitize_StoredProcedureCall_SanitizesParameters()
    {
        // Arrange
        var content = @"EXEC [LINKEDSRV01].[DBZBHI].[dbo].[sp_RefreshMetrics]
    @SourceDB = 'DBZMEW',
    @SourceServer = 'PRODSRV01';";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.WasSanitized.Should().BeTrue();
        result.Content.Should().NotContain("LINKEDSRV01");
        result.Content.Should().NotContain("DBZBHI");
        result.Content.Should().NotContain("DBZMEW");
        result.Content.Should().NotContain("PRODSRV01");
    }

    // ========================================
    // Consistency Tests
    // ========================================

    [Fact]
    public void Sanitize_SameTableMultipleTimes_UsesSameAlias()
    {
        // Arrange
        var content = @"SELECT * FROM TB00123;
UPDATE TB00123 SET STATUS = 'A';
DELETE FROM TB00123 WHERE ID = 1;";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        // Count occurrences of the alias
        var aliasForTB00123 = mappings.Forward["TB00123"];
        var aliasCount = result.Content.Split(aliasForTB00123).Length - 1;
        aliasCount.Should().Be(3, "same table should use same alias 3 times");
    }

    [Fact]
    public void Sanitize_DifferentTables_UseDifferentAliases()
    {
        // Arrange
        var content = @"SELECT * FROM TB00123;
SELECT * FROM TB00456;
SELECT * FROM TB00789;";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        mappings.Forward.Should().HaveCount(3);
        mappings.Forward.Values.Should().OnlyHaveUniqueItems();
    }

    // ========================================
    // Edge Cases
    // ========================================

    [Fact]
    public void Sanitize_EmptyQuery_ReturnsEmpty()
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
    public void Sanitize_QueryWithNoSensitiveData_ReturnsUnchanged()
    {
        // Arrange
        var content = "SELECT 1 + 1 AS Result;";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.Content.Should().Be(content);
        result.WasSanitized.Should().BeFalse();
    }

    [Fact]
    public void Sanitize_QueryWithComments_PreservesComments()
    {
        // Arrange
        var content = @"-- Query against DBZMEW
SELECT * FROM TB00123 -- Employee table";
        var mappings = new MappingTable();

        // Act
        var result = _sut.Sanitize(content, mappings);

        // Assert
        result.Content.Should().Contain("-- Query against");
        result.Content.Should().Contain("-- Employee table");
    }
}
