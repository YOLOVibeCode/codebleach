using CodeBleach.Core.Models;
using CodeBleach.Processors.Sql;
using FluentAssertions;
using Xunit;

namespace CodeBleach.Processors.Sql.Tests;

/// <summary>
/// Comprehensive tests for the SqlLanguageProcessor.
/// Tests cover: metadata, CanProcess, obfuscation of tables, columns, schemas,
/// bracket-delimited identifiers, temp tables, stored procedures, functions,
/// variables, CTEs, cursors, built-in function/data type protection,
/// system schema protection, comments, string literals, round-trip
/// deobfuscation, and validation.
/// </summary>
public class SqlLanguageProcessorTests
{
    private readonly SqlLanguageProcessor _processor = new();

    private static ObfuscationContext CreateContext(ObfuscationLevel level = ObfuscationLevel.Full)
        => new(level);

    // ═══════════════════════════════════════════════════════════════════
    // Metadata Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void ProcessorId_Returns_TSql()
    {
        _processor.ProcessorId.Should().Be("tsql");
    }

    [Fact]
    public void DisplayName_Returns_TSqlScriptDom()
    {
        _processor.DisplayName.Should().Be("T-SQL (ScriptDom)");
    }

    [Theory]
    [InlineData(".sql")]
    [InlineData(".SQL")]
    public void SupportedExtensions_ContainsSql(string ext)
    {
        _processor.SupportedExtensions.Should().Contain(ext);
    }

    // ═══════════════════════════════════════════════════════════════════
    // CanProcess Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void CanProcess_WithSqlExtension_ReturnsTrue()
    {
        _processor.CanProcess("test.sql", "").Should().BeTrue();
    }

    [Fact]
    public void CanProcess_WithUnsupportedExtension_ReturnsFalse()
    {
        _processor.CanProcess("test.cs", "SELECT 1").Should().BeFalse();
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Empty / Whitespace Input
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_EmptyContent_ReturnsUnchanged()
    {
        var ctx = CreateContext();
        var result = _processor.Obfuscate("", ctx, "test.sql");

        result.WasTransformed.Should().BeFalse();
        result.ProcessorId.Should().Be("tsql");
    }

    [Fact]
    public void Obfuscate_WhitespaceOnly_ReturnsUnchanged()
    {
        var ctx = CreateContext();
        var result = _processor.Obfuscate("   ", ctx, "test.sql");

        result.WasTransformed.Should().BeFalse();
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Table Names
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_TableName_IsRenamed()
    {
        var sql = "SELECT * FROM Customers WHERE 1=1";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(sql, ctx, "test.sql");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("Customers");
        result.Content.Should().Contain("TBL_");
    }

    [Fact]
    public void Obfuscate_SchemaQualifiedTable_IsRenamed()
    {
        var sql = "SELECT * FROM sales.CustomerOrders";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(sql, ctx, "test.sql");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("CustomerOrders");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Column Names
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ColumnName_IsRenamed()
    {
        var sql = "SELECT FirstName, LastName FROM Employees";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(sql, ctx, "test.sql");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("FirstName");
        result.Content.Should().NotContain("LastName");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Bracket-Delimited Identifiers
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_BracketDelimitedIdentifier_PreservesBrackets()
    {
        var sql = "SELECT [EmployeeName] FROM [Employees]";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(sql, ctx, "test.sql");

        result.WasTransformed.Should().BeTrue();
        // Output should still contain bracket syntax
        result.Content.Should().Contain("[");
        result.Content.Should().Contain("]");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Temp Tables
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_TempTable_UsesTmpPrefix()
    {
        var sql = "CREATE TABLE #TempOrders (Id INT)";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(sql, ctx, "test.sql");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("#TempOrders");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Stored Procedures
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_CreateProcedure_IsRenamed()
    {
        var sql = "CREATE PROCEDURE usp_CalculateTax AS SELECT 1";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(sql, ctx, "test.sql");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("usp_CalculateTax");
        result.Content.Should().Contain("SP_");
    }

    [Fact]
    public void Obfuscate_ExecProcedure_IsRenamed()
    {
        var sql = "EXEC usp_GetCustomerData";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(sql, ctx, "test.sql");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("usp_GetCustomerData");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Functions
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_CreateFunction_IsRenamed()
    {
        var sql = "CREATE FUNCTION fn_GetTotal(@x INT) RETURNS INT AS BEGIN RETURN @x END";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(sql, ctx, "test.sql");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("fn_GetTotal");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Variables
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_Variable_IsRenamed()
    {
        var sql = "DECLARE @TotalAmount INT = 0; SELECT @TotalAmount = 1";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(sql, ctx, "test.sql");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("@TotalAmount");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - CTEs
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_CTE_NameIsRenamed()
    {
        var sql = "WITH RankedOrders AS (SELECT 1 AS Id) SELECT * FROM RankedOrders";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(sql, ctx, "test.sql");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("RankedOrders");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Cursors
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_Cursor_IsRenamed()
    {
        var sql = @"DECLARE order_cursor CURSOR FOR SELECT 1;
OPEN order_cursor;
CLOSE order_cursor;
DEALLOCATE order_cursor";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(sql, ctx, "test.sql");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("order_cursor");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Built-in Functions Protection
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_BuiltInFunctions_AreNotRenamed()
    {
        var sql = "SELECT COUNT(*), SUM(Amount), GETDATE() FROM Orders";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(sql, ctx, "test.sql");

        result.Content.Should().Contain("COUNT");
        result.Content.Should().Contain("SUM");
        result.Content.Should().Contain("GETDATE");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Data Types Protection
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_DataTypes_AreNotRenamed()
    {
        var sql = "DECLARE @x INT; DECLARE @y VARCHAR(50)";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(sql, ctx, "test.sql");

        result.Content.Should().Contain("INT");
        result.Content.Should().Contain("VARCHAR");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - System Schemas Protection
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_SystemSchemas_AreNotRenamed()
    {
        var sql = "SELECT name FROM sys.objects WHERE type = 'U'";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(sql, ctx, "test.sql");

        result.Content.Should().Contain("sys");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Comments
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_Comments_AreReplaced()
    {
        var sql = "-- This is a secret comment\nSELECT 1";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(sql, ctx, "test.sql");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("secret");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - String Literals
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_StringLiteral_IsObfuscated()
    {
        var sql = "SELECT 'SensitiveData' AS Result";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(sql, ctx, "test.sql");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("SensitiveData");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Round-Trip Test (Obfuscate -> Deobfuscate)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void RoundTrip_SelectWithJoins_RestoresAll()
    {
        var sql = @"SELECT
    c.CustomerName,
    o.OrderDate,
    p.ProductName,
    od.Quantity
FROM Customers c
INNER JOIN Orders o ON c.CustomerId = o.CustomerId
INNER JOIN OrderDetails od ON o.OrderId = od.OrderId
INNER JOIN Products p ON od.ProductId = p.ProductId
WHERE c.CustomerName IS NOT NULL";
        var ctx = CreateContext();

        var obfuscated = _processor.Obfuscate(sql, ctx, "test.sql");
        obfuscated.WasTransformed.Should().BeTrue();
        obfuscated.Content.Should().NotContain("CustomerName");
        obfuscated.Content.Should().NotContain("OrderDate");
        obfuscated.Content.Should().NotContain("ProductName");

        var restored = _processor.Deobfuscate(obfuscated.Content, ctx, "test.sql");
        restored.WasTransformed.Should().BeTrue();
        restored.Content.Should().Contain("CustomerName");
        restored.Content.Should().Contain("OrderDate");
        restored.Content.Should().Contain("ProductName");
        restored.Content.Should().Contain("Quantity");
        restored.Content.Should().Contain("Customers");
        restored.Content.Should().Contain("Orders");
        restored.Content.Should().Contain("Products");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Validate Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Validate_ObfuscatedSql_ReturnsValid()
    {
        var sql = @"SELECT
    c.CustomerName,
    o.OrderDate
FROM Customers c
INNER JOIN Orders o ON c.CustomerId = o.CustomerId";
        var ctx = CreateContext();

        var obfuscated = _processor.Obfuscate(sql, ctx, "test.sql");
        obfuscated.WasTransformed.Should().BeTrue();

        var validation = _processor.Validate(obfuscated.Content);
        validation.IsValid.Should().BeTrue();
    }
}
