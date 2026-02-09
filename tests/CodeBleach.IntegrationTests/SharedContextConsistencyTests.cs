using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using CodeBleach.Core.Services;
using CodeBleach.Processors.Cobol;
using CodeBleach.Processors.CSharp;
using CodeBleach.Processors.FSharp;
using CodeBleach.Processors.JavaScript;
using CodeBleach.Processors.Jcl;
using CodeBleach.Processors.Sql;
using CodeBleach.Processors.VisualBasic;
using FluentAssertions;
using Xunit;

namespace CodeBleach.IntegrationTests;

/// <summary>
/// Shared ObfuscationContext consistency tests.
/// Verifies the core guarantee: the same identifier gets the same alias
/// across files, languages, and delegation paths.
/// </summary>
public class SharedContextConsistencyTests
{
    private static ObfuscationContext CreateContextWithRegistry()
    {
        var context = new ObfuscationContext(ObfuscationLevel.Full);
        var registry = new LanguageProcessorRegistry();
        registry.Register(new SqlLanguageProcessor());
        registry.Register(new CSharpLanguageProcessor());
        registry.Register(new VisualBasicLanguageProcessor());
        registry.Register(new JavaScriptLanguageProcessor());
        ILanguageProcessor fsharp = new FSharpLanguageProcessor();
        registry.Register(fsharp);
        registry.Register(new CobolLanguageProcessor());
        registry.Register(new JclLanguageProcessor());
        context.ProcessorRegistry = registry;
        return context;
    }

    private static string BuildCobolSource(params string[] lines)
        => string.Join("\n", lines);

    // ═══════════════════════════════════════════════════════════════════
    // Cross-Language Alias Consistency
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void SharedContext_CobolAndSql_SameTableGetsSameAlias()
    {
        var context = CreateContextWithRegistry();
        var cobolProcessor = new CobolLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        // COBOL program with EXEC SQL referencing EMPLOYEE_MASTER
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. EMPFETCH.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-EMP-ID              PIC 9(5).",
            "       PROCEDURE DIVISION.",
            "       FETCH-EMP.",
            "           EXEC SQL",
            "             SELECT EMP_NAME FROM EMPLOYEE_MASTER",
            "             WHERE EMP_ID = :WS-EMP-ID",
            "           END-EXEC",
            "           STOP RUN.");
        cobolProcessor.Obfuscate(cobol, context, "empfetch.cbl");

        // Standalone SQL referencing the same table
        var sql = "SELECT EMP_NAME, EMP_SALARY FROM EMPLOYEE_MASTER WHERE DEPT_CODE = 'IT'";
        sqlProcessor.Obfuscate(sql, context, "employees.sql");

        // EMPLOYEE_MASTER should have exactly one Forward entry (same alias)
        context.Mappings.Forward.Should().ContainKey("EMPLOYEE_MASTER");
        var alias = context.Mappings.Forward["EMPLOYEE_MASTER"];
        context.Mappings.Reverse.Should().ContainKey(alias);
        context.Mappings.Reverse[alias].Should().Be("EMPLOYEE_MASTER");

        // Should NOT have duplicate entries
        context.Mappings.Forward.Values.Count(v => v == alias).Should().Be(1);
    }

    [Fact]
    public void SharedContext_CSharpAndJavaScript_SameClassNameConsistent()
    {
        var context = CreateContextWithRegistry();
        var csharpProcessor = new CSharpLanguageProcessor();
        var jsProcessor = new JavaScriptLanguageProcessor();

        // C# class
        csharpProcessor.Obfuscate(
            "public class ShoppingCart { }", context, "ShoppingCart.cs");

        // JavaScript class with the same name
        jsProcessor.Obfuscate(
            "class ShoppingCart { constructor() { this.items = []; } }",
            context, "cart.js");

        // Both should share the same alias
        context.Mappings.Forward.Should().ContainKey("ShoppingCart");
        var alias = context.Mappings.Forward["ShoppingCart"];
        alias.Should().StartWith("CLS_");

        // Only one Forward entry for ShoppingCart
        context.Mappings.Forward.Keys.Count(k => k == "ShoppingCart").Should().Be(1);
    }

    [Fact]
    public void SharedContext_ThreeLanguages_SameColumnNameConsistent()
    {
        var context = CreateContextWithRegistry();
        var sqlProcessor = new SqlLanguageProcessor();
        var cobolProcessor = new CobolLanguageProcessor();
        var jsProcessor = new JavaScriptLanguageProcessor();

        // Standalone SQL
        sqlProcessor.Obfuscate(
            "SELECT CustomerName, OrderTotal FROM CustomerOrders",
            context, "report.sql");
        var aliasAfterSql = context.Mappings.Forward["CustomerName"];

        // COBOL with EXEC SQL referencing the same column
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. CUSTPGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-CUST-NAME           PIC X(40).",
            "       PROCEDURE DIVISION.",
            "       FETCH-CUST.",
            "           EXEC SQL",
            "             SELECT CustomerName INTO :WS-CUST-NAME",
            "             FROM CustomerOrders",
            "           END-EXEC",
            "           STOP RUN.");
        cobolProcessor.Obfuscate(cobol, context, "custpgm.cbl");

        // JavaScript with SQL-in-string referencing the same column
        jsProcessor.Obfuscate(
            "const q = 'SELECT CustomerName, OrderDate FROM CustomerOrders WHERE Active = 1';",
            context, "api.js");

        // All three should produce the same alias for "CustomerName"
        context.Mappings.Forward["CustomerName"].Should().Be(aliasAfterSql);
    }

    [Fact]
    public void SharedContext_SqlViaCobolAndStandalone_IdenticalAliases()
    {
        var context = CreateContextWithRegistry();
        var cobolProcessor = new CobolLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        // COBOL EXEC SQL delegates to SQL processor
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. LOOKUP.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       DO-LOOKUP.",
            "           EXEC SQL",
            "             SELECT ACCT_BALANCE FROM ACCOUNT_LEDGER",
            "           END-EXEC",
            "           STOP RUN.");
        cobolProcessor.Obfuscate(cobol, context, "lookup.cbl");
        var aliasViaCobol = context.Mappings.Forward["ACCOUNT_LEDGER"];

        // Standalone SQL references the same table
        sqlProcessor.Obfuscate(
            "SELECT ACCT_BALANCE, ACCT_TYPE FROM ACCOUNT_LEDGER WHERE STATUS = 'OPEN'",
            context, "ledger.sql");

        // Same alias regardless of entry path
        context.Mappings.Forward["ACCOUNT_LEDGER"].Should().Be(aliasViaCobol);
    }

    [Fact]
    public void SharedContext_CSharpAndVbNet_SameIdentifierConsistent()
    {
        var context = CreateContextWithRegistry();
        var csharpProcessor = new CSharpLanguageProcessor();
        var vbProcessor = new VisualBasicLanguageProcessor();

        // C# class
        csharpProcessor.Obfuscate(
            "public class PayrollEngine { }", context, "PayrollEngine.cs");
        var csharpAlias = context.Mappings.Forward["PayrollEngine"];

        // VB.NET class with the same name
        vbProcessor.Obfuscate(
            "Public Class PayrollEngine\r\nEnd Class", context, "PayrollEngine.vb");

        // Same alias across C# and VB.NET
        context.Mappings.Forward["PayrollEngine"].Should().Be(csharpAlias);
        csharpAlias.Should().StartWith("CLS_");
    }

    [Fact]
    public void SharedContext_FourFiles_MappingTableNoDuplicates()
    {
        var context = CreateContextWithRegistry();
        var cobolProcessor = new CobolLanguageProcessor();
        var csharpProcessor = new CSharpLanguageProcessor();
        var jsProcessor = new JavaScriptLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        // COBOL file
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. BATCHPROC.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-AMOUNT              PIC 9(7)V99.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           DISPLAY WS-AMOUNT",
            "           STOP RUN.");
        cobolProcessor.Obfuscate(cobol, context, "batch.cbl");

        // C# file
        csharpProcessor.Obfuscate(
            "public class OrderService { }", context, "OrderService.cs");

        // JavaScript file
        jsProcessor.Obfuscate(
            "const apiEndpoint = 'SELECT OrderId FROM PendingOrders';",
            context, "orders.js");

        // SQL file
        sqlProcessor.Obfuscate(
            "CREATE TABLE ProductCatalog (ProductId INT, ProductName VARCHAR(100))",
            context, "schema.sql");

        // Forward count should equal unique identifiers (no duplicates)
        var forwardCount = context.Mappings.Forward.Count;
        var reverseCount = context.Mappings.Reverse.Count;
        forwardCount.Should().Be(reverseCount, because: "every forward mapping should have a reverse");

        // All values in Forward should be unique
        context.Mappings.Forward.Values.Distinct().Count()
            .Should().Be(forwardCount, because: "no two originals should share an alias");
    }

    // ═══════════════════════════════════════════════════════════════════
    // SourceMap / FileMap Tracking
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void SharedContext_SourceMapFileMap_AllFilesRecorded()
    {
        var context = CreateContextWithRegistry();

        // Use processors that call RecordFileProcessing
        var csharpProcessor = new CSharpLanguageProcessor();
        var vbProcessor = new VisualBasicLanguageProcessor();
        var cobolProcessor = new CobolLanguageProcessor();
        var jclProcessor = new JclLanguageProcessor();
        ILanguageProcessor fsharpProcessor = new FSharpLanguageProcessor();

        csharpProcessor.Obfuscate(
            "public class OrderService { }", context, "OrderService.cs");

        vbProcessor.Obfuscate(
            "Public Class InvoiceModule\r\nEnd Class", context, "InvoiceModule.vb");

        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. MAINPGM.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           STOP RUN.");
        cobolProcessor.Obfuscate(cobol, context, "mainpgm.cbl");

        var jcl = "//RUNJOB   JOB (ACCT),'RUN',CLASS=A\n//STEP01   EXEC PGM=MYPROG\n//SYSPRINT DD SYSOUT=*\n//";
        jclProcessor.Obfuscate(jcl, context, "runjob.jcl");

        fsharpProcessor.Obfuscate(
            "module Calculator\n\nlet add x y = x + y", context, "Calculator.fs");

        // FileMap should have entries for all 5 files
        context.SourceMap.FileMap.Should().HaveCount(5);
        context.SourceMap.FileMap.Should().ContainKey("OrderService.cs");
        context.SourceMap.FileMap.Should().ContainKey("InvoiceModule.vb");
        context.SourceMap.FileMap.Should().ContainKey("mainpgm.cbl");
        context.SourceMap.FileMap.Should().ContainKey("runjob.jcl");
        context.SourceMap.FileMap.Should().ContainKey("Calculator.fs");

        // Verify correct ProcessorIds
        context.SourceMap.FileMap["OrderService.cs"].ProcessorId.Should().Be("csharp");
        context.SourceMap.FileMap["mainpgm.cbl"].ProcessorId.Should().Be("cobol");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Cross-Language Reference Tracking (COBOL host vars)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void SharedContext_CrossRef_CobolHostVarsRecorded()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. CROSSREF.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-EMP-ID              PIC 9(5).",
            "       PROCEDURE DIVISION.",
            "       LOOKUP-EMP.",
            "           EXEC SQL",
            "             SELECT EMP_NAME FROM EMPLOYEES",
            "             WHERE EMP_ID = :WS-EMP-ID",
            "           END-EXEC",
            "           STOP RUN.");
        processor.Obfuscate(cobol, context, "crossref.cbl");

        // Cross-references should be recorded for host variables
        context.SourceMap.CrossReferences.Should().NotBeEmpty();
        context.SourceMap.CrossReferences.Should().Contain(cr =>
            cr.SourceLanguage == "COBOL" && cr.TargetLanguage == "SQL");
    }

    [Fact]
    public void SharedContext_CrossRef_MultipleBlocks_AllTracked()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. MULTIPGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-ACCT-ID             PIC 9(8).",
            "       01  WS-ACCT-NAME           PIC X(30).",
            "       01  WS-ACCT-BAL            PIC 9(9)V99.",
            "       01  WS-TXN-AMT             PIC 9(9)V99.",
            "       01  WS-TXN-DATE            PIC X(10).",
            "       PROCEDURE DIVISION.",
            "       FETCH-ACCT.",
            "           EXEC SQL",
            "             SELECT ACCT_NAME, ACCT_BALANCE",
            "             INTO :WS-ACCT-NAME, :WS-ACCT-BAL",
            "             FROM ACCOUNT_MASTER",
            "             WHERE ACCT_ID = :WS-ACCT-ID",
            "           END-EXEC.",
            "       INSERT-TXN.",
            "           EXEC SQL",
            "             INSERT INTO TXN_HISTORY",
            "             (ACCT_ID, TXN_AMOUNT, TXN_DATE)",
            "             VALUES (:WS-ACCT-ID, :WS-TXN-AMT,",
            "              :WS-TXN-DATE)",
            "           END-EXEC.",
            "       UPDATE-BAL.",
            "           EXEC SQL",
            "             UPDATE ACCOUNT_MASTER",
            "             SET ACCT_BALANCE = :WS-ACCT-BAL",
            "             WHERE ACCT_ID = :WS-ACCT-ID",
            "           END-EXEC",
            "           STOP RUN.");
        processor.Obfuscate(cobol, context, "multipgm.cbl");

        // 3 EXEC SQL blocks with 5+ unique host variable references
        // (WS-ACCT-ID appears 3 times, WS-ACCT-NAME 1, WS-ACCT-BAL 2, WS-TXN-AMT 1, WS-TXN-DATE 1 = 8 refs)
        context.SourceMap.CrossReferences.Should().HaveCountGreaterThanOrEqualTo(5);
    }

    [Fact]
    public void SharedContext_CrossRef_DescriptionAndFilePath()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. DETAILPG.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-EMP-ID              PIC 9(5).",
            "       PROCEDURE DIVISION.",
            "       GET-DATA.",
            "           EXEC SQL",
            "             SELECT NAME FROM STAFF",
            "             WHERE ID = :WS-EMP-ID",
            "           END-EXEC",
            "           STOP RUN.");
        processor.Obfuscate(cobol, context, "payroll.cbl");

        // Verify cross-reference has description and source file
        var crossRef = context.SourceMap.CrossReferences
            .FirstOrDefault(cr => cr.Description.Contains("WS-EMP-ID"));
        crossRef.Should().NotBeNull();
        crossRef!.SourceFile.Should().Be("payroll.cbl");
        crossRef.SourceLanguage.Should().Be("COBOL");
        crossRef.TargetLanguage.Should().Be("SQL");
    }
}
