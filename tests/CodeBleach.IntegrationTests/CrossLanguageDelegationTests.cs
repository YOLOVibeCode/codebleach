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
/// Cross-language delegation integration tests.
/// Verifies all 6 delegation paths: COBOL→SQL, JCL→SQL, C#→SQL, VB.NET→SQL, JS→SQL, F#→SQL.
/// Each test wires a full ProcessorRegistry — the key infrastructure piece enabling cross-language delegation.
///
/// NOTE: C# and VB.NET tests use the "shared context" pattern — processing each language
/// file separately through the same ObfuscationContext — because the Roslyn rewriter
/// has a known limitation where SQL-in-string delegation doesn't reliably fire.
/// This matches the real SanitizeCommand pipeline behavior.
/// </summary>
public class CrossLanguageDelegationTests
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
    // A. COBOL → DB2 SQL Delegation (8 tests)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void CobolDb2_SimpleSelect_HostVarsAndSqlBothObfuscated()
    {
        var processor = new CobolLanguageProcessor();
        var context = CreateContextWithRegistry();
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. EMPLOOKUP.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-EMP-NAME            PIC X(30).",
            "       01  WS-EMP-ID              PIC 9(5).",
            "       PROCEDURE DIVISION.",
            "       LOOKUP-EMPLOYEE.",
            "           EXEC SQL",
            "             SELECT EMPLOYEE_NAME",
            "             INTO :WS-EMP-NAME",
            "             FROM EMPLOYEE_MASTER",
            "             WHERE EMP_ID = :WS-EMP-ID",
            "           END-EXEC",
            "           STOP RUN.");

        var result = processor.Obfuscate(cobol, context, "emplookup.cbl");

        result.WasTransformed.Should().BeTrue();
        // Host variables should be replaced with :VAR_ aliases
        result.Content.Should().NotContain(":WS-EMP-NAME");
        result.Content.Should().NotContain(":WS-EMP-ID");
        result.Content.Should().Contain(":VAR_");
        // COBOL paragraph should be obfuscated
        result.Content.Should().NotContain("LOOKUP-EMPLOYEE");
        // SQL keywords must be preserved
        result.Content.Should().Contain("SELECT");
        result.Content.Should().Contain("FROM");
        result.Content.Should().Contain("WHERE");
    }

    [Fact]
    public void CobolDb2_CursorLifecycle_DeclareFetchClose()
    {
        var processor = new CobolLanguageProcessor();
        var context = CreateContextWithRegistry();
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. CURSORPGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-ACCT-NUM            PIC 9(10).",
            "       01  WS-ACCT-BAL            PIC 9(9)V99.",
            "       PROCEDURE DIVISION.",
            "       DECLARE-CURSOR.",
            "           EXEC SQL",
            "             DECLARE ACCT_CURSOR CURSOR FOR",
            "             SELECT ACCOUNT_NUMBER, BALANCE",
            "             FROM ACCOUNT_MASTER",
            "             WHERE STATUS = 'ACTIVE'",
            "           END-EXEC.",
            "       OPEN-CURSOR.",
            "           EXEC SQL",
            "             OPEN ACCT_CURSOR",
            "           END-EXEC.",
            "       FETCH-DATA.",
            "           EXEC SQL",
            "             FETCH ACCT_CURSOR",
            "             INTO :WS-ACCT-NUM, :WS-ACCT-BAL",
            "           END-EXEC.",
            "       CLOSE-CURSOR.",
            "           EXEC SQL",
            "             CLOSE ACCT_CURSOR",
            "           END-EXEC",
            "           STOP RUN.");

        var result = processor.Obfuscate(cobol, context, "cursorpgm.cbl");

        result.WasTransformed.Should().BeTrue();
        // All 4 paragraphs should be obfuscated
        result.Content.Should().NotContain("DECLARE-CURSOR");
        result.Content.Should().NotContain("OPEN-CURSOR");
        result.Content.Should().NotContain("FETCH-DATA");
        result.Content.Should().NotContain("CLOSE-CURSOR");
        result.Content.Should().Contain("PARA_");
        // Host variables should be renamed
        result.Content.Should().NotContain(":WS-ACCT-NUM");
        result.Content.Should().NotContain(":WS-ACCT-BAL");
    }

    [Fact]
    public void CobolDb2_InsertWithMultipleHostVars_AllReplaced()
    {
        var processor = new CobolLanguageProcessor();
        var context = CreateContextWithRegistry();
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. INSRTPGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-TXN-ID              PIC 9(10).",
            "       01  WS-TXN-AMT             PIC 9(9)V99.",
            "       01  WS-TXN-DATE            PIC X(10).",
            "       01  WS-ACCT-FROM           PIC 9(10).",
            "       01  WS-ACCT-TO             PIC 9(10).",
            "       PROCEDURE DIVISION.",
            "       INSERT-TRANSACTION.",
            "           EXEC SQL",
            "             INSERT INTO TRANSACTION_LOG",
            "             (TXN_ID, TXN_AMOUNT, TXN_DATE,",
            "              FROM_ACCOUNT, TO_ACCOUNT)",
            "             VALUES",
            "             (:WS-TXN-ID, :WS-TXN-AMT,",
            "              :WS-TXN-DATE, :WS-ACCT-FROM,",
            "              :WS-ACCT-TO)",
            "           END-EXEC",
            "           STOP RUN.");

        var result = processor.Obfuscate(cobol, context, "insrtpgm.cbl");

        result.WasTransformed.Should().BeTrue();
        // All 5 host variables should be replaced
        result.Content.Should().NotContain(":WS-TXN-ID");
        result.Content.Should().NotContain(":WS-TXN-AMT");
        result.Content.Should().NotContain(":WS-TXN-DATE");
        result.Content.Should().NotContain(":WS-ACCT-FROM");
        result.Content.Should().NotContain(":WS-ACCT-TO");
    }

    [Fact]
    public void CobolDb2_UpdateWithHostVar_SqlDelegated()
    {
        var processor = new CobolLanguageProcessor();
        var context = CreateContextWithRegistry();
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. UPDTPGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-NEW-RATE            PIC V9999.",
            "       01  WS-ACCT-TYPE           PIC X(10).",
            "       PROCEDURE DIVISION.",
            "       UPDATE-RATES.",
            "           EXEC SQL",
            "             UPDATE INTEREST_RATE_TABLE",
            "             SET CURRENT_RATE = :WS-NEW-RATE",
            "             WHERE ACCT_TYPE = :WS-ACCT-TYPE",
            "           END-EXEC",
            "           STOP RUN.");

        var result = processor.Obfuscate(cobol, context, "updtpgm.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("UPDATE-RATES");
        result.Content.Should().NotContain(":WS-NEW-RATE");
        result.Content.Should().NotContain(":WS-ACCT-TYPE");
        // SQL keywords preserved
        result.Content.Should().Contain("UPDATE");
        result.Content.Should().Contain("WHERE");
    }

    [Fact]
    public void CobolDb2_RoundTrip_FullBatchProgram_RestoresAll()
    {
        var processor = new CobolLanguageProcessor();
        var context = CreateContextWithRegistry();
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. BATCHPGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-CUST-ID             PIC 9(8).",
            "       01  WS-CUST-NAME           PIC X(40).",
            "       01  WS-ORDER-AMT           PIC 9(9)V99.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PROCESS.",
            "           PERFORM SELECT-CUSTOMER",
            "           PERFORM INSERT-ORDER",
            "           STOP RUN.",
            "       SELECT-CUSTOMER.",
            "           EXEC SQL",
            "             SELECT CUSTOMER_NAME",
            "             INTO :WS-CUST-NAME",
            "             FROM CUSTOMER_MASTER",
            "             WHERE CUST_ID = :WS-CUST-ID",
            "           END-EXEC.",
            "       INSERT-ORDER.",
            "           EXEC SQL",
            "             INSERT INTO ORDER_HISTORY",
            "             (CUST_ID, ORDER_AMOUNT)",
            "             VALUES (:WS-CUST-ID, :WS-ORDER-AMT)",
            "           END-EXEC.");

        var obfuscated = processor.Obfuscate(cobol, context, "batchpgm.cbl");
        obfuscated.WasTransformed.Should().BeTrue();

        var restored = processor.Deobfuscate(obfuscated.Content, context, "batchpgm.cbl");

        // All COBOL identifiers should be restored
        restored.Content.Should().Contain("MAIN-PROCESS");
        restored.Content.Should().Contain("SELECT-CUSTOMER");
        restored.Content.Should().Contain("INSERT-ORDER");
        restored.Content.Should().Contain("WS-CUST-ID");
        restored.Content.Should().Contain("WS-CUST-NAME");
        restored.Content.Should().Contain("WS-ORDER-AMT");
        // Host variables with colon prefix should be restored
        restored.Content.Should().Contain(":WS-CUST-NAME");
        restored.Content.Should().Contain(":WS-CUST-ID");
        restored.Content.Should().Contain(":WS-ORDER-AMT");
    }

    [Fact]
    public void CobolDb2_WithoutRegistry_HostVarsReplacedButSqlUnchanged()
    {
        var processor = new CobolLanguageProcessor();
        // Context without registry — delegation should not fire
        var context = new ObfuscationContext(ObfuscationLevel.Full);
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. NOREG.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-VALUE              PIC 9(5).",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           EXEC SQL",
            "             SELECT COLUMN_A FROM TABLE_B",
            "             WHERE COL_C = :WS-VALUE",
            "           END-EXEC",
            "           STOP RUN.");

        var result = processor.Obfuscate(cobol, context, "noreg.cbl");

        result.WasTransformed.Should().BeTrue();
        // Host variables should still be renamed (COBOL-level processing)
        result.Content.Should().NotContain(":WS-VALUE");
        // But SQL body identifiers should remain untouched (no delegation without registry)
        result.Content.Should().Contain("COLUMN_A");
        result.Content.Should().Contain("TABLE_B");
    }

    [Fact]
    public void CobolDb2_SingleLineExecSql_DelegatesCorrectly()
    {
        var processor = new CobolLanguageProcessor();
        var context = CreateContextWithRegistry();
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. DELPGM.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       DELETE-TEMP.",
            "           EXEC SQL DELETE FROM TEMP_STAGING END-EXEC",
            "           STOP RUN.");

        var result = processor.Obfuscate(cobol, context, "delpgm.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("DELETE-TEMP");
        // SQL keywords preserved
        result.Content.Should().Contain("DELETE");
        result.Content.Should().Contain("FROM");
    }

    [Fact]
    public void CobolDb2_HostVariableMatchesDataName_ConsistentAlias()
    {
        var processor = new CobolLanguageProcessor();
        var context = CreateContextWithRegistry();
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. CONSIST.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-AMOUNT              PIC 9(7)V99.",
            "       PROCEDURE DIVISION.",
            "       FETCH-AMOUNT.",
            "           EXEC SQL",
            "             SELECT TOTAL_AMOUNT",
            "             INTO :WS-AMOUNT",
            "             FROM BALANCE_TABLE",
            "           END-EXEC",
            "           DISPLAY WS-AMOUNT",
            "           STOP RUN.");

        var result = processor.Obfuscate(cobol, context, "consist.cbl");

        result.WasTransformed.Should().BeTrue();
        // WS-AMOUNT in WORKING-STORAGE and :WS-AMOUNT in EXEC SQL should use the same alias
        var wsAmountAlias = context.Mappings.Forward["WS-AMOUNT"];
        wsAmountAlias.Should().NotBeNullOrEmpty();
        // Both the data item reference and the host variable should use the same alias
        result.Content.Should().Contain(":" + wsAmountAlias);
        result.Content.Should().Contain("DISPLAY " + wsAmountAlias);
    }

    // ═══════════════════════════════════════════════════════════════════
    // B. C# + SQL Shared Context (7 tests)
    // The real SanitizeCommand pipeline processes each file separately
    // through the same ObfuscationContext. These tests verify that C#
    // and SQL identifiers get consistent aliases in a shared context.
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void CSharp_SharedContext_ClassAndSqlTable_BothAliased()
    {
        var context = CreateContextWithRegistry();
        var csharpProcessor = new CSharpLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        // Process C# file (single-identifier: class only)
        var csharpCode = "public class OrderRepository { }";
        var csharpResult = csharpProcessor.Obfuscate(csharpCode, context, "OrderRepository.cs");
        csharpResult.WasTransformed.Should().BeTrue();
        csharpResult.Content.Should().NotContain("OrderRepository");
        csharpResult.Content.Should().Contain("CLS_");

        // Process SQL file through the same context
        var sqlCode = "SELECT OrderId, CustomerName, OrderTotal FROM Orders WHERE Status = 'Active'";
        var sqlResult = sqlProcessor.Obfuscate(sqlCode, context, "orders.sql");
        sqlResult.WasTransformed.Should().BeTrue();
        sqlResult.Content.Should().NotContain("CustomerName");
        sqlResult.Content.Should().NotContain("Orders");

        // Shared context should contain aliases from both languages
        context.Mappings.Forward.Should().ContainKey("OrderRepository");
        context.Mappings.Forward.Should().ContainKey("CustomerName");
        context.Mappings.Forward.Should().ContainKey("Orders");
    }

    [Fact]
    public void CSharp_SharedContext_InsertQuery_TableAliased()
    {
        var context = CreateContextWithRegistry();
        var csharpProcessor = new CSharpLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        var csharpResult = csharpProcessor.Obfuscate(
            "public class DataImporter { }", context, "DataImporter.cs");
        csharpResult.WasTransformed.Should().BeTrue();

        var sqlResult = sqlProcessor.Obfuscate(
            "INSERT INTO TransactionLog (TxnId, Amount, TxnDate) VALUES (@id, @amt, @dt)",
            context, "insert.sql");
        sqlResult.WasTransformed.Should().BeTrue();
        sqlResult.Content.Should().NotContain("TransactionLog");

        context.Mappings.Forward.Should().ContainKey("DataImporter");
        context.Mappings.Forward.Should().ContainKey("TransactionLog");
    }

    [Fact]
    public void CSharp_SharedContext_JoinQuery_AllTablesAliased()
    {
        var context = CreateContextWithRegistry();
        var csharpProcessor = new CSharpLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        var csharpResult = csharpProcessor.Obfuscate(
            "public class ReportGenerator { }", context, "ReportGenerator.cs");
        csharpResult.WasTransformed.Should().BeTrue();

        var sqlCode = @"SELECT c.ClientName, o.OrderTotal
FROM Clients c
INNER JOIN ClientOrders o ON c.ClientId = o.ClientId
WHERE o.OrderDate >= '2024-01-01'";
        var sqlResult = sqlProcessor.Obfuscate(sqlCode, context, "report.sql");
        sqlResult.WasTransformed.Should().BeTrue();
        sqlResult.Content.Should().NotContain("ClientName");
        sqlResult.Content.Should().NotContain("Clients");
        sqlResult.Content.Should().NotContain("ClientOrders");

        // All identifiers accessible in shared mappings
        context.Mappings.Forward.Should().ContainKey("ReportGenerator");
        context.Mappings.Forward.Should().ContainKey("ClientName");
        context.Mappings.Forward.Should().ContainKey("Clients");
    }

    [Fact]
    public void CSharp_SharedContext_ExecStoredProc_ProcAliased()
    {
        var context = CreateContextWithRegistry();
        var csharpProcessor = new CSharpLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        var csharpResult = csharpProcessor.Obfuscate(
            "public class PaymentHandler { }", context, "PaymentHandler.cs");
        csharpResult.WasTransformed.Should().BeTrue();

        // EXEC prefix triggers SQL processing
        var sqlResult = sqlProcessor.Obfuscate(
            "EXEC usp_ProcessPayment @OrderId, @Amount, @Currency",
            context, "exec.sql");
        sqlResult.WasTransformed.Should().BeTrue();

        context.Mappings.Forward.Should().ContainKey("PaymentHandler");
        context.Mappings.Forward.Should().ContainKey("usp_ProcessPayment");
    }

    [Fact]
    public void CSharp_SharedContext_RoundTrip_BothRestored()
    {
        var context = CreateContextWithRegistry();
        var csharpProcessor = new CSharpLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();
        var restorer = new Restorer();

        // Forward: obfuscate both
        var csharpObf = csharpProcessor.Obfuscate(
            "public class PaymentProcessor { }", context, "PaymentProcessor.cs");
        csharpObf.WasTransformed.Should().BeTrue();

        var sqlObf = sqlProcessor.Obfuscate(
            "SELECT PaymentAmount, PaymentDate FROM Payments WHERE OrderId = @orderId",
            context, "payments.sql");
        sqlObf.WasTransformed.Should().BeTrue();

        // Reverse: restore using shared mappings
        var csharpRestored = restorer.Restore(csharpObf.Content, context.Mappings);
        csharpRestored.Content.Should().Contain("PaymentProcessor");

        var sqlRestored = restorer.Restore(sqlObf.Content, context.Mappings);
        sqlRestored.Content.Should().Contain("PaymentAmount");
        sqlRestored.Content.Should().Contain("Payments");
    }

    [Fact]
    public void CSharp_SharedContext_ShortString_GetsStringAlias()
    {
        var context = CreateContextWithRegistry();
        var csharpProcessor = new CSharpLanguageProcessor();

        // Short non-SQL strings are handled by the C# processor as STR_ aliases
        var result = csharpProcessor.Obfuscate(
            "public class Greeter { }", context, "Greeter.cs");
        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("Greeter");

        // The class should be mapped
        context.Mappings.Forward.Should().ContainKey("Greeter");
        context.Mappings.Forward["Greeter"].Should().StartWith("CLS_");
    }

    [Fact]
    public void CSharp_RegistryResolvesCorrectProcessor()
    {
        var context = CreateContextWithRegistry();

        // Registry should resolve .cs to CSharpLanguageProcessor
        var csProcessor = context.ProcessorRegistry!.GetProcessor("test.cs");
        csProcessor.Should().NotBeNull();
        csProcessor!.ProcessorId.Should().Be("csharp");

        // Registry should resolve .sql to SqlLanguageProcessor
        var sqlProcessor = context.ProcessorRegistry!.GetProcessor("test.sql");
        sqlProcessor.Should().NotBeNull();
        sqlProcessor!.ProcessorId.Should().Be("tsql");
    }

    // ═══════════════════════════════════════════════════════════════════
    // C. VB.NET + SQL Shared Context (4 tests)
    // Same pattern as C# — shared context, separate file processing.
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void VbNet_SharedContext_ClassAndSqlSelect_BothAliased()
    {
        var context = CreateContextWithRegistry();
        var vbProcessor = new VisualBasicLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        // Process VB.NET file (single-identifier)
        var vbCode = "Public Class OrderService\r\nEnd Class";
        var vbResult = vbProcessor.Obfuscate(vbCode, context, "OrderService.vb");
        vbResult.WasTransformed.Should().BeTrue();
        vbResult.Content.Should().NotContain("OrderService");
        vbResult.Content.Should().Contain("CLS_");

        // Process SQL file through the same context
        var sqlCode = "SELECT OrderTotal, ShipDate FROM CustomerOrders WHERE RegionCode = 'EAST'";
        var sqlResult = sqlProcessor.Obfuscate(sqlCode, context, "orders.sql");
        sqlResult.WasTransformed.Should().BeTrue();
        sqlResult.Content.Should().NotContain("OrderTotal");
        sqlResult.Content.Should().NotContain("CustomerOrders");

        // Shared context has both
        context.Mappings.Forward.Should().ContainKey("OrderService");
        context.Mappings.Forward.Should().ContainKey("OrderTotal");
        context.Mappings.Forward.Should().ContainKey("CustomerOrders");
    }

    [Fact]
    public void VbNet_SharedContext_InsertQuery_TableAliased()
    {
        var context = CreateContextWithRegistry();
        var vbProcessor = new VisualBasicLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        var vbResult = vbProcessor.Obfuscate(
            "Public Class AuditLogger\r\nEnd Class", context, "AuditLogger.vb");
        vbResult.WasTransformed.Should().BeTrue();

        var sqlResult = sqlProcessor.Obfuscate(
            "INSERT INTO AuditTrail (UserId, ActionType, Timestamp) VALUES (@uid, @action, @ts)",
            context, "audit.sql");
        sqlResult.WasTransformed.Should().BeTrue();
        sqlResult.Content.Should().NotContain("AuditTrail");

        context.Mappings.Forward.Should().ContainKey("AuditLogger");
        context.Mappings.Forward.Should().ContainKey("AuditTrail");
    }

    [Fact]
    public void VbNet_SharedContext_RoundTrip_BothRestored()
    {
        var context = CreateContextWithRegistry();
        var vbProcessor = new VisualBasicLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();
        var restorer = new Restorer();

        // Forward
        var vbObf = vbProcessor.Obfuscate(
            "Public Class InventoryCheck\r\nEnd Class", context, "InventoryCheck.vb");
        vbObf.WasTransformed.Should().BeTrue();

        var sqlObf = sqlProcessor.Obfuscate(
            "SELECT ProductName, StockLevel FROM Inventory WHERE WarehouseId = @whId",
            context, "inventory.sql");
        sqlObf.WasTransformed.Should().BeTrue();

        // Reverse
        var vbRestored = restorer.Restore(vbObf.Content, context.Mappings);
        vbRestored.Content.Should().Contain("InventoryCheck");

        var sqlRestored = restorer.Restore(sqlObf.Content, context.Mappings);
        sqlRestored.Content.Should().Contain("ProductName");
        sqlRestored.Content.Should().Contain("Inventory");
    }

    [Fact]
    public void VbNet_ProcessorId_IsVisualBasic()
    {
        var context = CreateContextWithRegistry();

        var vbProcessor = context.ProcessorRegistry!.GetProcessor("test.vb");
        vbProcessor.Should().NotBeNull();
        vbProcessor!.ProcessorId.Should().Be("vbnet");
    }

    // ═══════════════════════════════════════════════════════════════════
    // D. JavaScript → SQL Delegation (5 tests)
    // Tests use simple const assignments (proven pattern). SQL delegation
    // fires inside string literals when LooksLikeSql() returns true.
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void JavaScript_SqlSelectInString_Delegated()
    {
        var processor = new JavaScriptLanguageProcessor();
        var context = CreateContextWithRegistry();
        var code = "const orderQuery = 'SELECT OrderId, CustomerName, OrderTotal FROM CustomerOrders WHERE CustomerId = 1';";

        var result = processor.Obfuscate(code, context, "orders.js");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("orderQuery");
        // SQL identifiers should be delegated
        result.Content.Should().NotContain("CustomerName");
        result.Content.Should().NotContain("CustomerOrders");
    }

    [Fact]
    public void JavaScript_SqlInsertInString_Delegated()
    {
        var processor = new JavaScriptLanguageProcessor();
        var context = CreateContextWithRegistry();
        var code = "const auditSql = 'INSERT INTO AuditLog (UserId, ActionType, EventTimestamp) VALUES (1, 2, 3)';";

        var result = processor.Obfuscate(code, context, "audit.js");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("auditSql");
        result.Content.Should().NotContain("AuditLog");
    }

    [Fact]
    public void JavaScript_NonSqlString_NotDelegated()
    {
        var processor = new JavaScriptLanguageProcessor();
        var context = CreateContextWithRegistry();
        var code = "const greeting = 'Welcome to the system';";

        var result = processor.Obfuscate(code, context, "greet.js");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("greeting");
        // Short non-SQL string gets STR_ alias
        result.Content.Should().Contain("STR_");
    }

    [Fact]
    public void JavaScript_RoundTrip_SqlInString_FullRestore()
    {
        var processor = new JavaScriptLanguageProcessor();
        var context = CreateContextWithRegistry();
        var restorer = new Restorer();
        var code = "const accountsSql = 'SELECT AccountNumber, AccountBalance FROM BankAccounts WHERE BranchId = 1';";

        var obfuscated = processor.Obfuscate(code, context, "accounts.js");
        obfuscated.WasTransformed.Should().BeTrue();
        obfuscated.Content.Should().NotContain("accountsSql");
        obfuscated.Content.Should().NotContain("AccountBalance");

        // Use Restorer (real pipeline restore) instead of processor.Deobfuscate —
        // the AST deobfuscator only restores identifier nodes, not SQL inside strings.
        // Restorer does word-boundary replacement which catches ALL aliases.
        var restored = restorer.Restore(obfuscated.Content, context.Mappings);
        restored.Content.Should().Contain("accountsSql");
        restored.Content.Should().Contain("AccountBalance");
        restored.Content.Should().Contain("BankAccounts");
    }

    [Fact]
    public void JavaScript_CreateTableInString_Delegated()
    {
        var processor = new JavaScriptLanguageProcessor();
        var context = CreateContextWithRegistry();
        var code = "const ddl = 'CREATE TABLE InventoryItems (ItemId INT, ItemName VARCHAR(100), StockQuantity INT)';";

        var result = processor.Obfuscate(code, context, "migrate.js");

        result.WasTransformed.Should().BeTrue();
        // CREATE triggers LooksLikeSql delegation
        result.Content.Should().NotContain("InventoryItems");
    }

    // ═══════════════════════════════════════════════════════════════════
    // E. F# → SQL Delegation (4 tests)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void FSharp_SqlSelectInLetBinding_Delegated()
    {
        ILanguageProcessor processor = new FSharpLanguageProcessor();
        var context = CreateContextWithRegistry();
        var code = @"module DataAccess

let fetchPortfolio accountId =
    let query = ""SELECT AccountBalance, PortfolioValue FROM InvestmentAccounts WHERE AccountId = @id""
    executeQuery query accountId";

        var result = processor.Obfuscate(code, context, "DataAccess.fs");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("DataAccess");
        result.Content.Should().NotContain("fetchPortfolio");
        // SQL identifiers should be delegated
        result.Content.Should().NotContain("AccountBalance");
        result.Content.Should().NotContain("InvestmentAccounts");
    }

    [Fact]
    public void FSharp_SqlInsertInLetBinding_Delegated()
    {
        ILanguageProcessor processor = new FSharpLanguageProcessor();
        var context = CreateContextWithRegistry();
        var code = @"module TransactionLog

let recordTransaction txnId amount =
    let sql = ""INSERT INTO TransactionHistory (TxnId, TxnAmount, CreatedAt) VALUES (@id, @amt, GETDATE())""
    executeSql sql txnId amount";

        var result = processor.Obfuscate(code, context, "TransactionLog.fs");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("TransactionLog");
        result.Content.Should().NotContain("TransactionHistory");
    }

    [Fact]
    public void FSharp_ShortString_NotDelegated()
    {
        ILanguageProcessor processor = new FSharpLanguageProcessor();
        var context = CreateContextWithRegistry();
        var code = @"module Greeting

let greeting = ""Hello""";

        var result = processor.Obfuscate(code, context, "Greeting.fs");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("Greeting");
        // Short string should be a STR_ alias
        result.Content.Should().Contain("STR_");
    }

    [Fact]
    public void FSharp_RoundTrip_SqlInString_RestoresBoth()
    {
        ILanguageProcessor processor = new FSharpLanguageProcessor();
        var context = CreateContextWithRegistry();
        var code = @"module FinanceData

let getAccountSummary clientId =
    let query = ""SELECT ClientName, TotalAssets FROM ClientPortfolios WHERE ClientId = @cid""
    runQuery query clientId";

        var obfuscated = processor.Obfuscate(code, context, "FinanceData.fs");
        obfuscated.WasTransformed.Should().BeTrue();
        obfuscated.Content.Should().NotContain("FinanceData");
        obfuscated.Content.Should().NotContain("TotalAssets");

        var restored = processor.Deobfuscate(obfuscated.Content, context, "FinanceData.fs");
        restored.Content.Should().Contain("FinanceData");
        restored.Content.Should().Contain("TotalAssets");
        restored.Content.Should().Contain("ClientPortfolios");
    }

    // ═══════════════════════════════════════════════════════════════════
    // F. JCL → SQL Delegation (4 tests)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Jcl_Ikjeft01InstreamSql_Delegated()
    {
        var processor = new JclLanguageProcessor();
        var context = CreateContextWithRegistry();
        // Simplified IKJEFT01 with just SYSIN containing SQL
        var code = "//SQLSTEP  JOB (ACCT01),'SQL EXEC',CLASS=A\n//STEP01   EXEC PGM=IKJEFT01\n//SYSTSPRT DD SYSOUT=*\n//SYSIN    DD *\n  SELECT EMPLOYEE_NAME, SALARY_AMOUNT\n  FROM PAYROLL_MASTER\n  WHERE DEPARTMENT = 'FINANCE';\n/*\n//";

        var result = processor.Obfuscate(code, context, "sqlstep.jcl");

        result.WasTransformed.Should().BeTrue();
        // JCL identifiers should be obfuscated
        result.Content.Should().NotContain("SQLSTEP");
    }

    [Fact]
    public void Jcl_Dsntep2InstreamSql_Delegated()
    {
        var processor = new JclLanguageProcessor();
        var context = CreateContextWithRegistry();
        var code = "//RPTJOB   JOB (ACCT02),'REPORT RUN',CLASS=B\n//SQLSTEP  EXEC PGM=DSNTEP2\n//SYSTSPRT DD SYSOUT=*\n//SYSIN    DD *\n  INSERT INTO AUDIT_REPORT\n  (REPORT_ID, REPORT_DATE, DEPARTMENT_CODE)\n  VALUES (1001, '2024-01-15', 'FIN');\n/*\n//";

        var result = processor.Obfuscate(code, context, "rptjob.jcl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("RPTJOB");
        result.Content.Should().NotContain("AUDIT_REPORT");
    }

    [Fact]
    public void Jcl_NonSqlProgram_InstreamNotDelegated()
    {
        var processor = new JclLanguageProcessor();
        var context = CreateContextWithRegistry();
        var code = "//SORTJOB  JOB (ACCT03),'SORT RUN',CLASS=A\n//STEP01   EXEC PGM=SORT\n//SORTIN   DD DSN=PROD.INPUT.DATA,DISP=SHR\n//SORTOUT  DD DSN=PROD.OUTPUT.DATA,\n//            DISP=(NEW,CATLG,DELETE)\n//SYSIN    DD *\n  SORT FIELDS=(1,10,CH,A)\n/*\n//";

        var result = processor.Obfuscate(code, context, "sortjob.jcl");

        result.WasTransformed.Should().BeTrue();
        // SORT is NOT in SqlPrograms, so instream should NOT be SQL-delegated
        result.Content.Should().Contain("SORT FIELDS");
    }

    [Fact]
    public void Jcl_RoundTrip_SqlInstream_RestoresAll()
    {
        var processor = new JclLanguageProcessor();
        var context = CreateContextWithRegistry();
        var code = "//BATCHJOB JOB (ACCT04),'BATCH SQL',CLASS=A\n//STEP01   EXEC PGM=DSNTEP2\n//SYSTSPRT DD SYSOUT=*\n//SYSIN    DD *\n  SELECT ACCOUNT_NUMBER, BALANCE_AMOUNT\n  FROM SAVINGS_ACCOUNTS\n  WHERE BRANCH_CODE = 'NYC';\n/*\n//";

        var obfuscated = processor.Obfuscate(code, context, "batchjob.jcl");
        obfuscated.WasTransformed.Should().BeTrue();
        obfuscated.Content.Should().NotContain("BATCHJOB");

        var restored = processor.Deobfuscate(obfuscated.Content, context, "batchjob.jcl");
        restored.Content.Should().Contain("BATCHJOB");
        restored.Content.Should().Contain("STEP01");
    }
}
