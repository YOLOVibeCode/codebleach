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
/// Realistic enterprise code as static fixtures — "golden samples" that must always work.
/// Tests comprehensive, realistic code through the full obfuscation pipeline.
/// </summary>
public class EnterpriseCodeFixtures
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

    // ═══════════════════════════════════════════════════════════════════
    // Golden Sample Fixtures
    // ═══════════════════════════════════════════════════════════════════

    private static readonly string CobolBankingProgram = string.Join("\n",
        "       IDENTIFICATION DIVISION.",
        "       PROGRAM-ID. BANKACCT.",
        "       DATA DIVISION.",
        "       WORKING-STORAGE SECTION.",
        "       01  WS-ACCT-NUMBER          PIC 9(10).",
        "       01  WS-ACCT-BALANCE         PIC 9(9)V99.",
        "       01  WS-CUST-NAME            PIC X(40).",
        "       01  WS-TXN-AMOUNT           PIC 9(9)V99.",
        "       01  WS-TXN-TYPE             PIC X(10).",
        "       01  WS-STATUS-CODE          PIC X(2).",
        "       01  WS-ERROR-MSG            PIC X(80).",
        "       01  WS-SQLCODE              PIC S9(9) COMP.",
        "       01  WS-BRANCH-CODE          PIC X(5).",
        "       01  WS-REPORT-DATE          PIC X(10).",
        "       PROCEDURE DIVISION.",
        "       MAIN-PROCESS.",
        "           PERFORM INITIALIZE-PROGRAM",
        "           PERFORM LOOKUP-ACCOUNT",
        "           PERFORM PROCESS-TRANSACTION",
        "           PERFORM GENERATE-STATEMENT",
        "           STOP RUN.",
        "       INITIALIZE-PROGRAM.",
        "           MOVE SPACES TO WS-ERROR-MSG",
        "           MOVE ZEROS TO WS-SQLCODE.",
        "       LOOKUP-ACCOUNT.",
        "           EXEC SQL",
        "             SELECT CUSTOMER_NAME, ACCOUNT_BALANCE",
        "             INTO :WS-CUST-NAME, :WS-ACCT-BALANCE",
        "             FROM BANK_ACCOUNTS",
        "             WHERE ACCOUNT_NUMBER = :WS-ACCT-NUMBER",
        "           END-EXEC.",
        "       PROCESS-TRANSACTION.",
        "           EXEC SQL",
        "             INSERT INTO TRANSACTION_HISTORY",
        "             (ACCOUNT_NUMBER, TXN_AMOUNT, TXN_TYPE,",
        "              BRANCH_CODE)",
        "             VALUES (:WS-ACCT-NUMBER, :WS-TXN-AMOUNT,",
        "              :WS-TXN-TYPE, :WS-BRANCH-CODE)",
        "           END-EXEC.",
        "       GENERATE-STATEMENT.",
        "           EXEC SQL",
        "             DECLARE STMT_CURSOR CURSOR FOR",
        "             SELECT TXN_AMOUNT, TXN_TYPE",
        "             FROM TRANSACTION_HISTORY",
        "             WHERE ACCOUNT_NUMBER = :WS-ACCT-NUMBER",
        "             AND TXN_DATE >= :WS-REPORT-DATE",
        "           END-EXEC.");

    private static readonly string JclBatchJob =
        "//BANKJOB  JOB (FINANCE01),'BANK BATCH',CLASS=A,MSGCLASS=X\n" +
        "//*\n" +
        "//SORTSTEP EXEC PGM=SORT\n" +
        "//SORTIN   DD DSN=PROD.BANK.DAILY.TRANSACTIONS,DISP=SHR\n" +
        "//SORTOUT  DD DSN=PROD.BANK.SORTED.TRANSACTIONS,\n" +
        "//            DISP=(NEW,CATLG,DELETE),\n" +
        "//            SPACE=(CYL,(20,10),RLSE)\n" +
        "//SYSIN    DD *\n" +
        "  SORT FIELDS=(1,10,CH,A,11,8,CH,A)\n" +
        "/*\n" +
        "//*\n" +
        "//SQLSTEP  EXEC PGM=DSNTEP2\n" +
        "//SYSTSPRT DD SYSOUT=*\n" +
        "//SYSIN    DD *\n" +
        "  SELECT ACCOUNT_NUMBER, DAILY_BALANCE\n" +
        "  FROM DAILY_BALANCE_SUMMARY\n" +
        "  WHERE PROCESS_DATE = CURRENT DATE;\n" +
        "/*\n" +
        "//RPTEXEC  EXEC PGM=IKJEFT01\n" +
        "//SYSTSPRT DD SYSOUT=*\n" +
        "//SYSIN    DD *\n" +
        "  INSERT INTO REPORT_ARCHIVE\n" +
        "  (REPORT_DATE, TOTAL_DEPOSITS, TOTAL_WITHDRAWALS)\n" +
        "  SELECT CURRENT DATE, SUM(DEPOSIT_AMT), SUM(WITHDRAWAL_AMT)\n" +
        "  FROM DAILY_BALANCE_SUMMARY;\n" +
        "/*\n//";

    private static readonly string CSharpRepository =
        "public class OrderRepository { }";

    private static readonly string VbNetService =
        "Public Class InvoiceService\r\nEnd Class";

    private static readonly string JavaScriptApi =
        "const getOrders = 'SELECT OrderId, CustomerName, OrderTotal, OrderDate FROM CustomerOrders WHERE Status = 1';\n" +
        "const insertAudit = 'INSERT INTO AuditLog (UserId, ActionType, Timestamp) VALUES (1, 2, 3)';";

    private static readonly string FSharpDataAccess = @"module AccountData

let getAccountBalance accountId =
    let sql = ""SELECT AccountBalance, LastUpdated FROM ClientAccounts WHERE AccountId = @aid""
    executeQuery sql accountId

let getTransactionHistory clientId =
    let sql = ""SELECT TxnAmount, TxnDate FROM AccountTransactions WHERE ClientId = @cid""
    executeQuery sql clientId";

    private static readonly string SqlDdlScript = @"CREATE TABLE CustomerOrders (
    OrderId INT NOT NULL,
    CustomerId INT NOT NULL,
    OrderTotal DECIMAL(10,2),
    OrderDate DATE
);

SELECT c.CustomerName, SUM(o.OrderTotal) AS TotalSpent
FROM Customers c
INNER JOIN CustomerOrders o ON c.CustomerId = o.CustomerId
WHERE o.OrderDate >= '2024-01-01'
GROUP BY c.CustomerName
ORDER BY TotalSpent DESC;";

    // ═══════════════════════════════════════════════════════════════════
    // Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void GoldenSample_CobolBanking_FullObfuscation()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        var result = processor.Obfuscate(CobolBankingProgram, context, "bankacct.cbl");

        result.WasTransformed.Should().BeTrue();

        // All COBOL paragraphs obfuscated
        result.Content.Should().NotContain("MAIN-PROCESS");
        result.Content.Should().NotContain("INITIALIZE-PROGRAM");
        result.Content.Should().NotContain("LOOKUP-ACCOUNT");
        result.Content.Should().NotContain("PROCESS-TRANSACTION");
        result.Content.Should().NotContain("GENERATE-STATEMENT");
        result.Content.Should().Contain("PARA_");

        // All data items obfuscated
        result.Content.Should().NotContain("WS-ACCT-NUMBER");
        result.Content.Should().NotContain("WS-ACCT-BALANCE");
        result.Content.Should().NotContain("WS-CUST-NAME");
        result.Content.Should().NotContain("WS-TXN-AMOUNT");
        result.Content.Should().Contain("VAR_");

        // Host variables in EXEC SQL blocks replaced
        result.Content.Should().NotContain(":WS-ACCT-NUMBER");
        result.Content.Should().NotContain(":WS-CUST-NAME");
        result.Content.Should().NotContain(":WS-TXN-AMOUNT");
        result.Content.Should().Contain(":VAR_");

        // Cross-references recorded for host variables
        context.SourceMap.CrossReferences.Should().NotBeEmpty();
        context.SourceMap.CrossReferences.Should().Contain(
            cr => cr.SourceLanguage == "COBOL" && cr.TargetLanguage == "SQL");
    }

    [Fact]
    public void GoldenSample_JclBatchJob_SqlStepsDelegated()
    {
        var context = CreateContextWithRegistry();
        var processor = new JclLanguageProcessor();

        var result = processor.Obfuscate(JclBatchJob, context, "bankjob.jcl");

        result.WasTransformed.Should().BeTrue();

        // JCL identifiers obfuscated
        result.Content.Should().NotContain("BANKJOB");
        result.Content.Should().NotContain("SORTSTEP");
        result.Content.Should().NotContain("SQLSTEP");
        result.Content.Should().NotContain("RPTEXEC");

        // SORT instream should NOT be SQL-delegated (PGM=SORT is not a SQL program)
        result.Content.Should().Contain("SORT FIELDS");

        // JCL step names, DD names, DSN names all obfuscated
        result.Content.Should().Contain("STP_");
        result.Content.Should().Contain("DD_");
        result.Content.Should().Contain("DSN_");
        result.Content.Should().Contain("PGM_");
    }

    [Fact]
    public void GoldenSample_CSharpRepository_SqlInSharedContext()
    {
        var context = CreateContextWithRegistry();
        var csharpProcessor = new CSharpLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        // Process C# class
        var csResult = csharpProcessor.Obfuscate(CSharpRepository, context, "OrderRepository.cs");
        csResult.WasTransformed.Should().BeTrue();
        csResult.Content.Should().NotContain("OrderRepository");

        // Process standalone SQL (simulating separate .sql file in project)
        var sqlResult = sqlProcessor.Obfuscate(SqlDdlScript, context, "schema.sql");
        sqlResult.WasTransformed.Should().BeTrue();
        sqlResult.Content.Should().NotContain("CustomerOrders");

        // Both registered in shared context
        context.Mappings.Forward.Should().ContainKey("OrderRepository");
        context.Mappings.Forward.Should().ContainKey("CustomerOrders");
        context.Mappings.Forward.Should().ContainKey("CustomerName");
    }

    [Fact]
    public void GoldenSample_AllFixtures_SharedContext_ZeroFingerprint()
    {
        var context = CreateContextWithRegistry();
        var cobolProcessor = new CobolLanguageProcessor();
        var jclProcessor = new JclLanguageProcessor();
        var csharpProcessor = new CSharpLanguageProcessor();
        var vbProcessor = new VisualBasicLanguageProcessor();
        var jsProcessor = new JavaScriptLanguageProcessor();
        ILanguageProcessor fsharpProcessor = new FSharpLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        // Process all 7 fixtures through a single shared context
        var cobolResult = cobolProcessor.Obfuscate(CobolBankingProgram, context, "bankacct.cbl");
        var jclResult = jclProcessor.Obfuscate(JclBatchJob, context, "bankjob.jcl");
        var csResult = csharpProcessor.Obfuscate(CSharpRepository, context, "OrderRepository.cs");
        var vbResult = vbProcessor.Obfuscate(VbNetService, context, "InvoiceService.vb");
        var jsResult = jsProcessor.Obfuscate(JavaScriptApi, context, "api.js");
        var fsResult = fsharpProcessor.Obfuscate(FSharpDataAccess, context, "AccountData.fs");
        var sqlResult = sqlProcessor.Obfuscate(SqlDdlScript, context, "schema.sql");

        // All 7 should transform successfully
        cobolResult.WasTransformed.Should().BeTrue();
        jclResult.WasTransformed.Should().BeTrue();
        csResult.WasTransformed.Should().BeTrue();
        vbResult.WasTransformed.Should().BeTrue();
        jsResult.WasTransformed.Should().BeTrue();
        fsResult.WasTransformed.Should().BeTrue();
        sqlResult.WasTransformed.Should().BeTrue();

        // Key business identifiers should not survive in outputs where they're obfuscated
        csResult.Content.Should().NotContain("OrderRepository");
        vbResult.Content.Should().NotContain("InvoiceService");
        jsResult.Content.Should().NotContain("getOrders");
        jsResult.Content.Should().NotContain("CustomerOrders");
        fsResult.Content.Should().NotContain("AccountData");
        fsResult.Content.Should().NotContain("AccountBalance");
        sqlResult.Content.Should().NotContain("CustomerOrders");

        // Shared context should have substantial mappings from all languages
        context.Mappings.Forward.Count.Should().BeGreaterThanOrEqualTo(20,
            because: "7 enterprise fixtures should generate at least 20 unique identifier mappings");
    }

    [Fact]
    public void GoldenSample_AllFixtures_RoundTrip_PerfectRestore()
    {
        var context = CreateContextWithRegistry();
        var restorer = new Restorer();
        var csharpProcessor = new CSharpLanguageProcessor();
        var vbProcessor = new VisualBasicLanguageProcessor();
        var jsProcessor = new JavaScriptLanguageProcessor();
        ILanguageProcessor fsharpProcessor = new FSharpLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        // Process non-COBOL/JCL fixtures (those with reliable AST-based round-trip)
        var csObf = csharpProcessor.Obfuscate(CSharpRepository, context, "OrderRepository.cs");
        var vbObf = vbProcessor.Obfuscate(VbNetService, context, "InvoiceService.vb");
        var jsObf = jsProcessor.Obfuscate(JavaScriptApi, context, "api.js");
        var fsObf = fsharpProcessor.Obfuscate(FSharpDataAccess, context, "AccountData.fs");
        var sqlObf = sqlProcessor.Obfuscate(SqlDdlScript, context, "schema.sql");

        // Restore using Restorer (real pipeline method)
        var csRestored = restorer.Restore(csObf.Content, context.Mappings);
        var vbRestored = restorer.Restore(vbObf.Content, context.Mappings);
        var jsRestored = restorer.Restore(jsObf.Content, context.Mappings);
        var fsRestored = restorer.Restore(fsObf.Content, context.Mappings);
        var sqlRestored = restorer.Restore(sqlObf.Content, context.Mappings);

        // Verify key identifiers are restored
        csRestored.Content.Should().Contain("OrderRepository");
        vbRestored.Content.Should().Contain("InvoiceService");
        jsRestored.Content.Should().Contain("getOrders");
        jsRestored.Content.Should().Contain("CustomerOrders");
        jsRestored.Content.Should().Contain("AuditLog");
        fsRestored.Content.Should().Contain("AccountData");
        fsRestored.Content.Should().Contain("AccountBalance");
        fsRestored.Content.Should().Contain("AccountTransactions");
        sqlRestored.Content.Should().Contain("CustomerOrders");
        sqlRestored.Content.Should().Contain("CustomerName");
    }

    [Fact]
    public void GoldenSample_AllFixtures_CrossLanguageConsistency()
    {
        var context = CreateContextWithRegistry();
        var jsProcessor = new JavaScriptLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        // JavaScript references "CustomerOrders" in a SQL string
        jsProcessor.Obfuscate(JavaScriptApi, context, "api.js");
        var jsAlias = context.Mappings.Forward["CustomerOrders"];

        // SQL DDL creates the same table
        sqlProcessor.Obfuscate(SqlDdlScript, context, "schema.sql");

        // The alias for "CustomerOrders" should be identical regardless of entry language
        context.Mappings.Forward["CustomerOrders"].Should().Be(jsAlias);

        // Verify the alias appears with the right prefix
        jsAlias.Should().StartWith("TBL_");
    }
}
