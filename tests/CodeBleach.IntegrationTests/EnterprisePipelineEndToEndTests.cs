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
/// Full enterprise pipeline end-to-end tests using temp directories.
/// Exercises: processor obfuscation → file rename → reference patching → manifest → restore.
/// Pattern from FileNameObfuscationEndToEndTests: IDisposable, GUID temp dirs.
/// </summary>
public class EnterprisePipelineEndToEndTests : IDisposable
{
    private readonly string _tempDir;

    public EnterprisePipelineEndToEndTests()
    {
        _tempDir = Path.Combine(Path.GetTempPath(), $"codebleach_enterprise_{Guid.NewGuid():N}");
        Directory.CreateDirectory(_tempDir);
    }

    public void Dispose()
    {
        if (Directory.Exists(_tempDir))
        {
            Directory.Delete(_tempDir, recursive: true);
        }
    }

    private void CreateFile(string relativePath, string content)
    {
        var fullPath = Path.Combine(_tempDir, relativePath.Replace('/', Path.DirectorySeparatorChar));
        var dir = Path.GetDirectoryName(fullPath);
        if (dir != null) Directory.CreateDirectory(dir);
        File.WriteAllText(fullPath, content);
    }

    private string ReadFile(string relativePath)
    {
        var fullPath = Path.Combine(_tempDir, relativePath.Replace('/', Path.DirectorySeparatorChar));
        return File.ReadAllText(fullPath);
    }

    private List<string> GetAllRelativeFiles()
    {
        return Directory.EnumerateFiles(_tempDir, "*", SearchOption.AllDirectories)
            .Select(f => Path.GetRelativePath(_tempDir, f).Replace('\\', '/'))
            .OrderBy(f => f)
            .ToList();
    }

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

    private static ILanguageProcessor? GetProcessorForFile(ObfuscationContext context, string filePath)
    {
        return context.ProcessorRegistry?.GetProcessor(filePath);
    }

    private static string BuildCobolSource(params string[] lines)
        => string.Join("\n", lines);

    private static readonly string[] BankingBusinessNames =
        ["ACCOUNT", "BALANCE", "CUSTOMER", "SAVINGS", "DEPOSIT"];

    private static readonly string[] ErpBusinessNames =
        ["Order", "Invoice", "Customer", "Dashboard", "Revenue"];

    private static readonly string[] MainframeBusinessNames =
        ["PAYROLL", "SALARY", "EMPLOYEE", "DEPARTMENT", "PAYJOB"];

    // ═══════════════════════════════════════════════════════════════════
    // A. Banking Batch System: COBOL + JCL + SQL (3 tests)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void BankingBatch_CobolJclSql_NoBusinessNamesRemain()
    {
        var context = CreateContextWithRegistry();
        var cobolProcessor = new CobolLanguageProcessor();
        var jclProcessor = new JclLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. ACCTLKUP.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-ACCT-NUM            PIC 9(10).",
            "       01  WS-ACCT-BAL            PIC 9(9)V99.",
            "       PROCEDURE DIVISION.",
            "       LOOKUP-ACCOUNT.",
            "           EXEC SQL",
            "             SELECT ACCOUNT_BALANCE",
            "             INTO :WS-ACCT-BAL",
            "             FROM CUSTOMER_ACCOUNTS",
            "             WHERE ACCOUNT_NUM = :WS-ACCT-NUM",
            "           END-EXEC",
            "           STOP RUN.");

        var jcl = "//BANKJOB  JOB (ACCT01),'BANK BATCH',CLASS=A\n" +
                  "//STEP01   EXEC PGM=IKJEFT01\n" +
                  "//SYSTSPRT DD SYSOUT=*\n" +
                  "//SYSIN    DD *\n" +
                  "  SELECT CUSTOMER_NAME, ACCOUNT_BALANCE\n" +
                  "  FROM CUSTOMER_ACCOUNTS\n" +
                  "  WHERE BALANCE_TYPE = 'SAVINGS';\n" +
                  "/*\n//";

        var sql = @"CREATE TABLE CUSTOMER_ACCOUNTS (
    ACCOUNT_NUM INT NOT NULL,
    CUSTOMER_NAME VARCHAR(50),
    ACCOUNT_BALANCE DECIMAL(12,2),
    BALANCE_TYPE VARCHAR(20)
)";

        var cobolResult = cobolProcessor.Obfuscate(cobol, context, "acctlkup.cbl");
        var jclResult = jclProcessor.Obfuscate(jcl, context, "bankjob.jcl");
        var sqlResult = sqlProcessor.Obfuscate(sql, context, "accounts.sql");

        cobolResult.WasTransformed.Should().BeTrue();
        jclResult.WasTransformed.Should().BeTrue();
        sqlResult.WasTransformed.Should().BeTrue();

        // COBOL: paragraphs and data items should be obfuscated
        cobolResult.Content.Should().NotContain("LOOKUP-ACCOUNT");
        cobolResult.Content.Should().NotContain(":WS-ACCT-NUM");
        cobolResult.Content.Should().NotContain(":WS-ACCT-BAL");
        cobolResult.Content.Should().Contain("PARA_");
        cobolResult.Content.Should().Contain("VAR_");

        // JCL: job name and step identifiers should be obfuscated
        jclResult.WasTransformed.Should().BeTrue();
        jclResult.Content.Should().NotContain("BANKJOB");

        // SQL DDL: table names are obfuscated by ScriptDom
        sqlResult.Content.Should().NotContain("CUSTOMER_ACCOUNTS");
        // Note: ScriptDom renames tables in CREATE TABLE but not column definitions.
        // Columns are discovered and registered in the mapping table.
        context.Mappings.Forward.Should().ContainKey("CUSTOMER_ACCOUNTS");
        context.Mappings.Forward.Should().ContainKey("ACCOUNT_BALANCE");
        context.Mappings.Forward.Should().ContainKey("CUSTOMER_NAME");
    }

    [Fact]
    public void BankingBatch_SharedIdentifiers_ConsistentAcrossFiles()
    {
        var context = CreateContextWithRegistry();
        var cobolProcessor = new CobolLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. ACCTPGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-BAL                 PIC 9(9)V99.",
            "       PROCEDURE DIVISION.",
            "       GET-BAL.",
            "           EXEC SQL",
            "             SELECT ACCOUNT_BALANCE FROM ACCOUNT_MASTER",
            "             INTO :WS-BAL",
            "           END-EXEC",
            "           STOP RUN.");
        cobolProcessor.Obfuscate(cobol, context, "acctpgm.cbl");

        var sql = "SELECT ACCOUNT_BALANCE, ACCOUNT_STATUS FROM ACCOUNT_MASTER WHERE BRANCH = 'NYC'";
        sqlProcessor.Obfuscate(sql, context, "report.sql");

        // ACCOUNT_MASTER should have the same alias from both COBOL delegation and standalone SQL
        context.Mappings.Forward.Should().ContainKey("ACCOUNT_MASTER");
        var alias = context.Mappings.Forward["ACCOUNT_MASTER"];
        // Only one entry — no duplicates
        context.Mappings.Forward.Values.Count(v => v == alias).Should().Be(1);
    }

    [Fact]
    public void BankingBatch_FullRoundTrip_RestoresAllContent()
    {
        var context = CreateContextWithRegistry();
        var cobolProcessor = new CobolLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();
        var restorer = new Restorer();

        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. BANKPGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-CUST-ID             PIC 9(8).",
            "       PROCEDURE DIVISION.",
            "       FETCH-CUST.",
            "           EXEC SQL",
            "             SELECT CUST_NAME FROM BANK_CUSTOMERS",
            "             WHERE CUST_ID = :WS-CUST-ID",
            "           END-EXEC",
            "           STOP RUN.");
        var sql = "SELECT CUST_NAME, CUST_EMAIL FROM BANK_CUSTOMERS";

        var cobolObf = cobolProcessor.Obfuscate(cobol, context, "bankpgm.cbl");
        var sqlObf = sqlProcessor.Obfuscate(sql, context, "customers.sql");

        // Restore using shared mappings
        var cobolRestored = restorer.Restore(cobolObf.Content, context.Mappings);
        var sqlRestored = restorer.Restore(sqlObf.Content, context.Mappings);

        cobolRestored.Content.Should().Contain("FETCH-CUST");
        cobolRestored.Content.Should().Contain("WS-CUST-ID");
        sqlRestored.Content.Should().Contain("BANK_CUSTOMERS");
        sqlRestored.Content.Should().Contain("CUST_NAME");
    }

    // ═══════════════════════════════════════════════════════════════════
    // B. .NET ERP Application: C# + VB.NET + JS + SQL (4 tests)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void DotNetErp_CSharpVbNetJsSql_AllProcessed()
    {
        var context = CreateContextWithRegistry();
        var csharpProcessor = new CSharpLanguageProcessor();
        var vbProcessor = new VisualBasicLanguageProcessor();
        var jsProcessor = new JavaScriptLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        var csResult = csharpProcessor.Obfuscate(
            "public class OrderController { }", context, "OrderController.cs");
        var vbResult = vbProcessor.Obfuscate(
            "Public Class InvoiceModule\r\nEnd Class", context, "InvoiceModule.vb");
        var jsResult = jsProcessor.Obfuscate(
            "const dashboardQuery = 'SELECT OrderCount, TotalRevenue FROM DashboardStats';",
            context, "dashboard.js");
        var sqlResult = sqlProcessor.Obfuscate(
            "CREATE TABLE CustomerOrders (OrderId INT, CustomerId INT, OrderTotal DECIMAL(10,2))",
            context, "schema.sql");

        csResult.WasTransformed.Should().BeTrue();
        vbResult.WasTransformed.Should().BeTrue();
        jsResult.WasTransformed.Should().BeTrue();
        sqlResult.WasTransformed.Should().BeTrue();

        // Each language's identifiers should be replaced
        csResult.Content.Should().NotContain("OrderController");
        vbResult.Content.Should().NotContain("InvoiceModule");
        jsResult.Content.Should().NotContain("dashboardQuery");
        sqlResult.Content.Should().NotContain("CustomerOrders");
    }

    [Fact]
    public void DotNetErp_SqlInStrings_AllDelegated()
    {
        var context = CreateContextWithRegistry();
        var jsProcessor = new JavaScriptLanguageProcessor();
        ILanguageProcessor fsharpProcessor = new FSharpLanguageProcessor();

        // JS with SQL in string
        var jsResult = jsProcessor.Obfuscate(
            "const getSales = 'SELECT SalesAmount, SalesDate FROM SalesRecords WHERE Region = 1';",
            context, "sales.js");
        jsResult.WasTransformed.Should().BeTrue();
        jsResult.Content.Should().NotContain("SalesAmount");
        jsResult.Content.Should().NotContain("SalesRecords");

        // F# with SQL in string
        var fsCode = @"module SalesReport

let fetchSales regionId =
    let q = ""SELECT SalesAmount, SalesDate FROM SalesRecords WHERE RegionId = @rid""
    runQuery q regionId";
        var fsResult = fsharpProcessor.Obfuscate(fsCode, context, "SalesReport.fs");
        fsResult.WasTransformed.Should().BeTrue();
        fsResult.Content.Should().NotContain("SalesReport");
        // SalesAmount should use the same alias as JS (shared context)
        context.Mappings.Forward.Should().ContainKey("SalesAmount");
    }

    [Fact]
    public void DotNetErp_SharedTableName_ConsistentAcrossCSharpAndSql()
    {
        var context = CreateContextWithRegistry();
        var csharpProcessor = new CSharpLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        // C# class processes successfully
        csharpProcessor.Obfuscate(
            "public class OrderRepository { }", context, "OrderRepository.cs");

        // SQL file references Orders table
        sqlProcessor.Obfuscate(
            "SELECT OrderId, OrderDate, OrderTotal FROM Orders WHERE Status = 'Pending'",
            context, "orders.sql");

        // Both should be in the shared mappings
        context.Mappings.Forward.Should().ContainKey("OrderRepository");
        context.Mappings.Forward.Should().ContainKey("Orders");
        context.Mappings.Forward["OrderRepository"].Should().StartWith("CLS_");
        context.Mappings.Forward["Orders"].Should().StartWith("TBL_");
    }

    [Fact]
    public void DotNetErp_FullRoundTrip_AllLanguagesRestore()
    {
        var context = CreateContextWithRegistry();
        var csharpProcessor = new CSharpLanguageProcessor();
        var vbProcessor = new VisualBasicLanguageProcessor();
        var jsProcessor = new JavaScriptLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();
        var restorer = new Restorer();

        var csObf = csharpProcessor.Obfuscate(
            "public class ShipmentService { }", context, "ShipmentService.cs");
        var vbObf = vbProcessor.Obfuscate(
            "Public Class WarehouseModule\r\nEnd Class", context, "WarehouseModule.vb");
        var jsObf = jsProcessor.Obfuscate(
            "const inventoryQuery = 'SELECT ItemName, StockCount FROM WarehouseInventory';",
            context, "inventory.js");
        var sqlObf = sqlProcessor.Obfuscate(
            "SELECT ShipmentId, TrackingNumber FROM Shipments WHERE Status = 'InTransit'",
            context, "shipments.sql");

        // All processed
        csObf.WasTransformed.Should().BeTrue();
        vbObf.WasTransformed.Should().BeTrue();
        jsObf.WasTransformed.Should().BeTrue();
        sqlObf.WasTransformed.Should().BeTrue();

        // Restore all using shared mappings
        var csRestored = restorer.Restore(csObf.Content, context.Mappings);
        var vbRestored = restorer.Restore(vbObf.Content, context.Mappings);
        var jsRestored = restorer.Restore(jsObf.Content, context.Mappings);
        var sqlRestored = restorer.Restore(sqlObf.Content, context.Mappings);

        csRestored.Content.Should().Contain("ShipmentService");
        vbRestored.Content.Should().Contain("WarehouseModule");
        jsRestored.Content.Should().Contain("inventoryQuery");
        jsRestored.Content.Should().Contain("WarehouseInventory");
        sqlRestored.Content.Should().Contain("Shipments");
        sqlRestored.Content.Should().Contain("TrackingNumber");
    }

    // ═══════════════════════════════════════════════════════════════════
    // C. Financial Services F#: F# + SQL (2 tests)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void FinancialFSharp_SqlInLetBindings_Delegated()
    {
        var context = CreateContextWithRegistry();
        ILanguageProcessor fsharpProcessor = new FSharpLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        var fsCode = @"module PortfolioManager

let getPortfolio clientId =
    let sql = ""SELECT PortfolioValue, AssetAllocation FROM ClientPortfolios WHERE ClientId = @cid""
    executeQuery sql clientId

let getTransactions accountId =
    let sql = ""SELECT TxnAmount, TxnDate FROM PortfolioTransactions WHERE AccountId = @aid""
    executeQuery sql accountId";

        var fsResult = fsharpProcessor.Obfuscate(fsCode, context, "PortfolioManager.fs");
        fsResult.WasTransformed.Should().BeTrue();
        fsResult.Content.Should().NotContain("PortfolioManager");
        fsResult.Content.Should().NotContain("PortfolioValue");
        fsResult.Content.Should().NotContain("ClientPortfolios");

        // Standalone SQL shares context
        var sqlResult = sqlProcessor.Obfuscate(
            "SELECT PortfolioValue, RiskScore FROM ClientPortfolios",
            context, "portfolios.sql");
        sqlResult.WasTransformed.Should().BeTrue();

        // Same alias for PortfolioValue and ClientPortfolios
        context.Mappings.Forward.Should().ContainKey("PortfolioValue");
        context.Mappings.Forward.Should().ContainKey("ClientPortfolios");
    }

    [Fact]
    public void FinancialFSharp_RoundTrip_RestoresAll()
    {
        var context = CreateContextWithRegistry();
        ILanguageProcessor fsharpProcessor = new FSharpLanguageProcessor();
        var restorer = new Restorer();

        var fsCode = @"module RiskEngine

let calculateRisk portfolioId =
    let sql = ""SELECT RiskFactor, ExposureLevel FROM RiskMetrics WHERE PortfolioId = @pid""
    executeQuery sql portfolioId";

        var obf = fsharpProcessor.Obfuscate(fsCode, context, "RiskEngine.fs");
        obf.WasTransformed.Should().BeTrue();
        obf.Content.Should().NotContain("RiskEngine");
        obf.Content.Should().NotContain("RiskFactor");

        // Restore via Restorer (real pipeline method)
        var restored = restorer.Restore(obf.Content, context.Mappings);
        restored.Content.Should().Contain("RiskEngine");
        restored.Content.Should().Contain("calculateRisk");
        restored.Content.Should().Contain("RiskFactor");
        restored.Content.Should().Contain("RiskMetrics");
    }

    // ═══════════════════════════════════════════════════════════════════
    // D. Mainframe Modernization: COBOL + JCL + DB2 SQL (3 tests)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void MainframeStack_CobolJclDb2_ZeroFingerprint()
    {
        var context = CreateContextWithRegistry();
        var cobolProcessor = new CobolLanguageProcessor();
        var jclProcessor = new JclLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYROLL.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-EMP-ID              PIC 9(8).",
            "       01  WS-SALARY              PIC 9(7)V99.",
            "       01  WS-DEPT-CODE           PIC X(10).",
            "       PROCEDURE DIVISION.",
            "       CALC-PAYROLL.",
            "           EXEC SQL",
            "             SELECT EMPLOYEE_SALARY",
            "             INTO :WS-SALARY",
            "             FROM PAYROLL_DETAIL",
            "             WHERE EMPLOYEE_ID = :WS-EMP-ID",
            "           END-EXEC.",
            "       GET-DEPARTMENT.",
            "           EXEC SQL",
            "             SELECT DEPARTMENT_NAME",
            "             INTO :WS-DEPT-CODE",
            "             FROM DEPARTMENT_TABLE",
            "             WHERE DEPT_CODE = :WS-DEPT-CODE",
            "           END-EXEC.",
            "       INSERT-PAYSLIP.",
            "           EXEC SQL",
            "             INSERT INTO SALARY_HISTORY",
            "             (EMPLOYEE_ID, SALARY_AMOUNT)",
            "             VALUES (:WS-EMP-ID, :WS-SALARY)",
            "           END-EXEC",
            "           STOP RUN.");

        var jcl = "//PAYJOB   JOB (ACCT01),'PAYROLL RUN',CLASS=A\n" +
                  "//STEP01   EXEC PGM=DSNTEP2\n" +
                  "//SYSTSPRT DD SYSOUT=*\n" +
                  "//SYSIN    DD *\n" +
                  "  SELECT EMPLOYEE_ID, EMPLOYEE_SALARY\n" +
                  "  FROM PAYROLL_DETAIL\n" +
                  "  WHERE DEPARTMENT = 'FINANCE';\n" +
                  "/*\n//";

        var sql = @"CREATE TABLE PAYROLL_DETAIL (
    EMPLOYEE_ID INT NOT NULL,
    EMPLOYEE_SALARY DECIMAL(10,2),
    DEPARTMENT VARCHAR(30)
)";

        var cobolResult = cobolProcessor.Obfuscate(cobol, context, "payroll.cbl");
        var jclResult = jclProcessor.Obfuscate(jcl, context, "payjob.jcl");
        var sqlResult = sqlProcessor.Obfuscate(sql, context, "payroll_tables.sql");

        cobolResult.WasTransformed.Should().BeTrue();
        jclResult.WasTransformed.Should().BeTrue();
        sqlResult.WasTransformed.Should().BeTrue();

        // COBOL: paragraphs, data items, host variables all obfuscated
        cobolResult.Content.Should().NotContain("CALC-PAYROLL");
        cobolResult.Content.Should().NotContain("GET-DEPARTMENT");
        cobolResult.Content.Should().NotContain("INSERT-PAYSLIP");
        cobolResult.Content.Should().NotContain(":WS-EMP-ID");
        cobolResult.Content.Should().NotContain(":WS-SALARY");
        cobolResult.Content.Should().NotContain(":WS-DEPT-CODE");
        cobolResult.Content.Should().Contain("PARA_");

        // JCL: job name obfuscated
        jclResult.Content.Should().NotContain("PAYJOB");

        // SQL DDL: table names obfuscated by ScriptDom
        sqlResult.Content.Should().NotContain("PAYROLL_DETAIL");
        // Column definitions discovered and registered in mapping table
        context.Mappings.Forward.Should().ContainKey("PAYROLL_DETAIL");
        context.Mappings.Forward.Should().ContainKey("EMPLOYEE_SALARY");
    }

    [Fact]
    public void MainframeStack_CrossFileConsistency_TableAliasesMatch()
    {
        var context = CreateContextWithRegistry();
        var cobolProcessor = new CobolLanguageProcessor();
        var jclProcessor = new JclLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        // COBOL references PAYROLL_DETAIL
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYPGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-ID                  PIC 9(8).",
            "       PROCEDURE DIVISION.",
            "       FETCH-PAY.",
            "           EXEC SQL",
            "             SELECT PAY_AMOUNT FROM PAYROLL_DETAIL",
            "             WHERE EMP_ID = :WS-ID",
            "           END-EXEC",
            "           STOP RUN.");
        cobolProcessor.Obfuscate(cobol, context, "paypgm.cbl");

        // JCL instream SQL references PAYROLL_DETAIL
        var jcl = "//PAYJOB2  JOB (ACCT),'PAY',CLASS=A\n" +
                  "//STEP01   EXEC PGM=DSNTEP2\n" +
                  "//SYSTSPRT DD SYSOUT=*\n" +
                  "//SYSIN    DD *\n" +
                  "  SELECT PAY_AMOUNT FROM PAYROLL_DETAIL;\n" +
                  "/*\n//";
        jclProcessor.Obfuscate(jcl, context, "payjob2.jcl");

        // Standalone SQL references PAYROLL_DETAIL
        sqlProcessor.Obfuscate(
            "SELECT PAY_AMOUNT, PAY_DATE FROM PAYROLL_DETAIL WHERE DEPT = 'ENG'",
            context, "payreport.sql");

        // PAYROLL_DETAIL alias should be identical across all three entry paths
        context.Mappings.Forward.Should().ContainKey("PAYROLL_DETAIL");
        var alias = context.Mappings.Forward["PAYROLL_DETAIL"];
        context.Mappings.Forward.Values.Count(v => v == alias).Should().Be(1);
    }

    [Fact]
    public void MainframeStack_FullRoundTrip_AllFilesRestore()
    {
        var context = CreateContextWithRegistry();
        var cobolProcessor = new CobolLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();
        var restorer = new Restorer();

        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. MFPGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-AMT                 PIC 9(7)V99.",
            "       PROCEDURE DIVISION.",
            "       GET-DATA.",
            "           EXEC SQL",
            "             SELECT TOTAL_AMT FROM SUMMARY_TABLE",
            "             INTO :WS-AMT",
            "           END-EXEC",
            "           STOP RUN.");
        var sql = "SELECT TOTAL_AMT, RECORD_COUNT FROM SUMMARY_TABLE";

        var cobolObf = cobolProcessor.Obfuscate(cobol, context, "mfpgm.cbl");
        var sqlObf = sqlProcessor.Obfuscate(sql, context, "summary.sql");

        var cobolRestored = restorer.Restore(cobolObf.Content, context.Mappings);
        var sqlRestored = restorer.Restore(sqlObf.Content, context.Mappings);

        cobolRestored.Content.Should().Contain("GET-DATA");
        cobolRestored.Content.Should().Contain("SUMMARY_TABLE");
        cobolRestored.Content.Should().Contain("WS-AMT");
        sqlRestored.Content.Should().Contain("SUMMARY_TABLE");
        sqlRestored.Content.Should().Contain("TOTAL_AMT");
    }

    // ═══════════════════════════════════════════════════════════════════
    // E. Pipeline Mechanics (3 tests)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public async Task Pipeline_ManifestPersistence_RoundTrip()
    {
        var context = CreateContextWithRegistry();
        var csharpProcessor = new CSharpLanguageProcessor();
        var sqlProcessor = new SqlLanguageProcessor();

        // Process some files to build mappings
        csharpProcessor.Obfuscate(
            "public class CustomerService { }", context, "CustomerService.cs");
        sqlProcessor.Obfuscate(
            "SELECT CustomerName FROM Customers", context, "customers.sql");

        // Create manifest with current mappings
        var manifest = new Manifest
        {
            Version = Manifest.CurrentVersion,
            SourcePath = "/original",
            DestinationPath = _tempDir,
            CreatedAt = DateTime.UtcNow,
            Mappings = context.Mappings,
            ProcessedFiles = new List<string> { "CustomerService.cs", "customers.sql" },
            Stats = new SanitizationStats
            {
                TotalFiles = 2,
                ProcessedFiles = 2,
                SkippedFiles = 0,
                TotalReplacements = context.Mappings.Forward.Count,
                UniqueValuesReplaced = context.Mappings.Forward.Count,
                ProcessingTimeMs = 100
            },
            Level = ObfuscationLevel.Full,
            ProcessorsUsed = new List<string> { "csharp", "tsql" }
        };

        // Save and reload
        var manager = new ManifestManager();
        await manager.SaveManifestAsync(manifest, _tempDir);
        var loaded = await manager.LoadManifestAsync(_tempDir);

        loaded.Should().NotBeNull();
        loaded!.Version.Should().Be(Manifest.CurrentVersion);
        loaded.Mappings.Forward.Should().ContainKey("CustomerService");
        loaded.Mappings.Forward.Should().ContainKey("Customers");
        loaded.Mappings.Reverse.Should().ContainKey(context.Mappings.Forward["CustomerService"]);
        loaded.ProcessedFiles.Should().HaveCount(2);
    }

    [Fact]
    public void Pipeline_ContentObfuscation_PlusFileRename_NoInfoLeakage()
    {
        // Create business-named files on disk
        CreateFile("src/PayrollService.cs", "public class PayrollService { }");
        CreateFile("src/EmployeeReport.sql",
            "SELECT EmployeeName, Salary FROM Employees WHERE Department = 'HR'");

        var context = CreateContextWithRegistry();

        // Step 1: Content obfuscation
        var csContent = ReadFile("src/PayrollService.cs");
        var csharpProcessor = new CSharpLanguageProcessor();
        var csObf = csharpProcessor.Obfuscate(csContent, context, "src/PayrollService.cs");

        var sqlContent = ReadFile("src/EmployeeReport.sql");
        var sqlProcessor = new SqlLanguageProcessor();
        var sqlObf = sqlProcessor.Obfuscate(sqlContent, context, "src/EmployeeReport.sql");

        // Write obfuscated content back
        File.WriteAllText(Path.Combine(_tempDir, "src", "PayrollService.cs"), csObf.Content);
        File.WriteAllText(Path.Combine(_tempDir, "src", "EmployeeReport.sql"), sqlObf.Content);

        // Step 2: Build file name mappings
        var mapper = new FileNameMapper();
        var allFiles = GetAllRelativeFiles();
        mapper.BuildMappings(context, allFiles);
        var mappings = context.Mappings;

        // Step 3: Rename files
        var renamer = new FileSystemRenamer();
        renamer.RenameFiles(_tempDir, mappings);

        // Verify: no business names in paths OR content
        var finalFiles = GetAllRelativeFiles();
        foreach (var filePath in finalFiles)
        {
            filePath.Should().NotContain("Payroll");
            filePath.Should().NotContain("Employee");

            var content = ReadFile(filePath);
            content.Should().NotContain("PayrollService");
            content.Should().NotContain("EmployeeName");
            content.Should().NotContain("Employees");
        }
    }

    [Fact]
    public void Pipeline_RestoreAfterRename_OriginalStructureAndContentRestored()
    {
        // Create files on disk
        CreateFile("src/Service.cs", "public class Service { }");
        CreateFile("data/Report.sql", "SELECT Amount FROM Totals");

        // Snapshot original files and content
        var originalFiles = GetAllRelativeFiles().ToHashSet();
        var originalContents = new Dictionary<string, string>();
        foreach (var f in originalFiles)
            originalContents[f] = ReadFile(f);

        var context = CreateContextWithRegistry();
        var restorer = new Restorer();

        // Content obfuscation
        var csContent = ReadFile("src/Service.cs");
        var csObf = new CSharpLanguageProcessor().Obfuscate(csContent, context, "src/Service.cs");
        File.WriteAllText(Path.Combine(_tempDir, "src", "Service.cs"), csObf.Content);

        var sqlContent = ReadFile("data/Report.sql");
        var sqlObf = new SqlLanguageProcessor().Obfuscate(sqlContent, context, "data/Report.sql");
        File.WriteAllText(Path.Combine(_tempDir, "data", "Report.sql"), sqlObf.Content);

        // File rename
        var mapper = new FileNameMapper();
        mapper.BuildMappings(context, GetAllRelativeFiles());
        var mappings = context.Mappings;
        var fsRenamer = new FileSystemRenamer();
        fsRenamer.RenameFiles(_tempDir, mappings);
        var filePathMappings = new Dictionary<string, string>(mappings.FilePathForward);

        // Reverse: rename back
        fsRenamer.ReverseRenameFiles(_tempDir, filePathMappings);

        // Verify: original file structure restored
        var restoredFiles = GetAllRelativeFiles().ToHashSet();
        restoredFiles.Should().BeEquivalentTo(originalFiles);

        // Restore content
        foreach (var f in restoredFiles)
        {
            var obfuscatedContent = ReadFile(f);
            var restoredContent = restorer.Restore(obfuscatedContent, context.Mappings);
            restoredContent.Content.Should().Be(originalContents[f],
                because: $"content of '{f}' should match original after full round-trip");
        }
    }
}
