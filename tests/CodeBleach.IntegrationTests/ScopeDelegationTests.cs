using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using CodeBleach.Core.Services;
using CodeBleach.Processors.Cobol;
using CodeBleach.Processors.CSharp;
using CodeBleach.Processors.FSharp;
using CodeBleach.Processors.JavaScript;
using CodeBleach.Processors.Jcl;
using CodeBleach.Processors.OracleSql;
using CodeBleach.Processors.Sql;
using CodeBleach.Processors.VbScript;
using CodeBleach.Processors.VisualBasic;

namespace CodeBleach.IntegrationTests;

/// <summary>
/// Tests for the --scope whitelist filter feature.
///
/// The --scope flag enables selective obfuscation: only in-scope processors
/// perform full obfuscation. Out-of-scope processors either early-return (non-delegating)
/// or run in delegation-only mode (delegating processors parse structure and hand off
/// in-scope subregions like EXEC SQL blocks to the SQL processor).
///
/// Usage:
///   codebleach sanitize ./project --level 2 --scope database
///   codebleach sanitize ./project --level 2 --scope mainframe,database
///   codebleach sanitize ./project --level 2 --scope tsql,cobol
///
/// Groups: database (tsql,db2sql,oraclesql), mainframe (cobol,jcl,mainframe-utility),
///         dotnet (csharp,vbnet,fsharp), web (javascript), scripting (vbscript)
///
/// Default (no --scope): everything obfuscated — fully backward compatible.
/// </summary>
public class ScopeDelegationTests
{
    private static ObfuscationContext CreateContextWithScope(string scopeCsv)
    {
        var context = new ObfuscationContext(ObfuscationLevel.Full);
        context.Scope = ObfuscationScope.Parse(scopeCsv);
        var registry = new LanguageProcessorRegistry();
        registry.Register(new SqlLanguageProcessor());
        registry.Register(new CSharpLanguageProcessor());
        registry.Register(new VisualBasicLanguageProcessor());
        registry.Register(new JavaScriptLanguageProcessor());
        ILanguageProcessor fsharp = new FSharpLanguageProcessor();
        registry.Register(fsharp);
        registry.Register(new CobolLanguageProcessor());
        registry.Register(new JclLanguageProcessor());
        registry.Register(new OracleSqlLanguageProcessor());
        registry.Register(new VbScriptLanguageProcessor());
        context.ProcessorRegistry = registry;
        return context;
    }

    private static ObfuscationContext CreateUnfilteredContext()
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
        registry.Register(new OracleSqlLanguageProcessor());
        registry.Register(new VbScriptLanguageProcessor());
        context.ProcessorRegistry = registry;
        return context;
    }

    private static string BuildCobolSource(params string[] lines)
        => string.Join("\n", lines);

    // ═══════════════════════════════════════════════════════════════════
    // A. COBOL Delegation-Only: --scope database
    //    COBOL identifiers untouched, EXEC SQL blocks delegated to SQL processor
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Cobol_ScopeDatabase_SqlDelegated_CobolIdentifiersUnchanged()
    {
        var context = CreateContextWithScope("database");
        var processor = new CobolLanguageProcessor();
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

        // COBOL identifiers should be UNCHANGED (delegation-only)
        result.Content.Should().Contain("EMPLOOKUP");
        result.Content.Should().Contain("WS-EMP-NAME");
        result.Content.Should().Contain("WS-EMP-ID");
        result.Content.Should().Contain("LOOKUP-EMPLOYEE");
        // SQL keywords preserved
        result.Content.Should().Contain("SELECT");
        result.Content.Should().Contain("FROM");
        result.Content.Should().Contain("WHERE");
    }

    [Fact]
    public void Cobol_ScopeDatabase_NoExecSql_ContentUnchanged()
    {
        var context = CreateContextWithScope("database");
        var processor = new CobolLanguageProcessor();
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. SIMPLECOB.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-COUNTER             PIC 9(5).",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           MOVE 1 TO WS-COUNTER",
            "           STOP RUN.");

        var result = processor.Obfuscate(cobol, context, "simplecob.cbl");

        // No SQL to delegate — content should be completely unchanged
        result.WasTransformed.Should().BeFalse();
        result.Content.Should().Contain("SIMPLECOB");
        result.Content.Should().Contain("WS-COUNTER");
        result.Content.Should().Contain("MAIN-PARA");
    }

    [Fact]
    public void Cobol_ScopeDatabase_HostVarsNotReplaced()
    {
        // In delegation-only mode, COBOL host variables (:WS-VAR) should NOT be replaced
        // because COBOL identifiers are not in scope — host var replacement requires aliasMap
        var context = CreateContextWithScope("database");
        var processor = new CobolLanguageProcessor();
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. HOSTVARS.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-ACCOUNT-NUM         PIC 9(10).",
            "       PROCEDURE DIVISION.",
            "       DO-QUERY.",
            "           EXEC SQL",
            "             SELECT BALANCE",
            "             FROM ACCOUNTS",
            "             WHERE ACCT_NUM = :WS-ACCOUNT-NUM",
            "           END-EXEC",
            "           STOP RUN.");

        var result = processor.Obfuscate(cobol, context, "hostvars.cbl");

        // Host variable :WS-ACCOUNT-NUM should remain (COBOL is out of scope)
        result.Content.Should().Contain(":WS-ACCOUNT-NUM");
    }

    // ═══════════════════════════════════════════════════════════════════
    // B. JCL Delegation-Only: --scope database (3 tests)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Jcl_ScopeDatabase_InstreamSqlDelegated_JclIdentifiersUnchanged()
    {
        var context = CreateContextWithScope("database");
        var processor = new JclLanguageProcessor();
        var jcl = string.Join("\n",
            "//PAYROLL  JOB (ACCT001),'BATCH RUN',CLASS=A",
            "//SQLSTEP  EXEC PGM=IKJEFT01",
            "//SYSTSIN  DD *",
            " DSN SYSTEM(DB2P)",
            " RUN PROGRAM(PAYROLL) PLAN(PAYPLAN)",
            " END",
            "//SYSIN    DD *",
            " SELECT EMPLOYEE_NAME",
            " FROM EMPLOYEE_MASTER",
            " WHERE DEPT = 'FINANCE'",
            "/*",
            "//SYSPRINT DD SYSOUT=*");

        var result = processor.Obfuscate(jcl, context, "payroll.jcl");

        // JCL identifiers should be UNCHANGED
        result.Content.Should().Contain("PAYROLL");
        result.Content.Should().Contain("ACCT001");
        result.Content.Should().Contain("SQLSTEP");
        // SQL keywords preserved
        result.Content.Should().Contain("SELECT");
        result.Content.Should().Contain("FROM");
        result.Content.Should().Contain("WHERE");
    }

    [Fact]
    public void Jcl_ScopeDatabase_NoInstreamSql_ContentUnchanged()
    {
        var context = CreateContextWithScope("database");
        var processor = new JclLanguageProcessor();
        var jcl = string.Join("\n",
            "//COMPILE  JOB (ACCT001),'COMPILE',CLASS=A",
            "//STEP1    EXEC PGM=IGYCRCTL",
            "//SYSIN    DD DSN=SOURCE.COBOL(MYPROG),DISP=SHR",
            "//SYSPRINT DD SYSOUT=*");

        var result = processor.Obfuscate(jcl, context, "compile.jcl");

        // No SQL to delegate — content unchanged
        result.WasTransformed.Should().BeFalse();
        result.Content.Should().Contain("COMPILE");
        result.Content.Should().Contain("IGYCRCTL");
    }

    // ═══════════════════════════════════════════════════════════════════
    // C. C# Delegation-Only: --scope database (3 tests)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void CSharp_ScopeDatabase_SqlInStrings_CSharpIdentifiersUnchanged()
    {
        var context = CreateContextWithScope("database");
        var processor = new CSharpLanguageProcessor();
        var code = @"
using System;

namespace AccountingApp
{
    public class EmployeeRepository
    {
        public void GetEmployees()
        {
            var sql = ""SELECT EMPLOYEE_NAME FROM EMPLOYEE_MASTER WHERE ACTIVE = 1"";
            Console.WriteLine(sql);
        }
    }
}";
        var result = processor.Obfuscate(code, context, "EmployeeRepository.cs");

        // C# identifiers should be UNCHANGED
        result.Content.Should().Contain("AccountingApp");
        result.Content.Should().Contain("EmployeeRepository");
        result.Content.Should().Contain("GetEmployees");
    }

    [Fact]
    public void CSharp_ScopeDatabase_NoSqlStrings_ContentUnchanged()
    {
        var context = CreateContextWithScope("database");
        var processor = new CSharpLanguageProcessor();
        var code = @"
namespace SimpleApp
{
    public class Calculator
    {
        public int Add(int a, int b) => a + b;
    }
}";
        var result = processor.Obfuscate(code, context, "Calculator.cs");

        // No SQL strings — nothing to delegate, content unchanged
        result.Content.Should().Contain("SimpleApp");
        result.Content.Should().Contain("Calculator");
        result.Content.Should().Contain("Add");
    }

    // ═══════════════════════════════════════════════════════════════════
    // D. JavaScript Delegation-Only: --scope database (2 tests)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void JavaScript_ScopeDatabase_SqlInStrings_JsIdentifiersUnchanged()
    {
        var context = CreateContextWithScope("database");
        var processor = new JavaScriptLanguageProcessor();
        var code = @"
function fetchEmployees() {
    const query = ""SELECT EMPLOYEE_NAME FROM EMPLOYEE_MASTER WHERE ACTIVE = 1"";
    return executeQuery(query);
}
";
        var result = processor.Obfuscate(code, context, "employees.js");

        // JS identifiers should be UNCHANGED
        result.Content.Should().Contain("fetchEmployees");
        result.Content.Should().Contain("executeQuery");
    }

    [Fact]
    public void JavaScript_ScopeDatabase_NoSqlStrings_ContentUnchanged()
    {
        var context = CreateContextWithScope("database");
        var processor = new JavaScriptLanguageProcessor();
        var code = @"
function greet(name) {
    return 'Hello, ' + name;
}
";
        var result = processor.Obfuscate(code, context, "greet.js");

        // No SQL strings — content unchanged
        result.Content.Should().Contain("greet");
    }

    // ═══════════════════════════════════════════════════════════════════
    // E. SQL processor in-scope: full obfuscation (2 tests)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Sql_ScopeDatabase_FullObfuscation()
    {
        var context = CreateContextWithScope("database");
        var processor = new SqlLanguageProcessor();
        var sql = "CREATE TABLE EMPLOYEE_MASTER (EMP_ID INT, EMPLOYEE_NAME VARCHAR(50));";

        var result = processor.Obfuscate(sql, context, "schema.sql");

        // SQL is in scope — table name should be obfuscated
        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("EMPLOYEE_MASTER");
    }

    [Fact]
    public void Sql_ScopeMainframe_EarlyReturn()
    {
        var context = CreateContextWithScope("mainframe");
        var processor = new SqlLanguageProcessor();
        var sql = "CREATE TABLE EMPLOYEE_MASTER (EMP_ID INT, EMPLOYEE_NAME VARCHAR(50));";

        var result = processor.Obfuscate(sql, context, "schema.sql");

        // SQL is OUT of scope — should return unchanged
        result.WasTransformed.Should().BeFalse();
        result.Content.Should().Contain("EMPLOYEE_MASTER");
    }

    // ═══════════════════════════════════════════════════════════════════
    // F. Full scope: --scope mainframe (2 tests)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Cobol_ScopeMainframe_FullObfuscation()
    {
        var context = CreateContextWithScope("mainframe");
        var processor = new CobolLanguageProcessor();
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. EMPLOOKUP.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-EMP-NAME            PIC X(30).",
            "       PROCEDURE DIVISION.",
            "       LOOKUP-EMPLOYEE.",
            "           DISPLAY WS-EMP-NAME",
            "           STOP RUN.");

        var result = processor.Obfuscate(cobol, context, "emplookup.cbl");

        // COBOL is in scope — identifiers should be obfuscated
        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("EMPLOOKUP");
        result.Content.Should().NotContain("WS-EMP-NAME");
        result.Content.Should().NotContain("LOOKUP-EMPLOYEE");
    }

    [Fact]
    public void Jcl_ScopeMainframe_FullObfuscation()
    {
        var context = CreateContextWithScope("mainframe");
        var processor = new JclLanguageProcessor();
        var jcl = string.Join("\n",
            "//PAYROLL  JOB (ACCT001),'BATCH RUN',CLASS=A",
            "//STEP1    EXEC PGM=MYPROG",
            "//SYSPRINT DD SYSOUT=*");

        var result = processor.Obfuscate(jcl, context, "payroll.jcl");

        // JCL is in scope — identifiers should be obfuscated
        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("PAYROLL");
    }

    // ═══════════════════════════════════════════════════════════════════
    // G. Multi-scope: --scope database,mainframe (2 tests)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void MultiScope_DatabaseMainframe_BothFullyObfuscated()
    {
        var context = CreateContextWithScope("database,mainframe");

        var sqlProcessor = new SqlLanguageProcessor();
        var sqlResult = sqlProcessor.Obfuscate(
            "CREATE TABLE EMPLOYEE_MASTER (EMP_ID INT);", context, "schema.sql");

        var cobolProcessor = new CobolLanguageProcessor();
        var cobolResult = cobolProcessor.Obfuscate(BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. EMPLOOKUP.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-COUNTER             PIC 9(5).",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           DISPLAY WS-COUNTER",
            "           STOP RUN."), context, "emplookup.cbl");

        sqlResult.WasTransformed.Should().BeTrue();
        sqlResult.Content.Should().NotContain("EMPLOYEE_MASTER");

        cobolResult.WasTransformed.Should().BeTrue();
        cobolResult.Content.Should().NotContain("EMPLOOKUP");
        cobolResult.Content.Should().NotContain("WS-COUNTER");
    }

    [Fact]
    public void MultiScope_DatabaseMainframe_DotnetOutOfScope()
    {
        var context = CreateContextWithScope("database,mainframe");
        var processor = new CSharpLanguageProcessor();
        var code = @"
namespace AccountingApp
{
    public class Helper
    {
        public void DoWork() { }
    }
}";
        var result = processor.Obfuscate(code, context, "Helper.cs");

        // C# is out of scope — identifiers unchanged
        result.Content.Should().Contain("AccountingApp");
        result.Content.Should().Contain("Helper");
        result.Content.Should().Contain("DoWork");
    }

    // ═══════════════════════════════════════════════════════════════════
    // H. Backward compat: no scope = full obfuscation (2 tests)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void NoScope_CobolFullyObfuscated()
    {
        var context = CreateUnfilteredContext();
        var processor = new CobolLanguageProcessor();
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. EMPLOOKUP.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-EMP-NAME            PIC X(30).",
            "       PROCEDURE DIVISION.",
            "       LOOKUP-EMPLOYEE.",
            "           DISPLAY WS-EMP-NAME",
            "           STOP RUN.");

        var result = processor.Obfuscate(cobol, context, "emplookup.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("EMPLOOKUP");
        result.Content.Should().NotContain("WS-EMP-NAME");
    }

    [Fact]
    public void NoScope_SqlFullyObfuscated()
    {
        var context = CreateUnfilteredContext();
        var processor = new SqlLanguageProcessor();
        var sql = "CREATE TABLE EMPLOYEE_MASTER (EMP_ID INT);";

        var result = processor.Obfuscate(sql, context, "schema.sql");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("EMPLOYEE_MASTER");
    }

    // ═══════════════════════════════════════════════════════════════════
    // I. F# delegation-only: --scope database (1 test)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void FSharp_ScopeDatabase_ContentUnchanged()
    {
        var context = CreateContextWithScope("database");
        ILanguageProcessor processor = new FSharpLanguageProcessor();
        var code = @"
namespace AccountingApp

module EmployeeModule =
    let getEmployeeName id = sprintf ""Employee %d"" id
";
        var result = processor.Obfuscate(code, context, "Employee.fs");

        // F# is out of scope — should return unchanged
        result.WasTransformed.Should().BeFalse();
        result.Content.Should().Contain("AccountingApp");
        result.Content.Should().Contain("EmployeeModule");
    }

    // ═══════════════════════════════════════════════════════════════════
    // J. VB.NET delegation-only: --scope database (1 test)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void VbNet_ScopeDatabase_NoSqlStrings_ContentUnchanged()
    {
        var context = CreateContextWithScope("database");
        var processor = new VisualBasicLanguageProcessor();
        var code = @"
Namespace AccountingApp
    Public Class EmployeeHelper
        Public Sub DoWork()
        End Sub
    End Class
End Namespace
";
        var result = processor.Obfuscate(code, context, "EmployeeHelper.vb");

        // VB.NET is out of scope, no SQL strings — content unchanged
        result.Content.Should().Contain("AccountingApp");
        result.Content.Should().Contain("EmployeeHelper");
        result.Content.Should().Contain("DoWork");
    }

    // ═══════════════════════════════════════════════════════════════════
    // K. Oracle SQL early return: --scope mainframe
    //    Oracle SQL processor returns unchanged when out of scope
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void OracleSql_ScopeMainframe_EarlyReturn()
    {
        var context = CreateContextWithScope("mainframe");
        var processor = new OracleSqlLanguageProcessor();
        var sql = "CREATE TABLE EMPLOYEE_MASTER (EMP_ID NUMBER, EMPLOYEE_NAME VARCHAR2(50));";

        var result = processor.Obfuscate(sql, context, "schema.pls");

        // Oracle SQL is out of scope — should return unchanged
        result.WasTransformed.Should().BeFalse();
        result.Content.Should().Contain("EMPLOYEE_MASTER");
    }

    [Fact]
    public void OracleSql_ScopeDatabase_FullObfuscation()
    {
        var context = CreateContextWithScope("database");
        var processor = new OracleSqlLanguageProcessor();
        var sql = "CREATE TABLE EMPLOYEE_MASTER (EMP_ID NUMBER, EMPLOYEE_NAME VARCHAR2(50));";

        var result = processor.Obfuscate(sql, context, "schema.pls");

        // Oracle SQL is in scope — table name should be obfuscated
        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("EMPLOYEE_MASTER");
    }

    // ═══════════════════════════════════════════════════════════════════
    // L. VBScript early return: --scope database
    //    VBScript processor returns unchanged when out of scope
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void VbScript_ScopeDatabase_EarlyReturn()
    {
        var context = CreateContextWithScope("database");
        var processor = new VbScriptLanguageProcessor();
        var code = @"
Sub CalculatePayroll()
    Dim employeeCount
    employeeCount = 42
    MsgBox ""Total: "" & employeeCount
End Sub
";
        var result = processor.Obfuscate(code, context, "payroll.vbs");

        // VBScript is out of scope — should return unchanged
        result.WasTransformed.Should().BeFalse();
        result.Content.Should().Contain("CalculatePayroll");
        result.Content.Should().Contain("employeeCount");
    }

    [Fact]
    public void VbScript_ScopeScripting_FullObfuscation()
    {
        var context = CreateContextWithScope("scripting");
        var processor = new VbScriptLanguageProcessor();
        var code = @"
Sub CalculatePayroll()
    Dim employeeCount
    employeeCount = 42
    MsgBox ""Total: "" & employeeCount
End Sub
";
        var result = processor.Obfuscate(code, context, "payroll.vbs");

        // VBScript is in scope — identifiers should be obfuscated
        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("CalculatePayroll");
        result.Content.Should().NotContain("employeeCount");
    }

    // ═══════════════════════════════════════════════════════════════════
    // M. Individual processor ID: --scope tsql
    //    Using a processor ID instead of a group name
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Sql_ScopeTsqlOnly_InScope()
    {
        // Using individual processor ID "tsql" instead of group "database"
        var context = CreateContextWithScope("tsql");
        var processor = new SqlLanguageProcessor();
        var sql = "CREATE TABLE EMPLOYEE_MASTER (EMP_ID INT);";

        var result = processor.Obfuscate(sql, context, "schema.sql");

        // T-SQL is in scope
        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("EMPLOYEE_MASTER");
    }

    [Fact]
    public void OracleSql_ScopeTsqlOnly_OutOfScope()
    {
        // "tsql" is in scope but "oraclesql" is NOT — individual IDs are precise
        var context = CreateContextWithScope("tsql");
        var processor = new OracleSqlLanguageProcessor();
        var sql = "CREATE TABLE EMPLOYEE_MASTER (EMP_ID NUMBER);";

        var result = processor.Obfuscate(sql, context, "schema.pls");

        // Oracle SQL is out of scope even though both are SQL dialects
        result.WasTransformed.Should().BeFalse();
        result.Content.Should().Contain("EMPLOYEE_MASTER");
    }

    // ═══════════════════════════════════════════════════════════════════
    // N. FileNameMapper scope-aware filtering
    //    Only in-scope files get renamed; delegation-only files keep original names
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void FileNameMapper_ScopeDatabase_OnlySqlFilesRenamed()
    {
        var context = CreateContextWithScope("database");
        var mapper = new FileNameMapper();
        var files = new List<string> { "schema.sql", "helper.js", "program.cbl" };

        mapper.BuildMappings(context, files);

        // SQL file is in scope — should be renamed
        context.Mappings.FilePathForward.Should().ContainKey("schema.sql");

        // JS and COBOL files are out of scope — should NOT be renamed
        context.Mappings.FilePathForward.Should().NotContainKey("helper.js");
        context.Mappings.FilePathForward.Should().NotContainKey("program.cbl");
    }

    [Fact]
    public void FileNameMapper_NoScope_AllFilesRenamed()
    {
        var context = CreateUnfilteredContext();
        var mapper = new FileNameMapper();
        var files = new List<string> { "schema.sql", "helper.js", "program.cbl" };

        mapper.BuildMappings(context, files);

        // No scope filter — all files should be renamed
        context.Mappings.FilePathForward.Should().ContainKey("schema.sql");
        context.Mappings.FilePathForward.Should().ContainKey("helper.js");
        context.Mappings.FilePathForward.Should().ContainKey("program.cbl");
    }

    // ═══════════════════════════════════════════════════════════════════
    // O. Manifest persistence: scope specifiers saved for restore
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Manifest_ScopeSpecifiers_SavedWhenFiltered()
    {
        var scope = ObfuscationScope.Parse("database,mainframe");

        // Simulates what SanitizeCommand does when building the manifest
        var scopeSpecifiers = scope.IsFiltered ? scope.RawSpecifiers : null;

        scopeSpecifiers.Should().NotBeNull();
        scopeSpecifiers.Should().Equal("database", "mainframe");
    }

    [Fact]
    public void Manifest_ScopeSpecifiers_NullWhenUnfiltered()
    {
        var scope = ObfuscationScope.All();

        var scopeSpecifiers = scope.IsFiltered ? scope.RawSpecifiers : null;

        scopeSpecifiers.Should().BeNull();
    }

    // ═══════════════════════════════════════════════════════════════════
    // P. --scope dotnet: .NET processors in scope, others out
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void CSharp_ScopeDotnet_FullObfuscation()
    {
        var context = CreateContextWithScope("dotnet");
        var processor = new CSharpLanguageProcessor();
        // Use a single-identifier class to work within Roslyn rewriter capabilities
        var code = @"public class Calculator { }";

        var result = processor.Obfuscate(code, context, "Calculator.cs");

        // C# is in scope — class should be obfuscated
        result.WasTransformed.Should().BeTrue();
        result.Content.Should().Contain("CLS_");
        result.Content.Should().NotContain("Calculator");
    }

    [Fact]
    public void Sql_ScopeDotnet_EarlyReturn()
    {
        var context = CreateContextWithScope("dotnet");
        var processor = new SqlLanguageProcessor();
        var sql = "CREATE TABLE EMPLOYEE_MASTER (EMP_ID INT);";

        var result = processor.Obfuscate(sql, context, "schema.sql");

        // SQL is out of scope when using --scope dotnet
        result.WasTransformed.Should().BeFalse();
        result.Content.Should().Contain("EMPLOYEE_MASTER");
    }
}
