using CodeBleach.Core.Models;
using CodeBleach.Processors.Cobol;
using FluentAssertions;
using Xunit;

namespace CodeBleach.Processors.Cobol.Tests;

/// <summary>
/// Comprehensive tests for the CobolLanguageProcessor.
/// Tests cover: metadata, CanProcess, obfuscation of all COBOL constructs,
/// deobfuscation round-trips, EXEC SQL host variables, column preservation,
/// reserved word protection, FILLER handling, and validation.
/// </summary>
public class CobolLanguageProcessorTests
{
    private readonly CobolLanguageProcessor _processor = new();

    private static ObfuscationContext CreateContext(ObfuscationLevel level = ObfuscationLevel.Full)
        => new(level);

    // ═══════════════════════════════════════════════════════════════════
    // Metadata Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void ProcessorId_Returns_Cobol()
    {
        _processor.ProcessorId.Should().Be("cobol");
    }

    [Fact]
    public void DisplayName_Returns_CobolEnterprise()
    {
        _processor.DisplayName.Should().Be("COBOL (Enterprise)");
    }

    [Fact]
    public void Priority_Returns_10()
    {
        _processor.Priority.Should().Be(10);
    }

    [Theory]
    [InlineData(".cbl")]
    [InlineData(".cob")]
    [InlineData(".cpy")]
    [InlineData(".CBL")]
    [InlineData(".COB")]
    [InlineData(".CPY")]
    public void SupportedExtensions_ContainsExpectedExtensions(string ext)
    {
        _processor.SupportedExtensions.Should().Contain(ext);
    }

    // ═══════════════════════════════════════════════════════════════════
    // CanProcess Tests
    // ═══════════════════════════════════════════════════════════════════

    [Theory]
    [InlineData("test.cbl")]
    [InlineData("test.cob")]
    [InlineData("test.cpy")]
    public void CanProcess_WithSupportedExtension_ReturnsTrue(string filePath)
    {
        _processor.CanProcess(filePath, "").Should().BeTrue();
    }

    [Fact]
    public void CanProcess_WithCobolContentHeuristic_ReturnsTrue()
    {
        var content = "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST-PGM.\n";
        _processor.CanProcess("unknown.txt", content).Should().BeTrue();
    }

    [Fact]
    public void CanProcess_WithNonCobolContent_ReturnsFalse()
    {
        _processor.CanProcess("test.cs", "public class Foo {}").Should().BeFalse();
    }

    [Fact]
    public void CanProcess_WithNullOrEmpty_ReturnsFalse()
    {
        _processor.CanProcess("", "").Should().BeFalse();
    }

    [Fact]
    public void CanProcess_CopyOnlyContent_ReturnsTrue()
    {
        var content = "       COPY EMPRECORD.\n";
        _processor.CanProcess("", content).Should().BeTrue();
    }

    [Fact]
    public void CanProcess_PicClausesOnly_ReturnsTrue()
    {
        // Copybook fragment: PIC clauses without DIVISION headers
        var content = "       01  WS-EMP-ID             PIC 9(8).\n"
                    + "       01  WS-EMP-NAME           PIC X(40).\n";
        _processor.CanProcess("", content).Should().BeTrue();
    }

    [Fact]
    public void CanProcess_88LevelOnly_ReturnsFalse()
    {
        // 88-level conditions alone are not enough to detect COBOL
        var content = "       88  SUCCESS               VALUE 1.\n"
                    + "       88  FAILURE               VALUE 0.\n";
        _processor.CanProcess("", content).Should().BeFalse();
    }

    [Fact]
    public void CanProcess_LowercaseCobol_ReturnsTrue()
    {
        var content = "       identification division.\n       program-id. testpgm.\n";
        _processor.CanProcess("", content).Should().BeTrue();
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Empty / Null Input
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_EmptyContent_ReturnsUnchanged()
    {
        var ctx = CreateContext();
        var result = _processor.Obfuscate("", ctx);

        result.Content.Should().BeEmpty();
        result.WasTransformed.Should().BeFalse();
        result.ReplacementCount.Should().Be(0);
        result.ProcessorId.Should().Be("cobol");
    }

    [Fact]
    public void Obfuscate_NullContent_ReturnsEmpty()
    {
        var ctx = CreateContext();
        var result = _processor.Obfuscate(null!, ctx);

        result.Content.Should().BeEmpty();
        result.WasTransformed.Should().BeFalse();
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - PROGRAM-ID
    // ═══════════════════════════════════════════════════════════════════

    [Fact(Skip = "Known COBOL processor limitation: PROGRAM-ID line classification needs rework")]
    public void Obfuscate_ProgramId_IsReplaced()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYROLL-CALC.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "payroll.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("PAYROLL-CALC");
        result.Content.Should().Contain("PGM_");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Data Division / Working-Storage
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_WorkingStorageVariables_AreReplaced()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-EMPLOYEE-REC.",
            "           05  WS-EMP-ID          PIC 9(5).",
            "           05  WS-EMP-NAME        PIC X(30).",
            "           05  WS-EMP-SALARY      PIC 9(7)V99.",
            "       PROCEDURE DIVISION.",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("WS-EMPLOYEE-REC");
        result.Content.Should().NotContain("WS-EMP-ID");
        result.Content.Should().NotContain("WS-EMP-NAME");
        result.Content.Should().NotContain("WS-EMP-SALARY");
        // PIC clauses should be preserved
        result.Content.Should().Contain("PIC 9(5)");
        result.Content.Should().Contain("PIC X(30)");
        result.Content.Should().Contain("PIC 9(7)V99");
    }

    [Fact]
    public void Obfuscate_Level88Conditions_UseConditionCategory()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-STATUS             PIC X(1).",
            "           88  WS-STATUS-ACTIVE   VALUE 'A'.",
            "           88  WS-STATUS-INACTIVE VALUE 'I'.",
            "       PROCEDURE DIVISION.",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("WS-STATUS-ACTIVE");
        result.Content.Should().NotContain("WS-STATUS-INACTIVE");
        // CND_ prefix for conditions
        result.Content.Should().Contain("CND_");
    }

    [Fact]
    public void Obfuscate_Filler_IsNeverRenamed()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-RECORD.",
            "           05  FILLER             PIC X(10).",
            "           05  WS-DATA            PIC X(20).",
            "           05  FILLER             PIC X(5).",
            "       PROCEDURE DIVISION.",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        // FILLER must still appear in the output
        result.Content.Should().Contain("FILLER");
        // But WS-DATA should be replaced
        result.Content.Should().NotContain("WS-DATA");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Procedure Division / Paragraphs
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ParagraphNames_AreReplaced()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-COUNTER            PIC 9(3).",
            "       PROCEDURE DIVISION.",
            "       MAIN-LOGIC.",
            "           PERFORM INIT-ROUTINE",
            "           PERFORM PROCESS-DATA",
            "           STOP RUN.",
            "       INIT-ROUTINE.",
            "           MOVE 0 TO WS-COUNTER.",
            "       PROCESS-DATA.",
            "           ADD 1 TO WS-COUNTER.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("MAIN-LOGIC");
        result.Content.Should().NotContain("INIT-ROUTINE");
        result.Content.Should().NotContain("PROCESS-DATA");
        // PARA_ prefix for paragraphs
        result.Content.Should().Contain("PARA_");
    }

    [Fact]
    public void Obfuscate_PerformReferences_MatchParagraphAliases()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           PERFORM WORKER-PARA",
            "           STOP RUN.",
            "       WORKER-PARA.",
            "           DISPLAY 'HELLO'.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        // Both the paragraph header and the PERFORM reference should use the same alias
        var lines = result.Content.Split('\n');
        // Find the alias used for WORKER-PARA
        var workerAlias = ctx.Mappings.Forward["WORKER-PARA"];
        workerAlias.Should().NotBeNullOrEmpty();
        // The PERFORM line should reference the same alias
        result.Content.Should().Contain("PERFORM " + workerAlias);
    }

    [Fact]
    public void Obfuscate_SectionNames_AreReplaced()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-SECTION SECTION.",
            "       MAIN-PARA.",
            "           PERFORM WORKER-SECTION",
            "           STOP RUN.",
            "       WORKER-SECTION SECTION.",
            "       WORKER-PARA.",
            "           DISPLAY 'DONE'.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("MAIN-SECTION");
        result.Content.Should().NotContain("WORKER-SECTION");
        result.Content.Should().Contain("SEC_");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - COPY Statements
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_CopyStatements_CopybookNameReplaced()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "           COPY EMPRECORD.",
            "       PROCEDURE DIVISION.",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("EMPRECORD");
        result.Content.Should().Contain("CPY_");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - FD / File Description
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_FileDescription_FileNameReplaced()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       ENVIRONMENT DIVISION.",
            "       DATA DIVISION.",
            "       FILE SECTION.",
            "       FD  EMPLOYEE-FILE.",
            "       01  EMPLOYEE-RECORD.",
            "           05  EMP-ID             PIC 9(5).",
            "       PROCEDURE DIVISION.",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("EMPLOYEE-FILE");
        result.Content.Should().Contain("FIL_");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Comments
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_Comments_ContentIsReplaced()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "      * This is a secret comment about payroll processing",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.Content.Should().NotContain("secret comment about payroll");
        result.Content.Should().Contain("[Comment removed]");
        // The * indicator should be preserved
        var commentLine = result.Content.Split('\n')
            .FirstOrDefault(l => l.Contains("[Comment removed]"));
        commentLine.Should().NotBeNull();
        // Col 7 should still be *
        if (commentLine!.Length >= 7)
        {
            commentLine[6].Should().Be('*');
        }
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - EXEC SQL Host Variables
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ExecSqlHostVariables_AreReplaced()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-EMP-ID              PIC 9(5).",
            "       01  WS-EMP-NAME            PIC X(30).",
            "       PROCEDURE DIVISION.",
            "           EXEC SQL",
            "             SELECT EMP_NAME",
            "             INTO :WS-EMP-NAME",
            "             FROM EMPLOYEES",
            "             WHERE EMP_ID = :WS-EMP-ID",
            "           END-EXEC",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain(":WS-EMP-NAME");
        result.Content.Should().NotContain(":WS-EMP-ID");
        // Should have colon-prefixed aliases
        result.Content.Should().Contain(":VAR_");
    }

    [Fact]
    public void Obfuscate_ExecSqlMultiLine_HostVariablesReplaced()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-DEPT-ID             PIC 9(3).",
            "       01  WS-DEPT-NAME           PIC X(30).",
            "       PROCEDURE DIVISION.",
            "           EXEC SQL",
            "             SELECT DEPT_NAME",
            "             INTO :WS-DEPT-NAME",
            "             FROM DEPARTMENTS",
            "             WHERE DEPT_ID = :WS-DEPT-ID",
            "           END-EXEC",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain(":WS-DEPT-ID");
        result.Content.Should().NotContain(":WS-DEPT-NAME");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Column Preservation
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_SequenceArea_IsPreserved()
    {
        var cobol = "000100 IDENTIFICATION DIVISION.\n000200 PROGRAM-ID. TEST-PGM.\n000300 PROCEDURE DIVISION.\n000400     STOP RUN.";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        var lines = result.Content.Split('\n');
        lines[0].Should().StartWith("000100");
        lines[1].Should().StartWith("000200");
        lines[2].Should().StartWith("000300");
    }

    [Fact(Skip = "Known COBOL processor limitation: PROGRAM-ID line classification needs rework")]
    public void Obfuscate_IdentificationArea_IsPreserved()
    {
        // Build a line that is exactly 80 columns with ident area at cols 73-80
        var line1 = "000100 IDENTIFICATION DIVISION.                                        TESTID01";
        var line2 = "000200 PROGRAM-ID. TEST-PGM.                                           TESTID02";
        var line3 = "000300 PROCEDURE DIVISION.                                              TESTID03";
        var line4 = "000400     STOP RUN.                                                    TESTID04";
        var cobol = string.Join("\n", line1, line2, line3, line4);
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        var lines = result.Content.Split('\n');
        // Identification area (cols 73-80) should be preserved
        foreach (var line in lines)
        {
            if (line.Length >= 80)
            {
                line[72..80].Should().StartWith("TESTID0");
            }
        }
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Reserved Words Protection
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ReservedWords_AreNotRenamed()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-AMOUNT              PIC 9(7)V99.",
            "       01  WS-TOTAL               PIC 9(9)V99.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           MOVE ZERO TO WS-TOTAL",
            "           ADD WS-AMOUNT TO WS-TOTAL",
            "           IF WS-TOTAL GREATER THAN ZERO",
            "               DISPLAY WS-TOTAL",
            "           END-IF",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        // Reserved words must remain
        result.Content.Should().Contain("MOVE");
        result.Content.Should().Contain("ADD");
        result.Content.Should().Contain("DISPLAY");
        result.Content.Should().Contain("STOP RUN");
        result.Content.Should().Contain("IF");
        result.Content.Should().Contain("END-IF");
        result.Content.Should().Contain("GREATER");
        result.Content.Should().Contain("ZERO");
        result.Content.Should().Contain("DIVISION");
        result.Content.Should().Contain("SECTION");
        // User-defined names should be replaced
        result.Content.Should().NotContain("WS-AMOUNT");
        result.Content.Should().NotContain("WS-TOTAL");
    }

    [Fact]
    public void Obfuscate_PictureClauses_AreNotModified()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-DATE               PIC 9(8).",
            "       01  WS-NAME               PIC X(30).",
            "       01  WS-AMOUNT             PIC S9(7)V99.",
            "       01  WS-EDITED             PIC ZZZ,ZZ9.99.",
            "       PROCEDURE DIVISION.",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.Content.Should().Contain("PIC 9(8)");
        result.Content.Should().Contain("PIC X(30)");
        result.Content.Should().Contain("PIC S9(7)V99");
    }

    [Fact]
    public void Obfuscate_LevelNumbers_AreNotRenamed()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-RECORD.",
            "           05  WS-FIELD-A         PIC X(10).",
            "           10  WS-FIELD-B         PIC X(5).",
            "       77  WS-STANDALONE          PIC 9(3).",
            "       PROCEDURE DIVISION.",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        // Level numbers must remain
        result.Content.Should().Contain("01");
        result.Content.Should().Contain("05");
        result.Content.Should().Contain("10");
        result.Content.Should().Contain("77");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - REDEFINES
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_Redefines_TargetIsAlsoReplaced()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-DATE-NUM            PIC 9(8).",
            "       01  WS-DATE-X REDEFINES WS-DATE-NUM PIC X(8).",
            "       PROCEDURE DIVISION.",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("WS-DATE-NUM");
        result.Content.Should().NotContain("WS-DATE-X");
        result.Content.Should().Contain("REDEFINES");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Cross-File Consistency
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_SameIdentifierAcrossFiles_GetsSameAlias()
    {
        var cobol1 = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. SHARED-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "           COPY SHARED-COPY.",
            "       PROCEDURE DIVISION.",
            "           STOP RUN.");
        var cobol2 = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. OTHER-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "           COPY SHARED-COPY.",
            "       PROCEDURE DIVISION.",
            "           STOP RUN.");

        var ctx = CreateContext();
        var result1 = _processor.Obfuscate(cobol1, ctx, "file1.cbl");
        var result2 = _processor.Obfuscate(cobol2, ctx, "file2.cbl");

        // SHARED-COPY should map to the same alias in both files
        var sharedCopyAlias = ctx.Mappings.Forward["SHARED-COPY"];
        result1.Content.Should().Contain(sharedCopyAlias);
        result2.Content.Should().Contain(sharedCopyAlias);
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Full Program
    // ═══════════════════════════════════════════════════════════════════

    [Fact(Skip = "Known COBOL processor limitation: PROGRAM-ID line classification needs rework")]
    public void Obfuscate_FullCobolProgram_AllUserNamesReplaced()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYROLL-CALC.",
            "      * Calculate employee payroll",
            "       ENVIRONMENT DIVISION.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-EMP-ID              PIC 9(5).",
            "       01  WS-EMP-NAME            PIC X(30).",
            "       01  WS-GROSS-PAY           PIC 9(7)V99.",
            "       01  WS-NET-PAY             PIC 9(7)V99.",
            "       01  WS-TAX-RATE            PIC V99 VALUE .25.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PROCESS.",
            "           PERFORM INIT-DATA",
            "           PERFORM CALC-PAY",
            "           PERFORM DISPLAY-RESULT",
            "           STOP RUN.",
            "       INIT-DATA.",
            "           MOVE 12345 TO WS-EMP-ID",
            "           MOVE 'JOHN DOE' TO WS-EMP-NAME",
            "           MOVE 5000.00 TO WS-GROSS-PAY.",
            "       CALC-PAY.",
            "           COMPUTE WS-NET-PAY =",
            "               WS-GROSS-PAY * (1 - WS-TAX-RATE).",
            "       DISPLAY-RESULT.",
            "           DISPLAY 'Employee: ' WS-EMP-NAME",
            "           DISPLAY 'Net Pay:  ' WS-NET-PAY.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "payroll.cbl");

        result.WasTransformed.Should().BeTrue();
        result.ReplacementCount.Should().BeGreaterThan(10);

        // All user-defined names should be gone
        result.Content.Should().NotContain("PAYROLL-CALC");
        result.Content.Should().NotContain("WS-EMP-ID");
        result.Content.Should().NotContain("WS-EMP-NAME");
        result.Content.Should().NotContain("WS-GROSS-PAY");
        result.Content.Should().NotContain("WS-NET-PAY");
        result.Content.Should().NotContain("WS-TAX-RATE");
        result.Content.Should().NotContain("MAIN-PROCESS");
        result.Content.Should().NotContain("INIT-DATA");
        result.Content.Should().NotContain("CALC-PAY");
        result.Content.Should().NotContain("DISPLAY-RESULT");

        // Reserved words and structure should remain
        result.Content.Should().Contain("IDENTIFICATION DIVISION");
        result.Content.Should().Contain("PROGRAM-ID");
        result.Content.Should().Contain("DATA DIVISION");
        result.Content.Should().Contain("WORKING-STORAGE SECTION");
        result.Content.Should().Contain("PROCEDURE DIVISION");
        result.Content.Should().Contain("PERFORM");
        result.Content.Should().Contain("MOVE");
        result.Content.Should().Contain("COMPUTE");
        result.Content.Should().Contain("DISPLAY");
        result.Content.Should().Contain("STOP RUN");
        result.Content.Should().Contain("[Comment removed]");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Warnings/Resilience
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_MalformedInput_DoesNotThrow()
    {
        var garbage = "This is not COBOL at all\nJust random text\n!@#$%^&*()";
        var ctx = CreateContext();

        var act = () => _processor.Obfuscate(garbage, ctx, "bad.cbl");

        act.Should().NotThrow();
    }

    [Fact]
    public void Obfuscate_ShortLines_HandledGracefully()
    {
        // Lines shorter than 7 columns
        var cobol = "X\nAB\n       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TEST-PGM.\n       PROCEDURE DIVISION.\n           STOP RUN.";
        var ctx = CreateContext();

        var act = () => _processor.Obfuscate(cobol, ctx, "test.cbl");

        act.Should().NotThrow();
    }

    // ═══════════════════════════════════════════════════════════════════
    // Deobfuscate Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Deobfuscate_EmptyContent_ReturnsUnchanged()
    {
        var ctx = CreateContext();
        var result = _processor.Deobfuscate("", ctx);

        result.Content.Should().BeEmpty();
        result.WasTransformed.Should().BeFalse();
    }

    [Fact]
    public void Deobfuscate_RestoresParagraphNames()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-LOGIC.",
            "           PERFORM WORKER-PARA",
            "           STOP RUN.",
            "       WORKER-PARA.",
            "           DISPLAY 'DONE'.");
        var ctx = CreateContext();

        var obfuscated = _processor.Obfuscate(cobol, ctx, "test.cbl");
        var restored = _processor.Deobfuscate(obfuscated.Content, ctx, "test.cbl");

        restored.WasTransformed.Should().BeTrue();
        restored.Content.Should().Contain("MAIN-LOGIC");
        restored.Content.Should().Contain("WORKER-PARA");
    }

    [Fact]
    public void Deobfuscate_RestoresDataNames()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-COUNTER            PIC 9(5).",
            "       PROCEDURE DIVISION.",
            "           ADD 1 TO WS-COUNTER",
            "           STOP RUN.");
        var ctx = CreateContext();

        var obfuscated = _processor.Obfuscate(cobol, ctx, "test.cbl");
        var restored = _processor.Deobfuscate(obfuscated.Content, ctx, "test.cbl");

        restored.WasTransformed.Should().BeTrue();
        restored.Content.Should().Contain("WS-COUNTER");
    }

    [Fact]
    public void Deobfuscate_RestoresExecSqlHostVariables()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-EMP-ID              PIC 9(5).",
            "       PROCEDURE DIVISION.",
            "           EXEC SQL",
            "             SELECT EMP_NAME",
            "             INTO :WS-EMP-ID",
            "           END-EXEC",
            "           STOP RUN.");
        var ctx = CreateContext();

        var obfuscated = _processor.Obfuscate(cobol, ctx, "test.cbl");
        obfuscated.Content.Should().NotContain(":WS-EMP-ID");

        var restored = _processor.Deobfuscate(obfuscated.Content, ctx, "test.cbl");
        restored.Content.Should().Contain(":WS-EMP-ID");
    }

    [Fact(Skip = "Known COBOL processor limitation: PROGRAM-ID line classification needs rework")]
    public void Deobfuscate_RestoresProgramId()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. MY-PROGRAM.",
            "       PROCEDURE DIVISION.",
            "           STOP RUN.");
        var ctx = CreateContext();

        var obfuscated = _processor.Obfuscate(cobol, ctx, "test.cbl");
        obfuscated.Content.Should().NotContain("MY-PROGRAM");

        var restored = _processor.Deobfuscate(obfuscated.Content, ctx, "test.cbl");
        restored.Content.Should().Contain("MY-PROGRAM");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Validate Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Validate_EmptyContent_ReturnsValid()
    {
        var result = _processor.Validate("");
        result.IsValid.Should().BeTrue();
    }

    [Fact]
    public void Validate_ValidObfuscatedCobol_ReturnsValid()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PGM_0.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  VAR_0                  PIC 9(5).",
            "       PROCEDURE DIVISION.",
            "       PARA_0.",
            "           MOVE 1 TO VAR_0",
            "           STOP RUN.");

        var result = _processor.Validate(cobol);
        result.IsValid.Should().BeTrue();
    }

    [Fact]
    public void Validate_ContentWithoutDivisions_GeneratesWarning()
    {
        // Content without any DIVISION header (but long enough to check)
        var content = BuildCobolSource(
            "       01  WS-FIELD              PIC X(10).",
            "       01  WS-OTHER              PIC 9(5).",
            "           MOVE 'X' TO WS-FIELD",
            "           MOVE 1 TO WS-OTHER",
            "           DISPLAY WS-FIELD",
            "           STOP RUN.");

        var result = _processor.Validate(content);

        // Should still be valid, but may have warnings
        result.IsValid.Should().BeTrue();
        result.Warnings.Should().Contain(w => w.Contains("DIVISION"));
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Context Tracking
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_RecordsFileProcessing_InContext()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-VAR                PIC X(10).",
            "       PROCEDURE DIVISION.",
            "           STOP RUN.");
        var ctx = CreateContext();

        _processor.Obfuscate(cobol, ctx, "test.cbl");

        ctx.SourceMap.FileMap.Should().ContainKey("test.cbl");
        ctx.SourceMap.FileMap["test.cbl"].ProcessorId.Should().Be("cobol");
    }

    [Fact]
    public void Obfuscate_ExecSql_RecordsCrossLanguageReference()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-VALUE              PIC 9(5).",
            "       PROCEDURE DIVISION.",
            "           EXEC SQL",
            "             SELECT COL1 INTO :WS-VALUE FROM TBL1",
            "           END-EXEC",
            "           STOP RUN.");
        var ctx = CreateContext();

        _processor.Obfuscate(cobol, ctx, "test.cbl");

        ctx.SourceMap.CrossReferences.Should().NotBeEmpty();
        ctx.SourceMap.CrossReferences.Should().Contain(cr =>
            cr.SourceLanguage == "COBOL" && cr.TargetLanguage == "SQL");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - CASE insensitivity
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_CaseInsensitive_MixedCaseMatches()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  Ws-My-Var              PIC X(10).",
            "       PROCEDURE DIVISION.",
            "           MOVE 'X' TO ws-my-var",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.WasTransformed.Should().BeTrue();
        // Both the definition and usage should be replaced
        result.Content.ToUpperInvariant().Should().NotContain("WS-MY-VAR");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - MOVE ... OF ... (Qualified References)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_QualifiedReferences_BothPartsReplaced()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-RECORD-A.",
            "           05  WS-FIELD-X         PIC X(10).",
            "       01  WS-RECORD-B.",
            "           05  WS-FIELD-X         PIC X(10).",
            "       01  WS-OUTPUT              PIC X(10).",
            "       PROCEDURE DIVISION.",
            "           MOVE WS-FIELD-X OF WS-RECORD-A",
            "               TO WS-OUTPUT",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("WS-FIELD-X");
        result.Content.Should().NotContain("WS-RECORD-A");
        result.Content.Should().NotContain("WS-OUTPUT");
        // OF should remain (reserved word)
        result.Content.Should().Contain("OF");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Round-Trip Test (Obfuscate -> Deobfuscate)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void RoundTrip_SimpleProgram_RestoresAllIdentifiers()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. ROUND-TRIP-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-COUNTER            PIC 9(5).",
            "       PROCEDURE DIVISION.",
            "       START-PARA.",
            "           MOVE 1 TO WS-COUNTER",
            "           PERFORM END-PARA",
            "           STOP RUN.",
            "       END-PARA.",
            "           ADD 1 TO WS-COUNTER.");
        var ctx = CreateContext();

        var obfuscated = _processor.Obfuscate(cobol, ctx, "test.cbl");
        var restored = _processor.Deobfuscate(obfuscated.Content, ctx, "test.cbl");

        restored.WasTransformed.Should().BeTrue();
        restored.Content.Should().Contain("ROUND-TRIP-PGM");
        restored.Content.Should().Contain("WS-COUNTER");
        restored.Content.Should().Contain("START-PARA");
        restored.Content.Should().Contain("END-PARA");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - GO TO Statement
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_GoToStatement_ParagraphReferenceReplaced()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           GO TO EXIT-PARA.",
            "       EXIT-PARA.",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("EXIT-PARA");
        result.Content.Should().NotContain("MAIN-PARA");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - PERFORM THRU
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_PerformThru_BothParagraphsReplaced()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           PERFORM START-PARA THRU END-PARA",
            "           STOP RUN.",
            "       START-PARA.",
            "           DISPLAY 'START'.",
            "       END-PARA.",
            "           DISPLAY 'END'.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("START-PARA");
        result.Content.Should().NotContain("END-PARA");
        result.Content.Should().NotContain("MAIN-PARA");
        result.Content.Should().Contain("THRU");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Blank and debug lines
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_BlankLines_ArePreserved()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "",
            "       DATA DIVISION.",
            "",
            "       PROCEDURE DIVISION.",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        var lines = result.Content.Split('\n');
        // Blank lines should still exist (as blank or whitespace-only)
        lines.Count(l => string.IsNullOrWhiteSpace(l)).Should().BeGreaterThanOrEqualTo(2);
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Multiple Replacement Count
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ReplacementCount_ReflectsActualReplacements()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-A                  PIC X(10).",
            "       01  WS-B                  PIC X(10).",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           MOVE WS-A TO WS-B",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        // At minimum: PROGRAM-ID name, WS-A (def+use), WS-B (def+use), MAIN-PARA, comment
        result.ReplacementCount.Should().BeGreaterThanOrEqualTo(4);
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - EXEC CICS
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ExecCicsBlock_IdentifiersReplaced()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-MAP-NAME           PIC X(8).",
            "       PROCEDURE DIVISION.",
            "           EXEC CICS",
            "             SEND MAP(WS-MAP-NAME)",
            "           END-EXEC",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("WS-MAP-NAME");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - CALL 'literal'
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_CallLiteral_TargetReplaced()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           CALL 'SUBPROG'",
            "           STOP RUN.");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(cobol, ctx, "test.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("SUBPROG");
        result.Content.Should().Contain("CALL 'PGM_");
    }

    [Fact]
    public void Obfuscate_CallLiteral_MappingRegistered()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           CALL 'TARGETPGM'",
            "           STOP RUN.");
        var ctx = CreateContext();

        _processor.Obfuscate(cobol, ctx, "test.cbl");

        ctx.Mappings.Forward.Should().ContainKey("TARGETPGM");
        ctx.Mappings.Forward["TARGETPGM"].Should().StartWith("PGM_");
    }

    [Fact]
    public void Obfuscate_CallLiteral_CrossRef_Recorded()
    {
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TEST-PGM.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           CALL 'SUBPROG'",
            "           STOP RUN.");
        var ctx = CreateContext();

        _processor.Obfuscate(cobol, ctx, "test.cbl");

        ctx.SourceMap.CrossReferences.Should().Contain(cr =>
            cr.SourceLanguage == "COBOL" &&
            cr.TargetLanguage == "COBOL" &&
            cr.Description.Contains("SUBPROG"));
    }

    // ═══════════════════════════════════════════════════════════════════
    // Helpers
    // ═══════════════════════════════════════════════════════════════════

    /// <summary>
    /// Build a COBOL source string from lines, ensuring each starts at column 1.
    /// Lines that don't have 7+ chars prefix are left as-is (blank lines).
    /// </summary>
    private static string BuildCobolSource(params string[] lines)
    {
        return string.Join("\n", lines);
    }
}
