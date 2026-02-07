using CodeBleach.Core.Models;
using CodeBleach.Processors.Jcl;
using FluentAssertions;
using Xunit;

namespace CodeBleach.Processors.Jcl.Tests;

/// <summary>
/// Comprehensive tests for the JclLanguageProcessor.
/// Tests cover: metadata, CanProcess (extension + heuristic), obfuscation of all JCL
/// constructs (JOB, EXEC, DD, PROC, SET, DSN, PGM, comments, symbolic parameters),
/// system DD name preservation, DISP value preservation, JCL keyword preservation,
/// system symbol preservation, deobfuscation round-trips, and validation.
/// </summary>
public class JclLanguageProcessorTests
{
    private readonly JclLanguageProcessor _processor = new();

    private static ObfuscationContext CreateContext(ObfuscationLevel level = ObfuscationLevel.Full)
        => new(level);

    // ═══════════════════════════════════════════════════════════════════
    // Metadata Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void ProcessorId_Returns_Jcl()
    {
        _processor.ProcessorId.Should().Be("jcl");
    }

    [Fact]
    public void DisplayName_Returns_Jcl()
    {
        _processor.DisplayName.Should().Be("JCL");
    }

    [Theory]
    [InlineData(".jcl")]
    [InlineData(".prc")]
    public void SupportedExtensions_ContainsExpected(string ext)
    {
        _processor.SupportedExtensions.Should().Contain(ext);
    }

    // ═══════════════════════════════════════════════════════════════════
    // CanProcess Tests
    // ═══════════════════════════════════════════════════════════════════

    [Theory]
    [InlineData("test.jcl")]
    [InlineData("test.prc")]
    public void CanProcess_WithSupportedExtension_ReturnsTrue(string filePath)
    {
        _processor.CanProcess(filePath, "//JOBNAME JOB").Should().BeTrue();
    }

    [Fact]
    public void CanProcess_WithJclContent_ReturnsTrue()
    {
        var content = "//MYJOB   JOB (ACCT),'USER',CLASS=A\n//STEP1   EXEC PGM=IEFBR14\n";
        _processor.CanProcess("test.txt", content).Should().BeTrue();
    }

    [Fact]
    public void CanProcess_WithNonJclContent_ReturnsFalse()
    {
        _processor.CanProcess("test.txt", "public class Foo {}").Should().BeFalse();
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Empty / Null Input
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_EmptyContent_ReturnsUnchanged()
    {
        var ctx = CreateContext();
        var result = _processor.Obfuscate("", ctx, "test.jcl");

        result.WasTransformed.Should().BeFalse();
        result.ProcessorId.Should().Be("jcl");
    }

    [Fact]
    public void Obfuscate_NullContent_ReturnsGracefully()
    {
        var ctx = CreateContext();
        var act = () => _processor.Obfuscate(null!, ctx, "test.jcl");

        act.Should().NotThrow();
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Job Name
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_JobName_IsRenamed()
    {
        var jcl = "//PAYROLL  JOB (ACCT),'USER',CLASS=A\n//STEP1   EXEC PGM=IEFBR14\n";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(jcl, ctx, "test.jcl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("PAYROLL");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Step Name
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_StepName_IsRenamed()
    {
        var jcl = "//MYJOB    JOB (ACCT),'USER',CLASS=A\n//STEP01   EXEC PGM=IEFBR14\n";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(jcl, ctx, "test.jcl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("STEP01");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - DD Name
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_DdName_IsRenamed()
    {
        var jcl = string.Join("\n",
            "//MYJOB    JOB (ACCT),'USER',CLASS=A",
            "//STEP1    EXEC PGM=IEFBR14",
            "//INPUT01  DD  DSN=PROD.DATA,DISP=SHR");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(jcl, ctx, "test.jcl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("INPUT01");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Proc Name
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ProcName_IsRenamed()
    {
        var jcl = "//MYPROC   PROC\n//STEP1    EXEC PGM=IEFBR14\n//  PEND\n";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(jcl, ctx, "test.jcl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("MYPROC");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Dataset Name
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_DatasetName_IsRenamed()
    {
        var jcl = string.Join("\n",
            "//MYJOB    JOB (ACCT),'USER',CLASS=A",
            "//STEP1    EXEC PGM=IEFBR14",
            "//INPUT01  DD  DSN=PROD.PAYROLL.DATA,DISP=SHR");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(jcl, ctx, "test.jcl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("PROD.PAYROLL.DATA");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Symbolic Parameter
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_SymbolicParameter_IsRenamed()
    {
        var jcl = string.Join("\n",
            "//MYJOB    JOB (ACCT),'USER',CLASS=A",
            "// SET DBNAME=PRODDB01",
            "//STEP1    EXEC PGM=IEFBR14");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(jcl, ctx, "test.jcl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("DBNAME");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - System DD Names Preserved
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_SystemDdNames_ArePreserved()
    {
        var jcl = string.Join("\n",
            "//MYJOB    JOB (ACCT),'USER',CLASS=A",
            "//STEP1    EXEC PGM=IEFBR14",
            "//SYSPRINT DD  SYSOUT=*",
            "//SYSIN    DD  DUMMY",
            "//STEPLIB  DD  DSN=MY.LOADLIB,DISP=SHR");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(jcl, ctx, "test.jcl");

        result.Content.Should().Contain("SYSPRINT");
        result.Content.Should().Contain("SYSIN");
        result.Content.Should().Contain("STEPLIB");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - DISP Values Preserved
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_DispValues_ArePreserved()
    {
        var jcl = string.Join("\n",
            "//MYJOB    JOB (ACCT),'USER',CLASS=A",
            "//STEP1    EXEC PGM=IEFBR14",
            "//OUTPUT1  DD  DSN=MY.OUTPUT,DISP=(NEW,CATLG,DELETE)");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(jcl, ctx, "test.jcl");

        result.Content.Should().Contain("NEW");
        result.Content.Should().Contain("CATLG");
        result.Content.Should().Contain("DELETE");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - JCL Keywords Preserved
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_JclKeywords_ArePreserved()
    {
        var jcl = string.Join("\n",
            "//MYJOB    JOB (ACCT),'USER',CLASS=A",
            "//STEP1    EXEC PGM=IEFBR14",
            "//INPUT01  DD  DSN=MY.DATA,DISP=SHR",
            "//MYPROC   PROC");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(jcl, ctx, "test.jcl");

        result.Content.Should().Contain("JOB");
        result.Content.Should().Contain("EXEC");
        result.Content.Should().Contain("DD");
        result.Content.Should().Contain("PROC");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - System Symbols Preserved
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_SystemSymbols_ArePreserved()
    {
        var jcl = string.Join("\n",
            "//MYJOB    JOB (ACCT),'USER',CLASS=A",
            "//STEP1    EXEC PGM=IEFBR14",
            "//OUTPUT1  DD  DSN=&SYSUID..MY.DATA,DISP=SHR");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(jcl, ctx, "test.jcl");

        result.Content.Should().Contain("&SYSUID");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - JCL Comment Replaced
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_JclComment_IsReplaced()
    {
        var jcl = string.Join("\n",
            "//MYJOB    JOB (ACCT),'USER',CLASS=A",
            "//*  SECRET COMMENT",
            "//STEP1    EXEC PGM=IEFBR14");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(jcl, ctx, "test.jcl");

        result.Content.Should().NotContain("SECRET COMMENT");
        result.Content.Should().Contain("[Comment removed]");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - PGM Name Renamed
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_PgmName_IsRenamed()
    {
        var jcl = string.Join("\n",
            "//MYJOB    JOB (ACCT),'USER',CLASS=A",
            "//STEP1    EXEC PGM=MYAPP01");
        var ctx = CreateContext();

        var result = _processor.Obfuscate(jcl, ctx, "test.jcl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("MYAPP01");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Round-Trip Test (Obfuscate -> Deobfuscate)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void RoundTrip_SimpleJob_RestoresAll()
    {
        var jcl = string.Join("\n",
            "//PAYROLL  JOB (ACCT),'ADMIN',CLASS=A",
            "//STEP01   EXEC PGM=MYAPP01",
            "//INPUT01  DD  DSN=PROD.PAYROLL.DATA,DISP=SHR",
            "//OUTPUT01 DD  DSN=PROD.PAYROLL.OUT,DISP=(NEW,CATLG,DELETE)");
        var ctx = CreateContext();

        var obfuscated = _processor.Obfuscate(jcl, ctx, "test.jcl");
        obfuscated.WasTransformed.Should().BeTrue();

        // Verify originals are gone after obfuscation
        obfuscated.Content.Should().NotContain("PAYROLL");
        obfuscated.Content.Should().NotContain("STEP01");
        obfuscated.Content.Should().NotContain("INPUT01");
        obfuscated.Content.Should().NotContain("OUTPUT01");
        obfuscated.Content.Should().NotContain("MYAPP01");

        // Deobfuscate and verify originals are restored
        var restored = _processor.Deobfuscate(obfuscated.Content, ctx, "test.jcl");
        restored.WasTransformed.Should().BeTrue();
        restored.Content.Should().Contain("PAYROLL");
        restored.Content.Should().Contain("STEP01");
        restored.Content.Should().Contain("INPUT01");
        restored.Content.Should().Contain("OUTPUT01");
        restored.Content.Should().Contain("MYAPP01");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Validate Test
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Validate_ObfuscatedJcl_ReturnsValid()
    {
        var jcl = string.Join("\n",
            "//MYJOB    JOB (ACCT),'USER',CLASS=A",
            "//STEP1    EXEC PGM=IEFBR14",
            "//INPUT01  DD  DSN=MY.DATA,DISP=SHR",
            "//SYSPRINT DD  SYSOUT=*");
        var ctx = CreateContext();

        var obfuscated = _processor.Obfuscate(jcl, ctx, "test.jcl");
        var validation = _processor.Validate(obfuscated.Content);

        validation.IsValid.Should().BeTrue();
    }
}
