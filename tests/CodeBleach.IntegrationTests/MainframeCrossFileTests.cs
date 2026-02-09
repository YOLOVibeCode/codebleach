using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using CodeBleach.Core.Services;
using CodeBleach.Processors.Cobol;
using CodeBleach.Processors.Jcl;
using CodeBleach.Processors.Sql;
using FluentAssertions;
using Xunit;

namespace CodeBleach.IntegrationTests;

/// <summary>
/// Tests mainframe cross-file asset handling: JCL ↔ COBOL ↔ Copybooks.
/// Verifies that when a mainframe bundle (JCL cards, COBOL programs, copybooks)
/// is obfuscated, all cross-file references remain consistent and the LLM
/// can understand the relationships between assets.
/// </summary>
public class MainframeCrossFileTests
{
    private static ObfuscationContext CreateContextWithRegistry()
    {
        var context = new ObfuscationContext(ObfuscationLevel.Full);
        var registry = new LanguageProcessorRegistry();
        registry.Register(new SqlLanguageProcessor());
        registry.Register(new CobolLanguageProcessor());
        registry.Register(new JclLanguageProcessor());
        context.ProcessorRegistry = registry;
        return context;
    }

    private static string BuildCobolSource(params string[] lines)
        => string.Join("\n", lines);

    // ═══════════════════════════════════════════════════════════════════
    // CALL 'literal' Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void CallLiteral_DiscoveredAndReplaced()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. MAINPROG.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           CALL 'SUBPROG'",
            "           STOP RUN.");
        var result = processor.Obfuscate(cobol, context, "mainprog.cbl");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("SUBPROG");
        result.Content.Should().Contain("CALL 'PGM_");
        context.Mappings.Forward.Should().ContainKey("SUBPROG");
        context.Mappings.Forward["SUBPROG"].Should().StartWith("PGM_");
    }

    [Fact]
    public void CallLiteral_ConsistentWithProgramId()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        // Main program calls SUBPROG
        var main = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. MAINPROG.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           CALL 'SUBPROG'",
            "           STOP RUN.");
        processor.Obfuscate(main, context, "mainprog.cbl");
        var callAlias = context.Mappings.Forward["SUBPROG"];

        // Sub program declares itself as SUBPROG
        var sub = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. SUBPROG.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       SUB-PARA.",
            "           DISPLAY 'HELLO'",
            "           STOP RUN.");
        var subResult = processor.Obfuscate(sub, context, "subprog.cbl");

        // CALL target alias should match PROGRAM-ID alias
        context.Mappings.Forward["SUBPROG"].Should().Be(callAlias);
        subResult.Content.Should().Contain($"PROGRAM-ID. {callAlias}");
    }

    [Fact]
    public void CallLiteral_RoundTrip_Restores()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();
        var restorer = new Restorer();

        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. CALLER.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       DO-CALL.",
            "           CALL 'TARGETPGM'",
            "           STOP RUN.");
        var obfuscated = processor.Obfuscate(cobol, context, "caller.cbl");
        var restored = restorer.Restore(obfuscated.Content, context.Mappings);

        restored.Content.Should().Contain("TARGETPGM");
        restored.Content.Should().Contain("CALL 'TARGETPGM'");
    }

    [Fact]
    public void CallLiteral_CrossRef_Recorded()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. MAINPROG.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           CALL 'SUBPROG'",
            "           STOP RUN.");
        processor.Obfuscate(cobol, context, "mainprog.cbl");

        context.SourceMap.CrossReferences.Should().Contain(cr =>
            cr.SourceLanguage == "COBOL" &&
            cr.TargetLanguage == "COBOL" &&
            cr.Description.Contains("SUBPROG") &&
            cr.SourceFile == "mainprog.cbl");
    }

    // ═══════════════════════════════════════════════════════════════════
    // JCL PGM= Cross-Reference Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void JclPgm_CrossRef_RecordedAsJclToCobol()
    {
        var context = CreateContextWithRegistry();
        var processor = new JclLanguageProcessor();

        var jcl = "//RUNJOB   JOB (ACCT),'RUN',CLASS=A\n//STEP01   EXEC PGM=PAYROLL\n//SYSPRINT DD SYSOUT=*\n//";
        processor.Obfuscate(jcl, context, "runjob.jcl");

        context.SourceMap.CrossReferences.Should().Contain(cr =>
            cr.SourceLanguage == "JCL" &&
            cr.TargetLanguage == "COBOL" &&
            cr.Description.Contains("PAYROLL") &&
            cr.SourceFile == "runjob.jcl");
    }

    [Fact]
    public void JclPgm_ConsistentWithCobolProgramId()
    {
        var context = CreateContextWithRegistry();
        var jclProcessor = new JclLanguageProcessor();
        var cobolProcessor = new CobolLanguageProcessor();

        // JCL references PGM=PAYROLL
        var jcl = "//RUNJOB   JOB (ACCT),'RUN',CLASS=A\n//STEP01   EXEC PGM=PAYROLL\n//SYSPRINT DD SYSOUT=*\n//";
        var jclResult = jclProcessor.Obfuscate(jcl, context, "runjob.jcl");
        var jclAlias = context.Mappings.Forward["PAYROLL"];

        // COBOL program declares PROGRAM-ID. PAYROLL
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYROLL.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           DISPLAY 'PAYROLL RUN'",
            "           STOP RUN.");
        var cobolResult = cobolProcessor.Obfuscate(cobol, context, "payroll.cbl");

        // Same alias used in both JCL and COBOL
        context.Mappings.Forward["PAYROLL"].Should().Be(jclAlias);
        jclAlias.Should().StartWith("PGM_");
        jclResult.Content.Should().Contain($"PGM={jclAlias}");
        cobolResult.Content.Should().Contain($"PROGRAM-ID. {jclAlias}");
    }

    // ═══════════════════════════════════════════════════════════════════
    // COPY Statement Cross-Reference Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void CopyStatement_CrossRef_RecordedAsCobolToCopybook()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. MAINPGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       COPY CUSTDATA.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           STOP RUN.");
        processor.Obfuscate(cobol, context, "mainpgm.cbl");

        context.SourceMap.CrossReferences.Should().Contain(cr =>
            cr.SourceLanguage == "COBOL" &&
            cr.TargetLanguage == "Copybook" &&
            cr.Description.Contains("CUSTDATA") &&
            cr.SourceFile == "mainpgm.cbl");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Copybook File Self-Registration Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void CopybookFile_SelfRegistration_StemAsCpyAlias()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        // Process a copybook file (partial COBOL — no IDENTIFICATION DIVISION)
        var copybook = BuildCobolSource(
            "       01  WS-CUSTOMER-ID         PIC 9(8).",
            "       01  WS-CUSTOMER-NAME       PIC X(40).",
            "       01  WS-CUSTOMER-BALANCE    PIC 9(9)V99.");
        processor.Obfuscate(copybook, context, "CUSTDATA.cpy");

        // File stem should be registered as a Copybook
        context.Mappings.Forward.Should().ContainKey("CUSTDATA");
        context.Mappings.Forward["CUSTDATA"].Should().StartWith("CPY_");
    }

    [Fact]
    public void CopybookFile_NameCorrelation_MatchesCopyAlias()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        // First: COBOL program with COPY CUSTDATA
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. MAINPGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       COPY CUSTDATA.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           STOP RUN.");
        processor.Obfuscate(cobol, context, "mainpgm.cbl");
        var copyAlias = context.Mappings.Forward["CUSTDATA"];

        // Then: process the copybook file itself
        var copybook = BuildCobolSource(
            "       01  WS-CUSTOMER-ID         PIC 9(8).",
            "       01  WS-CUSTOMER-NAME       PIC X(40).");
        processor.Obfuscate(copybook, context, "CUSTDATA.cpy");

        // The copybook stem should have the same alias as the COPY reference
        context.Mappings.Forward["CUSTDATA"].Should().Be(copyAlias);
        copyAlias.Should().StartWith("CPY_");

        // FileNameMapper should be able to correlate: verify the SourceMap has
        // a Copybook entry with a Location pointing to CUSTDATA.cpy
        context.SourceMap.Entries.Should().ContainKey("CUSTDATA");
        var entry = context.SourceMap.Entries["CUSTDATA"];
        entry.Category.Should().Be(SemanticCategory.Copybook);
        entry.Locations.Should().Contain(loc => loc.FilePath == "CUSTDATA.cpy");
    }

    [Fact]
    public void CopybookProcessedFirst_ThenCobolCopy_SameAlias()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        // Process copybook FIRST (order independence test)
        var copybook = BuildCobolSource(
            "       01  WS-CUSTOMER-ID         PIC 9(8).",
            "       01  WS-CUSTOMER-NAME       PIC X(40).");
        processor.Obfuscate(copybook, context, "CUSTDATA.cpy");
        var copybookAlias = context.Mappings.Forward["CUSTDATA"];

        // Then process COBOL program that references it
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. MAINPGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       COPY CUSTDATA.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           STOP RUN.");
        var result = processor.Obfuscate(cobol, context, "mainpgm.cbl");

        // Same alias regardless of processing order
        context.Mappings.Forward["CUSTDATA"].Should().Be(copybookAlias);
        copybookAlias.Should().StartWith("CPY_");
        result.Content.Should().Contain($"COPY {copybookAlias}");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Full Mainframe Bundle Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void MainframeBundle_JclCobolCopybook_AllCorrelated()
    {
        var context = CreateContextWithRegistry();
        var jclProcessor = new JclLanguageProcessor();
        var cobolProcessor = new CobolLanguageProcessor();

        // JCL: runs PAYROLL program
        var jcl = "//PAYJOB   JOB (FIN01),'PAY RUN',CLASS=A\n//PAYSTEP  EXEC PGM=PAYROLL\n//SYSPRINT DD SYSOUT=*\n//";
        jclProcessor.Obfuscate(jcl, context, "payjob.jcl");
        var payrollAlias = context.Mappings.Forward["PAYROLL"];

        // COBOL: PAYROLL program, COPY EMPDATA, CALL TAXCALC
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYROLL.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       COPY EMPDATA.",
            "       01  WS-GROSS-PAY           PIC 9(9)V99.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           CALL 'TAXCALC'",
            "           STOP RUN.");
        var cobolResult = cobolProcessor.Obfuscate(cobol, context, "payroll.cbl");

        // Copybook: EMPDATA
        var copybook = BuildCobolSource(
            "       01  WS-EMP-ID              PIC 9(8).",
            "       01  WS-EMP-NAME            PIC X(40).",
            "       01  WS-EMP-SALARY          PIC 9(9)V99.");
        cobolProcessor.Obfuscate(copybook, context, "EMPDATA.cpy");

        // Sub-program: TAXCALC
        var subprog = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. TAXCALC.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       CALC-TAX.",
            "           DISPLAY 'TAX'",
            "           STOP RUN.");
        var subResult = cobolProcessor.Obfuscate(subprog, context, "taxcalc.cbl");

        // All cross-file references should be consistent
        // JCL PGM=PAYROLL → COBOL PROGRAM-ID. PAYROLL → same alias
        payrollAlias.Should().StartWith("PGM_");
        cobolResult.Content.Should().Contain($"PROGRAM-ID. {payrollAlias}");

        // COBOL CALL 'TAXCALC' → COBOL PROGRAM-ID. TAXCALC → same alias
        var taxcalcAlias = context.Mappings.Forward["TAXCALC"];
        taxcalcAlias.Should().StartWith("PGM_");
        cobolResult.Content.Should().Contain($"CALL '{taxcalcAlias}'");
        subResult.Content.Should().Contain($"PROGRAM-ID. {taxcalcAlias}");

        // COBOL COPY EMPDATA → copybook file EMPDATA.cpy → same alias
        var empdataAlias = context.Mappings.Forward["EMPDATA"];
        empdataAlias.Should().StartWith("CPY_");
        cobolResult.Content.Should().Contain($"COPY {empdataAlias}");
    }

    [Fact]
    public void MainframeBundle_ZeroFingerprint()
    {
        var context = CreateContextWithRegistry();
        var jclProcessor = new JclLanguageProcessor();
        var cobolProcessor = new CobolLanguageProcessor();

        // JCL
        var jcl = "//ACCTJOB  JOB (ACCT01),'ACCOUNTS',CLASS=A\n//RUNSTEP  EXEC PGM=ACCTMAIN\n//ACCTFILE DD DSN=PROD.ACCOUNTS.MASTER,DISP=SHR\n//";
        var jclResult = jclProcessor.Obfuscate(jcl, context, "acctjob.jcl");

        // Main COBOL program
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. ACCTMAIN.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       COPY ACCTCOPY.",
            "       01  WS-ACCOUNT-ID          PIC 9(10).",
            "       PROCEDURE DIVISION.",
            "       PROCESS-ACCT.",
            "           CALL 'ACCTRPT'",
            "           STOP RUN.");
        var cobolResult = cobolProcessor.Obfuscate(cobol, context, "acctmain.cbl");

        // Copybook
        var copybook = BuildCobolSource(
            "       01  WS-ACCT-NUMBER         PIC 9(10).",
            "       01  WS-ACCT-NAME           PIC X(30).",
            "       01  WS-ACCT-BALANCE        PIC 9(9)V99.");
        var cpyResult = cobolProcessor.Obfuscate(copybook, context, "ACCTCOPY.cpy");

        // Sub-program
        var subprog = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. ACCTRPT.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       REPORT-PARA.",
            "           DISPLAY 'REPORT'",
            "           STOP RUN.");
        var subResult = cobolProcessor.Obfuscate(subprog, context, "acctrpt.cbl");

        // Zero fingerprint: no business identifiers in any output
        var businessTerms = new[] { "ACCTJOB", "ACCTMAIN", "ACCTCOPY", "ACCTRPT", "ACCTFILE",
            "PROCESS-ACCT", "REPORT-PARA" };
        foreach (var term in businessTerms)
        {
            jclResult.Content.Should().NotContain(term, because: $"JCL output should not contain '{term}'");
            cobolResult.Content.Should().NotContain(term, because: $"COBOL output should not contain '{term}'");
            cpyResult.Content.Should().NotContain(term, because: $"Copybook output should not contain '{term}'");
            subResult.Content.Should().NotContain(term, because: $"Sub-program output should not contain '{term}'");
        }
    }

    [Fact]
    public void MainframeBundle_RoundTrip_AllRestore()
    {
        var context = CreateContextWithRegistry();
        var jclProcessor = new JclLanguageProcessor();
        var cobolProcessor = new CobolLanguageProcessor();
        var restorer = new Restorer();

        // JCL
        var jcl = "//RUNJOB   JOB (ACCT),'RUN',CLASS=A\n//STEP01   EXEC PGM=BATCHPGM\n//SYSPRINT DD SYSOUT=*\n//";
        var jclObf = jclProcessor.Obfuscate(jcl, context, "runjob.jcl");

        // COBOL
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. BATCHPGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       COPY BATCHCPY.",
            "       01  WS-BATCH-ID            PIC 9(8).",
            "       PROCEDURE DIVISION.",
            "       BATCH-PARA.",
            "           CALL 'HELPPGM'",
            "           STOP RUN.");
        var cobolObf = cobolProcessor.Obfuscate(cobol, context, "batchpgm.cbl");

        // Copybook
        var copybook = BuildCobolSource(
            "       01  WS-BATCH-COUNT         PIC 9(6).",
            "       01  WS-BATCH-STATUS        PIC X(2).");
        var cpyObf = cobolProcessor.Obfuscate(copybook, context, "BATCHCPY.cpy");

        // Restore all
        var jclRestored = restorer.Restore(jclObf.Content, context.Mappings);
        var cobolRestored = restorer.Restore(cobolObf.Content, context.Mappings);
        var cpyRestored = restorer.Restore(cpyObf.Content, context.Mappings);

        // Key identifiers restored
        jclRestored.Content.Should().Contain("BATCHPGM");
        jclRestored.Content.Should().Contain("RUNJOB");
        cobolRestored.Content.Should().Contain("BATCHPGM");
        cobolRestored.Content.Should().Contain("BATCHCPY");
        cobolRestored.Content.Should().Contain("HELPPGM");
        cobolRestored.Content.Should().Contain("CALL 'HELPPGM'");
        cpyRestored.Content.Should().Contain("WS-BATCH-COUNT");
    }

    [Fact]
    public void MainframeBundle_CrossRefs_AllRecorded()
    {
        var context = CreateContextWithRegistry();
        var jclProcessor = new JclLanguageProcessor();
        var cobolProcessor = new CobolLanguageProcessor();

        // JCL with PGM=MAINPROG
        var jcl = "//RUNJOB   JOB (ACCT),'RUN',CLASS=A\n//STEP01   EXEC PGM=MAINPROG\n//SYSPRINT DD SYSOUT=*\n//";
        jclProcessor.Obfuscate(jcl, context, "runjob.jcl");

        // COBOL with COPY and CALL
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. MAINPROG.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       COPY SHAREDCPY.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           CALL 'HELPER'",
            "           STOP RUN.");
        cobolProcessor.Obfuscate(cobol, context, "mainprog.cbl");

        // Should have all 3 types of cross-references
        context.SourceMap.CrossReferences.Should().Contain(cr =>
            cr.SourceLanguage == "JCL" && cr.TargetLanguage == "COBOL",
            because: "JCL PGM= should record a JCL→COBOL cross-reference");

        context.SourceMap.CrossReferences.Should().Contain(cr =>
            cr.SourceLanguage == "COBOL" && cr.TargetLanguage == "Copybook",
            because: "COBOL COPY should record a COBOL→Copybook cross-reference");

        context.SourceMap.CrossReferences.Should().Contain(cr =>
            cr.SourceLanguage == "COBOL" && cr.TargetLanguage == "COBOL",
            because: "COBOL CALL should record a COBOL→COBOL cross-reference");
    }

    [Fact]
    public void MultiplePrograms_CallChain_ConsistentAliases()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        // Program A calls B
        var progA = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PROG-A.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           CALL 'PROG-B'",
            "           STOP RUN.");
        processor.Obfuscate(progA, context, "proga.cbl");

        // Program B calls C
        var progB = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PROG-B.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       PROCESS-PARA.",
            "           CALL 'PROG-C'",
            "           STOP RUN.");
        var resultB = processor.Obfuscate(progB, context, "progb.cbl");

        // Program C (leaf)
        var progC = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PROG-C.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       LEAF-PARA.",
            "           DISPLAY 'DONE'",
            "           STOP RUN.");
        var resultC = processor.Obfuscate(progC, context, "progc.cbl");

        // All program names should have PGM_ aliases
        var aliasA = context.Mappings.Forward["PROG-A"];
        var aliasB = context.Mappings.Forward["PROG-B"];
        var aliasC = context.Mappings.Forward["PROG-C"];

        aliasA.Should().StartWith("PGM_");
        aliasB.Should().StartWith("PGM_");
        aliasC.Should().StartWith("PGM_");

        // All aliases should be distinct
        new[] { aliasA, aliasB, aliasC }.Distinct().Should().HaveCount(3);

        // CALL in program A references program B's alias
        // CALL in program B references program C's alias
        // PROGRAM-ID in B and C match the aliases
        resultB.Content.Should().Contain($"PROGRAM-ID. {aliasB}");
        resultB.Content.Should().Contain($"CALL '{aliasC}'");
        resultC.Content.Should().Contain($"PROGRAM-ID. {aliasC}");
    }
}
