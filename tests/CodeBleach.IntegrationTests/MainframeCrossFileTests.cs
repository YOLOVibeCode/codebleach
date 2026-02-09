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

    // ═══════════════════════════════════════════════════════════════════
    // Extensionless Mainframe File Tests
    // ═══════════════════════════════════════════════════════════════════

    private static LanguageProcessorRegistry CreateRegistry()
    {
        var registry = new LanguageProcessorRegistry();
        registry.Register(new SqlLanguageProcessor());
        registry.Register(new CobolLanguageProcessor());
        registry.Register(new JclLanguageProcessor());
        return registry;
    }

    [Fact]
    public void ExtensionlessCobol_RegistryReturnsProcessor()
    {
        var registry = CreateRegistry();
        var cobolContent = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYROLL.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           STOP RUN.");
        var processor = registry.GetProcessor("PAYROLL", cobolContent);

        processor.Should().NotBeNull();
        processor!.ProcessorId.Should().Be("cobol");
    }

    [Fact]
    public void ExtensionlessJcl_RegistryReturnsProcessor()
    {
        var registry = CreateRegistry();
        var jclContent = "//RUNJOB   JOB (ACCT),'RUN',CLASS=A\n//STEP01   EXEC PGM=MYPROG\n//SYSPRINT DD SYSOUT=*\n//";
        var processor = registry.GetProcessor("RUNJOB", jclContent);

        processor.Should().NotBeNull();
        processor!.ProcessorId.Should().Be("jcl");
    }

    [Fact]
    public void ExtensionlessCopybook_DetectedByPicHeuristic()
    {
        var registry = CreateRegistry();
        var copybookContent = BuildCobolSource(
            "       01  WS-CUSTOMER-ID         PIC 9(8).",
            "       01  WS-CUSTOMER-NAME       PIC X(40).",
            "       01  WS-CUSTOMER-BALANCE    PIC 9(9)V99.");
        var processor = registry.GetProcessor("CUSTDATA", copybookContent);

        processor.Should().NotBeNull();
        processor!.ProcessorId.Should().Be("cobol");
    }

    [Fact]
    public void ExtensionlessCobol_DetectedAndProcessed()
    {
        var context = CreateContextWithRegistry();
        var registry = context.ProcessorRegistry!;

        var cobolContent = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYROLL.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-AMOUNT              PIC 9(9)V99.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           STOP RUN.");

        var processor = registry.GetProcessor("PAYROLL", cobolContent);
        processor.Should().NotBeNull();

        var result = processor!.Obfuscate(cobolContent, context, "PAYROLL");
        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("PAYROLL");
        result.Content.Should().NotContain("WS-AMOUNT");
        context.Mappings.Forward.Should().ContainKey("PAYROLL");
        context.Mappings.Forward["PAYROLL"].Should().StartWith("PGM_");
    }

    [Fact]
    public void ExtensionlessJcl_DetectedAndProcessed()
    {
        var context = CreateContextWithRegistry();
        var registry = context.ProcessorRegistry!;

        var jclContent = "//RUNJOB   JOB (ACCT),'RUN',CLASS=A\n//STEP01   EXEC PGM=BATCHPGM\n//SYSPRINT DD SYSOUT=*\n//";

        var processor = registry.GetProcessor("RUNJOB", jclContent);
        processor.Should().NotBeNull();

        var result = processor!.Obfuscate(jclContent, context, "RUNJOB");
        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("RUNJOB");
        result.Content.Should().NotContain("BATCHPGM");
    }

    [Fact]
    public void ExtensionlessBundle_CrossFileCorrelation()
    {
        var context = CreateContextWithRegistry();
        var registry = context.ProcessorRegistry!;

        // JCL (no extension)
        var jclContent = "//RUNJOB   JOB (ACCT),'RUN',CLASS=A\n//STEP01   EXEC PGM=PAYROLL\n//SYSPRINT DD SYSOUT=*\n//";
        var jclProcessor = registry.GetProcessor("RUNJOB", jclContent);
        jclProcessor.Should().NotBeNull();
        jclProcessor!.Obfuscate(jclContent, context, "RUNJOB");
        var payrollAlias = context.Mappings.Forward["PAYROLL"];

        // COBOL (no extension)
        var cobolContent = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYROLL.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       COPY EMPDATA.",
            "       01  WS-AMOUNT              PIC 9(9)V99.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           CALL 'TAXCALC'",
            "           STOP RUN.");
        var cobolProcessor = registry.GetProcessor("PAYROLL", cobolContent);
        cobolProcessor.Should().NotBeNull();
        var cobolResult = cobolProcessor!.Obfuscate(cobolContent, context, "PAYROLL");

        // Copybook (no extension, no DIVISION headers, just PIC clauses)
        var cpyContent = BuildCobolSource(
            "       01  WS-EMP-ID              PIC 9(8).",
            "       01  WS-EMP-NAME            PIC X(40).");
        var cpyProcessor = registry.GetProcessor("EMPDATA", cpyContent);
        cpyProcessor.Should().NotBeNull();
        cpyProcessor!.Obfuscate(cpyContent, context, "EMPDATA");

        // Cross-file correlation works without extensions
        payrollAlias.Should().StartWith("PGM_");
        cobolResult.Content.Should().Contain($"PROGRAM-ID. {payrollAlias}");

        var taxcalcAlias = context.Mappings.Forward["TAXCALC"];
        taxcalcAlias.Should().StartWith("PGM_");
        cobolResult.Content.Should().Contain($"CALL '{taxcalcAlias}'");

        var empdataAlias = context.Mappings.Forward["EMPDATA"];
        empdataAlias.Should().StartWith("CPY_");
        cobolResult.Content.Should().Contain($"COPY {empdataAlias}");
    }

    [Fact]
    public void ExtensionlessFiles_FileNameMapper_StrategyA()
    {
        var context = CreateContextWithRegistry();
        var cobolProcessor = new CobolLanguageProcessor();

        // Process extensionless COBOL file
        var cobolContent = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYROLL.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           STOP RUN.");
        cobolProcessor.Obfuscate(cobolContent, context, "PAYROLL");
        var payrollAlias = context.Mappings.Forward["PAYROLL"];

        // FileNameMapper should derive alias from primary type
        var mapper = new FileNameMapper();
        mapper.BuildMappings(context, ["PAYROLL"]);

        context.Mappings.FilePathForward.Should().ContainKey("PAYROLL");
        var mappedPath = context.Mappings.FilePathForward["PAYROLL"];
        mappedPath.Should().Be(payrollAlias, because: "extensionless COBOL file should get PGM_N name from Strategy A");
    }

    [Fact]
    public void ExtensionlessNonMainframe_FallsToNull()
    {
        var registry = CreateRegistry();
        var plainText = "This is just a plain text file\nwith no COBOL or JCL markers.";
        var processor = registry.GetProcessor("README", plainText);

        processor.Should().BeNull(because: "non-mainframe extensionless files should not match any processor");
    }

    // ═══════════════════════════════════════════════════════════════════
    // FileNameMapper with Mainframe Processor Output
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void FileNameMapper_CobolFile_GetsPgmAlias()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYROLL.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           STOP RUN.");
        processor.Obfuscate(cobol, context, "payroll.cbl");

        var mapper = new FileNameMapper();
        mapper.BuildMappings(context, ["payroll.cbl"]);

        context.Mappings.FilePathForward.Should().ContainKey("payroll.cbl");
        context.Mappings.FilePathForward["payroll.cbl"].Should().MatchRegex(@"^PGM_\d+\.cbl$");
    }

    [Fact]
    public void FileNameMapper_JclFile_GetsJobAlias()
    {
        var context = CreateContextWithRegistry();
        var jclProcessor = new JclLanguageProcessor();

        var jcl = "//PAYJOB   JOB (FIN01),'PAY RUN',CLASS=A\n//PAYSTEP  EXEC PGM=MYPROG\n//SYSPRINT DD SYSOUT=*\n//";
        jclProcessor.Obfuscate(jcl, context, "payjob.jcl");

        var mapper = new FileNameMapper();
        mapper.BuildMappings(context, ["payjob.jcl"]);

        context.Mappings.FilePathForward.Should().ContainKey("payjob.jcl");
        context.Mappings.FilePathForward["payjob.jcl"].Should().MatchRegex(@"^JOB_\d+\.jcl$");
    }

    [Fact]
    public void FileNameMapper_CopybookFile_GetsCpyAlias()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        var copybook = BuildCobolSource(
            "       01  WS-CUSTOMER-ID         PIC 9(8).",
            "       01  WS-CUSTOMER-NAME       PIC X(40).");
        processor.Obfuscate(copybook, context, "CUSTDATA.cpy");

        var mapper = new FileNameMapper();
        mapper.BuildMappings(context, ["CUSTDATA.cpy"]);

        context.Mappings.FilePathForward.Should().ContainKey("CUSTDATA.cpy");
        context.Mappings.FilePathForward["CUSTDATA.cpy"].Should().MatchRegex(@"^CPY_\d+\.cpy$");
    }

    [Fact]
    public void FileNameMapper_ExtensionlessCopybook_GetsFileAlias()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        // Extensionless copybook — self-registration only fires for .cpy files,
        // so the COPY statement in mainpgm.cbl registers the alias with a location
        // pointing to mainpgm.cbl, not to the extensionless copybook file.
        // Without a SourceMap location pointing to "CUSTDATA", FindPrimaryTypeAlias
        // won't match, and the file falls back to FILE_N.
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

        // Process the extensionless copybook file
        var copybook = BuildCobolSource(
            "       01  WS-CUSTOMER-ID         PIC 9(8).",
            "       01  WS-CUSTOMER-NAME       PIC X(40).");
        processor.Obfuscate(copybook, context, "CUSTDATA");

        var mapper = new FileNameMapper();
        mapper.BuildMappings(context, ["mainpgm.cbl", "CUSTDATA"]);

        context.Mappings.FilePathForward.Should().ContainKey("CUSTDATA");
        // With LooksLikeCopybook self-registration, extensionless copybooks now get CPY_N
        context.Mappings.FilePathForward["CUSTDATA"].Should().MatchRegex(@"^CPY_\d+$");

        // The copybook identifier is also registered with CPY_ prefix
        context.Mappings.Forward.Should().ContainKey("CUSTDATA");
        context.Mappings.Forward["CUSTDATA"].Should().StartWith("CPY_");
    }

    [Fact]
    public void FileNameMapper_ExtensionlessJcl_GetsJobAlias()
    {
        var context = CreateContextWithRegistry();
        var jclProcessor = new JclLanguageProcessor();

        var jcl = "//RUNJOB   JOB (ACCT),'RUN',CLASS=A\n//STEP01   EXEC PGM=MYPROG\n//SYSPRINT DD SYSOUT=*\n//";
        jclProcessor.Obfuscate(jcl, context, "RUNJOB");

        var mapper = new FileNameMapper();
        mapper.BuildMappings(context, ["RUNJOB"]);

        context.Mappings.FilePathForward.Should().ContainKey("RUNJOB");
        context.Mappings.FilePathForward["RUNJOB"].Should().MatchRegex(@"^JOB_\d+$");
    }

    [Fact]
    public void FileNameMapper_MainframeBundle_AllRenamed_Bidirectional()
    {
        var context = CreateContextWithRegistry();
        var jclProcessor = new JclLanguageProcessor();
        var cobolProcessor = new CobolLanguageProcessor();

        // Process all files
        var jcl = "//RUNJOB   JOB (ACCT),'RUN',CLASS=A\n//STEP01   EXEC PGM=PAYROLL\n//SYSPRINT DD SYSOUT=*\n//";
        jclProcessor.Obfuscate(jcl, context, "runjob.jcl");

        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYROLL.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       COPY EMPDATA.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           STOP RUN.");
        cobolProcessor.Obfuscate(cobol, context, "payroll.cbl");

        var copybook = BuildCobolSource(
            "       01  WS-EMP-ID              PIC 9(8).",
            "       01  WS-EMP-NAME            PIC X(40).");
        cobolProcessor.Obfuscate(copybook, context, "EMPDATA.cpy");

        // Build file name mappings
        var mapper = new FileNameMapper();
        var files = new List<string> { "runjob.jcl", "payroll.cbl", "EMPDATA.cpy" };
        mapper.BuildMappings(context, files);

        // All files should have Strategy A aliases
        context.Mappings.FilePathForward["runjob.jcl"].Should().MatchRegex(@"^JOB_\d+\.jcl$");
        context.Mappings.FilePathForward["payroll.cbl"].Should().MatchRegex(@"^PGM_\d+\.cbl$");
        context.Mappings.FilePathForward["EMPDATA.cpy"].Should().MatchRegex(@"^CPY_\d+\.cpy$");

        // Bidirectional consistency
        foreach (var (original, obfuscated) in context.Mappings.FilePathForward)
        {
            context.Mappings.FilePathReverse.Should().ContainKey(obfuscated);
            context.Mappings.FilePathReverse[obfuscated].Should().Be(original);
        }
    }

    // ═══════════════════════════════════════════════════════════════════
    // JCL DSN ↔ COBOL Correlation Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void DsnMember_CorrelatesWithCobolProgramId()
    {
        var context = CreateContextWithRegistry();
        var jclProcessor = new JclLanguageProcessor();
        var cobolProcessor = new CobolLanguageProcessor();

        // JCL with partitioned dataset member reference
        var jcl = "//RUNJOB   JOB (ACCT),'RUN',CLASS=A\n//STEP01   EXEC PGM=BATCHPGM\n//STEPLIB  DD DSN=PROD.LOADLIB(PAYROLL),DISP=SHR\n//SYSPRINT DD SYSOUT=*\n//";
        jclProcessor.Obfuscate(jcl, context, "runjob.jcl");

        // The member name PAYROLL should be registered as a Program
        context.Mappings.Forward.Should().ContainKey("PAYROLL");
        var dsnMemberAlias = context.Mappings.Forward["PAYROLL"];
        dsnMemberAlias.Should().StartWith("PGM_");

        // COBOL program with matching PROGRAM-ID
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYROLL.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           STOP RUN.");
        var cobolResult = cobolProcessor.Obfuscate(cobol, context, "payroll.cbl");

        // Same alias for DSN member and PROGRAM-ID
        context.Mappings.Forward["PAYROLL"].Should().Be(dsnMemberAlias);
        cobolResult.Content.Should().Contain($"PROGRAM-ID. {dsnMemberAlias}");
    }

    [Fact]
    public void DsnDataset_ObfuscatedAndRestorable()
    {
        var context = CreateContextWithRegistry();
        var jclProcessor = new JclLanguageProcessor();
        var restorer = new Restorer();

        var jcl = "//RUNJOB   JOB (ACCT),'RUN',CLASS=A\n//STEP01   EXEC PGM=MYPROG\n//INFILE   DD DSN=PROD.PAYROLL.DATA,DISP=SHR\n//SYSPRINT DD SYSOUT=*\n//";
        var obfuscated = jclProcessor.Obfuscate(jcl, context, "runjob.jcl");

        // DSN should be obfuscated
        obfuscated.Content.Should().NotContain("PROD.PAYROLL.DATA");

        // Dataset name should be in mapping table
        context.Mappings.Forward.Should().ContainKey("PROD.PAYROLL.DATA");
        context.Mappings.Forward["PROD.PAYROLL.DATA"].Should().StartWith("DSN_");

        // Round-trip restore
        var restored = restorer.Restore(obfuscated.Content, context.Mappings);
        restored.Content.Should().Contain("PROD.PAYROLL.DATA");
    }

    // ═══════════════════════════════════════════════════════════════════
    // DISP Preservation in Cross-File Bundle
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void DispValues_PreservedInCrossFileBundle()
    {
        var context = CreateContextWithRegistry();
        var jclProcessor = new JclLanguageProcessor();
        var cobolProcessor = new CobolLanguageProcessor();

        // JCL with various DISP values
        var jcl = string.Join("\n",
            "//RUNJOB   JOB (ACCT),'RUN',CLASS=A",
            "//STEP01   EXEC PGM=PAYROLL",
            "//INFILE   DD DSN=PROD.MASTER.DATA,DISP=SHR",
            "//OUTFILE  DD DSN=PROD.OUTPUT.DATA,DISP=(NEW,CATLG,DELETE),",
            "//            SPACE=(CYL,(10,5)),UNIT=SYSDA",
            "//SYSPRINT DD SYSOUT=*",
            "//");
        var jclResult = jclProcessor.Obfuscate(jcl, context, "runjob.jcl");

        // COBOL program
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYROLL.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           STOP RUN.");
        cobolProcessor.Obfuscate(cobol, context, "payroll.cbl");

        // All DISP values must survive obfuscation
        jclResult.Content.Should().Contain("DISP=SHR");
        jclResult.Content.Should().Contain("NEW");
        jclResult.Content.Should().Contain("CATLG");
        jclResult.Content.Should().Contain("DELETE");

        // JCL structural keywords preserved
        jclResult.Content.Should().Contain("EXEC PGM=");
        jclResult.Content.Should().Contain("SPACE=");
        jclResult.Content.Should().Contain("UNIT=");
        jclResult.Content.Should().Contain("SYSOUT=*");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Copybook Reference Chain Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void CopybookChain_MultipleProgramsShareCopybook_SameAlias()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        // Program A copies SHARED-REC
        var progA = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PROG-A.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       COPY SHARED-REC.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           STOP RUN.");
        var resultA = processor.Obfuscate(progA, context, "proga.cbl");
        var copyAliasFromA = context.Mappings.Forward["SHARED-REC"];

        // Program B also copies SHARED-REC
        var progB = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PROG-B.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       COPY SHARED-REC.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           STOP RUN.");
        var resultB = processor.Obfuscate(progB, context, "progb.cbl");

        // Copybook file itself
        var copybook = BuildCobolSource(
            "       01  WS-SHARED-ID           PIC 9(8).",
            "       01  WS-SHARED-NAME         PIC X(40).");
        processor.Obfuscate(copybook, context, "SHARED-REC.cpy");

        // All three files use same alias
        copyAliasFromA.Should().StartWith("CPY_");
        context.Mappings.Forward["SHARED-REC"].Should().Be(copyAliasFromA);
        resultA.Content.Should().Contain($"COPY {copyAliasFromA}");
        resultB.Content.Should().Contain($"COPY {copyAliasFromA}");
    }

    [Fact]
    public void ExtensionlessCopybookChain_SameAliasAcrossFiles()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        // Extensionless COBOL program with COPY
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. MAINPGM.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       COPY RECDATA.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           STOP RUN.");
        var cobolResult = processor.Obfuscate(cobol, context, "MAINPGM");
        var copyAlias = context.Mappings.Forward["RECDATA"];

        // Extensionless copybook file
        var copybook = BuildCobolSource(
            "       01  WS-REC-ID              PIC 9(8).",
            "       01  WS-REC-NAME            PIC X(30).");
        processor.Obfuscate(copybook, context, "RECDATA");

        // Alias consistency across extensionless files
        copyAlias.Should().StartWith("CPY_");
        context.Mappings.Forward["RECDATA"].Should().Be(copyAlias);
        cobolResult.Content.Should().Contain($"COPY {copyAlias}");
    }

    [Fact]
    public void CopybookChain_FileNameMapper_CpyFileCorrelatesWithCopyRef()
    {
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        // COBOL program references CUSTDATA via COPY
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

        // Process the copybook file
        var copybook = BuildCobolSource(
            "       01  WS-CUST-ID             PIC 9(8).",
            "       01  WS-CUST-NAME           PIC X(40).");
        processor.Obfuscate(copybook, context, "CUSTDATA.cpy");

        // Run FileNameMapper
        var mapper = new FileNameMapper();
        mapper.BuildMappings(context, ["mainpgm.cbl", "CUSTDATA.cpy"]);

        // The .cpy file name should correlate with the COPY reference alias
        var cpyFilePath = context.Mappings.FilePathForward["CUSTDATA.cpy"];
        cpyFilePath.Should().Be($"{copyAlias}.cpy",
            because: "copybook file name alias should match the COPY statement alias");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Enterprise FTP Extensionless Edge Cases
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Extensionless_CopybookSelfRegistration_GetsFileMapperCpyAlias()
    {
        // Extensionless copybook with PIC clauses → self-registers as Copybook → CPY_N via FileNameMapper
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        var copybook = BuildCobolSource(
            "       01  WS-EMP-ID             PIC 9(8).",
            "       01  WS-EMP-NAME           PIC X(40).");
        processor.Obfuscate(copybook, context, "EMPDATA");

        // Self-registration should have created a Copybook alias
        context.Mappings.Forward.Should().ContainKey("EMPDATA");
        context.Mappings.Forward["EMPDATA"].Should().StartWith("CPY_");

        // FileNameMapper should use that CPY alias for the file name
        var mapper = new FileNameMapper();
        mapper.BuildMappings(context, ["EMPDATA"]);

        var newPath = context.Mappings.FilePathForward["EMPDATA"];
        newPath.Should().StartWith("CPY_",
            because: "extensionless copybook should get CPY_N alias via self-registration");
    }

    [Fact]
    public void Extensionless_CobolProgram_DoesNotSelfRegisterAsCopybook()
    {
        // Full COBOL program (has IDENTIFICATION DIVISION) → NOT a copybook
        var context = CreateContextWithRegistry();
        var processor = new CobolLanguageProcessor();

        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYROLL.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       01  WS-COUNT             PIC 9(4).",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           STOP RUN.");
        processor.Obfuscate(cobol, context, "PAYROLL");

        // PAYROLL should be registered as Program, not Copybook
        context.Mappings.Forward.Should().ContainKey("PAYROLL");
        context.Mappings.Forward["PAYROLL"].Should().StartWith("PGM_");
    }

    [Fact]
    public void Extensionless_CopybookWithCopyStatements_Detected()
    {
        // Extensionless file with only COPY statements → CanProcess returns true
        var processor = new CobolLanguageProcessor();
        var content = "       COPY EMPRECORD.\n       COPY DEPTDATA.\n";
        ((ILanguageProcessor)processor).CanProcess("", content).Should().BeTrue();
    }

    [Fact]
    public void Extensionless_88LevelOnly_NotDetected()
    {
        // 88-level conditions alone → CanProcess returns false (documented limitation)
        var processor = new CobolLanguageProcessor();
        var content = "       88  SUCCESS               VALUE 1.\n"
                    + "       88  FAILURE               VALUE 0.\n";
        ((ILanguageProcessor)processor).CanProcess("", content).Should().BeFalse();
    }

    [Fact]
    public void Extensionless_LowercaseCobol_Detected()
    {
        var processor = new CobolLanguageProcessor();
        var content = "       identification division.\n       program-id. testpgm.\n";
        ((ILanguageProcessor)processor).CanProcess("", content).Should().BeTrue();
    }

    [Fact]
    public void Extensionless_SequenceNumberedCobol_Detected()
    {
        // Sequence-numbered COBOL (columns 1-6 are numbers)
        var processor = new CobolLanguageProcessor();
        var content = "000100 IDENTIFICATION DIVISION.\n000200 PROGRAM-ID. TESTPGM.\n";
        ((ILanguageProcessor)processor).CanProcess("", content).Should().BeTrue();
    }

    [Fact]
    public void Extensionless_JclProc_Detected()
    {
        // Cataloged JCL procedure (no JOB card, starts with // PROC)
        var registry = new LanguageProcessorRegistry();
        registry.Register(new JclLanguageProcessor());
        var content = "//MYPROC  PROC\n//STEP1   EXEC PGM=SORT\n//SORTIN  DD DSN=INPUT.FILE,DISP=SHR\n// PEND\n";
        var processor = registry.GetProcessor("", content);
        processor.Should().NotBeNull();
        processor!.ProcessorId.Should().Be("jcl");
    }

    [Fact]
    public void Extensionless_PlainText_NoProcessorMatches()
    {
        var registry = new LanguageProcessorRegistry();
        registry.Register(new SqlLanguageProcessor());
        registry.Register(new CobolLanguageProcessor());
        registry.Register(new JclLanguageProcessor());
        var content = "This is just a plain text file with no code markers whatsoever.\nIt has multiple lines but no programming constructs.\n";
        var processor = registry.GetProcessor("", content);
        processor.Should().BeNull();
    }

    [Fact]
    public void Extensionless_EmptyFile_NoProcessorMatches()
    {
        var registry = new LanguageProcessorRegistry();
        registry.Register(new SqlLanguageProcessor());
        registry.Register(new CobolLanguageProcessor());
        registry.Register(new JclLanguageProcessor());
        var processor = registry.GetProcessor("", "   \n  \n");
        processor.Should().BeNull();
    }

    [Fact]
    public void Extensionless_FtpBundle_FullCrossCorrelation()
    {
        // Full FTP-style bundle: JCL + COBOL + copybook, all extensionless
        var context = CreateContextWithRegistry();
        var cobolProcessor = new CobolLanguageProcessor();
        var jclProcessor = new JclLanguageProcessor();

        // 1. Process the COBOL program (extensionless)
        var cobol = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYROLL.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
            "       COPY EMPDATA.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           DISPLAY 'HELLO'.",
            "           STOP RUN.");
        cobolProcessor.Obfuscate(cobol, context, "PAYROLL");

        // 2. Process the copybook (extensionless, no DIVISION headers)
        var copybook = BuildCobolSource(
            "       01  WS-EMP-ID             PIC 9(8).",
            "       01  WS-EMP-NAME           PIC X(40).");
        cobolProcessor.Obfuscate(copybook, context, "EMPDATA");

        // 3. Process the JCL job (extensionless)
        var jcl = "//RUNJOB  JOB (ACCT),'BATCH',CLASS=A\n"
                + "//STEP1   EXEC PGM=PAYROLL\n"
                + "//INFILE  DD DSN=PROD.EMP.DATA,DISP=SHR\n";
        ((ILanguageProcessor)jclProcessor).Obfuscate(jcl, context, "RUNJOB");

        // Verify cross-correlation
        var payrollAlias = context.Mappings.Forward["PAYROLL"];
        var empdataAlias = context.Mappings.Forward["EMPDATA"];

        // JCL PGM=PAYROLL should get the same alias as COBOL PROGRAM-ID PAYROLL
        payrollAlias.Should().StartWith("PGM_");

        // Copybook EMPDATA should get CPY_ prefix (self-registered by LooksLikeCopybook)
        empdataAlias.Should().StartWith("CPY_");

        // FileNameMapper: all three extensionless files get correct aliases
        var mapper = new FileNameMapper();
        mapper.BuildMappings(context, ["PAYROLL", "EMPDATA", "RUNJOB"]);

        var payrollPath = context.Mappings.FilePathForward["PAYROLL"];
        var empdataPath = context.Mappings.FilePathForward["EMPDATA"];
        var runjobPath = context.Mappings.FilePathForward["RUNJOB"];

        payrollPath.Should().StartWith("PGM_",
            because: "extensionless COBOL program should get PGM_N file name");
        empdataPath.Should().StartWith("CPY_",
            because: "extensionless copybook should get CPY_N file name");
        runjobPath.Should().StartWith("JOB_",
            because: "extensionless JCL should get JOB_N file name");
    }

    [Fact]
    public void Extensionless_MixedBundle_WithAndWithoutExtensions()
    {
        // Mix of files with and without extensions → all processed correctly
        var context = CreateContextWithRegistry();
        var cobolProcessor = new CobolLanguageProcessor();
        var jclProcessor = new JclLanguageProcessor();

        // Extensionless COBOL program
        var cobol1 = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. PAYROLL.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           STOP RUN.");
        cobolProcessor.Obfuscate(cobol1, context, "PAYROLL");

        // COBOL program with extension
        var cobol2 = BuildCobolSource(
            "       IDENTIFICATION DIVISION.",
            "       PROGRAM-ID. BILLING.",
            "       DATA DIVISION.",
            "       PROCEDURE DIVISION.",
            "       MAIN-PARA.",
            "           CALL 'PAYROLL'.",
            "           STOP RUN.");
        cobolProcessor.Obfuscate(cobol2, context, "billing.cbl");

        // Extensionless copybook
        var copybook = BuildCobolSource(
            "       01  WS-AMT             PIC 9(8)V99.");
        cobolProcessor.Obfuscate(copybook, context, "AMOUNTS");

        // JCL with extension
        var jcl = "//RUNJOB  JOB (ACCT),'BATCH',CLASS=A\n"
                + "//STEP1   EXEC PGM=PAYROLL\n"
                + "//STEP2   EXEC PGM=BILLING\n";
        ((ILanguageProcessor)jclProcessor).Obfuscate(jcl, context, "runjob.jcl");

        // Cross-file consistency: PAYROLL alias is the same everywhere
        var payrollAlias = context.Mappings.Forward["PAYROLL"];
        var billingAlias = context.Mappings.Forward["BILLING"];

        payrollAlias.Should().StartWith("PGM_");
        billingAlias.Should().StartWith("PGM_");

        // FileNameMapper handles mixed extension/extensionless
        var mapper = new FileNameMapper();
        mapper.BuildMappings(context, ["PAYROLL", "billing.cbl", "AMOUNTS", "runjob.jcl"]);

        context.Mappings.FilePathForward["PAYROLL"].Should().StartWith("PGM_");
        context.Mappings.FilePathForward["billing.cbl"].Should().EndWith(".cbl");
        context.Mappings.FilePathForward["AMOUNTS"].Should().StartWith("CPY_");
        context.Mappings.FilePathForward["runjob.jcl"].Should().EndWith(".jcl");
    }
}
