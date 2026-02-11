using CodeBleach.Core.Models;

namespace CodeBleach.Tests.Models;

public class ObfuscationScopeTests
{
    // ═══════════════════════════════════════════════════════════════════
    // A. All() — unfiltered (default behavior)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void All_IsNotFiltered()
    {
        var scope = ObfuscationScope.All();

        scope.IsFiltered.Should().BeFalse();
        scope.RawSpecifiers.Should().BeEmpty();
    }

    [Theory]
    [InlineData("tsql")]
    [InlineData("cobol")]
    [InlineData("csharp")]
    [InlineData("javascript")]
    [InlineData("vbscript")]
    [InlineData("fsharp")]
    public void All_EverythingIsInScope(string processorId)
    {
        var scope = ObfuscationScope.All();

        scope.IsInScope(processorId).Should().BeTrue();
        scope.IsDelegationOnly(processorId).Should().BeFalse();
    }

    // ═══════════════════════════════════════════════════════════════════
    // B. Parse() — null/empty returns All
    // ═══════════════════════════════════════════════════════════════════

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData("   ")]
    public void Parse_NullOrEmpty_ReturnsAll(string? csv)
    {
        var scope = ObfuscationScope.Parse(csv);

        scope.IsFiltered.Should().BeFalse();
    }

    // ═══════════════════════════════════════════════════════════════════
    // C. Parse() — group resolution
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Parse_DatabaseGroup_ResolvesSqlProcessors()
    {
        var scope = ObfuscationScope.Parse("database");

        scope.IsFiltered.Should().BeTrue();
        scope.RawSpecifiers.Should().Equal("database");

        scope.IsInScope("tsql").Should().BeTrue();
        scope.IsInScope("db2sql").Should().BeTrue();
        scope.IsInScope("oraclesql").Should().BeTrue();

        scope.IsInScope("cobol").Should().BeFalse();
        scope.IsInScope("jcl").Should().BeFalse();
        scope.IsInScope("csharp").Should().BeFalse();
        scope.IsInScope("javascript").Should().BeFalse();
    }

    [Fact]
    public void Parse_MainframeGroup_ResolvesMainframeProcessors()
    {
        var scope = ObfuscationScope.Parse("mainframe");

        scope.IsInScope("cobol").Should().BeTrue();
        scope.IsInScope("jcl").Should().BeTrue();
        scope.IsInScope("mainframe-utility").Should().BeTrue();

        scope.IsInScope("tsql").Should().BeFalse();
        scope.IsInScope("csharp").Should().BeFalse();
    }

    [Fact]
    public void Parse_DotnetGroup_ResolvesDotnetProcessors()
    {
        var scope = ObfuscationScope.Parse("dotnet");

        scope.IsInScope("csharp").Should().BeTrue();
        scope.IsInScope("vbnet").Should().BeTrue();
        scope.IsInScope("fsharp").Should().BeTrue();

        scope.IsInScope("tsql").Should().BeFalse();
        scope.IsInScope("cobol").Should().BeFalse();
    }

    [Fact]
    public void Parse_WebGroup_ResolvesJavaScript()
    {
        var scope = ObfuscationScope.Parse("web");

        scope.IsInScope("javascript").Should().BeTrue();
        scope.IsInScope("csharp").Should().BeFalse();
    }

    [Fact]
    public void Parse_ScriptingGroup_ResolvesVbScript()
    {
        var scope = ObfuscationScope.Parse("scripting");

        scope.IsInScope("vbscript").Should().BeTrue();
        scope.IsInScope("javascript").Should().BeFalse();
    }

    // ═══════════════════════════════════════════════════════════════════
    // D. Parse() — individual processor IDs
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Parse_SingleProcessorId_OnlyThatInScope()
    {
        var scope = ObfuscationScope.Parse("cobol");

        scope.IsFiltered.Should().BeTrue();
        scope.IsInScope("cobol").Should().BeTrue();
        scope.IsInScope("jcl").Should().BeFalse();
        scope.IsInScope("tsql").Should().BeFalse();
    }

    // ═══════════════════════════════════════════════════════════════════
    // E. Parse() — combinations (group + ID, multiple groups)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Parse_MultipleGroups_BothResolved()
    {
        var scope = ObfuscationScope.Parse("database,mainframe");

        scope.IsFiltered.Should().BeTrue();
        scope.RawSpecifiers.Should().Equal("database", "mainframe");

        // Database processors
        scope.IsInScope("tsql").Should().BeTrue();
        scope.IsInScope("db2sql").Should().BeTrue();
        scope.IsInScope("oraclesql").Should().BeTrue();

        // Mainframe processors
        scope.IsInScope("cobol").Should().BeTrue();
        scope.IsInScope("jcl").Should().BeTrue();

        // Others out of scope
        scope.IsInScope("csharp").Should().BeFalse();
        scope.IsInScope("javascript").Should().BeFalse();
    }

    [Fact]
    public void Parse_GroupPlusProcessorId_BothResolved()
    {
        var scope = ObfuscationScope.Parse("database,cobol");

        scope.IsInScope("tsql").Should().BeTrue();
        scope.IsInScope("cobol").Should().BeTrue();
        scope.IsInScope("jcl").Should().BeFalse();
    }

    [Fact]
    public void Parse_CaseInsensitive()
    {
        var scope = ObfuscationScope.Parse("Database,COBOL");

        scope.IsInScope("tsql").Should().BeTrue();
        scope.IsInScope("cobol").Should().BeTrue();
    }

    [Fact]
    public void Parse_WhitespaceAroundSpecifiers_Trimmed()
    {
        var scope = ObfuscationScope.Parse(" database , mainframe ");

        scope.IsInScope("tsql").Should().BeTrue();
        scope.IsInScope("cobol").Should().BeTrue();
    }

    // ═══════════════════════════════════════════════════════════════════
    // F. Parse() — validation (unknown specifiers)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Parse_UnknownSpecifier_Throws()
    {
        var act = () => ObfuscationScope.Parse("nonexistent");

        act.Should().Throw<ArgumentException>()
            .WithMessage("*Unknown scope specifier 'nonexistent'*");
    }

    [Fact]
    public void Parse_MixedValidAndInvalid_Throws()
    {
        var act = () => ObfuscationScope.Parse("database,bogus");

        act.Should().Throw<ArgumentException>()
            .WithMessage("*Unknown scope specifier 'bogus'*");
    }

    // ═══════════════════════════════════════════════════════════════════
    // G. IsDelegationOnly — inverse of IsInScope when filtered
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void IsDelegationOnly_InScopeProcessor_ReturnsFalse()
    {
        var scope = ObfuscationScope.Parse("database");

        scope.IsDelegationOnly("tsql").Should().BeFalse();
    }

    [Fact]
    public void IsDelegationOnly_OutOfScopeProcessor_ReturnsTrue()
    {
        var scope = ObfuscationScope.Parse("database");

        scope.IsDelegationOnly("cobol").Should().BeTrue();
        scope.IsDelegationOnly("csharp").Should().BeTrue();
        scope.IsDelegationOnly("javascript").Should().BeTrue();
    }

    [Fact]
    public void IsDelegationOnly_Unfiltered_AlwaysFalse()
    {
        var scope = ObfuscationScope.All();

        scope.IsDelegationOnly("cobol").Should().BeFalse();
        scope.IsDelegationOnly("tsql").Should().BeFalse();
    }

    // ═══════════════════════════════════════════════════════════════════
    // H. Groups dictionary is populated
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Groups_ContainsFiveEntries()
    {
        ObfuscationScope.Groups.Should().HaveCount(5);
        ObfuscationScope.Groups.Should().ContainKeys("database", "mainframe", "dotnet", "web", "scripting");
    }
}
