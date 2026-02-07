using CodeBleach.Core.Models;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

public class FileNameMapperEdgeCaseTests
{
    private static ObfuscationContext CreateContext()
    {
        return new ObfuscationContext(ObfuscationLevel.Full);
    }

    [Theory]
    [InlineData("Makefile")]
    [InlineData("Dockerfile")]
    [InlineData("LICENSE")]
    public void BuildMappings_FileWithNoExtension_AssignsFileAlias(string filename)
    {
        var context = CreateContext();
        var mapper = new FileNameMapper();
        var files = new List<string> { filename };

        mapper.BuildMappings(context, files);

        var newPath = context.Mappings.FilePathForward[filename];
        // No extension, so FILE_N with no extension
        newPath.Should().MatchRegex(@"^FILE_\d+$");
    }

    [Fact]
    public void BuildMappings_DoubleExtension_MinJs_PreservesOuterOnly()
    {
        var context = CreateContext();
        var mapper = new FileNameMapper();
        var files = new List<string> { "bundle.min.js" };

        mapper.BuildMappings(context, files);

        var newPath = context.Mappings.FilePathForward["bundle.min.js"];
        newPath.Should().EndWith(".js");
        newPath.Should().MatchRegex(@"^FILE_\d+\.js$");
    }

    [Fact]
    public void BuildMappings_HiddenFile_AssignsFileAlias()
    {
        var context = CreateContext();
        var mapper = new FileNameMapper();
        var files = new List<string> { ".gitignore" };

        mapper.BuildMappings(context, files);

        context.Mappings.FilePathForward.Should().ContainKey(".gitignore");
        var newPath = context.Mappings.FilePathForward[".gitignore"];
        // .gitignore has extension ".gitignore" per Path.GetExtension
        newPath.Should().MatchRegex(@"^FILE_\d+\.gitignore$");
    }

    [Fact]
    public void BuildMappings_MultipleTypesInSameFile_PrefersFileStemMatch()
    {
        var context = CreateContext();
        // File "Employee.cs" contains both class "Employee" and class "EmployeeValidator"
        context.GetOrCreateAlias("EmployeeValidator", SemanticCategory.Class, "Employee.cs", 10, 0, 18);
        context.GetOrCreateAlias("Employee", SemanticCategory.Class, "Employee.cs", 1, 0, 8);

        var mapper = new FileNameMapper();
        var files = new List<string> { "Employee.cs" };

        mapper.BuildMappings(context, files);

        var newPath = context.Mappings.FilePathForward["Employee.cs"];
        // Should prefer "Employee" → CLS_1 because it matches the file stem
        newPath.Should().Be("CLS_1.cs");
    }

    [Fact]
    public void BuildMappings_PartialClasses_CollisionFallback()
    {
        var context = CreateContext();
        // Two files both have class "Helper" as primary type
        context.GetOrCreateAlias("Helper", SemanticCategory.Class, "Helper.cs", 1, 0, 6);
        context.SourceMap.GetOrAddEntry("Helper", "CLS_0", SemanticCategory.Class)
            .Locations.Add(new SourceLocation
            {
                FilePath = "HelperPart2.cs",
                LineNumber = 1,
                ColumnStart = 0,
                ColumnEnd = 6
            });

        var mapper = new FileNameMapper();
        var files = new List<string> { "Helper.cs", "HelperPart2.cs" };

        mapper.BuildMappings(context, files);

        var path1 = context.Mappings.FilePathForward["Helper.cs"];
        var path2 = context.Mappings.FilePathForward["HelperPart2.cs"];
        // First gets CLS_0, second collides → falls back to FILE_N
        path1.Should().Be("CLS_0.cs");
        path2.Should().MatchRegex(@"^FILE_\d+\.cs$");
    }

    [Fact]
    public void BuildMappings_FSharpModule_UsesModAlias()
    {
        var context = CreateContext();
        context.GetOrCreateAlias("Utilities", SemanticCategory.Module, "Utilities.fs", 1, 0, 9);

        var mapper = new FileNameMapper();
        var files = new List<string> { "Utilities.fs" };

        mapper.BuildMappings(context, files);

        var newPath = context.Mappings.FilePathForward["Utilities.fs"];
        newPath.Should().Be("MOD_0.fs");
    }

    [Fact]
    public void BuildMappings_FSharpInterfaceFile_Fsi_UsesStrategyA()
    {
        var context = CreateContext();
        context.GetOrCreateAlias("Types", SemanticCategory.Module, "Types.fsi", 1, 0, 5);

        var mapper = new FileNameMapper();
        var files = new List<string> { "Types.fsi" };

        mapper.BuildMappings(context, files);

        var newPath = context.Mappings.FilePathForward["Types.fsi"];
        // .fsi uses Strategy A → should use MOD_0 alias
        newPath.Should().Be("MOD_0.fsi");
    }

    [Fact]
    public void BuildMappings_AllStructuralDirsPreserved()
    {
        var structuralDirs = new[]
        {
            "src", "lib", "test", "tests", "public", "wwwroot",
            "assets", "config", "scripts", "docs", "resources", "Properties"
        };

        var context = CreateContext();
        var mapper = new FileNameMapper();
        var files = structuralDirs.Select(d => $"{d}/file.js").ToList();

        mapper.BuildMappings(context, files);

        foreach (var dir in structuralDirs)
        {
            var newPath = context.Mappings.FilePathForward[$"{dir}/file.js"];
            newPath.Should().StartWith($"{dir}/",
                because: $"structural directory '{dir}' should be preserved");
        }
    }

    [Fact]
    public void BuildMappings_DeeplyNestedMixedDirs_OnlyNonStructuralRenamed()
    {
        var context = CreateContext();
        var mapper = new FileNameMapper();
        var files = new List<string> { "src/AcmeWidgets/Services/Internal/helper.js" };

        mapper.BuildMappings(context, files);

        var newPath = context.Mappings.FilePathForward["src/AcmeWidgets/Services/Internal/helper.js"];
        // "src" is structural → preserved
        newPath.Should().StartWith("src/");
        // "AcmeWidgets", "Services", "Internal" are non-structural → DIR_N
        newPath.Should().NotContain("AcmeWidgets");
        newPath.Should().NotContain("Services");
        newPath.Should().NotContain("Internal");
        // Should have DIR_N segments
        newPath.Should().MatchRegex(@"src/DIR_\d+/DIR_\d+/DIR_\d+/FILE_\d+\.js");
    }
}
