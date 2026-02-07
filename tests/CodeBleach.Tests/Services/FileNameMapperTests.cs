using CodeBleach.Core.Models;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

public class FileNameMapperTests
{
    private static ObfuscationContext CreateContext()
    {
        return new ObfuscationContext(ObfuscationLevel.Full);
    }

    [Fact]
    public void BuildMappings_SimpleFile_AssignsFileAlias()
    {
        var context = CreateContext();
        var mapper = new FileNameMapper();
        var files = new List<string> { "helper.js" };

        mapper.BuildMappings(context, files);

        context.Mappings.FilePathForward.Should().ContainKey("helper.js");
        var newPath = context.Mappings.FilePathForward["helper.js"];
        newPath.Should().MatchRegex(@"^FILE_\d+\.js$");
    }

    [Fact]
    public void BuildMappings_ProjectFile_AssignsProjAlias()
    {
        var context = CreateContext();
        var mapper = new FileNameMapper();
        var files = new List<string> { "MyProject.csproj" };

        mapper.BuildMappings(context, files);

        var newPath = context.Mappings.FilePathForward["MyProject.csproj"];
        newPath.Should().MatchRegex(@"^PROJ_\d+\.csproj$");
    }

    [Fact]
    public void BuildMappings_SolutionFile_AssignsProjAlias()
    {
        var context = CreateContext();
        var mapper = new FileNameMapper();
        var files = new List<string> { "MyProject.sln" };

        mapper.BuildMappings(context, files);

        var newPath = context.Mappings.FilePathForward["MyProject.sln"];
        newPath.Should().MatchRegex(@"^PROJ_\d+\.sln$");
    }

    [Fact]
    public void BuildMappings_StructuralDirectory_PreservedInPath()
    {
        var context = CreateContext();
        var mapper = new FileNameMapper();
        var files = new List<string> { "src/helper.js" };

        mapper.BuildMappings(context, files);

        var newPath = context.Mappings.FilePathForward["src/helper.js"];
        newPath.Should().StartWith("src/");
    }

    [Fact]
    public void BuildMappings_BusinessDirectory_RenamedToDir()
    {
        var context = CreateContext();
        var mapper = new FileNameMapper();
        var files = new List<string> { "Payroll/Employee.js" };

        mapper.BuildMappings(context, files);

        var newPath = context.Mappings.FilePathForward["Payroll/Employee.js"];
        newPath.Should().MatchRegex(@"^DIR_\d+/FILE_\d+\.js$");
    }

    [Fact]
    public void BuildMappings_NestedDirectories_AllNonStructuralRenamed()
    {
        var context = CreateContext();
        var mapper = new FileNameMapper();
        var files = new List<string> { "src/HumanResources/Payroll/calculator.js" };

        mapper.BuildMappings(context, files);

        var newPath = context.Mappings.FilePathForward["src/HumanResources/Payroll/calculator.js"];
        // src should be preserved, HumanResources and Payroll should be DIR_N
        newPath.Should().StartWith("src/DIR_");
        newPath.Should().NotContain("HumanResources");
        newPath.Should().NotContain("Payroll");
    }

    [Fact]
    public void BuildMappings_StrategyA_CSharpFile_UsesPrimaryTypeAlias()
    {
        var context = CreateContext();
        // Simulate a C# class "Employee" being obfuscated to "CLS_0"
        context.GetOrCreateAlias("Employee", SemanticCategory.Class, "Employee.cs", 1, 0, 8);

        var mapper = new FileNameMapper();
        var files = new List<string> { "Employee.cs" };

        mapper.BuildMappings(context, files);

        var newPath = context.Mappings.FilePathForward["Employee.cs"];
        newPath.Should().Be("CLS_0.cs");
    }

    [Fact]
    public void BuildMappings_StrategyA_CSharpFile_FallsBackToFileN()
    {
        var context = CreateContext();
        // No type declarations recorded in SourceMap
        var mapper = new FileNameMapper();
        var files = new List<string> { "Utilities.cs" };

        mapper.BuildMappings(context, files);

        var newPath = context.Mappings.FilePathForward["Utilities.cs"];
        newPath.Should().MatchRegex(@"^FILE_\d+\.cs$");
    }

    [Fact]
    public void BuildMappings_StrategyA_CobolCopybook_UsesCpyAlias()
    {
        var context = CreateContext();
        context.GetOrCreateAlias("EMPLOYEE-RECORD", SemanticCategory.Copybook, "EMPLOYEE-RECORD.cpy", 1);

        var mapper = new FileNameMapper();
        var files = new List<string> { "EMPLOYEE-RECORD.cpy" };

        mapper.BuildMappings(context, files);

        var newPath = context.Mappings.FilePathForward["EMPLOYEE-RECORD.cpy"];
        newPath.Should().Be("CPY_0.cpy");
    }

    [Fact]
    public void BuildMappings_StrategyA_JclFile_UsesJobAlias()
    {
        var context = CreateContext();
        context.GetOrCreateAlias("PAYPROC", SemanticCategory.Job, "PAYPROC.jcl", 1);

        var mapper = new FileNameMapper();
        var files = new List<string> { "PAYPROC.jcl" };

        mapper.BuildMappings(context, files);

        var newPath = context.Mappings.FilePathForward["PAYPROC.jcl"];
        newPath.Should().Be("JOB_0.jcl");
    }

    [Fact]
    public void BuildMappings_MultipleFsprojFiles_DifferentProjAliases()
    {
        var context = CreateContext();
        var mapper = new FileNameMapper();
        var files = new List<string> { "MyLib.fsproj", "MyApp.fsproj" };

        mapper.BuildMappings(context, files);

        var proj0 = context.Mappings.FilePathForward["MyLib.fsproj"];
        var proj1 = context.Mappings.FilePathForward["MyApp.fsproj"];
        proj0.Should().NotBe(proj1);
        proj0.Should().MatchRegex(@"^PROJ_\d+\.fsproj$");
        proj1.Should().MatchRegex(@"^PROJ_\d+\.fsproj$");
    }

    [Fact]
    public void BuildMappings_SameDirectoryName_GetsSameAlias()
    {
        var context = CreateContext();
        var mapper = new FileNameMapper();
        var files = new List<string>
        {
            "src/A/Utils/file1.js",
            "src/B/Utils/file2.js"
        };

        mapper.BuildMappings(context, files);

        var path1 = context.Mappings.FilePathForward["src/A/Utils/file1.js"];
        var path2 = context.Mappings.FilePathForward["src/B/Utils/file2.js"];

        // Both "Utils" directories should get the same DIR alias
        var utils1 = path1.Split('/')[2]; // third segment
        var utils2 = path2.Split('/')[2];
        utils1.Should().Be(utils2);

        // But parent dirs A and B should be different
        var a = path1.Split('/')[1];
        var b = path2.Split('/')[1];
        a.Should().NotBe(b);
    }

    [Fact]
    public void BuildMappings_BidirectionalMappings_Consistent()
    {
        var context = CreateContext();
        var mapper = new FileNameMapper();
        var files = new List<string> { "src/Payroll/Service.cs", "src/Payroll/Model.cs" };

        mapper.BuildMappings(context, files);

        foreach (var (original, obfuscated) in context.Mappings.FilePathForward)
        {
            context.Mappings.FilePathReverse.Should().ContainKey(obfuscated);
            context.Mappings.FilePathReverse[obfuscated].Should().Be(original);
        }
    }

    [Fact]
    public void BuildMappings_PreservesExtension()
    {
        var context = CreateContext();
        var mapper = new FileNameMapper();
        var files = new List<string> { "data.json", "style.css", "report.sql", "app.tsx" };

        mapper.BuildMappings(context, files);

        context.Mappings.FilePathForward["data.json"].Should().EndWith(".json");
        context.Mappings.FilePathForward["style.css"].Should().EndWith(".css");
        context.Mappings.FilePathForward["report.sql"].Should().EndWith(".sql");
        context.Mappings.FilePathForward["app.tsx"].Should().EndWith(".tsx");
    }

    [Fact]
    public void BuildMappings_CollisionAvoidance_FallsBackToFileN()
    {
        var context = CreateContext();
        // Two files both containing class "Helper" in same directory
        context.GetOrCreateAlias("Helper", SemanticCategory.Class, "Helper.cs", 1);
        // The second file "HelperExtensions.cs" also has class "Helper" as its first type
        context.SourceMap.GetOrAddEntry("Helper", "CLS_0", SemanticCategory.Class)
            .Locations.Add(new SourceLocation { FilePath = "HelperExtensions.cs", LineNumber = 1, ColumnStart = 0, ColumnEnd = 0 });

        var mapper = new FileNameMapper();
        var files = new List<string> { "Helper.cs", "HelperExtensions.cs" };

        mapper.BuildMappings(context, files);

        var path1 = context.Mappings.FilePathForward["Helper.cs"];
        var path2 = context.Mappings.FilePathForward["HelperExtensions.cs"];

        // Both should have different names (collision avoidance)
        path1.Should().NotBe(path2);
    }
}
