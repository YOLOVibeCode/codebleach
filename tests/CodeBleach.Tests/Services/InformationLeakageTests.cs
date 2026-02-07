using System.Text.RegularExpressions;
using CodeBleach.Core.Models;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

public class InformationLeakageTests : IDisposable
{
    private readonly string _tempDir;

    public InformationLeakageTests()
    {
        _tempDir = Path.Combine(Path.GetTempPath(), $"codebleach_leak_{Guid.NewGuid():N}");
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

    private static readonly Regex ObfuscatedPathPattern = new(
        @"^((src|lib|test|tests|public|wwwroot|assets|config|scripts|docs|resources|Properties|DIR_\d+)/)*"
        + @"(FILE_\d+|CLS_\d+|PROJ_\d+|MOD_\d+|CPY_\d+|JOB_\d+|INT_\d+|REC_\d+|PGM_\d+|ENM_\d+|DLG_\d+)(\.\w+)?$",
        RegexOptions.Compiled);

    [Fact]
    public void FileNameMapper_BusinessDirectoryNames_NeverInOutput()
    {
        var context = new ObfuscationContext(ObfuscationLevel.Full);
        var mapper = new FileNameMapper();
        var files = new List<string>
        {
            "AcmeWidgets/PaymentGateway/processor.js",
            "HumanResources/Payroll/calculator.js",
            "FinancialReporting/Quarterly/report.js"
        };

        mapper.BuildMappings(context, files);

        var allPaths = context.Mappings.FilePathForward.Values.ToList();
        foreach (var path in allPaths)
        {
            path.Should().NotContain("AcmeWidgets");
            path.Should().NotContain("PaymentGateway");
            path.Should().NotContain("HumanResources");
            path.Should().NotContain("Payroll");
            path.Should().NotContain("FinancialReporting");
            path.Should().NotContain("Quarterly");
        }
    }

    [Fact]
    public void FileNameMapper_ProjectFileNames_NeverLeakOrgName()
    {
        var context = new ObfuscationContext(ObfuscationLevel.Full);
        var mapper = new FileNameMapper();
        var files = new List<string>
        {
            "AcmePayroll.csproj",
            "AcmePayroll.sln",
            "AcmeWidgets.Api.fsproj"
        };

        mapper.BuildMappings(context, files);

        foreach (var (_, newPath) in context.Mappings.FilePathForward)
        {
            newPath.Should().NotContain("Acme");
            newPath.Should().NotContain("Payroll");
            newPath.Should().NotContain("Widgets");
            newPath.Should().MatchRegex(@"^PROJ_\d+\.\w+$");
        }
    }

    [Fact]
    public void FileNameMapper_CSharpFiles_DeriveFromAliasNotOriginal()
    {
        var context = new ObfuscationContext(ObfuscationLevel.Full);
        context.GetOrCreateAlias("EmployeeService", SemanticCategory.Class, "EmployeeService.cs", 1, 0, 15);

        var mapper = new FileNameMapper();
        var files = new List<string> { "EmployeeService.cs" };

        mapper.BuildMappings(context, files);

        var newPath = context.Mappings.FilePathForward["EmployeeService.cs"];
        newPath.Should().NotContain("Employee");
        newPath.Should().NotContain("Service");
        newPath.Should().MatchRegex(@"^CLS_\d+\.cs$");
    }

    [Fact]
    public void FileNameMapper_SqlFiles_NeverLeakTableNames()
    {
        var context = new ObfuscationContext(ObfuscationLevel.Full);
        var mapper = new FileNameMapper();
        var files = new List<string>
        {
            "PayrollReport_2024.sql",
            "SalaryAdjustment.sql",
            "EmployeeBonus.sql"
        };

        mapper.BuildMappings(context, files);

        foreach (var (_, newPath) in context.Mappings.FilePathForward)
        {
            newPath.Should().NotContain("Payroll");
            newPath.Should().NotContain("Salary");
            newPath.Should().NotContain("Employee");
            newPath.Should().NotContain("Bonus");
            newPath.Should().MatchRegex(@"^FILE_\d+\.sql$");
        }
    }

    [Fact]
    public async Task FileReferencePatcher_PatchedRefs_OnlyObfuscatedPaths()
    {
        // Create a .csproj with business-revealing references in a subdirectory
        var csprojContent = @"<Project Sdk=""Microsoft.NET.Sdk"">
  <ItemGroup>
    <Compile Include=""PayrollService.cs"" />
    <Compile Include=""EmployeeModel.cs"" />
    <ProjectReference Include=""..\SalaryLib\SalaryLib.csproj"" />
  </ItemGroup>
</Project>";
        CreateFile("src/AcmeApp/AcmeApp.csproj", csprojContent);

        var mappings = new MappingTable();
        mappings.FilePathForward["src/AcmeApp/AcmeApp.csproj"] = "src/DIR_0/PROJ_0.csproj";
        mappings.FilePathForward["src/AcmeApp/PayrollService.cs"] = "src/DIR_0/CLS_0.cs";
        mappings.FilePathForward["src/AcmeApp/EmployeeModel.cs"] = "src/DIR_0/CLS_1.cs";
        mappings.FilePathForward["src/SalaryLib/SalaryLib.csproj"] = "src/DIR_1/PROJ_1.csproj";

        var patcher = new FileReferencePatcher();
        await patcher.PatchReferencesAsync(_tempDir, mappings);

        var patched = File.ReadAllText(Path.Combine(_tempDir, "src", "AcmeApp", "AcmeApp.csproj"));
        // Business names must be gone from Include attributes
        patched.Should().NotContain("PayrollService");
        patched.Should().NotContain("EmployeeModel");
        patched.Should().NotContain("SalaryLib");
        // Only obfuscated names in Include attributes
        patched.Should().Contain("CLS_0.cs");
        patched.Should().Contain("CLS_1.cs");
        patched.Should().Contain("PROJ_1.csproj");
    }

    [Fact]
    public void FullPipeline_AllPaths_MatchObfuscationPattern()
    {
        var context = new ObfuscationContext(ObfuscationLevel.Full);
        // Simulate typical project with business names
        context.GetOrCreateAlias("PayrollService", SemanticCategory.Class, "src/Services/PayrollService.cs", 1, 0, 14);
        context.GetOrCreateAlias("EmployeeModel", SemanticCategory.Class, "src/Models/EmployeeModel.cs", 1, 0, 13);
        context.GetOrCreateAlias("SalaryCalculator", SemanticCategory.Class, "src/Core/SalaryCalculator.cs", 1, 0, 16);

        var mapper = new FileNameMapper();
        var files = new List<string>
        {
            "src/Services/PayrollService.cs",
            "src/Models/EmployeeModel.cs",
            "src/Core/SalaryCalculator.cs",
            "src/config.json",
            "AcmePayroll.csproj",
            "AcmePayroll.sln"
        };

        mapper.BuildMappings(context, files);

        foreach (var (original, newPath) in context.Mappings.FilePathForward)
        {
            ObfuscatedPathPattern.IsMatch(newPath).Should().BeTrue(
                because: $"'{newPath}' (from '{original}') should match the obfuscation pattern");
        }
    }
}
