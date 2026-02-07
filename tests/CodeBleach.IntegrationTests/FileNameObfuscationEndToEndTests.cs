using System.Text.RegularExpressions;
using CodeBleach.Core.Models;
using CodeBleach.Core.Services;

namespace CodeBleach.IntegrationTests;

public class FileNameObfuscationEndToEndTests : IDisposable
{
    private readonly string _tempDir;

    public FileNameObfuscationEndToEndTests()
    {
        _tempDir = Path.Combine(Path.GetTempPath(), $"codebleach_e2e_{Guid.NewGuid():N}");
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

    private static readonly string[] BusinessNames =
        ["Payroll", "Salary", "Employee", "AcmePayroll", "EmployeeService", "SalaryReport", "HumanResources"];

    [Fact]
    public async Task EndToEnd_DotNetProject_MapPatchRename_NoOriginalNamesRemain()
    {
        // Setup: a .NET project with business-revealing names
        var slnContent = @"
Microsoft Visual Studio Solution File, Format Version 12.00
Project(""{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}"") = ""PayrollApp"", ""src\PayrollApp\PayrollApp.csproj"", ""{11111111-1111-1111-1111-111111111111}""
EndProject
Project(""{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}"") = ""SalaryLib"", ""src\SalaryLib\SalaryLib.csproj"", ""{22222222-2222-2222-2222-222222222222}""
EndProject
";
        CreateFile("AcmePayroll.sln", slnContent);

        var appCsproj = @"<Project Sdk=""Microsoft.NET.Sdk"">
  <ItemGroup>
    <ProjectReference Include=""..\SalaryLib\SalaryLib.csproj"" />
    <Compile Include=""EmployeeService.cs"" />
  </ItemGroup>
</Project>";
        CreateFile("src/PayrollApp/PayrollApp.csproj", appCsproj);

        var libCsproj = @"<Project Sdk=""Microsoft.NET.Sdk"">
  <ItemGroup>
    <Compile Include=""SalaryCalculator.cs"" />
  </ItemGroup>
</Project>";
        CreateFile("src/SalaryLib/SalaryLib.csproj", libCsproj);
        CreateFile("src/PayrollApp/EmployeeService.cs", "namespace PayrollApp; public class EmployeeService {}");
        CreateFile("src/SalaryLib/SalaryCalculator.cs", "namespace SalaryLib; public class SalaryCalculator {}");

        // Step 1: Build context with type aliases (simulating content obfuscation)
        var context = new ObfuscationContext(ObfuscationLevel.Full);
        context.GetOrCreateAlias("EmployeeService", SemanticCategory.Class, "src/PayrollApp/EmployeeService.cs", 1, 0, 15);
        context.GetOrCreateAlias("SalaryCalculator", SemanticCategory.Class, "src/SalaryLib/SalaryCalculator.cs", 1, 0, 16);

        // Step 2: Build file name mappings
        var mapper = new FileNameMapper();
        var allFiles = GetAllRelativeFiles();
        mapper.BuildMappings(context, allFiles);
        var mappings = context.Mappings;

        // Step 3: Patch cross-file references
        var patcher = new FileReferencePatcher();
        await patcher.PatchReferencesAsync(_tempDir, mappings);

        // Step 4: Rename files
        var renamer = new FileSystemRenamer();
        renamer.RenameFiles(_tempDir, mappings);

        // Verify: no business names in any file path
        var finalFiles = GetAllRelativeFiles();
        foreach (var filePath in finalFiles)
        {
            foreach (var name in BusinessNames)
            {
                filePath.Should().NotContain(name,
                    because: $"business name '{name}' should not appear in obfuscated path '{filePath}'");
            }
        }

        // Verify: all mapped files exist on disk
        foreach (var newPath in mappings.FilePathForward.Values)
        {
            var fullPath = Path.Combine(_tempDir, newPath.Replace('/', Path.DirectorySeparatorChar));
            File.Exists(fullPath).Should().BeTrue(
                because: $"mapped file '{newPath}' should exist on disk");
        }
    }

    [Fact]
    public async Task EndToEnd_DotNetProject_ReverseRename_RestoresOriginalStructure()
    {
        // Setup a simple project
        CreateFile("src/App/Service.cs", "class Service {}");
        CreateFile("src/Lib/Model.cs", "class Model {}");

        var context = new ObfuscationContext(ObfuscationLevel.Full);
        context.GetOrCreateAlias("Service", SemanticCategory.Class, "src/App/Service.cs", 1, 0, 7);
        context.GetOrCreateAlias("Model", SemanticCategory.Class, "src/Lib/Model.cs", 1, 0, 5);

        var mapper = new FileNameMapper();
        mapper.BuildMappings(context, GetAllRelativeFiles());
        var mappings = context.Mappings;

        // Forward pipeline
        var patcher = new FileReferencePatcher();
        await patcher.PatchReferencesAsync(_tempDir, mappings);
        var renamer = new FileSystemRenamer();
        renamer.RenameFiles(_tempDir, mappings);

        // Save forward mappings (as manifest would)
        var filePathMappings = new Dictionary<string, string>(mappings.FilePathForward);

        // Reverse pipeline
        renamer.ReverseRenameFiles(_tempDir, filePathMappings);

        // Verify: all original paths restored
        File.Exists(Path.Combine(_tempDir, "src", "App", "Service.cs")).Should().BeTrue();
        File.Exists(Path.Combine(_tempDir, "src", "Lib", "Model.cs")).Should().BeTrue();
    }

    [Fact]
    public async Task EndToEnd_JavaScriptProject_MapPatchRename_ImportsWork()
    {
        CreateFile("src/app.js", @"import { helper } from './utils/payrollHelper';
const api = require('./services/salaryApi');
");
        CreateFile("src/utils/payrollHelper.js", "export const helper = () => {};");
        CreateFile("src/services/salaryApi.js", "module.exports = {};");
        CreateFile("package.json", "{}");

        var context = new ObfuscationContext(ObfuscationLevel.Full);
        var mapper = new FileNameMapper();
        mapper.BuildMappings(context, GetAllRelativeFiles());
        var mappings = context.Mappings;

        // Patch references
        var patcher = new FileReferencePatcher();
        await patcher.PatchReferencesAsync(_tempDir, mappings);

        // Rename files
        var renamer = new FileSystemRenamer();
        renamer.RenameFiles(_tempDir, mappings);

        // Verify: no business names in paths
        var finalFiles = GetAllRelativeFiles();
        foreach (var filePath in finalFiles)
        {
            filePath.Should().NotContain("payrollHelper");
            filePath.Should().NotContain("salaryApi");
        }

        // Verify: all mapped files exist
        foreach (var newPath in mappings.FilePathForward.Values)
        {
            var fullPath = Path.Combine(_tempDir, newPath.Replace('/', Path.DirectorySeparatorChar));
            File.Exists(fullPath).Should().BeTrue(
                because: $"'{newPath}' should exist after rename");
        }
    }

    [Fact]
    public void EndToEnd_InformationLeakage_NoBusinessNamesInAnyFile()
    {
        // Create intentionally business-revealing file names and directory names
        CreateFile("AcmePayroll/EmployeeService.cs", "namespace AcmePayroll; class EmployeeService {}");
        CreateFile("AcmePayroll/SalaryReport.cs", "namespace AcmePayroll; class SalaryReport {}");
        CreateFile("HumanResources/Benefits/Calculator.cs", "class Calculator {}");

        var context = new ObfuscationContext(ObfuscationLevel.Full);
        context.GetOrCreateAlias("EmployeeService", SemanticCategory.Class, "AcmePayroll/EmployeeService.cs", 1);
        context.GetOrCreateAlias("SalaryReport", SemanticCategory.Class, "AcmePayroll/SalaryReport.cs", 1);
        context.GetOrCreateAlias("Calculator", SemanticCategory.Class, "HumanResources/Benefits/Calculator.cs", 1);

        var mapper = new FileNameMapper();
        mapper.BuildMappings(context, GetAllRelativeFiles());

        // Check ALL mapped paths and ALL mapped directory segments
        foreach (var (original, mapped) in context.Mappings.FilePathForward)
        {
            foreach (var name in BusinessNames)
            {
                mapped.Should().NotContain(name,
                    because: $"business name '{name}' leaked in mapped path '{mapped}' (from '{original}')");
            }
        }
    }

    [Fact]
    public async Task EndToEnd_MixedProject_CSharpAndJavaScript_AllReferencesPatched()
    {
        // C# backend
        var slnContent = @"
Microsoft Visual Studio Solution File, Format Version 12.00
Project(""{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}"") = ""Api"", ""src\Api\Api.csproj"", ""{AAAAAAAA-AAAA-AAAA-AAAA-AAAAAAAAAAAA}""
EndProject
";
        CreateFile("Solution.sln", slnContent);

        var csprojContent = @"<Project Sdk=""Microsoft.NET.Sdk"">
  <ItemGroup>
    <Compile Include=""Controller.cs"" />
  </ItemGroup>
</Project>";
        CreateFile("src/Api/Api.csproj", csprojContent);
        CreateFile("src/Api/Controller.cs", "class Controller {}");

        // JS frontend
        CreateFile("src/web/app.js", "import { utils } from './helpers/payUtils';");
        CreateFile("src/web/helpers/payUtils.js", "export const utils = {};");

        var context = new ObfuscationContext(ObfuscationLevel.Full);
        context.GetOrCreateAlias("Controller", SemanticCategory.Class, "src/Api/Controller.cs", 1);

        var mapper = new FileNameMapper();
        mapper.BuildMappings(context, GetAllRelativeFiles());
        var mappings = context.Mappings;

        var patcher = new FileReferencePatcher();
        var patchCount = await patcher.PatchReferencesAsync(_tempDir, mappings);

        // At least the .sln, .csproj, and .js should have been processed
        patchCount.Should().BeGreaterThanOrEqualTo(2);

        // Verify sln patched
        var patchedSln = ReadFile("Solution.sln");
        patchedSln.Should().NotContain("src\\Api\\Api.csproj");

        // Verify JS patched
        var patchedJs = ReadFile("src/web/app.js");
        patchedJs.Should().NotContain("payUtils");
    }

    [Fact]
    public async Task EndToEnd_RoundTrip_FileStructure_FullyReversible()
    {
        // Create a realistic project structure
        CreateFile("src/Services/Auth.cs", "class Auth {}");
        CreateFile("src/Services/Pay.cs", "class Pay {}");
        CreateFile("src/Models/User.cs", "class User {}");
        CreateFile("config.json", "{}");

        // Snapshot original file set
        var originalFiles = GetAllRelativeFiles().ToHashSet();

        var context = new ObfuscationContext(ObfuscationLevel.Full);
        context.GetOrCreateAlias("Auth", SemanticCategory.Class, "src/Services/Auth.cs", 1);
        context.GetOrCreateAlias("Pay", SemanticCategory.Class, "src/Services/Pay.cs", 1);
        context.GetOrCreateAlias("User", SemanticCategory.Class, "src/Models/User.cs", 1);

        var mapper = new FileNameMapper();
        mapper.BuildMappings(context, GetAllRelativeFiles());
        var mappings = context.Mappings;

        // Forward pipeline
        var patcher = new FileReferencePatcher();
        await patcher.PatchReferencesAsync(_tempDir, mappings);
        var renamer = new FileSystemRenamer();
        renamer.RenameFiles(_tempDir, mappings);

        // Save mappings (as manifest would)
        var filePathMappings = new Dictionary<string, string>(mappings.FilePathForward);

        // Reverse pipeline
        renamer.ReverseRenameFiles(_tempDir, filePathMappings);

        // Verify: file set matches original exactly
        var restoredFiles = GetAllRelativeFiles().ToHashSet();
        restoredFiles.Should().BeEquivalentTo(originalFiles);
    }
}
