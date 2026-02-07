using CodeBleach.Core.Models;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

public class FileReferencePatcherIntegrationTests : IDisposable
{
    private readonly string _tempDir;

    public FileReferencePatcherIntegrationTests()
    {
        _tempDir = Path.Combine(Path.GetTempPath(), $"codebleach_patchint_{Guid.NewGuid():N}");
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
        return File.ReadAllText(Path.Combine(_tempDir, relativePath.Replace('/', Path.DirectorySeparatorChar)));
    }

    [Fact]
    public async Task PatchReferencesAsync_MixedDotNetProject_PatchesBothSlnAndCsproj()
    {
        // Create .sln referencing two .csproj files
        var slnContent = @"
Microsoft Visual Studio Solution File, Format Version 12.00
Project(""{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}"") = ""App"", ""src\App\App.csproj"", ""{AAAAAAAA-AAAA-AAAA-AAAA-AAAAAAAAAAAA}""
EndProject
Project(""{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}"") = ""Core"", ""src\Core\Core.csproj"", ""{BBBBBBBB-BBBB-BBBB-BBBB-BBBBBBBBBBBB}""
EndProject
";
        CreateFile("Solution.sln", slnContent);

        var appCsproj = @"<Project Sdk=""Microsoft.NET.Sdk"">
  <ItemGroup>
    <ProjectReference Include=""..\Core\Core.csproj"" />
    <Compile Include=""Services\PayService.cs"" />
  </ItemGroup>
</Project>";
        CreateFile("src/App/App.csproj", appCsproj);

        var coreCsproj = @"<Project Sdk=""Microsoft.NET.Sdk"">
  <ItemGroup>
    <Compile Include=""Models\Employee.cs"" />
  </ItemGroup>
</Project>";
        CreateFile("src/Core/Core.csproj", coreCsproj);

        var mappings = new MappingTable();
        mappings.FilePathForward["Solution.sln"] = "PROJ_0.sln";
        mappings.FilePathForward["src/App/App.csproj"] = "src/DIR_0/PROJ_1.csproj";
        mappings.FilePathForward["src/Core/Core.csproj"] = "src/DIR_1/PROJ_2.csproj";
        mappings.FilePathForward["src/App/Services/PayService.cs"] = "src/DIR_0/DIR_2/CLS_0.cs";
        mappings.FilePathForward["src/Core/Models/Employee.cs"] = "src/DIR_1/DIR_3/CLS_1.cs";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        // .sln + 2 .csproj = 3 patched files (if all had changes)
        count.Should().BeGreaterThanOrEqualTo(2);

        var patchedSln = ReadFile("Solution.sln");
        patchedSln.Should().Contain("PROJ_1.csproj");
        patchedSln.Should().Contain("PROJ_2.csproj");

        var patchedApp = ReadFile("src/App/App.csproj");
        patchedApp.Should().Contain("PROJ_2.csproj");
        patchedApp.Should().Contain("CLS_0.cs");
    }

    [Fact]
    public async Task PatchReferencesAsync_JavaScriptProject_PatchesAllImportStyles()
    {
        CreateFile("src/app.js", @"import { a } from './utils/helper';
const b = require('./services/api');
const c = import('./lazy/module');
");
        CreateFile("src/utils/helper.js", "export const a = 1;");
        CreateFile("src/services/api.js", "module.exports = {};");
        CreateFile("src/lazy/module.js", "export default {};");

        var mappings = new MappingTable();
        mappings.FilePathForward["src/app.js"] = "src/FILE_0.js";
        mappings.FilePathForward["src/utils/helper.js"] = "src/DIR_0/FILE_1.js";
        mappings.FilePathForward["src/services/api.js"] = "src/DIR_1/FILE_2.js";
        mappings.FilePathForward["src/lazy/module.js"] = "src/DIR_2/FILE_3.js";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1); // one JS file patched

        var patched = ReadFile("src/app.js");
        patched.Should().Contain("FILE_1");
        patched.Should().Contain("FILE_2");
        patched.Should().Contain("FILE_3");
        patched.Should().NotContain("utils/helper");
        patched.Should().NotContain("services/api");
        patched.Should().NotContain("lazy/module");
    }

    [Fact]
    public async Task PatchReferencesAsync_EmptyMappings_ReturnsZero()
    {
        CreateFile("src/app.js", "import { a } from './helper';");
        CreateFile("src/App.csproj", @"<Project Sdk=""Microsoft.NET.Sdk""></Project>");

        var mappings = new MappingTable();

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(0);
    }

    [Fact]
    public async Task PatchReferencesAsync_NoMatchingFiles_ReturnsZero()
    {
        CreateFile("readme.txt", "Hello world");
        CreateFile("data.csv", "a,b,c");

        var mappings = new MappingTable();
        mappings.FilePathForward["readme.txt"] = "FILE_0.txt";
        mappings.FilePathForward["data.csv"] = "FILE_1.csv";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        // No .csproj, .sln, or .js files → nothing to patch
        count.Should().Be(0);
    }
}
