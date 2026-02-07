using CodeBleach.Core.Models;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

public class FileReferencePatcherSlnTests : IDisposable
{
    private readonly string _tempDir;

    public FileReferencePatcherSlnTests()
    {
        _tempDir = Path.Combine(Path.GetTempPath(), $"codebleach_sln_{Guid.NewGuid():N}");
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
    public async Task PatchSolutionFiles_SlnFile_UpdatesProjectPaths()
    {
        var slnContent = @"
Microsoft Visual Studio Solution File, Format Version 12.00
Project(""{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}"") = ""MyApp"", ""src\MyApp\MyApp.csproj"", ""{11111111-1111-1111-1111-111111111111}""
EndProject
Project(""{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}"") = ""MyLib"", ""src\MyLib\MyLib.csproj"", ""{22222222-2222-2222-2222-222222222222}""
EndProject
";
        CreateFile("MySolution.sln", slnContent);

        var mappings = new MappingTable();
        mappings.FilePathForward["MySolution.sln"] = "PROJ_0.sln";
        mappings.FilePathForward["src/MyApp/MyApp.csproj"] = "src/DIR_0/PROJ_1.csproj";
        mappings.FilePathForward["src/MyLib/MyLib.csproj"] = "src/DIR_1/PROJ_2.csproj";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("MySolution.sln");
        patched.Should().Contain("DIR_0");
        patched.Should().Contain("PROJ_1.csproj");
        patched.Should().Contain("DIR_1");
        patched.Should().Contain("PROJ_2.csproj");
        // Project names and GUIDs unchanged
        patched.Should().Contain("{11111111-1111-1111-1111-111111111111}");
        patched.Should().Contain("{22222222-2222-2222-2222-222222222222}");
    }

    [Fact]
    public async Task PatchSolutionFiles_SlnxFile_UpdatesPathAttribute()
    {
        var slnxContent = @"<Solution>
  <Project Path=""src/MyApp/MyApp.csproj"" />
  <Project Path=""src/MyLib/MyLib.csproj"" />
</Solution>";
        CreateFile("MySolution.slnx", slnxContent);

        var mappings = new MappingTable();
        mappings.FilePathForward["MySolution.slnx"] = "PROJ_0.slnx";
        mappings.FilePathForward["src/MyApp/MyApp.csproj"] = "src/DIR_0/PROJ_1.csproj";
        mappings.FilePathForward["src/MyLib/MyLib.csproj"] = "src/DIR_1/PROJ_2.csproj";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("MySolution.slnx");
        patched.Should().Contain("src/DIR_0/PROJ_1.csproj");
        patched.Should().Contain("src/DIR_1/PROJ_2.csproj");
        patched.Should().NotContain("MyApp");
        patched.Should().NotContain("MyLib");
    }

    [Fact]
    public async Task PatchSolutionFiles_SlnWithMultipleProjects_AllPatched()
    {
        var slnContent = @"
Microsoft Visual Studio Solution File, Format Version 12.00
Project(""{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}"") = ""App"", ""src\App\App.csproj"", ""{AAAAAAAA-AAAA-AAAA-AAAA-AAAAAAAAAAAA}""
EndProject
Project(""{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}"") = ""Core"", ""src\Core\Core.csproj"", ""{BBBBBBBB-BBBB-BBBB-BBBB-BBBBBBBBBBBB}""
EndProject
Project(""{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}"") = ""Data"", ""src\Data\Data.csproj"", ""{CCCCCCCC-CCCC-CCCC-CCCC-CCCCCCCCCCCC}""
EndProject
Project(""{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}"") = ""Tests"", ""tests\Tests\Tests.csproj"", ""{DDDDDDDD-DDDD-DDDD-DDDD-DDDDDDDDDDDD}""
EndProject
";
        CreateFile("Solution.sln", slnContent);

        var mappings = new MappingTable();
        mappings.FilePathForward["Solution.sln"] = "PROJ_0.sln";
        mappings.FilePathForward["src/App/App.csproj"] = "src/DIR_0/PROJ_1.csproj";
        mappings.FilePathForward["src/Core/Core.csproj"] = "src/DIR_1/PROJ_2.csproj";
        mappings.FilePathForward["src/Data/Data.csproj"] = "src/DIR_2/PROJ_3.csproj";
        // Tests project NOT mapped - should be left alone

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("Solution.sln");
        patched.Should().Contain("PROJ_1.csproj");
        patched.Should().Contain("PROJ_2.csproj");
        patched.Should().Contain("PROJ_3.csproj");
        // Unmapped Tests project path left unchanged
        patched.Should().Contain(@"tests\Tests\Tests.csproj");
    }

    [Fact]
    public async Task PatchSolutionFiles_SlnProjectNotInMappings_LeftUnchanged()
    {
        var slnContent = @"
Microsoft Visual Studio Solution File, Format Version 12.00
Project(""{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}"") = ""External"", ""external\Lib.csproj"", ""{EEEEEEEE-EEEE-EEEE-EEEE-EEEEEEEEEEEE}""
EndProject
";
        CreateFile("MySolution.sln", slnContent);

        var mappings = new MappingTable();
        mappings.FilePathForward["MySolution.sln"] = "PROJ_0.sln";
        // external\Lib.csproj NOT in forward mappings

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        // File was read but no changes made since no refs resolved
        count.Should().Be(0);
        var patched = ReadFile("MySolution.sln");
        patched.Should().Contain(@"external\Lib.csproj");
    }

    [Fact]
    public async Task PatchSolutionFiles_SlnFileItselfNotMapped_Skipped()
    {
        var slnContent = @"
Microsoft Visual Studio Solution File, Format Version 12.00
Project(""{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}"") = ""App"", ""src\App\App.csproj"", ""{FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF}""
EndProject
";
        CreateFile("MySolution.sln", slnContent);

        var mappings = new MappingTable();
        // Map a project but NOT the .sln file itself
        mappings.FilePathForward["src/App/App.csproj"] = "src/DIR_0/PROJ_0.csproj";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(0);
        var content = ReadFile("MySolution.sln");
        content.Should().Contain(@"src\App\App.csproj"); // unchanged
    }
}
