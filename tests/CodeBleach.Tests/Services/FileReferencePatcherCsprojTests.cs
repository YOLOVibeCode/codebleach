using CodeBleach.Core.Models;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

public class FileReferencePatcherCsprojTests : IDisposable
{
    private readonly string _tempDir;

    public FileReferencePatcherCsprojTests()
    {
        _tempDir = Path.Combine(Path.GetTempPath(), $"codebleach_csproj_{Guid.NewGuid():N}");
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
    public async Task PatchProjectFiles_FsprojCompileInclude_UpdatesPaths()
    {
        var fsprojContent = @"<Project Sdk=""Microsoft.NET.Sdk"">
  <ItemGroup>
    <Compile Include=""Domain\Types.fs"" />
    <Compile Include=""Program.fs"" />
  </ItemGroup>
</Project>";
        CreateFile("MyLib.fsproj", fsprojContent);

        var mappings = new MappingTable();
        mappings.FilePathForward["MyLib.fsproj"] = "PROJ_0.fsproj";
        mappings.FilePathForward["Domain/Types.fs"] = "DIR_0/FILE_0.fs";
        mappings.FilePathForward["Program.fs"] = "FILE_1.fs";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("MyLib.fsproj");
        patched.Should().Contain("DIR_0\\FILE_0.fs");
        patched.Should().Contain("FILE_1.fs");
        // Element order preserved: Types before Program
        var idx1 = patched.IndexOf("DIR_0", StringComparison.Ordinal);
        var idx2 = patched.IndexOf("FILE_1", StringComparison.Ordinal);
        idx1.Should().BeLessThan(idx2);
    }

    [Fact]
    public async Task PatchProjectFiles_CsprojProjectReference_UpdatesPath()
    {
        var csprojContent = @"<Project Sdk=""Microsoft.NET.Sdk"">
  <ItemGroup>
    <ProjectReference Include=""..\MyLib\MyLib.csproj"" />
  </ItemGroup>
</Project>";
        CreateFile("src/MyApp/MyApp.csproj", csprojContent);

        var mappings = new MappingTable();
        mappings.FilePathForward["src/MyApp/MyApp.csproj"] = "src/DIR_0/PROJ_0.csproj";
        mappings.FilePathForward["src/MyLib/MyLib.csproj"] = "src/DIR_1/PROJ_1.csproj";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("src/MyApp/MyApp.csproj");
        patched.Should().Contain("DIR_1");
        patched.Should().Contain("PROJ_1.csproj");
        patched.Should().NotContain("MyLib");
    }

    [Fact]
    public async Task PatchProjectFiles_VbprojProjectReference_UpdatesPath()
    {
        var vbprojContent = @"<Project Sdk=""Microsoft.NET.Sdk"">
  <ItemGroup>
    <ProjectReference Include=""..\SharedLib\SharedLib.vbproj"" />
  </ItemGroup>
</Project>";
        CreateFile("src/MyApp/MyApp.vbproj", vbprojContent);

        var mappings = new MappingTable();
        mappings.FilePathForward["src/MyApp/MyApp.vbproj"] = "src/DIR_0/PROJ_0.vbproj";
        mappings.FilePathForward["src/SharedLib/SharedLib.vbproj"] = "src/DIR_1/PROJ_1.vbproj";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("src/MyApp/MyApp.vbproj");
        patched.Should().Contain("DIR_1");
        patched.Should().Contain("PROJ_1.vbproj");
        patched.Should().NotContain("SharedLib");
    }

    [Fact]
    public async Task PatchProjectFiles_MultipleItemTypes_AllPatched()
    {
        var csprojContent = @"<Project Sdk=""Microsoft.NET.Sdk"">
  <ItemGroup>
    <None Include=""config.json"" />
    <Content Include=""wwwroot\index.html"" />
    <EmbeddedResource Include=""Resources\strings.resx"" />
    <Compile Include=""Service.cs"" />
  </ItemGroup>
</Project>";
        CreateFile("MyApp.csproj", csprojContent);

        var mappings = new MappingTable();
        mappings.FilePathForward["MyApp.csproj"] = "PROJ_0.csproj";
        mappings.FilePathForward["config.json"] = "FILE_0.json";
        mappings.FilePathForward["wwwroot/index.html"] = "wwwroot/FILE_1.html";
        mappings.FilePathForward["Resources/strings.resx"] = "DIR_0/FILE_2.resx";
        mappings.FilePathForward["Service.cs"] = "CLS_0.cs";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("MyApp.csproj");
        patched.Should().Contain("FILE_0.json");
        patched.Should().Contain("FILE_1.html");
        patched.Should().Contain("FILE_2.resx");
        patched.Should().Contain("CLS_0.cs");
    }

    [Fact]
    public async Task PatchProjectFiles_BackslashConvention_Preserved()
    {
        var csprojContent = @"<Project Sdk=""Microsoft.NET.Sdk"">
  <ItemGroup>
    <Compile Include=""Services\Employee.cs"" />
  </ItemGroup>
</Project>";
        CreateFile("MyApp.csproj", csprojContent);

        var mappings = new MappingTable();
        mappings.FilePathForward["MyApp.csproj"] = "PROJ_0.csproj";
        mappings.FilePathForward["Services/Employee.cs"] = "DIR_0/CLS_0.cs";

        var patcher = new FileReferencePatcher();
        await patcher.PatchReferencesAsync(_tempDir, mappings);

        var patched = ReadFile("MyApp.csproj");
        // Backslash convention should be preserved
        patched.Should().Contain(@"DIR_0\CLS_0.cs");
        patched.Should().NotContain("DIR_0/CLS_0.cs");
    }

    [Fact]
    public async Task PatchProjectFiles_ForwardSlashConvention_Preserved()
    {
        var csprojContent = @"<Project Sdk=""Microsoft.NET.Sdk"">
  <ItemGroup>
    <Compile Include=""Services/Employee.cs"" />
  </ItemGroup>
</Project>";
        CreateFile("MyApp.csproj", csprojContent);

        var mappings = new MappingTable();
        mappings.FilePathForward["MyApp.csproj"] = "PROJ_0.csproj";
        mappings.FilePathForward["Services/Employee.cs"] = "DIR_0/CLS_0.cs";

        var patcher = new FileReferencePatcher();
        await patcher.PatchReferencesAsync(_tempDir, mappings);

        var patched = ReadFile("MyApp.csproj");
        // Forward slash convention should be preserved
        patched.Should().Contain("DIR_0/CLS_0.cs");
        patched.Should().NotContain(@"DIR_0\CLS_0.cs");
    }

    [Fact]
    public async Task PatchProjectFiles_UnmappedReference_LeftUnchanged()
    {
        var csprojContent = @"<Project Sdk=""Microsoft.NET.Sdk"">
  <ItemGroup>
    <PackageReference Include=""Newtonsoft.Json"" Version=""13.0.3"" />
    <ProjectReference Include=""..\MyLib\MyLib.csproj"" />
  </ItemGroup>
</Project>";
        CreateFile("MyApp.csproj", csprojContent);

        var mappings = new MappingTable();
        mappings.FilePathForward["MyApp.csproj"] = "PROJ_0.csproj";
        // Only map the project itself, NOT MyLib - it should remain untouched

        var patcher = new FileReferencePatcher();
        await patcher.PatchReferencesAsync(_tempDir, mappings);

        var patched = ReadFile("MyApp.csproj");
        // PackageReference does not have a matching element name so is never touched
        patched.Should().Contain("Newtonsoft.Json");
        // ProjectReference points to unmapped path so is left as-is
        patched.Should().Contain(@"..\MyLib\MyLib.csproj");
    }

    [Fact]
    public async Task PatchProjectFiles_NoForwardMappings_ReturnsZero()
    {
        var csprojContent = @"<Project Sdk=""Microsoft.NET.Sdk"">
  <ItemGroup>
    <Compile Include=""Service.cs"" />
  </ItemGroup>
</Project>";
        CreateFile("MyApp.csproj", csprojContent);

        var mappings = new MappingTable();
        // No forward mappings at all

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(0);
    }

    [Fact]
    public async Task PatchProjectFiles_ProjectFileNotMapped_Skipped()
    {
        var csprojContent = @"<Project Sdk=""Microsoft.NET.Sdk"">
  <ItemGroup>
    <Compile Include=""Service.cs"" />
  </ItemGroup>
</Project>";
        CreateFile("MyApp.csproj", csprojContent);

        var mappings = new MappingTable();
        // Map a .cs file but NOT the .csproj itself
        mappings.FilePathForward["Service.cs"] = "CLS_0.cs";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        // csproj itself not in forward mappings, so it's skipped entirely
        count.Should().Be(0);
        var content = ReadFile("MyApp.csproj");
        content.Should().Contain("Service.cs"); // original reference untouched
    }
}
