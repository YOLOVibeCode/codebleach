using CodeBleach.Core.Models;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

public class FileSystemRenamerTests : IDisposable
{
    private readonly string _tempDir;

    public FileSystemRenamerTests()
    {
        _tempDir = Path.Combine(Path.GetTempPath(), $"codebleach_test_{Guid.NewGuid():N}");
        Directory.CreateDirectory(_tempDir);
    }

    public void Dispose()
    {
        if (Directory.Exists(_tempDir))
        {
            Directory.Delete(_tempDir, recursive: true);
        }
    }

    private void CreateFile(string relativePath, string content = "test")
    {
        var fullPath = Path.Combine(_tempDir, relativePath);
        var dir = Path.GetDirectoryName(fullPath);
        if (dir != null) Directory.CreateDirectory(dir);
        File.WriteAllText(fullPath, content);
    }

    [Fact]
    public void RenameFiles_SingleFile_MovesCorrectly()
    {
        CreateFile("original.cs", "class Foo {}");
        var mappings = new MappingTable();
        mappings.FilePathForward["original.cs"] = "CLS_0.cs";
        mappings.FilePathReverse["CLS_0.cs"] = "original.cs";

        var renamer = new FileSystemRenamer();
        var count = renamer.RenameFiles(_tempDir, mappings);

        count.Should().Be(1);
        File.Exists(Path.Combine(_tempDir, "CLS_0.cs")).Should().BeTrue();
        File.Exists(Path.Combine(_tempDir, "original.cs")).Should().BeFalse();
        File.ReadAllText(Path.Combine(_tempDir, "CLS_0.cs")).Should().Be("class Foo {}");
    }

    [Fact]
    public void RenameFiles_WithDirectoryRename_CreatesNewDirs()
    {
        CreateFile("Payroll/Service.cs", "content1");
        CreateFile("Payroll/Model.cs", "content2");
        var mappings = new MappingTable();
        mappings.FilePathForward["Payroll/Service.cs"] = "DIR_0/FILE_0.cs";
        mappings.FilePathReverse["DIR_0/FILE_0.cs"] = "Payroll/Service.cs";
        mappings.FilePathForward["Payroll/Model.cs"] = "DIR_0/FILE_1.cs";
        mappings.FilePathReverse["DIR_0/FILE_1.cs"] = "Payroll/Model.cs";

        var renamer = new FileSystemRenamer();
        var count = renamer.RenameFiles(_tempDir, mappings);

        count.Should().Be(2);
        File.Exists(Path.Combine(_tempDir, "DIR_0", "FILE_0.cs")).Should().BeTrue();
        File.Exists(Path.Combine(_tempDir, "DIR_0", "FILE_1.cs")).Should().BeTrue();
        Directory.Exists(Path.Combine(_tempDir, "Payroll")).Should().BeFalse(); // cleaned up
    }

    [Fact]
    public void RenameFiles_EmptyMappings_ReturnsZero()
    {
        CreateFile("file.cs");
        var mappings = new MappingTable();

        var renamer = new FileSystemRenamer();
        var count = renamer.RenameFiles(_tempDir, mappings);

        count.Should().Be(0);
    }

    [Fact]
    public void RenameFiles_SamePath_SkipsRename()
    {
        CreateFile("src/file.cs", "content");
        var mappings = new MappingTable();
        mappings.FilePathForward["src/file.cs"] = "src/file.cs";
        mappings.FilePathReverse["src/file.cs"] = "src/file.cs";

        var renamer = new FileSystemRenamer();
        var count = renamer.RenameFiles(_tempDir, mappings);

        count.Should().Be(0);
        File.Exists(Path.Combine(_tempDir, "src", "file.cs")).Should().BeTrue();
    }

    [Fact]
    public void ReverseRenameFiles_RestoresOriginalPaths()
    {
        // Setup: files are at obfuscated paths
        CreateFile("DIR_0/FILE_0.cs", "content1");
        CreateFile("DIR_0/FILE_1.cs", "content2");

        var filePathMappings = new Dictionary<string, string>
        {
            ["Payroll/Service.cs"] = "DIR_0/FILE_0.cs",
            ["Payroll/Model.cs"] = "DIR_0/FILE_1.cs"
        };

        var renamer = new FileSystemRenamer();
        var count = renamer.ReverseRenameFiles(_tempDir, filePathMappings);

        count.Should().Be(2);
        File.Exists(Path.Combine(_tempDir, "Payroll", "Service.cs")).Should().BeTrue();
        File.Exists(Path.Combine(_tempDir, "Payroll", "Model.cs")).Should().BeTrue();
        File.ReadAllText(Path.Combine(_tempDir, "Payroll", "Service.cs")).Should().Be("content1");
    }

    [Fact]
    public void ReverseRenameFiles_EmptyMappings_ReturnsZero()
    {
        var renamer = new FileSystemRenamer();
        var count = renamer.ReverseRenameFiles(_tempDir, new Dictionary<string, string>());

        count.Should().Be(0);
    }

    [Fact]
    public void RenameFiles_PreservesFileContent()
    {
        var content = "using System;\nclass Employee { }";
        CreateFile("src/Services/Employee.cs", content);

        var mappings = new MappingTable();
        mappings.FilePathForward["src/Services/Employee.cs"] = "src/DIR_0/CLS_0.cs";
        mappings.FilePathReverse["src/DIR_0/CLS_0.cs"] = "src/Services/Employee.cs";

        var renamer = new FileSystemRenamer();
        renamer.RenameFiles(_tempDir, mappings);

        File.ReadAllText(Path.Combine(_tempDir, "src", "DIR_0", "CLS_0.cs")).Should().Be(content);
    }

    [Fact]
    public void RenameFiles_CleansEmptyDirectories()
    {
        CreateFile("A/B/file.cs", "content");
        var mappings = new MappingTable();
        mappings.FilePathForward["A/B/file.cs"] = "X/Y/file.cs";
        mappings.FilePathReverse["X/Y/file.cs"] = "A/B/file.cs";

        var renamer = new FileSystemRenamer();
        renamer.RenameFiles(_tempDir, mappings);

        Directory.Exists(Path.Combine(_tempDir, "A")).Should().BeFalse();
        Directory.Exists(Path.Combine(_tempDir, "X", "Y")).Should().BeTrue();
    }
}
