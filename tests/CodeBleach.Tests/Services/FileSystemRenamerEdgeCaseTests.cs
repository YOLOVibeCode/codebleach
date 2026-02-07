using CodeBleach.Core.Models;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

public class FileSystemRenamerEdgeCaseTests : IDisposable
{
    private readonly string _tempDir;

    public FileSystemRenamerEdgeCaseTests()
    {
        _tempDir = Path.Combine(Path.GetTempPath(), $"codebleach_renedge_{Guid.NewGuid():N}");
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
    public void RenameFiles_SourceFileMissing_SkipsGracefully()
    {
        // Only create one of two mapped files
        CreateFile("existing.cs", "content");

        var mappings = new MappingTable();
        mappings.FilePathForward["missing.cs"] = "FILE_0.cs";
        mappings.FilePathReverse["FILE_0.cs"] = "missing.cs";
        mappings.FilePathForward["existing.cs"] = "FILE_1.cs";
        mappings.FilePathReverse["FILE_1.cs"] = "existing.cs";

        var renamer = new FileSystemRenamer();
        var count = renamer.RenameFiles(_tempDir, mappings);

        // Only the existing file should be renamed
        count.Should().Be(1);
        File.Exists(Path.Combine(_tempDir, "FILE_1.cs")).Should().BeTrue();
    }

    [Fact]
    public void RenameFiles_PartialFailure_ContinuesWithRemainingFiles()
    {
        CreateFile("a.cs", "content a");
        // b.cs deliberately NOT created
        CreateFile("c.cs", "content c");

        var mappings = new MappingTable();
        mappings.FilePathForward["a.cs"] = "FILE_0.cs";
        mappings.FilePathReverse["FILE_0.cs"] = "a.cs";
        mappings.FilePathForward["b.cs"] = "FILE_1.cs"; // will be missing
        mappings.FilePathReverse["FILE_1.cs"] = "b.cs";
        mappings.FilePathForward["c.cs"] = "FILE_2.cs";
        mappings.FilePathReverse["FILE_2.cs"] = "c.cs";

        var renamer = new FileSystemRenamer();
        var count = renamer.RenameFiles(_tempDir, mappings);

        count.Should().Be(2);
        File.Exists(Path.Combine(_tempDir, "FILE_0.cs")).Should().BeTrue();
        File.Exists(Path.Combine(_tempDir, "FILE_2.cs")).Should().BeTrue();
    }

    [Fact]
    public void RenameFiles_DeeplyNestedNewPath_CreatesAllDirs()
    {
        CreateFile("FILE_0.cs", "content");

        var mappings = new MappingTable();
        mappings.FilePathForward["FILE_0.cs"] = "a/b/c/d/e/FILE_0.cs";
        mappings.FilePathReverse["a/b/c/d/e/FILE_0.cs"] = "FILE_0.cs";

        var renamer = new FileSystemRenamer();
        var count = renamer.RenameFiles(_tempDir, mappings);

        count.Should().Be(1);
        File.Exists(Path.Combine(_tempDir, "a", "b", "c", "d", "e", "FILE_0.cs")).Should().BeTrue();
    }

    [Fact]
    public void RenameFiles_FileMovedToRoot_CleansOriginalDirs()
    {
        CreateFile("a/b/c/file.cs", "content");

        var mappings = new MappingTable();
        mappings.FilePathForward["a/b/c/file.cs"] = "FILE_0.cs";
        mappings.FilePathReverse["FILE_0.cs"] = "a/b/c/file.cs";

        var renamer = new FileSystemRenamer();
        var count = renamer.RenameFiles(_tempDir, mappings);

        count.Should().Be(1);
        File.Exists(Path.Combine(_tempDir, "FILE_0.cs")).Should().BeTrue();
        // Original empty dirs should be cleaned up
        Directory.Exists(Path.Combine(_tempDir, "a")).Should().BeFalse();
    }

    [Fact]
    public void ReverseRename_MissingObfuscatedFile_SkipsGracefully()
    {
        // Only create one of two obfuscated files
        CreateFile("FILE_0.cs", "content");

        var filePathMappings = new Dictionary<string, string>
        {
            ["original1.cs"] = "FILE_0.cs",
            ["original2.cs"] = "FILE_1.cs" // FILE_1.cs doesn't exist
        };

        var renamer = new FileSystemRenamer();
        var count = renamer.ReverseRenameFiles(_tempDir, filePathMappings);

        // Only the existing file should be reversed
        count.Should().Be(1);
        File.Exists(Path.Combine(_tempDir, "original1.cs")).Should().BeTrue();
    }
}
