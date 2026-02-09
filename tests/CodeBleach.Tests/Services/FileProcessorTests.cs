using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

public class FileProcessorTests : IDisposable
{
    private readonly string _tempDir;
    private readonly FileProcessor _processor = new();

    public FileProcessorTests()
    {
        _tempDir = Path.Combine(Path.GetTempPath(), $"codebleach_test_{Guid.NewGuid():N}");
        Directory.CreateDirectory(_tempDir);
    }

    public void Dispose()
    {
        if (Directory.Exists(_tempDir))
            Directory.Delete(_tempDir, recursive: true);
    }

    private void CreateFile(string relativePath, string content = "")
    {
        var fullPath = Path.Combine(_tempDir, relativePath);
        var dir = Path.GetDirectoryName(fullPath);
        if (dir != null) Directory.CreateDirectory(dir);
        File.WriteAllText(fullPath, content);
    }

    // ═══════════════════════════════════════════════════════════════════
    // GetFilesToProcess overload tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void GetFilesToProcess_IncludeExtensionless_True_IncludesExtensionlessFiles()
    {
        CreateFile("PAYROLL", "       IDENTIFICATION DIVISION.");
        CreateFile("RUNJOB", "//RUNJOB JOB");
        CreateFile("helper.cbl", "       IDENTIFICATION DIVISION.");

        var files = _processor.GetFilesToProcess(_tempDir, includeExtensionless: true).ToList();

        files.Should().HaveCount(3);
        files.Should().Contain(f => f.EndsWith("PAYROLL"));
        files.Should().Contain(f => f.EndsWith("RUNJOB"));
        files.Should().Contain(f => f.EndsWith("helper.cbl"));
    }

    [Fact]
    public void GetFilesToProcess_IncludeExtensionless_False_ExcludesExtensionlessFiles()
    {
        CreateFile("PAYROLL", "       IDENTIFICATION DIVISION.");
        CreateFile("RUNJOB", "//RUNJOB JOB");
        CreateFile("helper.cbl", "       IDENTIFICATION DIVISION.");

        var files = _processor.GetFilesToProcess(_tempDir, includeExtensionless: false).ToList();

        files.Should().HaveCount(1);
        files.Should().Contain(f => f.EndsWith("helper.cbl"));
        files.Should().NotContain(f => f.EndsWith("PAYROLL"));
    }

    [Fact]
    public void GetFilesToProcess_DefaultOverload_ExcludesExtensionlessFiles()
    {
        CreateFile("PAYROLL", "       IDENTIFICATION DIVISION.");
        CreateFile("helper.cbl", "       IDENTIFICATION DIVISION.");

        var files = _processor.GetFilesToProcess(_tempDir).ToList();

        files.Should().HaveCount(1);
        files.Should().Contain(f => f.EndsWith("helper.cbl"));
        files.Should().NotContain(f => f.EndsWith("PAYROLL"));
    }

    [Fact]
    public void ShouldProcess_ExtensionlessFile_ReturnsFalse()
    {
        _processor.ShouldProcess("PAYROLL").Should().BeFalse();
        _processor.ShouldProcess("RUNJOB").Should().BeFalse();
        _processor.ShouldProcess("EMPDATA").Should().BeFalse();
    }
}
