using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

public class FileReferencePatcherTests
{
    [Theory]
    [InlineData("src/A/../B/file.cs", "src/B/file.cs")]
    [InlineData("src/./B/file.cs", "src/B/file.cs")]
    [InlineData("A/B/C/../../D/file.cs", "A/D/file.cs")]
    [InlineData("file.cs", "file.cs")]
    [InlineData("../parent/file.cs", "../parent/file.cs")]
    public void NormalizePath_HandlesRelativeSegments(string input, string expected)
    {
        FileReferencePatcher.NormalizePath(input).Should().Be(expected);
    }

    [Theory]
    [InlineData("src/A", "src/B/file.cs", "../B/file.cs")]
    [InlineData("src/A/B", "src/A/C/file.cs", "../C/file.cs")]
    [InlineData("src", "src/file.cs", "file.cs")]
    [InlineData("A/B", "C/D/file.cs", "../../C/D/file.cs")]
    [InlineData("A", "A/file.cs", "file.cs")]
    public void ComputeRelativePath_ProducesCorrectResult(string fromDir, string toPath, string expected)
    {
        FileReferencePatcher.ComputeRelativePath(fromDir, toPath).Should().Be(expected);
    }

    [Fact]
    public void ComputeRelativePath_SameDirectory_NoUps()
    {
        var result = FileReferencePatcher.ComputeRelativePath("src/Services", "src/Services/file.cs");
        result.Should().Be("file.cs");
    }

    [Fact]
    public void ComputeRelativePath_DeeplyNested_CorrectUps()
    {
        var result = FileReferencePatcher.ComputeRelativePath("a/b/c/d", "a/x/y.cs");
        result.Should().Be("../../../x/y.cs");
    }

    [Fact]
    public void NormalizePath_BackslashNormalization()
    {
        FileReferencePatcher.NormalizePath(@"src\A\..\B\file.cs").Should().Be("src/B/file.cs");
    }
}
