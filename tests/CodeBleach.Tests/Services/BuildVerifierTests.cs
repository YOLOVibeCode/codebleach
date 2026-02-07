using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

public class BuildVerifierTests : IDisposable
{
    private readonly string _tempDir;

    public BuildVerifierTests()
    {
        _tempDir = Path.Combine(Path.GetTempPath(), $"codebleach_verify_{Guid.NewGuid():N}");
        Directory.CreateDirectory(_tempDir);
    }

    public void Dispose()
    {
        if (Directory.Exists(_tempDir))
        {
            Directory.Delete(_tempDir, recursive: true);
        }
    }

    [Fact]
    public void DetectBuildSystem_SlnFile_ReturnsDotNetSolution()
    {
        File.WriteAllText(Path.Combine(_tempDir, "Test.sln"), "");
        var verifier = new BuildVerifier();

        var result = verifier.DetectBuildSystem(_tempDir);

        result.Should().Be(BuildVerifier.BuildSystem.DotNetSolution);
    }

    [Fact]
    public void DetectBuildSystem_SlnxFile_ReturnsDotNetSolution()
    {
        File.WriteAllText(Path.Combine(_tempDir, "Test.slnx"), "");
        var verifier = new BuildVerifier();

        var result = verifier.DetectBuildSystem(_tempDir);

        result.Should().Be(BuildVerifier.BuildSystem.DotNetSolution);
    }

    [Fact]
    public void DetectBuildSystem_CsprojFile_ReturnsDotNetProject()
    {
        var subDir = Path.Combine(_tempDir, "src");
        Directory.CreateDirectory(subDir);
        File.WriteAllText(Path.Combine(subDir, "Test.csproj"), "");
        var verifier = new BuildVerifier();

        var result = verifier.DetectBuildSystem(_tempDir);

        result.Should().Be(BuildVerifier.BuildSystem.DotNetProject);
    }

    [Fact]
    public void DetectBuildSystem_TsConfig_ReturnsTypeScript()
    {
        File.WriteAllText(Path.Combine(_tempDir, "tsconfig.json"), "{}");
        var verifier = new BuildVerifier();

        var result = verifier.DetectBuildSystem(_tempDir);

        result.Should().Be(BuildVerifier.BuildSystem.TypeScript);
    }

    [Fact]
    public void DetectBuildSystem_PackageJson_ReturnsJavaScript()
    {
        File.WriteAllText(Path.Combine(_tempDir, "package.json"), "{}");
        var verifier = new BuildVerifier();

        var result = verifier.DetectBuildSystem(_tempDir);

        result.Should().Be(BuildVerifier.BuildSystem.JavaScript);
    }

    [Fact]
    public void DetectBuildSystem_Makefile_ReturnsMake()
    {
        File.WriteAllText(Path.Combine(_tempDir, "Makefile"), "all:");
        var verifier = new BuildVerifier();

        var result = verifier.DetectBuildSystem(_tempDir);

        result.Should().Be(BuildVerifier.BuildSystem.Make);
    }

    [Fact]
    public void DetectBuildSystem_EmptyDirectory_ReturnsUnknown()
    {
        var verifier = new BuildVerifier();

        var result = verifier.DetectBuildSystem(_tempDir);

        result.Should().Be(BuildVerifier.BuildSystem.Unknown);
    }

    [Fact]
    public void DetectBuildSystem_PrioritizesSlnOverCsproj()
    {
        File.WriteAllText(Path.Combine(_tempDir, "Test.sln"), "");
        File.WriteAllText(Path.Combine(_tempDir, "Test.csproj"), "");
        var verifier = new BuildVerifier();

        var result = verifier.DetectBuildSystem(_tempDir);

        result.Should().Be(BuildVerifier.BuildSystem.DotNetSolution);
    }

    [Fact]
    public void GetBuildCommand_DotNetSolution_ReturnsDotnetBuild()
    {
        var verifier = new BuildVerifier();

        var (command, args) = verifier.GetBuildCommand(BuildVerifier.BuildSystem.DotNetSolution, _tempDir);

        command.Should().Be("dotnet");
        args.Should().Contain("build");
    }

    [Fact]
    public void GetBuildCommand_TypeScript_ReturnsTsc()
    {
        var verifier = new BuildVerifier();

        var (command, args) = verifier.GetBuildCommand(BuildVerifier.BuildSystem.TypeScript, _tempDir);

        command.Should().Be("npx");
        args.Should().Contain("tsc");
    }

    [Fact]
    public async Task VerifyAsync_UnknownBuildSystem_ReturnsFailure()
    {
        var verifier = new BuildVerifier();

        var result = await verifier.VerifyAsync(_tempDir);

        result.Success.Should().BeFalse();
        result.DetectedBuildSystem.Should().Be(BuildVerifier.BuildSystem.Unknown);
    }
}
