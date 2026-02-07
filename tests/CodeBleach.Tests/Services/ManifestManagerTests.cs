using CodeBleach.Core.Models;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

/// <summary>
/// Tests for ManifestManager: save/load round-trips, error handling,
/// and directory detection.
/// </summary>
public class ManifestManagerTests : IDisposable
{
    private readonly ManifestManager _manager = new();
    private readonly string _tempDir;

    public ManifestManagerTests()
    {
        _tempDir = Path.Combine(Path.GetTempPath(), $"codebleach_manifest_{Guid.NewGuid():N}");
        Directory.CreateDirectory(_tempDir);
    }

    public void Dispose()
    {
        if (Directory.Exists(_tempDir))
            Directory.Delete(_tempDir, recursive: true);
    }

    private static Manifest CreateTestManifest(string sourcePath = "/src", string destPath = "/dest")
    {
        return new Manifest
        {
            Version = Manifest.CurrentVersion,
            SourcePath = sourcePath,
            DestinationPath = destPath,
            CreatedAt = new DateTime(2025, 1, 15, 10, 30, 0, DateTimeKind.Utc),
            Mappings = new MappingTable
            {
                Forward = new Dictionary<string, string>
                {
                    ["CustomerService"] = "CLS_0",
                    ["CalculateTotal"] = "MTD_0"
                },
                Reverse = new Dictionary<string, string>
                {
                    ["CLS_0"] = "CustomerService",
                    ["MTD_0"] = "CalculateTotal"
                }
            },
            ProcessedFiles = new List<string> { "Program.cs", "Service.cs" },
            Stats = new SanitizationStats
            {
                TotalFiles = 5,
                ProcessedFiles = 2,
                SkippedFiles = 3,
                TotalReplacements = 12,
                UniqueValuesReplaced = 2,
                ProcessingTimeMs = 150
            },
            Level = ObfuscationLevel.Full,
            ProcessorsUsed = new List<string> { "csharp" }
        };
    }

    // ═══════════════════════════════════════════════════════════════════
    // Save / Load Round-Trip
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public async Task SaveAndLoad_RoundTrip_PreservesManifest()
    {
        var original = CreateTestManifest(_tempDir, _tempDir);

        await _manager.SaveManifestAsync(original, _tempDir);
        var loaded = await _manager.LoadManifestAsync(_tempDir);

        loaded.Should().NotBeNull();
        loaded!.Version.Should().Be(Manifest.CurrentVersion);
        loaded.SourcePath.Should().Be(_tempDir);
        loaded.Mappings.Forward.Should().ContainKey("CustomerService");
        loaded.Mappings.Reverse.Should().ContainKey("CLS_0");
        loaded.ProcessedFiles.Should().HaveCount(2);
        loaded.Stats.TotalReplacements.Should().Be(12);
        loaded.Stats.UniqueValuesReplaced.Should().Be(2);
    }

    // ═══════════════════════════════════════════════════════════════════
    // Error Handling
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public async Task LoadManifest_WhenNotExists_ReturnsNull()
    {
        var emptyDir = Path.Combine(_tempDir, "empty");
        Directory.CreateDirectory(emptyDir);

        var result = await _manager.LoadManifestAsync(emptyDir);

        result.Should().BeNull();
    }

    [Fact]
    public async Task LoadManifest_WithCorruptedJson_ReturnsNull()
    {
        var codebleachDir = Path.Combine(_tempDir, Manifest.DirectoryName);
        Directory.CreateDirectory(codebleachDir);
        await File.WriteAllTextAsync(
            Path.Combine(codebleachDir, Manifest.FileName),
            "{ this is not valid JSON !!! }");

        var result = await _manager.LoadManifestAsync(_tempDir);

        result.Should().BeNull();
    }

    // ═══════════════════════════════════════════════════════════════════
    // Directory Detection
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public async Task IsSanitizedDirectory_WhenManifestExists_ReturnsTrue()
    {
        var manifest = CreateTestManifest(_tempDir, _tempDir);
        await _manager.SaveManifestAsync(manifest, _tempDir);

        var result = _manager.IsSanitizedDirectory(_tempDir);

        result.Should().BeTrue();
    }

    [Fact]
    public void IsSanitizedDirectory_WhenNoManifest_ReturnsFalse()
    {
        var emptyDir = Path.Combine(_tempDir, "fresh");
        Directory.CreateDirectory(emptyDir);

        var result = _manager.IsSanitizedDirectory(emptyDir);

        result.Should().BeFalse();
    }
}
