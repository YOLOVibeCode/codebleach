using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

public class RestorerTests
{
    private readonly IRestorer _sut = new Restorer();
    
    [Fact]
    public void Restore_WithAliases_ReplacesWithOriginals()
    {
        // Arrange
        var content = "SELECT * FROM SERVER_0.TABLE_0";
        var mappings = new MappingTable
        {
            Reverse = new Dictionary<string, string>
            {
                ["SERVER_0"] = "ProductionDB",
                ["TABLE_0"] = "users"
            }
        };
        
        // Act
        var result = _sut.Restore(content, mappings);
        
        // Assert
        result.Content.Should().Be("SELECT * FROM ProductionDB.users");
        result.WasRestored.Should().BeTrue();
        result.ReplacementCount.Should().Be(2);
    }
    
    [Fact]
    public void Restore_WithNoAliases_ReturnsUnchanged()
    {
        // Arrange
        var content = "Hello, World!";
        var mappings = new MappingTable();
        
        // Act
        var result = _sut.Restore(content, mappings);
        
        // Assert
        result.Content.Should().Be("Hello, World!");
        result.WasRestored.Should().BeFalse();
    }
    
    [Fact]
    public void Restore_WithLongerAliasFirst_RestoresCorrectly()
    {
        // Arrange - SERVER_10 should be replaced before SERVER_1
        var content = "Connect to SERVER_1 and SERVER_10";
        var mappings = new MappingTable
        {
            Reverse = new Dictionary<string, string>
            {
                ["SERVER_1"] = "StagingDB",
                ["SERVER_10"] = "ArchiveDB"
            }
        };
        
        // Act
        var result = _sut.Restore(content, mappings);
        
        // Assert
        result.Content.Should().Be("Connect to StagingDB and ArchiveDB");
        // Not "Connect to StagingDB and StagingDB0"
    }
    
    [Fact]
    public void Restore_WithUnmatchedAlias_ReportsIt()
    {
        // Arrange
        var content = "Connect to SERVER_99"; // Not in mappings
        var mappings = new MappingTable
        {
            Reverse = new Dictionary<string, string>
            {
                ["SERVER_0"] = "ProductionDB"
            }
        };
        
        // Act
        var result = _sut.Restore(content, mappings);
        
        // Assert
        result.Content.Should().Contain("SERVER_99"); // Unchanged
        result.UnmatchedAliases.Should().Contain("SERVER_99");
    }
    
    [Fact]
    public void Restore_WithEmptyContent_ReturnsEmpty()
    {
        // Arrange
        var content = "";
        var mappings = new MappingTable();
        
        // Act
        var result = _sut.Restore(content, mappings);
        
        // Assert
        result.Content.Should().BeEmpty();
        result.WasRestored.Should().BeFalse();
    }
}

