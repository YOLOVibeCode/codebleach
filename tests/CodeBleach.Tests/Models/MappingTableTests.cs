using CodeBleach.Core.Models;

namespace CodeBleach.Tests.Models;

public class MappingTableTests
{
    [Fact]
    public void GetOrCreateAlias_NewValue_CreatesAlias()
    {
        // Arrange
        var table = new MappingTable();
        
        // Act
        var alias = table.GetOrCreateAlias("ProductionDB", "SERVER");
        
        // Assert
        alias.Should().Be("SERVER_0");
        table.Forward.Should().ContainKey("ProductionDB");
        table.Reverse.Should().ContainKey("SERVER_0");
        table.Forward["ProductionDB"].Should().Be("SERVER_0");
        table.Reverse["SERVER_0"].Should().Be("ProductionDB");
    }
    
    [Fact]
    public void GetOrCreateAlias_ExistingValue_ReturnsSameAlias()
    {
        // Arrange
        var table = new MappingTable();
        table.GetOrCreateAlias("ProductionDB", "SERVER");
        
        // Act
        var alias = table.GetOrCreateAlias("ProductionDB", "SERVER");
        
        // Assert
        alias.Should().Be("SERVER_0");
        table.Counters["SERVER"].Should().Be(1); // Not incremented
    }
    
    [Fact]
    public void GetOrCreateAlias_MultipleValues_IncrementsCounter()
    {
        // Arrange
        var table = new MappingTable();
        
        // Act
        var alias1 = table.GetOrCreateAlias("ProductionDB", "SERVER");
        var alias2 = table.GetOrCreateAlias("StagingDB", "SERVER");
        
        // Assert
        alias1.Should().Be("SERVER_0");
        alias2.Should().Be("SERVER_1");
        table.Counters["SERVER"].Should().Be(2);
    }
    
    [Fact]
    public void GetOriginal_ExistingAlias_ReturnsOriginal()
    {
        // Arrange
        var table = new MappingTable();
        table.GetOrCreateAlias("ProductionDB", "SERVER");
        
        // Act
        var original = table.GetOriginal("SERVER_0");
        
        // Assert
        original.Should().Be("ProductionDB");
    }
    
    [Fact]
    public void GetOriginal_NonExistentAlias_ReturnsNull()
    {
        // Arrange
        var table = new MappingTable();
        
        // Act
        var original = table.GetOriginal("SERVER_99");
        
        // Assert
        original.Should().BeNull();
    }
    
    [Fact]
    public void HasMapping_ExistingValue_ReturnsTrue()
    {
        // Arrange
        var table = new MappingTable();
        table.GetOrCreateAlias("ProductionDB", "SERVER");
        
        // Act & Assert
        table.HasMapping("ProductionDB").Should().BeTrue();
    }
    
    [Fact]
    public void HasMapping_NonExistentValue_ReturnsFalse()
    {
        // Arrange
        var table = new MappingTable();
        
        // Act & Assert
        table.HasMapping("ProductionDB").Should().BeFalse();
    }
    
    [Fact]
    public void GetOrCreateAlias_DifferentPrefixes_UseSeparateCounters()
    {
        // Arrange
        var table = new MappingTable();
        
        // Act
        var serverAlias = table.GetOrCreateAlias("ProductionDB", "SERVER");
        var ipAlias = table.GetOrCreateAlias("192.168.1.100", "IP");
        
        // Assert
        serverAlias.Should().Be("SERVER_0");
        ipAlias.Should().Be("IP_0");
        table.Counters["SERVER"].Should().Be(1);
        table.Counters["IP"].Should().Be(1);
    }
}

