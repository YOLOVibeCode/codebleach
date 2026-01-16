using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using CodeBleach.Core.Rules;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

public class SanitizerTests
{
    private readonly IRuleRegistry _ruleRegistry;
    private readonly ISanitizer _sut;
    
    public SanitizerTests()
    {
        _ruleRegistry = new RuleRegistry();
        foreach (var rule in BuiltInRules.All)
            _ruleRegistry.AddRule(rule);
        
        _sut = new Sanitizer(_ruleRegistry);
    }
    
    // --- Basic Functionality ---
    
    [Fact]
    public void Sanitize_WithServerName_ReplacesWithAlias()
    {
        // Arrange
        var content = "SELECT * FROM ProductionDB.user_table";
        var mappings = new MappingTable();
        
        // Act
        var result = _sut.Sanitize(content, mappings);
        
        // Assert
        result.Content.Should().Contain("SERVER_0");
        result.WasSanitized.Should().BeTrue();
        result.Matches.Should().Contain(m => m.OriginalValue == "ProductionDB" && m.Alias == "SERVER_0");
    }
    
    [Fact]
    public void Sanitize_WithMultipleOccurrences_UsesSameAlias()
    {
        // Arrange
        var content = "USE ProductionDB;\nSELECT * FROM ProductionDB.data_table;";
        var mappings = new MappingTable();
        
        // Act
        var result = _sut.Sanitize(content, mappings);
        
        // Assert
        result.Content.Should().Contain("SERVER_0").And.NotContain("SERVER_1");
        var serverMatches = result.Matches.Where(m => m.OriginalValue == "ProductionDB").ToList();
        serverMatches.Should().HaveCount(2);
        serverMatches.Should().AllSatisfy(m => m.Alias.Should().Be("SERVER_0"));
    }
    
    [Theory]
    [InlineData("192.168.1.100", "IP")]
    [InlineData("10.0.0.50", "IP")]
    [InlineData("172.16.0.1", "IP")]
    public void Sanitize_WithPrivateIP_ReplacesWithAlias(string ip, string expectedPrefix)
    {
        // Arrange
        var content = $"Host: {ip}";
        var mappings = new MappingTable();
        
        // Act
        var result = _sut.Sanitize(content, mappings);
        
        // Assert
        result.Content.Should().StartWith($"Host: {expectedPrefix}_");
        result.WasSanitized.Should().BeTrue();
    }
    
    // --- Edge Cases ---
    
    [Fact]
    public void Sanitize_WithNoSensitiveData_ReturnsUnchanged()
    {
        // Arrange
        var content = "Hello, World!";
        var mappings = new MappingTable();
        
        // Act
        var result = _sut.Sanitize(content, mappings);
        
        // Assert
        result.Content.Should().Be("Hello, World!");
        result.WasSanitized.Should().BeFalse();
        result.Matches.Should().BeEmpty();
    }
    
    [Fact]
    public void Sanitize_WithEmptyContent_ReturnsEmpty()
    {
        // Arrange
        var content = "";
        var mappings = new MappingTable();
        
        // Act
        var result = _sut.Sanitize(content, mappings);
        
        // Assert
        result.Content.Should().BeEmpty();
        result.WasSanitized.Should().BeFalse();
    }
    
    [Fact]
    public void Sanitize_WithExistingMappings_ReusesAliases()
    {
        // Arrange
        var mappings = new MappingTable();
        mappings.GetOrCreateAlias("ProductionDB", "SERVER"); // Pre-populate
        
        var content = "Connect to ProductionDB";
        
        // Act
        var result = _sut.Sanitize(content, mappings);
        
        // Assert
        result.Content.Should().Be("Connect to SERVER_0");
    }
}

