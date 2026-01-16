using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using CodeBleach.Core.Rules;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Rules;

public class RuleRegistryTests
{
    private readonly IRuleRegistry _sut;
    
    public RuleRegistryTests()
    {
        _sut = new RuleRegistry();
    }
    
    [Fact]
    public void GetActiveRules_WithNoRules_ReturnsEmpty()
    {
        // Act
        var rules = _sut.GetActiveRules();
        
        // Assert
        rules.Should().BeEmpty();
    }
    
    [Fact]
    public void AddRule_ThenGetActiveRules_ReturnsRule()
    {
        // Arrange
        var rule = BuiltInRules.ServerNames;
        
        // Act
        _sut.AddRule(rule);
        var rules = _sut.GetActiveRules();
        
        // Assert
        rules.Should().ContainSingle();
        rules[0].RuleId.Should().Be("server_names");
    }
    
    [Fact]
    public void GetActiveRules_ReturnsRulesOrderedByOrder()
    {
        // Arrange
        var rule1 = BuiltInRules.ServerNames; // Order = 10
        var rule2 = BuiltInRules.PrivateIp192; // Order = 22
        
        // Act
        _sut.AddRule(rule2);
        _sut.AddRule(rule1);
        var rules = _sut.GetActiveRules();
        
        // Assert
        rules.Should().HaveCount(2);
        rules[0].Order.Should().BeLessThan(rules[1].Order);
    }
    
    [Fact]
    public void GetActiveRules_ExcludesDisabledRules()
    {
        // Arrange
        var rule = BuiltInRules.ServerNames;
        _sut.AddRule(rule);
        
        // Act
        _sut.DisableRule("server_names");
        var rules = _sut.GetActiveRules();
        
        // Assert
        rules.Should().BeEmpty();
    }
    
    [Fact]
    public void GetRule_WithExistingId_ReturnsRule()
    {
        // Arrange
        var rule = BuiltInRules.ServerNames;
        _sut.AddRule(rule);
        
        // Act
        var found = _sut.GetRule("server_names");
        
        // Assert
        found.Should().NotBeNull();
        found!.RuleId.Should().Be("server_names");
    }
    
    [Fact]
    public void GetRule_WithNonExistentId_ReturnsNull()
    {
        // Act
        var found = _sut.GetRule("nonexistent");
        
        // Assert
        found.Should().BeNull();
    }
    
    [Fact]
    public void EnableRule_ReEnablesDisabledRule()
    {
        // Arrange
        var rule = BuiltInRules.ServerNames;
        _sut.AddRule(rule);
        _sut.DisableRule("server_names");
        
        // Act
        _sut.EnableRule("server_names");
        var rules = _sut.GetActiveRules();
        
        // Assert
        rules.Should().ContainSingle();
    }
}

