using System.Text.RegularExpressions;
using CodeBleach.Core.Models;
using CodeBleach.Core.Rules;

namespace CodeBleach.Tests.Rules;

public class BuiltInRulesTests
{
    [Theory]
    [InlineData("ProductionDB", true)]
    [InlineData("StagingDB", true)]
    [InlineData("TestDB123", true)]
    [InlineData("mydb", false)] // lowercase, no match
    [InlineData("Database", false)] // doesn't end with DB
    public void ServerNamesRule_MatchesCorrectly(string input, bool shouldMatch)
    {
        // Arrange
        var rule = BuiltInRules.ServerNames;
        var regex = new Regex(rule.Pattern);
        
        // Act & Assert
        regex.IsMatch(input).Should().Be(shouldMatch);
    }
    
    [Theory]
    [InlineData("192.168.1.1", true)]
    [InlineData("192.168.255.255", true)]
    [InlineData("192.169.1.1", false)] // not 192.168.x.x
    [InlineData("8.8.8.8", false)] // public IP
    public void PrivateIp192Rule_MatchesCorrectly(string input, bool shouldMatch)
    {
        // Arrange
        var rule = BuiltInRules.PrivateIp192;
        var regex = new Regex(rule.Pattern);
        
        // Act & Assert
        regex.IsMatch(input).Should().Be(shouldMatch);
    }
    
    [Fact]
    public void AllRules_HaveUniqueIds()
    {
        // Act
        var ruleIds = BuiltInRules.All.Select(r => r.RuleId).ToList();
        
        // Assert
        ruleIds.Should().OnlyHaveUniqueItems();
    }
    
    [Fact]
    public void AllRules_HaveValidPatterns()
    {
        // Act & Assert
        foreach (var rule in BuiltInRules.All)
        {
            var action = () => new Regex(rule.Pattern);
            action.Should().NotThrow($"Rule '{rule.RuleId}' has invalid pattern");
        }
    }
    
    [Fact]
    public void AllRules_AreEnabledByDefault()
    {
        // Act & Assert
        foreach (var rule in BuiltInRules.All)
        {
            rule.Enabled.Should().BeTrue($"Rule '{rule.RuleId}' should be enabled by default");
        }
    }
}

