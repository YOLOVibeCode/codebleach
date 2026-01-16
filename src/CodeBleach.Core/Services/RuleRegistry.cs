using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;

namespace CodeBleach.Core.Services;

/// <summary>
/// Registry for managing sanitization rules.
/// </summary>
public sealed class RuleRegistry : IRuleRegistry
{
    private readonly Dictionary<string, SanitizationRule> _rules = new();
    
    public IReadOnlyList<SanitizationRule> GetActiveRules()
    {
        return _rules.Values
            .Where(r => r.Enabled)
            .OrderBy(r => r.Order)
            .ToList();
    }
    
    public SanitizationRule? GetRule(string ruleId)
    {
        return _rules.TryGetValue(ruleId, out var rule) ? rule : null;
    }
    
    public void AddRule(SanitizationRule rule)
    {
        _rules[rule.RuleId] = rule;
    }
    
    public void DisableRule(string ruleId)
    {
        if (_rules.TryGetValue(ruleId, out var rule))
        {
            _rules[ruleId] = rule with { Enabled = false };
        }
    }
    
    public void EnableRule(string ruleId)
    {
        if (_rules.TryGetValue(ruleId, out var rule))
        {
            _rules[ruleId] = rule with { Enabled = true };
        }
    }
}

