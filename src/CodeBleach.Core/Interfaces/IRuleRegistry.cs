using CodeBleach.Core.Models;

namespace CodeBleach.Core.Interfaces;

/// <summary>
/// Registry for sanitization rules.
/// </summary>
public interface IRuleRegistry
{
    /// <summary>Gets all active rules, ordered by priority.</summary>
    IReadOnlyList<SanitizationRule> GetActiveRules();
    
    /// <summary>Gets a rule by its ID.</summary>
    SanitizationRule? GetRule(string ruleId);
    
    /// <summary>Adds a custom rule.</summary>
    void AddRule(SanitizationRule rule);
    
    /// <summary>Disables a rule by ID.</summary>
    void DisableRule(string ruleId);
    
    /// <summary>Enables a rule by ID.</summary>
    void EnableRule(string ruleId);
}

