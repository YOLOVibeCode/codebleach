namespace CodeBleach.Core.Models;

/// <summary>
/// Severity level for sanitization rules.
/// </summary>
public enum RuleSeverity
{
    /// <summary>Low severity - informational only</summary>
    Low,
    
    /// <summary>Medium severity - should be sanitized</summary>
    Medium,
    
    /// <summary>High severity - critical sensitive data</summary>
    High,
    
    /// <summary>Critical severity - must be sanitized</summary>
    Critical
}

