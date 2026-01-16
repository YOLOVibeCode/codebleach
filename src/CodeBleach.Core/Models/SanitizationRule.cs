namespace CodeBleach.Core.Models;

/// <summary>
/// Defines a pattern for detecting and sanitizing sensitive data.
/// </summary>
public record SanitizationRule
{
    /// <summary>Unique identifier for the rule (e.g., "server_names")</summary>
    public required string RuleId { get; init; }
    
    /// <summary>Human-readable name</summary>
    public required string Name { get; init; }
    
    /// <summary>Description of what this rule detects</summary>
    public required string Description { get; init; }
    
    /// <summary>Regex pattern for detection</summary>
    public required string Pattern { get; init; }
    
    /// <summary>Alias prefix (e.g., "SERVER", "IP", "TABLE")</summary>
    public required string Prefix { get; init; }
    
    /// <summary>Severity level for reporting</summary>
    public required RuleSeverity Severity { get; init; }
    
    /// <summary>Whether this rule is active</summary>
    public bool Enabled { get; init; } = true;
    
    /// <summary>Values to exclude from matching</summary>
    public IReadOnlyList<string> Exceptions { get; init; } = [];
    
    /// <summary>Processing order (lower = earlier)</summary>
    public int Order { get; init; } = 100;
}

