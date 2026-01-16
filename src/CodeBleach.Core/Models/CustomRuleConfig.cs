using System.Text.Json.Serialization;

namespace CodeBleach.Core.Models;

/// <summary>
/// Configuration for custom sanitization rules loaded from file.
/// </summary>
public record CustomRuleConfig
{
    public const string DefaultFileName = ".codebleach-rules.json";
    
    [JsonPropertyName("rules")]
    public List<CustomRuleDefinition> Rules { get; init; } = [];
}

/// <summary>
/// Definition of a custom rule.
/// </summary>
public record CustomRuleDefinition
{
    [JsonPropertyName("ruleId")]
    public required string RuleId { get; init; }
    
    [JsonPropertyName("name")]
    public required string Name { get; init; }
    
    [JsonPropertyName("description")]
    public string? Description { get; init; }
    
    [JsonPropertyName("pattern")]
    public required string Pattern { get; init; }
    
    [JsonPropertyName("prefix")]
    public required string Prefix { get; init; }
    
    [JsonPropertyName("severity")]
    public RuleSeverity Severity { get; init; } = RuleSeverity.Medium;
    
    [JsonPropertyName("enabled")]
    public bool Enabled { get; init; } = true;
    
    [JsonPropertyName("exceptions")]
    public List<string> Exceptions { get; init; } = [];
    
    [JsonPropertyName("order")]
    public int Order { get; init; } = 100;
}

