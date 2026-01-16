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
/// The type of rule definition.
/// </summary>
[JsonConverter(typeof(JsonStringEnumConverter))]
public enum RuleType
{
    /// <summary>Inline regex pattern in the JSON</summary>
    Regex,
    
    /// <summary>Regex pattern loaded from external .regex file</summary>
    RegexFile,
    
    /// <summary>JavaScript function loaded from external .js file</summary>
    JavaScript
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
    
    /// <summary>
    /// The type of rule: "regex" (default), "regexFile", or "javascript"
    /// </summary>
    [JsonPropertyName("type")]
    public RuleType Type { get; init; } = RuleType.Regex;
    
    /// <summary>
    /// Inline regex pattern (used when type is "regex")
    /// </summary>
    [JsonPropertyName("pattern")]
    public string? Pattern { get; init; }
    
    /// <summary>
    /// Path to external pattern file (used when type is "regexFile")
    /// </summary>
    [JsonPropertyName("patternFile")]
    public string? PatternFile { get; init; }
    
    /// <summary>
    /// Path to JavaScript file (used when type is "javascript")
    /// </summary>
    [JsonPropertyName("scriptFile")]
    public string? ScriptFile { get; init; }
    
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

