namespace CodeBleach.Core.Models;

/// <summary>
/// Level of obfuscation to apply during sanitization.
/// </summary>
public enum ObfuscationLevel
{
    /// <summary>
    /// Level 1: Regex-based sanitization only (current CodeBleach v1 behavior).
    /// Replaces pattern-matched sensitive values (IPs, server names, connection strings).
    /// Does NOT rename identifiers. Fast, language-agnostic, low risk.
    /// </summary>
    Sanitize = 1,

    /// <summary>
    /// Level 2: Full identifier obfuscation using language-aware processors.
    /// Renames ALL user-defined identifiers (classes, methods, variables, tables, columns, paragraphs, etc.).
    /// Strips/replaces comments. Preserves syntax and semantic structure.
    /// Falls back to Level 1 for unsupported file types.
    /// </summary>
    Full = 2
}
