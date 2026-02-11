using System.Collections.Frozen;

namespace CodeBleach.Core.Models;

/// <summary>
/// Whitelist filter for selective obfuscation. When active, only in-scope processors
/// perform full obfuscation; out-of-scope processors either early-return or run in
/// delegation-only mode (parsing structure to delegate in-scope subregions).
/// </summary>
public sealed class ObfuscationScope
{
    /// <summary>
    /// Predefined groups that map to one or more processor IDs.
    /// </summary>
    public static readonly IReadOnlyDictionary<string, IReadOnlyList<string>> Groups =
        new Dictionary<string, IReadOnlyList<string>>(StringComparer.OrdinalIgnoreCase)
        {
            ["database"] = ["tsql", "db2sql", "oraclesql"],
            ["mainframe"] = ["cobol", "jcl", "mainframe-utility"],
            ["dotnet"] = ["csharp", "vbnet", "fsharp"],
            ["web"] = ["javascript"],
            ["scripting"] = ["vbscript"],
        }.AsReadOnly();

    /// <summary>All known processor IDs for validation.</summary>
    private static readonly FrozenSet<string> AllProcessorIds = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "tsql", "db2sql", "oraclesql",
        "csharp", "vbnet", "fsharp",
        "javascript",
        "cobol", "jcl", "mainframe-utility",
        "vbscript"
    }.ToFrozenSet(StringComparer.OrdinalIgnoreCase);

    private readonly FrozenSet<string> _inScopeProcessorIds;

    /// <summary>Whether this scope filters anything (false = everything in scope).</summary>
    public bool IsFiltered { get; }

    /// <summary>Raw specifiers as provided by the user, for manifest persistence.</summary>
    public IReadOnlyList<string> RawSpecifiers { get; }

    private ObfuscationScope(bool isFiltered, IReadOnlyList<string> rawSpecifiers, FrozenSet<string> inScopeProcessorIds)
    {
        IsFiltered = isFiltered;
        RawSpecifiers = rawSpecifiers;
        _inScopeProcessorIds = inScopeProcessorIds;
    }

    /// <summary>
    /// Creates an unfiltered scope where everything is in scope (default behavior).
    /// </summary>
    public static ObfuscationScope All() =>
        new(false, [], FrozenSet<string>.Empty);

    /// <summary>
    /// Parses a comma-separated string of group names and/or processor IDs.
    /// Returns <see cref="All"/> if the input is null or empty.
    /// </summary>
    /// <exception cref="ArgumentException">Thrown for unknown group names or processor IDs.</exception>
    public static ObfuscationScope Parse(string? csv)
    {
        if (string.IsNullOrWhiteSpace(csv))
            return All();

        var specifiers = csv.Split(',', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries);
        if (specifiers.Length == 0)
            return All();

        var resolved = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        var rawList = new List<string>(specifiers.Length);

        foreach (var specifier in specifiers)
        {
            rawList.Add(specifier);

            if (Groups.TryGetValue(specifier, out var groupIds))
            {
                foreach (var id in groupIds)
                    resolved.Add(id);
            }
            else if (AllProcessorIds.Contains(specifier))
            {
                resolved.Add(specifier);
            }
            else
            {
                var validOptions = string.Join(", ",
                    Groups.Keys.Order()
                        .Concat(AllProcessorIds.Order()));
                throw new ArgumentException(
                    $"Unknown scope specifier '{specifier}'. Valid options: {validOptions}");
            }
        }

        return new ObfuscationScope(true, rawList.AsReadOnly(), resolved.ToFrozenSet(StringComparer.OrdinalIgnoreCase));
    }

    /// <summary>
    /// Returns true if the given processor should perform full obfuscation.
    /// Always true when the scope is unfiltered.
    /// </summary>
    public bool IsInScope(string processorId)
    {
        if (!IsFiltered) return true;
        return _inScopeProcessorIds.Contains(processorId);
    }

    /// <summary>
    /// Returns true if the given processor is out of scope but should still parse
    /// structure and delegate in-scope subregions (e.g., EXEC SQL in COBOL when
    /// only database is in scope). Always false when the scope is unfiltered.
    /// </summary>
    public bool IsDelegationOnly(string processorId)
    {
        if (!IsFiltered) return false;
        return !_inScopeProcessorIds.Contains(processorId);
    }
}
