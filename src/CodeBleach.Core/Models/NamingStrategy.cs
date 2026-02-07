using System.Collections.Frozen;

namespace CodeBleach.Core.Models;

/// <summary>
/// Generates type-prefixed aliases (e.g., CLS_0, TBL_1, PARA_2) from semantic categories.
/// Ensures no collisions with language keywords in any supported language.
/// </summary>
public sealed class NamingStrategy
{
    private static readonly FrozenDictionary<SemanticCategory, string> PrefixMap = new Dictionary<SemanticCategory, string>
    {
        // .NET
        [SemanticCategory.Namespace] = "NS",
        [SemanticCategory.Class] = "CLS",
        [SemanticCategory.Interface] = "INTF",
        [SemanticCategory.Method] = "MTD",
        [SemanticCategory.Property] = "PROP",
        [SemanticCategory.Field] = "FLD",
        [SemanticCategory.Variable] = "VAR",
        [SemanticCategory.Parameter] = "PRM",
        [SemanticCategory.Event] = "EVT",
        [SemanticCategory.Delegate] = "DEL",
        [SemanticCategory.Enum] = "ENM",
        [SemanticCategory.EnumMember] = "ENMV",
        [SemanticCategory.TypeParameter] = "TP",
        [SemanticCategory.Module] = "MOD",
        [SemanticCategory.Record] = "REC",
        [SemanticCategory.UnionCase] = "UC",

        // JavaScript
        [SemanticCategory.ImportBinding] = "IMP",
        [SemanticCategory.ExportBinding] = "EXP",

        // VBScript/VBA
        [SemanticCategory.UserDefinedType] = "UDT",

        // SQL
        [SemanticCategory.Table] = "TBL",
        [SemanticCategory.Column] = "COL",
        [SemanticCategory.Schema] = "SCH",
        [SemanticCategory.Database] = "DB",
        [SemanticCategory.StoredProc] = "SP",
        [SemanticCategory.Function] = "FN",
        [SemanticCategory.CTE] = "CTE",
        [SemanticCategory.TempTable] = "TMP",
        [SemanticCategory.Cursor] = "CUR",
        [SemanticCategory.SqlPackage] = "SPKG",
        [SemanticCategory.Sequence] = "SEQ",

        // Oracle
        [SemanticCategory.OraclePackage] = "PKG",
        [SemanticCategory.OraclePackageBody] = "PKGB",
        [SemanticCategory.MaterializedView] = "MV",
        [SemanticCategory.Synonym] = "SYN",

        // DB2
        [SemanticCategory.Tablespace] = "TS",
        [SemanticCategory.Plan] = "PLN",
        [SemanticCategory.Db2Package] = "DPKG",
        [SemanticCategory.Collection] = "COLL",
        [SemanticCategory.StorageGroup] = "STOG",
        [SemanticCategory.BufferPool] = "BP",

        // COBOL
        [SemanticCategory.Program] = "PGM",
        [SemanticCategory.Paragraph] = "PARA",
        [SemanticCategory.Section] = "SEC",
        [SemanticCategory.Copybook] = "CPY",
        [SemanticCategory.CobolFile] = "FIL",
        [SemanticCategory.CobolRecord] = "CREC",
        [SemanticCategory.Condition] = "CND",

        // JCL
        [SemanticCategory.Job] = "JOB",
        [SemanticCategory.Step] = "STP",
        [SemanticCategory.DDName] = "DD",
        [SemanticCategory.Proc] = "PRC",
        [SemanticCategory.Dataset] = "DSN",
        [SemanticCategory.Symbol] = "SYM",
        [SemanticCategory.Include] = "INC",

        // File System
        [SemanticCategory.SourceFile] = "FILE",
        [SemanticCategory.Directory] = "DIR",
        [SemanticCategory.ProjectFile] = "PROJ",

        // Shared
        [SemanticCategory.StringLiteral] = "STR",
        [SemanticCategory.Comment] = "CMT",
        [SemanticCategory.HostVariable] = "HV",
        [SemanticCategory.Server] = "SERVER",
        [SemanticCategory.IpAddress] = "IP",
        [SemanticCategory.Path] = "PATH",
        [SemanticCategory.Hostname] = "HOST",
        [SemanticCategory.ConnectionString] = "CONNSTR",
        [SemanticCategory.Generic] = "GEN",
    }.ToFrozenDictionary();

    /// <summary>
    /// Gets the alias prefix for the given semantic category.
    /// </summary>
    public string GetPrefix(SemanticCategory category)
    {
        return PrefixMap.TryGetValue(category, out var prefix) ? prefix : "GEN";
    }

    /// <summary>
    /// Generates an alias from a prefix and counter (e.g., "CLS_0", "TBL_12").
    /// The PREFIX_N format never collides with keywords in any supported language.
    /// </summary>
    public string GenerateAlias(string prefix, int counter)
    {
        return $"{prefix}_{counter}";
    }
}
