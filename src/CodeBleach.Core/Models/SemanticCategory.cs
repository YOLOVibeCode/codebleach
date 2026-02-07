namespace CodeBleach.Core.Models;

/// <summary>
/// Semantic category of an identifier, used for type-prefixed alias generation
/// and enhanced source map metadata.
/// </summary>
public enum SemanticCategory
{
    // ── .NET (C#, VB.NET, F#) ──────────────────────────────────────
    Namespace,
    Class,
    Interface,
    Method,
    Property,
    Field,
    Variable,
    Parameter,
    Event,
    Delegate,
    Enum,
    EnumMember,
    TypeParameter,
    Module,
    Record,
    UnionCase,

    // ── JavaScript ─────────────────────────────────────────────────
    ImportBinding,
    ExportBinding,
    // JS also uses: Class, Method, Variable, Parameter, Function (shared)

    // ── VBScript / VBA ─────────────────────────────────────────────
    UserDefinedType,
    // VBS/VBA also uses shared .NET categories

    // ── SQL (T-SQL, DB2, Oracle) ───────────────────────────────────
    Table,
    Column,
    Schema,
    Database,
    StoredProc,
    Function,
    CTE,
    TempTable,
    Cursor,
    SqlPackage,
    Sequence,

    // ── Oracle-specific ────────────────────────────────────────────
    OraclePackage,
    OraclePackageBody,
    MaterializedView,
    Synonym,

    // ── DB2-specific ───────────────────────────────────────────────
    Tablespace,
    Plan,
    Db2Package,
    Collection,
    StorageGroup,
    BufferPool,

    // ── COBOL ──────────────────────────────────────────────────────
    Program,
    Paragraph,
    Section,
    Copybook,
    CobolFile,
    CobolRecord,
    Condition,

    // ── JCL ────────────────────────────────────────────────────────
    Job,
    Step,
    DDName,
    Proc,
    Dataset,
    Symbol,
    Include,

    // ── File System ────────────────────────────────────────────────
    SourceFile,
    Directory,
    ProjectFile,

    // ── Shared / Cross-language ────────────────────────────────────
    StringLiteral,
    Comment,
    HostVariable,
    Server,
    IpAddress,
    Path,
    Hostname,
    ConnectionString,
    Generic
}
