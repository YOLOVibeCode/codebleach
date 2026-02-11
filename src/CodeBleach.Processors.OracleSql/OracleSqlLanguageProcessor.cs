using System.Collections.Frozen;
using System.Text;
using System.Text.RegularExpressions;
using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;

namespace CodeBleach.Processors.OracleSql;

// ═══════════════════════════════════════════════════════════════════════════════
// Token Types and Token Model
// ═══════════════════════════════════════════════════════════════════════════════

/// <summary>
/// Token types recognized by the Oracle SQL/PL-SQL tokenizer.
/// </summary>
internal enum OracleTokenType
{
    Keyword,
    Identifier,
    QuotedIdentifier,
    StringLiteral,
    NumberLiteral,
    DateLiteral,
    BindVariable,
    Comment,
    LineComment,
    Operator,
    Punctuation,
    Whitespace,
    NewLine,
    Dot,
    Semicolon,
    LeftParen,
    RightParen,
    Comma,
    Star,
    Unknown
}

/// <summary>
/// A single token produced by the Oracle SQL/PL-SQL tokenizer, carrying position information
/// for back-to-front replacement.
/// </summary>
internal sealed class OracleToken
{
    public required OracleTokenType Type { get; init; }
    public required string Value { get; init; }
    public required int StartOffset { get; init; }
    public required int EndOffset { get; init; }
    public required int Line { get; init; }

    /// <summary>
    /// The normalized (uppercased) value, used for case-insensitive keyword lookups.
    /// </summary>
    public string NormalizedValue => Value.ToUpperInvariant();

    public int Length => EndOffset - StartOffset;

    public override string ToString() => $"[{Type}] '{Value}' @{StartOffset}-{EndOffset} L{Line}";
}

// ═══════════════════════════════════════════════════════════════════════════════
// Replacement Record
// ═══════════════════════════════════════════════════════════════════════════════

/// <summary>
/// A single text replacement to apply to the source, collected during analysis
/// and applied back-to-front to preserve character offsets.
/// </summary>
internal sealed record OracleReplacement(int Offset, int Length, string NewValue);

// ═══════════════════════════════════════════════════════════════════════════════
// Tokenizer
// ═══════════════════════════════════════════════════════════════════════════════

/// <summary>
/// PL/SQL-aware tokenizer that produces positioned tokens from Oracle SQL and PL/SQL source text.
/// Handles single-line comments, multi-line comments (including optimizer hints), string literals,
/// quoted identifiers, bind variables, numbers, dot-separated identifiers, and PL/SQL delimiters.
/// </summary>
internal static class OracleTokenizer
{
    public static List<OracleToken> Tokenize(string source)
    {
        var tokens = new List<OracleToken>();
        var pos = 0;
        var line = 1;
        var length = source.Length;

        while (pos < length)
        {
            var ch = source[pos];

            // ── Newlines ─────────────────────────────────────────────────
            if (ch == '\r' || ch == '\n')
            {
                var start = pos;
                if (ch == '\r' && pos + 1 < length && source[pos + 1] == '\n')
                {
                    pos += 2;
                }
                else
                {
                    pos++;
                }
                tokens.Add(new OracleToken
                {
                    Type = OracleTokenType.NewLine,
                    Value = source[start..pos],
                    StartOffset = start,
                    EndOffset = pos,
                    Line = line
                });
                line++;
                continue;
            }

            // ── Whitespace ───────────────────────────────────────────────
            if (char.IsWhiteSpace(ch))
            {
                var start = pos;
                while (pos < length && source[pos] != '\r' && source[pos] != '\n' && char.IsWhiteSpace(source[pos]))
                {
                    pos++;
                }
                tokens.Add(new OracleToken
                {
                    Type = OracleTokenType.Whitespace,
                    Value = source[start..pos],
                    StartOffset = start,
                    EndOffset = pos,
                    Line = line
                });
                continue;
            }

            // ── Single-line comment (--) ─────────────────────────────────
            if (ch == '-' && pos + 1 < length && source[pos + 1] == '-')
            {
                var start = pos;
                pos += 2;
                while (pos < length && source[pos] != '\r' && source[pos] != '\n')
                {
                    pos++;
                }
                tokens.Add(new OracleToken
                {
                    Type = OracleTokenType.LineComment,
                    Value = source[start..pos],
                    StartOffset = start,
                    EndOffset = pos,
                    Line = line
                });
                continue;
            }

            // ── Multi-line comment / Optimizer hint (/* ... */) ──────────
            if (ch == '/' && pos + 1 < length && source[pos + 1] == '*')
            {
                var start = pos;
                pos += 2;
                while (pos + 1 < length && !(source[pos] == '*' && source[pos + 1] == '/'))
                {
                    if (source[pos] == '\n') line++;
                    else if (source[pos] == '\r')
                    {
                        line++;
                        if (pos + 1 < length && source[pos + 1] == '\n') pos++;
                    }
                    pos++;
                }
                if (pos + 1 < length)
                {
                    pos += 2; // skip */
                }
                else
                {
                    pos = length; // unterminated comment, consume rest
                }
                tokens.Add(new OracleToken
                {
                    Type = OracleTokenType.Comment,
                    Value = source[start..pos],
                    StartOffset = start,
                    EndOffset = pos,
                    Line = line
                });
                continue;
            }

            // ── String literal ('...') ───────────────────────────────────
            if (ch == '\'')
            {
                var start = pos;
                pos++; // skip opening '
                while (pos < length)
                {
                    if (source[pos] == '\'')
                    {
                        pos++;
                        // Oracle uses '' for escaped quote inside string
                        if (pos < length && source[pos] == '\'')
                        {
                            pos++; // skip second quote of ''
                            continue;
                        }
                        break; // end of string
                    }
                    if (source[pos] == '\n') line++;
                    else if (source[pos] == '\r')
                    {
                        line++;
                        if (pos + 1 < length && source[pos + 1] == '\n') pos++;
                    }
                    pos++;
                }
                tokens.Add(new OracleToken
                {
                    Type = OracleTokenType.StringLiteral,
                    Value = source[start..pos],
                    StartOffset = start,
                    EndOffset = pos,
                    Line = line
                });
                continue;
            }

            // ── Quoted identifier ("...") ────────────────────────────────
            if (ch == '"')
            {
                var start = pos;
                pos++; // skip opening "
                while (pos < length && source[pos] != '"')
                {
                    pos++;
                }
                if (pos < length) pos++; // skip closing "
                tokens.Add(new OracleToken
                {
                    Type = OracleTokenType.QuotedIdentifier,
                    Value = source[start..pos],
                    StartOffset = start,
                    EndOffset = pos,
                    Line = line
                });
                continue;
            }

            // ── Bind variable (:name) ────────────────────────────────────
            if (ch == ':' && pos + 1 < length && (char.IsLetter(source[pos + 1]) || source[pos + 1] == '_'))
            {
                var start = pos;
                pos++; // skip :
                while (pos < length && (char.IsLetterOrDigit(source[pos]) || source[pos] == '_'))
                {
                    pos++;
                }
                tokens.Add(new OracleToken
                {
                    Type = OracleTokenType.BindVariable,
                    Value = source[start..pos],
                    StartOffset = start,
                    EndOffset = pos,
                    Line = line
                });
                continue;
            }

            // ── Number literal ───────────────────────────────────────────
            if (char.IsDigit(ch) || (ch == '.' && pos + 1 < length && char.IsDigit(source[pos + 1])))
            {
                var start = pos;
                var hasDot = false;

                if (ch == '.')
                {
                    hasDot = true;
                    pos++;
                }

                while (pos < length && char.IsDigit(source[pos]))
                {
                    pos++;
                }

                if (!hasDot && pos < length && source[pos] == '.')
                {
                    hasDot = true;
                    pos++;
                    while (pos < length && char.IsDigit(source[pos]))
                    {
                        pos++;
                    }
                }

                // Scientific notation
                if (pos < length && (source[pos] == 'e' || source[pos] == 'E'))
                {
                    pos++;
                    if (pos < length && (source[pos] == '+' || source[pos] == '-'))
                    {
                        pos++;
                    }
                    while (pos < length && char.IsDigit(source[pos]))
                    {
                        pos++;
                    }
                }

                tokens.Add(new OracleToken
                {
                    Type = OracleTokenType.NumberLiteral,
                    Value = source[start..pos],
                    StartOffset = start,
                    EndOffset = pos,
                    Line = line
                });
                continue;
            }

            // ── Identifiers and keywords ─────────────────────────────────
            if (char.IsLetter(ch) || ch == '_' || ch == '#' || ch == '$')
            {
                var start = pos;
                while (pos < length && (char.IsLetterOrDigit(source[pos]) || source[pos] == '_' || source[pos] == '$' || source[pos] == '#'))
                {
                    pos++;
                }

                var value = source[start..pos];
                var upper = value.ToUpperInvariant();

                // Check for DATE/TIMESTAMP literal prefix (e.g., DATE '2024-01-01')
                if ((upper == "DATE" || upper == "TIMESTAMP") &&
                    pos < length && SkipWhitespace(source, pos, length) is int nextPos && nextPos < length && source[nextPos] == '\'')
                {
                    // This is a DATE literal keyword -- emit as keyword, the string follows
                    tokens.Add(new OracleToken
                    {
                        Type = OracleTokenType.Keyword,
                        Value = value,
                        StartOffset = start,
                        EndOffset = pos,
                        Line = line
                    });
                    continue;
                }

                var type = OracleReservedWords.IsKeyword(upper) ? OracleTokenType.Keyword : OracleTokenType.Identifier;
                tokens.Add(new OracleToken
                {
                    Type = type,
                    Value = value,
                    StartOffset = start,
                    EndOffset = pos,
                    Line = line
                });
                continue;
            }

            // ── Dot ──────────────────────────────────────────────────────
            if (ch == '.')
            {
                tokens.Add(new OracleToken
                {
                    Type = OracleTokenType.Dot,
                    Value = ".",
                    StartOffset = pos,
                    EndOffset = pos + 1,
                    Line = line
                });
                pos++;
                continue;
            }

            // ── Semicolon ────────────────────────────────────────────────
            if (ch == ';')
            {
                tokens.Add(new OracleToken
                {
                    Type = OracleTokenType.Semicolon,
                    Value = ";",
                    StartOffset = pos,
                    EndOffset = pos + 1,
                    Line = line
                });
                pos++;
                continue;
            }

            // ── Parentheses ──────────────────────────────────────────────
            if (ch == '(')
            {
                tokens.Add(new OracleToken
                {
                    Type = OracleTokenType.LeftParen,
                    Value = "(",
                    StartOffset = pos,
                    EndOffset = pos + 1,
                    Line = line
                });
                pos++;
                continue;
            }
            if (ch == ')')
            {
                tokens.Add(new OracleToken
                {
                    Type = OracleTokenType.RightParen,
                    Value = ")",
                    StartOffset = pos,
                    EndOffset = pos + 1,
                    Line = line
                });
                pos++;
                continue;
            }

            // ── Comma ────────────────────────────────────────────────────
            if (ch == ',')
            {
                tokens.Add(new OracleToken
                {
                    Type = OracleTokenType.Comma,
                    Value = ",",
                    StartOffset = pos,
                    EndOffset = pos + 1,
                    Line = line
                });
                pos++;
                continue;
            }

            // ── Star / Asterisk ──────────────────────────────────────────
            if (ch == '*')
            {
                tokens.Add(new OracleToken
                {
                    Type = OracleTokenType.Star,
                    Value = "*",
                    StartOffset = pos,
                    EndOffset = pos + 1,
                    Line = line
                });
                pos++;
                continue;
            }

            // ── Operators and other punctuation ──────────────────────────
            if (IsOperatorChar(ch))
            {
                var start = pos;
                // Two-char operators: !=, <>, <=, >=, :=, ||, =>
                if (pos + 1 < length)
                {
                    var twoChar = source.Substring(pos, 2);
                    if (twoChar is "!=" or "<>" or "<=" or ">=" or ":=" or "||" or "=>" or "**")
                    {
                        pos += 2;
                        tokens.Add(new OracleToken
                        {
                            Type = OracleTokenType.Operator,
                            Value = twoChar,
                            StartOffset = start,
                            EndOffset = pos,
                            Line = line
                        });
                        continue;
                    }
                }
                tokens.Add(new OracleToken
                {
                    Type = OracleTokenType.Operator,
                    Value = ch.ToString(),
                    StartOffset = pos,
                    EndOffset = pos + 1,
                    Line = line
                });
                pos++;
                continue;
            }

            // ── Anything else ────────────────────────────────────────────
            tokens.Add(new OracleToken
            {
                Type = OracleTokenType.Unknown,
                Value = ch.ToString(),
                StartOffset = pos,
                EndOffset = pos + 1,
                Line = line
            });
            pos++;
        }

        return tokens;
    }

    private static bool IsOperatorChar(char ch) =>
        ch is '=' or '<' or '>' or '!' or '+' or '-' or '/' or '|' or '%' or '&' or '~' or '^' or '@';

    private static int SkipWhitespace(string source, int pos, int length)
    {
        while (pos < length && char.IsWhiteSpace(source[pos]))
        {
            pos++;
        }
        return pos;
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Reserved Words, Built-in Functions, System Objects, Data Types, Exceptions
// ═══════════════════════════════════════════════════════════════════════════════

/// <summary>
/// Lookup tables for Oracle SQL and PL/SQL reserved words, built-in functions,
/// system objects, data types, and built-in exceptions. All identifiers in these
/// sets are preserved (never renamed) during obfuscation.
/// </summary>
internal static class OracleReservedWords
{
    /// <summary>
    /// Oracle SQL and PL/SQL keywords.
    /// </summary>
    private static readonly FrozenSet<string> Keywords = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "ACCESS", "ADD", "ALL", "ALTER", "AND", "ANY", "AS", "ASC", "AUDIT",
        "BETWEEN", "BY", "CHAR", "CHECK", "CLUSTER", "COLUMN", "COMMENT", "COMPRESS",
        "CONNECT", "CREATE", "CURRENT", "DATE", "DECIMAL", "DEFAULT", "DELETE", "DESC",
        "DISTINCT", "DROP", "ELSE", "EXCLUSIVE", "EXISTS", "FILE", "FLOAT", "FOR",
        "FROM", "GRANT", "GROUP", "HAVING", "IDENTIFIED", "IMMEDIATE", "IN", "INCREMENT",
        "INDEX", "INITIAL", "INSERT", "INTEGER", "INTERSECT", "INTO", "IS", "LEVEL",
        "LIKE", "LOCK", "LONG", "MAXEXTENTS", "MINUS", "MLSLABEL", "MODE", "MODIFY",
        "NOAUDIT", "NOCOMPRESS", "NOT", "NOWAIT", "NULL", "NUMBER", "OF", "OFFLINE",
        "ON", "ONLINE", "OPTION", "OR", "ORDER", "PCTFREE", "PRIOR", "PUBLIC", "RAW",
        "RENAME", "RESOURCE", "REVOKE", "ROW", "ROWID", "ROWNUM", "ROWS", "SELECT",
        "SESSION", "SET", "SHARE", "SIZE", "SMALLINT", "START", "SUCCESSFUL", "SYNONYM",
        "SYSDATE", "TABLE", "THEN", "TO", "TRIGGER", "UID", "UNION", "UNIQUE", "UPDATE",
        "USER", "VALIDATE", "VALUES", "VARCHAR", "VARCHAR2", "VIEW", "WHENEVER", "WHERE", "WITH",

        // PL/SQL specific
        "DECLARE", "BEGIN", "END", "EXCEPTION", "WHEN", "LOOP", "EXIT", "CONTINUE",
        "WHILE", "FORALL", "BULK", "COLLECT", "CURSOR", "OPEN", "FETCH", "CLOSE",
        "IF", "ELSIF", "CASE", "GOTO", "RETURN", "RAISE", "PRAGMA", "RECORD",
        "SUBTYPE", "TYPE", "REF", "OUT", "NOCOPY", "CONSTANT", "EXECUTE", "REPLACE",
        "PROCEDURE", "FUNCTION", "PACKAGE", "BODY", "AUTHID", "CURRENT_USER", "DEFINER",
        "DETERMINISTIC", "PIPELINED", "PARALLEL_ENABLE", "RESULT_CACHE",
        "AUTONOMOUS_TRANSACTION", "EXCEPTION_INIT", "RESTRICT_REFERENCES", "SERIALLY_REUSABLE",

        // Additional SQL keywords
        "JOIN", "INNER", "OUTER", "LEFT", "RIGHT", "FULL", "CROSS", "NATURAL",
        "USING", "PIVOT", "UNPIVOT", "MERGE", "MATCHED", "MINUS", "EXCEPT",
        "ROLLUP", "CUBE", "GROUPING", "SETS", "OVER", "PARTITION",
        "RANGE", "UNBOUNDED", "PRECEDING", "FOLLOWING", "CURRENT",
        "SEQUENCE", "NEXTVAL", "CURRVAL", "MATERIALIZED", "FORCE", "NOFORCE",
        "CONSTRAINT", "PRIMARY", "KEY", "FOREIGN", "REFERENCES", "CASCADE",
        "ENABLE", "DISABLE", "NOVALIDATE", "RELY", "TRUNCATE", "PURGE",
        "NOCACHE", "CACHE", "CYCLE", "NOCYCLE", "MINVALUE", "MAXVALUE",
        "NOMINVALUE", "NOMAXVALUE", "GLOBAL", "TEMPORARY", "PRESERVE",
        "COMMIT", "ROLLBACK", "SAVEPOINT", "WORK", "ONLY", "READ", "WRITE",
        "ISOLATION", "SERIALIZABLE", "AUTONOMOUS", "TRANSACTION",
        "STORAGE", "TABLESPACE", "LOGGING", "NOLOGGING", "PARALLEL",
        "NOPARALLEL", "COMPRESS", "NOCOMPRESS", "INITRANS", "MAXTRANS",
        "PCTUSED", "FREELISTS", "FREELIST", "GROUPS", "BUFFER_POOL",
        "KEEP", "RECYCLE", "FLASH_BACK", "EDITION", "EDITIONABLE", "NONEDITIONABLE",
        "SHARING", "METADATA", "DATA", "NONE", "OBJECT", "COMPILE",

        // Additional PL/SQL
        "PIPE", "LIMIT", "SAVE", "EXCEPTIONS", "INDICES", "BETWEEN", "BOUND",
        "RETURNING", "ROWCOUNT", "ISOPEN", "NOTFOUND", "FOUND", "BULK_ROWCOUNT",
        "FIRST", "LAST", "NEXT", "PRIOR", "COUNT", "EXTEND", "TRIM", "DELETE",
        "EXISTS", "LIMIT", "TABLE_NAME", "COLUMN_NAME"
    }.ToFrozenSet();

    /// <summary>
    /// Oracle built-in functions (aggregate, scalar, analytic, conversion, etc.).
    /// </summary>
    private static readonly FrozenSet<string> BuiltInFunctions = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "ABS", "ACOS", "ADD_MONTHS", "ASCII", "ASIN", "ATAN", "ATAN2", "AVG",
        "CEIL", "CHR", "COALESCE", "CONCAT", "CONVERT", "COS", "COSH", "COUNT",
        "CURRENT_DATE", "CURRENT_TIMESTAMP", "DECODE", "DUMP", "EXP", "EXTRACT",
        "FLOOR", "GREATEST", "HEXTORAW", "INITCAP", "INSTR", "LAST_DAY", "LENGTH",
        "LN", "LOG", "LOWER", "LPAD", "LTRIM", "MAX", "MIN", "MOD", "MONTHS_BETWEEN",
        "NEW_TIME", "NEXT_DAY", "NLS_INITCAP", "NLS_LOWER", "NLS_UPPER",
        "NVL", "NVL2", "NULLIF", "POWER", "RAWTOHEX", "REPLACE", "ROUND",
        "RPAD", "RTRIM", "SIGN", "SIN", "SINH", "SQRT", "STDDEV", "SUBSTR",
        "SUM", "SYS_CONTEXT", "SYS_GUID", "SYSTIMESTAMP",
        "TAN", "TANH", "TO_CHAR", "TO_CLOB", "TO_DATE", "TO_LOB", "TO_MULTI_BYTE",
        "TO_NUMBER", "TO_SINGLE_BYTE", "TO_TIMESTAMP", "TO_TIMESTAMP_TZ",
        "TRANSLATE", "TRIM", "TRUNC", "UPPER", "USERENV", "VARIANCE", "VSIZE",
        "LISTAGG", "LAG", "LEAD", "FIRST_VALUE", "LAST_VALUE",
        "ROW_NUMBER", "RANK", "DENSE_RANK", "NTILE",
        "XMLAGG", "XMLELEMENT", "XMLFOREST",
        "REGEXP_LIKE", "REGEXP_REPLACE", "REGEXP_SUBSTR", "REGEXP_INSTR", "REGEXP_COUNT",
        "JSON_VALUE", "JSON_QUERY", "JSON_TABLE", "JSON_EXISTS", "JSON_OBJECT", "JSON_ARRAY",
        "CAST", "TREAT", "MULTISET", "CARDINALITY", "COLLECT", "POWERMULTISET",
        "EMPTY_CLOB", "EMPTY_BLOB", "BFILENAME", "ROWIDTOCHAR", "CHARTOROWID",
        "COMPOSE", "DECOMPOSE", "NANVL", "REMAINDER", "WIDTH_BUCKET",
        "BIN_TO_NUM", "LNNVL", "MEDIAN", "STATS_MODE", "PERCENTILE_CONT", "PERCENTILE_DISC",
        "CUME_DIST", "PERCENT_RANK", "RATIO_TO_REPORT", "CORR", "COVAR_POP", "COVAR_SAMP",
        "REGR_SLOPE", "REGR_INTERCEPT", "REGR_COUNT", "REGR_R2", "REGR_AVGX",
        "REGR_AVGY", "REGR_SXX", "REGR_SYY", "REGR_SXY",
        "ORA_HASH", "STANDARD_HASH", "DBMS_RANDOM"
    }.ToFrozenSet();

    /// <summary>
    /// Oracle system objects (schemas, packages, dictionary views, dynamic performance views).
    /// </summary>
    private static readonly FrozenSet<string> SystemObjects = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "SYS", "SYSTEM", "DUAL",
        "DBMS_OUTPUT", "DBMS_SQL", "DBMS_LOB", "DBMS_METADATA", "DBMS_STATS",
        "DBMS_RANDOM", "DBMS_UTILITY", "DBMS_SCHEDULER", "DBMS_LOCK", "DBMS_CRYPTO",
        "DBMS_XMLGEN", "DBMS_PIPE", "DBMS_ALERT", "DBMS_SESSION", "DBMS_APPLICATION_INFO",
        "DBMS_PROFILER", "DBMS_FLASHBACK", "DBMS_REDEFINITION", "DBMS_SPACE",
        "DBMS_ROWID", "DBMS_OBFUSCATION_TOOLKIT", "DBMS_XMLDOM", "DBMS_XMLPARSER",
        "UTL_FILE", "UTL_HTTP", "UTL_SMTP", "UTL_RAW", "UTL_ENCODE",
        "UTL_TCP", "UTL_URL", "UTL_I18N", "UTL_LMS", "UTL_CALL_STACK",
        "DBA_TABLES", "DBA_VIEWS", "DBA_OBJECTS", "DBA_USERS", "DBA_TAB_COLUMNS",
        "DBA_INDEXES", "DBA_CONSTRAINTS", "DBA_TRIGGERS", "DBA_SEQUENCES",
        "DBA_SYNONYMS", "DBA_SOURCE", "DBA_DEPENDENCIES", "DBA_TAB_PARTITIONS",
        "ALL_TABLES", "ALL_VIEWS", "ALL_OBJECTS", "ALL_TAB_COLUMNS",
        "ALL_INDEXES", "ALL_CONSTRAINTS", "ALL_TRIGGERS", "ALL_SEQUENCES",
        "ALL_SYNONYMS", "ALL_SOURCE", "ALL_DEPENDENCIES",
        "USER_TABLES", "USER_VIEWS", "USER_OBJECTS", "USER_TAB_COLUMNS",
        "USER_INDEXES", "USER_CONSTRAINTS", "USER_TRIGGERS", "USER_SEQUENCES",
        "USER_SYNONYMS", "USER_SOURCE", "USER_DEPENDENCIES",
        // V$ views -- we store without V$ prefix and match specially
        "V$SESSION", "V$SQL", "V$LOCK", "V$PROCESS", "V$INSTANCE", "V$DATABASE",
        "V$SQLAREA", "V$MYSTAT", "V$SYSSTAT", "V$PARAMETER"
    }.ToFrozenSet();

    /// <summary>
    /// Oracle data types (SQL and PL/SQL).
    /// </summary>
    private static readonly FrozenSet<string> DataTypes = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "VARCHAR2", "NVARCHAR2", "NUMBER", "FLOAT", "LONG", "DATE", "BINARY_FLOAT",
        "BINARY_DOUBLE", "TIMESTAMP", "INTERVAL", "RAW", "ROWID", "UROWID",
        "CHAR", "NCHAR", "CLOB", "NCLOB", "BLOB", "BFILE", "XMLTYPE",
        "SDO_GEOMETRY", "ANYDATA", "ANYTYPE", "ANYDATASET",
        "PLS_INTEGER", "BINARY_INTEGER", "NATURAL", "NATURALN", "POSITIVE",
        "POSITIVEN", "SIGNTYPE", "BOOLEAN", "SYS_REFCURSOR",
        "INTEGER", "SMALLINT", "REAL", "DECIMAL", "NUMERIC", "DEC",
        "DOUBLE", "INT", "STRING"
    }.ToFrozenSet();

    /// <summary>
    /// Oracle built-in exceptions (never rename these in WHEN clauses).
    /// </summary>
    private static readonly FrozenSet<string> BuiltInExceptions = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "ACCESS_INTO_NULL", "CASE_NOT_FOUND", "COLLECTION_IS_NULL", "CURSOR_ALREADY_OPEN",
        "DUP_VAL_ON_INDEX", "INVALID_CURSOR", "INVALID_NUMBER", "LOGIN_DENIED",
        "NO_DATA_FOUND", "NO_DATA_NEEDED", "NOT_LOGGED_ON", "PROGRAM_ERROR",
        "ROWTYPE_MISMATCH", "SELF_IS_NULL", "STORAGE_ERROR", "SUBSCRIPT_BEYOND_COUNT",
        "SUBSCRIPT_OUTSIDE_LIMIT", "SYS_INVALID_ROWID", "TIMEOUT_ON_RESOURCE",
        "TOO_MANY_ROWS", "VALUE_ERROR", "ZERO_DIVIDE", "OTHERS"
    }.ToFrozenSet();

    /// <summary>
    /// PL/SQL pseudo-columns and cursor attributes (never rename).
    /// </summary>
    private static readonly FrozenSet<string> PseudoColumns = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "ROWNUM", "ROWID", "LEVEL", "SYSDATE", "SYSTIMESTAMP", "USER",
        "UID", "CURRENT_DATE", "CURRENT_TIMESTAMP", "SESSIONTIMEZONE",
        "DBTIMEZONE", "LOCALTIMESTAMP"
    }.ToFrozenSet();

    public static bool IsKeyword(string upper) => Keywords.Contains(upper);

    public static bool IsBuiltInFunction(string name) => BuiltInFunctions.Contains(name);

    public static bool IsSystemObject(string name) =>
        SystemObjects.Contains(name) ||
        name.StartsWith("V$", StringComparison.OrdinalIgnoreCase) ||
        name.StartsWith("GV$", StringComparison.OrdinalIgnoreCase) ||
        name.StartsWith("DBA_", StringComparison.OrdinalIgnoreCase) ||
        name.StartsWith("ALL_", StringComparison.OrdinalIgnoreCase) ||
        name.StartsWith("USER_", StringComparison.OrdinalIgnoreCase) ||
        name.StartsWith("DBMS_", StringComparison.OrdinalIgnoreCase) ||
        name.StartsWith("UTL_", StringComparison.OrdinalIgnoreCase);

    public static bool IsDataType(string name) => DataTypes.Contains(name);

    public static bool IsBuiltInException(string name) => BuiltInExceptions.Contains(name);

    public static bool IsPseudoColumn(string name) => PseudoColumns.Contains(name);

    /// <summary>
    /// Returns true if the identifier should be preserved (never renamed).
    /// This is the master "do not touch" check.
    /// </summary>
    public static bool ShouldPreserve(string name)
    {
        return IsKeyword(name) ||
               IsBuiltInFunction(name) ||
               IsSystemObject(name) ||
               IsDataType(name) ||
               IsBuiltInException(name) ||
               IsPseudoColumn(name);
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Main Processor
// ═══════════════════════════════════════════════════════════════════════════════

/// <summary>
/// Language processor for Oracle SQL and PL/SQL files. Uses a custom tokenizer
/// and a two-pass analysis approach (discovery + replacement) to obfuscate
/// user-defined identifiers while preserving Oracle keywords, built-in functions,
/// system objects, data types, and syntactic structure.
/// </summary>
public sealed class OracleSqlLanguageProcessor : ILanguageProcessor
{
    public string ProcessorId => "oraclesql";
    public string DisplayName => "Oracle SQL/PL-SQL";

    public IReadOnlySet<string> SupportedExtensions { get; } = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        ".pls", ".plb", ".pks", ".pkb", ".fnc", ".prc", ".trg"
    }.ToFrozenSet();

    public int Priority => 10;

    /// <summary>
    /// Regex matching the alias format PREFIX_N produced by the NamingStrategy.
    /// </summary>
    private static readonly Regex AliasPattern = new(@"^[A-Z]+_\d+$", RegexOptions.Compiled);

    public bool CanProcess(string filePath, string content)
    {
        var ext = Path.GetExtension(filePath);
        if (SupportedExtensions.Contains(ext))
            return true;

        // Also accept .sql files with Oracle/PL-SQL indicators
        if (string.Equals(ext, ".sql", StringComparison.OrdinalIgnoreCase))
        {
            return ContainsOracleIndicators(content);
        }

        return false;
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Obfuscate
    // ═══════════════════════════════════════════════════════════════════════

    public LanguageProcessingResult Obfuscate(string content, ObfuscationContext context, string? filePath = null)
    {
        if (string.IsNullOrWhiteSpace(content) || context.Scope.IsDelegationOnly(ProcessorId))
        {
            return NoChange(content);
        }

        var warnings = new List<string>();

        try
        {
            var tokens = OracleTokenizer.Tokenize(content);
            var significantTokens = tokens
                .Where(t => t.Type != OracleTokenType.Whitespace && t.Type != OracleTokenType.NewLine)
                .ToList();

            // ── Pass 1: Discovery ────────────────────────────────────────
            // Analyze the token stream to find declarations and build
            // a set of known identifiers with their semantic categories.
            var discoveredIdentifiers = new Dictionary<string, SemanticCategory>(StringComparer.OrdinalIgnoreCase);
            DiscoverDeclarations(significantTokens, context, filePath, discoveredIdentifiers, warnings);

            // ── Pass 2: Replacement ──────────────────────────────────────
            // Walk all tokens and collect replacements for identifiers,
            // comments, string literals, bind variables, and quoted identifiers.
            var replacements = new List<OracleReplacement>();
            CollectReplacements(tokens, significantTokens, context, filePath, discoveredIdentifiers, replacements, warnings);

            // Deduplicate and sort descending by offset
            var deduped = DeduplicateReplacements(replacements);
            var sorted = deduped.OrderByDescending(r => r.Offset).ToList();

            // Apply replacements back-to-front
            var result = new StringBuilder(content);
            foreach (var replacement in sorted)
            {
                if (replacement.Offset >= 0 && replacement.Offset + replacement.Length <= result.Length)
                {
                    result.Remove(replacement.Offset, replacement.Length);
                    result.Insert(replacement.Offset, replacement.NewValue);
                }
            }

            var finalContent = result.ToString();

            if (filePath != null)
            {
                context.RecordFileProcessing(filePath, ProcessorId, sorted.Count);
            }

            return new LanguageProcessingResult
            {
                Content = finalContent,
                WasTransformed = sorted.Count > 0,
                ReplacementCount = sorted.Count,
                ProcessorId = ProcessorId,
                Warnings = warnings
            };
        }
        catch (Exception ex)
        {
            warnings.Add($"Oracle SQL processor failed, returning original content: {ex.Message}");
            return new LanguageProcessingResult
            {
                Content = content,
                WasTransformed = false,
                ReplacementCount = 0,
                ProcessorId = ProcessorId,
                Warnings = warnings
            };
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Deobfuscate
    // ═══════════════════════════════════════════════════════════════════════

    public LanguageProcessingResult Deobfuscate(string content, ObfuscationContext context, string? filePath = null)
    {
        if (string.IsNullOrWhiteSpace(content))
        {
            return NoChange(content);
        }

        var warnings = new List<string>();

        try
        {
            var reverse = context.Mappings.Reverse;
            var tokens = OracleTokenizer.Tokenize(content);
            var replacements = new List<OracleReplacement>();

            foreach (var token in tokens)
            {
                switch (token.Type)
                {
                    case OracleTokenType.Identifier:
                    {
                        if (AliasPattern.IsMatch(token.Value) && reverse.TryGetValue(token.Value, out var original))
                        {
                            replacements.Add(new OracleReplacement(token.StartOffset, token.Length, original));
                        }
                        // Also try case-insensitive match (aliases are uppercase PREFIX_N)
                        else if (AliasPattern.IsMatch(token.NormalizedValue) && reverse.TryGetValue(token.NormalizedValue, out var originalUpper))
                        {
                            replacements.Add(new OracleReplacement(token.StartOffset, token.Length, originalUpper));
                        }
                        break;
                    }

                    case OracleTokenType.QuotedIdentifier:
                    {
                        var inner = token.Value[1..^1]; // strip quotes
                        if (AliasPattern.IsMatch(inner) && reverse.TryGetValue(inner, out var original))
                        {
                            replacements.Add(new OracleReplacement(token.StartOffset, token.Length, $"\"{original}\""));
                        }
                        break;
                    }

                    case OracleTokenType.BindVariable:
                    {
                        var varName = token.Value[1..]; // strip leading :
                        if (AliasPattern.IsMatch(varName) && reverse.TryGetValue(varName, out var original))
                        {
                            replacements.Add(new OracleReplacement(token.StartOffset, token.Length, $":{original}"));
                        }
                        break;
                    }

                    case OracleTokenType.StringLiteral:
                    {
                        var inner = ExtractStringLiteralInner(token.Value);
                        if (!string.IsNullOrEmpty(inner) && AliasPattern.IsMatch(inner) &&
                            reverse.TryGetValue(inner, out var original))
                        {
                            replacements.Add(new OracleReplacement(token.StartOffset, token.Length, $"'{original}'"));
                        }
                        break;
                    }

                    // Keywords that happen to match alias patterns (unlikely but defensive)
                    case OracleTokenType.Keyword:
                    {
                        if (AliasPattern.IsMatch(token.NormalizedValue) && reverse.TryGetValue(token.NormalizedValue, out var original))
                        {
                            replacements.Add(new OracleReplacement(token.StartOffset, token.Length, original));
                        }
                        break;
                    }
                }
            }

            // Also do a regex pass for aliases that might have been embedded in unsupported constructs
            DeobfuscateRegexFallback(content, reverse, replacements);

            var deduped = DeduplicateReplacements(replacements);
            var sorted = deduped.OrderByDescending(r => r.Offset).ToList();

            var result = new StringBuilder(content);
            foreach (var replacement in sorted)
            {
                if (replacement.Offset >= 0 && replacement.Offset + replacement.Length <= result.Length)
                {
                    result.Remove(replacement.Offset, replacement.Length);
                    result.Insert(replacement.Offset, replacement.NewValue);
                }
            }

            var finalContent = result.ToString();
            return new LanguageProcessingResult
            {
                Content = finalContent,
                WasTransformed = sorted.Count > 0,
                ReplacementCount = sorted.Count,
                ProcessorId = ProcessorId,
                Warnings = warnings
            };
        }
        catch (Exception ex)
        {
            warnings.Add($"Oracle SQL deobfuscation failed: {ex.Message}. Attempting regex fallback.");
            return DeobfuscateByRegexOnly(content, context, warnings);
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Validate
    // ═══════════════════════════════════════════════════════════════════════

    public ValidationResult Validate(string obfuscatedContent)
    {
        if (string.IsNullOrWhiteSpace(obfuscatedContent))
        {
            return ValidationResult.Valid();
        }

        try
        {
            var errors = new List<string>();
            var tokens = OracleTokenizer.Tokenize(obfuscatedContent);
            var significantTokens = tokens
                .Where(t => t.Type != OracleTokenType.Whitespace && t.Type != OracleTokenType.NewLine)
                .ToList();

            // Check balanced parentheses
            var parenDepth = 0;
            foreach (var token in significantTokens)
            {
                if (token.Type == OracleTokenType.LeftParen) parenDepth++;
                else if (token.Type == OracleTokenType.RightParen) parenDepth--;

                if (parenDepth < 0)
                {
                    errors.Add($"Line {token.Line}: Unmatched closing parenthesis.");
                    break;
                }
            }
            if (parenDepth > 0)
            {
                errors.Add($"Unmatched opening parenthesis ({parenDepth} unclosed).");
            }

            // Check balanced BEGIN/END blocks
            var blockDepth = 0;
            foreach (var token in significantTokens)
            {
                if (token.Type == OracleTokenType.Keyword)
                {
                    var upper = token.NormalizedValue;
                    if (upper == "BEGIN") blockDepth++;
                    else if (upper == "END") blockDepth--;
                }

                if (blockDepth < 0)
                {
                    errors.Add($"Line {token.Line}: Unmatched END without corresponding BEGIN.");
                    break;
                }
            }
            if (blockDepth > 0)
            {
                errors.Add($"Unmatched BEGIN block ({blockDepth} unclosed).");
            }

            // Check for unterminated string literals
            foreach (var token in tokens)
            {
                if (token.Type == OracleTokenType.StringLiteral)
                {
                    if (!token.Value.EndsWith("'"))
                    {
                        errors.Add($"Line {token.Line}: Unterminated string literal.");
                    }
                }
            }

            return errors.Count == 0 ? ValidationResult.Valid() : ValidationResult.Invalid(errors);
        }
        catch (Exception ex)
        {
            return ValidationResult.Invalid(new List<string> { $"Validation exception: {ex.Message}" });
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Pass 1: Discovery
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Analyzes the significant token stream to find declarations and register
    /// discovered identifiers with their semantic categories.
    /// </summary>
    private static void DiscoverDeclarations(
        List<OracleToken> tokens,
        ObfuscationContext context,
        string? filePath,
        Dictionary<string, SemanticCategory> discovered,
        List<string> warnings)
    {
        for (var i = 0; i < tokens.Count; i++)
        {
            var token = tokens[i];
            if (token.Type != OracleTokenType.Keyword)
                continue;

            var kw = token.NormalizedValue;

            // ── CREATE statements ────────────────────────────────────────
            if (kw == "CREATE")
            {
                DiscoverCreateStatement(tokens, ref i, context, filePath, discovered, warnings);
                continue;
            }

            // ── SELECT ... FROM (column discovery) ───────────────────────
            if (kw == "SELECT")
            {
                DiscoverSelectColumns(tokens, ref i, context, filePath, discovered);
                continue;
            }

            // ── FROM / JOIN (table references) ───────────────────────────
            if (kw is "FROM" or "JOIN" or "INNER" or "LEFT" or "RIGHT" or
                "FULL" or "CROSS" or "NATURAL")
            {
                if (kw != "FROM" && kw != "JOIN")
                {
                    // Skip to JOIN keyword
                    var j = i + 1;
                    while (j < tokens.Count && tokens[j].NormalizedValue != "JOIN" &&
                           tokens[j].NormalizedValue != "FROM")
                        j++;
                    if (j < tokens.Count)
                    {
                        i = j;
                        kw = tokens[i].NormalizedValue;
                    }
                }
                if (kw is "FROM" or "JOIN")
                {
                    DiscoverTableReferences(tokens, ref i, context, filePath, discovered);
                }
                continue;
            }

            // ── PL/SQL DECLARE block variable declarations ───────────────
            if (kw == "DECLARE")
            {
                DiscoverDeclareBlock(tokens, ref i, context, filePath, discovered);
                continue;
            }

            // ── Procedure/Function parameters (standalone or in package) ─
            if (kw is "PROCEDURE" or "FUNCTION")
            {
                DiscoverSubprogramParameters(tokens, ref i, context, filePath, discovered);
                continue;
            }

            // ── CURSOR declarations ──────────────────────────────────────
            if (kw == "CURSOR")
            {
                DiscoverCursorDeclaration(tokens, ref i, context, filePath, discovered);
                continue;
            }

            // ── UPDATE SET (column references) ───────────────────────────
            if (kw == "UPDATE")
            {
                DiscoverUpdateStatement(tokens, ref i, context, filePath, discovered);
                continue;
            }

            // ── INSERT INTO columns ──────────────────────────────────────
            if (kw == "INSERT")
            {
                DiscoverInsertStatement(tokens, ref i, context, filePath, discovered);
                continue;
            }

            // ── WHERE / ON / SET (column references in expressions) ──────
            if (kw is "WHERE" or "ON" or "SET" or "HAVING")
            {
                DiscoverExpressionColumns(tokens, ref i, context, filePath, discovered);
                continue;
            }
        }
    }

    /// <summary>
    /// Discovers identifiers in CREATE statements (TABLE, VIEW, PROCEDURE, FUNCTION,
    /// PACKAGE, SEQUENCE, SYNONYM, MATERIALIZED VIEW, TRIGGER, INDEX).
    /// </summary>
    private static void DiscoverCreateStatement(
        List<OracleToken> tokens,
        ref int i,
        ObfuscationContext context,
        string? filePath,
        Dictionary<string, SemanticCategory> discovered,
        List<string> warnings)
    {
        i++; // move past CREATE

        // Skip optional OR REPLACE
        if (i < tokens.Count && tokens[i].NormalizedValue == "OR")
        {
            i++;
            if (i < tokens.Count && tokens[i].NormalizedValue == "REPLACE")
                i++;
        }

        // Skip optional GLOBAL, TEMPORARY, FORCE, EDITIONABLE, NONEDITIONABLE
        while (i < tokens.Count && tokens[i].Type == OracleTokenType.Keyword &&
               tokens[i].NormalizedValue is "GLOBAL" or "TEMPORARY" or "FORCE" or
               "NOFORCE" or "EDITIONABLE" or "NONEDITIONABLE" or "PUBLIC")
        {
            i++;
        }

        if (i >= tokens.Count) return;

        var objectType = tokens[i].NormalizedValue;
        i++; // move past object type keyword

        switch (objectType)
        {
            case "TABLE":
            {
                // CREATE TABLE [schema.]tablename (columns...)
                var (schema, name) = ReadQualifiedName(tokens, ref i);
                if (schema != null)
                    RegisterIdentifier(schema, SemanticCategory.Schema, context, filePath, discovered, tokens, i);
                if (name != null)
                    RegisterIdentifier(name, SemanticCategory.Table, context, filePath, discovered, tokens, i);

                // Parse column definitions inside parentheses
                DiscoverTableColumns(tokens, ref i, context, filePath, discovered);
                break;
            }

            case "VIEW":
            {
                var (schema, name) = ReadQualifiedName(tokens, ref i);
                if (schema != null)
                    RegisterIdentifier(schema, SemanticCategory.Schema, context, filePath, discovered, tokens, i);
                if (name != null)
                    RegisterIdentifier(name, SemanticCategory.Table, context, filePath, discovered, tokens, i);
                break;
            }

            case "MATERIALIZED":
            {
                // CREATE MATERIALIZED VIEW name
                if (i < tokens.Count && tokens[i].NormalizedValue == "VIEW")
                {
                    i++;
                    var (schema, name) = ReadQualifiedName(tokens, ref i);
                    if (schema != null)
                        RegisterIdentifier(schema, SemanticCategory.Schema, context, filePath, discovered, tokens, i);
                    if (name != null)
                        RegisterIdentifier(name, SemanticCategory.MaterializedView, context, filePath, discovered, tokens, i);
                }
                break;
            }

            case "SEQUENCE":
            {
                var (schema, name) = ReadQualifiedName(tokens, ref i);
                if (schema != null)
                    RegisterIdentifier(schema, SemanticCategory.Schema, context, filePath, discovered, tokens, i);
                if (name != null)
                    RegisterIdentifier(name, SemanticCategory.Sequence, context, filePath, discovered, tokens, i);
                break;
            }

            case "SYNONYM":
            {
                // CREATE [PUBLIC] SYNONYM name FOR target
                var (schema, name) = ReadQualifiedName(tokens, ref i);
                if (schema != null)
                    RegisterIdentifier(schema, SemanticCategory.Schema, context, filePath, discovered, tokens, i);
                if (name != null)
                    RegisterIdentifier(name, SemanticCategory.Synonym, context, filePath, discovered, tokens, i);

                // FOR target
                if (i < tokens.Count && tokens[i].NormalizedValue == "FOR")
                {
                    i++;
                    var (tSchema, tName) = ReadQualifiedName(tokens, ref i);
                    if (tSchema != null)
                        RegisterIdentifier(tSchema, SemanticCategory.Schema, context, filePath, discovered, tokens, i);
                    if (tName != null)
                        RegisterIdentifier(tName, SemanticCategory.Table, context, filePath, discovered, tokens, i);
                }
                break;
            }

            case "PROCEDURE":
            {
                var (schema, name) = ReadQualifiedName(tokens, ref i);
                if (schema != null)
                    RegisterIdentifier(schema, SemanticCategory.Schema, context, filePath, discovered, tokens, i);
                if (name != null)
                    RegisterIdentifier(name, SemanticCategory.StoredProc, context, filePath, discovered, tokens, i);
                // Parameters are discovered separately
                break;
            }

            case "FUNCTION":
            {
                var (schema, name) = ReadQualifiedName(tokens, ref i);
                if (schema != null)
                    RegisterIdentifier(schema, SemanticCategory.Schema, context, filePath, discovered, tokens, i);
                if (name != null)
                    RegisterIdentifier(name, SemanticCategory.Function, context, filePath, discovered, tokens, i);
                break;
            }

            case "PACKAGE":
            {
                // CREATE PACKAGE [BODY] [schema.]name
                var isBody = false;
                if (i < tokens.Count && tokens[i].NormalizedValue == "BODY")
                {
                    isBody = true;
                    i++;
                }
                var (schema, name) = ReadQualifiedName(tokens, ref i);
                if (schema != null)
                    RegisterIdentifier(schema, SemanticCategory.Schema, context, filePath, discovered, tokens, i);
                if (name != null)
                {
                    var cat = isBody ? SemanticCategory.OraclePackageBody : SemanticCategory.OraclePackage;
                    RegisterIdentifier(name, cat, context, filePath, discovered, tokens, i);
                }
                break;
            }

            case "TRIGGER":
            {
                var (schema, name) = ReadQualifiedName(tokens, ref i);
                if (schema != null)
                    RegisterIdentifier(schema, SemanticCategory.Schema, context, filePath, discovered, tokens, i);
                if (name != null)
                    RegisterIdentifier(name, SemanticCategory.StoredProc, context, filePath, discovered, tokens, i);
                break;
            }

            case "INDEX":
            {
                // CREATE [UNIQUE] INDEX [schema.]indexname ON [schema.]tablename(cols)
                // Skip optional UNIQUE that might come before INDEX
                var (schema, name) = ReadQualifiedName(tokens, ref i);
                if (schema != null)
                    RegisterIdentifier(schema, SemanticCategory.Schema, context, filePath, discovered, tokens, i);
                if (name != null)
                    RegisterIdentifier(name, SemanticCategory.Table, context, filePath, discovered, tokens, i);

                // ON clause
                if (i < tokens.Count && tokens[i].NormalizedValue == "ON")
                {
                    i++;
                    var (tSchema, tName) = ReadQualifiedName(tokens, ref i);
                    if (tSchema != null)
                        RegisterIdentifier(tSchema, SemanticCategory.Schema, context, filePath, discovered, tokens, i);
                    if (tName != null)
                        RegisterIdentifier(tName, SemanticCategory.Table, context, filePath, discovered, tokens, i);
                }
                break;
            }

            case "TYPE":
            {
                var (schema, name) = ReadQualifiedName(tokens, ref i);
                if (schema != null)
                    RegisterIdentifier(schema, SemanticCategory.Schema, context, filePath, discovered, tokens, i);
                if (name != null)
                    RegisterIdentifier(name, SemanticCategory.Table, context, filePath, discovered, tokens, i);
                break;
            }
        }
    }

    /// <summary>
    /// Discovers column names in a CREATE TABLE column list.
    /// </summary>
    private static void DiscoverTableColumns(
        List<OracleToken> tokens,
        ref int i,
        ObfuscationContext context,
        string? filePath,
        Dictionary<string, SemanticCategory> discovered)
    {
        // Find the opening parenthesis
        while (i < tokens.Count && tokens[i].Type != OracleTokenType.LeftParen)
        {
            // If we hit a keyword like AS (for CREATE TABLE AS SELECT), stop
            if (tokens[i].Type == OracleTokenType.Keyword &&
                tokens[i].NormalizedValue is "AS" or "SELECT")
                return;
            i++;
        }

        if (i >= tokens.Count) return;
        i++; // skip (

        var parenDepth = 1;
        var expectingColumnName = true;

        while (i < tokens.Count && parenDepth > 0)
        {
            var t = tokens[i];

            if (t.Type == OracleTokenType.LeftParen) { parenDepth++; expectingColumnName = false; i++; continue; }
            if (t.Type == OracleTokenType.RightParen) { parenDepth--; i++; continue; }

            if (parenDepth == 1)
            {
                if (t.Type == OracleTokenType.Comma)
                {
                    expectingColumnName = true;
                    i++;
                    continue;
                }

                if (expectingColumnName && t.Type == OracleTokenType.Identifier && !OracleReservedWords.ShouldPreserve(t.Value))
                {
                    // Check this is not a constraint keyword
                    if (t.NormalizedValue is not ("CONSTRAINT" or "PRIMARY" or "FOREIGN" or "UNIQUE" or "CHECK"))
                    {
                        RegisterIdentifier(t, SemanticCategory.Column, context, filePath, discovered, tokens, i);
                    }
                    expectingColumnName = false;
                }
                else if (expectingColumnName && t.Type == OracleTokenType.QuotedIdentifier)
                {
                    var inner = t.Value[1..^1];
                    RegisterIdentifierRaw(inner, SemanticCategory.Column, context, filePath, discovered, t);
                    expectingColumnName = false;
                }
                else if (t.Type == OracleTokenType.Keyword &&
                         t.NormalizedValue is "CONSTRAINT" or "PRIMARY" or "FOREIGN" or "UNIQUE" or "CHECK")
                {
                    // Skip constraint definitions
                    expectingColumnName = false;
                }
            }

            i++;
        }
    }

    /// <summary>
    /// Discovers column references in a SELECT list (between SELECT and FROM).
    /// </summary>
    private static void DiscoverSelectColumns(
        List<OracleToken> tokens,
        ref int i,
        ObfuscationContext context,
        string? filePath,
        Dictionary<string, SemanticCategory> discovered)
    {
        i++; // move past SELECT

        // Skip DISTINCT/ALL/UNIQUE
        if (i < tokens.Count && tokens[i].Type == OracleTokenType.Keyword &&
            tokens[i].NormalizedValue is "DISTINCT" or "ALL" or "UNIQUE")
        {
            i++;
        }

        var parenDepth = 0;

        while (i < tokens.Count)
        {
            var t = tokens[i];

            if (t.Type == OracleTokenType.LeftParen) { parenDepth++; i++; continue; }
            if (t.Type == OracleTokenType.RightParen) { parenDepth--; i++; continue; }

            // Stop at FROM (at paren depth 0)
            if (parenDepth == 0 && t.Type == OracleTokenType.Keyword && t.NormalizedValue == "FROM")
            {
                break;
            }

            // At the top level, identifiers not preceded by a function call are column refs
            if (parenDepth == 0 && t.Type == OracleTokenType.Identifier &&
                !OracleReservedWords.ShouldPreserve(t.Value))
            {
                // Check if followed by ( which would mean it's a function call
                if (i + 1 < tokens.Count && tokens[i + 1].Type == OracleTokenType.LeftParen)
                {
                    // Function call - skip
                    i++;
                    continue;
                }

                // Check if this is a table alias qualifier (identifier.column)
                if (i + 1 < tokens.Count && tokens[i + 1].Type == OracleTokenType.Dot)
                {
                    // Could be table.column or schema.table.column
                    RegisterIdentifier(t, SemanticCategory.Table, context, filePath, discovered, tokens, i);
                    i++;
                    continue;
                }

                // Check if preceded by dot (then this is the column part of table.column)
                if (i > 0 && tokens[i - 1].Type == OracleTokenType.Dot)
                {
                    RegisterIdentifier(t, SemanticCategory.Column, context, filePath, discovered, tokens, i);
                    i++;
                    continue;
                }

                // Check if followed by AS/alias or comma -> column reference or column alias
                RegisterIdentifier(t, SemanticCategory.Column, context, filePath, discovered, tokens, i);
            }
            else if (parenDepth == 0 && t.Type == OracleTokenType.QuotedIdentifier)
            {
                var inner = t.Value[1..^1];
                RegisterIdentifierRaw(inner, SemanticCategory.Column, context, filePath, discovered, t);
            }

            i++;
        }
    }

    /// <summary>
    /// Discovers table references after FROM or JOIN keywords.
    /// </summary>
    private static void DiscoverTableReferences(
        List<OracleToken> tokens,
        ref int i,
        ObfuscationContext context,
        string? filePath,
        Dictionary<string, SemanticCategory> discovered)
    {
        i++; // move past FROM/JOIN

        var parenDepth = 0;

        while (i < tokens.Count)
        {
            var t = tokens[i];

            if (t.Type == OracleTokenType.LeftParen) { parenDepth++; i++; continue; }
            if (t.Type == OracleTokenType.RightParen) { parenDepth--; i++; continue; }

            // Stop at keywords that end the FROM clause
            if (parenDepth == 0 && t.Type == OracleTokenType.Keyword &&
                t.NormalizedValue is "WHERE" or "GROUP" or "HAVING" or "ORDER" or
                "UNION" or "INTERSECT" or "MINUS" or "EXCEPT" or "SELECT" or
                "ON" or "USING" or "SET" or "RETURNING" or "BEGIN" or
                "DECLARE" or "PIVOT" or "UNPIVOT" or "MERGE" or
                "FOR" or "CONNECT" or "START" or "FETCH" or "OFFSET")
            {
                break;
            }

            // Also stop at JOIN variants
            if (parenDepth == 0 && t.Type == OracleTokenType.Keyword &&
                t.NormalizedValue is "JOIN" or "INNER" or "LEFT" or "RIGHT" or
                "FULL" or "CROSS" or "NATURAL")
            {
                break;
            }

            if (parenDepth == 0 && t.Type == OracleTokenType.Identifier &&
                !OracleReservedWords.ShouldPreserve(t.Value))
            {
                // Check if preceded by dot -> this is the object part of schema.object
                if (i > 0 && tokens[i - 1].Type == OracleTokenType.Dot)
                {
                    RegisterIdentifier(t, SemanticCategory.Table, context, filePath, discovered, tokens, i);
                    i++;
                    continue;
                }

                // Check if followed by dot -> this is a schema
                if (i + 1 < tokens.Count && tokens[i + 1].Type == OracleTokenType.Dot)
                {
                    RegisterIdentifier(t, SemanticCategory.Schema, context, filePath, discovered, tokens, i);
                    i++;
                    continue;
                }

                // Otherwise it's a table name (possibly followed by an alias)
                RegisterIdentifier(t, SemanticCategory.Table, context, filePath, discovered, tokens, i);

                // Check for table alias (next non-keyword identifier)
                if (i + 1 < tokens.Count)
                {
                    var next = tokens[i + 1];
                    if (next.Type == OracleTokenType.Keyword && next.NormalizedValue == "AS")
                    {
                        // Skip AS, then the alias
                        i += 2;
                        if (i < tokens.Count && tokens[i].Type == OracleTokenType.Identifier &&
                            !OracleReservedWords.ShouldPreserve(tokens[i].Value))
                        {
                            RegisterIdentifier(tokens[i], SemanticCategory.Table, context, filePath, discovered, tokens, i);
                        }
                    }
                    else if (next.Type == OracleTokenType.Identifier && !OracleReservedWords.ShouldPreserve(next.Value))
                    {
                        // Implicit alias
                        i++;
                        RegisterIdentifier(tokens[i], SemanticCategory.Table, context, filePath, discovered, tokens, i);
                    }
                }
            }
            else if (parenDepth == 0 && t.Type == OracleTokenType.QuotedIdentifier)
            {
                var inner = t.Value[1..^1];
                RegisterIdentifierRaw(inner, SemanticCategory.Table, context, filePath, discovered, t);
            }

            i++;
        }
    }

    /// <summary>
    /// Discovers variable declarations in a PL/SQL DECLARE block.
    /// Variables are declared as: name TYPE; or name table.column%TYPE;
    /// </summary>
    private static void DiscoverDeclareBlock(
        List<OracleToken> tokens,
        ref int i,
        ObfuscationContext context,
        string? filePath,
        Dictionary<string, SemanticCategory> discovered)
    {
        i++; // move past DECLARE

        while (i < tokens.Count)
        {
            var t = tokens[i];

            // Stop at BEGIN
            if (t.Type == OracleTokenType.Keyword && t.NormalizedValue == "BEGIN")
                break;

            // CURSOR declaration
            if (t.Type == OracleTokenType.Keyword && t.NormalizedValue == "CURSOR")
            {
                DiscoverCursorDeclaration(tokens, ref i, context, filePath, discovered);
                continue;
            }

            // TYPE declaration
            if (t.Type == OracleTokenType.Keyword && t.NormalizedValue == "TYPE")
            {
                i++; // skip TYPE
                if (i < tokens.Count && tokens[i].Type == OracleTokenType.Identifier &&
                    !OracleReservedWords.ShouldPreserve(tokens[i].Value))
                {
                    RegisterIdentifier(tokens[i], SemanticCategory.Variable, context, filePath, discovered, tokens, i);
                }
                // Skip to semicolon
                while (i < tokens.Count && tokens[i].Type != OracleTokenType.Semicolon) i++;
                if (i < tokens.Count) i++;
                continue;
            }

            // SUBTYPE declaration
            if (t.Type == OracleTokenType.Keyword && t.NormalizedValue == "SUBTYPE")
            {
                i++;
                if (i < tokens.Count && tokens[i].Type == OracleTokenType.Identifier &&
                    !OracleReservedWords.ShouldPreserve(tokens[i].Value))
                {
                    RegisterIdentifier(tokens[i], SemanticCategory.Variable, context, filePath, discovered, tokens, i);
                }
                while (i < tokens.Count && tokens[i].Type != OracleTokenType.Semicolon) i++;
                if (i < tokens.Count) i++;
                continue;
            }

            // PROCEDURE/FUNCTION in declare block (forward declaration)
            if (t.Type == OracleTokenType.Keyword && t.NormalizedValue is "PROCEDURE" or "FUNCTION")
            {
                DiscoverSubprogramParameters(tokens, ref i, context, filePath, discovered);
                continue;
            }

            // Variable declaration: identifier followed by a data type or table.column%TYPE
            if (t.Type == OracleTokenType.Identifier && !OracleReservedWords.ShouldPreserve(t.Value))
            {
                // Check that the next significant token is a type or CONSTANT keyword
                var peek = i + 1;
                if (peek < tokens.Count)
                {
                    var next = tokens[peek];
                    var isDeclaration = next.Type == OracleTokenType.Keyword &&
                                        (OracleReservedWords.IsDataType(next.Value) ||
                                         next.NormalizedValue is "CONSTANT" or "EXCEPTION");
                    var isRefDeclaration = next.Type == OracleTokenType.Identifier; // could be table.col%TYPE

                    if (isDeclaration || isRefDeclaration)
                    {
                        // Check for EXCEPTION declaration
                        if (next.NormalizedValue == "EXCEPTION")
                        {
                            RegisterIdentifier(t, SemanticCategory.Variable, context, filePath, discovered, tokens, i);
                        }
                        else
                        {
                            RegisterIdentifier(t, SemanticCategory.Variable, context, filePath, discovered, tokens, i);
                        }
                    }
                }

                // Skip to semicolon
                while (i < tokens.Count && tokens[i].Type != OracleTokenType.Semicolon) i++;
                if (i < tokens.Count) i++;
                continue;
            }

            i++;
        }
    }

    /// <summary>
    /// Discovers a CURSOR declaration: CURSOR name IS SELECT ...
    /// </summary>
    private static void DiscoverCursorDeclaration(
        List<OracleToken> tokens,
        ref int i,
        ObfuscationContext context,
        string? filePath,
        Dictionary<string, SemanticCategory> discovered)
    {
        i++; // move past CURSOR

        if (i < tokens.Count && tokens[i].Type == OracleTokenType.Identifier &&
            !OracleReservedWords.ShouldPreserve(tokens[i].Value))
        {
            RegisterIdentifier(tokens[i], SemanticCategory.Cursor, context, filePath, discovered, tokens, i);
        }

        // Skip to semicolon or IS/AS
        while (i < tokens.Count && tokens[i].Type != OracleTokenType.Semicolon &&
               !(tokens[i].Type == OracleTokenType.Keyword &&
                 tokens[i].NormalizedValue is "IS" or "AS"))
        {
            // Discover parameters in cursor
            if (tokens[i].Type == OracleTokenType.LeftParen)
            {
                DiscoverParameterList(tokens, ref i, context, filePath, discovered);
                continue;
            }
            i++;
        }
        if (i < tokens.Count && tokens[i].Type == OracleTokenType.Semicolon) i++;
    }

    /// <summary>
    /// Discovers parameters for a PROCEDURE or FUNCTION declaration.
    /// </summary>
    private static void DiscoverSubprogramParameters(
        List<OracleToken> tokens,
        ref int i,
        ObfuscationContext context,
        string? filePath,
        Dictionary<string, SemanticCategory> discovered)
    {
        var subType = tokens[i].NormalizedValue;
        i++; // move past PROCEDURE/FUNCTION

        // Skip the name (already discovered by CREATE or will be discovered as an identifier)
        if (i < tokens.Count && (tokens[i].Type == OracleTokenType.Identifier ||
                                  tokens[i].Type == OracleTokenType.QuotedIdentifier))
        {
            i++; // skip the first identifier
            // Skip optional schema.name continuation (dot + identifier pairs)
            while (i < tokens.Count && tokens[i].Type == OracleTokenType.Dot)
            {
                i++; // skip dot
                if (i < tokens.Count && (tokens[i].Type == OracleTokenType.Identifier ||
                                          tokens[i].Type == OracleTokenType.QuotedIdentifier))
                {
                    i++; // skip next identifier
                }
            }
        }

        // Look for parameter list
        if (i < tokens.Count && tokens[i].Type == OracleTokenType.LeftParen)
        {
            DiscoverParameterList(tokens, ref i, context, filePath, discovered);
        }

        // For FUNCTION, skip RETURN clause
        if (subType == "FUNCTION")
        {
            while (i < tokens.Count && tokens[i].NormalizedValue != "RETURN") i++;
            if (i < tokens.Count) i++; // skip RETURN
            // Skip return type
            while (i < tokens.Count && tokens[i].Type != OracleTokenType.Semicolon &&
                   !(tokens[i].Type == OracleTokenType.Keyword &&
                     tokens[i].NormalizedValue is "IS" or "AS" or "DETERMINISTIC" or
                     "PIPELINED" or "PARALLEL_ENABLE" or "RESULT_CACHE"))
            {
                i++;
            }
        }
    }

    /// <summary>
    /// Discovers parameters in a parenthesized parameter list.
    /// </summary>
    private static void DiscoverParameterList(
        List<OracleToken> tokens,
        ref int i,
        ObfuscationContext context,
        string? filePath,
        Dictionary<string, SemanticCategory> discovered)
    {
        if (i >= tokens.Count || tokens[i].Type != OracleTokenType.LeftParen) return;
        i++; // skip (

        var parenDepth = 1;
        var expectParam = true;

        while (i < tokens.Count && parenDepth > 0)
        {
            var t = tokens[i];

            if (t.Type == OracleTokenType.LeftParen) { parenDepth++; expectParam = false; i++; continue; }
            if (t.Type == OracleTokenType.RightParen) { parenDepth--; i++; continue; }

            if (parenDepth == 1)
            {
                if (t.Type == OracleTokenType.Comma)
                {
                    expectParam = true;
                    i++;
                    continue;
                }

                if (expectParam && t.Type == OracleTokenType.Identifier &&
                    !OracleReservedWords.ShouldPreserve(t.Value))
                {
                    RegisterIdentifier(t, SemanticCategory.Parameter, context, filePath, discovered, tokens, i);
                    expectParam = false;
                }
            }

            i++;
        }
    }

    /// <summary>
    /// Discovers column references in WHERE, ON, SET, HAVING clauses.
    /// </summary>
    private static void DiscoverExpressionColumns(
        List<OracleToken> tokens,
        ref int i,
        ObfuscationContext context,
        string? filePath,
        Dictionary<string, SemanticCategory> discovered)
    {
        i++; // move past WHERE/ON/SET/HAVING

        var parenDepth = 0;

        while (i < tokens.Count)
        {
            var t = tokens[i];

            if (t.Type == OracleTokenType.LeftParen) { parenDepth++; i++; continue; }
            if (t.Type == OracleTokenType.RightParen)
            {
                if (parenDepth == 0) break;
                parenDepth--;
                i++;
                continue;
            }

            // Stop at clause boundaries
            if (parenDepth == 0 && t.Type == OracleTokenType.Keyword &&
                t.NormalizedValue is "GROUP" or "ORDER" or "HAVING" or "UNION" or
                "INTERSECT" or "MINUS" or "EXCEPT" or "FROM" or "SELECT" or
                "WHERE" or "SET" or "BEGIN" or "DECLARE" or "RETURNING" or
                "FOR" or "CONNECT" or "START" or "FETCH" or "OFFSET" or
                "JOIN" or "INNER" or "LEFT" or "RIGHT" or "FULL" or "CROSS" or
                "INTO" or "VALUES")
            {
                break;
            }

            if (t.Type == OracleTokenType.Semicolon) break;

            if (t.Type == OracleTokenType.Identifier && !OracleReservedWords.ShouldPreserve(t.Value))
            {
                // Check if followed by ( -> function call
                if (i + 1 < tokens.Count && tokens[i + 1].Type == OracleTokenType.LeftParen)
                {
                    i++;
                    continue;
                }

                // Check context for table.column pattern
                if (i > 0 && tokens[i - 1].Type == OracleTokenType.Dot)
                {
                    RegisterIdentifier(t, SemanticCategory.Column, context, filePath, discovered, tokens, i);
                }
                else if (i + 1 < tokens.Count && tokens[i + 1].Type == OracleTokenType.Dot)
                {
                    RegisterIdentifier(t, SemanticCategory.Table, context, filePath, discovered, tokens, i);
                }
                else
                {
                    RegisterIdentifier(t, SemanticCategory.Column, context, filePath, discovered, tokens, i);
                }
            }
            else if (t.Type == OracleTokenType.QuotedIdentifier)
            {
                var inner = t.Value[1..^1];
                RegisterIdentifierRaw(inner, SemanticCategory.Column, context, filePath, discovered, t);
            }

            i++;
        }
    }

    /// <summary>
    /// Discovers identifiers in an UPDATE statement.
    /// </summary>
    private static void DiscoverUpdateStatement(
        List<OracleToken> tokens,
        ref int i,
        ObfuscationContext context,
        string? filePath,
        Dictionary<string, SemanticCategory> discovered)
    {
        i++; // move past UPDATE

        // Table name
        if (i < tokens.Count && tokens[i].Type == OracleTokenType.Identifier &&
            !OracleReservedWords.ShouldPreserve(tokens[i].Value))
        {
            // Check for schema.table
            if (i + 1 < tokens.Count && tokens[i + 1].Type == OracleTokenType.Dot)
            {
                RegisterIdentifier(tokens[i], SemanticCategory.Schema, context, filePath, discovered, tokens, i);
                i += 2; // skip schema and dot
                if (i < tokens.Count && tokens[i].Type == OracleTokenType.Identifier)
                {
                    RegisterIdentifier(tokens[i], SemanticCategory.Table, context, filePath, discovered, tokens, i);
                    i++;
                }
            }
            else
            {
                RegisterIdentifier(tokens[i], SemanticCategory.Table, context, filePath, discovered, tokens, i);
                i++;
            }

            // Optional alias
            if (i < tokens.Count && tokens[i].Type == OracleTokenType.Identifier &&
                !OracleReservedWords.ShouldPreserve(tokens[i].Value) &&
                tokens[i].NormalizedValue != "SET")
            {
                RegisterIdentifier(tokens[i], SemanticCategory.Table, context, filePath, discovered, tokens, i);
                i++;
            }
        }
    }

    /// <summary>
    /// Discovers identifiers in an INSERT statement.
    /// </summary>
    private static void DiscoverInsertStatement(
        List<OracleToken> tokens,
        ref int i,
        ObfuscationContext context,
        string? filePath,
        Dictionary<string, SemanticCategory> discovered)
    {
        i++; // move past INSERT

        // INTO
        if (i < tokens.Count && tokens[i].NormalizedValue == "INTO")
            i++;

        // Table name
        if (i < tokens.Count && tokens[i].Type == OracleTokenType.Identifier &&
            !OracleReservedWords.ShouldPreserve(tokens[i].Value))
        {
            if (i + 1 < tokens.Count && tokens[i + 1].Type == OracleTokenType.Dot)
            {
                RegisterIdentifier(tokens[i], SemanticCategory.Schema, context, filePath, discovered, tokens, i);
                i += 2;
                if (i < tokens.Count && tokens[i].Type == OracleTokenType.Identifier)
                {
                    RegisterIdentifier(tokens[i], SemanticCategory.Table, context, filePath, discovered, tokens, i);
                    i++;
                }
            }
            else
            {
                RegisterIdentifier(tokens[i], SemanticCategory.Table, context, filePath, discovered, tokens, i);
                i++;
            }
        }

        // Column list in parens
        if (i < tokens.Count && tokens[i].Type == OracleTokenType.LeftParen)
        {
            i++; // skip (
            var parenDepth = 1;
            while (i < tokens.Count && parenDepth > 0)
            {
                var t = tokens[i];
                if (t.Type == OracleTokenType.LeftParen) parenDepth++;
                else if (t.Type == OracleTokenType.RightParen) { parenDepth--; if (parenDepth == 0) { i++; break; } }
                else if (parenDepth == 1 && t.Type == OracleTokenType.Identifier &&
                         !OracleReservedWords.ShouldPreserve(t.Value))
                {
                    RegisterIdentifier(t, SemanticCategory.Column, context, filePath, discovered, tokens, i);
                }
                i++;
            }
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Pass 2: Replacement Collection
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Walks all tokens and collects replacements for identifiers that were discovered
    /// or appear in known SQL contexts, comments, string literals, and bind variables.
    /// </summary>
    private static void CollectReplacements(
        List<OracleToken> allTokens,
        List<OracleToken> significantTokens,
        ObfuscationContext context,
        string? filePath,
        Dictionary<string, SemanticCategory> discovered,
        List<OracleReplacement> replacements,
        List<string> warnings)
    {
        // Build a lookup of significant token indices for context analysis
        // We iterate all tokens (including whitespace) for position-accurate replacement
        foreach (var token in allTokens)
        {
            switch (token.Type)
            {
                case OracleTokenType.Comment:
                {
                    // Preserve optimizer hint structure: /*+ HINT */
                    if (token.Value.StartsWith("/*+", StringComparison.Ordinal))
                    {
                        // This is an optimizer hint - preserve the hint text but rename
                        // table aliases within it. For safety, just preserve the entire hint.
                        // Only replace if it contains identifiers we know about.
                        var hintContent = ReplaceIdentifiersInHint(token.Value, context, filePath, discovered);
                        if (hintContent != token.Value)
                        {
                            replacements.Add(new OracleReplacement(token.StartOffset, token.Length, hintContent));
                        }
                    }
                    else
                    {
                        replacements.Add(new OracleReplacement(
                            token.StartOffset,
                            token.Length,
                            "/* [Comment removed] */"));
                    }
                    break;
                }

                case OracleTokenType.LineComment:
                {
                    replacements.Add(new OracleReplacement(
                        token.StartOffset,
                        token.Length,
                        "-- [Comment removed]"));
                    break;
                }

                case OracleTokenType.StringLiteral:
                {
                    var inner = ExtractStringLiteralInner(token.Value);
                    if (!string.IsNullOrEmpty(inner) && !IsNumericOrDateLiteral(inner))
                    {
                        var alias = context.GetOrCreateAlias(
                            inner,
                            SemanticCategory.StringLiteral,
                            filePath,
                            token.Line,
                            token.StartOffset,
                            token.EndOffset);
                        var newLiteral = $"'{alias}'";
                        replacements.Add(new OracleReplacement(token.StartOffset, token.Length, newLiteral));
                    }
                    break;
                }

                case OracleTokenType.Identifier:
                {
                    var name = token.Value;
                    var upper = token.NormalizedValue;

                    if (OracleReservedWords.ShouldPreserve(name))
                        break;

                    // Look up in discovered identifiers
                    if (discovered.TryGetValue(upper, out var category))
                    {
                        var alias = context.GetOrCreateAlias(
                            name,
                            category,
                            filePath,
                            token.Line,
                            token.StartOffset,
                            token.EndOffset);
                        replacements.Add(new OracleReplacement(token.StartOffset, token.Length, alias));
                    }
                    // Also check if the context already has this identifier from another file/pass
                    else if (context.Mappings.HasMapping(name))
                    {
                        var alias = context.Mappings.Forward[name];
                        replacements.Add(new OracleReplacement(token.StartOffset, token.Length, alias));
                    }
                    break;
                }

                case OracleTokenType.QuotedIdentifier:
                {
                    var inner = token.Value[1..^1]; // strip quotes
                    if (string.IsNullOrEmpty(inner) || OracleReservedWords.ShouldPreserve(inner))
                        break;

                    SemanticCategory category;
                    if (discovered.TryGetValue(inner.ToUpperInvariant(), out var cat))
                    {
                        category = cat;
                    }
                    else if (context.Mappings.HasMapping(inner))
                    {
                        var alias = context.Mappings.Forward[inner];
                        replacements.Add(new OracleReplacement(token.StartOffset, token.Length, $"\"{alias}\""));
                        break;
                    }
                    else
                    {
                        category = SemanticCategory.Column; // default for quoted identifiers
                    }

                    var newAlias = context.GetOrCreateAlias(
                        inner,
                        category,
                        filePath,
                        token.Line,
                        token.StartOffset,
                        token.EndOffset);
                    replacements.Add(new OracleReplacement(token.StartOffset, token.Length, $"\"{newAlias}\""));
                    break;
                }

                case OracleTokenType.BindVariable:
                {
                    var varName = token.Value[1..]; // strip :
                    if (OracleReservedWords.ShouldPreserve(varName))
                        break;

                    SemanticCategory category;
                    if (discovered.TryGetValue(varName.ToUpperInvariant(), out var cat))
                    {
                        category = cat;
                    }
                    else
                    {
                        category = SemanticCategory.Variable;
                    }

                    var alias = context.GetOrCreateAlias(
                        varName,
                        category,
                        filePath,
                        token.Line,
                        token.StartOffset,
                        token.EndOffset);
                    replacements.Add(new OracleReplacement(token.StartOffset, token.Length, $":{alias}"));
                    break;
                }
            }
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Helper Methods
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Reads a potentially qualified name (schema.name) from the token stream.
    /// Advances the index past the name.
    /// </summary>
    private static (OracleToken? schema, OracleToken? name) ReadQualifiedName(
        List<OracleToken> tokens, ref int i)
    {
        if (i >= tokens.Count)
            return (null, null);

        var first = tokens[i];
        if (first.Type != OracleTokenType.Identifier && first.Type != OracleTokenType.QuotedIdentifier)
            return (null, null);

        i++;

        // Check for dot-separated name
        if (i < tokens.Count && tokens[i].Type == OracleTokenType.Dot)
        {
            i++; // skip dot
            if (i < tokens.Count && (tokens[i].Type == OracleTokenType.Identifier ||
                                      tokens[i].Type == OracleTokenType.QuotedIdentifier))
            {
                var second = tokens[i];
                i++;

                // Could be schema.table.column (three-part) - check for another dot
                if (i < tokens.Count && tokens[i].Type == OracleTokenType.Dot)
                {
                    // Three-part name - schema was first, table was second
                    // Skip to third part
                    i++; // skip dot
                    if (i < tokens.Count && (tokens[i].Type == OracleTokenType.Identifier ||
                                              tokens[i].Type == OracleTokenType.QuotedIdentifier))
                    {
                        i++;
                    }
                }

                return (first, second);
            }
            return (null, first); // dot but no second part
        }

        return (null, first);
    }

    /// <summary>
    /// Registers an identifier from a token into the context and discovery dictionary.
    /// </summary>
    private static void RegisterIdentifier(
        OracleToken token,
        SemanticCategory category,
        ObfuscationContext context,
        string? filePath,
        Dictionary<string, SemanticCategory> discovered,
        List<OracleToken> tokens,
        int tokenIndex)
    {
        var name = token.Type == OracleTokenType.QuotedIdentifier ? token.Value[1..^1] : token.Value;
        RegisterIdentifierRaw(name, category, context, filePath, discovered, token);
    }

    /// <summary>
    /// Registers an identifier by raw name value.
    /// </summary>
    private static void RegisterIdentifierRaw(
        string name,
        SemanticCategory category,
        ObfuscationContext context,
        string? filePath,
        Dictionary<string, SemanticCategory> discovered,
        OracleToken token)
    {
        if (string.IsNullOrEmpty(name) || OracleReservedWords.ShouldPreserve(name))
            return;

        var upper = name.ToUpperInvariant();
        if (!discovered.ContainsKey(upper))
        {
            discovered[upper] = category;
        }

        context.GetOrCreateAlias(
            name,
            category,
            filePath,
            token.Line,
            token.StartOffset,
            token.EndOffset);
    }

    /// <summary>
    /// Extracts the inner value from an Oracle string literal, handling escaped quotes ('').
    /// </summary>
    internal static string ExtractStringLiteralInner(string literal)
    {
        if (literal.Length >= 2 && literal[0] == '\'' && literal[^1] == '\'')
        {
            return literal[1..^1].Replace("''", "'");
        }
        return literal;
    }

    /// <summary>
    /// Checks whether a string value is numeric or a date literal.
    /// </summary>
    internal static bool IsNumericOrDateLiteral(string value)
    {
        if (decimal.TryParse(value, System.Globalization.NumberStyles.Any,
            System.Globalization.CultureInfo.InvariantCulture, out _))
        {
            return true;
        }

        if (DateTime.TryParse(value, System.Globalization.CultureInfo.InvariantCulture,
            System.Globalization.DateTimeStyles.None, out _))
        {
            return true;
        }

        // Simple date-like patterns: YYYY-MM-DD, MM/DD/YYYY, DD-MON-YYYY, etc.
        if (Regex.IsMatch(value, @"^\d{4}[-/]\d{2}[-/]\d{2}") ||
            Regex.IsMatch(value, @"^\d{2}[-/]\w{3}[-/]\d{4}"))
        {
            return true;
        }

        return false;
    }

    /// <summary>
    /// Replaces known identifiers within an optimizer hint comment.
    /// Preserves the hint structure: /*+ HINT_NAME(alias) */ -> /*+ HINT_NAME(new_alias) */
    /// </summary>
    private static string ReplaceIdentifiersInHint(
        string hint,
        ObfuscationContext context,
        string? filePath,
        Dictionary<string, SemanticCategory> discovered)
    {
        // Tokenize the hint content (between /*+ and */)
        if (hint.StartsWith("/*+") && hint.EndsWith("*/"))
        {
            var inner = hint[3..^2]; // strip /*+ and */
            var hintTokens = OracleTokenizer.Tokenize(inner);
            var result = new StringBuilder(inner);
            var replacements = new List<OracleReplacement>();

            foreach (var t in hintTokens)
            {
                if (t.Type == OracleTokenType.Identifier && !OracleReservedWords.ShouldPreserve(t.Value))
                {
                    var upper = t.NormalizedValue;
                    if (discovered.TryGetValue(upper, out var cat))
                    {
                        var alias = context.GetOrCreateAlias(t.Value, cat, filePath, t.Line, t.StartOffset, t.EndOffset);
                        replacements.Add(new OracleReplacement(t.StartOffset, t.Length, alias));
                    }
                    else if (context.Mappings.HasMapping(t.Value))
                    {
                        var alias = context.Mappings.Forward[t.Value];
                        replacements.Add(new OracleReplacement(t.StartOffset, t.Length, alias));
                    }
                }
            }

            // Apply replacements back-to-front
            foreach (var r in replacements.OrderByDescending(r => r.Offset))
            {
                if (r.Offset >= 0 && r.Offset + r.Length <= result.Length)
                {
                    result.Remove(r.Offset, r.Length);
                    result.Insert(r.Offset, r.NewValue);
                }
            }

            return $"/*+{result}*/";
        }

        return hint;
    }

    /// <summary>
    /// Removes overlapping replacements, keeping the one with the larger span.
    /// </summary>
    private static List<OracleReplacement> DeduplicateReplacements(List<OracleReplacement> replacements)
    {
        if (replacements.Count <= 1)
            return replacements;

        var sorted = replacements
            .OrderBy(r => r.Offset)
            .ThenByDescending(r => r.Length)
            .ToList();

        var result = new List<OracleReplacement>();
        var lastEnd = -1;

        foreach (var replacement in sorted)
        {
            if (replacement.Offset >= lastEnd)
            {
                result.Add(replacement);
                lastEnd = replacement.Offset + replacement.Length;
            }
        }

        return result;
    }

    /// <summary>
    /// Checks whether content contains Oracle/PL-SQL indicators (for .sql file detection).
    /// </summary>
    private static bool ContainsOracleIndicators(string content)
    {
        // Check for PL/SQL block structure or Oracle-specific keywords
        var indicators = new[]
        {
            "DECLARE", "BEGIN", "EXCEPTION", "DBMS_OUTPUT", "DBMS_SQL",
            "VARCHAR2", "PLS_INTEGER", "BINARY_INTEGER", "SYS_REFCURSOR",
            "PRAGMA", "AUTHID", "PIPELINED", "BULK COLLECT", "FORALL",
            "CREATE OR REPLACE PACKAGE", "CREATE OR REPLACE PROCEDURE",
            "CREATE OR REPLACE FUNCTION", "CREATE OR REPLACE TRIGGER",
            "%TYPE", "%ROWTYPE", "NOCOPY", "NVL(", "NVL2(",
            "DECODE(", "SYSDATE", "SYSTIMESTAMP", "ROWNUM",
            "CONNECT BY", "START WITH", "LEVEL"
        };

        foreach (var indicator in indicators)
        {
            if (content.Contains(indicator, StringComparison.OrdinalIgnoreCase))
                return true;
        }

        return false;
    }

    /// <summary>
    /// Regex fallback for deobfuscation when tokenizer-based approach fails or misses some replacements.
    /// Only adds replacements that don't overlap with existing ones.
    /// </summary>
    private static void DeobfuscateRegexFallback(
        string content,
        Dictionary<string, string> reverse,
        List<OracleReplacement> existingReplacements)
    {
        // Build a set of already-covered offsets
        var covered = new HashSet<int>();
        foreach (var r in existingReplacements)
        {
            for (var offset = r.Offset; offset < r.Offset + r.Length; offset++)
                covered.Add(offset);
        }

        foreach (var kvp in reverse.OrderByDescending(k => k.Key.Length))
        {
            var alias = kvp.Key;
            var original = kvp.Value;

            var pattern = @"(?<![A-Za-z0-9_])" + Regex.Escape(alias) + @"(?![A-Za-z0-9_])";
            foreach (Match match in Regex.Matches(content, pattern))
            {
                if (!covered.Contains(match.Index))
                {
                    existingReplacements.Add(new OracleReplacement(match.Index, match.Length, original));
                    for (var offset = match.Index; offset < match.Index + match.Length; offset++)
                        covered.Add(offset);
                }
            }
        }
    }

    /// <summary>
    /// Full regex-based deobfuscation fallback when tokenization fails entirely.
    /// </summary>
    private LanguageProcessingResult DeobfuscateByRegexOnly(
        string content,
        ObfuscationContext context,
        List<string> warnings)
    {
        var reverse = context.Mappings.Reverse;
        var result = content;
        var count = 0;

        foreach (var kvp in reverse.OrderByDescending(k => k.Key.Length))
        {
            var alias = kvp.Key;
            var original = kvp.Value;

            var pattern = @"(?<![A-Za-z0-9_])" + Regex.Escape(alias) + @"(?![A-Za-z0-9_])";
            var newResult = Regex.Replace(result, pattern, original);
            if (newResult != result)
            {
                count += Regex.Matches(result, pattern).Count;
                result = newResult;
            }
        }

        return new LanguageProcessingResult
        {
            Content = result,
            WasTransformed = count > 0,
            ReplacementCount = count,
            ProcessorId = ProcessorId,
            Warnings = warnings
        };
    }

    /// <summary>
    /// Returns a no-change result for empty/whitespace content.
    /// </summary>
    private LanguageProcessingResult NoChange(string content)
    {
        return new LanguageProcessingResult
        {
            Content = content,
            WasTransformed = false,
            ReplacementCount = 0,
            ProcessorId = ProcessorId
        };
    }
}
