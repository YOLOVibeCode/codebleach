using System.Text;
using System.Text.RegularExpressions;
using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using Microsoft.SqlServer.TransactSql.ScriptDom;

namespace CodeBleach.Processors.Sql;

/// <summary>
/// Represents a single text replacement to be applied to the source SQL.
/// Replacements are collected during visitor traversal, then applied back-to-front
/// to preserve character offsets.
/// </summary>
internal sealed record Replacement(int Offset, int Length, string NewValue);

#region SqlLanguageProcessor

/// <summary>
/// Language processor for T-SQL files (.sql). Uses Microsoft.SqlServer.TransactSql.ScriptDom
/// (TSql160Parser) for AST-aware parsing and a TSqlFragmentVisitor to collect all user-defined
/// identifiers. Replacements are applied from back-to-front to preserve character positions.
/// </summary>
public sealed class SqlLanguageProcessor : ILanguageProcessor
{
    public string ProcessorId => "tsql";
    public string DisplayName => "T-SQL (ScriptDom)";
    public IReadOnlySet<string> SupportedExtensions { get; } = new HashSet<string>(StringComparer.OrdinalIgnoreCase) { ".sql" };
    public int Priority => 10;

    public bool CanProcess(string filePath, string content)
    {
        var ext = Path.GetExtension(filePath);
        return SupportedExtensions.Contains(ext);
    }

    public LanguageProcessingResult Obfuscate(string content, ObfuscationContext context, string? filePath = null)
    {
        if (string.IsNullOrWhiteSpace(content) || context.Scope.IsDelegationOnly(ProcessorId))
        {
            return new LanguageProcessingResult
            {
                Content = content,
                WasTransformed = false,
                ReplacementCount = 0,
                ProcessorId = ProcessorId
            };
        }

        var warnings = new List<string>();

        TSqlFragment fragment;
        IList<ParseError> parseErrors;
        try
        {
            var parser = new TSql160Parser(initialQuotedIdentifiers: true);
            fragment = parser.Parse(new StringReader(content), out parseErrors);
        }
        catch (Exception ex)
        {
            warnings.Add($"ScriptDom parse failed: {ex.Message}. Returning content unmodified.");
            return new LanguageProcessingResult
            {
                Content = content,
                WasTransformed = false,
                ReplacementCount = 0,
                ProcessorId = ProcessorId,
                Warnings = warnings
            };
        }

        if (parseErrors.Count > 0)
        {
            foreach (var err in parseErrors)
            {
                warnings.Add($"Parse warning at line {err.Line}, col {err.Column}: {err.Message}");
            }
        }

        var visitor = new IdentifierCollectorVisitor(context, filePath, content);
        fragment.Accept(visitor);

        // Collect comment and string literal replacements from tokens
        CollectCommentAndStringReplacements(fragment, context, filePath, content, visitor.Replacements, warnings);

        // Deduplicate replacements that overlap (keep the outermost / first-added)
        var deduped = DeduplicateReplacements(visitor.Replacements);

        // Sort descending by offset to preserve positions
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

    public LanguageProcessingResult Deobfuscate(string content, ObfuscationContext context, string? filePath = null)
    {
        if (string.IsNullOrWhiteSpace(content))
        {
            return new LanguageProcessingResult
            {
                Content = content,
                WasTransformed = false,
                ReplacementCount = 0,
                ProcessorId = ProcessorId
            };
        }

        var warnings = new List<string>();
        var reverse = context.Mappings.Reverse;

        TSqlFragment fragment;
        try
        {
            var parser = new TSql160Parser(initialQuotedIdentifiers: true);
            fragment = parser.Parse(new StringReader(content), out _);
        }
        catch (Exception ex)
        {
            warnings.Add($"ScriptDom parse failed during deobfuscation: {ex.Message}. Attempting regex fallback.");
            return DeobfuscateByRegex(content, context, warnings);
        }

        // Walk the parsed tree, collecting identifiers that match alias patterns
        var replacements = new List<Replacement>();
        var deobfuscateVisitor = new DeobfuscateCollectorVisitor(context, replacements, content);
        fragment.Accept(deobfuscateVisitor);

        // Also handle string literals and comments via token scan
        CollectDeobfuscateTokenReplacements(fragment, reverse, content, replacements);

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

    /// <summary>
    /// Obfuscates a SQL fragment embedded in another language (e.g., COBOL EXEC SQL, C# string literal).
    /// Tolerates parse errors gracefully — returns input unchanged if parsing fails.
    /// </summary>
    public LanguageProcessingResult ObfuscateFragment(string sqlText, ObfuscationContext context, string? filePath = null)
    {
        if (string.IsNullOrWhiteSpace(sqlText))
        {
            return new LanguageProcessingResult
            {
                Content = sqlText,
                WasTransformed = false,
                ReplacementCount = 0,
                ProcessorId = ProcessorId
            };
        }

        TSqlFragment fragment;
        try
        {
            var parser = new TSql160Parser(initialQuotedIdentifiers: true);
            fragment = parser.Parse(new StringReader(sqlText), out _);
        }
        catch
        {
            return new LanguageProcessingResult
            {
                Content = sqlText,
                WasTransformed = false,
                ReplacementCount = 0,
                ProcessorId = ProcessorId
            };
        }

        var visitor = new IdentifierCollectorVisitor(context, filePath, sqlText);
        fragment.Accept(visitor);

        var deduped = DeduplicateReplacements(visitor.Replacements);
        var sorted = deduped.OrderByDescending(r => r.Offset).ToList();

        var result = new StringBuilder(sqlText);
        foreach (var replacement in sorted)
        {
            if (replacement.Offset >= 0 && replacement.Offset + replacement.Length <= result.Length)
            {
                result.Remove(replacement.Offset, replacement.Length);
                result.Insert(replacement.Offset, replacement.NewValue);
            }
        }

        return new LanguageProcessingResult
        {
            Content = result.ToString(),
            WasTransformed = sorted.Count > 0,
            ReplacementCount = sorted.Count,
            ProcessorId = ProcessorId
        };
    }

    public ValidationResult Validate(string obfuscatedContent)
    {
        if (string.IsNullOrWhiteSpace(obfuscatedContent))
        {
            return ValidationResult.Valid();
        }

        try
        {
            var parser = new TSql160Parser(initialQuotedIdentifiers: true);
            parser.Parse(new StringReader(obfuscatedContent), out var errors);

            if (errors.Count == 0)
            {
                return ValidationResult.Valid();
            }

            var errorMessages = errors.Select(e => $"Line {e.Line}, Col {e.Column}: {e.Message}").ToList();
            return ValidationResult.Invalid(errorMessages);
        }
        catch (Exception ex)
        {
            return ValidationResult.Invalid(new List<string> { $"Parse exception: {ex.Message}" });
        }
    }

    /// <summary>
    /// Scans all tokens from the parsed fragment to find comments and string literals.
    /// </summary>
    internal static void CollectCommentAndStringReplacements(
        TSqlFragment fragment,
        ObfuscationContext context,
        string? filePath,
        string content,
        List<Replacement> replacements,
        List<string> warnings)
    {
        foreach (var token in fragment.ScriptTokenStream)
        {
            switch (token.TokenType)
            {
                case TSqlTokenType.SingleLineComment:
                {
                    var offset = token.Offset;
                    var length = token.Text.Length;
                    replacements.Add(new Replacement(offset, length, "-- [Comment removed]"));
                    break;
                }

                case TSqlTokenType.MultilineComment:
                {
                    var offset = token.Offset;
                    var length = token.Text.Length;
                    replacements.Add(new Replacement(offset, length, "/* [Comment removed] */"));
                    break;
                }

                case TSqlTokenType.AsciiStringLiteral:
                case TSqlTokenType.UnicodeStringLiteral:
                {
                    var literalText = token.Text;
                    // Extract inner value (strip surrounding quotes)
                    var inner = ExtractStringLiteralInner(literalText);

                    // Only replace non-numeric, non-empty text
                    if (!string.IsNullOrEmpty(inner) && !IsNumericOrDateLiteral(inner))
                    {
                        var alias = context.GetOrCreateAlias(
                            inner,
                            SemanticCategory.StringLiteral,
                            filePath,
                            token.Line,
                            token.Column,
                            token.Column + token.Text.Length);

                        // Reconstruct the literal with the alias value
                        var prefix = literalText.StartsWith("N'", StringComparison.OrdinalIgnoreCase) ? "N'" : "'";
                        var newLiteral = $"{prefix}{alias}'";

                        replacements.Add(new Replacement(token.Offset, token.Text.Length, newLiteral));
                    }
                    break;
                }
            }
        }
    }

    /// <summary>
    /// Scans tokens from a parsed obfuscated fragment to find alias patterns in string literals
    /// and comment placeholders for deobfuscation.
    /// </summary>
    private static void CollectDeobfuscateTokenReplacements(
        TSqlFragment fragment,
        Dictionary<string, string> reverse,
        string content,
        List<Replacement> replacements)
    {
        foreach (var token in fragment.ScriptTokenStream)
        {
            switch (token.TokenType)
            {
                case TSqlTokenType.AsciiStringLiteral:
                case TSqlTokenType.UnicodeStringLiteral:
                {
                    var literalText = token.Text;
                    var inner = ExtractStringLiteralInner(literalText);
                    if (!string.IsNullOrEmpty(inner) && reverse.TryGetValue(inner, out var original))
                    {
                        var prefix = literalText.StartsWith("N'", StringComparison.OrdinalIgnoreCase) ? "N'" : "'";
                        var newLiteral = $"{prefix}{original}'";
                        replacements.Add(new Replacement(token.Offset, token.Text.Length, newLiteral));
                    }
                    break;
                }
            }
            // Comments were replaced with "[Comment removed]" -- we cannot restore them
        }
    }

    /// <summary>
    /// Extracts the inner value from a string literal, handling both regular and N-prefixed strings.
    /// </summary>
    internal static string ExtractStringLiteralInner(string literalText)
    {
        if (literalText.StartsWith("N'", StringComparison.OrdinalIgnoreCase) && literalText.EndsWith("'"))
        {
            return literalText[2..^1].Replace("''", "'");
        }
        if (literalText.StartsWith("'") && literalText.EndsWith("'") && literalText.Length >= 2)
        {
            return literalText[1..^1].Replace("''", "'");
        }
        return literalText;
    }

    /// <summary>
    /// Checks whether a string value is purely numeric or a date literal that should not be replaced.
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

        // Check for simple date-like patterns (YYYY-MM-DD, MM/DD/YYYY, etc.)
        if (Regex.IsMatch(value, @"^\d{4}[-/]\d{2}[-/]\d{2}"))
        {
            return true;
        }

        return false;
    }

    /// <summary>
    /// Removes overlapping replacements, keeping the one with the larger span (or first-added if equal).
    /// </summary>
    internal static List<Replacement> DeduplicateReplacements(List<Replacement> replacements)
    {
        if (replacements.Count <= 1)
            return replacements;

        // Sort by offset ascending, then by length descending (prefer larger span)
        var sorted = replacements
            .Select((r, i) => (Replacement: r, Index: i))
            .OrderBy(x => x.Replacement.Offset)
            .ThenByDescending(x => x.Replacement.Length)
            .ToList();

        var result = new List<Replacement>();
        var lastEnd = -1;

        foreach (var (replacement, _) in sorted)
        {
            if (replacement.Offset >= lastEnd)
            {
                result.Add(replacement);
                lastEnd = replacement.Offset + replacement.Length;
            }
            // else: overlaps with a previous replacement, skip it
        }

        return result;
    }

    /// <summary>
    /// Regex-based fallback for deobfuscation when the parser fails on obfuscated content.
    /// </summary>
    private LanguageProcessingResult DeobfuscateByRegex(string content, ObfuscationContext context, List<string> warnings)
    {
        var reverse = context.Mappings.Reverse;
        var result = content;
        var count = 0;

        // Sort aliases by length descending to avoid partial replacements
        foreach (var kvp in reverse.OrderByDescending(k => k.Key.Length))
        {
            var alias = kvp.Key;
            var original = kvp.Value;

            // Use word boundary matching to avoid partial replacements
            var pattern = @"(?<!\w)" + Regex.Escape(alias) + @"(?!\w)";
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
}

#endregion

#region IdentifierCollectorVisitor

/// <summary>
/// TSqlFragmentVisitor that walks the AST and collects all user-defined identifiers
/// along with their positions, mapping them to aliases via ObfuscationContext.
/// </summary>
internal sealed class IdentifierCollectorVisitor : TSqlFragmentVisitor
{
    private readonly ObfuscationContext _context;
    private readonly string? _filePath;
    private readonly string _content;

    /// <summary>
    /// Collected replacements to be applied to the source content.
    /// </summary>
    public List<Replacement> Replacements { get; } = new();

    /// <summary>
    /// Set of system schemas that should never be renamed.
    /// </summary>
    private static readonly HashSet<string> SystemSchemas = new(StringComparer.OrdinalIgnoreCase)
    {
        "sys", "dbo", "INFORMATION_SCHEMA", "guest", "db_owner", "db_datareader", "db_datawriter"
    };

    /// <summary>
    /// Set of built-in function names that should never be renamed.
    /// </summary>
    private static readonly HashSet<string> BuiltInFunctions = new(StringComparer.OrdinalIgnoreCase)
    {
        // Aggregate
        "COUNT", "SUM", "AVG", "MIN", "MAX", "COUNT_BIG", "STDEV", "STDEVP", "VAR", "VARP",
        "GROUPING", "GROUPING_ID", "STRING_AGG", "APPROX_COUNT_DISTINCT",
        // String
        "LEN", "DATALENGTH", "LEFT", "RIGHT", "SUBSTRING", "UPPER", "LOWER", "LTRIM", "RTRIM",
        "TRIM", "REPLACE", "REPLICATE", "REVERSE", "STUFF", "CHARINDEX", "PATINDEX",
        "CONCAT", "CONCAT_WS", "FORMAT", "STRING_ESCAPE", "STRING_SPLIT", "TRANSLATE",
        "CHAR", "ASCII", "UNICODE", "NCHAR", "QUOTENAME", "SPACE", "STR",
        // Date/time
        "GETDATE", "GETUTCDATE", "SYSDATETIME", "SYSUTCDATETIME", "SYSDATETIMEOFFSET",
        "DATEADD", "DATEDIFF", "DATEDIFF_BIG", "DATENAME", "DATEPART", "YEAR", "MONTH", "DAY",
        "EOMONTH", "DATEFROMPARTS", "DATETIME2FROMPARTS", "DATETIMEFROMPARTS",
        "DATETIMEOFFSETFROMPARTS", "SMALLDATETIMEFROMPARTS", "TIMEFROMPARTS",
        "ISDATE", "CURRENT_TIMESTAMP", "SWITCHOFFSET", "TODATETIMEOFFSET",
        // Conversion
        "CAST", "CONVERT", "TRY_CAST", "TRY_CONVERT", "PARSE", "TRY_PARSE",
        // NULL handling
        "ISNULL", "COALESCE", "NULLIF", "IIF",
        // Math
        "ABS", "CEILING", "FLOOR", "ROUND", "SIGN", "POWER", "SQRT", "LOG", "LOG10",
        "EXP", "PI", "RAND", "SQUARE", "SIN", "COS", "TAN", "ASIN", "ACOS", "ATAN", "ATN2",
        "DEGREES", "RADIANS",
        // System
        "NEWID", "NEWSEQUENTIALID", "SCOPE_IDENTITY", "IDENT_CURRENT", "IDENTITY",
        "@@IDENTITY", "@@ROWCOUNT", "@@ERROR", "@@TRANCOUNT", "@@FETCH_STATUS",
        "@@SPID", "@@VERSION", "@@SERVERNAME", "@@SERVICENAME",
        "ROW_NUMBER", "RANK", "DENSE_RANK", "NTILE", "LAG", "LEAD",
        "FIRST_VALUE", "LAST_VALUE", "PERCENT_RANK", "CUME_DIST",
        "OBJECT_ID", "OBJECT_NAME", "DB_ID", "DB_NAME", "SCHEMA_ID", "SCHEMA_NAME",
        "TYPE_ID", "TYPE_NAME", "COL_NAME", "COL_LENGTH", "INDEX_COL",
        "APP_NAME", "HOST_NAME", "SUSER_SNAME", "SUSER_SID", "USER_NAME", "USER_ID",
        "SYSTEM_USER", "SESSION_USER", "CURRENT_USER", "ORIGINAL_LOGIN",
        "IS_MEMBER", "IS_SRVROLEMEMBER", "HAS_PERMS_BY_NAME", "PERMISSIONS",
        "ERROR_NUMBER", "ERROR_MESSAGE", "ERROR_SEVERITY", "ERROR_STATE",
        "ERROR_LINE", "ERROR_PROCEDURE", "XACT_STATE",
        // JSON
        "JSON_VALUE", "JSON_QUERY", "JSON_MODIFY", "ISJSON", "JSON_PATH_EXISTS",
        "OPENJSON", "JSON_OBJECT", "JSON_ARRAY",
        // XML
        "OPENXML",
        // Crypto
        "HASHBYTES", "CHECKSUM", "BINARY_CHECKSUM",
        // Other
        "CHOOSE", "GREATEST", "LEAST", "GENERATE_SERIES"
    };

    /// <summary>
    /// SQL data type keywords that should never be renamed.
    /// </summary>
    private static readonly HashSet<string> DataTypes = new(StringComparer.OrdinalIgnoreCase)
    {
        "INT", "INTEGER", "BIGINT", "SMALLINT", "TINYINT",
        "DECIMAL", "NUMERIC", "FLOAT", "REAL", "MONEY", "SMALLMONEY",
        "BIT", "CHAR", "VARCHAR", "NCHAR", "NVARCHAR", "TEXT", "NTEXT",
        "BINARY", "VARBINARY", "IMAGE",
        "DATE", "TIME", "DATETIME", "DATETIME2", "DATETIMEOFFSET", "SMALLDATETIME",
        "TIMESTAMP", "ROWVERSION",
        "UNIQUEIDENTIFIER", "XML", "SQL_VARIANT", "HIERARCHYID", "GEOMETRY", "GEOGRAPHY",
        "SYSNAME", "CURSOR", "TABLE"
    };

    /// <summary>
    /// Tracks SchemaObjectName nodes we have already visited to avoid double-processing.
    /// </summary>
    private readonly HashSet<SchemaObjectName> _visitedSchemaObjects = new(ReferenceEqualityComparer.Instance);

    /// <summary>
    /// Tracks column reference nodes we have already visited.
    /// </summary>
    private readonly HashSet<ColumnReferenceExpression> _visitedColumnRefs = new(ReferenceEqualityComparer.Instance);

    public IdentifierCollectorVisitor(ObfuscationContext context, string? filePath, string content)
    {
        _context = context;
        _filePath = filePath;
        _content = content;
    }

    #region SchemaObjectName (tables, views, stored procs, functions)

    public override void Visit(SchemaObjectName node)
    {
        if (!_visitedSchemaObjects.Add(node))
            return;

        // Multi-part name: [ServerIdentifier].[DatabaseIdentifier].[SchemaIdentifier].[BaseIdentifier]
        if (node.ServerIdentifier != null)
        {
            AddIdentifierReplacement(node.ServerIdentifier, SemanticCategory.Server);
        }

        if (node.DatabaseIdentifier != null)
        {
            AddIdentifierReplacement(node.DatabaseIdentifier, SemanticCategory.Database);
        }

        if (node.SchemaIdentifier != null && !IsSystemSchema(node.SchemaIdentifier.Value))
        {
            AddIdentifierReplacement(node.SchemaIdentifier, SemanticCategory.Schema);
        }

        if (node.BaseIdentifier != null)
        {
            var name = node.BaseIdentifier.Value;
            var category = DetermineObjectCategory(node);

            // Skip system objects (anything under sys or INFORMATION_SCHEMA)
            if (node.SchemaIdentifier != null && IsSystemSchema(node.SchemaIdentifier.Value))
            {
                // Don't rename base identifier of system objects
                return;
            }

            // Handle temp tables
            if (name.StartsWith("#"))
            {
                category = SemanticCategory.TempTable;
            }

            if (!IsBuiltInFunction(name) && !IsDataType(name))
            {
                AddIdentifierReplacement(node.BaseIdentifier, category);
            }
        }
    }

    #endregion

    #region ColumnReferenceExpression

    public override void Visit(ColumnReferenceExpression node)
    {
        if (!_visitedColumnRefs.Add(node))
            return;

        if (node.MultiPartIdentifier?.Identifiers == null || node.MultiPartIdentifier.Identifiers.Count == 0)
            return;

        var identifiers = node.MultiPartIdentifier.Identifiers;

        if (node.ColumnType == ColumnType.Wildcard)
            return; // Don't rename *

        // Multi-part column reference: [schema.]table.column or alias.column
        if (identifiers.Count >= 2)
        {
            // Last identifier is the column name
            var columnIdent = identifiers[^1];
            if (!IsBuiltInFunction(columnIdent.Value) && !IsDataType(columnIdent.Value))
            {
                AddIdentifierReplacement(columnIdent, SemanticCategory.Column);
            }

            // Preceding identifiers could be table alias, table name, schema, etc.
            // We only rename them if they are not system schemas
            for (int i = 0; i < identifiers.Count - 1; i++)
            {
                var ident = identifiers[i];
                if (!IsSystemSchema(ident.Value) && !IsBuiltInFunction(ident.Value))
                {
                    // Could be table alias or table name -- use Variable for aliases (consistent mapping)
                    // The actual resolution depends on context; we use Table as the category since
                    // these are table/alias qualifiers. If it was registered as an alias, the mapping
                    // table will return the same alias.
                    AddIdentifierReplacement(ident, SemanticCategory.Table);
                }
            }
        }
        else if (identifiers.Count == 1)
        {
            // Single column name
            var columnIdent = identifiers[0];
            if (!IsBuiltInFunction(columnIdent.Value) && !IsDataType(columnIdent.Value))
            {
                AddIdentifierReplacement(columnIdent, SemanticCategory.Column);
            }
        }
    }

    #endregion

    #region Variables (@var)

    public override void Visit(VariableReference node)
    {
        if (string.IsNullOrEmpty(node.Name))
            return;

        // VariableReference.Name does NOT include the @ prefix, but the token in the source does.
        // The fragment position covers the full @name text.
        var name = node.Name;
        var alias = _context.GetOrCreateAlias(
            "@" + name,
            SemanticCategory.Variable,
            _filePath,
            node.StartLine,
            node.StartColumn,
            node.StartColumn + node.FragmentLength);

        // The alias is like "VAR_0" -- we need to prepend @ in the output
        var newValue = "@" + alias;
        AddReplacement(node.StartOffset, node.FragmentLength, newValue);
    }

    public override void Visit(DeclareVariableElement node)
    {
        // DeclareVariableElement.VariableName is an Identifier (without @).
        // The fragment for the element starts at the @ token.
        // We need to replace the full @varname token.
        if (node.VariableName != null && !string.IsNullOrEmpty(node.VariableName.Value))
        {
            var name = node.VariableName.Value;
            var alias = _context.GetOrCreateAlias(
                "@" + name,
                SemanticCategory.Variable,
                _filePath,
                node.StartLine,
                node.StartColumn,
                node.StartColumn + name.Length + 1);

            // Scan from the element's StartOffset to find the full @varname token
            var offset = node.StartOffset;
            var tokenLength = FindParameterTokenLength(offset, name);
            var newValue = "@" + alias;
            AddReplacement(offset, tokenLength, newValue);
        }
    }

    #endregion

    #region Cursor

    public override void Visit(DeclareCursorStatement node)
    {
        // DeclareCursorStatement.Name is of type Identifier.
        if (node.Name != null && !string.IsNullOrEmpty(node.Name.Value))
        {
            AddIdentifierReplacement(node.Name, SemanticCategory.Cursor);
        }
    }

    public override void Visit(OpenCursorStatement node)
    {
        // OpenCursorStatement.Cursor is of type CursorId.
        // CursorId contains an Identifier for the cursor name.
        HandleCursorId(node.Cursor);
    }

    public override void Visit(CloseCursorStatement node)
    {
        HandleCursorId(node.Cursor);
    }

    public override void Visit(FetchCursorStatement node)
    {
        HandleCursorId(node.Cursor);
    }

    public override void Visit(DeallocateCursorStatement node)
    {
        HandleCursorId(node.Cursor);
    }

    private void HandleCursorId(CursorId? cursorId)
    {
        if (cursorId == null)
            return;

        // CursorId.Name is an Identifier in ScriptDom
        // Walk its children to find the Identifier
        var extractor = new CursorNameExtractor();
        cursorId.Accept(extractor);
        if (extractor.FoundIdentifier != null)
        {
            AddIdentifierReplacement(extractor.FoundIdentifier, SemanticCategory.Cursor);
        }
    }

    #endregion

    #region CTE

    public override void Visit(CommonTableExpression node)
    {
        if (node.ExpressionName != null && !string.IsNullOrEmpty(node.ExpressionName.Value))
        {
            AddIdentifierReplacement(node.ExpressionName, SemanticCategory.CTE);
        }
    }

    #endregion

    #region Procedure/Function definitions

    public override void Visit(ProcedureParameter node)
    {
        if (node.VariableName != null && !string.IsNullOrEmpty(node.VariableName.Value))
        {
            var name = node.VariableName.Value;
            var alias = _context.GetOrCreateAlias(
                "@" + name,
                SemanticCategory.Parameter,
                _filePath,
                node.StartLine,
                node.StartColumn,
                node.StartColumn + name.Length + 1);

            // In ScriptDom, the ProcedureParameter fragment starts at the @ token.
            // We scan from the fragment's StartOffset to find the full @paramName span.
            var offset = node.StartOffset;
            var paramLength = FindParameterTokenLength(offset, name);
            var newValue = "@" + alias;
            AddReplacement(offset, paramLength, newValue);
        }
    }

    #endregion

    #region Table aliases (FROM Table t, FROM Table AS t)

    public override void Visit(NamedTableReference node)
    {
        // Handle the table alias
        if (node.Alias != null && !string.IsNullOrEmpty(node.Alias.Value))
        {
            AddIdentifierReplacement(node.Alias, SemanticCategory.Variable);
        }

        // The table name itself is handled by Visit(SchemaObjectName)
    }

    public override void Visit(QueryDerivedTable node)
    {
        if (node.Alias != null && !string.IsNullOrEmpty(node.Alias.Value))
        {
            AddIdentifierReplacement(node.Alias, SemanticCategory.Variable);
        }
    }

    #endregion

    #region Stored Procedure Execution

    public override void Visit(ExecuteStatement node)
    {
        // Mark procedure names referenced in EXEC statements with StoredProc category.
        // We walk the ExecuteSpecification to find the SchemaObjectName.
        var entity = node.ExecuteSpecification?.ExecutableEntity as ExecutableProcedureReference;
        if (entity?.ProcedureReference == null)
            return;

        // Walk the tokens belonging to the ProcedureReference fragment to find all identifiers.
        // The SchemaObjectName is a child of ProcedureReferenceName and will be picked up by
        // Visit(SchemaObjectName), but we pre-register it here with StoredProc category.
        // Find the SchemaObjectName child by walking the fragment's children via visitor pattern.
        var innerVisitor = new ProcNameExtractor();
        entity.ProcedureReference.Accept(innerVisitor);

        if (innerVisitor.FoundName != null)
        {
            var schemaObj = innerVisitor.FoundName;
            if (_visitedSchemaObjects.Add(schemaObj))
            {
                if (schemaObj.SchemaIdentifier != null && !IsSystemSchema(schemaObj.SchemaIdentifier.Value))
                {
                    AddIdentifierReplacement(schemaObj.SchemaIdentifier, SemanticCategory.Schema);
                }

                if (schemaObj.DatabaseIdentifier != null)
                {
                    AddIdentifierReplacement(schemaObj.DatabaseIdentifier, SemanticCategory.Database);
                }

                if (schemaObj.ServerIdentifier != null)
                {
                    AddIdentifierReplacement(schemaObj.ServerIdentifier, SemanticCategory.Server);
                }

                if (schemaObj.BaseIdentifier != null &&
                    !IsBuiltInFunction(schemaObj.BaseIdentifier.Value) &&
                    !IsSystemProcedure(schemaObj.BaseIdentifier.Value))
                {
                    AddIdentifierReplacement(schemaObj.BaseIdentifier, SemanticCategory.StoredProc);
                }
            }
        }
    }

    #endregion

    #region CREATE PROCEDURE / CREATE FUNCTION

    public override void Visit(CreateProcedureStatement node)
    {
        if (node.ProcedureReference?.Name != null)
        {
            var schemaObj = node.ProcedureReference.Name;
            if (!_visitedSchemaObjects.Contains(schemaObj))
            {
                _visitedSchemaObjects.Add(schemaObj);

                if (schemaObj.SchemaIdentifier != null && !IsSystemSchema(schemaObj.SchemaIdentifier.Value))
                    AddIdentifierReplacement(schemaObj.SchemaIdentifier, SemanticCategory.Schema);
                if (schemaObj.DatabaseIdentifier != null)
                    AddIdentifierReplacement(schemaObj.DatabaseIdentifier, SemanticCategory.Database);
                if (schemaObj.BaseIdentifier != null)
                    AddIdentifierReplacement(schemaObj.BaseIdentifier, SemanticCategory.StoredProc);
            }
        }
    }

    public override void Visit(CreateFunctionStatement node)
    {
        if (node.Name != null)
        {
            var schemaObj = node.Name;
            if (!_visitedSchemaObjects.Contains(schemaObj))
            {
                _visitedSchemaObjects.Add(schemaObj);

                if (schemaObj.SchemaIdentifier != null && !IsSystemSchema(schemaObj.SchemaIdentifier.Value))
                    AddIdentifierReplacement(schemaObj.SchemaIdentifier, SemanticCategory.Schema);
                if (schemaObj.DatabaseIdentifier != null)
                    AddIdentifierReplacement(schemaObj.DatabaseIdentifier, SemanticCategory.Database);
                if (schemaObj.BaseIdentifier != null)
                    AddIdentifierReplacement(schemaObj.BaseIdentifier, SemanticCategory.Function);
            }
        }
    }

    #endregion

    #region Label (GOTO targets)

    public override void Visit(GoToStatement node)
    {
        // Label names -- these are user-defined, rename them
        if (!string.IsNullOrEmpty(node.LabelName?.Value))
        {
            AddIdentifierReplacement(node.LabelName, SemanticCategory.Variable);
        }
    }

    public override void Visit(LabelStatement node)
    {
        // LabelStatement.Value is a string (the label name without the colon).
        // The fragment in the source is "labelname:" so we need to replace just the label name part.
        if (!string.IsNullOrEmpty(node.Value))
        {
            var labelName = node.Value;
            var alias = _context.GetOrCreateAlias(
                labelName,
                SemanticCategory.Variable,
                _filePath,
                node.StartLine,
                node.StartColumn,
                node.StartColumn + labelName.Length);

            // Replace just the label name (not the colon)
            AddReplacement(node.StartOffset, labelName.Length, alias);
        }
    }

    #endregion

    #region Helper methods

    private void AddIdentifierReplacement(Identifier identifier, SemanticCategory category)
    {
        if (identifier == null || string.IsNullOrEmpty(identifier.Value))
            return;

        var originalName = identifier.Value;

        // Skip SQL keywords, built-in functions, and data types
        if (IsBuiltInFunction(originalName) || IsDataType(originalName))
            return;

        var alias = _context.GetOrCreateAlias(
            originalName,
            category,
            _filePath,
            identifier.StartLine,
            identifier.StartColumn,
            identifier.StartColumn + identifier.FragmentLength);

        // Determine the replacement text based on how the identifier is quoted in source
        var offset = identifier.StartOffset;
        var length = identifier.FragmentLength;

        // Check what the actual source text looks like
        var sourceText = _content.Substring(offset, Math.Min(length, _content.Length - offset));

        string newValue;
        if (sourceText.StartsWith("[") && sourceText.EndsWith("]"))
        {
            // Bracket-delimited identifier: [Name] -> [Alias]
            newValue = $"[{alias}]";
        }
        else if (sourceText.StartsWith("\"") && sourceText.EndsWith("\""))
        {
            // Quoted identifier: "Name" -> "Alias"
            newValue = $"\"{alias}\"";
        }
        else
        {
            newValue = alias;
        }

        AddReplacement(offset, length, newValue);
    }

    private void AddReplacement(int offset, int length, string newValue)
    {
        if (offset >= 0 && length > 0 && offset + length <= _content.Length)
        {
            Replacements.Add(new Replacement(offset, length, newValue));
        }
    }

    private int FindParameterTokenLength(int offset, string paramName)
    {
        // The parameter token starts at offset and includes @paramName
        // Walk from offset to find the full @paramName token
        if (offset < _content.Length && _content[offset] == '@')
        {
            var end = offset + 1;
            while (end < _content.Length && (char.IsLetterOrDigit(_content[end]) || _content[end] == '_'))
            {
                end++;
            }
            return end - offset;
        }
        // Fallback: use name length + 1 for @
        return paramName.Length + 1;
    }

    /// <summary>
    /// Determines whether a SchemaObjectName should be categorized as Table, StoredProc, Function, etc.
    /// based on its parent AST context.
    /// </summary>
    private static SemanticCategory DetermineObjectCategory(SchemaObjectName node)
    {
        // Walk up to find the parent statement type
        // Since ScriptDom doesn't provide parent references, we default to Table
        // and rely on the specific Visit overrides for procs/functions to set the correct category
        return SemanticCategory.Table;
    }

    private static bool IsSystemSchema(string name)
    {
        return SystemSchemas.Contains(name);
    }

    private static bool IsBuiltInFunction(string name)
    {
        return BuiltInFunctions.Contains(name);
    }

    private static bool IsDataType(string name)
    {
        return DataTypes.Contains(name);
    }

    private static bool IsSystemProcedure(string name)
    {
        // System stored procedures start with sp_ or xp_ in SQL Server
        return name.StartsWith("sp_", StringComparison.OrdinalIgnoreCase) ||
               name.StartsWith("xp_", StringComparison.OrdinalIgnoreCase) ||
               name.StartsWith("fn_", StringComparison.OrdinalIgnoreCase);
    }

    #endregion
}

#endregion

#region ProcNameExtractor

/// <summary>
/// Small visitor used to find the SchemaObjectName inside a ProcedureReferenceName fragment.
/// </summary>
internal sealed class ProcNameExtractor : TSqlFragmentVisitor
{
    public SchemaObjectName? FoundName { get; private set; }

    public override void Visit(SchemaObjectName node)
    {
        FoundName ??= node;
    }
}

#endregion

#region CursorNameExtractor

/// <summary>
/// Small visitor used to find the Identifier inside a CursorId fragment.
/// </summary>
internal sealed class CursorNameExtractor : TSqlFragmentVisitor
{
    public Identifier? FoundIdentifier { get; private set; }

    public override void Visit(Identifier node)
    {
        FoundIdentifier ??= node;
    }
}

#endregion

#region DeobfuscateCollectorVisitor

/// <summary>
/// TSqlFragmentVisitor that walks the AST of obfuscated SQL and collects positions
/// of identifiers that match alias patterns for reverse mapping.
/// </summary>
internal sealed class DeobfuscateCollectorVisitor : TSqlFragmentVisitor
{
    private readonly List<Replacement> _replacements;
    private readonly string _content;
    private readonly Dictionary<string, string> _reverse;

    public DeobfuscateCollectorVisitor(ObfuscationContext context, List<Replacement> replacements, string content)
    {
        _replacements = replacements;
        _content = content;
        _reverse = context.Mappings.Reverse;
    }

    public override void Visit(Identifier node)
    {
        TryReverseIdentifier(node);
    }

    public override void Visit(VariableReference node)
    {
        if (string.IsNullOrEmpty(node.Name))
            return;

        // The variable in source is @VAR_0, the alias stored in reverse is @VAR_0 -> @originalName
        var aliasWithAt = "@" + node.Name;
        if (_reverse.TryGetValue(aliasWithAt, out var original))
        {
            _replacements.Add(new Replacement(node.StartOffset, node.FragmentLength, original));
        }
    }

    private void TryReverseIdentifier(Identifier identifier)
    {
        if (identifier == null || string.IsNullOrEmpty(identifier.Value))
            return;

        var aliasValue = identifier.Value;
        if (!_reverse.TryGetValue(aliasValue, out var original))
            return;

        var offset = identifier.StartOffset;
        var length = identifier.FragmentLength;
        var sourceText = _content.Substring(offset, Math.Min(length, _content.Length - offset));

        string newValue;
        if (sourceText.StartsWith("[") && sourceText.EndsWith("]"))
        {
            newValue = $"[{original}]";
        }
        else if (sourceText.StartsWith("\"") && sourceText.EndsWith("\""))
        {
            newValue = $"\"{original}\"";
        }
        else
        {
            newValue = original;
        }

        _replacements.Add(new Replacement(offset, length, newValue));
    }
}

#endregion

#region Db2SqlLanguageProcessor

/// <summary>
/// Language processor for DB2 SQL files (.db2). Extends the T-SQL processor with
/// DB2-specific syntax handling. Uses ScriptDom where possible for standard SQL,
/// and falls back to regex-based tokenization for DB2-specific DDL.
/// </summary>
public sealed class Db2SqlLanguageProcessor : ILanguageProcessor
{
    public string ProcessorId => "db2sql";
    public string DisplayName => "DB2 SQL";
    public IReadOnlySet<string> SupportedExtensions { get; } = new HashSet<string>(StringComparer.OrdinalIgnoreCase) { ".db2" };
    public int Priority => 5;

    /// <summary>
    /// DB2-specific keywords for content detection.
    /// </summary>
    private static readonly HashSet<string> Db2Keywords = new(StringComparer.OrdinalIgnoreCase)
    {
        "TABLESPACE", "BUFFERPOOL", "STOGROUP", "COLLECTION", "PACKAGE",
        "LOCKSIZE", "CCSID", "DATABASE", "DSNDB", "ACQUIRE", "RELEASE",
        "BIND", "REBIND", "EXPLAIN", "RUNSTATS", "REORG", "QUIESCE"
    };

    /// <summary>
    /// Regex patterns for DB2-specific constructs.
    /// </summary>
    private static readonly Regex HostVariablePattern = new(
        @"(?<=\s|,|\(|=):([A-Za-z_][A-Za-z0-9_\-]*)",
        RegexOptions.Compiled);

    private static readonly Regex TablespacePattern = new(
        @"(?:IN|TABLESPACE)\s+(?:DATABASE\s+)?([A-Za-z_][A-Za-z0-9_]*)",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private static readonly Regex BufferPoolPattern = new(
        @"BUFFERPOOL\s+([A-Za-z_][A-Za-z0-9_]*)",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private static readonly Regex StorageGroupPattern = new(
        @"(?:STOGROUP|USING\s+STOGROUP)\s+([A-Za-z_][A-Za-z0-9_]*)",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private static readonly Regex CollectionPattern = new(
        @"COLLECTION\s+([A-Za-z_][A-Za-z0-9_]*)",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private static readonly Regex PackagePattern = new(
        @"PACKAGE\s+(?:([A-Za-z_][A-Za-z0-9_]*)\.)?([A-Za-z_][A-Za-z0-9_]*)",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private static readonly Regex PlanPattern = new(
        @"PLAN\s*\(\s*([A-Za-z_][A-Za-z0-9_]*)\s*\)",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private static readonly Regex SingleLineCommentPattern = new(
        @"--[^\r\n]*",
        RegexOptions.Compiled);

    private static readonly Regex MultiLineCommentPattern = new(
        @"/\*[\s\S]*?\*/",
        RegexOptions.Compiled);

    private static readonly Regex StringLiteralPattern = new(
        @"(?:N?'(?:[^']|'')*')",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    public bool CanProcess(string filePath, string content)
    {
        var ext = Path.GetExtension(filePath);
        if (SupportedExtensions.Contains(ext))
            return true;

        // Also check content for DB2-specific keywords even if extension is .sql
        if (string.Equals(ext, ".sql", StringComparison.OrdinalIgnoreCase))
        {
            return ContainsDb2Keywords(content);
        }

        return false;
    }

    public LanguageProcessingResult Obfuscate(string content, ObfuscationContext context, string? filePath = null)
    {
        if (string.IsNullOrWhiteSpace(content) || context.Scope.IsDelegationOnly(ProcessorId))
        {
            return new LanguageProcessingResult
            {
                Content = content,
                WasTransformed = false,
                ReplacementCount = 0,
                ProcessorId = ProcessorId
            };
        }

        var warnings = new List<string>();
        var replacements = new List<Replacement>();

        // Try ScriptDom parsing first for standard SQL portions
        bool scriptDomSucceeded = false;
        try
        {
            var parser = new TSql160Parser(initialQuotedIdentifiers: true);
            var fragment = parser.Parse(new StringReader(content), out var parseErrors);

            if (parseErrors.Count == 0 || parseErrors.Count <= 5)
            {
                // ScriptDom could parse (or mostly parse) the content
                scriptDomSucceeded = true;

                if (parseErrors.Count > 0)
                {
                    foreach (var err in parseErrors)
                    {
                        warnings.Add($"DB2 parse note at line {err.Line}: {err.Message}");
                    }
                }

                var visitor = new IdentifierCollectorVisitor(context, filePath, content);
                fragment.Accept(visitor);
                replacements.AddRange(visitor.Replacements);

                // Collect comments and string literals from ScriptDom tokens
                SqlLanguageProcessor.CollectCommentAndStringReplacements(
                    fragment, context, filePath, content, replacements, warnings);
            }
        }
        catch (Exception ex)
        {
            warnings.Add($"ScriptDom parse failed for DB2 content: {ex.Message}. Using regex fallback.");
        }

        // Always do DB2-specific regex pass for constructs ScriptDom doesn't understand
        CollectDb2SpecificReplacements(content, context, filePath, replacements, warnings);

        if (!scriptDomSucceeded)
        {
            // Full regex fallback for comments and strings when ScriptDom failed
            CollectRegexCommentAndStringReplacements(content, context, filePath, replacements);
        }

        // Deduplicate and sort
        var deduped = SqlLanguageProcessor.DeduplicateReplacements(replacements);
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

    public LanguageProcessingResult Deobfuscate(string content, ObfuscationContext context, string? filePath = null)
    {
        if (string.IsNullOrWhiteSpace(content))
        {
            return new LanguageProcessingResult
            {
                Content = content,
                WasTransformed = false,
                ReplacementCount = 0,
                ProcessorId = ProcessorId
            };
        }

        var warnings = new List<string>();
        var reverse = context.Mappings.Reverse;

        // For deobfuscation, do a simple regex-based replacement of all aliases back to originals
        var result = content;
        var count = 0;

        // Sort aliases by length descending to avoid partial matches
        foreach (var kvp in reverse.OrderByDescending(k => k.Key.Length))
        {
            var alias = kvp.Key;
            var original = kvp.Value;

            // Handle host variables: :ALIAS -> :original
            var hostVarPattern = @"(?<=:)" + Regex.Escape(alias) + @"(?!\w)";
            var hostVarResult = Regex.Replace(result, hostVarPattern, original);
            if (hostVarResult != result)
            {
                count += Regex.Matches(result, hostVarPattern).Count;
                result = hostVarResult;
            }

            // Handle @variable aliases
            if (alias.StartsWith("@"))
            {
                var varPattern = Regex.Escape(alias) + @"(?!\w)";
                var varResult = Regex.Replace(result, varPattern, original);
                if (varResult != result)
                {
                    count += Regex.Matches(result, varPattern).Count;
                    result = varResult;
                }
                continue;
            }

            // Handle regular identifiers (with word boundaries)
            var pattern = @"(?<![A-Za-z0-9_])" + Regex.Escape(alias) + @"(?![A-Za-z0-9_])";
            var newResult = Regex.Replace(result, pattern, original);
            if (newResult != result)
            {
                count += Regex.Matches(result, pattern).Count;
                result = newResult;
            }

            // Also handle bracket-delimited: [ALIAS] -> [original]
            var bracketPattern = @"\[" + Regex.Escape(alias) + @"\]";
            var bracketResult = Regex.Replace(result, bracketPattern, $"[{original}]");
            if (bracketResult != result)
            {
                count += Regex.Matches(result, bracketPattern).Count;
                result = bracketResult;
            }

            // Handle string literals: 'ALIAS' -> 'original'
            var stringPattern = @"'" + Regex.Escape(alias) + @"'";
            var stringResult = Regex.Replace(result, stringPattern, $"'{original}'");
            if (stringResult != result)
            {
                count += Regex.Matches(result, stringPattern).Count;
                result = stringResult;
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

    public ValidationResult Validate(string obfuscatedContent)
    {
        if (string.IsNullOrWhiteSpace(obfuscatedContent))
        {
            return ValidationResult.Valid();
        }

        // DB2 SQL may not fully parse with ScriptDom, so we do a best-effort check
        try
        {
            var parser = new TSql160Parser(initialQuotedIdentifiers: true);
            parser.Parse(new StringReader(obfuscatedContent), out var errors);

            if (errors.Count == 0)
            {
                return ValidationResult.Valid();
            }

            // For DB2, we're more lenient -- only report errors that seem structural
            var structuralErrors = errors
                .Where(e => !IsDb2SpecificParseError(e))
                .Select(e => $"Line {e.Line}, Col {e.Column}: {e.Message}")
                .ToList();

            if (structuralErrors.Count == 0)
            {
                return ValidationResult.Valid();
            }

            return ValidationResult.Invalid(structuralErrors);
        }
        catch (Exception ex)
        {
            return ValidationResult.Invalid(new List<string> { $"Parse exception: {ex.Message}" });
        }
    }

    /// <summary>
    /// Collects replacements for DB2-specific syntax constructs using regex.
    /// </summary>
    private void CollectDb2SpecificReplacements(
        string content,
        ObfuscationContext context,
        string? filePath,
        List<Replacement> replacements,
        List<string> warnings)
    {
        // Host variables: :varname
        foreach (Match match in HostVariablePattern.Matches(content))
        {
            var varName = match.Groups[1].Value;
            var alias = context.GetOrCreateAlias(
                varName,
                SemanticCategory.HostVariable,
                filePath,
                GetLineNumber(content, match.Index),
                match.Index,
                match.Index + match.Length);

            // Replace just the variable name part (after the colon)
            replacements.Add(new Replacement(
                match.Groups[1].Index,
                match.Groups[1].Length,
                alias));
        }

        // TABLESPACE references
        foreach (Match match in TablespacePattern.Matches(content))
        {
            var name = match.Groups[1].Value;
            if (!IsInsideStringOrComment(content, match.Index))
            {
                var alias = context.GetOrCreateAlias(name, SemanticCategory.Tablespace, filePath);
                replacements.Add(new Replacement(
                    match.Groups[1].Index,
                    match.Groups[1].Length,
                    alias));
            }
        }

        // BUFFERPOOL references
        foreach (Match match in BufferPoolPattern.Matches(content))
        {
            var name = match.Groups[1].Value;
            if (!IsInsideStringOrComment(content, match.Index))
            {
                var alias = context.GetOrCreateAlias(name, SemanticCategory.BufferPool, filePath);
                replacements.Add(new Replacement(
                    match.Groups[1].Index,
                    match.Groups[1].Length,
                    alias));
            }
        }

        // STOGROUP references
        foreach (Match match in StorageGroupPattern.Matches(content))
        {
            var name = match.Groups[1].Value;
            if (!IsInsideStringOrComment(content, match.Index))
            {
                var alias = context.GetOrCreateAlias(name, SemanticCategory.StorageGroup, filePath);
                replacements.Add(new Replacement(
                    match.Groups[1].Index,
                    match.Groups[1].Length,
                    alias));
            }
        }

        // COLLECTION references
        foreach (Match match in CollectionPattern.Matches(content))
        {
            var name = match.Groups[1].Value;
            if (!IsInsideStringOrComment(content, match.Index))
            {
                var alias = context.GetOrCreateAlias(name, SemanticCategory.Collection, filePath);
                replacements.Add(new Replacement(
                    match.Groups[1].Index,
                    match.Groups[1].Length,
                    alias));
            }
        }

        // PACKAGE references (may include collection prefix: collection.package)
        foreach (Match match in PackagePattern.Matches(content))
        {
            if (IsInsideStringOrComment(content, match.Index))
                continue;

            // Group 1 = collection prefix (optional), Group 2 = package name
            if (match.Groups[1].Success)
            {
                var collName = match.Groups[1].Value;
                var collAlias = context.GetOrCreateAlias(collName, SemanticCategory.Collection, filePath);
                replacements.Add(new Replacement(
                    match.Groups[1].Index,
                    match.Groups[1].Length,
                    collAlias));
            }

            var pkgName = match.Groups[2].Value;
            var pkgAlias = context.GetOrCreateAlias(pkgName, SemanticCategory.Db2Package, filePath);
            replacements.Add(new Replacement(
                match.Groups[2].Index,
                match.Groups[2].Length,
                pkgAlias));
        }

        // PLAN references
        foreach (Match match in PlanPattern.Matches(content))
        {
            var name = match.Groups[1].Value;
            if (!IsInsideStringOrComment(content, match.Index))
            {
                var alias = context.GetOrCreateAlias(name, SemanticCategory.Plan, filePath);
                replacements.Add(new Replacement(
                    match.Groups[1].Index,
                    match.Groups[1].Length,
                    alias));
            }
        }
    }

    /// <summary>
    /// Regex-based collection of comments and string literals when ScriptDom parsing fails.
    /// </summary>
    private void CollectRegexCommentAndStringReplacements(
        string content,
        ObfuscationContext context,
        string? filePath,
        List<Replacement> replacements)
    {
        // Multi-line comments
        foreach (Match match in MultiLineCommentPattern.Matches(content))
        {
            replacements.Add(new Replacement(match.Index, match.Length, "/* [Comment removed] */"));
        }

        // Single-line comments (avoid matching inside multi-line comments)
        foreach (Match match in SingleLineCommentPattern.Matches(content))
        {
            if (!IsInsideMultiLineComment(content, match.Index))
            {
                replacements.Add(new Replacement(match.Index, match.Length, "-- [Comment removed]"));
            }
        }

        // String literals
        foreach (Match match in StringLiteralPattern.Matches(content))
        {
            if (IsInsideComment(content, match.Index))
                continue;

            var inner = SqlLanguageProcessor.ExtractStringLiteralInner(match.Value);
            if (!string.IsNullOrEmpty(inner) && !SqlLanguageProcessor.IsNumericOrDateLiteral(inner))
            {
                var alias = context.GetOrCreateAlias(inner, SemanticCategory.StringLiteral, filePath);
                var prefix = match.Value.StartsWith("N'", StringComparison.OrdinalIgnoreCase) ? "N'" : "'";
                var newLiteral = $"{prefix}{alias}'";
                replacements.Add(new Replacement(match.Index, match.Length, newLiteral));
            }
        }
    }

    /// <summary>
    /// Simple check for whether a position falls inside a string literal or comment.
    /// </summary>
    private static bool IsInsideStringOrComment(string content, int position)
    {
        return IsInsideComment(content, position) || IsInsideStringLiteral(content, position);
    }

    private static bool IsInsideComment(string content, int position)
    {
        return IsInsideMultiLineComment(content, position) || IsInsideSingleLineComment(content, position);
    }

    private static bool IsInsideMultiLineComment(string content, int position)
    {
        var searchArea = content[..Math.Min(position, content.Length)];
        var lastOpen = searchArea.LastIndexOf("/*", StringComparison.Ordinal);
        if (lastOpen < 0) return false;

        var lastClose = searchArea.LastIndexOf("*/", StringComparison.Ordinal);
        return lastClose < lastOpen;
    }

    private static bool IsInsideSingleLineComment(string content, int position)
    {
        // Find the start of the current line
        var lineStart = content.LastIndexOf('\n', Math.Max(0, position - 1));
        if (lineStart < 0) lineStart = 0;
        else lineStart++; // move past the newline

        var lineContent = content[lineStart..position];
        var dashDash = lineContent.IndexOf("--", StringComparison.Ordinal);
        return dashDash >= 0;
    }

    private static bool IsInsideStringLiteral(string content, int position)
    {
        // Count unescaped single quotes before position
        var count = 0;
        for (var i = 0; i < position && i < content.Length; i++)
        {
            if (content[i] == '\'' && (i == 0 || content[i - 1] != '\''))
            {
                count++;
            }
        }
        // If odd number of quotes, we're inside a string
        return count % 2 == 1;
    }

    private static int GetLineNumber(string content, int position)
    {
        var line = 1;
        for (var i = 0; i < position && i < content.Length; i++)
        {
            if (content[i] == '\n')
                line++;
        }
        return line;
    }

    private static bool ContainsDb2Keywords(string content)
    {
        foreach (var keyword in Db2Keywords)
        {
            if (content.Contains(keyword, StringComparison.OrdinalIgnoreCase))
                return true;
        }
        return false;
    }

    /// <summary>
    /// Determines whether a parse error is likely caused by DB2-specific syntax
    /// rather than an actual structural issue.
    /// </summary>
    private static bool IsDb2SpecificParseError(ParseError error)
    {
        var msg = error.Message;
        // DB2-specific keywords that ScriptDom doesn't understand
        return msg.Contains("TABLESPACE", StringComparison.OrdinalIgnoreCase) ||
               msg.Contains("BUFFERPOOL", StringComparison.OrdinalIgnoreCase) ||
               msg.Contains("STOGROUP", StringComparison.OrdinalIgnoreCase) ||
               msg.Contains("COLLECTION", StringComparison.OrdinalIgnoreCase) ||
               msg.Contains("CCSID", StringComparison.OrdinalIgnoreCase) ||
               msg.Contains("LOCKSIZE", StringComparison.OrdinalIgnoreCase) ||
               msg.Contains("DSNDB", StringComparison.OrdinalIgnoreCase) ||
               msg.Contains("ACQUIRE", StringComparison.OrdinalIgnoreCase) ||
               msg.Contains("BIND", StringComparison.OrdinalIgnoreCase) ||
               msg.Contains("PACKAGE", StringComparison.OrdinalIgnoreCase);
    }
}

#endregion
