using System.Collections.Frozen;
using System.Text;
using System.Text.RegularExpressions;
using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;

namespace CodeBleach.Processors.Cobol;

// ─────────────────────────────────────────────────────────────────────────────
// Internal types: CobolLineType, CobolLine, CobolParser
// ─────────────────────────────────────────────────────────────────────────────

/// <summary>
/// Classification of a single COBOL source line based on column-aware parsing.
/// </summary>
internal enum CobolLineType
{
    Comment,
    Continuation,
    DivisionHeader,
    SectionHeader,
    ParagraphHeader,
    DataDefinition,
    ProcedureStatement,
    CopyStatement,
    ExecSqlBlock,
    ExecCicsBlock,
    Blank,
    Other
}

/// <summary>
/// Represents a single parsed COBOL source line with its column areas separated.
/// COBOL fixed-format layout:
///   Cols 1-6:   Sequence number area
///   Col 7:      Indicator area (* = comment, - = continuation, D = debug, / = page eject)
///   Cols 8-11:  Area A (division/section/paragraph headers, 01/77 level numbers)
///   Cols 12-72: Area B (code statements)
///   Cols 73-80: Identification area
/// </summary>
internal sealed class CobolLine
{
    public int LineNumber { get; init; }
    public string OriginalText { get; init; } = string.Empty;
    public string SequenceArea { get; init; } = string.Empty;   // cols 1-6
    public char Indicator { get; init; } = ' ';                  // col 7
    public string AreaA { get; init; } = string.Empty;           // cols 8-11
    public string AreaB { get; init; } = string.Empty;           // cols 12-72
    public string IdentArea { get; init; } = string.Empty;       // cols 73-80 (may be empty)
    public CobolLineType LineType { get; set; } = CobolLineType.Other;
}

/// <summary>
/// Column-aware COBOL line parser and classifier.
/// Parses each source line into its column areas and classifies the line type.
/// </summary>
internal static class CobolParser
{
    /// <summary>
    /// Parse a single source line into a <see cref="CobolLine"/> with column areas separated.
    /// Short lines are logically padded; long lines are split at column boundaries.
    /// </summary>
    public static CobolLine ParseLine(string text, int lineNumber)
    {
        // Handle lines shorter than the standard 80-column layout by logical padding
        var padded = text.Length < 80 ? text.PadRight(80) : text;

        var sequenceArea = padded[..6];
        var indicator = padded[6];
        var areaA = padded[7..11];
        var areaB = padded.Length >= 72 ? padded[11..72] : padded[11..];
        var identArea = padded.Length > 72 ? padded[72..Math.Min(padded.Length, 80)] : string.Empty;

        // Trim the identity area trailing padding only if the original line was short
        if (text.Length <= 72)
        {
            identArea = string.Empty;
        }

        var line = new CobolLine
        {
            LineNumber = lineNumber,
            OriginalText = text,
            SequenceArea = sequenceArea,
            Indicator = indicator,
            AreaA = areaA,
            AreaB = areaB,
            IdentArea = identArea
        };

        line.LineType = ClassifyLine(line);
        return line;
    }

    /// <summary>
    /// Parse all lines in a COBOL source file.
    /// </summary>
    public static List<CobolLine> ParseAllLines(string content)
    {
        var rawLines = content.Split('\n');
        var result = new List<CobolLine>(rawLines.Length);

        for (int i = 0; i < rawLines.Length; i++)
        {
            // Strip trailing \r for Windows line endings
            var text = rawLines[i].TrimEnd('\r');
            result.Add(ParseLine(text, i + 1));
        }

        return result;
    }

    /// <summary>
    /// Classify a parsed line into its <see cref="CobolLineType"/>.
    /// </summary>
    private static CobolLineType ClassifyLine(CobolLine line)
    {
        // Comment: col 7 is * or /
        if (line.Indicator == '*' || line.Indicator == '/')
            return CobolLineType.Comment;

        // Continuation: col 7 is -
        if (line.Indicator == '-')
            return CobolLineType.Continuation;

        var combined = (line.AreaA + line.AreaB).Trim();

        // Blank line
        if (string.IsNullOrWhiteSpace(combined))
            return CobolLineType.Blank;

        var combinedUpper = combined.ToUpperInvariant();

        // COPY statement
        if (IsCopyStatement(combinedUpper))
            return CobolLineType.CopyStatement;

        // EXEC SQL ... (may span multiple lines)
        if (IsExecSqlStart(combinedUpper))
            return CobolLineType.ExecSqlBlock;

        // EXEC CICS ...
        if (IsExecCicsStart(combinedUpper))
            return CobolLineType.ExecCicsBlock;

        // Division header: IDENTIFICATION DIVISION, DATA DIVISION, etc.
        if (IsDivisionHeader(combinedUpper))
            return CobolLineType.DivisionHeader;

        // Section header: WORKING-STORAGE SECTION, FILE SECTION, etc.
        if (IsSectionHeader(combinedUpper))
            return CobolLineType.SectionHeader;

        // Data definition: line starts with a level number in Area A or Area B
        if (IsDataDefinition(line))
            return CobolLineType.DataDefinition;

        // Paragraph header: a word starting in Area A (not a reserved statement keyword that starts a procedure)
        if (IsParagraphHeader(line))
            return CobolLineType.ParagraphHeader;

        return CobolLineType.ProcedureStatement;
    }

    private static bool IsCopyStatement(string combinedUpper)
    {
        return combinedUpper.StartsWith("COPY ", StringComparison.Ordinal)
            || combinedUpper == "COPY"
            || Regex.IsMatch(combinedUpper, @"^\s*COPY\s+");
    }

    private static bool IsExecSqlStart(string combinedUpper)
    {
        return Regex.IsMatch(combinedUpper, @"^\s*EXEC\s+SQL\b");
    }

    private static bool IsExecCicsStart(string combinedUpper)
    {
        return Regex.IsMatch(combinedUpper, @"^\s*EXEC\s+CICS\b");
    }

    private static bool IsDivisionHeader(string combinedUpper)
    {
        return Regex.IsMatch(combinedUpper, @"\b(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\b");
    }

    private static bool IsSectionHeader(string combinedUpper)
    {
        // Match both standard sections and user-defined sections in PROCEDURE DIVISION
        return Regex.IsMatch(combinedUpper, @"\bSECTION\s*\.\s*$")
            || Regex.IsMatch(combinedUpper, @"\b(WORKING-STORAGE|LINKAGE|FILE|LOCAL-STORAGE|COMMUNICATION|REPORT|SCREEN|INPUT-OUTPUT|CONFIGURATION)\s+SECTION\b");
    }

    /// <summary>
    /// Checks if the line is a data definition (starts with a level number).
    /// Level numbers: 01-49, 66, 77, 88.
    /// </summary>
    private static bool IsDataDefinition(CobolLine line)
    {
        var combined = (line.AreaA + line.AreaB).TrimStart();

        // Try to extract a leading number
        var match = Regex.Match(combined, @"^(\d{1,2})\b");
        if (!match.Success) return false;

        if (!int.TryParse(match.Groups[1].Value, out var level)) return false;

        return (level >= 1 && level <= 49) || level == 66 || level == 77 || level == 88;
    }

    /// <summary>
    /// A paragraph header is a user-defined name that starts in Area A (cols 8-11)
    /// and is NOT a level number, NOT a reserved division/section keyword.
    /// </summary>
    private static bool IsParagraphHeader(CobolLine line)
    {
        var areaATrimmed = line.AreaA.TrimStart();
        if (string.IsNullOrWhiteSpace(areaATrimmed))
            return false;

        // Must start with a letter or digit (paragraph names can start with digits in some dialects)
        if (!char.IsLetterOrDigit(areaATrimmed[0]))
            return false;

        // Extract the first word from Area A
        var firstWord = Regex.Match(areaATrimmed, @"^[\w-]+").Value.ToUpperInvariant();
        if (string.IsNullOrEmpty(firstWord))
            return false;

        // Not a level number
        if (int.TryParse(firstWord, out var num) && ((num >= 1 && num <= 49) || num == 66 || num == 77 || num == 88))
            return false;

        // Not a division/section keyword that could start in Area A
        var nonParagraphKeywords = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
        {
            "IDENTIFICATION", "ENVIRONMENT", "DATA", "PROCEDURE",
            "FD", "SD", "RD", "COPY", "REPLACE", "EXEC",
            "SELECT", "USE", "DECLARATIVES", "END"
        };

        if (nonParagraphKeywords.Contains(firstWord))
            return false;

        // Check that the line looks like "NAME." or "NAME SECTION." patterns
        // A paragraph header typically has just a name (possibly followed by a period)
        var combined = (line.AreaA + line.AreaB).Trim();
        var combinedUpper = combined.ToUpperInvariant();

        // If it contains DIVISION or SECTION, it's not a paragraph header (already classified above)
        if (combinedUpper.Contains("DIVISION") || combinedUpper.Contains("SECTION"))
            return false;

        return true;
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Discovery types: tracks user-defined names found during Pass 1
// ─────────────────────────────────────────────────────────────────────────────

/// <summary>
/// A user-defined identifier discovered during Pass 1 (discovery pass).
/// </summary>
internal sealed class DiscoveredIdentifier
{
    public required string OriginalName { get; init; }
    public required SemanticCategory Category { get; init; }
    public required int LineNumber { get; init; }
    public int ColumnStart { get; init; }
    public int ColumnEnd { get; init; }
    public string? QualifyingPrefix { get; init; }
}

// ─────────────────────────────────────────────────────────────────────────────
// CobolLanguageProcessor: ILanguageProcessor implementation
// ─────────────────────────────────────────────────────────────────────────────

/// <summary>
/// COBOL language processor for CodeBleach v2.0.
/// Performs column-aware, two-pass obfuscation of COBOL fixed-format source files.
///
/// Pass 1 (Discovery): Walks classified lines to collect all user-defined names
/// and register them in the shared ObfuscationContext with semantic categories.
///
/// Pass 2 (Replacement): Rebuilds each line with aliases while preserving
/// COBOL fixed-format column constraints (cols 1-6 sequence, col 7 indicator,
/// cols 8-72 code, cols 73-80 identification).
/// </summary>
public sealed class CobolLanguageProcessor : ILanguageProcessor
{
    // ── ILanguageProcessor metadata ─────────────────────────────────────

    public string ProcessorId => "cobol";
    public string DisplayName => "COBOL (Enterprise)";
    public IReadOnlySet<string> SupportedExtensions { get; } =
        new HashSet<string>(StringComparer.OrdinalIgnoreCase) { ".cbl", ".cob", ".cpy" };
    public int Priority => 10;

    // ── Reserved words (200+ entries) ───────────────────────────────────

    private static readonly FrozenSet<string> ReservedWords = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "ACCEPT", "ACCESS", "ADD", "ADVANCING", "AFTER", "ALL", "ALPHABETIC",
        "ALPHABETIC-LOWER", "ALPHABETIC-UPPER", "ALPHANUMERIC", "ALPHANUMERIC-EDITED",
        "ALSO", "ALTER", "AND", "ANY", "ARE", "AREA", "AREAS",
        "ASCENDING", "ASSIGN", "AT",
        "BEFORE", "BEGINNING", "BINARY", "BLANK", "BLOCK", "BOTTOM", "BY",
        "CALL", "CANCEL", "CBL", "CHARACTER", "CHARACTERS", "CLASS", "CLOSE",
        "COBOL", "CODE", "CODE-SET", "COLLATING", "COLUMN", "COMMA", "COMMON",
        "COMMUNICATION", "COMP", "COMP-1", "COMP-2", "COMP-3", "COMP-4", "COMP-5",
        "COMPUTATIONAL", "COMPUTATIONAL-1", "COMPUTATIONAL-2", "COMPUTATIONAL-3",
        "COMPUTATIONAL-4", "COMPUTATIONAL-5",
        "COMPUTE", "CONFIGURATION", "CONTAINS", "CONTENT", "CONTINUE",
        "CONTROL", "CONTROLS", "CONVERTING", "COPY", "CORR", "CORRESPONDING",
        "COUNT", "CURRENCY",
        "DATA", "DATE", "DATE-COMPILED", "DATE-WRITTEN", "DAY", "DAY-OF-WEEK",
        "DBCS", "DEBUGGING", "DECIMAL-POINT", "DECLARATIVES", "DELETE",
        "DELIMITED", "DELIMITER", "DEPENDING", "DESCENDING", "DESTINATION",
        "DETAIL", "DISPLAY", "DISPLAY-1", "DIVIDE", "DIVISION", "DOWN",
        "DUPLICATES", "DYNAMIC",
        "EGCS", "ELSE", "EMI", "ENABLE", "END", "END-ADD", "END-CALL",
        "END-COMPUTE", "END-DELETE", "END-DIVIDE", "END-EVALUATE", "END-EXEC",
        "END-IF", "END-MULTIPLY", "END-OF-PAGE", "END-PERFORM", "END-READ",
        "END-RECEIVE", "END-RETURN", "END-REWRITE", "END-SEARCH", "END-START",
        "END-STRING", "END-SUBTRACT", "END-UNSTRING", "END-WRITE",
        "ENDING", "ENTER", "ENTRY", "ENVIRONMENT", "EOP", "EQUAL", "ERROR",
        "ESI", "EVALUATE", "EVERY", "EXCEPTION", "EXEC", "EXECUTE", "EXIT",
        "EXTEND", "EXTERNAL",
        "FALSE", "FD", "FILE", "FILE-CONTROL", "FILLER", "FINAL", "FIRST",
        "FOOTING", "FOR", "FROM", "FUNCTION",
        "GENERATE", "GIVING", "GLOBAL", "GO", "GOBACK", "GREATER", "GROUP",
        "HEADING", "HIGH-VALUE", "HIGH-VALUES",
        "I-O", "I-O-CONTROL", "IDENTIFICATION", "IF", "IN", "INDEX", "INDEXED",
        "INDICATE", "INITIAL", "INITIALIZE", "INITIATE", "INPUT", "INPUT-OUTPUT",
        "INSPECT", "INSTALLATION", "INTO", "INVALID", "IS",
        "JUST", "JUSTIFIED", "KANJI", "KEY",
        "LABEL", "LAST", "LEADING", "LEFT", "LENGTH", "LESS", "LIMIT", "LIMITS",
        "LINAGE", "LINAGE-COUNTER", "LINE", "LINE-COUNTER", "LINES", "LINKAGE",
        "LOCK", "LOW-VALUE", "LOW-VALUES",
        "MEMORY", "MERGE", "MESSAGE", "MODE", "MODULES", "MORE-LABELS",
        "MOVE", "MULTIPLE", "MULTIPLY",
        "NATIVE", "NATIVE-2", "NEGATIVE", "NEXT", "NO", "NOT", "NULL", "NULLS",
        "NUMBER", "NUMERIC", "NUMERIC-EDITED",
        "OBJECT-COMPUTER", "OCCURS", "OF", "OFF", "OMITTED", "ON", "OPEN",
        "OPTIONAL", "OR", "ORDER", "ORGANIZATION", "OTHER", "OUTPUT", "OVERFLOW",
        "PACKED-DECIMAL", "PADDING", "PAGE", "PAGE-COUNTER", "PERFORM",
        "PIC", "PICTURE", "PLUS", "POINTER", "POSITION", "POSITIVE",
        "PROCEDURE", "PROCEDURES", "PROCEED", "PROCESSING", "PROGRAM",
        "PROGRAM-ID", "PURGE",
        "QUEUE", "QUOTE", "QUOTES",
        "RANDOM", "READ", "RECEIVE", "RECORD", "RECORDING", "RECORDS",
        "REDEFINES", "REEL", "REFERENCE", "REFERENCES", "RELATIVE", "RELEASE",
        "REMAINDER", "REMOVAL", "RENAMES", "REPLACE", "REPLACING", "REPORT",
        "REPORTING", "REPORTS", "REPOSITORY", "RERUN", "RESERVE", "RESET",
        "RETURN", "RETURN-CODE", "RETURNING", "REVERSED", "REWRITE",
        "RIGHT", "ROUNDED", "RUN",
        "SAME", "SD", "SEARCH", "SECTION", "SECURITY", "SEGMENT", "SEGMENT-LIMIT",
        "SELECT", "SEND", "SENTENCE", "SEPARATE", "SEQUENCE", "SEQUENTIAL",
        "SERVICE", "SET", "SHIFT-IN", "SHIFT-OUT", "SIGN", "SIZE",
        "SORT", "SORT-CONTROL", "SORT-CORE-SIZE", "SORT-FILE-SIZE", "SORT-MERGE",
        "SORT-MESSAGE", "SORT-MODE-SIZE", "SORT-RETURN",
        "SOURCE", "SOURCE-COMPUTER", "SPACE", "SPACES",
        "SPECIAL-NAMES", "STANDARD", "STANDARD-1", "STANDARD-2",
        "START", "STATUS", "STOP", "STRING", "SUB-QUEUE-1", "SUB-QUEUE-2",
        "SUB-QUEUE-3", "SUBTRACT", "SUM", "SUPPRESS", "SYMBOLIC", "SYNC",
        "SYNCHRONIZED",
        "TABLE", "TALLYING", "TAPE", "TERMINAL", "TERMINATE", "TEST",
        "TEXT", "THAN", "THEN", "THROUGH", "THRU", "TIME", "TIMES",
        "TO", "TOP", "TRAILING", "TRUE", "TYPE",
        "UNIT", "UNSTRING", "UNTIL", "UP", "UPON", "USAGE", "USE", "USING",
        "VALUE", "VALUES", "VARYING",
        "WHEN", "WHEN-COMPILED", "WITH", "WORDS", "WORKING-STORAGE", "WRITE",
        "ZERO", "ZEROES", "ZEROS",
        // Figurative constants
        "SPACE", "SPACES", "ZERO", "ZEROS", "ZEROES",
        "HIGH-VALUE", "HIGH-VALUES", "LOW-VALUE", "LOW-VALUES",
        "QUOTE", "QUOTES", "ALL",
        // Additional IBM extensions commonly encountered
        "EJECT", "SKIP1", "SKIP2", "SKIP3", "TITLE", "READY", "TRACE",
        "BASIS", "INSERT", "DELETE", "CBL", "PROCESS",
        "XML", "JSON", "END-XML", "END-JSON",
        "DISPLAY-OF", "NATIONAL-OF", "FUNCTION",
        "INVOKE", "OBJECT", "METHOD", "METHOD-ID", "END-METHOD",
        "CLASS-ID", "END-CLASS", "FACTORY", "END-FACTORY",
        "RAISE", "RESUME", "VALIDATE",
        "LOCAL-STORAGE", "SCREEN", "REPORT"
    }.ToFrozenSet(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// Regex pattern matching a valid COBOL identifier:
    /// starts with a letter, followed by letters/digits/hyphens, must not end with a hyphen.
    /// </summary>
    private static readonly Regex CobolIdentifierPattern =
        new(@"[A-Za-z][\w-]*[A-Za-z0-9]|[A-Za-z]", RegexOptions.Compiled);

    /// <summary>
    /// Pattern to match COBOL host variables in EXEC SQL blocks: :VARIABLE-NAME
    /// </summary>
    private static readonly Regex HostVariablePattern =
        new(@":([A-Za-z][\w-]*)", RegexOptions.Compiled);

    /// <summary>
    /// Pattern to match alias names (PREFIX_N format) for deobfuscation.
    /// </summary>
    private static readonly Regex AliasPattern =
        new(@"\b([A-Z]+_\d+)\b", RegexOptions.Compiled);

    /// <summary>
    /// PIC/PICTURE clause pattern - everything after PIC/PICTURE (including IS) until the next clause or period.
    /// Used to protect PIC clause content from being renamed.
    /// </summary>
    private static readonly Regex PicClausePattern =
        new(@"\bPIC(?:TURE)?\s+(?:IS\s+)?[\w\(\)\.\+\-\*\/,XABSV9Z0]+",
            RegexOptions.Compiled | RegexOptions.IgnoreCase);

    // ── ILanguageProcessor methods ──────────────────────────────────────

    public bool CanProcess(string filePath, string content)
    {
        if (string.IsNullOrWhiteSpace(filePath) && string.IsNullOrWhiteSpace(content))
            return false;

        // Check extension
        if (!string.IsNullOrWhiteSpace(filePath))
        {
            var ext = Path.GetExtension(filePath);
            if (SupportedExtensions.Contains(ext))
                return true;
        }

        // Content heuristic: look for COBOL markers
        if (!string.IsNullOrWhiteSpace(content))
        {
            var upper = content.ToUpperInvariant();
            return upper.Contains("IDENTIFICATION DIVISION")
                || upper.Contains("PROCEDURE DIVISION")
                || upper.Contains("DATA DIVISION")
                || upper.Contains("WORKING-STORAGE SECTION");
        }

        return false;
    }

    public LanguageProcessingResult Obfuscate(string content, ObfuscationContext context, string? filePath = null)
    {
        if (string.IsNullOrEmpty(content))
        {
            return new LanguageProcessingResult
            {
                Content = content ?? string.Empty,
                WasTransformed = false,
                ReplacementCount = 0,
                ProcessorId = ProcessorId
            };
        }

        var warnings = new List<string>();

        try
        {
            var lines = CobolParser.ParseAllLines(content);

            // Resolve multi-line EXEC blocks into single logical classifications
            ResolveExecBlocks(lines);

            // Pass 1: Discovery - collect all user-defined identifiers
            var identifiers = DiscoverIdentifiers(lines, filePath, warnings);

            // Register all discovered identifiers in the ObfuscationContext
            var aliasMap = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            foreach (var id in identifiers)
            {
                var normalizedName = id.OriginalName.ToUpperInvariant();
                if (!aliasMap.ContainsKey(normalizedName))
                {
                    var alias = context.GetOrCreateAlias(
                        normalizedName,
                        id.Category,
                        filePath,
                        id.LineNumber,
                        id.ColumnStart,
                        id.ColumnEnd);
                    aliasMap[normalizedName] = alias;
                }
            }

            // Pass 2: Replacement - rebuild each line with aliases
            int replacementCount = 0;
            var resultLines = new List<string>(lines.Count);

            for (int i = 0; i < lines.Count; i++)
            {
                var line = lines[i];

                // Collect contiguous EXEC SQL block lines and delegate SQL body processing
                if (line.LineType == CobolLineType.ExecSqlBlock)
                {
                    // Gather the full contiguous group of ExecSqlBlock lines
                    var execGroup = new List<(CobolLine Line, string CurrentText)>();
                    int groupStart = i;
                    while (i < lines.Count && lines[i].LineType == CobolLineType.ExecSqlBlock)
                    {
                        var execLine = lines[i];
                        // First apply host variable replacement (existing logic)
                        var (replacedText, hvCount) = ReplaceExecSqlBlock(execLine, aliasMap, context, filePath);
                        replacementCount += hvCount;
                        execGroup.Add((execLine, replacedText));
                        i++;
                    }
                    // Adjust i since the for-loop will increment it
                    i--;

                    // Now delegate the SQL body to the SQL processor for identifier obfuscation
                    var (updatedLines, sqlCount) = DelegateExecSqlToSqlProcessor(execGroup, context, filePath);
                    replacementCount += sqlCount;
                    resultLines.AddRange(updatedLines);
                    continue;
                }

                var (newLineText, count) = ReplaceLine(line, aliasMap, context, filePath, warnings);
                resultLines.Add(newLineText);
                replacementCount += count;
            }

            var result = string.Join("\n", resultLines);

            if (filePath != null)
            {
                context.RecordFileProcessing(filePath, ProcessorId, replacementCount);
            }

            return new LanguageProcessingResult
            {
                Content = result,
                WasTransformed = replacementCount > 0,
                ReplacementCount = replacementCount,
                ProcessorId = ProcessorId,
                Warnings = warnings
            };
        }
        catch (Exception ex)
        {
            warnings.Add($"COBOL processing error: {ex.Message}");
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

    public LanguageProcessingResult Deobfuscate(string content, ObfuscationContext context, string? filePath = null)
    {
        if (string.IsNullOrEmpty(content))
        {
            return new LanguageProcessingResult
            {
                Content = content ?? string.Empty,
                WasTransformed = false,
                ReplacementCount = 0,
                ProcessorId = ProcessorId
            };
        }

        var warnings = new List<string>();

        try
        {
            var lines = CobolParser.ParseAllLines(content);
            ResolveExecBlocks(lines);

            int replacementCount = 0;
            var resultLines = new List<string>(lines.Count);

            for (int i = 0; i < lines.Count; i++)
            {
                var line = lines[i];
                var (newLineText, count) = DeobfuscateLine(line, context, warnings);
                resultLines.Add(newLineText);
                replacementCount += count;
            }

            var result = string.Join("\n", resultLines);

            return new LanguageProcessingResult
            {
                Content = result,
                WasTransformed = replacementCount > 0,
                ReplacementCount = replacementCount,
                ProcessorId = ProcessorId,
                Warnings = warnings
            };
        }
        catch (Exception ex)
        {
            warnings.Add($"COBOL deobfuscation error: {ex.Message}");
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

    public ValidationResult Validate(string obfuscatedContent)
    {
        if (string.IsNullOrEmpty(obfuscatedContent))
            return ValidationResult.Valid();

        var errors = new List<string>();
        var warnings = new List<string>();

        try
        {
            var lines = CobolParser.ParseAllLines(obfuscatedContent);

            for (int i = 0; i < lines.Count; i++)
            {
                var line = lines[i];
                var originalLength = line.OriginalText.Length;

                // Check that lines are not excessively long (standard is 80 cols, but allow some flexibility)
                if (originalLength > 80)
                {
                    // Only warn if the original was within 80 and we expanded it
                    warnings.Add($"Line {line.LineNumber}: exceeds 80 columns ({originalLength} chars).");
                }

                // Check that code content is within cols 8-72 for non-comment lines
                // COBOL compilers typically ignore cols 73-80 (identification area)
                // so we only validate the overall structure here.

                // Check that no reserved word was accidentally used as an alias
                // (aliases use PREFIX_N format which should never collide)
                if (line.LineType == CobolLineType.ParagraphHeader ||
                    line.LineType == CobolLineType.SectionHeader)
                {
                    var combined = (line.AreaA + line.AreaB).Trim().TrimEnd('.');
                    var firstName = combined.Split(' ', StringSplitOptions.RemoveEmptyEntries).FirstOrDefault();
                    if (firstName != null && ReservedWords.Contains(firstName) && !firstName.Contains('_'))
                    {
                        // Reserved words in header position are fine (like SECTION keyword)
                        // Only flag if a user identifier somehow became a reserved word
                    }
                }
            }

            // Verify basic COBOL structure - should have at least one DIVISION
            var hasAnyDivision = lines.Any(l => l.LineType == CobolLineType.DivisionHeader);
            if (!hasAnyDivision && lines.Count > 5)
            {
                warnings.Add("No DIVISION header found - file may not be valid COBOL.");
            }
        }
        catch (Exception ex)
        {
            errors.Add($"Validation error: {ex.Message}");
        }

        if (errors.Count > 0)
            return ValidationResult.Invalid(errors);

        return warnings.Count > 0
            ? new ValidationResult { IsValid = true, Warnings = warnings }
            : ValidationResult.Valid();
    }

    // ── Pass 1: Discovery ───────────────────────────────────────────────

    /// <summary>
    /// Walk classified lines and discover all user-defined identifiers.
    /// </summary>
    private List<DiscoveredIdentifier> DiscoverIdentifiers(
        List<CobolLine> lines,
        string? filePath,
        List<string> warnings)
    {
        // filePath is available for future per-file discovery tracking
        _ = filePath;

        var identifiers = new List<DiscoveredIdentifier>();
        var currentDivision = "";
        var currentSection = "";
        bool inProcedureDivision = false;
        bool inFileSection = false;
        string? currentFdFile = null;

        for (int i = 0; i < lines.Count; i++)
        {
            var line = lines[i];

            try
            {
                switch (line.LineType)
                {
                    case CobolLineType.DivisionHeader:
                        currentDivision = ExtractDivisionName(line);
                        inProcedureDivision = currentDivision.Equals("PROCEDURE", StringComparison.OrdinalIgnoreCase);
                        inFileSection = false;
                        currentFdFile = null;
                        break;

                    case CobolLineType.SectionHeader:
                        currentSection = ExtractSectionName(line);
                        inFileSection = currentSection.Equals("FILE", StringComparison.OrdinalIgnoreCase);

                        // User-defined sections in PROCEDURE DIVISION get registered
                        if (inProcedureDivision)
                        {
                            var sectionName = ExtractUserSectionName(line);
                            if (sectionName != null && !ReservedWords.Contains(sectionName))
                            {
                                identifiers.Add(new DiscoveredIdentifier
                                {
                                    OriginalName = sectionName.ToUpperInvariant(),
                                    Category = SemanticCategory.Section,
                                    LineNumber = line.LineNumber,
                                    ColumnStart = 8,
                                    ColumnEnd = 8 + sectionName.Length
                                });
                            }
                        }
                        break;

                    case CobolLineType.ParagraphHeader:
                        if (inProcedureDivision)
                        {
                            var paraName = ExtractParagraphName(line);
                            if (paraName != null && !ReservedWords.Contains(paraName))
                            {
                                identifiers.Add(new DiscoveredIdentifier
                                {
                                    OriginalName = paraName.ToUpperInvariant(),
                                    Category = SemanticCategory.Paragraph,
                                    LineNumber = line.LineNumber,
                                    ColumnStart = 8,
                                    ColumnEnd = 8 + paraName.Length
                                });
                            }
                        }
                        else
                        {
                            // Check for PROGRAM-ID
                            DiscoverProgramId(line, identifiers);
                        }
                        break;

                    case CobolLineType.DataDefinition:
                        DiscoverDataDefinition(line, identifiers, currentSection,
                            inFileSection, currentFdFile);
                        break;

                    case CobolLineType.CopyStatement:
                        DiscoverCopybook(line, identifiers);
                        break;

                    case CobolLineType.ProcedureStatement:
                        // Check for PROGRAM-ID in IDENTIFICATION DIVISION (may be classified as ProcedureStatement)
                        DiscoverProgramId(line, identifiers);

                        // Check for FD/SD statements
                        var fdFile = DiscoverFileDescription(line, identifiers);
                        if (fdFile != null)
                        {
                            currentFdFile = fdFile;
                        }
                        break;

                    case CobolLineType.Other:
                        // Also check for PROGRAM-ID and FD here
                        DiscoverProgramId(line, identifiers);
                        var fdFile2 = DiscoverFileDescription(line, identifiers);
                        if (fdFile2 != null)
                        {
                            currentFdFile = fdFile2;
                        }
                        break;
                }
            }
            catch (Exception ex)
            {
                warnings.Add($"Discovery error on line {line.LineNumber}: {ex.Message}");
            }
        }

        return identifiers;
    }

    /// <summary>
    /// Extract the division name (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE).
    /// </summary>
    private static string ExtractDivisionName(CobolLine line)
    {
        var combined = (line.AreaA + line.AreaB).Trim().ToUpperInvariant();
        var match = Regex.Match(combined, @"(\w[\w-]*)\s+DIVISION");
        return match.Success ? match.Groups[1].Value : string.Empty;
    }

    /// <summary>
    /// Extract the section name from a section header line.
    /// </summary>
    private static string ExtractSectionName(CobolLine line)
    {
        var combined = (line.AreaA + line.AreaB).Trim().ToUpperInvariant();

        // Standard sections: WORKING-STORAGE SECTION, FILE SECTION, etc.
        var match = Regex.Match(combined, @"([\w-]+)\s+SECTION");
        return match.Success ? match.Groups[1].Value : string.Empty;
    }

    /// <summary>
    /// Extract a user-defined section name in PROCEDURE DIVISION.
    /// E.g., "MAIN-LOGIC SECTION." -> "MAIN-LOGIC"
    /// </summary>
    private static string? ExtractUserSectionName(CobolLine line)
    {
        var combined = (line.AreaA + line.AreaB).Trim().ToUpperInvariant();
        var match = Regex.Match(combined, @"^([\w-]+)\s+SECTION\s*\.");
        if (match.Success)
        {
            var name = match.Groups[1].Value;
            // Don't return standard section names
            var standardSections = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
            {
                "WORKING-STORAGE", "LINKAGE", "FILE", "LOCAL-STORAGE",
                "COMMUNICATION", "REPORT", "SCREEN", "INPUT-OUTPUT", "CONFIGURATION"
            };
            if (!standardSections.Contains(name))
                return name;
        }
        return null;
    }

    /// <summary>
    /// Extract a paragraph name from a paragraph header line.
    /// </summary>
    private static string? ExtractParagraphName(CobolLine line)
    {
        var combined = (line.AreaA + line.AreaB).Trim();
        var match = Regex.Match(combined, @"^([\w-]+)");
        if (match.Success)
        {
            var name = match.Groups[1].Value;
            if (!ReservedWords.Contains(name))
                return name;
        }
        return null;
    }

    /// <summary>
    /// Check for PROGRAM-ID declaration and discover the program name.
    /// </summary>
    private static void DiscoverProgramId(CobolLine line, List<DiscoveredIdentifier> identifiers)
    {
        var combined = (line.AreaA + line.AreaB).Trim();
        var match = Regex.Match(combined, @"PROGRAM-ID\s*\.\s*([\w-]+)",
            RegexOptions.IgnoreCase);
        if (match.Success)
        {
            var programName = match.Groups[1].Value;
            if (!ReservedWords.Contains(programName))
            {
                identifiers.Add(new DiscoveredIdentifier
                {
                    OriginalName = programName.ToUpperInvariant(),
                    Category = SemanticCategory.Program,
                    LineNumber = line.LineNumber,
                    ColumnStart = match.Groups[1].Index + 8, // approximate column
                    ColumnEnd = match.Groups[1].Index + 8 + programName.Length
                });
            }
        }
    }

    /// <summary>
    /// Discover data definitions (level numbers with data names).
    /// </summary>
    private void DiscoverDataDefinition(
        CobolLine line,
        List<DiscoveredIdentifier> identifiers,
        string currentSection,
        bool inFileSection,
        string? currentFdFile)
    {
        var combined = (line.AreaA + line.AreaB).Trim().ToUpperInvariant();

        // Parse: LEVEL-NUM DATA-NAME ...
        var match = Regex.Match(combined, @"^(\d{1,2})\s+([\w-]+)");
        if (!match.Success) return;

        var levelStr = match.Groups[1].Value;
        var dataName = match.Groups[2].Value;

        if (!int.TryParse(levelStr, out var level)) return;

        // FILLER must never be renamed
        if (dataName.Equals("FILLER", StringComparison.OrdinalIgnoreCase)) return;

        // Reserved words must not be renamed
        if (ReservedWords.Contains(dataName)) return;

        // Determine semantic category based on context
        SemanticCategory category;
        if (level == 88)
        {
            category = SemanticCategory.Condition;
        }
        else if (inFileSection && currentFdFile != null && level == 1)
        {
            category = SemanticCategory.CobolRecord;
        }
        else
        {
            category = SemanticCategory.Variable;
        }

        identifiers.Add(new DiscoveredIdentifier
        {
            OriginalName = dataName,
            Category = category,
            LineNumber = line.LineNumber,
            ColumnStart = match.Groups[2].Index + 8,
            ColumnEnd = match.Groups[2].Index + 8 + dataName.Length,
            QualifyingPrefix = GetVariablePrefix(currentSection)
        });
    }

    /// <summary>
    /// Discover COPY copybook-name.
    /// </summary>
    private static void DiscoverCopybook(CobolLine line, List<DiscoveredIdentifier> identifiers)
    {
        var combined = (line.AreaA + line.AreaB).Trim();
        var match = Regex.Match(combined, @"COPY\s+([\w-]+)", RegexOptions.IgnoreCase);
        if (match.Success)
        {
            var copybookName = match.Groups[1].Value;
            if (!ReservedWords.Contains(copybookName))
            {
                identifiers.Add(new DiscoveredIdentifier
                {
                    OriginalName = copybookName.ToUpperInvariant(),
                    Category = SemanticCategory.Copybook,
                    LineNumber = line.LineNumber,
                    ColumnStart = match.Groups[1].Index + 8,
                    ColumnEnd = match.Groups[1].Index + 8 + copybookName.Length
                });
            }
        }
    }

    /// <summary>
    /// Discover FD/SD file descriptions.
    /// Returns the file name if found, null otherwise.
    /// </summary>
    private static string? DiscoverFileDescription(CobolLine line, List<DiscoveredIdentifier> identifiers)
    {
        var combined = (line.AreaA + line.AreaB).Trim();
        var match = Regex.Match(combined, @"^(FD|SD)\s+([\w-]+)", RegexOptions.IgnoreCase);
        if (match.Success)
        {
            var fileName = match.Groups[2].Value;
            if (!ReservedWords.Contains(fileName))
            {
                identifiers.Add(new DiscoveredIdentifier
                {
                    OriginalName = fileName.ToUpperInvariant(),
                    Category = SemanticCategory.CobolFile,
                    LineNumber = line.LineNumber,
                    ColumnStart = match.Groups[2].Index + 8,
                    ColumnEnd = match.Groups[2].Index + 8 + fileName.Length
                });
                return fileName.ToUpperInvariant();
            }
        }
        return null;
    }

    /// <summary>
    /// Get a prefix hint for variable naming based on the current COBOL section.
    /// </summary>
    private static string GetVariablePrefix(string currentSection)
    {
        return currentSection.ToUpperInvariant() switch
        {
            "WORKING-STORAGE" => "WS",
            "LINKAGE" => "LS",
            "LOCAL-STORAGE" => "LC",
            "FILE" => "FL",
            "COMMUNICATION" => "CM",
            _ => "WS"
        };
    }

    // ── Pass 2: Replacement ─────────────────────────────────────────────

    /// <summary>
    /// Replace identifiers in a single COBOL line with their aliases.
    /// Returns the new line text and the count of replacements made.
    /// </summary>
    private (string NewLine, int Count) ReplaceLine(
        CobolLine line,
        Dictionary<string, string> aliasMap,
        ObfuscationContext context,
        string? filePath,
        List<string> warnings)
    {
        // warnings is available for future use by individual replacement methods
        _ = warnings;

        switch (line.LineType)
        {
            case CobolLineType.Comment:
                return ReplaceCommentLine(line);

            case CobolLineType.Blank:
                return (line.OriginalText, 0);

            case CobolLineType.Continuation:
                return ReplaceContinuationLine(line, aliasMap);

            case CobolLineType.DivisionHeader:
                // Division headers contain only reserved words - pass through
                // But check for PROGRAM-ID on the same line (rare)
                return ReplaceProgramIdLine(line, aliasMap);

            case CobolLineType.SectionHeader:
                return ReplaceSectionHeader(line, aliasMap);

            case CobolLineType.ParagraphHeader:
                return ReplaceParagraphHeader(line, aliasMap);

            case CobolLineType.DataDefinition:
                return ReplaceDataDefinition(line, aliasMap);

            case CobolLineType.CopyStatement:
                return ReplaceCopyStatement(line, aliasMap);

            case CobolLineType.ExecSqlBlock:
                return ReplaceExecSqlBlock(line, aliasMap, context, filePath);

            case CobolLineType.ExecCicsBlock:
                return ReplaceExecCicsBlock(line, aliasMap);

            case CobolLineType.ProcedureStatement:
            case CobolLineType.Other:
            default:
                return ReplaceProcedureStatement(line, aliasMap);
        }
    }

    /// <summary>
    /// Replace comment content with "[Comment removed]" while preserving column structure.
    /// </summary>
    private static (string NewLine, int Count) ReplaceCommentLine(CobolLine line)
    {
        // Preserve cols 1-6 (sequence) and col 7 (indicator), replace content
        var commentReplacement = "[Comment removed]";

        // Build the new line preserving column structure
        var newLine = RebuildLine(
            line.SequenceArea,
            line.Indicator,
            "    ",  // Area A: 4 spaces
            " " + commentReplacement.PadRight(60)[..60], // Area B: pad/truncate to fit
            line.IdentArea);

        return (TrimToOriginalLength(newLine, line.OriginalText), 1);
    }

    /// <summary>
    /// Handle continuation lines - apply identifier replacement to the content.
    /// </summary>
    private static (string NewLine, int Count) ReplaceContinuationLine(
        CobolLine line,
        Dictionary<string, string> aliasMap)
    {
        // Continuation lines may contain identifiers - apply simple replacement
        var codeContent = line.AreaA + line.AreaB;
        var (newContent, count) = ReplaceIdentifiersInText(codeContent, aliasMap);
        var newLine = RebuildLine(line.SequenceArea, line.Indicator,
            newContent.Length >= 4 ? newContent[..4] : newContent.PadRight(4),
            newContent.Length > 4 ? newContent[4..] : string.Empty,
            line.IdentArea);
        return (TrimToOriginalLength(newLine, line.OriginalText), count);
    }

    /// <summary>
    /// Replace PROGRAM-ID if present in a division header or similar line.
    /// </summary>
    private static (string NewLine, int Count) ReplaceProgramIdLine(
        CobolLine line,
        Dictionary<string, string> aliasMap)
    {
        var combined = line.AreaA + line.AreaB;
        var combinedUpper = combined.ToUpperInvariant();
        int count = 0;

        if (combinedUpper.Contains("PROGRAM-ID"))
        {
            var match = Regex.Match(combined, @"(PROGRAM-ID\s*\.\s*)([\w-]+)",
                RegexOptions.IgnoreCase);
            if (match.Success)
            {
                var progName = match.Groups[2].Value.ToUpperInvariant();
                if (aliasMap.TryGetValue(progName, out var alias))
                {
                    combined = combined[..match.Groups[2].Index]
                        + alias
                        + combined[(match.Groups[2].Index + match.Groups[2].Length)..];
                    count++;
                }
            }
        }

        return (RebuildFromCombined(line, combined), count);
    }

    /// <summary>
    /// Replace user-defined section names in PROCEDURE DIVISION section headers.
    /// </summary>
    private static (string NewLine, int Count) ReplaceSectionHeader(
        CobolLine line,
        Dictionary<string, string> aliasMap)
    {
        var combined = line.AreaA + line.AreaB;
        int count = 0;

        // Match: SECTION-NAME SECTION.
        var match = Regex.Match(combined, @"^(\s*)([\w-]+)(\s+SECTION\s*\.)",
            RegexOptions.IgnoreCase);
        if (match.Success)
        {
            var sectionName = match.Groups[2].Value.ToUpperInvariant();
            if (aliasMap.TryGetValue(sectionName, out var alias))
            {
                combined = match.Groups[1].Value + alias + match.Groups[3].Value;
                count++;
            }
        }

        return (RebuildFromCombined(line, combined), count);
    }

    /// <summary>
    /// Replace paragraph name in a paragraph header.
    /// </summary>
    private static (string NewLine, int Count) ReplaceParagraphHeader(
        CobolLine line,
        Dictionary<string, string> aliasMap)
    {
        var combined = line.AreaA + line.AreaB;
        int count = 0;

        // Paragraph header: NAME. or NAME (at start)
        var match = Regex.Match(combined, @"^(\s*)([\w-]+)(.*)$");
        if (match.Success)
        {
            var paraName = match.Groups[2].Value.ToUpperInvariant();
            if (aliasMap.TryGetValue(paraName, out var alias))
            {
                combined = match.Groups[1].Value + alias + match.Groups[3].Value;
                count++;
            }
        }

        return (RebuildFromCombined(line, combined), count);
    }

    /// <summary>
    /// Replace data names in data definition lines.
    /// Preserves level number, FILLER, PIC clauses, and VALUE clauses.
    /// </summary>
    private (string NewLine, int Count) ReplaceDataDefinition(
        CobolLine line,
        Dictionary<string, string> aliasMap)
    {
        var combined = line.AreaA + line.AreaB;
        int count = 0;

        // Match: LEVEL-NUM DATA-NAME rest-of-line
        var match = Regex.Match(combined, @"^(\s*\d{1,2}\s+)([\w-]+)(.*)$");
        if (match.Success)
        {
            var dataName = match.Groups[2].Value.ToUpperInvariant();

            // Don't rename FILLER or reserved words
            if (!dataName.Equals("FILLER", StringComparison.OrdinalIgnoreCase)
                && !ReservedWords.Contains(dataName)
                && aliasMap.TryGetValue(dataName, out var alias))
            {
                var restOfLine = match.Groups[3].Value;

                // Also replace any referenced data names in REDEFINES, RENAMES, etc.
                restOfLine = ReplaceReferencesInDataClause(restOfLine, aliasMap, ref count);

                combined = match.Groups[1].Value + alias + restOfLine;
                count++;
            }
            else
            {
                // Even if the data name itself wasn't replaced, check references in the rest
                var restOfLine = match.Groups[3].Value;
                var originalRest = restOfLine;
                restOfLine = ReplaceReferencesInDataClause(restOfLine, aliasMap, ref count);
                if (restOfLine != originalRest)
                {
                    combined = match.Groups[1].Value + match.Groups[2].Value + restOfLine;
                }
            }
        }

        return (RebuildFromCombined(line, combined), count);
    }

    /// <summary>
    /// Replace data item references in data definition clauses (REDEFINES, RENAMES, VALUE, etc.)
    /// while preserving PIC/PICTURE clauses.
    /// </summary>
    private string ReplaceReferencesInDataClause(
        string clause,
        Dictionary<string, string> aliasMap,
        ref int count)
    {
        // Replace REDEFINES target
        var redefinesMatch = Regex.Match(clause, @"(\bREDEFINES\s+)([\w-]+)",
            RegexOptions.IgnoreCase);
        if (redefinesMatch.Success)
        {
            var targetName = redefinesMatch.Groups[2].Value.ToUpperInvariant();
            if (aliasMap.TryGetValue(targetName, out var alias))
            {
                clause = clause[..redefinesMatch.Groups[2].Index]
                    + alias
                    + clause[(redefinesMatch.Groups[2].Index + redefinesMatch.Groups[2].Length)..];
                count++;
            }
        }

        // Replace RENAMES targets: RENAMES data-name-1 (THROUGH data-name-2)?
        var renamesMatch = Regex.Match(clause, @"(\bRENAMES\s+)([\w-]+)(\s+(?:THROUGH|THRU)\s+)([\w-]+)",
            RegexOptions.IgnoreCase);
        if (renamesMatch.Success)
        {
            var name1 = renamesMatch.Groups[2].Value.ToUpperInvariant();
            var name2 = renamesMatch.Groups[4].Value.ToUpperInvariant();
            var sb = new StringBuilder(clause);
            bool changed = false;

            // Replace in reverse order to maintain indices
            if (aliasMap.TryGetValue(name2, out var alias2))
            {
                sb.Remove(renamesMatch.Groups[4].Index, renamesMatch.Groups[4].Length);
                sb.Insert(renamesMatch.Groups[4].Index, alias2);
                count++;
                changed = true;
            }
            if (aliasMap.TryGetValue(name1, out var alias1))
            {
                sb.Remove(renamesMatch.Groups[2].Index, renamesMatch.Groups[2].Length);
                sb.Insert(renamesMatch.Groups[2].Index, alias1);
                count++;
                changed = true;
            }
            if (changed)
            {
                clause = sb.ToString();
            }
        }

        return clause;
    }

    /// <summary>
    /// Replace copybook name in a COPY statement.
    /// </summary>
    private static (string NewLine, int Count) ReplaceCopyStatement(
        CobolLine line,
        Dictionary<string, string> aliasMap)
    {
        var combined = line.AreaA + line.AreaB;
        int count = 0;

        var match = Regex.Match(combined, @"(\bCOPY\s+)([\w-]+)",
            RegexOptions.IgnoreCase);
        if (match.Success)
        {
            var copyName = match.Groups[2].Value.ToUpperInvariant();
            if (aliasMap.TryGetValue(copyName, out var alias))
            {
                combined = combined[..match.Groups[2].Index]
                    + alias
                    + combined[(match.Groups[2].Index + match.Groups[2].Length)..];
                count++;
            }
        }

        return (RebuildFromCombined(line, combined), count);
    }

    /// <summary>
    /// Replace host variables in EXEC SQL blocks.
    /// Host variables are prefixed with ':' (e.g., :WS-EMP-ID).
    /// </summary>
    private static (string NewLine, int Count) ReplaceExecSqlBlock(
        CobolLine line,
        Dictionary<string, string> aliasMap,
        ObfuscationContext context,
        string? filePath)
    {
        var combined = line.AreaA + line.AreaB;
        int count = 0;

        // Replace host variables: :VARIABLE-NAME
        combined = HostVariablePattern.Replace(combined, match =>
        {
            var varName = match.Groups[1].Value.ToUpperInvariant();
            if (aliasMap.TryGetValue(varName, out var alias))
            {
                count++;

                // Record cross-language reference
                context.AddCrossReference(new CrossLanguageReference
                {
                    Alias = alias,
                    SourceLanguage = "COBOL",
                    TargetLanguage = "SQL",
                    SourceFile = filePath,
                    Description = $"Host variable :{varName} used in EXEC SQL block"
                });

                return ":" + alias;
            }
            return match.Value;
        });

        return (RebuildFromCombined(line, combined), count);
    }

    /// <summary>
    /// Delegates the SQL body of a contiguous group of EXEC SQL block lines to the
    /// SQL language processor (looked up via the registry). Extracts the pure SQL text
    /// by stripping the EXEC SQL prefix and END-EXEC suffix, runs it through the SQL
    /// processor's Obfuscate method, and patches the obfuscated SQL back into the
    /// COBOL line structure. Host variable replacement must already have been applied
    /// to the lines before calling this method.
    /// </summary>
    /// <param name="execGroup">
    /// Contiguous lines (already host-variable-replaced) classified as ExecSqlBlock.
    /// </param>
    /// <param name="context">Shared obfuscation context (provides ProcessorRegistry).</param>
    /// <param name="filePath">Current file path for cross-language tracking.</param>
    /// <returns>
    /// Updated line texts for the group and the count of additional SQL replacements.
    /// </returns>
    private static (List<string> UpdatedLines, int Count) DelegateExecSqlToSqlProcessor(
        List<(CobolLine Line, string CurrentText)> execGroup,
        ObfuscationContext context,
        string? filePath)
    {
        // If no registry or no SQL processor available, return lines unchanged
        var sqlProcessor = context.ProcessorRegistry?.GetProcessor("temp.sql", "");
        if (sqlProcessor == null)
        {
            return (execGroup.Select(g => g.CurrentText).ToList(), 0);
        }

        // Extract the SQL body from the EXEC SQL block lines.
        // Each line's code area (AreaA + AreaB) may contain EXEC SQL, END-EXEC, or pure SQL.
        // We need to strip those COBOL-specific wrappers and concatenate the pure SQL parts.
        var sqlFragments = new List<(int GroupIndex, string SqlPart, int CodeStart, int CodeEnd)>();
        var fullSqlBuilder = new StringBuilder();

        for (int g = 0; g < execGroup.Count; g++)
        {
            var line = execGroup[g].Line;
            var codeArea = line.AreaA + line.AreaB;
            var codeAreaTrimmed = codeArea.TrimStart();
            var codeAreaUpper = codeAreaTrimmed.ToUpperInvariant();

            string sqlPart;

            if (Regex.IsMatch(codeAreaUpper, @"^\s*EXEC\s+SQL\b"))
            {
                // This line starts the EXEC SQL block - extract everything after "EXEC SQL"
                var execMatch = Regex.Match(codeArea, @"EXEC\s+SQL\s*", RegexOptions.IgnoreCase);
                sqlPart = codeArea[(execMatch.Index + execMatch.Length)..];
            }
            else if (codeAreaUpper.Contains("END-EXEC"))
            {
                // This line ends the block - extract everything before "END-EXEC"
                var endIdx = codeArea.IndexOf("END-EXEC", StringComparison.OrdinalIgnoreCase);
                sqlPart = codeArea[..endIdx];
            }
            else
            {
                // Middle line - entire code area is SQL
                sqlPart = codeArea;
            }

            // Also handle case where EXEC SQL and END-EXEC are on the same line
            if (Regex.IsMatch(codeAreaUpper, @"^\s*EXEC\s+SQL\b") && codeAreaUpper.Contains("END-EXEC"))
            {
                var execMatch = Regex.Match(codeArea, @"EXEC\s+SQL\s*", RegexOptions.IgnoreCase);
                var endIdx = codeArea.IndexOf("END-EXEC", StringComparison.OrdinalIgnoreCase);
                var afterExec = execMatch.Index + execMatch.Length;
                sqlPart = afterExec < endIdx ? codeArea[afterExec..endIdx] : string.Empty;
            }

            sqlFragments.Add((g, sqlPart, fullSqlBuilder.Length, fullSqlBuilder.Length + sqlPart.Length));
            if (fullSqlBuilder.Length > 0)
                fullSqlBuilder.Append(' ');
            fullSqlBuilder.Append(sqlPart.Trim());
        }

        var fullSql = fullSqlBuilder.ToString().Trim();
        if (string.IsNullOrWhiteSpace(fullSql))
        {
            return (execGroup.Select(g => g.CurrentText).ToList(), 0);
        }

        // Delegate to the SQL processor
        LanguageProcessingResult sqlResult;
        try
        {
            sqlResult = sqlProcessor.Obfuscate(fullSql, context, filePath);
        }
        catch
        {
            // If SQL processing fails, return lines unchanged
            return (execGroup.Select(g => g.CurrentText).ToList(), 0);
        }

        if (!sqlResult.WasTransformed || sqlResult.ReplacementCount == 0)
        {
            return (execGroup.Select(g => g.CurrentText).ToList(), 0);
        }

        // Now we need to patch the obfuscated SQL back into the COBOL lines.
        // The SQL processor may have changed identifier lengths, so we need to
        // redistribute the obfuscated SQL across the original line structure.
        //
        // Strategy: split the obfuscated SQL back into tokens/fragments that correspond
        // to each original line's SQL portion. We use a word-level mapping approach:
        // tokenize the original combined SQL and the obfuscated SQL, then walk through
        // and replace each original fragment line-by-line.
        var obfuscatedSql = sqlResult.Content.Trim();

        // For single-line EXEC SQL blocks (most common case), direct replacement is simple
        if (execGroup.Count == 1)
        {
            var line = execGroup[0].Line;
            var codeArea = line.AreaA + line.AreaB;
            var codeAreaUpper = codeArea.TrimStart().ToUpperInvariant();

            string newCodeArea;
            if (Regex.IsMatch(codeAreaUpper, @"^\s*EXEC\s+SQL\b") && codeAreaUpper.Contains("END-EXEC"))
            {
                // Single line: EXEC SQL <sql> END-EXEC
                var execMatch = Regex.Match(codeArea, @"(.*?EXEC\s+SQL\s*)", RegexOptions.IgnoreCase);
                var endMatch = Regex.Match(codeArea, @"(\s*END-EXEC.*)", RegexOptions.IgnoreCase);
                newCodeArea = execMatch.Groups[1].Value + obfuscatedSql + endMatch.Groups[1].Value;
            }
            else
            {
                // Should not happen for a single-line block, but handle gracefully
                newCodeArea = codeArea;
            }

            var newLine = RebuildFromCombined(line, newCodeArea);
            return (new List<string> { newLine }, sqlResult.ReplacementCount);
        }

        // Multi-line: distribute the obfuscated SQL back across lines.
        // We split the obfuscated SQL by whitespace into tokens, then fill each line's
        // SQL area with as many tokens as fit (respecting the original line's available space).
        var obfuscatedTokens = new Queue<string>(
            obfuscatedSql.Split((char[]?)null, StringSplitOptions.RemoveEmptyEntries));
        var updatedLines = new List<string>(execGroup.Count);

        for (int g = 0; g < execGroup.Count; g++)
        {
            var line = execGroup[g].Line;
            var codeArea = line.AreaA + line.AreaB;
            var codeAreaUpper = codeArea.TrimStart().ToUpperInvariant();

            string newCodeArea;

            if (Regex.IsMatch(codeAreaUpper, @"^\s*EXEC\s+SQL\b"))
            {
                // First line: preserve "EXEC SQL" prefix, fill remaining with tokens
                var execMatch = Regex.Match(codeArea, @"(.*?EXEC\s+SQL\s*)", RegexOptions.IgnoreCase);
                var prefix = execMatch.Groups[1].Value;
                var availableLen = 65 - prefix.Length; // cols 8-72 = 65 chars total
                var sqlPart = FillWithTokens(obfuscatedTokens, availableLen > 0 ? availableLen : 20);
                newCodeArea = prefix + sqlPart;
            }
            else if (codeAreaUpper.Contains("END-EXEC"))
            {
                // Last line: put remaining tokens before END-EXEC
                var endMatch = Regex.Match(codeArea, @"(\s*END-EXEC.*)", RegexOptions.IgnoreCase);
                var leadingSpaces = Regex.Match(codeArea, @"^(\s*)").Groups[1].Value;
                // Drain any remaining tokens
                var remaining = string.Join(" ", obfuscatedTokens);
                obfuscatedTokens.Clear();
                newCodeArea = leadingSpaces + (remaining.Length > 0 ? remaining + " " : "") + endMatch.Groups[1].Value.TrimStart();
            }
            else
            {
                // Middle line: fill with tokens preserving leading whitespace
                var leadingSpaces = Regex.Match(codeArea, @"^(\s*)").Groups[1].Value;
                var availableLen = 65 - leadingSpaces.Length;
                var sqlPart = FillWithTokens(obfuscatedTokens, availableLen > 0 ? availableLen : 20);
                newCodeArea = leadingSpaces + sqlPart;
            }

            var newLine = RebuildFromCombined(line, newCodeArea);
            updatedLines.Add(newLine);
        }

        return (updatedLines, sqlResult.ReplacementCount);
    }

    /// <summary>
    /// Dequeue tokens from the queue and concatenate them with spaces until
    /// the result would exceed <paramref name="maxLen"/> characters.
    /// </summary>
    private static string FillWithTokens(Queue<string> tokens, int maxLen)
    {
        if (tokens.Count == 0)
            return string.Empty;

        var sb = new StringBuilder();
        while (tokens.Count > 0)
        {
            var next = tokens.Peek();
            var needed = sb.Length == 0 ? next.Length : sb.Length + 1 + next.Length;
            if (needed > maxLen && sb.Length > 0)
                break;
            if (sb.Length > 0)
                sb.Append(' ');
            sb.Append(tokens.Dequeue());
        }
        return sb.ToString();
    }

    /// <summary>
    /// Replace identifiers in EXEC CICS blocks.
    /// </summary>
    private static (string NewLine, int Count) ReplaceExecCicsBlock(
        CobolLine line,
        Dictionary<string, string> aliasMap)
    {
        var combined = line.AreaA + line.AreaB;
        var (newContent, count) = ReplaceIdentifiersInText(combined, aliasMap);
        return (RebuildFromCombined(line, newContent), count);
    }

    /// <summary>
    /// Replace data item and paragraph/section references in procedure statements.
    /// Handles PERFORM, GO TO, MOVE, COMPUTE, IF, EVALUATE, CALL, etc.
    /// </summary>
    private static (string NewLine, int Count) ReplaceProcedureStatement(
        CobolLine line,
        Dictionary<string, string> aliasMap)
    {
        var combined = line.AreaA + line.AreaB;
        int count = 0;

        // Check for PROGRAM-ID in this line (can appear in IDENTIFICATION DIVISION)
        var progIdMatch = Regex.Match(combined, @"(PROGRAM-ID\s*\.\s*)([\w-]+)",
            RegexOptions.IgnoreCase);
        if (progIdMatch.Success)
        {
            var progName = progIdMatch.Groups[2].Value.ToUpperInvariant();
            if (aliasMap.TryGetValue(progName, out var progAlias))
            {
                combined = combined[..progIdMatch.Groups[2].Index]
                    + progAlias
                    + combined[(progIdMatch.Groups[2].Index + progIdMatch.Groups[2].Length)..];
                count++;
                return (RebuildFromCombined(line, combined), count);
            }
        }

        // Check for FD/SD file description
        var fdMatch = Regex.Match(combined, @"^(\s*(?:FD|SD)\s+)([\w-]+)(.*)$",
            RegexOptions.IgnoreCase);
        if (fdMatch.Success)
        {
            var fileName = fdMatch.Groups[2].Value.ToUpperInvariant();
            if (aliasMap.TryGetValue(fileName, out var fileAlias))
            {
                combined = fdMatch.Groups[1].Value + fileAlias + fdMatch.Groups[3].Value;
                count++;
            }
            return (RebuildFromCombined(line, combined), count);
        }

        // General identifier replacement in procedure statements
        // Protect string literals first
        var (newContent, replCount) = ReplaceIdentifiersInText(combined, aliasMap);
        count += replCount;

        return (RebuildFromCombined(line, newContent), count);
    }

    /// <summary>
    /// Replace all matching identifiers in a text fragment while preserving
    /// string literals, reserved words, level numbers, and PIC clause content.
    /// </summary>
    private static (string Result, int Count) ReplaceIdentifiersInText(
        string text,
        Dictionary<string, string> aliasMap)
    {
        if (string.IsNullOrWhiteSpace(text))
            return (text, 0);

        int count = 0;

        // Identify protected regions: string literals (single-quoted and double-quoted)
        var protectedRegions = new List<(int Start, int End)>();

        // Protect string literals
        foreach (Match m in Regex.Matches(text, @"'[^']*'|""[^""]*"""))
        {
            protectedRegions.Add((m.Index, m.Index + m.Length));
        }

        // Protect PIC/PICTURE clauses
        foreach (Match m in PicClausePattern.Matches(text))
        {
            // Protect everything after PIC/PICTURE keyword
            var picKeywordEnd = text.IndexOf(' ', m.Index) + 1;
            protectedRegions.Add((picKeywordEnd, m.Index + m.Length));
        }

        // Replace identifiers token by token
        var result = CobolIdentifierPattern.Replace(text, match =>
        {
            // Check if this match is in a protected region
            foreach (var (start, end) in protectedRegions)
            {
                if (match.Index >= start && match.Index < end)
                    return match.Value;
            }

            var word = match.Value.ToUpperInvariant();

            // Don't replace reserved words
            if (ReservedWords.Contains(word))
                return match.Value;

            // Don't replace level numbers
            if (int.TryParse(word, out var num) &&
                ((num >= 1 && num <= 49) || num == 66 || num == 77 || num == 88))
                return match.Value;

            // Don't replace numeric literals
            if (word.All(c => char.IsDigit(c) || c == '.' || c == '-' || c == '+'))
                return match.Value;

            if (aliasMap.TryGetValue(word, out var alias))
            {
                count++;
                return alias;
            }

            return match.Value;
        });

        return (result, count);
    }

    // ── Deobfuscation ───────────────────────────────────────────────────

    /// <summary>
    /// Deobfuscate a single line by finding alias patterns and looking them up
    /// in the context's reverse mapping.
    /// </summary>
    private (string NewLine, int Count) DeobfuscateLine(
        CobolLine line,
        ObfuscationContext context,
        List<string> warnings)
    {
        // warnings is available for future use when deobfuscation encounters issues
        _ = warnings;

        if (line.LineType == CobolLineType.Blank)
            return (line.OriginalText, 0);

        // For comment lines that were replaced, we cannot restore original comments
        // (the original text is not in the mapping). Leave as-is.
        if (line.LineType == CobolLineType.Comment)
            return (line.OriginalText, 0);

        var combined = line.AreaA + line.AreaB;
        int count = 0;

        // For EXEC SQL blocks, also handle host variables with : prefix
        if (line.LineType == CobolLineType.ExecSqlBlock)
        {
            combined = Regex.Replace(combined, @":([\w]+_\d+)", match =>
            {
                var alias = match.Groups[1].Value;
                var original = context.Mappings.GetOriginal(alias);
                if (original != null)
                {
                    count++;
                    return ":" + original;
                }
                return match.Value;
            });
        }

        // Replace all alias patterns (PREFIX_N) with original values
        combined = AliasPattern.Replace(combined, match =>
        {
            var alias = match.Groups[1].Value;
            var original = context.Mappings.GetOriginal(alias);
            if (original != null)
            {
                count++;
                return original;
            }
            return match.Value;
        });

        if (count > 0)
        {
            return (RebuildFromCombined(line, combined), count);
        }

        return (line.OriginalText, 0);
    }

    // ── Line rebuilding utilities ───────────────────────────────────────

    /// <summary>
    /// Rebuild a COBOL line from its component areas.
    /// </summary>
    private static string RebuildLine(
        string sequenceArea,
        char indicator,
        string areaA,
        string areaB,
        string identArea)
    {
        // Ensure areas are properly sized
        var seq = sequenceArea.Length >= 6 ? sequenceArea[..6] : sequenceArea.PadRight(6);
        var aA = areaA.Length >= 4 ? areaA[..4] : areaA.PadRight(4);

        // Area B should be at most 61 characters (cols 12-72)
        string aB;
        if (areaB.Length > 61)
            aB = areaB[..61];
        else
            aB = areaB;

        var sb = new StringBuilder(80);
        sb.Append(seq);
        sb.Append(indicator);
        sb.Append(aA);
        sb.Append(aB);

        // Add identification area if present
        if (!string.IsNullOrEmpty(identArea))
        {
            // Pad to column 73 if needed
            while (sb.Length < 72)
                sb.Append(' ');
            sb.Append(identArea.Length > 8 ? identArea[..8] : identArea);
        }

        return sb.ToString();
    }

    /// <summary>
    /// Rebuild a line from the combined Area A + Area B content, preserving
    /// the original sequence area, indicator, and identification area.
    /// </summary>
    private static string RebuildFromCombined(CobolLine line, string newCombined)
    {
        // Split combined content back into Area A (4 chars) and Area B (remainder)
        string areaA, areaB;
        if (newCombined.Length >= 4)
        {
            areaA = newCombined[..4];
            areaB = newCombined[4..];
        }
        else
        {
            areaA = newCombined.PadRight(4);
            areaB = string.Empty;
        }

        // Truncate Area B to fit within columns 12-72 (max 61 chars)
        if (areaB.Length > 61)
        {
            areaB = areaB[..61];
        }

        var sb = new StringBuilder(80);
        sb.Append(line.SequenceArea.Length >= 6 ? line.SequenceArea[..6] : line.SequenceArea.PadRight(6));
        sb.Append(line.Indicator);
        sb.Append(areaA);
        sb.Append(areaB);

        // Preserve identification area at cols 73-80
        if (!string.IsNullOrEmpty(line.IdentArea) && line.OriginalText.Length > 72)
        {
            while (sb.Length < 72)
                sb.Append(' ');
            sb.Append(line.IdentArea.Length > 8 ? line.IdentArea[..8] : line.IdentArea);
        }

        return TrimToOriginalLength(sb.ToString(), line.OriginalText);
    }

    /// <summary>
    /// Trim the rebuilt line to not exceed the original line's length,
    /// or leave it as-is if the original was shorter than 72 columns.
    /// Also handles the case where the original line had trailing spaces.
    /// </summary>
    private static string TrimToOriginalLength(string newLine, string originalText)
    {
        // If the original line was short (no ident area), trim trailing spaces
        // but preserve at least the content
        if (originalText.Length <= 72)
        {
            // Trim trailing spaces but not below the content length
            return newLine.TrimEnd();
        }

        // For full 80-column lines, maintain the exact length
        if (newLine.Length > 80 && originalText.Length <= 80)
        {
            return newLine[..80];
        }

        return newLine;
    }

    // ── EXEC block resolution ───────────────────────────────────────────

    /// <summary>
    /// Resolve multi-line EXEC SQL/CICS blocks.
    /// When EXEC SQL starts on one line and END-EXEC is on a later line,
    /// mark all intermediate lines as part of the exec block.
    /// </summary>
    private static void ResolveExecBlocks(List<CobolLine> lines)
    {
        bool inExecSql = false;
        bool inExecCics = false;

        for (int i = 0; i < lines.Count; i++)
        {
            var line = lines[i];
            var combined = (line.AreaA + line.AreaB).Trim().ToUpperInvariant();

            if (line.LineType == CobolLineType.Comment || line.LineType == CobolLineType.Blank)
                continue;

            if (inExecSql)
            {
                line.LineType = CobolLineType.ExecSqlBlock;
                if (combined.Contains("END-EXEC"))
                {
                    inExecSql = false;
                }
            }
            else if (inExecCics)
            {
                line.LineType = CobolLineType.ExecCicsBlock;
                if (combined.Contains("END-EXEC"))
                {
                    inExecCics = false;
                }
            }
            else if (line.LineType == CobolLineType.ExecSqlBlock)
            {
                if (!combined.Contains("END-EXEC"))
                {
                    inExecSql = true;
                }
            }
            else if (line.LineType == CobolLineType.ExecCicsBlock)
            {
                if (!combined.Contains("END-EXEC"))
                {
                    inExecCics = true;
                }
            }
        }
    }
}
