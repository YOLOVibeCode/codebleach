using System.Collections.Frozen;
using System.Text;
using System.Text.RegularExpressions;
using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;

namespace CodeBleach.Processors.Jcl;

#region Line Classification

/// <summary>
/// Classifies the type of a JCL source line.
/// </summary>
internal enum JclLineType
{
    Job,
    Exec,
    Dd,
    Proc,
    Pend,
    Set,
    If,
    Endif,
    Include,
    Jcllib,
    Comment,
    Instream,
    Null,
    Continuation,
    Delimiter,
    Unknown
}

/// <summary>
/// Parsed representation of a single JCL source line, capturing the label/name field,
/// operation keyword, and operand text for downstream obfuscation or deobfuscation.
/// </summary>
internal sealed class JclLine
{
    public int LineNumber { get; init; }
    public required string OriginalText { get; init; }
    public JclLineType LineType { get; set; }
    public string? Name { get; set; }
    public string? Operation { get; set; }
    public string? Operands { get; set; }

    /// <summary>
    /// For instream data lines, the PGM= value from the most recent EXEC card
    /// in the same step. Used to determine whether to delegate to the SQL processor.
    /// </summary>
    public string? AssociatedProgram { get; set; }
}

#endregion

/// <summary>
/// Language processor for IBM JCL (Job Control Language). Implements a custom line-based
/// parser with a two-pass approach: discovery (extract all identifiers) followed by
/// line-by-line replacement. Handles JOB, EXEC, DD, PROC, SET, and continuation cards.
/// </summary>
public sealed class JclLanguageProcessor : ILanguageProcessor
{
    #region Constants and Reserved Sets

    private const string ProcessorIdValue = "jcl";
    private const int MaxJclLineLength = 80;

    private static readonly FrozenSet<string> SupportedExtensionSet =
        new HashSet<string>(StringComparer.OrdinalIgnoreCase) { ".jcl", ".prc" }.ToFrozenSet(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// System DD names that must never be renamed during obfuscation.
    /// </summary>
    private static readonly FrozenSet<string> SystemDdNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "SYSPRINT", "SYSIN", "SYSOUT", "SYSUDUMP", "SYSABEND", "SYSDBOUT",
        "SYSDUMP", "SYSTSPRT", "STEPLIB", "JOBLIB", "SYSLIB", "SYSLMOD",
        "SYSUT1", "SYSUT2", "SYSUT3", "SYSUT4",
        "CEEDUMP", "SYSMDUMP", "SORTLIB", "DFHRPL", "SYSTSIN"
    }.ToFrozenSet(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// JCL operation keywords that must be preserved verbatim.
    /// </summary>
    private static readonly FrozenSet<string> JclKeywords = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "JOB", "EXEC", "DD", "PROC", "PEND", "SET", "IF", "ENDIF", "ELSE",
        "INCLUDE", "JCLLIB", "OUTPUT", "COMMAND", "CNTL", "ENDCNTL",
        "EXPORT", "NOTIFY", "XMIT"
    }.ToFrozenSet(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// DD parameter keywords within the operand field that must not be renamed.
    /// Used to distinguish keyword parameters from user values in operand parsing.
    /// </summary>
    internal static readonly FrozenSet<string> DdParameterKeywords = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "DSN", "DSNAME", "DISP", "SPACE", "DCB", "UNIT", "VOL", "SYSOUT", "DUMMY",
        "REFER", "LIKE", "PATH", "LRECL", "BLKSIZE", "RECFM", "DSORG", "BUFNO",
        "EROPT", "LABEL", "EXPDT", "RETPD", "AMP", "BURST", "CHARS", "COPIES",
        "DEST", "FCB", "FLASH", "HOLD", "MODIFY", "OUTLIM", "SPIN", "FREE",
        "DLM", "DATA", "AVGREC", "MGMTCLAS", "STORCLAS", "DATACLAS", "PGM",
        "PARM", "COND", "REGION", "TIME", "ACCT", "CLASS", "MSGCLASS",
        "MSGLEVEL", "TYPRUN", "RESTART", "RD", "ADDRSPC", "BYTES", "CARDS",
        "LINES", "PAGES", "PERFORM", "SCHENV", "NOTIFY"
    }.ToFrozenSet(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// DISP sub-parameter values that must be preserved verbatim.
    /// Used to protect disposition keywords when encountered in operand values.
    /// </summary>
    internal static readonly FrozenSet<string> DispValues = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "NEW", "OLD", "SHR", "MOD", "CATLG", "UNCATLG", "DELETE", "KEEP", "PASS"
    }.ToFrozenSet(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// z/OS system symbols that must never be replaced.
    /// </summary>
    private static readonly FrozenSet<string> SystemSymbols = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "&SYSUID", "&SYSDATE", "&SYSTIME", "&SYSJOBID", "&SYSSTEP", "&SYSNAME"
    }.ToFrozenSet(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// z/OS programs whose SYSIN instream data contains SQL and should be
    /// delegated to the SQL processor for obfuscation.
    /// </summary>
    private static readonly FrozenSet<string> SqlPrograms = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "IKJEFT01", "DSNTEP2", "DSNTEP4", "DSNTIAD", "DSNUPROC", "IKJEFT1B"
    }.ToFrozenSet(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// Pattern matching SORTWKnn (SORTWK01 through SORTWK99).
    /// </summary>
    private static readonly Regex SortWorkDdPattern = new(@"^SORTWK\d{1,2}$", RegexOptions.IgnoreCase | RegexOptions.Compiled);

    /// <summary>
    /// Pattern for DSN= or DSNAME= values: <c>DSN=hlq.qualifier.data</c> or <c>DSN=hlq.lib(member)</c>.
    /// </summary>
    private static readonly Regex DsnPattern = new(
        @"(?:DSN|DSNAME)\s*=\s*(?<dsn>[A-Za-z@#$][A-Za-z0-9@#$.\-]*(?:\([A-Za-z@#$][A-Za-z0-9@#$]*\))?)",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    /// <summary>
    /// Pattern for PGM= values: <c>PGM=IKJEFT01</c>.
    /// </summary>
    private static readonly Regex PgmPattern = new(
        @"PGM\s*=\s*(?<pgm>[A-Za-z@#$][A-Za-z0-9@#$]{0,7})",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    /// <summary>
    /// Pattern for PARM= values including quoted strings: <c>PARM='value'</c> or <c>PARM=value</c>.
    /// </summary>
    private static readonly Regex ParmPattern = new(
        @"PARM\s*=\s*(?:'(?<parmq>[^']*)'|(?<parm>[^,\s)]+))",
        RegexOptions.IgnoreCase | RegexOptions.Compiled);

    /// <summary>
    /// Pattern for JCL symbolic parameters: <c>&amp;SYMBOL</c> or <c>&amp;SYMBOL.</c>.
    /// </summary>
    private static readonly Regex SymbolPattern = new(
        @"&(?<sym>[A-Za-z@#$][A-Za-z0-9@#$]*)\.?",
        RegexOptions.Compiled);

    /// <summary>
    /// Pattern for an alias produced by the naming strategy: <c>PREFIX_N</c>.
    /// </summary>
    private static readonly Regex AliasPattern = new(
        @"\b[A-Z]{2,5}_\d+\b",
        RegexOptions.Compiled);

    #endregion

    #region ILanguageProcessor Properties

    public string ProcessorId => ProcessorIdValue;
    public string DisplayName => "JCL";
    public IReadOnlySet<string> SupportedExtensions => SupportedExtensionSet;
    public int Priority => 10;

    #endregion

    #region ILanguageProcessor Methods

    /// <inheritdoc />
    public bool CanProcess(string filePath, string content)
    {
        if (string.IsNullOrWhiteSpace(content))
            return false;

        var extension = System.IO.Path.GetExtension(filePath);
        if (SupportedExtensionSet.Contains(extension))
            return true;

        // Heuristic: if content starts with // lines typical of JCL
        var lines = content.Split('\n', 5);
        int jclLineCount = 0;
        foreach (var line in lines)
        {
            var trimmed = line.TrimEnd('\r');
            if (trimmed.StartsWith("//", StringComparison.Ordinal))
                jclLineCount++;
        }

        return jclLineCount >= 2;
    }

    /// <inheritdoc />
    public LanguageProcessingResult Obfuscate(string content, ObfuscationContext context, string? filePath = null)
    {
        // Delegation-only: parse structure but only delegate instream SQL blocks.
        // JCL identifiers (job names, step names, DD names, etc.) remain untouched.
        if (context.Scope.IsDelegationOnly(ProcessorIdValue))
        {
            return ObfuscateDelegationOnly(content, context, filePath);
        }

        try
        {
            var warnings = new List<string>();
            var lines = ParseLines(content, warnings);

            // Pass 1: Discovery - register all identifiers
            DiscoverIdentifiers(lines, context, filePath, warnings);

            // Pass 2: Replacement - rebuild each line
            // Instream blocks belonging to SQL programs are collected and delegated
            // to the SQL processor as a single concatenated string.
            var replacementCount = 0;
            var builder = new StringBuilder(content.Length);

            for (int i = 0; i < lines.Count; i++)
            {
                if (i > 0) builder.Append('\n');

                var line = lines[i];

                // Buffer consecutive instream lines for potential SQL delegation
                if (line.LineType == JclLineType.Instream
                    && line.AssociatedProgram != null
                    && SqlPrograms.Contains(line.AssociatedProgram))
                {
                    var instreamLines = new List<JclLine> { line };
                    while (i + 1 < lines.Count && lines[i + 1].LineType == JclLineType.Instream)
                    {
                        i++;
                        instreamLines.Add(lines[i]);
                    }

                    var obfuscated = ObfuscateInstreamSqlBlock(instreamLines, context, ref replacementCount, warnings);
                    builder.Append(obfuscated);
                    continue;
                }

                var replaced = ObfuscateLine(line, context, filePath, ref replacementCount, warnings);
                builder.Append(replaced);
            }

            var result = builder.ToString();

            if (filePath != null)
            {
                context.RecordFileProcessing(filePath, ProcessorIdValue, replacementCount);
            }

            return new LanguageProcessingResult
            {
                Content = result,
                WasTransformed = replacementCount > 0,
                ReplacementCount = replacementCount,
                ProcessorId = ProcessorIdValue,
                Warnings = warnings
            };
        }
        catch (Exception ex)
        {
            return new LanguageProcessingResult
            {
                Content = content,
                WasTransformed = false,
                ReplacementCount = 0,
                ProcessorId = ProcessorIdValue,
                Warnings = [$"JCL obfuscation failed: {ex.Message}"]
            };
        }
    }

    /// <summary>
    /// Delegation-only mode: parse JCL structure but only delegate instream SQL blocks
    /// to the SQL processor. All JCL identifiers remain untouched.
    /// Used when JCL is out of scope but SQL (database) is in scope.
    /// </summary>
    private LanguageProcessingResult ObfuscateDelegationOnly(string content, ObfuscationContext context, string? filePath)
    {
        try
        {
            var warnings = new List<string>();
            var lines = ParseLines(content, warnings);

            // Skip Pass 1 (discovery) — JCL identifiers are not being obfuscated.
            // We still need to parse lines so we know which instream blocks are SQL.

            var replacementCount = 0;
            var builder = new StringBuilder(content.Length);

            for (int i = 0; i < lines.Count; i++)
            {
                if (i > 0) builder.Append('\n');

                var line = lines[i];

                // Buffer consecutive instream lines for SQL delegation
                if (line.LineType == JclLineType.Instream
                    && line.AssociatedProgram != null
                    && SqlPrograms.Contains(line.AssociatedProgram))
                {
                    var instreamLines = new List<JclLine> { line };
                    while (i + 1 < lines.Count && lines[i + 1].LineType == JclLineType.Instream)
                    {
                        i++;
                        instreamLines.Add(lines[i]);
                    }

                    var obfuscated = ObfuscateInstreamSqlBlock(instreamLines, context, ref replacementCount, warnings);
                    builder.Append(obfuscated);
                    continue;
                }

                // All non-SQL lines pass through unchanged
                builder.Append(line.OriginalText);
            }

            var result = builder.ToString();

            if (filePath != null && replacementCount > 0)
            {
                context.RecordFileProcessing(filePath, ProcessorIdValue, replacementCount);
            }

            return new LanguageProcessingResult
            {
                Content = result,
                WasTransformed = replacementCount > 0,
                ReplacementCount = replacementCount,
                ProcessorId = ProcessorIdValue,
                Warnings = warnings
            };
        }
        catch (Exception ex)
        {
            return new LanguageProcessingResult
            {
                Content = content,
                WasTransformed = false,
                ReplacementCount = 0,
                ProcessorId = ProcessorIdValue,
                Warnings = [$"JCL delegation-only processing failed: {ex.Message}"]
            };
        }
    }

    /// <inheritdoc />
    public LanguageProcessingResult Deobfuscate(string content, ObfuscationContext context, string? filePath = null)
    {
        try
        {
            var warnings = new List<string>();
            var replacementCount = 0;
            var result = new StringBuilder(content.Length);

            var rawLines = content.Split('\n');
            for (int i = 0; i < rawLines.Length; i++)
            {
                if (i > 0) result.Append('\n');

                var line = rawLines[i].TrimEnd('\r');
                var restored = DeobfuscateLine(line, context, ref replacementCount);
                result.Append(restored);
            }

            return new LanguageProcessingResult
            {
                Content = result.ToString(),
                WasTransformed = replacementCount > 0,
                ReplacementCount = replacementCount,
                ProcessorId = ProcessorIdValue,
                Warnings = warnings
            };
        }
        catch (Exception ex)
        {
            return new LanguageProcessingResult
            {
                Content = content,
                WasTransformed = false,
                ReplacementCount = 0,
                ProcessorId = ProcessorIdValue,
                Warnings = [$"JCL deobfuscation failed: {ex.Message}"]
            };
        }
    }

    /// <inheritdoc />
    public ValidationResult Validate(string obfuscatedContent)
    {
        try
        {
            var errors = new List<string>();
            var warnings = new List<string>();
            var rawLines = obfuscatedContent.Split('\n');

            bool hasJobCard = false;
            bool inInstreamData = false;
            string? instreamDelimiter = null;

            for (int i = 0; i < rawLines.Length; i++)
            {
                var line = rawLines[i].TrimEnd('\r');
                int lineNum = i + 1;

                if (inInstreamData)
                {
                    // Check for delimiter ending instream data
                    if (instreamDelimiter != null && line.StartsWith(instreamDelimiter, StringComparison.Ordinal))
                    {
                        inInstreamData = false;
                        instreamDelimiter = null;
                    }
                    else if (line == "/*")
                    {
                        inInstreamData = false;
                        instreamDelimiter = null;
                    }
                    continue;
                }

                if (string.IsNullOrWhiteSpace(line))
                    continue;

                if (line.StartsWith("//", StringComparison.Ordinal))
                {
                    // Check for JOB card
                    if (!hasJobCard)
                    {
                        var parsed = ClassifyLine(line, i + 1);
                        if (parsed.LineType == JclLineType.Job)
                            hasJobCard = true;
                    }

                    // Check if this DD introduces instream data
                    var parsedLine = ClassifyLine(line, lineNum);
                    if (parsedLine.LineType == JclLineType.Dd && parsedLine.Operands != null)
                    {
                        var operands = parsedLine.Operands.Trim();
                        if (operands == "*" || operands.StartsWith("*,", StringComparison.Ordinal))
                        {
                            inInstreamData = true;
                            // Check for DLM= parameter
                            var dlmMatch = Regex.Match(operands, @"DLM=(.{2})", RegexOptions.IgnoreCase);
                            instreamDelimiter = dlmMatch.Success ? dlmMatch.Groups[1].Value : null;
                        }
                        else if (operands.StartsWith("DATA", StringComparison.OrdinalIgnoreCase))
                        {
                            inInstreamData = true;
                            var dlmMatch = Regex.Match(operands, @"DLM=(.{2})", RegexOptions.IgnoreCase);
                            instreamDelimiter = dlmMatch.Success ? dlmMatch.Groups[1].Value : null;
                        }
                    }
                }
                else if (line == "/*")
                {
                    // Delimiter statement outside instream - valid
                }
                else if (!line.StartsWith("//", StringComparison.Ordinal))
                {
                    // Unexpected line outside instream data - could be problematic
                    // but some JCL decks have trailing blank lines; be lenient
                    if (!string.IsNullOrWhiteSpace(line))
                    {
                        warnings.Add($"Line {lineNum}: Non-JCL line outside instream data.");
                    }
                }
            }

            if (!hasJobCard)
            {
                // Not necessarily invalid - could be a PROC without JOB card
                warnings.Add("No JOB card found. This may be a catalogued procedure.");
            }

            if (errors.Count > 0)
                return ValidationResult.Invalid(errors);

            return new ValidationResult
            {
                IsValid = true,
                Warnings = warnings
            };
        }
        catch (Exception ex)
        {
            return ValidationResult.Invalid([$"Validation error: {ex.Message}"]);
        }
    }

    #endregion

    #region Line Parsing

    /// <summary>
    /// Splits raw content into classified <see cref="JclLine"/> records, handling
    /// comment detection, null statements, continuation merging, and instream data boundaries.
    /// </summary>
    private static List<JclLine> ParseLines(string content, List<string> warnings)
    {
        var rawLines = content.Split('\n');
        var parsed = new List<JclLine>(rawLines.Length);
        bool inInstreamData = false;
        string? instreamDelimiter = null;
        string? currentProgram = null; // PGM= from the most recent EXEC card

        for (int i = 0; i < rawLines.Length; i++)
        {
            var rawText = rawLines[i].TrimEnd('\r');
            int lineNum = i + 1;

            if (inInstreamData)
            {
                // Check for end-of-instream delimiters
                if (instreamDelimiter != null && rawText.StartsWith(instreamDelimiter, StringComparison.Ordinal))
                {
                    parsed.Add(new JclLine
                    {
                        LineNumber = lineNum,
                        OriginalText = rawText,
                        LineType = JclLineType.Delimiter
                    });
                    inInstreamData = false;
                    instreamDelimiter = null;
                }
                else if (rawText == "/*")
                {
                    parsed.Add(new JclLine
                    {
                        LineNumber = lineNum,
                        OriginalText = rawText,
                        LineType = JclLineType.Delimiter
                    });
                    inInstreamData = false;
                    instreamDelimiter = null;
                }
                else
                {
                    parsed.Add(new JclLine
                    {
                        LineNumber = lineNum,
                        OriginalText = rawText,
                        LineType = JclLineType.Instream,
                        AssociatedProgram = currentProgram
                    });
                }
                continue;
            }

            var jclLine = ClassifyLine(rawText, lineNum);
            parsed.Add(jclLine);

            // Track PGM= from EXEC cards
            if (jclLine.LineType == JclLineType.Exec && !string.IsNullOrEmpty(jclLine.Operands))
            {
                var pgmMatch = PgmPattern.Match(jclLine.Operands);
                currentProgram = pgmMatch.Success ? pgmMatch.Groups["pgm"].Value : null;
            }

            // Detect start of instream data: DD * or DD DATA
            if (jclLine.LineType == JclLineType.Dd && jclLine.Operands != null)
            {
                var operandsTrimmed = jclLine.Operands.Trim();
                bool isInstream = false;

                if (operandsTrimmed == "*" || operandsTrimmed.StartsWith("*,", StringComparison.Ordinal)
                    || operandsTrimmed.StartsWith("* ", StringComparison.Ordinal))
                {
                    isInstream = true;
                }
                else if (operandsTrimmed.StartsWith("DATA", StringComparison.OrdinalIgnoreCase))
                {
                    isInstream = true;
                }

                if (isInstream)
                {
                    inInstreamData = true;
                    var dlmMatch = Regex.Match(operandsTrimmed, @"DLM=(.{2})", RegexOptions.IgnoreCase);
                    instreamDelimiter = dlmMatch.Success ? dlmMatch.Groups[1].Value : null;
                }
            }
        }

        // Identify continuation lines and link them to their parent statement
        for (int i = 1; i < parsed.Count; i++)
        {
            if (parsed[i].LineType != JclLineType.Unknown)
                continue;

            // A continuation is a line starting with // followed by at least one space and no name
            var text = parsed[i].OriginalText;
            if (text.StartsWith("//", StringComparison.Ordinal) && text.Length > 2)
            {
                var afterSlashes = text[2..];
                if (afterSlashes.Length > 0 && afterSlashes[0] == ' ')
                {
                    // Look back for a preceding statement whose operands end with comma
                    for (int j = i - 1; j >= 0; j--)
                    {
                        var prev = parsed[j];
                        if (prev.LineType == JclLineType.Comment || prev.LineType == JclLineType.Instream)
                            continue;

                        if (prev.Operands != null && prev.Operands.TrimEnd().EndsWith(','))
                        {
                            parsed[i].LineType = JclLineType.Continuation;
                            parsed[i].Operands = afterSlashes.TrimStart();
                        }
                        break;
                    }
                }
            }
        }

        return parsed;
    }

    /// <summary>
    /// Classifies a single raw JCL line and extracts its name, operation, and operand fields.
    /// </summary>
    private static JclLine ClassifyLine(string rawText, int lineNum)
    {
        var jclLine = new JclLine
        {
            LineNumber = lineNum,
            OriginalText = rawText
        };

        // Comment: //*
        if (rawText.StartsWith("//*", StringComparison.Ordinal))
        {
            jclLine.LineType = JclLineType.Comment;
            return jclLine;
        }

        // Null statement: exactly //
        if (rawText == "//")
        {
            jclLine.LineType = JclLineType.Null;
            return jclLine;
        }

        // Delimiter: /*
        if (rawText == "/*")
        {
            jclLine.LineType = JclLineType.Delimiter;
            return jclLine;
        }

        // JCL statement: // followed by content
        if (rawText.StartsWith("//", StringComparison.Ordinal) && rawText.Length > 2)
        {
            var body = rawText[2..]; // everything after //

            // Check if continuation (starts with spaces, no name)
            if (body.Length > 0 && body[0] == ' ')
            {
                // Could be a continuation or a nameless statement
                var trimmed = body.TrimStart();
                if (string.IsNullOrEmpty(trimmed))
                {
                    jclLine.LineType = JclLineType.Null;
                    return jclLine;
                }

                // Extract operation from trimmed content
                var opEnd = FindTokenEnd(trimmed, 0);
                var operation = trimmed[..opEnd].ToUpperInvariant();

                if (JclKeywords.Contains(operation))
                {
                    jclLine.Operation = operation;
                    jclLine.LineType = MapOperationToLineType(operation);
                    if (opEnd < trimmed.Length)
                    {
                        jclLine.Operands = trimmed[opEnd..].TrimStart();
                    }
                    return jclLine;
                }

                // If not a recognized keyword, mark as unknown (may be reclassified as continuation)
                jclLine.LineType = JclLineType.Unknown;
                jclLine.Operands = trimmed;
                return jclLine;
            }

            // Has a name field starting at column 3
            var nameEnd = FindTokenEnd(body, 0);
            var name = body[..nameEnd];
            jclLine.Name = name;

            var rest = nameEnd < body.Length ? body[nameEnd..].TrimStart() : string.Empty;

            if (string.IsNullOrEmpty(rest))
            {
                // Name only, no operation - unusual but possible
                jclLine.LineType = JclLineType.Unknown;
                return jclLine;
            }

            var restOpEnd = FindTokenEnd(rest, 0);
            var op = rest[..restOpEnd].ToUpperInvariant();
            jclLine.Operation = op;
            jclLine.LineType = MapOperationToLineType(op);

            if (restOpEnd < rest.Length)
            {
                jclLine.Operands = rest[restOpEnd..].TrimStart();
            }

            return jclLine;
        }

        // Non-JCL line (no // prefix) - treat as instream data or unknown
        if (!rawText.StartsWith("//", StringComparison.Ordinal))
        {
            jclLine.LineType = JclLineType.Unknown;
            return jclLine;
        }

        jclLine.LineType = JclLineType.Unknown;
        return jclLine;
    }

    /// <summary>
    /// Maps an operation keyword string to the corresponding <see cref="JclLineType"/>.
    /// </summary>
    private static JclLineType MapOperationToLineType(string operation) => operation.ToUpperInvariant() switch
    {
        "JOB" => JclLineType.Job,
        "EXEC" => JclLineType.Exec,
        "DD" => JclLineType.Dd,
        "PROC" => JclLineType.Proc,
        "PEND" => JclLineType.Pend,
        "SET" => JclLineType.Set,
        "IF" => JclLineType.If,
        "ENDIF" => JclLineType.Endif,
        "INCLUDE" => JclLineType.Include,
        "JCLLIB" => JclLineType.Jcllib,
        _ => JclLineType.Unknown
    };

    /// <summary>
    /// Finds the end index of a whitespace-delimited token starting at <paramref name="start"/>.
    /// </summary>
    private static int FindTokenEnd(string text, int start)
    {
        int i = start;
        while (i < text.Length && !char.IsWhiteSpace(text[i]))
            i++;
        return i;
    }

    #endregion

    #region Pass 1 - Discovery

    /// <summary>
    /// First pass over all parsed lines: registers every user-defined identifier
    /// with the shared <see cref="ObfuscationContext"/> so aliases are deterministic
    /// regardless of the order lines are later processed.
    /// </summary>
    private void DiscoverIdentifiers(List<JclLine> lines, ObfuscationContext context, string? filePath, List<string> warnings)
    {
        for (int i = 0; i < lines.Count; i++)
        {
            var line = lines[i];

            try
            {
                switch (line.LineType)
                {
                    case JclLineType.Job:
                        DiscoverJobCard(line, context, filePath);
                        break;

                    case JclLineType.Exec:
                        DiscoverExecCard(line, context, filePath);
                        break;

                    case JclLineType.Dd:
                        DiscoverDdCard(line, context, filePath);
                        break;

                    case JclLineType.Proc:
                        if (!string.IsNullOrEmpty(line.Name))
                        {
                            context.GetOrCreateAlias(line.Name, SemanticCategory.Proc, filePath, line.LineNumber);
                        }
                        break;

                    case JclLineType.Set:
                        DiscoverSetStatement(line, context, filePath);
                        break;

                    case JclLineType.Include:
                        DiscoverIncludeStatement(line, context, filePath);
                        break;

                    case JclLineType.Continuation:
                        // Discover identifiers in continuation operands using the same logic
                        // as operand scanning (DSN, PGM, PARM, symbols)
                        if (!string.IsNullOrEmpty(line.Operands))
                        {
                            DiscoverOperandIdentifiers(line.Operands, context, filePath, line.LineNumber);
                        }
                        break;
                }
            }
            catch (Exception ex)
            {
                warnings.Add($"Line {line.LineNumber}: Discovery error - {ex.Message}");
            }
        }
    }

    private static void DiscoverJobCard(JclLine line, ObfuscationContext context, string? filePath)
    {
        // Job name
        if (!string.IsNullOrEmpty(line.Name))
        {
            context.GetOrCreateAlias(line.Name, SemanticCategory.Job, filePath, line.LineNumber);
        }

        if (string.IsNullOrEmpty(line.Operands))
            return;

        // JOB operands: accounting-info,'programmer-name',...
        // Parse accounting info and programmer name as parameters
        var operands = line.Operands;
        var parts = SplitTopLevelOperands(operands);

        // First positional: accounting information
        if (parts.Count > 0 && !string.IsNullOrWhiteSpace(parts[0]) && !parts[0].Contains('='))
        {
            var acctInfo = parts[0].Trim().Trim('\'', '(', ')');
            if (!string.IsNullOrWhiteSpace(acctInfo))
            {
                context.GetOrCreateAlias(acctInfo, SemanticCategory.Parameter, filePath, line.LineNumber);
            }
        }

        // Second positional: programmer name (often quoted)
        if (parts.Count > 1 && !string.IsNullOrWhiteSpace(parts[1]) && !parts[1].Contains('='))
        {
            var progName = parts[1].Trim().Trim('\'');
            if (!string.IsNullOrWhiteSpace(progName))
            {
                context.GetOrCreateAlias(progName, SemanticCategory.Parameter, filePath, line.LineNumber);
            }
        }

        // Discover any DSN, PGM, PARM, symbols in keyword parameters
        DiscoverOperandIdentifiers(operands, context, filePath, line.LineNumber);
    }

    private static void DiscoverExecCard(JclLine line, ObfuscationContext context, string? filePath)
    {
        // Step name
        if (!string.IsNullOrEmpty(line.Name))
        {
            context.GetOrCreateAlias(line.Name, SemanticCategory.Step, filePath, line.LineNumber);
        }

        if (string.IsNullOrEmpty(line.Operands))
            return;

        var operands = line.Operands;

        // PGM=programname
        var pgmMatch = PgmPattern.Match(operands);
        if (pgmMatch.Success)
        {
            var pgmName = pgmMatch.Groups["pgm"].Value;
            var pgmAlias = context.GetOrCreateAlias(pgmName, SemanticCategory.Program, filePath, line.LineNumber);

            // Record JCL→COBOL cross-reference
            context.AddCrossReference(new CrossLanguageReference
            {
                Alias = pgmAlias,
                SourceLanguage = "JCL",
                TargetLanguage = "COBOL",
                SourceFile = filePath,
                Description = $"JCL step executes COBOL program {pgmName} as {pgmAlias}"
            });
        }
        else
        {
            // EXEC procname (no PGM=, first token is a proc name)
            var firstToken = operands.Split([' ', ','], StringSplitOptions.RemoveEmptyEntries).FirstOrDefault();
            if (firstToken != null && !firstToken.Contains('=') && !JclKeywords.Contains(firstToken))
            {
                context.GetOrCreateAlias(firstToken, SemanticCategory.Proc, filePath, line.LineNumber);
            }
        }

        // PARM= and symbols
        DiscoverOperandIdentifiers(operands, context, filePath, line.LineNumber);
    }

    private static void DiscoverDdCard(JclLine line, ObfuscationContext context, string? filePath)
    {
        // DD name
        if (!string.IsNullOrEmpty(line.Name) && !IsSystemDdName(line.Name))
        {
            context.GetOrCreateAlias(line.Name, SemanticCategory.DDName, filePath, line.LineNumber);
        }

        if (string.IsNullOrEmpty(line.Operands))
            return;

        // DSN, PGM, PARM, symbols in operands
        DiscoverOperandIdentifiers(line.Operands, context, filePath, line.LineNumber);
    }

    private static void DiscoverSetStatement(JclLine line, ObfuscationContext context, string? filePath)
    {
        if (string.IsNullOrEmpty(line.Operands))
            return;

        // SET SYMBOL=value or SET &SYMBOL=value
        var operands = line.Operands;
        var assignments = operands.Split(',', StringSplitOptions.RemoveEmptyEntries);
        foreach (var assignment in assignments)
        {
            var eqIndex = assignment.IndexOf('=');
            if (eqIndex > 0)
            {
                var symbolName = assignment[..eqIndex].Trim().TrimStart('&');
                if (!string.IsNullOrWhiteSpace(symbolName) && !IsSystemSymbol("&" + symbolName))
                {
                    context.GetOrCreateAlias("&" + symbolName, SemanticCategory.Symbol, filePath, line.LineNumber);
                }
            }
        }
    }

    private static void DiscoverIncludeStatement(JclLine line, ObfuscationContext context, string? filePath)
    {
        if (string.IsNullOrEmpty(line.Operands))
            return;

        // INCLUDE MEMBER=membername
        var memberMatch = Regex.Match(line.Operands, @"MEMBER\s*=\s*(\S+)", RegexOptions.IgnoreCase);
        if (memberMatch.Success)
        {
            context.GetOrCreateAlias(memberMatch.Groups[1].Value, SemanticCategory.Include, filePath, line.LineNumber);
        }
    }

    /// <summary>
    /// Scans a raw operand string for DSN=, PGM=, PARM=, and &amp;SYMBOL references
    /// and registers them with the context.
    /// </summary>
    private static void DiscoverOperandIdentifiers(string operands, ObfuscationContext context, string? filePath, int lineNumber)
    {
        // DSN=
        foreach (Match m in DsnPattern.Matches(operands))
        {
            var dsn = m.Groups["dsn"].Value;
            if (!string.IsNullOrWhiteSpace(dsn))
            {
                // Check if DSN contains a member reference: DSN=name(member)
                var parenIdx = dsn.IndexOf('(');
                if (parenIdx > 0)
                {
                    var dsnBase = dsn[..parenIdx];
                    context.GetOrCreateAlias(dsnBase, SemanticCategory.Dataset, filePath, lineNumber);
                    var member = dsn[(parenIdx + 1)..].TrimEnd(')');
                    if (!string.IsNullOrWhiteSpace(member))
                    {
                        context.GetOrCreateAlias(member, SemanticCategory.Program, filePath, lineNumber);
                    }
                }
                else
                {
                    context.GetOrCreateAlias(dsn, SemanticCategory.Dataset, filePath, lineNumber);
                }
            }
        }

        // PGM=
        foreach (Match m in PgmPattern.Matches(operands))
        {
            var pgm = m.Groups["pgm"].Value;
            if (!string.IsNullOrWhiteSpace(pgm))
            {
                context.GetOrCreateAlias(pgm, SemanticCategory.Program, filePath, lineNumber);
            }
        }

        // PARM=
        foreach (Match m in ParmPattern.Matches(operands))
        {
            var parmVal = m.Groups["parmq"].Success ? m.Groups["parmq"].Value : m.Groups["parm"].Value;
            if (!string.IsNullOrWhiteSpace(parmVal))
            {
                context.GetOrCreateAlias(parmVal, SemanticCategory.Parameter, filePath, lineNumber);
            }
        }

        // Symbolic parameters: &SYMBOL
        foreach (Match m in SymbolPattern.Matches(operands))
        {
            var fullSymbol = "&" + m.Groups["sym"].Value;
            if (!IsSystemSymbol(fullSymbol))
            {
                context.GetOrCreateAlias(fullSymbol, SemanticCategory.Symbol, filePath, lineNumber);
            }
        }
    }

    #endregion

    #region Pass 2 - Obfuscation

    /// <summary>
    /// Transforms a single parsed JCL line by replacing user identifiers with their aliases,
    /// preserving all JCL keywords, system DD names, and column structure.
    /// </summary>
    private string ObfuscateLine(JclLine line, ObfuscationContext context, string? filePath, ref int replacementCount, List<string> warnings)
    {
        switch (line.LineType)
        {
            case JclLineType.Comment:
                replacementCount++;
                return "//* [Comment removed]";

            case JclLineType.Instream:
                return ObfuscateInstreamLine(line, context, ref replacementCount);

            case JclLineType.Delimiter:
            case JclLineType.Null:
                return line.OriginalText;

            case JclLineType.Job:
                return ObfuscateJobCard(line, context, ref replacementCount, warnings);

            case JclLineType.Exec:
                return ObfuscateExecCard(line, context, ref replacementCount, warnings);

            case JclLineType.Dd:
                return ObfuscateDdCard(line, context, ref replacementCount, warnings);

            case JclLineType.Proc:
                return ObfuscateProcCard(line, context, ref replacementCount);

            case JclLineType.Set:
                return ObfuscateSetCard(line, context, ref replacementCount);

            case JclLineType.Include:
                return ObfuscateIncludeCard(line, context, ref replacementCount);

            case JclLineType.Continuation:
                return ObfuscateContinuation(line, context, ref replacementCount);

            case JclLineType.If:
            case JclLineType.Endif:
            case JclLineType.Pend:
            case JclLineType.Jcllib:
                // Preserve these structural statements; only replace symbols in operands
                if (!string.IsNullOrEmpty(line.Operands))
                {
                    var newOperands = ReplaceSymbols(line.Operands, context, ref replacementCount);
                    return RebuildLine(line.Name, line.Operation, newOperands);
                }
                return line.OriginalText;

            default:
                return line.OriginalText;
        }
    }

    /// <summary>
    /// Concatenates a block of consecutive instream data lines that belong to a SQL program,
    /// delegates the combined text to the SQL processor for obfuscation, and returns the
    /// result split back into lines joined by newlines.
    /// Falls back to returning lines unchanged if the SQL processor is unavailable or fails.
    /// </summary>
    // ── DB2 DSN command patterns for non-SQL instream data ──
    private static readonly Regex DsnProgramPlanPattern = new(
        @"(?:PROGRAM|PLAN)\s*\(\s*(\w+)\s*\)",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private static readonly Regex DsnSystemPattern = new(
        @"(?:SYSTEM|S)\s*\(\s*(\w+)\s*\)",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private static readonly Regex DsnLibraryPattern = new(
        @"(?:LIBRARY|LIB)\s*\(\s*'([^']+)'\s*\)",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    /// <summary>
    /// Obfuscate a single non-SQL instream line. Handles DB2 DSN commands
    /// (PROGRAM, PLAN, SYSTEM, LIBRARY) that appear in TSO batch instream data.
    /// Continuation lines (e.g., "PLAN(xxx) -") are also handled since patterns
    /// can appear on any line in a multi-line DSN command.
    /// </summary>
    private static string ObfuscateInstreamLine(JclLine line, ObfuscationContext context, ref int replacementCount)
    {
        var result = line.OriginalText;
        var localCount = 0;

        result = DsnProgramPlanPattern.Replace(result, m =>
        {
            var name = m.Groups[1].Value;
            var alias = context.GetOrCreateAlias(name, SemanticCategory.Program, null);
            localCount++;
            return m.Value.Replace(name, alias);
        });

        result = DsnSystemPattern.Replace(result, m =>
        {
            var name = m.Groups[1].Value;
            var alias = context.GetOrCreateAlias(name, SemanticCategory.Db2System, null);
            localCount++;
            return m.Value.Replace(name, alias);
        });

        result = DsnLibraryPattern.Replace(result, m =>
        {
            var dsn = m.Groups[1].Value;
            var alias = context.GetOrCreateAlias(dsn, SemanticCategory.MvsDataset, null);
            localCount++;
            return m.Value.Replace(dsn, alias);
        });

        replacementCount += localCount;
        return result;
    }

    private static string ObfuscateInstreamSqlBlock(
        List<JclLine> instreamLines,
        ObfuscationContext context,
        ref int replacementCount,
        List<string> warnings)
    {
        // Build the concatenated SQL text from the instream lines
        var sqlBuilder = new StringBuilder();
        for (int j = 0; j < instreamLines.Count; j++)
        {
            if (j > 0) sqlBuilder.Append('\n');
            sqlBuilder.Append(instreamLines[j].OriginalText);
        }
        var result = sqlBuilder.ToString();

        // Try SQL delegation first
        try
        {
            var sqlProcessor = context.ProcessorRegistry?.GetProcessor("temp.sql", "");
            if (sqlProcessor != null)
            {
                var sqlResult = sqlProcessor.Obfuscate(result, context);
                if (sqlResult.WasTransformed)
                {
                    replacementCount += sqlResult.ReplacementCount;
                    result = sqlResult.Content;
                }
            }
        }
        catch (Exception ex)
        {
            warnings.Add($"SQL delegation failed for instream block at line {instreamLines[0].LineNumber}: {ex.Message}");
        }

        // Always apply DSN command patterns (PROGRAM, PLAN, SYSTEM, LIBRARY)
        // as post-processing. Instream data for IKJEFT01 often contains DB2 DSN
        // commands that the SQL processor doesn't handle.
        var dsnCount = 0;

        result = DsnProgramPlanPattern.Replace(result, m =>
        {
            var name = m.Groups[1].Value;
            var alias = context.GetOrCreateAlias(name, SemanticCategory.Program, null);
            dsnCount++;
            return m.Value.Replace(name, alias);
        });

        result = DsnSystemPattern.Replace(result, m =>
        {
            var name = m.Groups[1].Value;
            var alias = context.GetOrCreateAlias(name, SemanticCategory.Db2System, null);
            dsnCount++;
            return m.Value.Replace(name, alias);
        });

        result = DsnLibraryPattern.Replace(result, m =>
        {
            var dsn = m.Groups[1].Value;
            var alias = context.GetOrCreateAlias(dsn, SemanticCategory.MvsDataset, null);
            dsnCount++;
            return m.Value.Replace(dsn, alias);
        });

        replacementCount += dsnCount;
        return result;
    }

    private string ObfuscateJobCard(JclLine line, ObfuscationContext context, ref int replacementCount, List<string> warnings)
    {
        string? newName = null;
        if (!string.IsNullOrEmpty(line.Name))
        {
            newName = context.Mappings.Forward.GetValueOrDefault(line.Name) ?? line.Name;
            if (newName != line.Name) replacementCount++;
        }

        if (string.IsNullOrEmpty(line.Operands))
        {
            return RebuildLine(newName, "JOB", null);
        }

        var operands = line.Operands;
        var parts = SplitTopLevelOperands(operands);
        var newParts = new List<string>();

        for (int i = 0; i < parts.Count; i++)
        {
            var part = parts[i];

            if (i == 0 && !part.Contains('='))
            {
                // Accounting info (first positional parameter)
                var acctInfo = part.Trim().Trim('\'', '(', ')');
                if (!string.IsNullOrWhiteSpace(acctInfo))
                {
                    var alias = context.Mappings.Forward.GetValueOrDefault(acctInfo);
                    if (alias != null)
                    {
                        // Preserve surrounding quotes/parens
                        var replacement = part.Replace(acctInfo, alias);
                        newParts.Add(replacement);
                        replacementCount++;
                        continue;
                    }
                }
                newParts.Add(part);
            }
            else if (i == 1 && !part.Contains('='))
            {
                // Programmer name (second positional parameter)
                var progName = part.Trim().Trim('\'');
                if (!string.IsNullOrWhiteSpace(progName))
                {
                    var alias = context.Mappings.Forward.GetValueOrDefault(progName);
                    if (alias != null)
                    {
                        var replacement = part.Contains('\'')
                            ? $"'{alias}'"
                            : alias;
                        newParts.Add(replacement);
                        replacementCount++;
                        continue;
                    }
                }
                newParts.Add(part);
            }
            else
            {
                // Keyword parameter - apply operand-level replacements
                var replaced = ReplaceOperandValues(part, context, ref replacementCount);
                newParts.Add(replaced);
            }
        }

        var newOperands = string.Join(",", newParts);
        return RebuildLine(newName, "JOB", newOperands);
    }

    private string ObfuscateExecCard(JclLine line, ObfuscationContext context, ref int replacementCount, List<string> warnings)
    {
        string? newName = null;
        if (!string.IsNullOrEmpty(line.Name))
        {
            newName = context.Mappings.Forward.GetValueOrDefault(line.Name) ?? line.Name;
            if (newName != line.Name) replacementCount++;
        }

        if (string.IsNullOrEmpty(line.Operands))
        {
            return RebuildLine(newName, "EXEC", null);
        }

        var operands = line.Operands;
        var newOperands = operands;

        // Replace PGM=name
        var pgmCount = 0;
        newOperands = PgmPattern.Replace(newOperands, m =>
        {
            var pgmName = m.Groups["pgm"].Value;
            var alias = context.Mappings.Forward.GetValueOrDefault(pgmName);
            if (alias != null)
            {
                pgmCount++;
                return $"PGM={alias}";
            }
            return m.Value;
        });
        replacementCount += pgmCount;

        // If no PGM=, first token is a proc name
        if (!PgmPattern.IsMatch(operands))
        {
            var firstToken = operands.Split([' ', ','], StringSplitOptions.RemoveEmptyEntries).FirstOrDefault();
            if (firstToken != null && !firstToken.Contains('=') && !JclKeywords.Contains(firstToken))
            {
                var alias = context.Mappings.Forward.GetValueOrDefault(firstToken);
                if (alias != null)
                {
                    // Replace first occurrence of the proc name in operands
                    var idx = newOperands.IndexOf(firstToken, StringComparison.Ordinal);
                    if (idx >= 0)
                    {
                        newOperands = string.Concat(newOperands.AsSpan(0, idx), alias, newOperands.AsSpan(idx + firstToken.Length));
                        replacementCount++;
                    }
                }
            }
        }

        // Replace PARM= and symbols
        newOperands = ReplaceOperandValues(newOperands, context, ref replacementCount);

        return RebuildLine(newName, "EXEC", newOperands);
    }

    private string ObfuscateDdCard(JclLine line, ObfuscationContext context, ref int replacementCount, List<string> warnings)
    {
        string? newName = null;
        if (!string.IsNullOrEmpty(line.Name))
        {
            if (IsSystemDdName(line.Name))
            {
                newName = line.Name; // Preserve system DD names
            }
            else
            {
                newName = context.Mappings.Forward.GetValueOrDefault(line.Name) ?? line.Name;
                if (newName != line.Name) replacementCount++;
            }
        }

        if (string.IsNullOrEmpty(line.Operands))
        {
            return RebuildLine(newName, "DD", null);
        }

        var newOperands = ReplaceOperandValues(line.Operands, context, ref replacementCount);
        return RebuildLine(newName, "DD", newOperands);
    }

    private static string ObfuscateProcCard(JclLine line, ObfuscationContext context, ref int replacementCount)
    {
        string? newName = null;
        if (!string.IsNullOrEmpty(line.Name))
        {
            newName = context.Mappings.Forward.GetValueOrDefault(line.Name) ?? line.Name;
            if (newName != line.Name) replacementCount++;
        }

        var newOperands = line.Operands;
        if (!string.IsNullOrEmpty(newOperands))
        {
            newOperands = ReplaceSymbols(newOperands, context, ref replacementCount);
        }

        return RebuildLine(newName, "PROC", newOperands);
    }

    private static string ObfuscateSetCard(JclLine line, ObfuscationContext context, ref int replacementCount)
    {
        if (string.IsNullOrEmpty(line.Operands))
        {
            return RebuildLine(line.Name, "SET", null);
        }

        var newOperands = line.Operands;

        // SET SYMBOL=value or SET &SYMBOL=value
        var assignments = newOperands.Split(',');
        var newAssignments = new List<string>();

        foreach (var assignment in assignments)
        {
            var eqIndex = assignment.IndexOf('=');
            if (eqIndex > 0)
            {
                var symbolPart = assignment[..eqIndex].Trim();
                var valuePart = assignment[(eqIndex + 1)..];

                // Replace the symbol name
                var fullSymbol = symbolPart.StartsWith('&') ? symbolPart : "&" + symbolPart;
                if (!IsSystemSymbol(fullSymbol))
                {
                    var alias = context.Mappings.Forward.GetValueOrDefault(fullSymbol);
                    if (alias != null)
                    {
                        // The alias for a symbol includes the & prefix in the mapping key
                        // but the alias itself is like SYM_0; we need to output it properly
                        symbolPart = alias;
                        replacementCount++;
                    }
                }

                newAssignments.Add($"{symbolPart}={valuePart}");
            }
            else
            {
                newAssignments.Add(assignment);
            }
        }

        return RebuildLine(line.Name, "SET", string.Join(",", newAssignments));
    }

    private static string ObfuscateIncludeCard(JclLine line, ObfuscationContext context, ref int replacementCount)
    {
        if (string.IsNullOrEmpty(line.Operands))
        {
            return RebuildLine(line.Name, "INCLUDE", null);
        }

        var count = 0;
        var newOperands = Regex.Replace(line.Operands, @"MEMBER\s*=\s*(\S+)", m =>
        {
            var memberName = m.Groups[1].Value;
            var alias = context.Mappings.Forward.GetValueOrDefault(memberName);
            if (alias != null)
            {
                count++;
                return $"MEMBER={alias}";
            }
            return m.Value;
        }, RegexOptions.IgnoreCase);
        replacementCount += count;

        return RebuildLine(line.Name, "INCLUDE", newOperands);
    }

    private static string ObfuscateContinuation(JclLine line, ObfuscationContext context, ref int replacementCount)
    {
        if (string.IsNullOrEmpty(line.Operands))
            return line.OriginalText;

        var newOperands = ReplaceOperandValues(line.Operands, context, ref replacementCount);

        // Rebuild continuation line: //             operands (continuation at column 16)
        var prefix = "//";
        // Determine original indentation
        var originalBody = line.OriginalText.Length > 2 ? line.OriginalText[2..] : "";
        var leadingSpaces = originalBody.Length - originalBody.TrimStart().Length;
        var indent = new string(' ', Math.Max(leadingSpaces, 1));

        var rebuilt = $"{prefix}{indent}{newOperands}";
        return TruncateToJclLength(rebuilt);
    }

    #endregion

    #region Operand-Level Replacements

    /// <summary>
    /// Applies DSN=, PGM=, PARM=, and symbolic parameter replacements within an operand string.
    /// </summary>
    private static string ReplaceOperandValues(string operands, ObfuscationContext context, ref int replacementCount)
    {
        var result = operands;
        var count = 0;

        // Replace DSN= values
        result = DsnPattern.Replace(result, m =>
        {
            var dsn = m.Groups["dsn"].Value;
            var prefix = m.Value[..m.Value.IndexOf('=')]; // DSN or DSNAME
            var parenIdx = dsn.IndexOf('(');

            if (parenIdx > 0)
            {
                var dsnBase = dsn[..parenIdx];
                var member = dsn[(parenIdx + 1)..].TrimEnd(')');

                var dsnAlias = context.Mappings.Forward.GetValueOrDefault(dsnBase);
                var memberAlias = context.Mappings.Forward.GetValueOrDefault(member);

                bool changed = false;
                var newDsn = dsnAlias ?? dsnBase;
                if (dsnAlias != null) { changed = true; count++; }

                var newMember = memberAlias ?? member;
                if (memberAlias != null) { changed = true; count++; }

                return changed ? $"{prefix}={newDsn}({newMember})" : m.Value;
            }
            else
            {
                var alias = context.Mappings.Forward.GetValueOrDefault(dsn);
                if (alias != null)
                {
                    count++;
                    return $"{prefix}={alias}";
                }
                return m.Value;
            }
        });

        // Replace PGM= values
        result = PgmPattern.Replace(result, m =>
        {
            var pgmName = m.Groups["pgm"].Value;
            var alias = context.Mappings.Forward.GetValueOrDefault(pgmName);
            if (alias != null)
            {
                count++;
                return $"PGM={alias}";
            }
            return m.Value;
        });

        // Replace PARM= values
        result = ParmPattern.Replace(result, m =>
        {
            var isQuoted = m.Groups["parmq"].Success;
            var parmVal = isQuoted ? m.Groups["parmq"].Value : m.Groups["parm"].Value;
            var alias = context.Mappings.Forward.GetValueOrDefault(parmVal);
            if (alias != null)
            {
                count++;
                return isQuoted ? $"PARM='{alias}'" : $"PARM={alias}";
            }
            return m.Value;
        });

        replacementCount += count;

        // Replace symbolic parameters
        result = ReplaceSymbols(result, context, ref replacementCount);

        return result;
    }

    /// <summary>
    /// Replaces <c>&amp;SYMBOL</c> references with their aliases, preserving system symbols.
    /// </summary>
    private static string ReplaceSymbols(string text, ObfuscationContext context, ref int replacementCount)
    {
        var count = 0;
        var result = SymbolPattern.Replace(text, m =>
        {
            var fullSymbol = "&" + m.Groups["sym"].Value;
            if (IsSystemSymbol(fullSymbol))
                return m.Value;

            var alias = context.Mappings.Forward.GetValueOrDefault(fullSymbol);
            if (alias != null)
            {
                count++;
                // Preserve the trailing dot if present
                var trailingDot = m.Value.EndsWith('.') ? "." : "";
                return $"{alias}{trailingDot}";
            }
            return m.Value;
        });
        replacementCount += count;
        return result;
    }

    #endregion

    #region Deobfuscation

    /// <summary>
    /// Restores a single line by scanning for alias patterns and replacing each with
    /// the original value from the reverse mapping table.
    /// </summary>
    private static string DeobfuscateLine(string line, ObfuscationContext context, ref int replacementCount)
    {
        if (string.IsNullOrEmpty(line))
            return line;

        // Comment lines that were blanked - leave as-is (original comments are lost)
        if (line == "//* [Comment removed]")
            return line;

        // Scan for alias patterns and replace with originals
        var count = 0;
        var result = AliasPattern.Replace(line, m =>
        {
            var alias = m.Value;
            var original = context.Mappings.GetOriginal(alias);
            if (original != null)
            {
                count++;
                return original;
            }
            return alias;
        });
        replacementCount += count;

        return result;
    }

    #endregion

    #region Line Rebuilding

    /// <summary>
    /// Reconstructs a JCL statement line from its parsed components, respecting
    /// the standard <c>//NAME    OP  OPERANDS</c> column layout and the 80-character limit.
    /// </summary>
    private static string RebuildLine(string? name, string? operation, string? operands)
    {
        var builder = new StringBuilder(MaxJclLineLength);
        builder.Append("//");

        if (!string.IsNullOrEmpty(name))
        {
            builder.Append(name);
        }

        if (!string.IsNullOrEmpty(operation))
        {
            // Pad to ensure operation starts after the name with at least one space
            if (builder.Length < 12)
            {
                builder.Append(' ', 12 - builder.Length);
            }
            else
            {
                builder.Append(' ');
            }
            builder.Append(operation);
        }

        if (!string.IsNullOrEmpty(operands))
        {
            builder.Append(' ');
            builder.Append(operands);
        }

        return TruncateToJclLength(builder.ToString());
    }

    /// <summary>
    /// Ensures a rebuilt line does not exceed the 80-character JCL line limit.
    /// </summary>
    private static string TruncateToJclLength(string line)
    {
        if (line.Length <= MaxJclLineLength)
            return line;

        // Truncate to 80 characters - JCL ignores columns 73-80 (sequence numbers)
        // but we truncate conservatively to avoid data loss in operands
        return line[..MaxJclLineLength];
    }

    #endregion

    #region Helper Methods

    /// <summary>
    /// Determines whether a DD name is a system-reserved name that must not be renamed.
    /// Handles both exact matches and the SORTWKnn pattern.
    /// </summary>
    private static bool IsSystemDdName(string name)
    {
        if (SystemDdNames.Contains(name))
            return true;

        // SORTWK01 through SORTWK99
        if (SortWorkDdPattern.IsMatch(name))
            return true;

        return false;
    }

    /// <summary>
    /// Determines whether a symbolic parameter is a z/OS system symbol that must be preserved.
    /// </summary>
    private static bool IsSystemSymbol(string symbol)
    {
        return SystemSymbols.Contains(symbol);
    }

    /// <summary>
    /// Splits a JCL operand string at top-level commas, respecting parenthesized groups
    /// and quoted strings so that commas inside <c>(a,b)</c> or <c>'a,b'</c> are not treated
    /// as delimiters.
    /// </summary>
    private static List<string> SplitTopLevelOperands(string operands)
    {
        var parts = new List<string>();
        int depth = 0;
        bool inQuote = false;
        int start = 0;

        for (int i = 0; i < operands.Length; i++)
        {
            char c = operands[i];

            if (c == '\'' && !inQuote)
            {
                inQuote = true;
            }
            else if (c == '\'' && inQuote)
            {
                // Check for escaped quote ''
                if (i + 1 < operands.Length && operands[i + 1] == '\'')
                {
                    i++; // skip escaped quote
                }
                else
                {
                    inQuote = false;
                }
            }
            else if (!inQuote)
            {
                if (c == '(') depth++;
                else if (c == ')') depth = Math.Max(0, depth - 1);
                else if (c == ',' && depth == 0)
                {
                    parts.Add(operands[start..i]);
                    start = i + 1;
                }
            }
        }

        if (start < operands.Length)
        {
            parts.Add(operands[start..]);
        }

        // Trim off any trailing comments (space-separated text after operands)
        // In JCL, comments on a statement line follow operands separated by space
        // We preserve this splitting at the top-level for further processing
        return parts;
    }

    #endregion
}
