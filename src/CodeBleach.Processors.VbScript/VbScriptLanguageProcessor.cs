using System.Collections.Frozen;
using System.Text;
using System.Text.RegularExpressions;
using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;

namespace CodeBleach.Processors.VbScript;

// ═══════════════════════════════════════════════════════════════════════════════
// Token model
// ═══════════════════════════════════════════════════════════════════════════════

/// <summary>
/// The kind of token produced by the VBScript/VBA tokenizer.
/// </summary>
internal enum VbTokenKind
{
    Keyword,
    Identifier,
    StringLiteral,
    NumericLiteral,
    Comment,
    Operator,
    Punctuation,
    Whitespace,
    NewLine,
    LineContinuation,
    EndOfFile,
    Unknown
}

/// <summary>
/// A single token extracted from VBScript/VBA source, with its exact character
/// position and length for position-based replacement.
/// </summary>
internal readonly record struct VbToken(VbTokenKind Kind, string Text, int Offset, int Length)
{
    /// <summary>Exclusive end position in the source.</summary>
    public int End => Offset + Length;
}

// ═══════════════════════════════════════════════════════════════════════════════
// Replacement model
// ═══════════════════════════════════════════════════════════════════════════════

/// <summary>
/// A deferred text replacement at a known character position.
/// Applied back-to-front to keep earlier offsets stable.
/// </summary>
internal sealed record VbReplacement(int Offset, int Length, string NewValue);

// ═══════════════════════════════════════════════════════════════════════════════
// Dialect flag
// ═══════════════════════════════════════════════════════════════════════════════

/// <summary>
/// Distinguishes VBScript (.vbs) from VBA (.bas, .cls, .frm).
/// VBA has additional constructs: Type, Enum, Implements, Office built-ins.
/// </summary>
internal enum VbDialect
{
    VbScript,
    Vba
}

// ═══════════════════════════════════════════════════════════════════════════════
// Declaration tracking
// ═══════════════════════════════════════════════════════════════════════════════

/// <summary>
/// Represents a user-defined name discovered during the declaration-scanning pass.
/// </summary>
internal sealed record DeclaredSymbol(string Name, SemanticCategory Category);

// ═══════════════════════════════════════════════════════════════════════════════
// VbScriptLanguageProcessor
// ═══════════════════════════════════════════════════════════════════════════════

/// <summary>
/// Language processor for VBScript (.vbs) and VBA (.bas, .cls, .frm) files.
/// Uses a two-pass custom tokenizer/parser approach:
///   Pass 1 - Discovery: regex-based line-by-line scan to find all declarations.
///   Pass 2 - Replacement: tokenize entire content, match declared identifiers,
///            replace back-to-front using position offsets.
/// </summary>
public sealed class VbScriptLanguageProcessor : ILanguageProcessor
{
    // ── ILanguageProcessor metadata ──────────────────────────────────────

    public string ProcessorId => "vbscript";
    public string DisplayName => "VBScript/VBA";

    public IReadOnlySet<string> SupportedExtensions { get; } =
        new HashSet<string>(StringComparer.OrdinalIgnoreCase) { ".vbs", ".bas", ".cls", ".frm" }
            .ToFrozenSet();

    public int Priority => 10;

    // ── Alias pattern for deobfuscation ──────────────────────────────────

    private static readonly Regex AliasPattern = new(@"^[A-Z]+_\d+$", RegexOptions.Compiled);

    // ══════════════════════════════════════════════════════════════════════
    // Keyword / built-in sets (case-insensitive)
    // ══════════════════════════════════════════════════════════════════════

    #region Keyword and built-in sets

    /// <summary>
    /// VBScript/VBA language keywords that must never be renamed.
    /// </summary>
    private static readonly FrozenSet<string> Keywords = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "And", "As", "Boolean", "ByRef", "Byte", "ByVal", "Call", "Case", "Class", "Const",
        "Currency", "Date", "Dim", "Do", "Double", "Each", "Else", "ElseIf", "Empty", "End",
        "Enum", "Eqv", "Error", "Event", "Exit", "False", "For", "Function", "Get", "GoTo",
        "If", "Imp", "Implements", "In", "Integer", "Is", "Let", "Like", "Long", "Loop", "Me",
        "Mod", "New", "Next", "Not", "Nothing", "Null", "On", "Option", "Optional", "Or",
        "ParamArray", "Preserve", "Private", "Property", "Public", "RaiseEvent", "ReDim",
        "Rem", "Resume", "Select", "Set", "Single", "Static", "Step", "Stop", "String", "Sub",
        "Then", "To", "True", "Type", "Until", "Variant", "Wend", "While", "With", "WithEvents",
        "Xor",
        // Additional structural keywords
        "Attribute", "Begin", "Declare", "DefBool", "DefByte", "DefCur", "DefDate", "DefDbl",
        "DefDec", "DefInt", "DefLng", "DefObj", "DefSng", "DefStr", "DefVar", "Friend",
        "Global", "GoSub", "LSet", "RSet", "Return", "TypeOf", "Wend"
    }.ToFrozenSet();

    /// <summary>
    /// VBScript built-in objects and functions.
    /// </summary>
    private static readonly FrozenSet<string> VbScriptBuiltIns = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "WScript", "CreateObject", "GetObject", "MsgBox", "InputBox",
        "CStr", "CInt", "CLng", "CDbl", "CSng", "CBool", "CDate", "CByte",
        "Err", "Now", "Date", "Time", "Timer",
        "Left", "Right", "Mid", "Len", "LCase", "UCase", "Trim", "LTrim", "RTrim",
        "InStr", "InStrRev", "Replace", "Split", "Join", "StrComp", "Space", "String",
        "Abs", "Int", "Fix", "Sgn", "Sqr", "Rnd", "Round", "Log", "Exp", "Hex", "Oct",
        "IsEmpty", "IsNull", "IsNumeric", "IsArray", "IsDate", "IsObject", "TypeName", "VarType",
        "Array", "UBound", "LBound", "Asc", "Chr", "AscW", "ChrW",
        "Year", "Month", "Day", "Hour", "Minute", "Second",
        "DateAdd", "DateDiff", "DatePart", "DateSerial",
        "FormatDateTime", "FormatNumber", "FormatPercent", "FormatCurrency",
        "RGB", "ScriptEngine", "ScriptEngineBuildVersion", "ScriptEngineMajorVersion",
        "ScriptEngineMinorVersion",
        // File system objects commonly used in scripts
        "FileSystemObject", "Dictionary", "RegExp",
        // Common constants
        "vbCrLf", "vbCr", "vbLf", "vbTab", "vbNewLine", "vbNullString", "vbNullChar",
        "vbObjectError", "vbTrue", "vbFalse"
    }.ToFrozenSet();

    /// <summary>
    /// VBA-specific built-ins from the Office object model.
    /// </summary>
    private static readonly FrozenSet<string> VbaBuiltIns = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "Application", "ThisWorkbook", "ActiveWorkbook", "ActiveSheet", "Worksheets", "Range",
        "Cells", "Rows", "Columns", "Selection", "ActiveCell", "Debug", "Print",
        "MsgBox", "InputBox", "DoEvents", "Shell", "Environ", "Dir", "Kill",
        "FileLen", "FreeFile", "Open", "Close", "Write", "Print", "Input", "Line", "EOF",
        "Format", "Val", "Str", "Asc", "Chr",
        // Additional Office model objects
        "ThisDocument", "ActiveDocument", "ActivePresentation", "ActiveExplorer",
        "Workbook", "Worksheet", "Sheet", "Sheets", "Charts", "Names",
        "CommandBars", "UserForm", "Controls", "MSForms",
        // File I/O
        "FreeFile", "LOF", "Seek",
        // Common VBA functions
        "Environ", "Choose", "Switch", "IIf", "Nz",
        "LenB", "LeftB", "RightB", "MidB", "AscB", "ChrB",
        "StrConv", "StrReverse", "InStrB",
        "DateValue", "TimeValue", "DateSerial", "TimeSerial",
        "CVErr", "Error", "IsError", "IsMissing",
        // Common constants
        "xlUp", "xlDown", "xlToLeft", "xlToRight", "xlWhole", "xlPart",
        "xlValues", "xlFormulas", "xlNone", "xlCenter", "xlLeft", "xlRight",
        "vbYesNo", "vbYes", "vbNo", "vbOK", "vbCancel", "vbAbort", "vbRetry", "vbIgnore",
        "vbCritical", "vbQuestion", "vbExclamation", "vbInformation",
        "vbString", "vbInteger", "vbLong", "vbSingle", "vbDouble", "vbCurrency",
        "vbDate", "vbBoolean", "vbVariant", "vbEmpty", "vbNull", "vbObject", "vbArray"
    }.ToFrozenSet();

    /// <summary>
    /// Combined reserved set used for both dialects (keywords always reserved,
    /// built-ins checked per dialect).
    /// </summary>
    private static bool IsReserved(string name, VbDialect dialect)
    {
        if (Keywords.Contains(name))
            return true;
        if (VbScriptBuiltIns.Contains(name))
            return true;
        if (dialect == VbDialect.Vba && VbaBuiltIns.Contains(name))
            return true;
        return false;
    }

    #endregion

    // ══════════════════════════════════════════════════════════════════════
    // Discovery patterns (Pass 1)
    // ══════════════════════════════════════════════════════════════════════

    #region Discovery regex patterns

    // Identifier character class for VBS/VBA
    private const string IdPat = @"[A-Za-z_]\w*";

    // Sub/Function declarations: captures name and optional param list
    private static readonly Regex SubFuncPattern = new(
        @"(?:^|\:)\s*(?:(?:Public|Private|Friend)\s+)?(?:Static\s+)?(?:Sub|Function)\s+(" + IdPat + @")\s*(?:\(([^)]*)\))?",
        RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Multiline);

    // Property declarations: Property Get|Let|Set Name(params)
    private static readonly Regex PropertyPattern = new(
        @"(?:^|\:)\s*(?:(?:Public|Private|Friend)\s+)?(?:Static\s+)?Property\s+(?:Get|Let|Set)\s+(" + IdPat + @")\s*(?:\(([^)]*)\))?",
        RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Multiline);

    // Class declaration (VBScript): Class ClassName
    private static readonly Regex ClassPattern = new(
        @"(?:^|\:)\s*(?:(?:Public|Private)\s+)?Class\s+(" + IdPat + @")",
        RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Multiline);

    // Variable declarations: Dim|Private|Public|Static name [, name2, ...]
    private static readonly Regex VarDeclPattern = new(
        @"(?:^|\:)\s*(?:Dim|Private|Public|Static|Global)\s+(.+?)(?=$|\:|')",
        RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Multiline);

    // Const declaration: [Public|Private] Const name = value
    private static readonly Regex ConstPattern = new(
        @"(?:^|\:)\s*(?:(?:Public|Private)\s+)?Const\s+(" + IdPat + @")",
        RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Multiline);

    // Type declaration (VBA): [Public|Private] Type TypeName
    private static readonly Regex TypeDeclPattern = new(
        @"(?:^|\:)\s*(?:(?:Public|Private)\s+)?Type\s+(" + IdPat + @")",
        RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Multiline);

    // Enum declaration (VBA): [Public|Private] Enum EnumName
    private static readonly Regex EnumDeclPattern = new(
        @"(?:^|\:)\s*(?:(?:Public|Private)\s+)?Enum\s+(" + IdPat + @")",
        RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Multiline);

    // ReDim: ReDim [Preserve] name(size)
    private static readonly Regex ReDimPattern = new(
        @"(?:^|\:)\s*ReDim\s+(?:Preserve\s+)?(" + IdPat + @")",
        RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Multiline);

    // For loop variable: For varName = ...
    private static readonly Regex ForPattern = new(
        @"(?:^|\:)\s*For\s+(" + IdPat + @")\s*=",
        RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Multiline);

    // For Each variable: For Each varName In ...
    private static readonly Regex ForEachPattern = new(
        @"(?:^|\:)\s*For\s+Each\s+(" + IdPat + @")\s+In\b",
        RegexOptions.Compiled | RegexOptions.IgnoreCase | RegexOptions.Multiline);

    // Single identifier from variable list (for parsing Dim a, b, c)
    private static readonly Regex SingleVarNamePattern = new(
        @"(" + IdPat + @")(?:\s*\([^)]*\))?(?:\s+As\s+" + IdPat + @")?",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    // Parameter pattern: [Optional] [ByRef|ByVal] name [As Type] [= default]
    private static readonly Regex ParamNamePattern = new(
        @"(?:Optional\s+)?(?:ByRef\s+|ByVal\s+)?(?:ParamArray\s+)?(" + IdPat + @")(?:\s*\(\s*\))?(?:\s+As\s+" + IdPat + @")?",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    #endregion

    // ══════════════════════════════════════════════════════════════════════
    // CanProcess
    // ══════════════════════════════════════════════════════════════════════

    public bool CanProcess(string filePath, string content)
    {
        var ext = Path.GetExtension(filePath);
        if (!SupportedExtensions.Contains(ext))
            return false;

        // For .bas files, verify it looks like VBA rather than some other format
        // (e.g., BASIC dialects, FreeBasic, etc.)
        if (string.Equals(ext, ".bas", StringComparison.OrdinalIgnoreCase))
        {
            return LooksLikeVba(content);
        }

        return true;
    }

    /// <summary>
    /// Heuristic check: does the content contain typical VBA constructs?
    /// </summary>
    private static bool LooksLikeVba(string content)
    {
        if (string.IsNullOrWhiteSpace(content))
            return false;

        // Look for typical VBA patterns
        var patterns = new[]
        {
            @"\bSub\s+\w+",
            @"\bFunction\s+\w+",
            @"\bDim\s+\w+",
            @"\bOption\s+(Explicit|Base|Compare|Private)",
            @"\bProperty\s+(Get|Let|Set)\s+\w+",
            @"\bPublic\s+(Sub|Function|Property|Const|Enum|Type)\b",
            @"\bPrivate\s+(Sub|Function|Property|Const|Enum|Type)\b",
            @"\bAttribute\s+VB_"
        };

        foreach (var pattern in patterns)
        {
            if (Regex.IsMatch(content, pattern, RegexOptions.IgnoreCase | RegexOptions.Multiline))
                return true;
        }

        return false;
    }

    // ══════════════════════════════════════════════════════════════════════
    // Dialect detection
    // ══════════════════════════════════════════════════════════════════════

    private static VbDialect DetectDialect(string? filePath)
    {
        if (filePath == null)
            return VbDialect.VbScript;

        var ext = Path.GetExtension(filePath);
        if (string.Equals(ext, ".vbs", StringComparison.OrdinalIgnoreCase))
            return VbDialect.VbScript;

        // .bas, .cls, .frm → VBA
        return VbDialect.Vba;
    }

    // ══════════════════════════════════════════════════════════════════════
    // Obfuscate
    // ══════════════════════════════════════════════════════════════════════

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
        var dialect = DetectDialect(filePath);

        try
        {
            // ── Pass 1: Discovery ────────────────────────────────────────
            // Normalize line continuations for discovery (join lines ending with _)
            var normalizedForDiscovery = NormalizeLineContinuations(content);
            var declaredNames = DiscoverDeclarations(normalizedForDiscovery, context, dialect, filePath, warnings);

            // ── Pass 2: Tokenize and replace ─────────────────────────────
            var tokens = Tokenize(content);
            var replacements = new List<VbReplacement>();

            foreach (var token in tokens)
            {
                switch (token.Kind)
                {
                    case VbTokenKind.Identifier:
                        if (declaredNames.TryGetValue(token.Text.ToUpperInvariant(), out var symbol))
                        {
                            var alias = context.GetOrCreateAlias(
                                symbol.Name, symbol.Category, filePath,
                                GetLineNumber(content, token.Offset),
                                token.Offset, token.End);

                            if (!string.Equals(alias, token.Text, StringComparison.Ordinal))
                            {
                                replacements.Add(new VbReplacement(token.Offset, token.Length, alias));
                            }
                        }
                        break;

                    case VbTokenKind.Comment:
                        replacements.Add(new VbReplacement(token.Offset, token.Length, "' [Comment removed]"));
                        break;

                    case VbTokenKind.StringLiteral:
                        ProcessStringLiteral(token, context, filePath, content, replacements);
                        break;
                }
            }

            // Deduplicate and apply back-to-front
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
            var replacementCount = sorted.Count;

            if (filePath != null)
            {
                context.RecordFileProcessing(filePath, ProcessorId, replacementCount);
            }

            return new LanguageProcessingResult
            {
                Content = finalContent,
                WasTransformed = replacementCount > 0,
                ReplacementCount = replacementCount,
                ProcessorId = ProcessorId,
                Warnings = warnings
            };
        }
        catch (Exception ex)
        {
            warnings.Add($"VBScript/VBA processor failed: {ex.Message}. Returning content unmodified.");
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

    // ══════════════════════════════════════════════════════════════════════
    // Deobfuscate
    // ══════════════════════════════════════════════════════════════════════

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

        try
        {
            var tokens = Tokenize(content);
            var replacements = new List<VbReplacement>();

            foreach (var token in tokens)
            {
                switch (token.Kind)
                {
                    case VbTokenKind.Identifier:
                        if (AliasPattern.IsMatch(token.Text) && reverse.TryGetValue(token.Text, out var original))
                        {
                            replacements.Add(new VbReplacement(token.Offset, token.Length, original));
                        }
                        break;

                    case VbTokenKind.StringLiteral:
                        // Try to reverse string literal aliases
                        var inner = ExtractStringLiteralInner(token.Text);
                        if (!string.IsNullOrEmpty(inner) && AliasPattern.IsMatch(inner) &&
                            reverse.TryGetValue(inner, out var origStr))
                        {
                            var newLiteral = "\"" + origStr.Replace("\"", "\"\"") + "\"";
                            replacements.Add(new VbReplacement(token.Offset, token.Length, newLiteral));
                        }
                        break;
                }
            }

            // Also do a fallback regex scan for aliases that the tokenizer might classify differently
            // (e.g., inside Rem comments or complex expressions)
            foreach (var kvp in reverse.OrderByDescending(k => k.Key.Length))
            {
                var alias = kvp.Key;
                if (!AliasPattern.IsMatch(alias))
                    continue;

                // Check for aliases not yet covered by token-based replacements
                var pattern = @"(?<![A-Za-z0-9_])" + Regex.Escape(alias) + @"(?![A-Za-z0-9_])";
                foreach (Match m in Regex.Matches(content, pattern))
                {
                    // Only add if not already covered
                    if (!replacements.Any(r => r.Offset == m.Index && r.Length == m.Length))
                    {
                        // Skip if this position is already in a replacement range
                        if (!replacements.Any(r => m.Index >= r.Offset && m.Index < r.Offset + r.Length))
                        {
                            replacements.Add(new VbReplacement(m.Index, m.Length, kvp.Value));
                        }
                    }
                }
            }

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
            warnings.Add($"VBScript/VBA deobfuscation failed: {ex.Message}. Attempting regex fallback.");
            return DeobfuscateByRegex(content, context, warnings);
        }
    }

    // ══════════════════════════════════════════════════════════════════════
    // Validate
    // ══════════════════════════════════════════════════════════════════════

    public ValidationResult Validate(string obfuscatedContent)
    {
        if (string.IsNullOrWhiteSpace(obfuscatedContent))
            return ValidationResult.Valid();

        var errors = new List<string>();

        try
        {
            // Structural check: matching block-end pairs.
            // Normalize line continuations so multi-line declarations don't confuse counting.
            var normalized = NormalizeLineContinuations(obfuscatedContent);
            var lines = normalized.Split('\n');

            var blockStack = new Stack<(string BlockType, int LineNumber)>();

            for (var i = 0; i < lines.Length; i++)
            {
                var line = lines[i].Trim();

                // Skip empty lines and comments
                if (string.IsNullOrWhiteSpace(line) || line.StartsWith("'", StringComparison.Ordinal))
                    continue;

                // Remove trailing comment
                var commentIdx = FindCommentStart(line);
                if (commentIdx >= 0)
                    line = line[..commentIdx].TrimEnd();

                // Handle multiple statements on one line
                var statements = SplitStatements(line);

                foreach (var stmt in statements)
                {
                    var trimmed = stmt.Trim();
                    if (string.IsNullOrEmpty(trimmed))
                        continue;

                    var upper = trimmed.ToUpperInvariant();

                    // Opening blocks
                    if (Regex.IsMatch(upper, @"^\s*(?:(?:PUBLIC|PRIVATE|FRIEND)\s+)?(?:STATIC\s+)?SUB\s+"))
                        blockStack.Push(("Sub", i + 1));
                    else if (Regex.IsMatch(upper, @"^\s*(?:(?:PUBLIC|PRIVATE|FRIEND)\s+)?(?:STATIC\s+)?FUNCTION\s+"))
                        blockStack.Push(("Function", i + 1));
                    else if (Regex.IsMatch(upper, @"^\s*(?:(?:PUBLIC|PRIVATE|FRIEND)\s+)?(?:STATIC\s+)?PROPERTY\s+"))
                        blockStack.Push(("Property", i + 1));
                    else if (Regex.IsMatch(upper, @"^\s*(?:(?:PUBLIC|PRIVATE)\s+)?CLASS\s+"))
                        blockStack.Push(("Class", i + 1));
                    else if (Regex.IsMatch(upper, @"^\s*(?:(?:PUBLIC|PRIVATE)\s+)?TYPE\s+"))
                        blockStack.Push(("Type", i + 1));
                    else if (Regex.IsMatch(upper, @"^\s*(?:(?:PUBLIC|PRIVATE)\s+)?ENUM\s+"))
                        blockStack.Push(("Enum", i + 1));
                    else if (Regex.IsMatch(upper, @"^\s*WITH\s+"))
                        blockStack.Push(("With", i + 1));
                    // Single-line If: If ... Then ... (with statement after Then on same line, no block)
                    else if (Regex.IsMatch(upper, @"^\s*IF\b") && upper.Contains("THEN") &&
                             !upper.TrimEnd().EndsWith("THEN"))
                    {
                        // Single-line If — no block pushed
                    }
                    else if (Regex.IsMatch(upper, @"^\s*IF\b") && upper.Contains("THEN"))
                        blockStack.Push(("If", i + 1));
                    else if (Regex.IsMatch(upper, @"^\s*SELECT\s+CASE\b"))
                        blockStack.Push(("Select", i + 1));
                    else if (Regex.IsMatch(upper, @"^\s*DO\b"))
                        blockStack.Push(("Do", i + 1));
                    else if (Regex.IsMatch(upper, @"^\s*FOR\b"))
                        blockStack.Push(("For", i + 1));
                    else if (Regex.IsMatch(upper, @"^\s*WHILE\b") && !upper.Contains("WEND"))
                        blockStack.Push(("While", i + 1));

                    // Closing blocks
                    else if (upper.StartsWith("END SUB", StringComparison.Ordinal))
                        PopExpecting(blockStack, "Sub", i + 1, errors);
                    else if (upper.StartsWith("END FUNCTION", StringComparison.Ordinal))
                        PopExpecting(blockStack, "Function", i + 1, errors);
                    else if (upper.StartsWith("END PROPERTY", StringComparison.Ordinal))
                        PopExpecting(blockStack, "Property", i + 1, errors);
                    else if (upper.StartsWith("END CLASS", StringComparison.Ordinal))
                        PopExpecting(blockStack, "Class", i + 1, errors);
                    else if (upper.StartsWith("END TYPE", StringComparison.Ordinal))
                        PopExpecting(blockStack, "Type", i + 1, errors);
                    else if (upper.StartsWith("END ENUM", StringComparison.Ordinal))
                        PopExpecting(blockStack, "Enum", i + 1, errors);
                    else if (upper.StartsWith("END WITH", StringComparison.Ordinal))
                        PopExpecting(blockStack, "With", i + 1, errors);
                    else if (upper.StartsWith("END IF", StringComparison.Ordinal))
                        PopExpecting(blockStack, "If", i + 1, errors);
                    else if (upper.StartsWith("END SELECT", StringComparison.Ordinal))
                        PopExpecting(blockStack, "Select", i + 1, errors);
                    else if (upper == "LOOP" || upper.StartsWith("LOOP ", StringComparison.Ordinal))
                        PopExpecting(blockStack, "Do", i + 1, errors);
                    else if (upper.StartsWith("NEXT", StringComparison.Ordinal))
                        PopExpecting(blockStack, "For", i + 1, errors);
                    else if (upper == "WEND")
                        PopExpecting(blockStack, "While", i + 1, errors);
                }
            }

            // Anything left on the stack is unclosed
            while (blockStack.Count > 0)
            {
                var (blockType, lineNum) = blockStack.Pop();
                errors.Add($"Unclosed '{blockType}' block starting at line {lineNum}");
            }

            return errors.Count == 0
                ? ValidationResult.Valid()
                : ValidationResult.Invalid(errors);
        }
        catch (Exception ex)
        {
            return ValidationResult.Invalid([$"Validation failed: {ex.Message}"]);
        }
    }

    // ══════════════════════════════════════════════════════════════════════
    // Pass 1: Discovery
    // ══════════════════════════════════════════════════════════════════════

    #region Discovery

    /// <summary>
    /// Scans the (line-continuation-normalized) source for all user-defined declarations.
    /// Returns a dictionary of declared names (case-insensitive key → canonical DeclaredSymbol).
    /// Each symbol is also registered in the ObfuscationContext for alias creation.
    /// </summary>
    private Dictionary<string, DeclaredSymbol> DiscoverDeclarations(
        string normalizedContent,
        ObfuscationContext context,
        VbDialect dialect,
        string? filePath,
        List<string> warnings)
    {
        var declared = new Dictionary<string, DeclaredSymbol>(StringComparer.OrdinalIgnoreCase);

        // Helper: register a name if it's not reserved
        void Register(string name, SemanticCategory category)
        {
            if (string.IsNullOrWhiteSpace(name) || IsReserved(name, dialect))
                return;
            // Store canonical casing from first declaration
            if (!declared.ContainsKey(name))
            {
                declared[name] = new DeclaredSymbol(name, category);
                context.GetOrCreateAlias(name, category, filePath);
            }
        }

        // Sub/Function declarations
        foreach (Match match in SubFuncPattern.Matches(normalizedContent))
        {
            var name = match.Groups[1].Value;
            Register(name, SemanticCategory.Method);

            // Parse parameters
            if (match.Groups[2].Success)
            {
                DiscoverParameters(match.Groups[2].Value, declared, context, dialect, filePath);
            }
        }

        // Property declarations
        foreach (Match match in PropertyPattern.Matches(normalizedContent))
        {
            var name = match.Groups[1].Value;
            Register(name, SemanticCategory.Property);

            if (match.Groups[2].Success)
            {
                DiscoverParameters(match.Groups[2].Value, declared, context, dialect, filePath);
            }
        }

        // Class declarations
        foreach (Match match in ClassPattern.Matches(normalizedContent))
        {
            Register(match.Groups[1].Value, SemanticCategory.Class);
        }

        // Variable declarations (Dim/Private/Public/Static/Global)
        foreach (Match match in VarDeclPattern.Matches(normalizedContent))
        {
            var varList = match.Groups[1].Value;
            DiscoverVariableList(varList, declared, context, dialect, filePath);
        }

        // Const declarations
        foreach (Match match in ConstPattern.Matches(normalizedContent))
        {
            Register(match.Groups[1].Value, SemanticCategory.Variable);
        }

        // ReDim declarations (may introduce new variables in VBScript)
        foreach (Match match in ReDimPattern.Matches(normalizedContent))
        {
            Register(match.Groups[1].Value, SemanticCategory.Variable);
        }

        // For loop variables
        foreach (Match match in ForPattern.Matches(normalizedContent))
        {
            Register(match.Groups[1].Value, SemanticCategory.Variable);
        }

        // For Each variables
        foreach (Match match in ForEachPattern.Matches(normalizedContent))
        {
            Register(match.Groups[1].Value, SemanticCategory.Variable);
        }

        // VBA-specific: Type declarations
        if (dialect == VbDialect.Vba)
        {
            foreach (Match match in TypeDeclPattern.Matches(normalizedContent))
            {
                Register(match.Groups[1].Value, SemanticCategory.UserDefinedType);
            }

            // Discover Type members (fields inside Type...End Type blocks)
            DiscoverTypeMembers(normalizedContent, declared, context, dialect, filePath);

            // Enum declarations
            foreach (Match match in EnumDeclPattern.Matches(normalizedContent))
            {
                Register(match.Groups[1].Value, SemanticCategory.Enum);
            }

            // Discover Enum members
            DiscoverEnumMembers(normalizedContent, declared, context, dialect, filePath);
        }

        return declared;
    }

    /// <summary>
    /// Parses a comma-separated parameter list and registers each parameter name.
    /// </summary>
    private static void DiscoverParameters(
        string paramList,
        Dictionary<string, DeclaredSymbol> declared,
        ObfuscationContext context,
        VbDialect dialect,
        string? filePath)
    {
        if (string.IsNullOrWhiteSpace(paramList))
            return;

        var parts = paramList.Split(',');
        foreach (var part in parts)
        {
            var trimmed = part.Trim();
            if (string.IsNullOrEmpty(trimmed))
                continue;

            var match = ParamNamePattern.Match(trimmed);
            if (match.Success)
            {
                var name = match.Groups[1].Value;
                if (!string.IsNullOrWhiteSpace(name) && !IsReserved(name, dialect))
                {
                    if (!declared.ContainsKey(name))
                    {
                        declared[name] = new DeclaredSymbol(name, SemanticCategory.Parameter);
                        context.GetOrCreateAlias(name, SemanticCategory.Parameter, filePath);
                    }
                }
            }
        }
    }

    /// <summary>
    /// Parses a comma-separated variable list from Dim/Private/Public/Static.
    /// Handles: name, name(), name As Type, name(size) As Type
    /// </summary>
    private static void DiscoverVariableList(
        string varList,
        Dictionary<string, DeclaredSymbol> declared,
        ObfuscationContext context,
        VbDialect dialect,
        string? filePath)
    {
        // Split on commas, but be careful of commas inside parentheses
        var parts = SplitOutsideParens(varList);

        foreach (var part in parts)
        {
            var trimmed = part.Trim();
            if (string.IsNullOrEmpty(trimmed))
                continue;

            var match = SingleVarNamePattern.Match(trimmed);
            if (match.Success)
            {
                var name = match.Groups[1].Value;
                if (!string.IsNullOrWhiteSpace(name) && !IsReserved(name, dialect))
                {
                    if (!declared.ContainsKey(name))
                    {
                        declared[name] = new DeclaredSymbol(name, SemanticCategory.Variable);
                        context.GetOrCreateAlias(name, SemanticCategory.Variable, filePath);
                    }
                }
            }
        }
    }

    /// <summary>
    /// Discovers fields declared inside Type...End Type blocks (VBA).
    /// </summary>
    private static void DiscoverTypeMembers(
        string content,
        Dictionary<string, DeclaredSymbol> declared,
        ObfuscationContext context,
        VbDialect dialect,
        string? filePath)
    {
        var typeBlockPattern = new Regex(
            @"(?:^|\n)\s*(?:(?:Public|Private)\s+)?Type\s+" + IdPat + @"\s*\n([\s\S]*?)(?:^|\n)\s*End\s+Type\b",
            RegexOptions.IgnoreCase | RegexOptions.Multiline);

        foreach (Match typeMatch in typeBlockPattern.Matches(content))
        {
            var body = typeMatch.Groups[1].Value;
            var memberLines = body.Split('\n');
            foreach (var memberLine in memberLines)
            {
                var trimmed = memberLine.Trim();
                if (string.IsNullOrEmpty(trimmed) || trimmed.StartsWith("'", StringComparison.Ordinal))
                    continue;

                // Type members: fieldName As DataType or fieldName(size) As DataType
                var memberMatch = Regex.Match(trimmed, @"^(" + IdPat + @")(?:\s*\([^)]*\))?\s+As\s+", RegexOptions.IgnoreCase);
                if (memberMatch.Success)
                {
                    var name = memberMatch.Groups[1].Value;
                    if (!IsReserved(name, dialect) && !declared.ContainsKey(name))
                    {
                        declared[name] = new DeclaredSymbol(name, SemanticCategory.Field);
                        context.GetOrCreateAlias(name, SemanticCategory.Field, filePath);
                    }
                }
            }
        }
    }

    /// <summary>
    /// Discovers member names inside Enum...End Enum blocks (VBA).
    /// </summary>
    private static void DiscoverEnumMembers(
        string content,
        Dictionary<string, DeclaredSymbol> declared,
        ObfuscationContext context,
        VbDialect dialect,
        string? filePath)
    {
        var enumBlockPattern = new Regex(
            @"(?:^|\n)\s*(?:(?:Public|Private)\s+)?Enum\s+" + IdPat + @"\s*\n([\s\S]*?)(?:^|\n)\s*End\s+Enum\b",
            RegexOptions.IgnoreCase | RegexOptions.Multiline);

        foreach (Match enumMatch in enumBlockPattern.Matches(content))
        {
            var body = enumMatch.Groups[1].Value;
            var memberLines = body.Split('\n');
            foreach (var memberLine in memberLines)
            {
                var trimmed = memberLine.Trim();
                if (string.IsNullOrEmpty(trimmed) || trimmed.StartsWith("'", StringComparison.Ordinal))
                    continue;

                // Enum members: memberName [= value]
                var memberMatch = Regex.Match(trimmed, @"^(" + IdPat + @")(?:\s*=\s*.*)?$", RegexOptions.IgnoreCase);
                if (memberMatch.Success)
                {
                    var name = memberMatch.Groups[1].Value;
                    if (!IsReserved(name, dialect) && !declared.ContainsKey(name))
                    {
                        declared[name] = new DeclaredSymbol(name, SemanticCategory.EnumMember);
                        context.GetOrCreateAlias(name, SemanticCategory.EnumMember, filePath);
                    }
                }
            }
        }
    }

    #endregion

    // ══════════════════════════════════════════════════════════════════════
    // Tokenizer
    // ══════════════════════════════════════════════════════════════════════

    #region Tokenizer

    /// <summary>
    /// Tokenizes VBScript/VBA source into a flat list of tokens with exact positions.
    /// Handles strings, comments, keywords, identifiers, numbers, operators, and punctuation.
    /// </summary>
    private static List<VbToken> Tokenize(string content)
    {
        var tokens = new List<VbToken>();
        var pos = 0;
        var length = content.Length;

        while (pos < length)
        {
            var ch = content[pos];

            // ── Newline ──────────────────────────────────────────────
            if (ch == '\r' || ch == '\n')
            {
                var start = pos;
                if (ch == '\r' && pos + 1 < length && content[pos + 1] == '\n')
                    pos += 2;
                else
                    pos++;
                tokens.Add(new VbToken(VbTokenKind.NewLine, content[start..pos], start, pos - start));
                continue;
            }

            // ── Whitespace ───────────────────────────────────────────
            if (char.IsWhiteSpace(ch))
            {
                var start = pos;
                while (pos < length && content[pos] != '\r' && content[pos] != '\n' && char.IsWhiteSpace(content[pos]))
                    pos++;
                tokens.Add(new VbToken(VbTokenKind.Whitespace, content[start..pos], start, pos - start));
                continue;
            }

            // ── Line continuation ( _ at end of line before newline) ──
            if (ch == '_' && IsLineContinuation(content, pos))
            {
                var start = pos;
                pos++; // skip _
                // Skip any whitespace/newline that follows
                while (pos < length && (content[pos] == ' ' || content[pos] == '\t'))
                    pos++;
                if (pos < length && content[pos] == '\r')
                    pos++;
                if (pos < length && content[pos] == '\n')
                    pos++;
                tokens.Add(new VbToken(VbTokenKind.LineContinuation, content[start..pos], start, pos - start));
                continue;
            }

            // ── Comment: ' (apostrophe) ──────────────────────────────
            if (ch == '\'')
            {
                var start = pos;
                while (pos < length && content[pos] != '\r' && content[pos] != '\n')
                    pos++;
                tokens.Add(new VbToken(VbTokenKind.Comment, content[start..pos], start, pos - start));
                continue;
            }

            // ── String literal: "..." ────────────────────────────────
            if (ch == '"')
            {
                var start = pos;
                pos++; // skip opening "
                while (pos < length)
                {
                    if (content[pos] == '"')
                    {
                        pos++;
                        // Doubled quote "" inside string = escaped quote, continue
                        if (pos < length && content[pos] == '"')
                        {
                            pos++;
                            continue;
                        }
                        break;
                    }
                    // VBScript strings cannot span lines
                    if (content[pos] == '\r' || content[pos] == '\n')
                        break;
                    pos++;
                }
                tokens.Add(new VbToken(VbTokenKind.StringLiteral, content[start..pos], start, pos - start));
                continue;
            }

            // ── Numeric literal ──────────────────────────────────────
            if (char.IsDigit(ch) || (ch == '.' && pos + 1 < length && char.IsDigit(content[pos + 1])))
            {
                var start = pos;
                // Check for &H (hex) or &O (octal)
                if (ch == '&' || (pos > 0 && content[pos - 1] == '&'))
                {
                    // handled below as operator
                }

                while (pos < length && (char.IsDigit(content[pos]) || content[pos] == '.'))
                    pos++;
                // Handle scientific notation
                if (pos < length && (content[pos] == 'e' || content[pos] == 'E'))
                {
                    pos++;
                    if (pos < length && (content[pos] == '+' || content[pos] == '-'))
                        pos++;
                    while (pos < length && char.IsDigit(content[pos]))
                        pos++;
                }
                // Type suffix: %, &, !, #, @
                if (pos < length && "%&!#@".Contains(content[pos]))
                    pos++;
                tokens.Add(new VbToken(VbTokenKind.NumericLiteral, content[start..pos], start, pos - start));
                continue;
            }

            // ── Hex/Octal literal: &H... or &O... ───────────────────
            if (ch == '&' && pos + 1 < length && (content[pos + 1] == 'H' || content[pos + 1] == 'h' ||
                                                    content[pos + 1] == 'O' || content[pos + 1] == 'o'))
            {
                var start = pos;
                pos += 2; // skip &H or &O
                while (pos < length && IsHexDigit(content[pos]))
                    pos++;
                // Optional & suffix
                if (pos < length && content[pos] == '&')
                    pos++;
                tokens.Add(new VbToken(VbTokenKind.NumericLiteral, content[start..pos], start, pos - start));
                continue;
            }

            // ── Identifier / Keyword / Rem comment ───────────────────
            if (char.IsLetter(ch) || ch == '_')
            {
                var start = pos;
                while (pos < length && (char.IsLetterOrDigit(content[pos]) || content[pos] == '_'))
                    pos++;

                var text = content[start..pos];

                // Check for Rem comment: "Rem " followed by rest of line
                if (string.Equals(text, "Rem", StringComparison.OrdinalIgnoreCase))
                {
                    // Rem must be followed by whitespace or end of line to be a comment
                    if (pos >= length || content[pos] == ' ' || content[pos] == '\t' ||
                        content[pos] == '\r' || content[pos] == '\n')
                    {
                        // Consume rest of line as comment
                        while (pos < length && content[pos] != '\r' && content[pos] != '\n')
                            pos++;
                        tokens.Add(new VbToken(VbTokenKind.Comment, content[start..pos], start, pos - start));
                        continue;
                    }
                }

                // Check if it's a keyword
                if (Keywords.Contains(text))
                {
                    tokens.Add(new VbToken(VbTokenKind.Keyword, text, start, text.Length));
                }
                else
                {
                    tokens.Add(new VbToken(VbTokenKind.Identifier, text, start, text.Length));
                }
                continue;
            }

            // ── Dot-prefixed member access (.Property) ───────────────
            // The dot itself is punctuation; the identifier after it is separate
            if (ch == '.')
            {
                tokens.Add(new VbToken(VbTokenKind.Punctuation, ".", pos, 1));
                pos++;
                continue;
            }

            // ── Colon (statement separator) ──────────────────────────
            if (ch == ':')
            {
                tokens.Add(new VbToken(VbTokenKind.Punctuation, ":", pos, 1));
                pos++;
                continue;
            }

            // ── Multi-character operators ────────────────────────────
            if (pos + 1 < length)
            {
                var two = content.Substring(pos, 2);
                if (two is "<>" or "<=" or ">=" or ":=")
                {
                    tokens.Add(new VbToken(VbTokenKind.Operator, two, pos, 2));
                    pos += 2;
                    continue;
                }
            }

            // ── Single-character operators / punctuation ─────────────
            if ("=<>+-*/\\^&(),;".Contains(ch))
            {
                var kind = "=<>+-*/\\^&".Contains(ch) ? VbTokenKind.Operator : VbTokenKind.Punctuation;
                tokens.Add(new VbToken(kind, ch.ToString(), pos, 1));
                pos++;
                continue;
            }

            // ── Unknown character ────────────────────────────────────
            tokens.Add(new VbToken(VbTokenKind.Unknown, ch.ToString(), pos, 1));
            pos++;
        }

        return tokens;
    }

    /// <summary>
    /// Determines whether the underscore at the given position is a line continuation character.
    /// A line continuation is _ preceded by whitespace (or start of token) and followed only by
    /// optional whitespace then a newline.
    /// </summary>
    private static bool IsLineContinuation(string content, int pos)
    {
        if (content[pos] != '_')
            return false;

        // Must not be part of an identifier (check char before)
        if (pos > 0 && (char.IsLetterOrDigit(content[pos - 1]) || content[pos - 1] == '_'))
            return false;

        // Must not be followed by an identifier char
        if (pos + 1 < content.Length && (char.IsLetterOrDigit(content[pos + 1]) || content[pos + 1] == '_'))
            return false;

        // Check that after optional whitespace comes a newline
        var ahead = pos + 1;
        while (ahead < content.Length && (content[ahead] == ' ' || content[ahead] == '\t'))
            ahead++;
        return ahead >= content.Length || content[ahead] == '\r' || content[ahead] == '\n';
    }

    private static bool IsHexDigit(char c)
    {
        return char.IsDigit(c) || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
    }

    #endregion

    // ══════════════════════════════════════════════════════════════════════
    // String literal processing
    // ══════════════════════════════════════════════════════════════════════

    #region String literals

    /// <summary>
    /// Extracts the inner value from a VBScript/VBA string literal (strips quotes, unescapes "").
    /// </summary>
    private static string ExtractStringLiteralInner(string literal)
    {
        if (literal.Length < 2 || !literal.StartsWith("\"") || !literal.EndsWith("\""))
            return literal;
        return literal[1..^1].Replace("\"\"", "\"");
    }

    /// <summary>
    /// Processes a string literal token for obfuscation.
    /// Only replaces non-numeric, non-empty text inside double quotes.
    /// </summary>
    private static void ProcessStringLiteral(
        VbToken token,
        ObfuscationContext context,
        string? filePath,
        string content,
        List<VbReplacement> replacements)
    {
        var inner = ExtractStringLiteralInner(token.Text);
        if (string.IsNullOrEmpty(inner))
            return;

        // Preserve purely numeric or date-like values
        if (IsNumericOrDateLiteral(inner))
            return;

        var alias = context.GetOrCreateAlias(
            inner,
            SemanticCategory.StringLiteral,
            filePath,
            GetLineNumber(content, token.Offset),
            token.Offset,
            token.End);

        // Reconstruct the string literal with the alias value
        var newLiteral = "\"" + alias.Replace("\"", "\"\"") + "\"";
        replacements.Add(new VbReplacement(token.Offset, token.Length, newLiteral));
    }

    /// <summary>
    /// Returns true if the string is purely numeric or a date literal.
    /// </summary>
    private static bool IsNumericOrDateLiteral(string value)
    {
        if (decimal.TryParse(value, System.Globalization.NumberStyles.Any,
            System.Globalization.CultureInfo.InvariantCulture, out _))
            return true;

        if (DateTime.TryParse(value, System.Globalization.CultureInfo.InvariantCulture,
            System.Globalization.DateTimeStyles.None, out _))
            return true;

        if (Regex.IsMatch(value, @"^\d{4}[-/]\d{2}[-/]\d{2}"))
            return true;

        return false;
    }

    #endregion

    // ══════════════════════════════════════════════════════════════════════
    // Helpers
    // ══════════════════════════════════════════════════════════════════════

    #region Helpers

    /// <summary>
    /// Joins lines that end with the line continuation character _ into single logical lines.
    /// Used only for the discovery pass so multi-line declarations are matched as a whole.
    /// </summary>
    private static string NormalizeLineContinuations(string content)
    {
        // Pattern: _ followed by optional whitespace, then \r?\n
        return Regex.Replace(content, @"\s+_[ \t]*\r?\n\s*", " ");
    }

    /// <summary>
    /// Splits a VBScript line on the colon statement separator, but not inside strings.
    /// </summary>
    private static List<string> SplitStatements(string line)
    {
        var result = new List<string>();
        var sb = new StringBuilder();
        var inString = false;

        for (var i = 0; i < line.Length; i++)
        {
            var c = line[i];
            if (c == '"')
            {
                inString = !inString;
                sb.Append(c);
            }
            else if (c == ':' && !inString)
            {
                result.Add(sb.ToString());
                sb.Clear();
            }
            else if (c == '\'' && !inString)
            {
                // Rest of line is comment, stop splitting
                sb.Append(line[i..]);
                break;
            }
            else
            {
                sb.Append(c);
            }
        }

        if (sb.Length > 0)
            result.Add(sb.ToString());

        return result;
    }

    /// <summary>
    /// Splits a string on commas, but not inside parentheses.
    /// Used for parsing variable declaration lists like: a, b(10), c As String
    /// </summary>
    private static List<string> SplitOutsideParens(string text)
    {
        var result = new List<string>();
        var sb = new StringBuilder();
        var depth = 0;

        foreach (var c in text)
        {
            if (c == '(') depth++;
            else if (c == ')') depth = Math.Max(0, depth - 1);
            else if (c == ',' && depth == 0)
            {
                result.Add(sb.ToString());
                sb.Clear();
                continue;
            }
            sb.Append(c);
        }

        if (sb.Length > 0)
            result.Add(sb.ToString());

        return result;
    }

    /// <summary>
    /// Finds the start of a comment (single-quote) on a line, accounting for string literals.
    /// Returns -1 if no comment found.
    /// </summary>
    private static int FindCommentStart(string line)
    {
        var inString = false;
        for (var i = 0; i < line.Length; i++)
        {
            if (line[i] == '"')
                inString = !inString;
            else if (line[i] == '\'' && !inString)
                return i;
        }
        return -1;
    }

    /// <summary>
    /// Returns the 1-based line number for a character offset in the content.
    /// </summary>
    private static int GetLineNumber(string content, int offset)
    {
        var line = 1;
        for (var i = 0; i < offset && i < content.Length; i++)
        {
            if (content[i] == '\n')
                line++;
        }
        return line;
    }

    /// <summary>
    /// Removes overlapping replacements, keeping the one with the larger span
    /// (or first-added if equal). Replacements are sorted by offset ascending.
    /// </summary>
    private static List<VbReplacement> DeduplicateReplacements(List<VbReplacement> replacements)
    {
        if (replacements.Count <= 1)
            return replacements;

        var sorted = replacements
            .OrderBy(r => r.Offset)
            .ThenByDescending(r => r.Length)
            .ToList();

        var result = new List<VbReplacement>();
        var lastEnd = -1;

        foreach (var replacement in sorted)
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
    /// Pops the top of the block stack, expecting the given block type.
    /// Adds an error if the stack is empty or the top doesn't match.
    /// </summary>
    private static void PopExpecting(
        Stack<(string BlockType, int LineNumber)> stack,
        string expected,
        int closingLine,
        List<string> errors)
    {
        if (stack.Count == 0)
        {
            errors.Add($"Unexpected 'End {expected}' at line {closingLine} with no matching opening block");
            return;
        }

        var (blockType, openLine) = stack.Pop();
        if (!string.Equals(blockType, expected, StringComparison.OrdinalIgnoreCase))
        {
            errors.Add($"Mismatched block: expected 'End {blockType}' (opened at line {openLine}) but found 'End {expected}' at line {closingLine}");
        }
    }

    /// <summary>
    /// Regex-based fallback for deobfuscation when tokenization fails.
    /// </summary>
    private LanguageProcessingResult DeobfuscateByRegex(string content, ObfuscationContext context, List<string> warnings)
    {
        var reverse = context.Mappings.Reverse;
        var result = content;
        var count = 0;

        foreach (var kvp in reverse.OrderByDescending(k => k.Key.Length))
        {
            var alias = kvp.Key;
            var original = kvp.Value;

            var pattern = @"(?<![A-Za-z0-9_])" + Regex.Escape(alias) + @"(?![A-Za-z0-9_])";
            var matches = Regex.Matches(result, pattern);
            if (matches.Count > 0)
            {
                count += matches.Count;
                result = Regex.Replace(result, pattern, original);
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

    #endregion
}
