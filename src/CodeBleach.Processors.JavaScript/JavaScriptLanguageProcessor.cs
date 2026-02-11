using System.Collections.Frozen;
using System.Text;
using Acornima;
using Acornima.Ast;
using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;

namespace CodeBleach.Processors.JavaScript;

/// <summary>
/// JavaScript language processor using Acornima for AST-aware obfuscation.
/// Parses JS/ESM source with Acornima, discovers all declared identifiers via AST visitor,
/// then performs position-based replacement from back-to-front for maximum fidelity.
/// </summary>
public sealed class JavaScriptLanguageProcessor : ILanguageProcessor
{
    public string ProcessorId => "javascript";
    public string DisplayName => "JavaScript (Acornima)";
    public IReadOnlySet<string> SupportedExtensions { get; } = new HashSet<string> { ".js", ".mjs", ".cjs" };
    public int Priority => 10;

    // ── Reserved identifiers that must never be renamed ────────────────────────

    private static readonly FrozenSet<string> JsKeywords = new HashSet<string>
    {
        // Language keywords
        "var", "let", "const", "function", "class", "return", "if", "else",
        "for", "while", "do", "switch", "case", "break", "continue", "new",
        "delete", "typeof", "instanceof", "void", "in", "of", "async", "await",
        "yield", "throw", "try", "catch", "finally", "with", "debugger",
        "import", "export", "default", "from", "as", "extends", "super",
        "this", "static", "get", "set", "enum", "implements", "interface",
        "package", "private", "protected", "public",

        // Literal values
        "true", "false", "null", "undefined", "NaN", "Infinity",

        // Global objects and constructors
        "window", "document", "console", "Math", "JSON", "Array", "Object",
        "Promise", "Map", "Set", "WeakMap", "WeakSet", "Symbol", "Proxy",
        "Reflect", "Number", "String", "Boolean", "RegExp", "Error", "Date",
        "Function", "ArrayBuffer", "SharedArrayBuffer", "DataView",
        "Int8Array", "Uint8Array", "Uint8ClampedArray", "Int16Array",
        "Uint16Array", "Int32Array", "Uint32Array", "Float32Array",
        "Float64Array", "BigInt64Array", "BigUint64Array", "BigInt",
        "TypeError", "RangeError", "ReferenceError", "SyntaxError",
        "URIError", "EvalError", "AggregateError", "Intl", "globalThis",
        "WeakRef", "FinalizationRegistry", "Iterator", "AsyncIterator",
        "Generator", "GeneratorFunction", "AsyncFunction", "AsyncGeneratorFunction",

        // Global functions
        "parseInt", "parseFloat", "isNaN", "isFinite", "decodeURI",
        "decodeURIComponent", "encodeURI", "encodeURIComponent", "eval",
        "setTimeout", "setInterval", "clearTimeout", "clearInterval",
        "requestAnimationFrame", "cancelAnimationFrame", "fetch", "alert",
        "confirm", "prompt", "atob", "btoa", "structuredClone", "queueMicrotask",

        // Special identifiers
        "arguments", "constructor", "prototype", "__proto__",

        // Node.js globals
        "require", "module", "exports", "process", "__dirname", "__filename",
        "Buffer", "global",

        // DOM API members commonly used as identifiers
        "getElementById", "querySelector", "querySelectorAll",
        "addEventListener", "removeEventListener", "createElement",
        "appendChild", "removeChild", "insertBefore", "replaceChild",
        "getAttribute", "setAttribute", "removeAttribute", "classList",
        "innerHTML", "outerHTML", "textContent", "innerText", "style",
        "parentNode", "parentElement", "childNodes", "children",
        "firstChild", "lastChild", "nextSibling", "previousSibling",
        "firstElementChild", "lastElementChild", "nextElementSibling",
        "previousElementSibling", "nodeType", "nodeName", "nodeValue",
        "ownerDocument", "cloneNode", "contains", "hasChildNodes",
        "normalize", "isEqualNode", "isSameNode", "compareDocumentPosition",
        "dispatchEvent", "preventDefault", "stopPropagation",
        "stopImmediatePropagation", "target", "currentTarget", "type",
        "bubbles", "cancelable", "defaultPrevented", "timeStamp",
        "composedPath",
        "location", "history", "navigator", "screen", "localStorage",
        "sessionStorage", "XMLHttpRequest", "FormData", "Headers",
        "Request", "Response", "URL", "URLSearchParams", "AbortController",
        "AbortSignal", "ReadableStream", "WritableStream", "TransformStream",
        "Blob", "File", "FileReader", "FileList",
        "performance", "Worker", "ServiceWorker", "MessageChannel",
        "MessagePort", "BroadcastChannel", "Notification",
        "IntersectionObserver", "MutationObserver", "ResizeObserver",
        "PerformanceObserver", "EventSource", "WebSocket",
        "CustomEvent", "Event", "MouseEvent", "KeyboardEvent",
        "TouchEvent", "FocusEvent", "InputEvent", "DragEvent",
        "ClipboardEvent", "AnimationEvent", "TransitionEvent",
        "crypto", "TextEncoder", "TextDecoder",
    }.ToFrozenSet();

    /// <summary>
    /// Alias prefix pattern used for deobfuscation detection.
    /// All aliases generated by NamingStrategy follow the pattern PREFIX_N.
    /// </summary>
    private static readonly FrozenSet<string> AliasPrefixes = new HashSet<string>
    {
        "CLS", "MTD", "VAR", "PRM", "FN", "IMP", "EXP", "PROP", "FLD", "STR", "CMT", "GEN",
    }.ToFrozenSet();

    // ── ILanguageProcessor implementation ──────────────────────────────────────

    public bool CanProcess(string filePath, string content)
    {
        var ext = Path.GetExtension(filePath);
        return SupportedExtensions.Contains(ext);
    }

    public LanguageProcessingResult Obfuscate(string content, ObfuscationContext context, string? filePath = null)
    {
        if (string.IsNullOrWhiteSpace(content))
        {
            return NoTransformation(content);
        }

        try
        {
            var ast = ParseJavaScript(content);
            var delegationOnly = context.Scope.IsDelegationOnly(ProcessorId);
            var replacements = new List<IdentifierReplacement>();
            var warnings = new List<string>();

            if (!delegationOnly)
            {
                // Full mode: discover declarations and collect identifier replacements
                var discoveryVisitor = new DeclarationDiscoveryVisitor();
                discoveryVisitor.Visit(ast);

                var declaredNames = discoveryVisitor.DeclaredNames;

                var identifierCollector = new IdentifierCollector(declaredNames, context, filePath, content);
                identifierCollector.Visit(ast);
                replacements.AddRange(identifierCollector.Replacements);
                warnings.AddRange(identifierCollector.Warnings);

                // Collect comment replacements
                CollectCommentReplacements(content, context, filePath, replacements);
            }

            // Collect string literal replacements
            // In delegation-only mode, only SQL-in-strings are processed.
            var literalCollector = new StringLiteralCollector(context, filePath, content, delegationOnly);
            literalCollector.Visit(ast);
            replacements.AddRange(literalCollector.Replacements);

            if (replacements.Count == 0)
            {
                return NoTransformation(content);
            }

            // Sort by position descending so we replace from back to front
            replacements.Sort((a, b) => b.Start.CompareTo(a.Start));

            // Deduplicate overlapping replacements (keep the one that starts later / is encountered first in desc order)
            var deduped = DeduplicateReplacements(replacements);

            var result = ApplyReplacements(content, deduped);

            return new LanguageProcessingResult
            {
                Content = result,
                WasTransformed = true,
                ReplacementCount = deduped.Count + literalCollector.SqlReplacementCount,
                ProcessorId = ProcessorId,
                Warnings = warnings,
            };
        }
        catch (Exception ex)
        {
            return new LanguageProcessingResult
            {
                Content = content,
                WasTransformed = false,
                ReplacementCount = 0,
                ProcessorId = ProcessorId,
                Warnings = [$"JavaScript parsing failed: {ex.Message}"],
            };
        }
    }

    public LanguageProcessingResult Deobfuscate(string content, ObfuscationContext context, string? filePath = null)
    {
        if (string.IsNullOrWhiteSpace(content))
        {
            return NoTransformation(content);
        }

        try
        {
            var ast = ParseJavaScript(content);
            var replacements = new List<IdentifierReplacement>();

            // Walk all identifiers and check if they match an alias pattern
            var deobfuscator = new DeobfuscationVisitor(context);
            deobfuscator.Visit(ast);
            replacements.AddRange(deobfuscator.Replacements);

            // Restore comments: look for [Comment removed] patterns
            CollectCommentRestorations(content, context, replacements);

            // Restore string literals: walk AST for string literals matching alias patterns
            var literalDeobfuscator = new StringLiteralDeobfuscator(context, content);
            literalDeobfuscator.Visit(ast);
            replacements.AddRange(literalDeobfuscator.Replacements);

            if (replacements.Count == 0)
            {
                return NoTransformation(content);
            }

            replacements.Sort((a, b) => b.Start.CompareTo(a.Start));
            var deduped = DeduplicateReplacements(replacements);

            var result = ApplyReplacements(content, deduped);

            return new LanguageProcessingResult
            {
                Content = result,
                WasTransformed = true,
                ReplacementCount = deduped.Count,
                ProcessorId = ProcessorId,
            };
        }
        catch (Exception ex)
        {
            return new LanguageProcessingResult
            {
                Content = content,
                WasTransformed = false,
                ReplacementCount = 0,
                ProcessorId = ProcessorId,
                Warnings = [$"JavaScript parsing failed during deobfuscation: {ex.Message}"],
            };
        }
    }

    public ValidationResult Validate(string obfuscatedContent)
    {
        try
        {
            ParseJavaScript(obfuscatedContent);
            return ValidationResult.Valid();
        }
        catch (SyntaxErrorException ex)
        {
            return ValidationResult.Invalid(
                [$"Syntax error at index {ex.Error.Index}, line {ex.Error.LineNumber}: {ex.Error.Description}"]);
        }
        catch (Exception ex)
        {
            return ValidationResult.Invalid([$"Parse error: {ex.Message}"]);
        }
    }

    // ── Parsing ────────────────────────────────────────────────────────────────

    /// <summary>
    /// Attempts to parse as ES module first (supports import/export),
    /// falls back to script mode for CommonJS / classic scripts.
    /// </summary>
    private static Node ParseJavaScript(string code)
    {
        var options = new ParserOptions
        {
            AllowReturnOutsideFunction = true,
            Tolerant = true,
        };

        var parser = new Parser(options);

        try
        {
            return parser.ParseModule(code);
        }
        catch
        {
            // Fall back to script mode for CommonJS or non-module code
            parser = new Parser(options);
            return parser.ParseScript(code);
        }
    }

    // ── Comment handling ───────────────────────────────────────────────────────

    /// <summary>
    /// Collects all comments from the source text and creates replacements.
    /// Uses a manual lexer-style scan since Acornima does not attach comments to AST nodes.
    /// </summary>
    private static void CollectCommentReplacements(
        string content,
        ObfuscationContext context,
        string? filePath,
        List<IdentifierReplacement> replacements)
    {
        // Scan source text directly for all comments.
        // Acornima does not attach comments to AST nodes by default,
        // so we use a manual lexer-style scan for maximum reliability.
        var merged = FindAllComments(content);

        foreach (var comment in merged)
        {
            var originalText = content[comment.Start..comment.End];

            // Skip if it's just a shebang line
            if (originalText.StartsWith("#!"))
            {
                continue;
            }

            var lineNumber = GetLineNumber(content, comment.Start);

            context.GetOrCreateAlias(
                originalText,
                SemanticCategory.Comment,
                filePath,
                lineNumber,
                comment.Start,
                comment.End);

            var replacement = comment.IsBlock
                ? "/* [Comment removed] */"
                : "// [Comment removed]";

            replacements.Add(new IdentifierReplacement(comment.Start, comment.End, replacement));
        }
    }

    /// <summary>
    /// Scans source text for all single-line (//) and block (/* */) comments.
    /// This is a fallback to catch comments that Acornima may not attach to AST nodes.
    /// </summary>
    private static List<CommentInfo> FindAllComments(string source)
    {
        var comments = new List<CommentInfo>();
        var i = 0;
        var length = source.Length;

        while (i < length)
        {
            var ch = source[i];

            // Skip string literals to avoid false positives
            if (ch is '"' or '\'')
            {
                i = SkipStringLiteral(source, i);
                continue;
            }

            // Skip template literals
            if (ch == '`')
            {
                i = SkipTemplateLiteral(source, i);
                continue;
            }

            // Skip regex literals (simple heuristic)
            if (ch == '/' && i + 1 < length && source[i + 1] != '/' && source[i + 1] != '*')
            {
                // Could be regex or division; skip for safety
                i++;
                continue;
            }

            if (ch == '/' && i + 1 < length)
            {
                if (source[i + 1] == '/')
                {
                    // Single-line comment
                    var start = i;
                    i += 2;
                    while (i < length && source[i] != '\n' && source[i] != '\r')
                    {
                        i++;
                    }
                    comments.Add(new CommentInfo(start, i, IsBlock: false));
                    continue;
                }

                if (source[i + 1] == '*')
                {
                    // Block comment
                    var start = i;
                    i += 2;
                    while (i + 1 < length && !(source[i] == '*' && source[i + 1] == '/'))
                    {
                        i++;
                    }
                    if (i + 1 < length)
                    {
                        i += 2; // Skip */
                    }
                    comments.Add(new CommentInfo(start, i, IsBlock: true));
                    continue;
                }
            }

            i++;
        }

        return comments;
    }

    private static int SkipStringLiteral(string source, int pos)
    {
        var quote = source[pos];
        pos++;
        while (pos < source.Length)
        {
            if (source[pos] == '\\')
            {
                pos += 2; // Skip escaped character
                continue;
            }
            if (source[pos] == quote)
            {
                return pos + 1;
            }
            pos++;
        }
        return pos;
    }

    private static int SkipTemplateLiteral(string source, int pos)
    {
        pos++; // Skip opening backtick
        var depth = 0;
        while (pos < source.Length)
        {
            if (source[pos] == '\\')
            {
                pos += 2;
                continue;
            }
            if (source[pos] == '$' && pos + 1 < source.Length && source[pos + 1] == '{')
            {
                depth++;
                pos += 2;
                continue;
            }
            if (source[pos] == '}' && depth > 0)
            {
                depth--;
                pos++;
                continue;
            }
            if (source[pos] == '`' && depth == 0)
            {
                return pos + 1;
            }
            pos++;
        }
        return pos;
    }

    /// <summary>
    /// During deobfuscation, find comment placeholders and restore from context.
    /// </summary>
    private static void CollectCommentRestorations(
        string content,
        ObfuscationContext context,
        List<IdentifierReplacement> replacements)
    {
        // Restore single-line comment placeholders
        var singleLinePattern = "// [Comment removed]";
        var blockPattern = "/* [Comment removed] */";

        var pos = 0;
        while (pos < content.Length)
        {
            var singleIdx = content.IndexOf(singleLinePattern, pos, StringComparison.Ordinal);
            var blockIdx = content.IndexOf(blockPattern, pos, StringComparison.Ordinal);

            if (singleIdx < 0 && blockIdx < 0) break;

            int foundIdx;
            string pattern;
            if (singleIdx >= 0 && (blockIdx < 0 || singleIdx <= blockIdx))
            {
                foundIdx = singleIdx;
                pattern = singleLinePattern;
            }
            else
            {
                foundIdx = blockIdx;
                pattern = blockPattern;
            }

            // Try to find the original comment in the reverse mapping.
            // Look through reverse mappings for any CMT_ alias that maps to a matching comment type.
            foreach (var kvp in context.Mappings.Reverse)
            {
                if (kvp.Key.StartsWith("CMT_", StringComparison.Ordinal))
                {
                    var originalComment = kvp.Value;
                    // Check if this comment was mapped and matches the placeholder type
                    if (pattern == singleLinePattern && originalComment.StartsWith("//", StringComparison.Ordinal))
                    {
                        // Could be this one; we need order-based matching
                        // For simplicity, replace first occurrence with first mapping
                        replacements.Add(new IdentifierReplacement(foundIdx, foundIdx + pattern.Length, originalComment));
                        pos = foundIdx + pattern.Length;
                        goto nextComment;
                    }
                    if (pattern == blockPattern && originalComment.StartsWith("/*", StringComparison.Ordinal))
                    {
                        replacements.Add(new IdentifierReplacement(foundIdx, foundIdx + pattern.Length, originalComment));
                        pos = foundIdx + pattern.Length;
                        goto nextComment;
                    }
                }
            }

            pos = foundIdx + pattern.Length;
            nextComment:;
        }
    }

    // ── Replacement infrastructure ─────────────────────────────────────────────

    /// <summary>
    /// Removes overlapping replacements, preferring the one with the later start position.
    /// Input must already be sorted descending by Start.
    /// </summary>
    private static List<IdentifierReplacement> DeduplicateReplacements(List<IdentifierReplacement> sorted)
    {
        if (sorted.Count == 0) return sorted;

        var result = new List<IdentifierReplacement>(sorted.Count) { sorted[0] };

        for (var i = 1; i < sorted.Count; i++)
        {
            var current = sorted[i];
            var previous = result[^1]; // Last accepted (which has a higher or equal Start)

            // current.Start < previous.Start because sorted descending.
            // Overlap if current.End > previous.Start.
            if (current.End > previous.Start)
            {
                // Skip this overlapping replacement (keep the later/higher-start one)
                continue;
            }

            result.Add(current);
        }

        return result;
    }

    /// <summary>
    /// Applies replacements to the source string. Replacements must be sorted descending by Start.
    /// </summary>
    private static string ApplyReplacements(string source, List<IdentifierReplacement> replacements)
    {
        var sb = new StringBuilder(source);

        foreach (var r in replacements)
        {
            sb.Remove(r.Start, r.End - r.Start);
            sb.Insert(r.Start, r.NewText);
        }

        return sb.ToString();
    }

    private LanguageProcessingResult NoTransformation(string content)
    {
        return new LanguageProcessingResult
        {
            Content = content,
            WasTransformed = false,
            ReplacementCount = 0,
            ProcessorId = ProcessorId,
        };
    }

    private static int GetLineNumber(string content, int offset)
    {
        var line = 1;
        for (var i = 0; i < offset && i < content.Length; i++)
        {
            if (content[i] == '\n') line++;
        }
        return line;
    }

    /// <summary>
    /// Determines whether a string value looks like a SQL statement based on common
    /// SQL keyword prefixes. Used to delegate SQL-like string literals to the SQL processor.
    /// </summary>
    private static bool LooksLikeSql(string text)
    {
        if (text.Length < 10) return false; // Too short to be meaningful SQL
        var trimmed = text.TrimStart().ToUpperInvariant();
        return trimmed.StartsWith("SELECT ", StringComparison.Ordinal) ||
               trimmed.StartsWith("INSERT ", StringComparison.Ordinal) ||
               trimmed.StartsWith("UPDATE ", StringComparison.Ordinal) ||
               trimmed.StartsWith("DELETE ", StringComparison.Ordinal) ||
               trimmed.StartsWith("CREATE ", StringComparison.Ordinal) ||
               trimmed.StartsWith("ALTER ", StringComparison.Ordinal) ||
               trimmed.StartsWith("DROP ", StringComparison.Ordinal) ||
               trimmed.StartsWith("EXEC ", StringComparison.Ordinal) ||
               trimmed.StartsWith("EXECUTE ", StringComparison.Ordinal) ||
               trimmed.StartsWith("WITH ", StringComparison.Ordinal) ||
               trimmed.StartsWith("MERGE ", StringComparison.Ordinal) ||
               trimmed.StartsWith("DECLARE ", StringComparison.Ordinal) ||
               trimmed.StartsWith("BEGIN ", StringComparison.Ordinal);
    }

    /// <summary>
    /// Checks if a name looks like a generated alias (PREFIX_N pattern).
    /// </summary>
    private static bool IsAliasPattern(string name)
    {
        var underscoreIdx = name.LastIndexOf('_');
        if (underscoreIdx <= 0 || underscoreIdx >= name.Length - 1) return false;

        var prefix = name[..underscoreIdx];
        var suffix = name[(underscoreIdx + 1)..];

        return AliasPrefixes.Contains(prefix) && int.TryParse(suffix, out _);
    }

    // ── Data types ─────────────────────────────────────────────────────────────

    private readonly record struct IdentifierReplacement(int Start, int End, string NewText);

    private readonly record struct CommentInfo(int Start, int End, bool IsBlock);

    // ── AST Visitors ───────────────────────────────────────────────────────────

    /// <summary>
    /// Discovery pass: walks the AST to collect all declared identifier names.
    /// An identifier is considered "declared" if it appears in a var/let/const declaration,
    /// function/class declaration, parameter list, import binding, etc.
    /// Only declared identifiers will be renamed; undeclared references are preserved
    /// as they are assumed to be external/global.
    /// </summary>
    private sealed class DeclarationDiscoveryVisitor : AstVisitor
    {
        public HashSet<string> DeclaredNames { get; } = [];

        protected override object? VisitVariableDeclarator(VariableDeclarator node)
        {
            CollectBindingNames(node.Id);
            return base.VisitVariableDeclarator(node);
        }

        protected override object? VisitFunctionDeclaration(FunctionDeclaration node)
        {
            if (node.Id is not null)
            {
                DeclaredNames.Add(node.Id.Name);
            }
            CollectParameterNames(node.Params);
            return base.VisitFunctionDeclaration(node);
        }

        protected override object? VisitFunctionExpression(FunctionExpression node)
        {
            if (node.Id is not null)
            {
                DeclaredNames.Add(node.Id.Name);
            }
            CollectParameterNames(node.Params);
            return base.VisitFunctionExpression(node);
        }

        protected override object? VisitArrowFunctionExpression(ArrowFunctionExpression node)
        {
            CollectParameterNames(node.Params);
            return base.VisitArrowFunctionExpression(node);
        }

        protected override object? VisitClassDeclaration(ClassDeclaration node)
        {
            if (node.Id is not null)
            {
                DeclaredNames.Add(node.Id.Name);
            }
            return base.VisitClassDeclaration(node);
        }

        protected override object? VisitClassExpression(ClassExpression node)
        {
            if (node.Id is not null)
            {
                DeclaredNames.Add(node.Id.Name);
            }
            return base.VisitClassExpression(node);
        }

        protected override object? VisitImportSpecifier(ImportSpecifier node)
        {
            DeclaredNames.Add(node.Local.Name);
            return base.VisitImportSpecifier(node);
        }

        protected override object? VisitImportDefaultSpecifier(ImportDefaultSpecifier node)
        {
            DeclaredNames.Add(node.Local.Name);
            return base.VisitImportDefaultSpecifier(node);
        }

        protected override object? VisitImportNamespaceSpecifier(ImportNamespaceSpecifier node)
        {
            DeclaredNames.Add(node.Local.Name);
            return base.VisitImportNamespaceSpecifier(node);
        }

        protected override object? VisitCatchClause(CatchClause node)
        {
            if (node.Param is not null)
            {
                CollectBindingNames(node.Param);
            }
            return base.VisitCatchClause(node);
        }

        /// <summary>
        /// Extracts all identifier names from a binding pattern (simple Identifier,
        /// ArrayPattern, ObjectPattern, AssignmentPattern, RestElement).
        /// </summary>
        private void CollectBindingNames(Node pattern)
        {
            switch (pattern)
            {
                case Identifier id:
                    DeclaredNames.Add(id.Name);
                    break;
                case ArrayPattern array:
                    foreach (var element in array.Elements)
                    {
                        if (element is not null)
                        {
                            CollectBindingNames(element);
                        }
                    }
                    break;
                case ObjectPattern obj:
                    foreach (var prop in obj.Properties)
                    {
                        if (prop is Property p)
                        {
                            // In destructuring { a: b }, b is the binding name
                            CollectBindingNames(p.Value);
                        }
                        else if (prop is RestElement rest)
                        {
                            CollectBindingNames(rest.Argument);
                        }
                    }
                    break;
                case AssignmentPattern assign:
                    CollectBindingNames(assign.Left);
                    break;
                case RestElement restElement:
                    CollectBindingNames(restElement.Argument);
                    break;
            }
        }

        private void CollectParameterNames(in NodeList<Node> parameters)
        {
            foreach (var param in parameters)
            {
                CollectBindingNames(param);
            }
        }
    }

    /// <summary>
    /// Main obfuscation visitor: walks the AST collecting identifier positions
    /// and their semantic categories for replacement.
    /// Only renames identifiers that were found in the declaration discovery pass.
    /// </summary>
    private sealed class IdentifierCollector : AstVisitor
    {
        private readonly HashSet<string> _declaredNames;
        private readonly ObfuscationContext _context;
        private readonly string? _filePath;
        private readonly string _source;

        public List<IdentifierReplacement> Replacements { get; } = [];
        public List<string> Warnings { get; } = [];

        public IdentifierCollector(
            HashSet<string> declaredNames,
            ObfuscationContext context,
            string? filePath,
            string source)
        {
            _declaredNames = declaredNames;
            _context = context;
            _filePath = filePath;
            _source = source;
        }

        // ── Variable declarations ──────────────────────────────────────────

        protected override object? VisitVariableDeclarator(VariableDeclarator node)
        {
            CollectBindingReplacements(node.Id, SemanticCategory.Variable);
            // Visit the initializer but NOT the Id again (we already handled it)
            if (node.Init is not null)
            {
                Visit(node.Init);
            }
            return node;
        }

        // ── Function declarations ──────────────────────────────────────────

        protected override object? VisitFunctionDeclaration(FunctionDeclaration node)
        {
            if (node.Id is not null)
            {
                AddIdentifierReplacement(node.Id, SemanticCategory.Function);
            }
            CollectParameterReplacements(node.Params);
            Visit(node.Body);
            return node;
        }

        protected override object? VisitFunctionExpression(FunctionExpression node)
        {
            if (node.Id is not null)
            {
                AddIdentifierReplacement(node.Id, SemanticCategory.Function);
            }
            CollectParameterReplacements(node.Params);
            Visit(node.Body);
            return node;
        }

        protected override object? VisitArrowFunctionExpression(ArrowFunctionExpression node)
        {
            CollectParameterReplacements(node.Params);
            Visit(node.Body);
            return node;
        }

        // ── Class declarations ─────────────────────────────────────────────

        protected override object? VisitClassDeclaration(ClassDeclaration node)
        {
            if (node.Id is not null)
            {
                AddIdentifierReplacement(node.Id, SemanticCategory.Class);
            }
            if (node.SuperClass is not null)
            {
                Visit(node.SuperClass);
            }
            Visit(node.Body);
            return node;
        }

        protected override object? VisitClassExpression(ClassExpression node)
        {
            if (node.Id is not null)
            {
                AddIdentifierReplacement(node.Id, SemanticCategory.Class);
            }
            if (node.SuperClass is not null)
            {
                Visit(node.SuperClass);
            }
            Visit(node.Body);
            return node;
        }

        // ── Class members ──────────────────────────────────────────────────

        protected override object? VisitMethodDefinition(MethodDefinition node)
        {
            // Only rename non-computed method keys that are Identifiers
            if (!node.Computed && node.Key is Identifier methodName)
            {
                // Skip constructor
                if (methodName.Name != "constructor")
                {
                    AddIdentifierReplacement(methodName, SemanticCategory.Method);
                }
            }

            // Visit the function value (which has parameters and body)
            Visit(node.Value);
            return node;
        }

        protected override object? VisitPropertyDefinition(PropertyDefinition node)
        {
            // Only rename non-computed property keys that are Identifiers
            if (!node.Computed && node.Key is Identifier propName)
            {
                AddIdentifierReplacement(propName, SemanticCategory.Property);
            }

            if (node.Value is not null)
            {
                Visit(node.Value);
            }
            return node;
        }

        // ── Import/Export bindings ─────────────────────────────────────────

        protected override object? VisitImportSpecifier(ImportSpecifier node)
        {
            // import { Foo as Bar } from '...'
            // Foo = Imported, Bar = Local
            // Both are binding declarations that should be renamed
            if (node.Imported is Identifier imported)
            {
                AddIdentifierReplacement(imported, SemanticCategory.ImportBinding);
            }

            // Only rename Local separately if it's a different identifier from Imported
            if (node.Local.Range.Start != (node.Imported is Identifier imp ? imp.Range.Start : -1))
            {
                AddIdentifierReplacement(node.Local, SemanticCategory.ImportBinding);
            }

            return node;
        }

        protected override object? VisitImportDefaultSpecifier(ImportDefaultSpecifier node)
        {
            AddIdentifierReplacement(node.Local, SemanticCategory.ImportBinding);
            return node;
        }

        protected override object? VisitImportNamespaceSpecifier(ImportNamespaceSpecifier node)
        {
            AddIdentifierReplacement(node.Local, SemanticCategory.ImportBinding);
            return node;
        }

        protected override object? VisitExportSpecifier(ExportSpecifier node)
        {
            // export { Foo as Bar }
            // Foo = Local, Bar = Exported
            if (node.Local is Identifier local && _declaredNames.Contains(local.Name))
            {
                AddIdentifierReplacement(local, SemanticCategory.ExportBinding);
            }

            if (node.Exported is Identifier exported
                && exported.Range.Start != (node.Local is Identifier loc ? loc.Range.Start : -1))
            {
                AddIdentifierReplacement(exported, SemanticCategory.ExportBinding);
            }

            return node;
        }

        protected override object? VisitExportDefaultDeclaration(ExportDefaultDeclaration node)
        {
            Visit(node.Declaration);
            return node;
        }

        protected override object? VisitExportNamedDeclaration(ExportNamedDeclaration node)
        {
            if (node.Declaration is not null)
            {
                Visit(node.Declaration);
            }
            foreach (var specifier in node.Specifiers)
            {
                Visit(specifier);
            }
            return node;
        }

        // ── Import declarations (skip the source path) ────────────────────

        protected override object? VisitImportDeclaration(ImportDeclaration node)
        {
            // Visit specifiers but NOT the source (import path string)
            foreach (var specifier in node.Specifiers)
            {
                Visit(specifier);
            }
            return node;
        }

        // ── Catch clause ──────────────────────────────────────────────────

        protected override object? VisitCatchClause(CatchClause node)
        {
            if (node.Param is not null)
            {
                CollectBindingReplacements(node.Param, SemanticCategory.Parameter);
            }
            Visit(node.Body);
            return node;
        }

        // ── For-in / For-of ───────────────────────────────────────────────

        protected override object? VisitForInStatement(ForInStatement node)
        {
            if (node.Left is VariableDeclaration varDecl)
            {
                Visit(varDecl);
            }
            else
            {
                Visit(node.Left);
            }
            Visit(node.Right);
            Visit(node.Body);
            return node;
        }

        protected override object? VisitForOfStatement(ForOfStatement node)
        {
            if (node.Left is VariableDeclaration varDecl)
            {
                Visit(varDecl);
            }
            else
            {
                Visit(node.Left);
            }
            Visit(node.Right);
            Visit(node.Body);
            return node;
        }

        // ── General identifier references ──────────────────────────────────

        protected override object? VisitIdentifier(Identifier node)
        {
            // This catches all remaining identifier references (usages, not declarations).
            // We only rename if the name was found in the declaration set and is not a keyword/global.
            if (ShouldRenameReference(node.Name))
            {
                AddIdentifierReplacement(node, SemanticCategory.Variable);
            }
            return base.VisitIdentifier(node);
        }

        // ── Member expressions ─────────────────────────────────────────────

        protected override object? VisitMemberExpression(MemberExpression node)
        {
            // Visit the object part (may contain renameable identifiers)
            Visit(node.Object);

            // For non-computed member access (obj.property), do NOT rename the property
            // as we can't safely determine if it's user-defined without type information.
            // For computed access (obj[expr]), visit the expression normally.
            if (node.Computed)
            {
                Visit(node.Property);
            }

            return node;
        }

        // ── Object literal properties ──────────────────────────────────────

        protected override object? VisitObjectProperty(ObjectProperty node)
        {
            // Object literal property: { key: value }
            // Don't rename property keys in object literals (they're structural, not declarations)
            // unless it's a shorthand { myVar } which is both key and value referencing a variable
            if (node.Shorthand && node.Key is Identifier shorthandId)
            {
                // Shorthand property: { myVar } means { myVar: myVar }
                // Only rename if it's a declared name
                if (ShouldRenameReference(shorthandId.Name))
                {
                    AddIdentifierReplacement(shorthandId, SemanticCategory.Variable);
                }
                // Don't visit value separately as it shares the same node position
                return node;
            }

            // For non-shorthand properties, don't rename the key, just visit the value
            if (node.Value is not null)
            {
                Visit(node.Value);
            }
            return node;
        }

        // ── Helpers ────────────────────────────────────────────────────────

        private bool ShouldRenameReference(string name)
        {
            return _declaredNames.Contains(name) && !JsKeywords.Contains(name);
        }

        private void AddIdentifierReplacement(Identifier node, SemanticCategory category)
        {
            var name = node.Name;

            if (JsKeywords.Contains(name)) return;

            var lineNumber = GetLineNumber(_source, node.Range.Start);
            var alias = _context.GetOrCreateAlias(
                name,
                category,
                _filePath,
                lineNumber,
                node.Range.Start,
                node.Range.End);

            Replacements.Add(new IdentifierReplacement(
                node.Range.Start,
                node.Range.End,
                alias));
        }

        private void CollectBindingReplacements(Node pattern, SemanticCategory category)
        {
            switch (pattern)
            {
                case Identifier id:
                    AddIdentifierReplacement(id, category);
                    break;

                case ArrayPattern array:
                    foreach (var element in array.Elements)
                    {
                        if (element is not null)
                        {
                            CollectBindingReplacements(element, category);
                        }
                    }
                    break;

                case ObjectPattern obj:
                    foreach (var prop in obj.Properties)
                    {
                        if (prop is Property p)
                        {
                            // In destructuring { a: b = default }, only b is the binding
                            // If shorthand { a }, a is the binding
                            if (p.Shorthand && p.Key is Identifier shorthandKey)
                            {
                                AddIdentifierReplacement(shorthandKey, category);
                            }
                            else
                            {
                                // p.Key is the source property name (don't rename),
                                // p.Value is the target binding (do rename)
                                CollectBindingReplacements(p.Value, category);
                            }
                        }
                        else if (prop is RestElement rest)
                        {
                            CollectBindingReplacements(rest.Argument, category);
                        }
                    }
                    break;

                case AssignmentPattern assign:
                    CollectBindingReplacements(assign.Left, category);
                    // Visit the default value expression for references
                    Visit(assign.Right);
                    break;

                case RestElement restElement:
                    CollectBindingReplacements(restElement.Argument, category);
                    break;
            }
        }

        private void CollectParameterReplacements(in NodeList<Node> parameters)
        {
            foreach (var param in parameters)
            {
                CollectBindingReplacements(param, SemanticCategory.Parameter);
            }
        }
    }

    /// <summary>
    /// Collects string literals for obfuscation (non-empty, non-numeric strings).
    /// Skips import/export source paths and property keys.
    /// </summary>
    private sealed class StringLiteralCollector : AstVisitor
    {
        private readonly ObfuscationContext _context;
        private readonly string? _filePath;
        private readonly string _source;
        private readonly bool _delegationOnly;

        public List<IdentifierReplacement> Replacements { get; } = [];

        /// <summary>
        /// Tracks additional replacement count from SQL processor delegation.
        /// SQL delegation replaces a single string literal but the SQL processor
        /// may report multiple internal replacements.
        /// </summary>
        public int SqlReplacementCount { get; private set; }

        public StringLiteralCollector(ObfuscationContext context, string? filePath, string source, bool delegationOnly = false)
        {
            _context = context;
            _filePath = filePath;
            _source = source;
            _delegationOnly = delegationOnly;
        }

        protected override object? VisitImportDeclaration(ImportDeclaration node)
        {
            // Visit specifiers but skip the source path
            foreach (var specifier in node.Specifiers)
            {
                Visit(specifier);
            }
            // Do NOT visit node.Source
            return node;
        }

        protected override object? VisitExportAllDeclaration(ExportAllDeclaration node)
        {
            // Skip the source path
            return node;
        }

        protected override object? VisitExportNamedDeclaration(ExportNamedDeclaration node)
        {
            if (node.Declaration is not null)
            {
                Visit(node.Declaration);
            }
            foreach (var specifier in node.Specifiers)
            {
                Visit(specifier);
            }
            // Do NOT visit node.Source (the 'from' path)
            return node;
        }

        protected override object? VisitCallExpression(CallExpression node)
        {
            // Skip require('...') paths - the string argument is a module path, not user content
            if (node.Callee is Identifier calleeId && calleeId.Name == "require" && node.Arguments.Count == 1)
            {
                // Don't visit the argument (module path); still visit callee for consistency
                return node;
            }

            return base.VisitCallExpression(node);
        }

        protected override object? VisitImportExpression(ImportExpression node)
        {
            // Skip dynamic import('...') paths - the source argument is a module path
            return node;
        }

        protected override object? VisitLiteral(Literal node)
        {
            // Only process string literals
            if (node.Value is not string strValue) return base.VisitLiteral(node);

            // Skip empty strings
            if (string.IsNullOrEmpty(strValue)) return base.VisitLiteral(node);

            // Skip strings that look like paths/modules (contain / or \)
            // These are likely require paths or similar
            if (strValue.Contains('/') || strValue.Contains('\\')) return base.VisitLiteral(node);

            // Skip numeric-only strings
            if (double.TryParse(strValue, out _)) return base.VisitLiteral(node);

            // Determine the quote style from the original source
            var originalText = _source[node.Range.Start..node.Range.End];
            var quoteChar = originalText.Length > 0 ? originalText[0] : '\'';
            if (quoteChar != '\'' && quoteChar != '"') quoteChar = '\'';

            // Check if string looks like SQL - delegate to SQL processor
            if (LooksLikeSql(strValue))
            {
                var sqlProcessor = _context.ProcessorRegistry?.GetProcessor("temp.sql", strValue);
                if (sqlProcessor != null)
                {
                    var sqlResult = sqlProcessor.Obfuscate(strValue, _context, _filePath);
                    if (sqlResult.WasTransformed)
                    {
                        var escapedSql = EscapeStringForJs(sqlResult.Content, quoteChar);
                        var replacement = $"{quoteChar}{escapedSql}{quoteChar}";

                        Replacements.Add(new IdentifierReplacement(
                            node.Range.Start,
                            node.Range.End,
                            replacement));

                        SqlReplacementCount += sqlResult.ReplacementCount;

                        return base.VisitLiteral(node);
                    }
                }
            }

            // In delegation-only mode, skip non-SQL string literal obfuscation
            if (_delegationOnly)
            {
                return base.VisitLiteral(node);
            }

            var lineNumber = GetLineNumber(_source, node.Range.Start);
            var alias = _context.GetOrCreateAlias(
                strValue,
                SemanticCategory.StringLiteral,
                _filePath,
                lineNumber,
                node.Range.Start,
                node.Range.End);

            var aliasReplacement = $"{quoteChar}{alias}{quoteChar}";

            Replacements.Add(new IdentifierReplacement(
                node.Range.Start,
                node.Range.End,
                aliasReplacement));

            return base.VisitLiteral(node);
        }
    }

    /// <summary>
    /// Deobfuscation visitor: walks all identifiers and checks if they match an alias pattern,
    /// then looks up the original value in the context's reverse mappings.
    /// </summary>
    private sealed class DeobfuscationVisitor : AstVisitor
    {
        private readonly ObfuscationContext _context;

        public List<IdentifierReplacement> Replacements { get; } = [];

        public DeobfuscationVisitor(ObfuscationContext context)
        {
            _context = context;
        }

        protected override object? VisitIdentifier(Identifier node)
        {
            TryAddReverseReplacement(node.Name, node.Range.Start, node.Range.End);
            return base.VisitIdentifier(node);
        }

        protected override object? VisitMemberExpression(MemberExpression node)
        {
            Visit(node.Object);
            // Also check computed properties, and non-computed if they match alias pattern
            if (node.Property is Identifier propId && IsAliasPattern(propId.Name))
            {
                TryAddReverseReplacement(propId.Name, propId.Range.Start, propId.Range.End);
            }
            else if (node.Computed)
            {
                Visit(node.Property);
            }
            return node;
        }

        private void TryAddReverseReplacement(string name, int start, int end)
        {
            var original = _context.Mappings.GetOriginal(name);
            if (original is not null)
            {
                Replacements.Add(new IdentifierReplacement(start, end, original));
            }
        }
    }

    /// <summary>
    /// Deobfuscation visitor for string literals: looks for alias patterns in string values.
    /// </summary>
    private sealed class StringLiteralDeobfuscator : AstVisitor
    {
        private readonly ObfuscationContext _context;
        private readonly string _source;

        public List<IdentifierReplacement> Replacements { get; } = [];

        public StringLiteralDeobfuscator(ObfuscationContext context, string source)
        {
            _context = context;
            _source = source;
        }

        protected override object? VisitLiteral(Literal node)
        {
            if (node.Value is string strValue && IsAliasPattern(strValue))
            {
                var original = _context.Mappings.GetOriginal(strValue);
                if (original is not null)
                {
                    // Reconstruct with the same quote style
                    var originalText = _source[node.Range.Start..node.Range.End];
                    var quoteChar = originalText.Length > 0 ? originalText[0] : '\'';
                    if (quoteChar != '\'' && quoteChar != '"') quoteChar = '\'';

                    var replacement = $"{quoteChar}{EscapeStringForJs(original, quoteChar)}{quoteChar}";
                    Replacements.Add(new IdentifierReplacement(node.Range.Start, node.Range.End, replacement));
                }
            }

            return base.VisitLiteral(node);
        }
    }

    /// <summary>
    /// Escapes a string value for safe inclusion in a JS string literal.
    /// </summary>
    private static string EscapeStringForJs(string value, char quoteChar)
    {
        var sb = new StringBuilder(value.Length);
        foreach (var ch in value)
        {
            if (ch == quoteChar)
            {
                sb.Append('\\');
                sb.Append(ch);
            }
            else if (ch == '\\')
            {
                sb.Append("\\\\");
            }
            else if (ch == '\n')
            {
                sb.Append("\\n");
            }
            else if (ch == '\r')
            {
                sb.Append("\\r");
            }
            else if (ch == '\t')
            {
                sb.Append("\\t");
            }
            else
            {
                sb.Append(ch);
            }
        }
        return sb.ToString();
    }
}
