using System.Collections.Frozen;
using System.Text.RegularExpressions;
using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.VisualBasic;
using Microsoft.CodeAnalysis.VisualBasic.Syntax;

namespace CodeBleach.Processors.VisualBasic;

/// <summary>
/// Roslyn-powered VB.NET language processor for CodeBleach v2.0.
/// Uses a two-pass pipeline: discovery (VisualBasicSyntaxWalker) then rewrite (VisualBasicSyntaxRewriter).
/// Supports batch compilation across multiple files for accurate semantic analysis.
/// </summary>
public sealed class VisualBasicLanguageProcessor : ILanguageProcessor
{
    public string ProcessorId => "vbnet";
    public string DisplayName => "VB.NET (Roslyn)";
    public IReadOnlySet<string> SupportedExtensions { get; } =
        new HashSet<string>(StringComparer.OrdinalIgnoreCase) { ".vb" }.ToFrozenSet();
    public int Priority => 10;

    /// <summary>
    /// Regex pattern matching the alias format PREFIX_N produced by the NamingStrategy.
    /// Used during deobfuscation to identify tokens that need reverse-mapping.
    /// </summary>
    private static readonly Regex AliasPattern = new(@"^[A-Z]+_\d+$", RegexOptions.Compiled);

    /// <summary>
    /// The Roslyn compilation built during PrepareBatch, covering all files in the batch.
    /// Null if PrepareBatch was not called (single-file mode).
    /// </summary>
    private VisualBasicCompilation? _batchCompilation;

    /// <summary>
    /// Parsed syntax trees from PrepareBatch, keyed by file path for quick lookup.
    /// </summary>
    private Dictionary<string, SyntaxTree>? _batchSyntaxTrees;

    public bool CanProcess(string filePath, string content)
    {
        var ext = Path.GetExtension(filePath);
        return SupportedExtensions.Contains(ext);
    }

    /// <summary>
    /// Builds a VisualBasicCompilation from all files in the batch, enabling cross-file
    /// semantic analysis. This must be called before Obfuscate for accurate symbol resolution.
    /// </summary>
    public void PrepareBatch(IReadOnlyList<string> filePaths, ObfuscationContext context)
    {
        var syntaxTrees = new List<SyntaxTree>(filePaths.Count);
        var treeMap = new Dictionary<string, SyntaxTree>(filePaths.Count, StringComparer.OrdinalIgnoreCase);

        foreach (var filePath in filePaths)
        {
            var sourceText = File.ReadAllText(filePath);
            var tree = VisualBasicSyntaxTree.ParseText(sourceText, path: filePath);
            syntaxTrees.Add(tree);
            treeMap[filePath] = tree;
        }

        var refs = AppDomain.CurrentDomain.GetAssemblies()
            .Where(a => !a.IsDynamic && !string.IsNullOrEmpty(a.Location))
            .Select(a => MetadataReference.CreateFromFile(a.Location))
            .Cast<MetadataReference>()
            .ToList();

        _batchCompilation = VisualBasicCompilation.Create("Obfuscation", syntaxTrees, refs);
        _batchSyntaxTrees = treeMap;
    }

    public LanguageProcessingResult Obfuscate(string content, ObfuscationContext context, string? filePath = null)
    {
        var warnings = new List<string>();

        try
        {
            SyntaxTree tree;
            VisualBasicCompilation compilation;

            // Resolve the syntax tree and compilation, preferring the batch compilation if available.
            if (_batchCompilation != null && _batchSyntaxTrees != null &&
                filePath != null && _batchSyntaxTrees.TryGetValue(filePath, out var batchTree))
            {
                tree = batchTree;
                compilation = _batchCompilation;
            }
            else
            {
                // Single-file mode: create an ad-hoc compilation for this file alone.
                tree = VisualBasicSyntaxTree.ParseText(content, path: filePath ?? "anonymous.vb");

                var refs = AppDomain.CurrentDomain.GetAssemblies()
                    .Where(a => !a.IsDynamic && !string.IsNullOrEmpty(a.Location))
                    .Select(a => MetadataReference.CreateFromFile(a.Location))
                    .Cast<MetadataReference>()
                    .ToList();

                compilation = VisualBasicCompilation.Create("Obfuscation", new[] { tree }, refs);
            }

            var semanticModel = compilation.GetSemanticModel(tree);

            // -- Pass 1: Discovery ------------------------------------------------
            // Walk the entire syntax tree to register all user-defined identifiers
            // in the ObfuscationContext. This pass does NOT modify the tree.
            var discoveryWalker = new DiscoveryWalker(semanticModel, context, filePath);
            discoveryWalker.Visit(tree.GetRoot());

            // -- Pass 2: Rewrite --------------------------------------------------
            // Walk the tree again, replacing identifiers, comments, and string literals.
            var rewriter = new ObfuscationRewriter(semanticModel, context, filePath, warnings);
            var newRoot = rewriter.Visit(tree.GetRoot());

            var transformedContent = newRoot?.ToFullString() ?? content;
            var replacementCount = rewriter.ReplacementCount;

            if (filePath != null)
            {
                context.RecordFileProcessing(filePath, ProcessorId, replacementCount);
            }

            return new LanguageProcessingResult
            {
                Content = transformedContent,
                WasTransformed = replacementCount > 0,
                ReplacementCount = replacementCount,
                ProcessorId = ProcessorId,
                Warnings = warnings
            };
        }
        catch (Exception ex)
        {
            // Resilience: if Roslyn fails for any reason, return content unchanged with a warning.
            warnings.Add($"VB.NET processor failed, returning original content: {ex.Message}");
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
        var warnings = new List<string>();

        try
        {
            var tree = VisualBasicSyntaxTree.ParseText(content, path: filePath ?? "anonymous.vb");
            var root = tree.GetRoot();

            var deobfuscationRewriter = new DeobfuscationRewriter(context, warnings);
            var newRoot = deobfuscationRewriter.Visit(root);

            var transformedContent = newRoot?.ToFullString() ?? content;
            var replacementCount = deobfuscationRewriter.ReplacementCount;

            return new LanguageProcessingResult
            {
                Content = transformedContent,
                WasTransformed = replacementCount > 0,
                ReplacementCount = replacementCount,
                ProcessorId = ProcessorId,
                Warnings = warnings
            };
        }
        catch (Exception ex)
        {
            warnings.Add($"VB.NET deobfuscation failed, returning original content: {ex.Message}");
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
        try
        {
            var tree = VisualBasicSyntaxTree.ParseText(obfuscatedContent);
            var diagnostics = tree.GetDiagnostics()
                .Where(d => d.Severity == DiagnosticSeverity.Error)
                .ToList();

            if (diagnostics.Count == 0)
            {
                return ValidationResult.Valid();
            }

            var errors = diagnostics
                .Select(d => $"[{d.Location.GetLineSpan().StartLinePosition}] {d.GetMessage()}")
                .ToList();

            return ValidationResult.Invalid(errors);
        }
        catch (Exception ex)
        {
            return ValidationResult.Invalid(new[] { $"Failed to parse: {ex.Message}" });
        }
    }

    // =========================================================================
    // Shared helpers
    // =========================================================================

    /// <summary>
    /// VB.NET framework type names that must never be renamed.
    /// Includes intrinsic VB types and common BCL type aliases.
    /// </summary>
    private static readonly FrozenSet<string> VbFrameworkTypes = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "String", "Integer", "Boolean", "Long", "Short", "Double", "Single",
        "Decimal", "Date", "Object", "Byte", "Char", "SByte", "UShort",
        "UInteger", "ULong"
    }.ToFrozenSet(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// Predefined VB conversion functions that must be preserved.
    /// </summary>
    private static readonly FrozenSet<string> VbConversionFunctions = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "CStr", "CInt", "CLng", "CDbl", "CSng", "CDec", "CBool", "CDate",
        "CByte", "CChar", "CObj", "CShort", "CType", "DirectCast", "TryCast",
        "CUInt", "CULng", "CUShort", "CSByte"
    }.ToFrozenSet(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// Well-known VB.NET attributes that should be preserved.
    /// </summary>
    private static readonly FrozenSet<string> WellKnownAttributes = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "Serializable", "Obsolete", "Conditional", "AttributeUsage",
        "DllImport", "MarshalAs", "StructLayout", "FieldOffset",
        "ComVisible", "Guid", "CLSCompliant", "DebuggerDisplay",
        "DebuggerBrowsable", "DebuggerStepThrough", "DebuggerNonUserCode",
        "CompilerGenerated", "CallerMemberName", "CallerFilePath", "CallerLineNumber",
        "STAThread", "MTAThread", "ThreadStatic", "Flags",
        "DataContract", "DataMember", "ServiceContract", "OperationContract",
        "XmlRoot", "XmlElement", "XmlAttribute", "XmlIgnore",
        "JsonProperty", "JsonIgnore", "Required", "Key",
        "TestClass", "TestMethod", "Fact", "Theory",
        "HideModuleName", "DefaultMember", "Extension"
    }.ToFrozenSet(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// Determines whether a symbol is user-defined (not from the BCL/framework).
    /// Symbols without a containing assembly (e.g., locals, labels) are considered user-defined.
    /// Also filters out VB My namespace, conversion functions, and framework types.
    /// </summary>
    private static bool IsUserDefined(ISymbol? symbol)
    {
        if (symbol == null) return false;

        // Preserve VB intrinsic types (String, Integer, etc.) and well-known attributes
        if (symbol is INamedTypeSymbol namedType &&
            (VbFrameworkTypes.Contains(namedType.Name) || WellKnownAttributes.Contains(namedType.Name)))
            return false;

        // Preserve VB conversion functions (CStr, CInt, etc.)
        if (symbol is IMethodSymbol && VbConversionFunctions.Contains(symbol.Name))
            return false;

        // Preserve My namespace and its members
        if (IsMyNamespaceMember(symbol))
            return false;

        var asm = symbol.ContainingAssembly;
        if (asm == null) return true; // local symbol, no assembly

        var name = asm.Name;
        return !name.StartsWith("System", StringComparison.Ordinal) &&
               !name.StartsWith("Microsoft", StringComparison.Ordinal) &&
               !string.Equals(name, "mscorlib", StringComparison.Ordinal) &&
               !string.Equals(name, "netstandard", StringComparison.Ordinal);
    }

    /// <summary>
    /// Checks whether a symbol belongs to the VB My namespace hierarchy.
    /// </summary>
    private static bool IsMyNamespaceMember(ISymbol symbol)
    {
        var current = symbol;
        while (current != null)
        {
            if (current is INamespaceSymbol ns &&
                string.Equals(ns.Name, "My", StringComparison.OrdinalIgnoreCase))
            {
                return true;
            }
            current = current.ContainingSymbol;
        }
        return false;
    }

    /// <summary>
    /// Maps an ISymbol to the appropriate SemanticCategory for alias generation.
    /// Includes VB.NET-specific Module type mapping.
    /// </summary>
    private static SemanticCategory GetCategory(ISymbol symbol)
    {
        return symbol switch
        {
            INamespaceSymbol => SemanticCategory.Namespace,
            INamedTypeSymbol nts => nts.TypeKind switch
            {
                TypeKind.Interface => SemanticCategory.Interface,
                TypeKind.Enum => SemanticCategory.Enum,
                TypeKind.Delegate => SemanticCategory.Delegate,
                TypeKind.Module => SemanticCategory.Module,
                TypeKind.Struct when nts.IsRecord => SemanticCategory.Record,
                TypeKind.Class when nts.IsRecord => SemanticCategory.Record,
                _ => SemanticCategory.Class // class, struct
            },
            IMethodSymbol => SemanticCategory.Method,
            IPropertySymbol => SemanticCategory.Property,
            IFieldSymbol fs => fs.ContainingType?.TypeKind == TypeKind.Enum
                ? SemanticCategory.EnumMember
                : SemanticCategory.Field,
            ILocalSymbol => SemanticCategory.Variable,
            IParameterSymbol => SemanticCategory.Parameter,
            IEventSymbol => SemanticCategory.Event,
            ITypeParameterSymbol => SemanticCategory.TypeParameter,
            _ => SemanticCategory.Generic
        };
    }

    /// <summary>
    /// Extracts location information (line, column) from a SyntaxToken for source map tracking.
    /// </summary>
    private static (int line, int colStart, int colEnd) GetLocation(SyntaxToken token)
    {
        var span = token.GetLocation().GetLineSpan();
        return (
            span.StartLinePosition.Line + 1,
            span.StartLinePosition.Character,
            span.EndLinePosition.Character
        );
    }

    /// <summary>
    /// Returns true if the string consists entirely of numeric characters.
    /// </summary>
    private static bool IsNumeric(string value)
    {
        return decimal.TryParse(value, System.Globalization.NumberStyles.Any,
            System.Globalization.CultureInfo.InvariantCulture, out _);
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

    // =========================================================================
    // Pass 1: Discovery Walker
    // =========================================================================

    /// <summary>
    /// Walks the VB.NET syntax tree to discover all user-defined identifiers and register
    /// them in the ObfuscationContext. Does not modify the tree.
    /// VB.NET is case-insensitive; Roslyn provides the declared-case name from symbols,
    /// ensuring canonical registration regardless of casing at usage sites.
    /// </summary>
    private sealed class DiscoveryWalker : VisualBasicSyntaxWalker
    {
        private readonly SemanticModel _semanticModel;
        private readonly ObfuscationContext _context;
        private readonly string? _filePath;

        public DiscoveryWalker(SemanticModel semanticModel, ObfuscationContext context, string? filePath)
            : base(SyntaxWalkerDepth.Trivia)
        {
            _semanticModel = semanticModel;
            _context = context;
            _filePath = filePath;
        }

        // -- Type declarations ------------------------------------------------

        public override void VisitClassBlock(ClassBlockSyntax node)
        {
            RegisterDeclaredSymbol(node, node.ClassStatement.Identifier);
            base.VisitClassBlock(node);
        }

        public override void VisitModuleBlock(ModuleBlockSyntax node)
        {
            RegisterDeclaredSymbol(node, node.ModuleStatement.Identifier);
            base.VisitModuleBlock(node);
        }

        public override void VisitStructureBlock(StructureBlockSyntax node)
        {
            RegisterDeclaredSymbol(node, node.StructureStatement.Identifier);
            base.VisitStructureBlock(node);
        }

        public override void VisitInterfaceBlock(InterfaceBlockSyntax node)
        {
            RegisterDeclaredSymbol(node, node.InterfaceStatement.Identifier);
            base.VisitInterfaceBlock(node);
        }

        // -- Methods (Sub/Function) -------------------------------------------

        public override void VisitMethodBlock(MethodBlockSyntax node)
        {
            // MethodBlockSyntax handles Sub/Function; constructors are ConstructorBlockSyntax.
            RegisterDeclaredSymbol(node, node.SubOrFunctionStatement.Identifier);
            base.VisitMethodBlock(node);
        }

        // -- Properties -------------------------------------------------------

        public override void VisitPropertyBlock(PropertyBlockSyntax node)
        {
            RegisterDeclaredSymbol(node, node.PropertyStatement.Identifier);
            base.VisitPropertyBlock(node);
        }

        // -- Fields -----------------------------------------------------------

        public override void VisitFieldDeclaration(FieldDeclarationSyntax node)
        {
            foreach (var declarator in node.Declarators)
            {
                foreach (var name in declarator.Names)
                {
                    var symbol = _semanticModel.GetDeclaredSymbol(name);
                    if (symbol != null && IsUserDefined(symbol))
                    {
                        var category = GetCategory(symbol);
                        var (line, colStart, colEnd) = GetLocation(name.Identifier);
                        _context.GetOrCreateAlias(symbol.Name, category, _filePath, line, colStart, colEnd);
                    }
                }
            }
            base.VisitFieldDeclaration(node);
        }

        // -- Local variable declarations (Dim inside methods) -----------------

        public override void VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
        {
            foreach (var declarator in node.Declarators)
            {
                foreach (var name in declarator.Names)
                {
                    var symbol = _semanticModel.GetDeclaredSymbol(name);
                    if (symbol != null && IsUserDefined(symbol))
                    {
                        var (line, colStart, colEnd) = GetLocation(name.Identifier);
                        _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Variable, _filePath, line, colStart, colEnd);
                    }
                }
            }
            base.VisitLocalDeclarationStatement(node);
        }

        // -- Parameters -------------------------------------------------------

        public override void VisitParameter(ParameterSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var (line, colStart, colEnd) = GetLocation(node.Identifier.Identifier);
                _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Parameter, _filePath, line, colStart, colEnd);
            }
            base.VisitParameter(node);
        }

        // -- Enums ------------------------------------------------------------

        public override void VisitEnumBlock(EnumBlockSyntax node)
        {
            RegisterDeclaredSymbol(node, node.EnumStatement.Identifier);
            base.VisitEnumBlock(node);
        }

        public override void VisitEnumMemberDeclaration(EnumMemberDeclarationSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                _context.GetOrCreateAlias(symbol.Name, SemanticCategory.EnumMember, _filePath, line, colStart, colEnd);
            }
            base.VisitEnumMemberDeclaration(node);
        }

        // -- Events -----------------------------------------------------------

        public override void VisitEventStatement(EventStatementSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Event, _filePath, line, colStart, colEnd);
            }
            base.VisitEventStatement(node);
        }

        // -- Delegates --------------------------------------------------------

        public override void VisitDelegateStatement(DelegateStatementSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Delegate, _filePath, line, colStart, colEnd);
            }
            base.VisitDelegateStatement(node);
        }

        // -- Imports (namespace renaming) -------------------------------------

        public override void VisitImportsStatement(ImportsStatementSyntax node)
        {
            foreach (var clause in node.ImportsClauses)
            {
                if (clause is SimpleImportsClauseSyntax simpleClause)
                {
                    RegisterImportsName(simpleClause.Name);
                }
            }
            base.VisitImportsStatement(node);
        }

        /// <summary>
        /// Registers each component of a namespace in an Imports statement.
        /// Handles qualified names like Foo.Bar.Baz.
        /// </summary>
        private void RegisterImportsName(NameSyntax name)
        {
            if (name is QualifiedNameSyntax qualified)
            {
                RegisterImportsName(qualified.Left);
                RegisterImportsName(qualified.Right);
            }
            else if (name is IdentifierNameSyntax identifier)
            {
                var symbol = _semanticModel.GetSymbolInfo(identifier).Symbol;
                if (symbol != null && IsUserDefined(symbol))
                {
                    var category = GetCategory(symbol);
                    var (line, colStart, colEnd) = GetLocation(identifier.Identifier);
                    _context.GetOrCreateAlias(symbol.Name, category, _filePath, line, colStart, colEnd);
                }
            }
        }

        // -- Type parameters --------------------------------------------------

        public override void VisitTypeParameterList(TypeParameterListSyntax node)
        {
            foreach (var typeParam in node.Parameters)
            {
                var symbol = _semanticModel.GetDeclaredSymbol(typeParam);
                if (symbol != null && IsUserDefined(symbol))
                {
                    var (line, colStart, colEnd) = GetLocation(typeParam.Identifier);
                    _context.GetOrCreateAlias(symbol.Name, SemanticCategory.TypeParameter, _filePath, line, colStart, colEnd);
                }
            }
            base.VisitTypeParameterList(node);
        }

        // -- ForEach loop variable --------------------------------------------

        public override void VisitForEachBlock(ForEachBlockSyntax node)
        {
            var forEachStmt = node.ForEachStatement;
            if (forEachStmt.ControlVariable is IdentifierNameSyntax identName)
            {
                var symbol = _semanticModel.GetSymbolInfo(identName).Symbol;
                if (symbol is ILocalSymbol local && IsUserDefined(local))
                {
                    var (line, colStart, colEnd) = GetLocation(identName.Identifier);
                    _context.GetOrCreateAlias(local.Name, SemanticCategory.Variable, _filePath, line, colStart, colEnd);
                }
            }
            base.VisitForEachBlock(node);
        }

        // -- For loop variable ------------------------------------------------

        public override void VisitForBlock(ForBlockSyntax node)
        {
            var forStmt = node.ForStatement;
            if (forStmt.ControlVariable is IdentifierNameSyntax identName)
            {
                var symbol = _semanticModel.GetSymbolInfo(identName).Symbol;
                if (symbol is ILocalSymbol local && IsUserDefined(local))
                {
                    var (line, colStart, colEnd) = GetLocation(identName.Identifier);
                    _context.GetOrCreateAlias(local.Name, SemanticCategory.Variable, _filePath, line, colStart, colEnd);
                }
            }
            base.VisitForBlock(node);
        }

        // -- Catch variable ---------------------------------------------------

        public override void VisitCatchBlock(CatchBlockSyntax node)
        {
            if (node.CatchStatement.IdentifierName != null)
            {
                var identToken = node.CatchStatement.IdentifierName.Identifier;
                var symbol = _semanticModel.GetSymbolInfo(node.CatchStatement.IdentifierName).Symbol;
                // Catch variable might also come from GetDeclaredSymbol on the CatchStatement
                symbol ??= _semanticModel.GetDeclaredSymbol(node.CatchStatement);
                if (symbol != null && IsUserDefined(symbol))
                {
                    var (line, colStart, colEnd) = GetLocation(identToken);
                    _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Variable, _filePath, line, colStart, colEnd);
                }
            }
            base.VisitCatchBlock(node);
        }

        /// <summary>
        /// Registers a declared symbol from a syntax node using the provided identifier token.
        /// </summary>
        private void RegisterDeclaredSymbol(SyntaxNode node, SyntaxToken identifierToken)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol == null || !IsUserDefined(symbol)) return;

            var category = GetCategory(symbol);
            var (line, colStart, colEnd) = GetLocation(identifierToken);
            _context.GetOrCreateAlias(symbol.Name, category, _filePath, line, colStart, colEnd);
        }
    }

    // =========================================================================
    // Pass 2: Obfuscation Rewriter
    // =========================================================================

    /// <summary>
    /// Rewrites the VB.NET syntax tree by replacing user-defined identifiers with their aliases,
    /// replacing comment content, and obfuscating string literal content.
    /// </summary>
    private sealed class ObfuscationRewriter : VisualBasicSyntaxRewriter
    {
        private readonly SemanticModel _semanticModel;
        private readonly ObfuscationContext _context;
        private readonly string? _filePath;
        private readonly List<string> _warnings;

        /// <summary>Total number of replacements made by this rewriter.</summary>
        public int ReplacementCount { get; private set; }

        public ObfuscationRewriter(
            SemanticModel semanticModel,
            ObfuscationContext context,
            string? filePath,
            List<string> warnings)
        {
            _semanticModel = semanticModel;
            _context = context;
            _filePath = filePath;
            _warnings = warnings;
        }

        // -- Identifier rewriting (main replacement point) --------------------

        public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node)
        {
            // Preserve XML literal element/attribute names
            if (IsInsideXmlLiteral(node))
            {
                return base.VisitIdentifierName(node);
            }

            var symbolInfo = _semanticModel.GetSymbolInfo(node);
            var symbol = symbolInfo.Symbol;

            // If primary resolution failed, try declared symbol (defensive).
            symbol ??= _semanticModel.GetDeclaredSymbol(node);

            if (symbol != null && IsUserDefined(symbol))
            {
                var category = GetCategory(symbol);
                // VB.NET is case-insensitive: use symbol.Name (declared case) for canonical registration.
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, category, _filePath, line, colStart, colEnd);

                if (!string.Equals(alias, node.Identifier.ValueText, StringComparison.Ordinal))
                {
                    ReplacementCount++;
                    return node.WithIdentifier(
                        SyntaxFactory.Identifier(node.Identifier.LeadingTrivia, alias, node.Identifier.TrailingTrivia));
                }
            }

            return base.VisitIdentifierName(node);
        }

        public override SyntaxNode? VisitGenericName(GenericNameSyntax node)
        {
            var symbolInfo = _semanticModel.GetSymbolInfo(node);
            var symbol = symbolInfo.Symbol;

            if (symbol != null && IsUserDefined(symbol))
            {
                var category = GetCategory(symbol);
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, category, _filePath, line, colStart, colEnd);

                if (!string.Equals(alias, node.Identifier.ValueText, StringComparison.Ordinal))
                {
                    ReplacementCount++;
                    var newNode = node.WithIdentifier(
                        SyntaxFactory.Identifier(node.Identifier.LeadingTrivia, alias, node.Identifier.TrailingTrivia));

                    // Continue visiting the type argument list so nested types get rewritten.
                    return base.VisitGenericName(newNode);
                }
            }

            return base.VisitGenericName(node);
        }

        // -- Declaration identifier rewriting ---------------------------------
        // In VB.NET, the identifier token on declarations lives on statement nodes
        // (ClassStatement, ModuleStatement, etc.) embedded within block nodes.

        public override SyntaxNode? VisitClassBlock(ClassBlockSyntax node)
        {
            var newStatement = RenameStatementIdentifier(
                node, node.ClassStatement, node.ClassStatement.Identifier,
                (stmt, id) => ((ClassStatementSyntax)stmt).WithIdentifier(id));

            if (newStatement != null)
            {
                node = node.WithClassStatement((ClassStatementSyntax)newStatement);
            }
            return base.VisitClassBlock(node);
        }

        public override SyntaxNode? VisitModuleBlock(ModuleBlockSyntax node)
        {
            var newStatement = RenameStatementIdentifier(
                node, node.ModuleStatement, node.ModuleStatement.Identifier,
                (stmt, id) => ((ModuleStatementSyntax)stmt).WithIdentifier(id));

            if (newStatement != null)
            {
                node = node.WithModuleStatement((ModuleStatementSyntax)newStatement);
            }
            return base.VisitModuleBlock(node);
        }

        public override SyntaxNode? VisitStructureBlock(StructureBlockSyntax node)
        {
            var newStatement = RenameStatementIdentifier(
                node, node.StructureStatement, node.StructureStatement.Identifier,
                (stmt, id) => ((StructureStatementSyntax)stmt).WithIdentifier(id));

            if (newStatement != null)
            {
                node = node.WithStructureStatement((StructureStatementSyntax)newStatement);
            }
            return base.VisitStructureBlock(node);
        }

        public override SyntaxNode? VisitInterfaceBlock(InterfaceBlockSyntax node)
        {
            var newStatement = RenameStatementIdentifier(
                node, node.InterfaceStatement, node.InterfaceStatement.Identifier,
                (stmt, id) => ((InterfaceStatementSyntax)stmt).WithIdentifier(id));

            if (newStatement != null)
            {
                node = node.WithInterfaceStatement((InterfaceStatementSyntax)newStatement);
            }
            return base.VisitInterfaceBlock(node);
        }

        public override SyntaxNode? VisitEnumBlock(EnumBlockSyntax node)
        {
            var newStatement = RenameStatementIdentifier(
                node, node.EnumStatement, node.EnumStatement.Identifier,
                (stmt, id) => ((EnumStatementSyntax)stmt).WithIdentifier(id));

            if (newStatement != null)
            {
                node = node.WithEnumStatement((EnumStatementSyntax)newStatement);
            }
            return base.VisitEnumBlock(node);
        }

        public override SyntaxNode? VisitEnumMemberDeclaration(EnumMemberDeclarationSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, SemanticCategory.EnumMember, _filePath, line, colStart, colEnd);

                if (!string.Equals(alias, node.Identifier.ValueText, StringComparison.Ordinal))
                {
                    ReplacementCount++;
                    node = node.WithIdentifier(
                        SyntaxFactory.Identifier(node.Identifier.LeadingTrivia, alias, node.Identifier.TrailingTrivia));
                }
            }
            return base.VisitEnumMemberDeclaration(node);
        }

        public override SyntaxNode? VisitMethodBlock(MethodBlockSyntax node)
        {
            // MethodBlockSyntax handles Sub/Function; constructors are ConstructorBlockSyntax.
            var methodStatement = node.SubOrFunctionStatement;

            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var category = GetCategory(symbol);
                var (line, colStart, colEnd) = GetLocation(methodStatement.Identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, category, _filePath, line, colStart, colEnd);

                if (!string.Equals(alias, methodStatement.Identifier.ValueText, StringComparison.Ordinal))
                {
                    ReplacementCount++;
                    var newMethodStatement = methodStatement.WithIdentifier(
                        SyntaxFactory.Identifier(methodStatement.Identifier.LeadingTrivia, alias, methodStatement.Identifier.TrailingTrivia));
                    node = node.WithSubOrFunctionStatement(newMethodStatement);
                }
            }
            return base.VisitMethodBlock(node);
        }

        public override SyntaxNode? VisitPropertyBlock(PropertyBlockSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var (line, colStart, colEnd) = GetLocation(node.PropertyStatement.Identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Property, _filePath, line, colStart, colEnd);

                if (!string.Equals(alias, node.PropertyStatement.Identifier.ValueText, StringComparison.Ordinal))
                {
                    ReplacementCount++;
                    var newPropStatement = node.PropertyStatement.WithIdentifier(
                        SyntaxFactory.Identifier(
                            node.PropertyStatement.Identifier.LeadingTrivia,
                            alias,
                            node.PropertyStatement.Identifier.TrailingTrivia));
                    node = node.WithPropertyStatement(newPropStatement);
                }
            }
            return base.VisitPropertyBlock(node);
        }

        public override SyntaxNode? VisitEventStatement(EventStatementSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Event, _filePath, line, colStart, colEnd);

                if (!string.Equals(alias, node.Identifier.ValueText, StringComparison.Ordinal))
                {
                    ReplacementCount++;
                    node = node.WithIdentifier(
                        SyntaxFactory.Identifier(node.Identifier.LeadingTrivia, alias, node.Identifier.TrailingTrivia));
                }
            }
            return base.VisitEventStatement(node);
        }

        public override SyntaxNode? VisitDelegateStatement(DelegateStatementSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Delegate, _filePath, line, colStart, colEnd);

                if (!string.Equals(alias, node.Identifier.ValueText, StringComparison.Ordinal))
                {
                    ReplacementCount++;
                    node = node.WithIdentifier(
                        SyntaxFactory.Identifier(node.Identifier.LeadingTrivia, alias, node.Identifier.TrailingTrivia));
                }
            }
            return base.VisitDelegateStatement(node);
        }

        // -- Field declarations -----------------------------------------------

        public override SyntaxNode? VisitFieldDeclaration(FieldDeclarationSyntax node)
        {
            var newDeclarators = new List<VariableDeclaratorSyntax>();
            var modified = false;

            foreach (var declarator in node.Declarators)
            {
                var newNames = new List<ModifiedIdentifierSyntax>();
                var namesModified = false;

                foreach (var name in declarator.Names)
                {
                    var symbol = _semanticModel.GetDeclaredSymbol(name);
                    if (symbol != null && IsUserDefined(symbol))
                    {
                        var category = GetCategory(symbol);
                        var (line, colStart, colEnd) = GetLocation(name.Identifier);
                        var alias = _context.GetOrCreateAlias(symbol.Name, category, _filePath, line, colStart, colEnd);

                        if (!string.Equals(alias, name.Identifier.ValueText, StringComparison.Ordinal))
                        {
                            ReplacementCount++;
                            namesModified = true;
                            newNames.Add(name.WithIdentifier(
                                SyntaxFactory.Identifier(name.Identifier.LeadingTrivia, alias, name.Identifier.TrailingTrivia)));
                            continue;
                        }
                    }
                    newNames.Add(name);
                }

                if (namesModified)
                {
                    modified = true;
                    newDeclarators.Add(declarator.WithNames(SyntaxFactory.SeparatedList(newNames)));
                }
                else
                {
                    newDeclarators.Add(declarator);
                }
            }

            if (modified)
            {
                node = node.WithDeclarators(SyntaxFactory.SeparatedList(newDeclarators));
            }

            return base.VisitFieldDeclaration(node);
        }

        // -- Local variable declarations (Dim) --------------------------------

        public override SyntaxNode? VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
        {
            var newDeclarators = new List<VariableDeclaratorSyntax>();
            var modified = false;

            foreach (var declarator in node.Declarators)
            {
                var newNames = new List<ModifiedIdentifierSyntax>();
                var namesModified = false;

                foreach (var name in declarator.Names)
                {
                    var symbol = _semanticModel.GetDeclaredSymbol(name);
                    if (symbol != null && IsUserDefined(symbol))
                    {
                        var (line, colStart, colEnd) = GetLocation(name.Identifier);
                        var alias = _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Variable, _filePath, line, colStart, colEnd);

                        if (!string.Equals(alias, name.Identifier.ValueText, StringComparison.Ordinal))
                        {
                            ReplacementCount++;
                            namesModified = true;
                            newNames.Add(name.WithIdentifier(
                                SyntaxFactory.Identifier(name.Identifier.LeadingTrivia, alias, name.Identifier.TrailingTrivia)));
                            continue;
                        }
                    }
                    newNames.Add(name);
                }

                if (namesModified)
                {
                    modified = true;
                    newDeclarators.Add(declarator.WithNames(SyntaxFactory.SeparatedList(newNames)));
                }
                else
                {
                    newDeclarators.Add(declarator);
                }
            }

            if (modified)
            {
                node = node.WithDeclarators(SyntaxFactory.SeparatedList(newDeclarators));
            }

            return base.VisitLocalDeclarationStatement(node);
        }

        // -- Parameters -------------------------------------------------------

        public override SyntaxNode? VisitParameter(ParameterSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var (line, colStart, colEnd) = GetLocation(node.Identifier.Identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Parameter, _filePath, line, colStart, colEnd);

                if (!string.Equals(alias, node.Identifier.Identifier.ValueText, StringComparison.Ordinal))
                {
                    ReplacementCount++;
                    var newModifiedIdentifier = node.Identifier.WithIdentifier(
                        SyntaxFactory.Identifier(
                            node.Identifier.Identifier.LeadingTrivia,
                            alias,
                            node.Identifier.Identifier.TrailingTrivia));
                    node = node.WithIdentifier(newModifiedIdentifier);
                }
            }
            return base.VisitParameter(node);
        }

        // -- Type parameters --------------------------------------------------

        public override SyntaxNode? VisitTypeParameter(TypeParameterSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, SemanticCategory.TypeParameter, _filePath, line, colStart, colEnd);

                if (!string.Equals(alias, node.Identifier.ValueText, StringComparison.Ordinal))
                {
                    ReplacementCount++;
                    node = node.WithIdentifier(
                        SyntaxFactory.Identifier(node.Identifier.LeadingTrivia, alias, node.Identifier.TrailingTrivia));
                }
            }
            return base.VisitTypeParameter(node);
        }

        // -- Namespace rewriting (Imports statements) -------------------------

        public override SyntaxNode? VisitImportsStatement(ImportsStatementSyntax node)
        {
            var newClauses = new List<ImportsClauseSyntax>();
            var modified = false;

            foreach (var clause in node.ImportsClauses)
            {
                if (clause is SimpleImportsClauseSyntax simpleClause)
                {
                    var newName = RewriteNamespaceName(simpleClause.Name);
                    if (newName != simpleClause.Name)
                    {
                        modified = true;
                        newClauses.Add(simpleClause.WithName(newName));
                        continue;
                    }
                }
                newClauses.Add(clause);
            }

            if (modified)
            {
                node = node.WithImportsClauses(SyntaxFactory.SeparatedList(newClauses));
            }

            return base.VisitImportsStatement(node);
        }

        /// <summary>
        /// Rewrites namespace name components (qualified names like Foo.Bar.Baz).
        /// Each component is rewritten individually via GetOrCreateAlias.
        /// </summary>
        private NameSyntax RewriteNamespaceName(NameSyntax name)
        {
            if (name is QualifiedNameSyntax qualified)
            {
                var newLeft = RewriteNamespaceName(qualified.Left);
                var newRight = RewriteNamespaceName(qualified.Right);

                if (newLeft != qualified.Left || newRight != qualified.Right)
                {
                    return qualified.WithLeft(newLeft).WithRight((SimpleNameSyntax)newRight);
                }
                return name;
            }

            if (name is IdentifierNameSyntax identifier)
            {
                var symbol = _semanticModel.GetSymbolInfo(identifier).Symbol;
                if (symbol != null && IsUserDefined(symbol))
                {
                    var category = GetCategory(symbol);
                    var (line, colStart, colEnd) = GetLocation(identifier.Identifier);
                    var alias = _context.GetOrCreateAlias(symbol.Name, category, _filePath, line, colStart, colEnd);

                    if (!string.Equals(alias, identifier.Identifier.ValueText, StringComparison.Ordinal))
                    {
                        ReplacementCount++;
                        return identifier.WithIdentifier(
                            SyntaxFactory.Identifier(identifier.Identifier.LeadingTrivia, alias, identifier.Identifier.TrailingTrivia));
                    }
                }
            }

            return name;
        }

        // -- String literal rewriting -----------------------------------------

        public override SyntaxNode? VisitLiteralExpression(LiteralExpressionSyntax node)
        {
            if (node.IsKind(SyntaxKind.StringLiteralExpression))
            {
                var text = node.Token.ValueText;

                // Preserve empty strings and purely numeric strings.
                if (string.IsNullOrEmpty(text) || IsNumeric(text))
                {
                    return base.VisitLiteralExpression(node);
                }

                // Check if string looks like SQL - delegate to SQL processor
                if (LooksLikeSql(text))
                {
                    var sqlProcessor = _context.ProcessorRegistry?.GetProcessor("temp.sql", text);
                    if (sqlProcessor != null)
                    {
                        var sqlResult = sqlProcessor.Obfuscate(text, _context, _filePath);
                        if (sqlResult.WasTransformed)
                        {
                            ReplacementCount += sqlResult.ReplacementCount;

                            var obfuscatedSql = sqlResult.Content;
                            var escapedSql = obfuscatedSql.Replace("\"", "\"\"");
                            var tokenText = "\"" + escapedSql + "\"";

                            var newToken = SyntaxFactory.StringLiteralToken(
                                node.Token.LeadingTrivia,
                                tokenText,
                                obfuscatedSql,
                                node.Token.TrailingTrivia);

                            return node.WithToken(newToken);
                        }
                    }
                }

                var alias = _context.GetOrCreateAlias(text, SemanticCategory.StringLiteral, _filePath);

                if (!string.Equals(alias, text, StringComparison.Ordinal))
                {
                    ReplacementCount++;

                    // VB.NET string literals: escape internal double quotes by doubling them.
                    var escapedAlias = alias.Replace("\"", "\"\"");
                    var tokenText2 = "\"" + escapedAlias + "\"";

                    var newToken = SyntaxFactory.StringLiteralToken(
                        node.Token.LeadingTrivia,
                        tokenText2,
                        alias,
                        node.Token.TrailingTrivia);

                    return node.WithToken(newToken);
                }
            }

            return base.VisitLiteralExpression(node);
        }

        public override SyntaxNode? VisitInterpolatedStringText(InterpolatedStringTextSyntax node)
        {
            var text = node.TextToken.ValueText;

            if (!string.IsNullOrEmpty(text) && !IsNumeric(text))
            {
                // Check if string looks like SQL - delegate to SQL processor
                if (LooksLikeSql(text))
                {
                    var sqlProcessor = _context.ProcessorRegistry?.GetProcessor("temp.sql", text);
                    if (sqlProcessor != null)
                    {
                        var sqlResult = sqlProcessor.Obfuscate(text, _context, _filePath);
                        if (sqlResult.WasTransformed)
                        {
                            ReplacementCount += sqlResult.ReplacementCount;

                            var newToken = SyntaxFactory.InterpolatedStringTextToken(
                                node.TextToken.LeadingTrivia,
                                sqlResult.Content,
                                sqlResult.Content,
                                node.TextToken.TrailingTrivia);

                            return node.WithTextToken(newToken);
                        }
                    }
                }

                var alias = _context.GetOrCreateAlias(text, SemanticCategory.StringLiteral, _filePath);

                if (!string.Equals(alias, text, StringComparison.Ordinal))
                {
                    ReplacementCount++;

                    // Create replacement interpolated string text token with new alias text.
                    var newToken = SyntaxFactory.InterpolatedStringTextToken(
                        node.TextToken.LeadingTrivia,
                        alias,
                        alias,
                        node.TextToken.TrailingTrivia);

                    return node.WithTextToken(newToken);
                }
            }

            return base.VisitInterpolatedStringText(node);
        }

        // -- Comment rewriting via trivia -------------------------------------

        public override SyntaxToken VisitToken(SyntaxToken token)
        {
            var newToken = token;

            if (token.HasLeadingTrivia)
            {
                var newLeadingTrivia = RewriteTriviaList(token.LeadingTrivia);
                newToken = newToken.WithLeadingTrivia(newLeadingTrivia);
            }

            if (token.HasTrailingTrivia)
            {
                var newTrailingTrivia = RewriteTriviaList(token.TrailingTrivia);
                newToken = newToken.WithTrailingTrivia(newTrailingTrivia);
            }

            return base.VisitToken(newToken);
        }

        /// <summary>
        /// Rewrites a trivia list, replacing comment content with placeholder text.
        /// VB.NET comments use ' (apostrophe) or REM keyword. Both are represented
        /// as CommentTrivia in the Roslyn VB model.
        /// </summary>
        private SyntaxTriviaList RewriteTriviaList(SyntaxTriviaList triviaList)
        {
            var modified = false;
            var newTrivia = new List<SyntaxTrivia>(triviaList.Count);

            foreach (var trivia in triviaList)
            {
                if (trivia.IsKind(SyntaxKind.CommentTrivia))
                {
                    var triviaText = trivia.ToFullString();

                    // Detect REM-style comments vs ' style
                    if (triviaText.TrimStart().StartsWith("REM", StringComparison.OrdinalIgnoreCase))
                    {
                        ReplacementCount++;
                        modified = true;
                        newTrivia.Add(SyntaxFactory.CommentTrivia("' [Comment removed]"));
                    }
                    else
                    {
                        ReplacementCount++;
                        modified = true;
                        newTrivia.Add(SyntaxFactory.CommentTrivia("' [Comment removed]"));
                    }
                }
                else if (trivia.IsKind(SyntaxKind.DocumentationCommentTrivia))
                {
                    ReplacementCount++;
                    modified = true;
                    newTrivia.Add(SyntaxFactory.CommentTrivia("' [Comment removed]"));
                }
                else
                {
                    newTrivia.Add(trivia);
                }
            }

            return modified ? SyntaxFactory.TriviaList(newTrivia) : triviaList;
        }

        // -- Helpers ----------------------------------------------------------

        /// <summary>
        /// Renames the identifier on a statement node within a block declaration.
        /// Returns the new statement node, or null if no rename occurred.
        /// </summary>
        private SyntaxNode? RenameStatementIdentifier(
            SyntaxNode blockNode,
            SyntaxNode statementNode,
            SyntaxToken identifier,
            Func<SyntaxNode, SyntaxToken, SyntaxNode> withIdentifier)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(blockNode);
            if (symbol != null && IsUserDefined(symbol))
            {
                var category = GetCategory(symbol);
                var (line, colStart, colEnd) = GetLocation(identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, category, _filePath, line, colStart, colEnd);

                if (!string.Equals(alias, identifier.ValueText, StringComparison.Ordinal))
                {
                    ReplacementCount++;
                    var newIdentifier = SyntaxFactory.Identifier(identifier.LeadingTrivia, alias, identifier.TrailingTrivia);
                    return withIdentifier(statementNode, newIdentifier);
                }
            }
            return null;
        }

        /// <summary>
        /// Determines whether a node is inside an XML literal expression.
        /// VB.NET supports inline XML; element/attribute names should be preserved.
        /// </summary>
        private static bool IsInsideXmlLiteral(SyntaxNode node)
        {
            var current = node.Parent;
            while (current != null)
            {
                if (current is XmlElementSyntax ||
                    current is XmlEmptyElementSyntax ||
                    current is XmlAttributeSyntax ||
                    current is XmlMemberAccessExpressionSyntax)
                {
                    // Check if this identifier is the element/attribute name, not an embedded expression
                    if (current is XmlMemberAccessExpressionSyntax)
                    {
                        return true;
                    }
                    if (node.Parent is XmlNameSyntax)
                    {
                        return true;
                    }
                }
                current = current.Parent;
            }
            return false;
        }
    }

    // =========================================================================
    // Deobfuscation Rewriter
    // =========================================================================

    /// <summary>
    /// Rewrites obfuscated VB.NET source back to original identifiers by reversing alias mappings.
    /// Does not require semantic analysis since alias tokens follow the PREFIX_N pattern.
    /// </summary>
    private sealed class DeobfuscationRewriter : VisualBasicSyntaxRewriter
    {
        private readonly ObfuscationContext _context;
        private readonly List<string> _warnings;

        public int ReplacementCount { get; private set; }

        public DeobfuscationRewriter(ObfuscationContext context, List<string> warnings)
        {
            _context = context;
            _warnings = warnings;
        }

        public override SyntaxToken VisitToken(SyntaxToken token)
        {
            var newToken = token;

            // Attempt to reverse-map identifier tokens.
            if (token.IsKind(SyntaxKind.IdentifierToken))
            {
                var text = token.ValueText;
                if (AliasPattern.IsMatch(text))
                {
                    var original = _context.Mappings.GetOriginal(text);
                    if (original != null)
                    {
                        ReplacementCount++;
                        newToken = SyntaxFactory.Identifier(token.LeadingTrivia, original, token.TrailingTrivia);
                    }
                }
            }

            // Attempt to reverse-map string literal tokens.
            if (token.IsKind(SyntaxKind.StringLiteralToken))
            {
                var text = token.ValueText;
                if (AliasPattern.IsMatch(text))
                {
                    var original = _context.Mappings.GetOriginal(text);
                    if (original != null)
                    {
                        ReplacementCount++;

                        // VB.NET string literals: escape double quotes by doubling.
                        var escapedOriginal = original.Replace("\"", "\"\"");
                        var tokenText = "\"" + escapedOriginal + "\"";

                        newToken = SyntaxFactory.StringLiteralToken(
                            token.LeadingTrivia,
                            tokenText,
                            original,
                            token.TrailingTrivia);
                    }
                }
            }

            // Attempt to reverse-map interpolated string text tokens.
            if (token.IsKind(SyntaxKind.InterpolatedStringTextToken))
            {
                var text = token.ValueText;
                if (AliasPattern.IsMatch(text))
                {
                    var original = _context.Mappings.GetOriginal(text);
                    if (original != null)
                    {
                        ReplacementCount++;
                        newToken = SyntaxFactory.InterpolatedStringTextToken(
                            token.LeadingTrivia,
                            original,
                            original,
                            token.TrailingTrivia);
                    }
                }
            }

            return base.VisitToken(newToken);
        }
    }
}
