using System.Collections.Frozen;
using System.Text.RegularExpressions;
using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace CodeBleach.Processors.CSharp;

/// <summary>
/// Roslyn-powered C# language processor for CodeBleach v2.0.
/// Uses a two-pass pipeline: discovery (CSharpSyntaxWalker) then rewrite (CSharpSyntaxRewriter).
/// Supports batch compilation across multiple files for accurate semantic analysis.
/// </summary>
public sealed class CSharpLanguageProcessor : ILanguageProcessor
{
    public string ProcessorId => "csharp";
    public string DisplayName => "C# (Roslyn)";
    public IReadOnlySet<string> SupportedExtensions { get; } = new HashSet<string>(StringComparer.OrdinalIgnoreCase) { ".cs", ".csx" }.ToFrozenSet();
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
    private CSharpCompilation? _batchCompilation;

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
    /// Builds a CSharpCompilation from all files in the batch, enabling cross-file
    /// semantic analysis. This must be called before Obfuscate for accurate symbol resolution.
    /// </summary>
    public void PrepareBatch(IReadOnlyList<string> filePaths, ObfuscationContext context)
    {
        var syntaxTrees = new List<SyntaxTree>(filePaths.Count);
        var treeMap = new Dictionary<string, SyntaxTree>(filePaths.Count, StringComparer.OrdinalIgnoreCase);

        foreach (var filePath in filePaths)
        {
            var sourceText = File.ReadAllText(filePath);
            var tree = CSharpSyntaxTree.ParseText(sourceText, path: filePath);
            syntaxTrees.Add(tree);
            treeMap[filePath] = tree;
        }

        var refs = AppDomain.CurrentDomain.GetAssemblies()
            .Where(a => !a.IsDynamic && !string.IsNullOrEmpty(a.Location))
            .Select(a => MetadataReference.CreateFromFile(a.Location))
            .Cast<MetadataReference>()
            .ToList();

        _batchCompilation = CSharpCompilation.Create("Obfuscation", syntaxTrees, refs);
        _batchSyntaxTrees = treeMap;
    }

    public LanguageProcessingResult Obfuscate(string content, ObfuscationContext context, string? filePath = null)
    {
        var warnings = new List<string>();

        try
        {
            SyntaxTree tree;
            CSharpCompilation compilation;

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
                tree = CSharpSyntaxTree.ParseText(content, path: filePath ?? "anonymous.cs");

                var refs = AppDomain.CurrentDomain.GetAssemblies()
                    .Where(a => !a.IsDynamic && !string.IsNullOrEmpty(a.Location))
                    .Select(a => MetadataReference.CreateFromFile(a.Location))
                    .Cast<MetadataReference>()
                    .ToList();

                compilation = CSharpCompilation.Create("Obfuscation", new[] { tree }, refs);
            }

            var semanticModel = compilation.GetSemanticModel(tree);

            // ── Pass 1: Discovery ──────────────────────────────────────────
            // Walk the entire syntax tree to register all user-defined identifiers
            // in the ObfuscationContext. This pass does NOT modify the tree.
            var discoveryWalker = new DiscoveryWalker(semanticModel, context, filePath);
            discoveryWalker.Visit(tree.GetRoot());

            // ── Pass 2: Rewrite ────────────────────────────────────────────
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
            warnings.Add($"C# processor failed, returning original content: {ex.Message}");
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
            var tree = CSharpSyntaxTree.ParseText(content, path: filePath ?? "anonymous.cs");
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
            warnings.Add($"C# deobfuscation failed, returning original content: {ex.Message}");
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
            var tree = CSharpSyntaxTree.ParseText(obfuscatedContent);
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

    // ═══════════════════════════════════════════════════════════════════════
    // Shared helpers
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Determines whether a symbol is user-defined (not from the BCL/framework).
    /// Symbols without a containing assembly (e.g., locals, labels) are considered user-defined.
    /// </summary>
    private static bool IsUserDefined(ISymbol? symbol)
    {
        if (symbol == null) return false;

        var asm = symbol.ContainingAssembly;
        if (asm == null) return true; // local symbol, no assembly

        var name = asm.Name;
        return !name.StartsWith("System", StringComparison.Ordinal) &&
               !name.StartsWith("Microsoft", StringComparison.Ordinal) &&
               !string.Equals(name, "mscorlib", StringComparison.Ordinal) &&
               !string.Equals(name, "netstandard", StringComparison.Ordinal);
    }

    /// <summary>
    /// Maps an ISymbol to the appropriate SemanticCategory for alias generation.
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
                TypeKind.Struct when IsRecord(nts) => SemanticCategory.Record,
                TypeKind.Class when IsRecord(nts) => SemanticCategory.Record,
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
    /// Checks whether a named type symbol represents a record (class or struct).
    /// </summary>
    private static bool IsRecord(INamedTypeSymbol symbol)
    {
        return symbol.IsRecord;
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

    // ═══════════════════════════════════════════════════════════════════════
    // Pass 1: Discovery Walker
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Walks the syntax tree to discover all user-defined identifiers and register them
    /// in the ObfuscationContext. Does not modify the tree.
    /// </summary>
    private sealed class DiscoveryWalker : CSharpSyntaxWalker
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

        public override void VisitNamespaceDeclaration(NamespaceDeclarationSyntax node)
        {
            RegisterNamespaceName(node.Name);
            base.VisitNamespaceDeclaration(node);
        }

        public override void VisitFileScopedNamespaceDeclaration(FileScopedNamespaceDeclarationSyntax node)
        {
            RegisterNamespaceName(node.Name);
            base.VisitFileScopedNamespaceDeclaration(node);
        }

        /// <summary>
        /// Registers each component of a namespace name (e.g., "Foo.Bar.Baz" registers Foo, Bar, Baz).
        /// </summary>
        private void RegisterNamespaceName(NameSyntax name)
        {
            if (name is QualifiedNameSyntax qualified)
            {
                RegisterNamespaceName(qualified.Left);
                RegisterNamespaceName(qualified.Right);
            }
            else if (name is IdentifierNameSyntax identifier)
            {
                var symbol = _semanticModel.GetSymbolInfo(identifier).Symbol;
                if (symbol != null && IsUserDefined(symbol))
                {
                    var (line, colStart, colEnd) = GetLocation(identifier.Identifier);
                    _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Namespace, _filePath, line, colStart, colEnd);
                }
            }
        }

        public override void VisitClassDeclaration(ClassDeclarationSyntax node)
        {
            RegisterDeclaredSymbol(node);
            base.VisitClassDeclaration(node);
        }

        public override void VisitStructDeclaration(StructDeclarationSyntax node)
        {
            RegisterDeclaredSymbol(node);
            base.VisitStructDeclaration(node);
        }

        public override void VisitRecordDeclaration(RecordDeclarationSyntax node)
        {
            RegisterDeclaredSymbol(node);
            base.VisitRecordDeclaration(node);
        }

        public override void VisitInterfaceDeclaration(InterfaceDeclarationSyntax node)
        {
            RegisterDeclaredSymbol(node);
            base.VisitInterfaceDeclaration(node);
        }

        public override void VisitEnumDeclaration(EnumDeclarationSyntax node)
        {
            RegisterDeclaredSymbol(node);
            base.VisitEnumDeclaration(node);
        }

        public override void VisitEnumMemberDeclaration(EnumMemberDeclarationSyntax node)
        {
            RegisterDeclaredSymbol(node);
            base.VisitEnumMemberDeclaration(node);
        }

        public override void VisitDelegateDeclaration(DelegateDeclarationSyntax node)
        {
            RegisterDeclaredSymbol(node);
            base.VisitDelegateDeclaration(node);
        }

        public override void VisitMethodDeclaration(MethodDeclarationSyntax node)
        {
            RegisterDeclaredSymbol(node);
            base.VisitMethodDeclaration(node);
        }

        public override void VisitConstructorDeclaration(ConstructorDeclarationSyntax node)
        {
            // Constructors use the class name; they are not separately registered
            // since the class name alias will apply. We still walk children.
            base.VisitConstructorDeclaration(node);
        }

        public override void VisitPropertyDeclaration(PropertyDeclarationSyntax node)
        {
            RegisterDeclaredSymbol(node);
            base.VisitPropertyDeclaration(node);
        }

        public override void VisitFieldDeclaration(FieldDeclarationSyntax node)
        {
            // FieldDeclaration doesn't have a DeclaredSymbol directly; iterate over variable declarators.
            foreach (var variable in node.Declaration.Variables)
            {
                RegisterDeclaredSymbol(variable);
            }
            base.VisitFieldDeclaration(node);
        }

        public override void VisitEventFieldDeclaration(EventFieldDeclarationSyntax node)
        {
            foreach (var variable in node.Declaration.Variables)
            {
                RegisterDeclaredSymbol(variable);
            }
            base.VisitEventFieldDeclaration(node);
        }

        public override void VisitEventDeclaration(EventDeclarationSyntax node)
        {
            RegisterDeclaredSymbol(node);
            base.VisitEventDeclaration(node);
        }

        public override void VisitVariableDeclarator(VariableDeclaratorSyntax node)
        {
            // Only register local variables here; field variables are handled in VisitFieldDeclaration.
            // Check if the parent is a local declaration statement.
            if (node.Parent is VariableDeclarationSyntax varDecl &&
                varDecl.Parent is LocalDeclarationStatementSyntax)
            {
                RegisterDeclaredSymbol(node);
            }
            base.VisitVariableDeclarator(node);
        }

        public override void VisitSingleVariableDesignation(SingleVariableDesignationSyntax node)
        {
            // Handles deconstruction patterns, out var declarations, etc.
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var category = GetCategory(symbol);
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                _context.GetOrCreateAlias(symbol.Name, category, _filePath, line, colStart, colEnd);
            }
            base.VisitSingleVariableDesignation(node);
        }

        public override void VisitParameter(ParameterSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Parameter, _filePath, line, colStart, colEnd);
            }
            base.VisitParameter(node);
        }

        public override void VisitTypeParameter(TypeParameterSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                _context.GetOrCreateAlias(symbol.Name, SemanticCategory.TypeParameter, _filePath, line, colStart, colEnd);
            }
            base.VisitTypeParameter(node);
        }

        public override void VisitForEachStatement(ForEachStatementSyntax node)
        {
            // The iteration variable in foreach is declared here.
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Variable, _filePath, line, colStart, colEnd);
            }
            base.VisitForEachStatement(node);
        }

        public override void VisitCatchDeclaration(CatchDeclarationSyntax node)
        {
            if (node.Identifier != default)
            {
                var symbol = _semanticModel.GetDeclaredSymbol(node);
                if (symbol != null && IsUserDefined(symbol))
                {
                    var (line, colStart, colEnd) = GetLocation(node.Identifier);
                    _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Variable, _filePath, line, colStart, colEnd);
                }
            }
            base.VisitCatchDeclaration(node);
        }

        /// <summary>
        /// Registers a declared symbol from any syntax node that supports GetDeclaredSymbol.
        /// </summary>
        private void RegisterDeclaredSymbol(SyntaxNode node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol == null || !IsUserDefined(symbol)) return;

            var category = GetCategory(symbol);

            // Get the identifier token location for source mapping.
            var identifierToken = node switch
            {
                BaseTypeDeclarationSyntax typeDecl => typeDecl.Identifier,
                DelegateDeclarationSyntax delegateDecl => delegateDecl.Identifier,
                MethodDeclarationSyntax methodDecl => methodDecl.Identifier,
                PropertyDeclarationSyntax propDecl => propDecl.Identifier,
                EventDeclarationSyntax eventDecl => eventDecl.Identifier,
                VariableDeclaratorSyntax varDecl => varDecl.Identifier,
                EnumMemberDeclarationSyntax enumMember => enumMember.Identifier,
                _ => default
            };

            if (identifierToken != default)
            {
                var (line, colStart, colEnd) = GetLocation(identifierToken);
                _context.GetOrCreateAlias(symbol.Name, category, _filePath, line, colStart, colEnd);
            }
            else
            {
                _context.GetOrCreateAlias(symbol.Name, category, _filePath);
            }
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Pass 2: Obfuscation Rewriter
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Rewrites the syntax tree by replacing user-defined identifiers with their aliases,
    /// replacing comment content, and obfuscating string literal content.
    /// </summary>
    private sealed class ObfuscationRewriter : CSharpSyntaxRewriter
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
            : base(visitIntoStructuredTrivia: true)
        {
            _semanticModel = semanticModel;
            _context = context;
            _filePath = filePath;
            _warnings = warnings;
        }

        // ── Identifier rewriting ───────────────────────────────────────

        public override SyntaxNode? VisitIdentifierName(IdentifierNameSyntax node)
        {
            var symbolInfo = _semanticModel.GetSymbolInfo(node);
            var symbol = symbolInfo.Symbol;

            // If the primary resolution failed, try declared symbol (rare for IdentifierName but defensive).
            symbol ??= _semanticModel.GetDeclaredSymbol(node);

            if (symbol != null && IsUserDefined(symbol))
            {
                var category = GetCategory(symbol);
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, category, _filePath, line, colStart, colEnd);

                if (alias != node.Identifier.ValueText)
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

                if (alias != node.Identifier.ValueText)
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

        // ── Declaration identifiers ────────────────────────────────────
        // For declarations, the identifier token is on the declaration node itself,
        // not on an IdentifierNameSyntax child. We override specific Visit methods.

        public override SyntaxNode? VisitClassDeclaration(ClassDeclarationSyntax node)
        {
            node = (ClassDeclarationSyntax)RenameDeclarationIdentifier(node, node.Identifier,
                (n, id) => ((ClassDeclarationSyntax)n).WithIdentifier(id));
            return base.VisitClassDeclaration(node);
        }

        public override SyntaxNode? VisitStructDeclaration(StructDeclarationSyntax node)
        {
            node = (StructDeclarationSyntax)RenameDeclarationIdentifier(node, node.Identifier,
                (n, id) => ((StructDeclarationSyntax)n).WithIdentifier(id));
            return base.VisitStructDeclaration(node);
        }

        public override SyntaxNode? VisitRecordDeclaration(RecordDeclarationSyntax node)
        {
            node = (RecordDeclarationSyntax)RenameDeclarationIdentifier(node, node.Identifier,
                (n, id) => ((RecordDeclarationSyntax)n).WithIdentifier(id));
            return base.VisitRecordDeclaration(node);
        }

        public override SyntaxNode? VisitInterfaceDeclaration(InterfaceDeclarationSyntax node)
        {
            node = (InterfaceDeclarationSyntax)RenameDeclarationIdentifier(node, node.Identifier,
                (n, id) => ((InterfaceDeclarationSyntax)n).WithIdentifier(id));
            return base.VisitInterfaceDeclaration(node);
        }

        public override SyntaxNode? VisitEnumDeclaration(EnumDeclarationSyntax node)
        {
            node = (EnumDeclarationSyntax)RenameDeclarationIdentifier(node, node.Identifier,
                (n, id) => ((EnumDeclarationSyntax)n).WithIdentifier(id));
            return base.VisitEnumDeclaration(node);
        }

        public override SyntaxNode? VisitEnumMemberDeclaration(EnumMemberDeclarationSyntax node)
        {
            node = (EnumMemberDeclarationSyntax)RenameDeclarationIdentifier(node, node.Identifier,
                (n, id) => ((EnumMemberDeclarationSyntax)n).WithIdentifier(id));
            return base.VisitEnumMemberDeclaration(node);
        }

        public override SyntaxNode? VisitDelegateDeclaration(DelegateDeclarationSyntax node)
        {
            node = (DelegateDeclarationSyntax)RenameDeclarationIdentifier(node, node.Identifier,
                (n, id) => ((DelegateDeclarationSyntax)n).WithIdentifier(id));
            return base.VisitDelegateDeclaration(node);
        }

        public override SyntaxNode? VisitMethodDeclaration(MethodDeclarationSyntax node)
        {
            node = (MethodDeclarationSyntax)RenameDeclarationIdentifier(node, node.Identifier,
                (n, id) => ((MethodDeclarationSyntax)n).WithIdentifier(id));
            return base.VisitMethodDeclaration(node);
        }

        public override SyntaxNode? VisitConstructorDeclaration(ConstructorDeclarationSyntax node)
        {
            // Constructor identifier must match the (renamed) class name.
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol?.ContainingType != null && IsUserDefined(symbol.ContainingType))
            {
                var category = GetCategory(symbol.ContainingType);
                var alias = _context.GetOrCreateAlias(symbol.ContainingType.Name, category, _filePath);

                if (alias != node.Identifier.ValueText)
                {
                    ReplacementCount++;
                    node = node.WithIdentifier(
                        SyntaxFactory.Identifier(node.Identifier.LeadingTrivia, alias, node.Identifier.TrailingTrivia));
                }
            }
            return base.VisitConstructorDeclaration(node);
        }

        public override SyntaxNode? VisitDestructorDeclaration(DestructorDeclarationSyntax node)
        {
            // Destructor identifier must match the (renamed) class name.
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol?.ContainingType != null && IsUserDefined(symbol.ContainingType))
            {
                var category = GetCategory(symbol.ContainingType);
                var alias = _context.GetOrCreateAlias(symbol.ContainingType.Name, category, _filePath);

                if (alias != node.Identifier.ValueText)
                {
                    ReplacementCount++;
                    node = node.WithIdentifier(
                        SyntaxFactory.Identifier(node.Identifier.LeadingTrivia, alias, node.Identifier.TrailingTrivia));
                }
            }
            return base.VisitDestructorDeclaration(node);
        }

        public override SyntaxNode? VisitPropertyDeclaration(PropertyDeclarationSyntax node)
        {
            node = (PropertyDeclarationSyntax)RenameDeclarationIdentifier(node, node.Identifier,
                (n, id) => ((PropertyDeclarationSyntax)n).WithIdentifier(id));
            return base.VisitPropertyDeclaration(node);
        }

        public override SyntaxNode? VisitEventDeclaration(EventDeclarationSyntax node)
        {
            node = (EventDeclarationSyntax)RenameDeclarationIdentifier(node, node.Identifier,
                (n, id) => ((EventDeclarationSyntax)n).WithIdentifier(id));
            return base.VisitEventDeclaration(node);
        }

        public override SyntaxNode? VisitVariableDeclarator(VariableDeclaratorSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var category = GetCategory(symbol);
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, category, _filePath, line, colStart, colEnd);

                if (alias != node.Identifier.ValueText)
                {
                    ReplacementCount++;
                    node = node.WithIdentifier(
                        SyntaxFactory.Identifier(node.Identifier.LeadingTrivia, alias, node.Identifier.TrailingTrivia));
                }
            }
            return base.VisitVariableDeclarator(node);
        }

        public override SyntaxNode? VisitSingleVariableDesignation(SingleVariableDesignationSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var category = GetCategory(symbol);
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, category, _filePath, line, colStart, colEnd);

                if (alias != node.Identifier.ValueText)
                {
                    ReplacementCount++;
                    node = node.WithIdentifier(
                        SyntaxFactory.Identifier(node.Identifier.LeadingTrivia, alias, node.Identifier.TrailingTrivia));
                }
            }
            return base.VisitSingleVariableDesignation(node);
        }

        public override SyntaxNode? VisitParameter(ParameterSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Parameter, _filePath, line, colStart, colEnd);

                if (alias != node.Identifier.ValueText)
                {
                    ReplacementCount++;
                    node = node.WithIdentifier(
                        SyntaxFactory.Identifier(node.Identifier.LeadingTrivia, alias, node.Identifier.TrailingTrivia));
                }
            }
            return base.VisitParameter(node);
        }

        public override SyntaxNode? VisitTypeParameter(TypeParameterSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, SemanticCategory.TypeParameter, _filePath, line, colStart, colEnd);

                if (alias != node.Identifier.ValueText)
                {
                    ReplacementCount++;
                    node = node.WithIdentifier(
                        SyntaxFactory.Identifier(node.Identifier.LeadingTrivia, alias, node.Identifier.TrailingTrivia));
                }
            }
            return base.VisitTypeParameter(node);
        }

        public override SyntaxNode? VisitForEachStatement(ForEachStatementSyntax node)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var (line, colStart, colEnd) = GetLocation(node.Identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Variable, _filePath, line, colStart, colEnd);

                if (alias != node.Identifier.ValueText)
                {
                    ReplacementCount++;
                    node = node.WithIdentifier(
                        SyntaxFactory.Identifier(node.Identifier.LeadingTrivia, alias, node.Identifier.TrailingTrivia));
                }
            }
            return base.VisitForEachStatement(node);
        }

        public override SyntaxNode? VisitCatchDeclaration(CatchDeclarationSyntax node)
        {
            if (node.Identifier != default)
            {
                var symbol = _semanticModel.GetDeclaredSymbol(node);
                if (symbol != null && IsUserDefined(symbol))
                {
                    var (line, colStart, colEnd) = GetLocation(node.Identifier);
                    var alias = _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Variable, _filePath, line, colStart, colEnd);

                    if (alias != node.Identifier.ValueText)
                    {
                        ReplacementCount++;
                        node = node.WithIdentifier(
                            SyntaxFactory.Identifier(node.Identifier.LeadingTrivia, alias, node.Identifier.TrailingTrivia));
                    }
                }
            }
            return base.VisitCatchDeclaration(node);
        }

        // ── Namespace rewriting ────────────────────────────────────────

        public override SyntaxNode? VisitNamespaceDeclaration(NamespaceDeclarationSyntax node)
        {
            var newName = RewriteNamespaceName(node.Name);
            if (newName != node.Name)
            {
                node = node.WithName(newName);
            }
            return base.VisitNamespaceDeclaration(node);
        }

        public override SyntaxNode? VisitFileScopedNamespaceDeclaration(FileScopedNamespaceDeclarationSyntax node)
        {
            var newName = RewriteNamespaceName(node.Name);
            if (newName != node.Name)
            {
                node = node.WithName(newName);
            }
            return base.VisitFileScopedNamespaceDeclaration(node);
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
                    var (line, colStart, colEnd) = GetLocation(identifier.Identifier);
                    var alias = _context.GetOrCreateAlias(symbol.Name, SemanticCategory.Namespace, _filePath, line, colStart, colEnd);

                    if (alias != identifier.Identifier.ValueText)
                    {
                        ReplacementCount++;
                        return identifier.WithIdentifier(
                            SyntaxFactory.Identifier(identifier.Identifier.LeadingTrivia, alias, identifier.Identifier.TrailingTrivia));
                    }
                }
            }

            return name;
        }

        // ── String literal rewriting ───────────────────────────────────

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
                            var tokenText = node.Token.Text;
                            string newTokenText;

                            if (tokenText.StartsWith("@\"", StringComparison.Ordinal))
                            {
                                newTokenText = "@\"" + obfuscatedSql.Replace("\"", "\"\"") + "\"";
                            }
                            else
                            {
                                newTokenText = "\"" + obfuscatedSql.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\"";
                            }

                            var newToken = SyntaxFactory.Literal(
                                node.Token.LeadingTrivia,
                                newTokenText,
                                obfuscatedSql,
                                node.Token.TrailingTrivia);

                            return node.WithToken(newToken);
                        }
                    }
                }

                var alias = _context.GetOrCreateAlias(text, SemanticCategory.StringLiteral, _filePath);

                if (alias != text)
                {
                    ReplacementCount++;

                    // Determine if the original token used verbatim string syntax.
                    var tokenText2 = node.Token.Text;
                    string newTokenText2;

                    if (tokenText2.StartsWith("@\"", StringComparison.Ordinal))
                    {
                        // Verbatim string: only need to escape double quotes by doubling them.
                        newTokenText2 = "@\"" + alias.Replace("\"", "\"\"") + "\"";
                    }
                    else
                    {
                        // Regular string literal.
                        newTokenText2 = "\"" + alias.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\"";
                    }

                    var newToken2 = SyntaxFactory.Literal(
                        node.Token.LeadingTrivia,
                        newTokenText2,
                        alias,
                        node.Token.TrailingTrivia);

                    return node.WithToken(newToken2);
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
                            var newToken = SyntaxFactory.Token(
                                node.TextToken.LeadingTrivia,
                                SyntaxKind.InterpolatedStringTextToken,
                                sqlResult.Content,
                                sqlResult.Content,
                                node.TextToken.TrailingTrivia);

                            return node.WithTextToken(newToken);
                        }
                    }
                }

                var alias = _context.GetOrCreateAlias(text, SemanticCategory.StringLiteral, _filePath);

                if (alias != text)
                {
                    ReplacementCount++;
                    var newToken = SyntaxFactory.Token(
                        node.TextToken.LeadingTrivia,
                        SyntaxKind.InterpolatedStringTextToken,
                        alias,
                        alias,
                        node.TextToken.TrailingTrivia);

                    return node.WithTextToken(newToken);
                }
            }

            return base.VisitInterpolatedStringText(node);
        }

        // ── Comment rewriting via trivia ───────────────────────────────

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
        /// </summary>
        private SyntaxTriviaList RewriteTriviaList(SyntaxTriviaList triviaList)
        {
            var modified = false;
            var newTrivia = new List<SyntaxTrivia>(triviaList.Count);

            foreach (var trivia in triviaList)
            {
                if (trivia.IsKind(SyntaxKind.SingleLineCommentTrivia))
                {
                    ReplacementCount++;
                    modified = true;
                    newTrivia.Add(SyntaxFactory.Comment("// [Comment removed]"));
                }
                else if (trivia.IsKind(SyntaxKind.MultiLineCommentTrivia))
                {
                    ReplacementCount++;
                    modified = true;
                    newTrivia.Add(SyntaxFactory.Comment("/* [Comment removed] */"));
                }
                else if (trivia.IsKind(SyntaxKind.SingleLineDocumentationCommentTrivia) ||
                         trivia.IsKind(SyntaxKind.MultiLineDocumentationCommentTrivia))
                {
                    ReplacementCount++;
                    modified = true;
                    newTrivia.Add(SyntaxFactory.Comment("// [Comment removed]"));
                }
                else
                {
                    newTrivia.Add(trivia);
                }
            }

            return modified ? SyntaxFactory.TriviaList(newTrivia) : triviaList;
        }

        // ── Helpers ────────────────────────────────────────────────────

        /// <summary>
        /// Renames the identifier of a declaration node if the declared symbol is user-defined.
        /// Returns the (potentially modified) node.
        /// </summary>
        private SyntaxNode RenameDeclarationIdentifier(
            SyntaxNode node,
            SyntaxToken identifier,
            Func<SyntaxNode, SyntaxToken, SyntaxNode> withIdentifier)
        {
            var symbol = _semanticModel.GetDeclaredSymbol(node);
            if (symbol != null && IsUserDefined(symbol))
            {
                var category = GetCategory(symbol);
                var (line, colStart, colEnd) = GetLocation(identifier);
                var alias = _context.GetOrCreateAlias(symbol.Name, category, _filePath, line, colStart, colEnd);

                if (alias != identifier.ValueText)
                {
                    ReplacementCount++;
                    var newIdentifier = SyntaxFactory.Identifier(identifier.LeadingTrivia, alias, identifier.TrailingTrivia);
                    return withIdentifier(node, newIdentifier);
                }
            }
            return node;
        }

        /// <summary>
        /// Returns true if the string consists entirely of digits and optionally a single decimal point.
        /// </summary>
        private static bool IsNumeric(string value)
        {
            return decimal.TryParse(value, System.Globalization.NumberStyles.Any,
                System.Globalization.CultureInfo.InvariantCulture, out _);
        }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Deobfuscation Rewriter
    // ═══════════════════════════════════════════════════════════════════════

    /// <summary>
    /// Rewrites obfuscated C# source back to original identifiers by reversing alias mappings.
    /// Does not require semantic analysis since alias tokens follow the PREFIX_N pattern.
    /// </summary>
    private sealed class DeobfuscationRewriter : CSharpSyntaxRewriter
    {
        private readonly ObfuscationContext _context;
        private readonly List<string> _warnings;

        public int ReplacementCount { get; private set; }

        public DeobfuscationRewriter(ObfuscationContext context, List<string> warnings)
            : base(visitIntoStructuredTrivia: true)
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

                        var tokenText = token.Text;
                        string newTokenText;

                        if (tokenText.StartsWith("@\"", StringComparison.Ordinal))
                        {
                            newTokenText = "@\"" + original.Replace("\"", "\"\"") + "\"";
                        }
                        else
                        {
                            newTokenText = "\"" + original.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\"";
                        }

                        newToken = SyntaxFactory.Literal(token.LeadingTrivia, newTokenText, original, token.TrailingTrivia);
                    }
                }
            }

            // Reverse-map interpolated string text tokens.
            if (token.IsKind(SyntaxKind.InterpolatedStringTextToken))
            {
                var text = token.ValueText;
                if (AliasPattern.IsMatch(text))
                {
                    var original = _context.Mappings.GetOriginal(text);
                    if (original != null)
                    {
                        ReplacementCount++;
                        newToken = SyntaxFactory.Token(
                            token.LeadingTrivia,
                            SyntaxKind.InterpolatedStringTextToken,
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
