namespace CodeBleach.Processors.FSharp

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open CodeBleach.Core.Interfaces
open CodeBleach.Core.Models
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

// ═══════════════════════════════════════════════════════════════════════════════
// Types
// ═══════════════════════════════════════════════════════════════════════════════

/// A single replacement to apply to the source text. Offset and Length refer to
/// character positions in the original string; NewValue is the replacement text.
[<Struct>]
type internal Replacement =
    { Offset: int
      Length: int
      OriginalValue: string
      NewValue: string }

// ═══════════════════════════════════════════════════════════════════════════════
// Preserved-identifier sets
// ═══════════════════════════════════════════════════════════════════════════════

[<RequireQualifiedAccess>]
module internal Preserved =

    /// F# keywords that must never be renamed.
    let keywords =
        HashSet<string>(
            [| "let"; "match"; "if"; "then"; "else"; "elif"; "fun"; "function"
               "module"; "namespace"; "type"; "open"; "with"; "for"; "while"
               "do"; "done"; "in"; "to"; "downto"; "yield"; "return"; "and"
               "or"; "not"; "true"; "false"; "null"; "rec"; "mutable"; "inline"
               "abstract"; "override"; "default"; "member"; "static"; "val"
               "new"; "base"; "begin"; "end"; "class"; "struct"; "interface"
               "inherit"; "upcast"; "downcast"; "use"; "try"; "finally"; "raise"
               "when"; "as"; "of"; "ref"; "lazy"; "private"; "public"; "internal"
               "extern"; "void"; "global"; "const"; "fixed"; "volatile"; "asr"
               "land"; "lor"; "lxor"; "lsl"; "lsr"; "sig"; "exception"; "assert"
               "yield!"; "return!"; "let!"; "do!"; "use!"; "match!"; "select"
               "where"; "join"; "groupBy"; "sortBy"; "thenBy"; "elif" |],
            StringComparer.Ordinal
        )

    /// Standard library functions / well-known identifiers that must not be renamed.
    let stdlibFunctions =
        HashSet<string>(
            [| "printfn"; "printf"; "sprintf"; "eprintfn"; "eprintf"
               "failwith"; "failwithf"; "ignore"; "fst"; "snd"; "id"; "not"
               "raise"; "typeof"; "sizeof"; "nameof"; "box"; "unbox"
               "string"; "int"; "float"; "decimal"; "char"; "byte"; "sbyte"
               "int16"; "uint16"; "int32"; "uint32"; "int64"; "uint64"
               "float32"; "nativeint"; "unativeint"; "hash"; "compare"
               "defaultof"; "enum"; "invalidArg"; "invalidOp"; "nullArg"
               "stdin"; "stdout"; "stderr"; "exit"; "lock"; "using"
               "async"; "task"; "seq"; "query"
               "Some"; "None"; "Ok"; "Error"; "Choice1Of2"; "Choice2Of3"
               "Choice1Of3"; "Choice2Of2"; "Choice3Of3" |],
            StringComparer.Ordinal
        )

    /// Module prefixes from the standard library that must not be renamed.
    let stdlibModules =
        HashSet<string>(
            [| "List"; "Array"; "Seq"; "Map"; "Set"; "Option"; "Result"
               "Async"; "Task"; "String"; "Char"; "Int32"; "Int64"; "Math"
               "Console"; "Environment"; "IO"; "File"; "Path"; "Directory"
               "Array2D"; "Array3D"; "Nullable"; "ValueOption"; "Lazy"
               "Event"; "Observable"; "Printf"; "Checked"; "Operators"
               "LanguagePrimitives"; "ExtraTopLevelOperators"
               "Collections"; "Generic"; "Linq"; "Threading"; "Tasks"
               "System"; "Microsoft"; "FSharp" |],
            StringComparer.Ordinal
        )

    /// Returns true if the identifier must be preserved (keyword, stdlib, etc.).
    let isPreserved (name: string) =
        String.IsNullOrWhiteSpace(name)
        || name.Length <= 1
        || keywords.Contains(name)
        || stdlibFunctions.Contains(name)
        || stdlibModules.Contains(name)
        || name.StartsWith("op_", StringComparison.Ordinal)

// ═══════════════════════════════════════════════════════════════════════════════
// SQL detection heuristic
// ═══════════════════════════════════════════════════════════════════════════════

[<RequireQualifiedAccess>]
module internal SqlDetection =

    /// Determines whether a string value looks like a SQL statement based on common
    /// SQL keyword prefixes. Used to delegate SQL-like string literals to the SQL processor.
    let looksLikeSql (text: string) =
        if text.Length < 10 then false
        else
            let trimmed = text.TrimStart().ToUpperInvariant()
            trimmed.StartsWith("SELECT ", StringComparison.Ordinal) ||
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
            trimmed.StartsWith("BEGIN ", StringComparison.Ordinal)

// ═══════════════════════════════════════════════════════════════════════════════
// Helpers: line-offset table for Range → character-offset conversion
// ═══════════════════════════════════════════════════════════════════════════════

[<RequireQualifiedAccess>]
module internal LineOffsets =

    /// Builds an array where element i is the character offset of the start of
    /// (1-based) line i. Index 0 is unused.  So lineStartOffsets.[1] = 0 always.
    let build (content: string) : int array =
        let mutable offsets = ResizeArray<int>()
        offsets.Add(0) // placeholder for index 0 (lines are 1-based in FCS)
        offsets.Add(0) // line 1 starts at offset 0
        for i in 0 .. content.Length - 1 do
            if content.[i] = '\n' then
                offsets.Add(i + 1)
        offsets.ToArray()

    /// Converts an FCS Range (1-based lines, 0-based columns) to a character offset + length.
    let rangeToOffset (lineStarts: int array) (range: FSharp.Compiler.Text.Range) : int * int =
        let startLine = range.StartLine
        let startCol = range.StartColumn
        let endLine = range.EndLine
        let endCol = range.EndColumn
        if startLine >= 0 && startLine < lineStarts.Length && endLine >= 0 && endLine < lineStarts.Length then
            let startOffset = lineStarts.[startLine] + startCol
            let endOffset = lineStarts.[endLine] + endCol
            (startOffset, max 0 (endOffset - startOffset))
        elif startLine >= 0 && startLine < lineStarts.Length then
            let startOffset = lineStarts.[startLine] + startCol
            (startOffset, 0)
        else
            (-1, 0)

// ═══════════════════════════════════════════════════════════════════════════════
// Comment detection via regex (FCS AST does not include comments directly)
// ═══════════════════════════════════════════════════════════════════════════════

[<RequireQualifiedAccess>]
module internal Comments =

    /// Matches single-line comments: // ...
    let private singleLinePattern = Regex(@"//[^\r\n]*", RegexOptions.Compiled)

    /// Matches multi-line (potentially nested) comments: (* ... *)
    /// We handle nesting manually rather than with regex.
    let findSingleLine (content: string) : (int * int) list =
        let mutable results = []
        let mutable m = singleLinePattern.Match(content)
        while m.Success do
            results <- (m.Index, m.Length) :: results
            m <- m.NextMatch()
        results |> List.rev

    /// Finds all top-level (* ... *) comment spans, correctly handling nesting.
    let findMultiLine (content: string) : (int * int) list =
        let mutable results = []
        let mutable i = 0
        while i < content.Length - 1 do
            if content.[i] = '(' && content.[i + 1] = '*' then
                let startIdx = i
                let mutable depth = 1
                i <- i + 2
                while i < content.Length - 1 && depth > 0 do
                    if content.[i] = '(' && content.[i + 1] = '*' then
                        depth <- depth + 1
                        i <- i + 2
                    elif content.[i] = '*' && content.[i + 1] = ')' then
                        depth <- depth - 1
                        i <- i + 2
                    else
                        i <- i + 1
                // If we ran out of content while still nested, include rest
                if depth > 0 then
                    i <- content.Length
                let len = i - startIdx
                results <- (startIdx, len) :: results
            else
                i <- i + 1
        results |> List.rev

    /// Returns true if character offset falls inside a comment span.
    let isInsideComment (commentSpans: (int * int) list) (offset: int) : bool =
        commentSpans
        |> List.exists (fun (start, len) -> offset >= start && offset < start + len)

    /// Builds replacement entries for all comments in the source.
    let buildCommentReplacements (content: string) : Replacement list =
        let singles =
            findSingleLine content
            |> List.map (fun (offset, len) ->
                { Offset = offset
                  Length = len
                  OriginalValue = content.Substring(offset, len)
                  NewValue = "// [Comment removed]" })
        let multis =
            findMultiLine content
            |> List.map (fun (offset, len) ->
                { Offset = offset
                  Length = len
                  OriginalValue = content.Substring(offset, len)
                  NewValue = "(* [Comment removed] *)" })
        singles @ multis

// ═══════════════════════════════════════════════════════════════════════════════
// AST Walking: collects identifiers + string literals from the untyped AST
// ═══════════════════════════════════════════════════════════════════════════════

[<RequireQualifiedAccess>]
module internal AstWalker =

    /// Collects a single Ident into a replacement, applying context alias.
    let private collectIdent
        (context: ObfuscationContext)
        (filePath: string option)
        (lineStarts: int array)
        (category: SemanticCategory)
        (ident: Ident)
        : Replacement list =
        let name = ident.idText
        if Preserved.isPreserved name then
            []
        else
            let range = ident.idRange
            let (offset, length) = LineOffsets.rangeToOffset lineStarts range
            if offset < 0 then
                []
            else
                let fp = filePath |> Option.defaultValue ""
                let alias =
                    context.GetOrCreateAlias(
                        name,
                        category,
                        (if String.IsNullOrEmpty(fp) then null else fp),
                        Nullable<int>(range.StartLine),
                        Nullable<int>(range.StartColumn),
                        Nullable<int>(range.EndColumn)
                    )
                if alias <> name then
                    [ { Offset = offset
                        Length = length
                        OriginalValue = name
                        NewValue = alias } ]
                else
                    []

    /// Collects replacements for a LongIdent (list of Idents) with a given category.
    let private collectLongIdent context filePath lineStarts category (longId: LongIdent) =
        longId |> List.collect (collectIdent context filePath lineStarts category)

    // ── Pattern walking ──────────────────────────────────────────────────

    let rec private walkPat context filePath lineStarts (pat: SynPat) : Replacement list =
        match pat with
        | SynPat.Named(SynIdent(ident, _), _, _, _) ->
            collectIdent context filePath lineStarts SemanticCategory.Variable ident
        | SynPat.LongIdent(longDotId, _, _, argPats, _, _) ->
            let idReplacements =
                match longDotId.LongIdent with
                | [ single ] ->
                    // Could be a function name or union case constructor
                    collectIdent context filePath lineStarts SemanticCategory.Method single
                | idents ->
                    // Qualified: last ident is the name, earlier are module/namespace qualifiers
                    let leading = idents |> List.take (idents.Length - 1)
                    let last = idents |> List.last
                    let lrs = leading |> List.collect (collectIdent context filePath lineStarts SemanticCategory.Module)
                    let lr = collectIdent context filePath lineStarts SemanticCategory.Method last
                    lrs @ lr
            let argReplacements = walkArgPats context filePath lineStarts argPats
            idReplacements @ argReplacements
        | SynPat.Typed(innerPat, typeSig, _) ->
            let patRs = walkPat context filePath lineStarts innerPat
            let typeRs = walkType context filePath lineStarts typeSig
            patRs @ typeRs
        | SynPat.Paren(innerPat, _) ->
            walkPat context filePath lineStarts innerPat
        | SynPat.Tuple(_, pats, _, _) ->
            pats |> List.collect (walkPat context filePath lineStarts)
        | SynPat.ArrayOrList(_, pats, _) ->
            pats |> List.collect (walkPat context filePath lineStarts)
        | SynPat.Or(lhs, rhs, _, _) ->
            walkPat context filePath lineStarts lhs
            @ walkPat context filePath lineStarts rhs
        | SynPat.As(lhs, rhs, _) ->
            walkPat context filePath lineStarts lhs
            @ walkPat context filePath lineStarts rhs
        | SynPat.Record(fieldPats, _) ->
            fieldPats
            |> List.collect (fun (NamePatPairField(_, _, _, pat, _)) ->
                walkPat context filePath lineStarts pat)
        | SynPat.Wild _ -> []
        | SynPat.Const _ -> []
        | SynPat.Null _ -> []
        | SynPat.OptionalVal(ident, _) ->
            collectIdent context filePath lineStarts SemanticCategory.Parameter ident
        | SynPat.IsInst(typeSig, _) ->
            walkType context filePath lineStarts typeSig
        | SynPat.Attrib(pat, _, _) ->
            walkPat context filePath lineStarts pat
        | SynPat.ListCons(lhs, rhs, _, _) ->
            walkPat context filePath lineStarts lhs
            @ walkPat context filePath lineStarts rhs
        | _ -> []

    and private walkArgPats context filePath lineStarts (argPats: SynArgPats) : Replacement list =
        match argPats with
        | SynArgPats.Pats pats ->
            pats |> List.collect (walkPat context filePath lineStarts)
        | SynArgPats.NamePatPairs(pairs, _, _) ->
            pairs
            |> List.collect (fun (NamePatPairField(_, _, _, pat, _)) ->
                walkPat context filePath lineStarts pat)

    // ── Type walking ─────────────────────────────────────────────────────

    and private walkType context filePath lineStarts (synType: SynType) : Replacement list =
        match synType with
        | SynType.LongIdent(longDotId) ->
            longDotId.LongIdent |> List.collect (fun ident ->
                if Preserved.isPreserved ident.idText then []
                else collectIdent context filePath lineStarts SemanticCategory.Class ident)
        | SynType.App(typeName, _, typeArgs, _, _, _, _) ->
            walkType context filePath lineStarts typeName
            @ (typeArgs |> List.collect (walkType context filePath lineStarts))
        | SynType.Fun(argType, returnType, _, _) ->
            walkType context filePath lineStarts argType
            @ walkType context filePath lineStarts returnType
        | SynType.Tuple(_, segments, _) ->
            segments
            |> List.collect (fun segment ->
                match segment with
                | SynTupleTypeSegment.Type t -> walkType context filePath lineStarts t
                | _ -> [])
        | SynType.Array(_, elementType, _) ->
            walkType context filePath lineStarts elementType
        | SynType.Var(typar, _) ->
            walkTypar context filePath lineStarts typar
        | SynType.Paren(innerType, _) ->
            walkType context filePath lineStarts innerType
        | SynType.Anon _ -> []
        | _ -> []

    and private walkTypar context filePath lineStarts (typar: SynTypar) : Replacement list =
        match typar with
        | SynTypar(ident, _, _) ->
            if Preserved.isPreserved ident.idText then []
            else collectIdent context filePath lineStarts SemanticCategory.TypeParameter ident

    // ── Expression walking ───────────────────────────────────────────────

    and private walkExpr context filePath lineStarts (expr: SynExpr) : Replacement list =
        match expr with
        | SynExpr.Ident(ident) ->
            if Preserved.isPreserved ident.idText then []
            else collectIdent context filePath lineStarts SemanticCategory.Variable ident
        | SynExpr.LongIdent(_, longDotId, _, _) ->
            let idents = longDotId.LongIdent
            match idents with
            | [] -> []
            | [ single ] ->
                if Preserved.isPreserved single.idText then []
                else collectIdent context filePath lineStarts SemanticCategory.Variable single
            | _ ->
                // For qualified identifiers, the first is typically a module/namespace
                // and the last is the member/function
                idents |> List.collect (fun ident ->
                    if Preserved.isPreserved ident.idText then []
                    else collectIdent context filePath lineStarts SemanticCategory.Variable ident)
        | SynExpr.Const(constant, range) ->
            walkConst context filePath lineStarts constant range
        | SynExpr.App(_, _, funcExpr, argExpr, _) ->
            walkExpr context filePath lineStarts funcExpr
            @ walkExpr context filePath lineStarts argExpr
        | SynExpr.Lambda(_, _, pats, body, _, _, _) ->
            let patRs = walkSimplePats context filePath lineStarts pats
            let bodyRs = walkExpr context filePath lineStarts body
            patRs @ bodyRs
        | SynExpr.LetOrUse(_, _, _, _, bindings, body, _, _) ->
            let bindRs = bindings |> List.collect (walkBinding context filePath lineStarts)
            let bodyRs = walkExpr context filePath lineStarts body
            bindRs @ bodyRs
        | SynExpr.IfThenElse(cond, thenExpr, elseExpr, _, _, _, _) ->
            let condRs = walkExpr context filePath lineStarts cond
            let thenRs = walkExpr context filePath lineStarts thenExpr
            let elseRs =
                match elseExpr with
                | Some e -> walkExpr context filePath lineStarts e
                | None -> []
            condRs @ thenRs @ elseRs
        | SynExpr.Match(_, matchExpr, clauses, _, _) ->
            let exprRs = walkExpr context filePath lineStarts matchExpr
            let clauseRs = clauses |> List.collect (walkClause context filePath lineStarts)
            exprRs @ clauseRs
        | SynExpr.MatchBang(_, matchExpr, clauses, _, _) ->
            let exprRs = walkExpr context filePath lineStarts matchExpr
            let clauseRs = clauses |> List.collect (walkClause context filePath lineStarts)
            exprRs @ clauseRs
        | SynExpr.ForEach(_, _, _, _, pat, enumExpr, bodyExpr, _) ->
            walkPat context filePath lineStarts pat
            @ walkExpr context filePath lineStarts enumExpr
            @ walkExpr context filePath lineStarts bodyExpr
        | SynExpr.For(_, _, ident, _, startExpr, _, endExpr, bodyExpr, _) ->
            collectIdent context filePath lineStarts SemanticCategory.Variable ident
            @ walkExpr context filePath lineStarts startExpr
            @ walkExpr context filePath lineStarts endExpr
            @ walkExpr context filePath lineStarts bodyExpr
        | SynExpr.While(_, condExpr, bodyExpr, _) ->
            walkExpr context filePath lineStarts condExpr
            @ walkExpr context filePath lineStarts bodyExpr
        | SynExpr.TryWith(tryExpr, clauses, _, _, _, _) ->
            walkExpr context filePath lineStarts tryExpr
            @ (clauses |> List.collect (walkClause context filePath lineStarts))
        | SynExpr.TryFinally(tryExpr, finallyExpr, _, _, _, _) ->
            walkExpr context filePath lineStarts tryExpr
            @ walkExpr context filePath lineStarts finallyExpr
        | SynExpr.Paren(expr, _, _, _) ->
            walkExpr context filePath lineStarts expr
        | SynExpr.Tuple(_, exprs, _, _) ->
            exprs |> List.collect (walkExpr context filePath lineStarts)
        | SynExpr.ArrayOrList(_, exprs, _) ->
            exprs |> List.collect (walkExpr context filePath lineStarts)
        | SynExpr.ArrayOrListComputed(_, expr, _) ->
            walkExpr context filePath lineStarts expr
        | SynExpr.Record(_, copyInfo, fields, _) ->
            let copyRs =
                match copyInfo with
                | Some(expr, _) -> walkExpr context filePath lineStarts expr
                | None -> []
            let fieldRs =
                fields
                |> List.collect (fun (SynExprRecordField((fieldName, _), _, exprOpt, _, _)) ->
                    let nameRs =
                        fieldName.LongIdent
                        |> List.collect (fun ident ->
                            if Preserved.isPreserved ident.idText then []
                            else collectIdent context filePath lineStarts SemanticCategory.Property ident)
                    let valRs =
                        match exprOpt with
                        | Some e -> walkExpr context filePath lineStarts e
                        | None -> []
                    nameRs @ valRs)
            copyRs @ fieldRs
        | SynExpr.ObjExpr(ty, argOpt, _, bindings, members, extraImpls, _, _) ->
            let typeRs = walkType context filePath lineStarts ty
            let argRs =
                match argOpt with
                | Some(expr, _) -> walkExpr context filePath lineStarts expr
                | None -> []
            let bindRs = bindings |> List.collect (walkBinding context filePath lineStarts)
            let memberRs = members |> List.collect (walkMemberDefn context filePath lineStarts)
            let implRs =
                extraImpls
                |> List.collect (fun impl ->
                    let (SynInterfaceImpl(interfaceType, _, bindings, members, _)) = impl
                    walkType context filePath lineStarts interfaceType
                    @ (bindings |> List.collect (walkBinding context filePath lineStarts))
                    @ (members |> List.collect (walkMemberDefn context filePath lineStarts)))
            typeRs @ argRs @ bindRs @ memberRs @ implRs
        | SynExpr.Sequential(_, _, expr1, expr2, _, _) ->
            walkExpr context filePath lineStarts expr1
            @ walkExpr context filePath lineStarts expr2
        | SynExpr.Do(expr, _) ->
            walkExpr context filePath lineStarts expr
        | SynExpr.DoBang(expr, _, _) ->
            walkExpr context filePath lineStarts expr
        | SynExpr.YieldOrReturn(_, expr, _, _) ->
            walkExpr context filePath lineStarts expr
        | SynExpr.YieldOrReturnFrom(_, expr, _, _) ->
            walkExpr context filePath lineStarts expr
        | SynExpr.New(_, typeName, expr, _) ->
            walkType context filePath lineStarts typeName
            @ walkExpr context filePath lineStarts expr
        | SynExpr.TypeApp(expr, _, typeArgs, _, _, _, _) ->
            walkExpr context filePath lineStarts expr
            @ (typeArgs |> List.collect (walkType context filePath lineStarts))
        | SynExpr.Typed(expr, typeSig, _) ->
            walkExpr context filePath lineStarts expr
            @ walkType context filePath lineStarts typeSig
        | SynExpr.Upcast(expr, typeSig, _) ->
            walkExpr context filePath lineStarts expr
            @ walkType context filePath lineStarts typeSig
        | SynExpr.Downcast(expr, typeSig, _) ->
            walkExpr context filePath lineStarts expr
            @ walkType context filePath lineStarts typeSig
        | SynExpr.TypeTest(expr, typeSig, _) ->
            walkExpr context filePath lineStarts expr
            @ walkType context filePath lineStarts typeSig
        | SynExpr.DotGet(expr, _, longDotId, _) ->
            let exprRs = walkExpr context filePath lineStarts expr
            let dotRs =
                longDotId.LongIdent
                |> List.collect (fun ident ->
                    if Preserved.isPreserved ident.idText then []
                    else collectIdent context filePath lineStarts SemanticCategory.Property ident)
            exprRs @ dotRs
        | SynExpr.DotSet(target, longDotId, value, _) ->
            let targetRs = walkExpr context filePath lineStarts target
            let dotRs =
                longDotId.LongIdent
                |> List.collect (fun ident ->
                    if Preserved.isPreserved ident.idText then []
                    else collectIdent context filePath lineStarts SemanticCategory.Property ident)
            let valRs = walkExpr context filePath lineStarts value
            targetRs @ dotRs @ valRs
        | SynExpr.DotIndexedGet(expr, indexArgs, _, _) ->
            walkExpr context filePath lineStarts expr
            @ walkExpr context filePath lineStarts indexArgs
        | SynExpr.DotIndexedSet(target, indexArgs, value, _, _, _) ->
            walkExpr context filePath lineStarts target
            @ walkExpr context filePath lineStarts indexArgs
            @ walkExpr context filePath lineStarts value
        | SynExpr.Set(target, value, _) ->
            walkExpr context filePath lineStarts target
            @ walkExpr context filePath lineStarts value
        | SynExpr.ComputationExpr(_, expr, _) ->
            walkExpr context filePath lineStarts expr
        | SynExpr.MatchLambda(_, _, clauses, _, _) ->
            clauses |> List.collect (walkClause context filePath lineStarts)
        | SynExpr.Assert(expr, _) ->
            walkExpr context filePath lineStarts expr
        | SynExpr.Lazy(expr, _) ->
            walkExpr context filePath lineStarts expr
        | SynExpr.AddressOf(_, expr, _, _) ->
            walkExpr context filePath lineStarts expr
        | SynExpr.InferredUpcast(expr, _) ->
            walkExpr context filePath lineStarts expr
        | SynExpr.InferredDowncast(expr, _) ->
            walkExpr context filePath lineStarts expr
        | SynExpr.InterpolatedString(parts, _, _) ->
            parts
            |> List.collect (fun part ->
                match part with
                | SynInterpolatedStringPart.FillExpr(fillExpr, _) ->
                    walkExpr context filePath lineStarts fillExpr
                | SynInterpolatedStringPart.String(text, range) ->
                    if String.IsNullOrWhiteSpace(text) then []
                    else
                        let (offset, length) = LineOffsets.rangeToOffset lineStarts range
                        if offset < 0 || length <= 0 then []
                        else
                            // Check if string looks like SQL - delegate to SQL processor
                            if SqlDetection.looksLikeSql text then
                                let registry = context.ProcessorRegistry
                                if registry <> null then
                                    let sqlProcessor = registry.GetProcessor("temp.sql", text)
                                    if sqlProcessor <> null then
                                        let fp = filePath |> Option.defaultValue ""
                                        let sqlResult = sqlProcessor.Obfuscate(text, context, (if String.IsNullOrEmpty(fp) then null else fp))
                                        if sqlResult.WasTransformed then
                                            [ { Offset = offset
                                                Length = length
                                                OriginalValue = text
                                                NewValue = sqlResult.Content } ]
                                        else
                                            walkInterpolatedStringFallback context filePath lineStarts text range offset length
                                    else
                                        walkInterpolatedStringFallback context filePath lineStarts text range offset length
                                else
                                    walkInterpolatedStringFallback context filePath lineStarts text range offset length
                            else
                                walkInterpolatedStringFallback context filePath lineStarts text range offset length)
        | _ -> []

    and private walkConst context filePath lineStarts (constant: SynConst) (range: FSharp.Compiler.Text.Range) : Replacement list =
        match constant with
        | SynConst.String(text, _, _) ->
            if String.IsNullOrEmpty(text) then
                []
            else
                // Check if string looks like SQL - delegate to SQL processor
                if SqlDetection.looksLikeSql text then
                    let registry = context.ProcessorRegistry
                    if registry <> null then
                        let sqlProcessor = registry.GetProcessor("temp.sql", text)
                        if sqlProcessor <> null then
                            let fp = filePath |> Option.defaultValue ""
                            let sqlResult = sqlProcessor.Obfuscate(text, context, (if String.IsNullOrEmpty(fp) then null else fp))
                            if sqlResult.WasTransformed then
                                let (offset, length) = LineOffsets.rangeToOffset lineStarts range
                                if offset < 0 then
                                    []
                                else
                                    [ { Offset = offset
                                        Length = length
                                        OriginalValue = text
                                        NewValue = "\"" + sqlResult.Content + "\"" } ]
                            else
                                walkConstFallback context filePath lineStarts text range
                        else
                            walkConstFallback context filePath lineStarts text range
                    else
                        walkConstFallback context filePath lineStarts text range
                else
                    walkConstFallback context filePath lineStarts text range
        | _ -> []

    and private walkConstFallback context filePath lineStarts (text: string) (range: FSharp.Compiler.Text.Range) : Replacement list =
        let fp = filePath |> Option.defaultValue ""
        let alias =
            context.GetOrCreateAlias(
                text,
                SemanticCategory.StringLiteral,
                (if String.IsNullOrEmpty(fp) then null else fp),
                Nullable<int>(range.StartLine),
                Nullable<int>(range.StartColumn),
                Nullable<int>(range.EndColumn)
            )
        if alias <> text then
            let (offset, length) = LineOffsets.rangeToOffset lineStarts range
            if offset < 0 then
                []
            else
                [ { Offset = offset
                    Length = length
                    OriginalValue = text
                    NewValue = "\"" + alias + "\"" } ]
        else
            []

    and private walkInterpolatedStringFallback context (filePath: string option) (_lineStarts: int array) (text: string) (range: FSharp.Compiler.Text.Range) (offset: int) (length: int) : Replacement list =
        let fp = filePath |> Option.defaultValue ""
        let alias =
            context.GetOrCreateAlias(
                text,
                SemanticCategory.StringLiteral,
                (if String.IsNullOrEmpty(fp) then null else fp),
                Nullable<int>(range.StartLine),
                Nullable<int>(range.StartColumn),
                Nullable<int>(range.EndColumn)
            )
        if alias <> text then
            [ { Offset = offset
                Length = length
                OriginalValue = text
                NewValue = alias } ]
        else []

    // ── Match clause walking ─────────────────────────────────────────────

    and private walkClause context filePath lineStarts (SynMatchClause(pat, whenExpr, body, _, _, _)) =
        let patRs = walkPat context filePath lineStarts pat
        let whenRs =
            match whenExpr with
            | Some e -> walkExpr context filePath lineStarts e
            | None -> []
        let bodyRs = walkExpr context filePath lineStarts body
        patRs @ whenRs @ bodyRs

    // ── Simple pats (lambda parameters) ──────────────────────────────────

    and private walkSimplePats context filePath lineStarts (pats: SynSimplePats) : Replacement list =
        match pats with
        | SynSimplePats.SimplePats(pats, _, _) ->
            pats
            |> List.collect (fun pat ->
                match pat with
                | SynSimplePat.Id(ident, _, _, _, _, _) ->
                    if Preserved.isPreserved ident.idText then []
                    else collectIdent context filePath lineStarts SemanticCategory.Parameter ident
                | SynSimplePat.Typed(innerPat, typeSig, _) ->
                    let innerRs =
                        match innerPat with
                        | SynSimplePat.Id(ident, _, _, _, _, _) ->
                            if Preserved.isPreserved ident.idText then []
                            else collectIdent context filePath lineStarts SemanticCategory.Parameter ident
                        | _ -> []
                    innerRs @ walkType context filePath lineStarts typeSig
                | _ -> [])

    // ── Binding walking ──────────────────────────────────────────────────

    and walkBinding context filePath lineStarts (SynBinding(_, _, _, _, _, _, _, pat, returnInfo, body, _, _, _)) =
        let patRs = walkPat context filePath lineStarts pat
        let returnRs =
            match returnInfo with
            | Some(SynBindingReturnInfo(typeSig, _, _, _)) -> walkType context filePath lineStarts typeSig
            | None -> []
        let bodyRs = walkExpr context filePath lineStarts body
        patRs @ returnRs @ bodyRs

    // ── Type definition walking ──────────────────────────────────────────

    and walkTypeDef context filePath lineStarts (SynTypeDefn(typeInfo, typeRepr, members, implicitCtor, _, _)) =
        let infoRs = walkComponentInfo context filePath lineStarts typeInfo
        let reprRs = walkTypeRepr context filePath lineStarts typeRepr
        let memberRs = members |> List.collect (walkMemberDefn context filePath lineStarts)
        let ctorRs =
            match implicitCtor with
            | Some(SynMemberDefn.ImplicitCtor(_, _, ctorArgs, selfIdent, _, _, _)) ->
                let argRs = walkPat context filePath lineStarts ctorArgs
                let selfRs =
                    match selfIdent with
                    | Some ident -> collectIdent context filePath lineStarts SemanticCategory.Variable ident
                    | None -> []
                argRs @ selfRs
            | _ -> []
        infoRs @ reprRs @ memberRs @ ctorRs

    and private walkComponentInfo context filePath lineStarts (SynComponentInfo(_, typeParams, _, longId, _, _, _, _)) =
        let nameRs = collectLongIdent context filePath lineStarts SemanticCategory.Class longId
        let typeParamRs =
            match typeParams with
            | Some(SynTyparDecls.PostfixList(decls, _, _))
            | Some(SynTyparDecls.PrefixList(decls, _)) ->
                decls
                |> List.collect (fun (SynTyparDecl(_, typar, _, _)) ->
                    walkTypar context filePath lineStarts typar)
            | Some(SynTyparDecls.SinglePrefix(SynTyparDecl(_, typar, _, _), _)) ->
                walkTypar context filePath lineStarts typar
            | None -> []
        nameRs @ typeParamRs

    and private walkTypeRepr context filePath lineStarts (repr: SynTypeDefnRepr) =
        match repr with
        | SynTypeDefnRepr.Simple(simpleRepr, _) ->
            walkSimpleTypeRepr context filePath lineStarts simpleRepr
        | SynTypeDefnRepr.ObjectModel(_, members, _) ->
            members |> List.collect (walkMemberDefn context filePath lineStarts)
        | SynTypeDefnRepr.Exception _ -> []

    and private walkSimpleTypeRepr context filePath lineStarts (repr: SynTypeDefnSimpleRepr) =
        match repr with
        | SynTypeDefnSimpleRepr.Record(_, fields, _) ->
            fields
            |> List.collect (fun (SynField(_, _, identOpt, fieldType, _, _, _, _, _)) ->
                let nameRs =
                    match identOpt with
                    | Some ident ->
                        collectIdent context filePath lineStarts SemanticCategory.Property ident
                    | None -> []
                let typeRs = walkType context filePath lineStarts fieldType
                nameRs @ typeRs)
        | SynTypeDefnSimpleRepr.Union(_, cases, _) ->
            cases
            |> List.collect (fun (SynUnionCase(_, SynIdent(ident, _), caseType, _, _, _, _)) ->
                let nameRs = collectIdent context filePath lineStarts SemanticCategory.UnionCase ident
                let fieldRs =
                    match caseType with
                    | SynUnionCaseKind.Fields fields ->
                        fields
                        |> List.collect (fun (SynField(_, _, identOpt, fieldType, _, _, _, _, _)) ->
                            let fNameRs =
                                match identOpt with
                                | Some fIdent -> collectIdent context filePath lineStarts SemanticCategory.Field fIdent
                                | None -> []
                            let fTypeRs = walkType context filePath lineStarts fieldType
                            fNameRs @ fTypeRs)
                    | SynUnionCaseKind.FullType(fullType, _) ->
                        walkType context filePath lineStarts fullType
                fieldRs @ nameRs)
        | SynTypeDefnSimpleRepr.Enum(cases, _) ->
            cases
            |> List.collect (fun (SynEnumCase(_, SynIdent(ident, _), _, _, _, _)) ->
                collectIdent context filePath lineStarts SemanticCategory.EnumMember ident)
        | SynTypeDefnSimpleRepr.TypeAbbrev(_, typeSig, _) ->
            walkType context filePath lineStarts typeSig
        | _ -> []

    // ── Member definition walking ────────────────────────────────────────

    and walkMemberDefn context filePath lineStarts (memberDefn: SynMemberDefn) : Replacement list =
        match memberDefn with
        | SynMemberDefn.Member(binding, _) ->
            walkBinding context filePath lineStarts binding
        | SynMemberDefn.LetBindings(bindings, _, _, _) ->
            bindings |> List.collect (walkBinding context filePath lineStarts)
        | SynMemberDefn.AbstractSlot(valSig, _, _, _) ->
            walkValSig context filePath lineStarts valSig
        | SynMemberDefn.Interface(interfaceType, _, members, _) ->
            let typeRs = walkType context filePath lineStarts interfaceType
            let memRs =
                match members with
                | Some ms -> ms |> List.collect (walkMemberDefn context filePath lineStarts)
                | None -> []
            typeRs @ memRs
        | SynMemberDefn.Inherit(baseTypeOpt, _, _, _) ->
            match baseTypeOpt with
            | Some baseType -> walkType context filePath lineStarts baseType
            | None -> []
        | SynMemberDefn.AutoProperty(_, _, ident, typeOpt, _, _, _, _, _, expr, _, _) ->
            let nameRs = collectIdent context filePath lineStarts SemanticCategory.Property ident
            let typeRs =
                match typeOpt with
                | Some t -> walkType context filePath lineStarts t
                | None -> []
            let exprRs = walkExpr context filePath lineStarts expr
            nameRs @ typeRs @ exprRs
        | SynMemberDefn.ValField(SynField(_, _, identOpt, fieldType, _, _, _, _, _), _) ->
            let nameRs =
                match identOpt with
                | Some ident -> collectIdent context filePath lineStarts SemanticCategory.Field ident
                | None -> []
            let typeRs = walkType context filePath lineStarts fieldType
            nameRs @ typeRs
        | SynMemberDefn.ImplicitCtor(_, _, ctorArgs, selfIdent, _, _, _) ->
            let argRs = walkPat context filePath lineStarts ctorArgs
            let selfRs =
                match selfIdent with
                | Some ident -> collectIdent context filePath lineStarts SemanticCategory.Variable ident
                | None -> []
            argRs @ selfRs
        | SynMemberDefn.ImplicitInherit(baseType, args, _, _, _) ->
            walkType context filePath lineStarts baseType
            @ walkExpr context filePath lineStarts args
        | SynMemberDefn.GetSetMember(getBinding, setBinding, _, _) ->
            let getRs =
                match getBinding with
                | Some b -> walkBinding context filePath lineStarts b
                | None -> []
            let setRs =
                match setBinding with
                | Some b -> walkBinding context filePath lineStarts b
                | None -> []
            getRs @ setRs
        | _ -> []

    and private walkValSig context filePath lineStarts (SynValSig(_, SynIdent(ident, _), typeParams, synType, _, _, _, _, _, _, _, _)) =
        let nameRs = collectIdent context filePath lineStarts SemanticCategory.Method ident
        let typeParamRs =
            match typeParams with
            | SynValTyparDecls(Some(SynTyparDecls.PostfixList(decls, _, _)), _)
            | SynValTyparDecls(Some(SynTyparDecls.PrefixList(decls, _)), _) ->
                decls
                |> List.collect (fun (SynTyparDecl(_, typar, _, _)) ->
                    walkTypar context filePath lineStarts typar)
            | SynValTyparDecls(Some(SynTyparDecls.SinglePrefix(SynTyparDecl(_, typar, _, _), _)), _) ->
                walkTypar context filePath lineStarts typar
            | _ -> []
        let typeRs = walkType context filePath lineStarts synType
        nameRs @ typeParamRs @ typeRs

    // ── Module declaration walking ───────────────────────────────────────

    let rec walkDecl context filePath lineStarts (decl: SynModuleDecl) : Replacement list =
        match decl with
        | SynModuleDecl.Let(_, bindings, _) ->
            bindings |> List.collect (walkBinding context filePath lineStarts)
        | SynModuleDecl.Types(typeDefs, _) ->
            typeDefs |> List.collect (walkTypeDef context filePath lineStarts)
        | SynModuleDecl.NestedModule(info, _, decls, _, _, _) ->
            let infoRs = walkComponentInfo context filePath lineStarts info
            let declRs = decls |> List.collect (walkDecl context filePath lineStarts)
            infoRs @ declRs
        | SynModuleDecl.Open(SynOpenDeclTarget.ModuleOrNamespace(longId, _), _) ->
            // Do not rename open targets - they refer to external or already-renamed modules
            []
        | SynModuleDecl.Expr(expr, _) ->
            walkExpr context filePath lineStarts expr
        | SynModuleDecl.Exception(SynExceptionDefn(repr, _, members, _), _) ->
            let reprRs =
                match repr with
                | SynExceptionDefnRepr(_, case, _, _, _, _) ->
                    match case with
                    | SynUnionCase(_, SynIdent(ident, _), caseType, _, _, _, _) ->
                        let nameRs = collectIdent context filePath lineStarts SemanticCategory.Class ident
                        let fieldRs =
                            match caseType with
                            | SynUnionCaseKind.Fields fields ->
                                fields
                                |> List.collect (fun (SynField(_, _, identOpt, fieldType, _, _, _, _, _)) ->
                                    let fNameRs =
                                        match identOpt with
                                        | Some fIdent -> collectIdent context filePath lineStarts SemanticCategory.Field fIdent
                                        | None -> []
                                    let fTypeRs = walkType context filePath lineStarts fieldType
                                    fNameRs @ fTypeRs)
                            | _ -> []
                        nameRs @ fieldRs
            let memberRs = members |> List.collect (walkMemberDefn context filePath lineStarts)
            reprRs @ memberRs
        | SynModuleDecl.Attributes _ -> []
        | SynModuleDecl.HashDirective _ -> []
        | SynModuleDecl.ModuleAbbrev(ident, _, _) ->
            collectIdent context filePath lineStarts SemanticCategory.Module ident
        | _ -> []

    // ── Top-level module/namespace walking ───────────────────────────────

    let walkModuleOrNamespace context filePath lineStarts (modul: SynModuleOrNamespace) : Replacement list =
        match modul with
        | SynModuleOrNamespace(longId, _, kind, decls, _, _, _, _, _) ->
            let nsCategory =
                match kind with
                | SynModuleOrNamespaceKind.NamedModule -> SemanticCategory.Module
                | _ -> SemanticCategory.Namespace
            let nsReplacements =
                longId
                |> List.collect (fun ident ->
                    if Preserved.isPreserved ident.idText then []
                    else collectIdent context filePath lineStarts nsCategory ident)
            let declReplacements =
                decls |> List.collect (walkDecl context filePath lineStarts)
            nsReplacements @ declReplacements

// ═══════════════════════════════════════════════════════════════════════════════
// Replacement application
// ═══════════════════════════════════════════════════════════════════════════════

[<RequireQualifiedAccess>]
module internal ReplacementApplier =

    /// Removes overlapping replacements, keeping the one with the smallest offset
    /// (leftmost). For ties, keeps the longest.
    let dedup (replacements: Replacement list) : Replacement list =
        let sorted =
            replacements
            |> List.sortBy (fun r -> r.Offset, -r.Length)

        let rec loop (acc: Replacement list) (remaining: Replacement list) =
            match remaining with
            | [] -> List.rev acc
            | r :: rest ->
                match acc with
                | [] -> loop [ r ] rest
                | prev :: _ ->
                    // Check if r overlaps with the previously accepted replacement
                    if r.Offset < prev.Offset + prev.Length then
                        // Overlapping; skip this replacement
                        loop acc rest
                    else
                        loop (r :: acc) rest

        loop [] sorted

    /// Applies replacements to the content string, replacing from back to front.
    let apply (content: string) (replacements: Replacement list) : string =
        let dedupedReplacements = dedup replacements
        // Sort descending by offset so we replace from back to front,
        // preserving earlier offsets as we go.
        let sorted =
            dedupedReplacements
            |> List.sortByDescending (fun r -> r.Offset)
        let mutable result = content
        for r in sorted do
            if r.Offset >= 0 && r.Offset + r.Length <= result.Length then
                result <- result.Substring(0, r.Offset) + r.NewValue + result.Substring(r.Offset + r.Length)
        result

// ═══════════════════════════════════════════════════════════════════════════════
// Deobfuscation: find alias-pattern tokens and reverse them
// ═══════════════════════════════════════════════════════════════════════════════

[<RequireQualifiedAccess>]
module internal Deobfuscator =

    /// Regex matching the alias format PREFIX_N produced by NamingStrategy.
    let private aliasPattern = Regex(@"\b[A-Z]+_\d+\b", RegexOptions.Compiled)

    /// Scans content for alias-pattern identifiers and builds reverse replacements.
    let buildReplacements (content: string) (context: ObfuscationContext) : Replacement list =
        let mutable results = []
        let mutable m = aliasPattern.Match(content)
        while m.Success do
            let alias = m.Value
            let original = context.Mappings.GetOriginal(alias)
            if original <> null then
                results <-
                    { Offset = m.Index
                      Length = m.Length
                      OriginalValue = alias
                      NewValue = original }
                    :: results
            m <- m.NextMatch()
        results |> List.rev

// ═══════════════════════════════════════════════════════════════════════════════
// Main processor class
// ═══════════════════════════════════════════════════════════════════════════════

/// F# language processor for CodeBleach v2.0.
/// Uses FSharp.Compiler.Service to parse F# source into an untyped AST,
/// recursively walks the AST to collect identifier positions and semantic
/// categories, then applies position-based string replacements from back to front.
type FSharpLanguageProcessor() =

    let checker = FSharpChecker.Create()

    let extensions: IReadOnlySet<string> =
        HashSet<string>([ ".fs"; ".fsx"; ".fsi" ], StringComparer.OrdinalIgnoreCase)
        :> IReadOnlySet<string>

    /// Parses F# source content into an untyped AST.
    let parseContent (filePath: string) (content: string) =
        let sourceText = SourceText.ofString content
        let parsingOptions =
            { FSharpParsingOptions.Default with
                SourceFiles = [| filePath |] }
        let parseResult =
            checker.ParseFile(filePath, sourceText, parsingOptions)
            |> Async.RunSynchronously
        parseResult

    interface ILanguageProcessor with

        member _.ProcessorId = "fsharp"

        member _.DisplayName = "F# (FSharp.Compiler.Service)"

        member _.SupportedExtensions = extensions

        member _.Priority = 10

        member _.CanProcess(filePath: string, _content: string) =
            let ext = IO.Path.GetExtension(filePath)
            extensions.Contains(ext)

        member _.PrepareBatch(_filePaths: IReadOnlyList<string>, _context: ObfuscationContext) =
            // FSharp.Compiler.Service untyped AST parsing is file-level;
            // no cross-file compilation needed for syntactic obfuscation.
            ()

        member this.Obfuscate(content: string, context: ObfuscationContext, filePath: string) =
            // Delegation-only: F# identifiers stay untouched; only SQL-in-string
            // literals would be delegated.  The SQL delegation is woven into the
            // full AST walk, so for now we short-circuit and return unchanged.
            if context.Scope.IsDelegationOnly("fsharp") then
                LanguageProcessingResult(
                    Content = content,
                    WasTransformed = false,
                    ReplacementCount = 0,
                    ProcessorId = "fsharp",
                    Warnings = (ResizeArray<string>() :> IReadOnlyList<string>)
                )
            else

            let warnings = ResizeArray<string>()

            try
                let effectivePath =
                    if String.IsNullOrEmpty(filePath) then "anonymous.fs"
                    else filePath

                let parseResult = parseContent effectivePath content
                let lineStarts = LineOffsets.build content

                // Collect any parse warnings (non-fatal)
                for diag in parseResult.Diagnostics do
                    if diag.Severity = FSharp.Compiler.Diagnostics.FSharpDiagnosticSeverity.Warning then
                        warnings.Add($"Parse warning: {diag.Message}")

                // Walk the AST to collect identifier replacements
                let astReplacements =
                    match parseResult.ParseTree with
                    | ParsedInput.ImplFile(ParsedImplFileInput(_, _, _, _, modules, _, _, _)) ->
                        modules
                        |> List.collect (
                            AstWalker.walkModuleOrNamespace
                                context
                                (if String.IsNullOrEmpty(filePath) then None else Some filePath)
                                lineStarts
                        )
                    | ParsedInput.SigFile _ ->
                        // Signature files are not yet supported for obfuscation
                        []

                // Collect comment replacements
                let commentReplacements = Comments.buildCommentReplacements content

                // Combine and apply all replacements
                let allReplacements = astReplacements @ commentReplacements
                let transformedContent = ReplacementApplier.apply content allReplacements
                let replacementCount = (ReplacementApplier.dedup allReplacements).Length

                // Record file processing
                if not (String.IsNullOrEmpty(filePath)) then
                    context.RecordFileProcessing(filePath, "fsharp", replacementCount)

                LanguageProcessingResult(
                    Content = transformedContent,
                    WasTransformed = (replacementCount > 0),
                    ReplacementCount = replacementCount,
                    ProcessorId = "fsharp",
                    Warnings = (warnings :> IReadOnlyList<string>)
                )
            with ex ->
                warnings.Add($"F# processor failed, returning original content: {ex.Message}")
                LanguageProcessingResult(
                    Content = content,
                    WasTransformed = false,
                    ReplacementCount = 0,
                    ProcessorId = "fsharp",
                    Warnings = (warnings :> IReadOnlyList<string>)
                )

        member this.Deobfuscate(content: string, context: ObfuscationContext, filePath: string) =
            let warnings = ResizeArray<string>()

            try
                let replacements = Deobfuscator.buildReplacements content context
                let transformedContent = ReplacementApplier.apply content replacements
                let replacementCount = (ReplacementApplier.dedup replacements).Length

                LanguageProcessingResult(
                    Content = transformedContent,
                    WasTransformed = (replacementCount > 0),
                    ReplacementCount = replacementCount,
                    ProcessorId = "fsharp",
                    Warnings = (warnings :> IReadOnlyList<string>)
                )
            with ex ->
                warnings.Add($"F# deobfuscation failed, returning original content: {ex.Message}")
                LanguageProcessingResult(
                    Content = content,
                    WasTransformed = false,
                    ReplacementCount = 0,
                    ProcessorId = "fsharp",
                    Warnings = (warnings :> IReadOnlyList<string>)
                )

        member this.Validate(obfuscatedContent: string) =
            try
                let parseResult = parseContent "validation.fs" obfuscatedContent
                let errors =
                    parseResult.Diagnostics
                    |> Array.filter (fun d ->
                        d.Severity = FSharp.Compiler.Diagnostics.FSharpDiagnosticSeverity.Error)
                    |> Array.toList

                if errors.IsEmpty then
                    ValidationResult.Valid()
                else
                    let errorMessages =
                        errors
                        |> List.map (fun d ->
                            $"[Line {d.Range.StartLine}, Col {d.Range.StartColumn}] {d.Message}")
                    ValidationResult.Invalid(errorMessages :> IReadOnlyList<string>)
            with ex ->
                ValidationResult.Invalid([ $"Failed to parse: {ex.Message}" ] :> IReadOnlyList<string>)
