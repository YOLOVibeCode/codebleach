using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;
using CodeBleach.Processors.FSharp;
using FluentAssertions;
using Xunit;

namespace CodeBleach.Processors.FSharp.Tests;

/// <summary>
/// Comprehensive tests for the FSharpLanguageProcessor.
/// Tests cover: metadata, CanProcess, obfuscation of F# constructs
/// (let bindings, functions, modules, types, discriminated unions, records,
/// parameters), keyword and stdlib preservation, comment removal,
/// string literal obfuscation, deobfuscation round-trips, and validation.
/// </summary>
public class FSharpLanguageProcessorTests
{
    private readonly ILanguageProcessor _processor = new FSharpLanguageProcessor();

    private static ObfuscationContext CreateContext(ObfuscationLevel level = ObfuscationLevel.Full)
        => new(level);

    // ═══════════════════════════════════════════════════════════════════
    // Metadata Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void ProcessorId_Returns_FSharp()
    {
        _processor.ProcessorId.Should().Be("fsharp");
    }

    [Fact]
    public void DisplayName_Returns_FSharpFcs()
    {
        _processor.DisplayName.Should().Be("F# (FSharp.Compiler.Service)");
    }

    [Theory]
    [InlineData(".fs")]
    [InlineData(".fsi")]
    [InlineData(".fsx")]
    public void SupportedExtensions_ContainsExpected(string ext)
    {
        _processor.SupportedExtensions.Should().Contain(ext);
    }

    // ═══════════════════════════════════════════════════════════════════
    // CanProcess Tests
    // ═══════════════════════════════════════════════════════════════════

    [Theory]
    [InlineData("test.fs")]
    [InlineData("test.fsi")]
    [InlineData("test.fsx")]
    public void CanProcess_WithSupportedExtension_ReturnsTrue(string filePath)
    {
        _processor.CanProcess(filePath, "").Should().BeTrue();
    }

    [Fact]
    public void CanProcess_WithUnsupportedExtension_ReturnsFalse()
    {
        _processor.CanProcess("test.cs", "").Should().BeFalse();
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Empty / Null Input
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_EmptyContent_DoesNotThrow()
    {
        var ctx = CreateContext();

        var act = () => _processor.Obfuscate("", ctx, "test.fs");

        act.Should().NotThrow();
    }

    [Fact]
    public void Obfuscate_NullContent_ReturnsGracefully()
    {
        var ctx = CreateContext();

        var act = () => _processor.Obfuscate(null!, ctx, "test.fs");

        act.Should().NotThrow();
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Let Binding
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_LetBinding_IsRenamed()
    {
        var code = "module TestModule\nlet totalPaymentAmount = 42";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.fs");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("totalPaymentAmount");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Function Declaration
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_FunctionDeclaration_IsRenamed()
    {
        var code = "module TestModule\nlet calculateOrderTotal x y = x + y";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.fs");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("calculateOrderTotal");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Module Declaration
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ModuleDeclaration_IsRenamed()
    {
        var code = "module BillingServices\nlet x = 1";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.fs");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("BillingServices");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Type Declaration
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_TypeDeclaration_IsRenamed()
    {
        var code = "module TestModule\ntype CustomerRecord = { FullName: string; Age: int }";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.fs");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("CustomerRecord");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Union Case
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_UnionCase_IsRenamed()
    {
        var code = "module TestModule\ntype PaymentShape = CirclePayment of float | SquarePayment of float";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.fs");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("CirclePayment");
        result.Content.Should().NotContain("SquarePayment");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Parameter Name
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ParameterName_IsRenamed()
    {
        var code = "module TestModule\nlet processItem (orderItem: int) = orderItem + 1";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.fs");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("orderItem");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - F# Keywords Preserved
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_FSharpKeywords_ArePreserved()
    {
        var code = "module TestModule\ntype CustomerShape = Circle of float | Square of float\nlet processItem (orderItem: int) = orderItem + 1";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.fs");

        result.Content.Should().Contain("let");
        result.Content.Should().Contain("module");
        result.Content.Should().Contain("type");
        result.Content.Should().Contain("of");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Stdlib Functions Preserved
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_StdlibFunctions_ArePreserved()
    {
        var code = "module TestModule\nlet runDemo () =\n    printfn \"hello\"\n    [1;2;3] |> List.map (fun x -> x + 1) |> Seq.filter (fun x -> x > 1) |> ignore";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.fs");

        result.Content.Should().Contain("printfn");
        result.Content.Should().Contain("List");
        result.Content.Should().Contain("Seq");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Stdlib Modules Preserved
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_StdlibModules_ArePreserved()
    {
        var code = "module TestModule\nlet demo () =\n    let xs = List.empty<int>\n    let arr = Array.empty<int>\n    let sq = Seq.empty<int>\n    let mp = Map.empty<string, int>\n    let st = Set.empty<int>\n    ignore (xs, arr, sq, mp, st)";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.fs");

        result.Content.Should().Contain("List");
        result.Content.Should().Contain("Array");
        result.Content.Should().Contain("Seq");
        result.Content.Should().Contain("Map");
        result.Content.Should().Contain("Set");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Single-Line Comment Replaced
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_SingleLineComment_IsReplaced()
    {
        var code = "// This is a secret comment\nmodule TestModule\nlet x = 1";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.fs");

        result.Content.Should().NotContain("secret");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Nested Block Comment Replaced
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_NestedBlockComment_IsReplaced()
    {
        var code = "(* outer (* inner secret *) *)\nmodule TestModule\nlet x = 1";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.fs");

        result.Content.Should().NotContain("secret");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - String Literal Obfuscated
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_StringLiteral_IsObfuscated()
    {
        var code = "module TestModule\nlet greeting = \"SensitiveGreeting\"";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.fs");

        result.Content.Should().NotContain("SensitiveGreeting");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Record Fields Renamed
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_RecordFields_AreRenamed()
    {
        var code = "module TestModule\ntype Order = { OrderName: string; OrderTotal: int }";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.fs");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("OrderName");
        result.Content.Should().NotContain("OrderTotal");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Round-Trip Test (Obfuscate -> Deobfuscate)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void RoundTrip_SimpleModule_RestoresAll()
    {
        var code = "module TestModule\nlet calculateTotal x y = x + y\nlet runCalc () = calculateTotal 10 20";
        var ctx = CreateContext();

        var obfuscated = _processor.Obfuscate(code, ctx, "test.fs");

        obfuscated.WasTransformed.Should().BeTrue();
        obfuscated.Content.Should().NotContain("calculateTotal");

        var restored = _processor.Deobfuscate(obfuscated.Content, ctx, "test.fs");

        restored.WasTransformed.Should().BeTrue();
        restored.Content.Should().Contain("calculateTotal");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Validate Test
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Validate_ObfuscatedFSharp_ReturnsValid()
    {
        var code = "module TestModule\nlet calculateTotal x y = x + y\nlet result = calculateTotal 10 20";
        var ctx = CreateContext();

        var obfuscated = _processor.Obfuscate(code, ctx, "test.fs");

        var validation = _processor.Validate(obfuscated.Content);

        validation.IsValid.Should().BeTrue();
    }
}
