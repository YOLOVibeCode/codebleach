using CodeBleach.Core.Models;
using CodeBleach.Processors.JavaScript;
using FluentAssertions;
using Xunit;

namespace CodeBleach.Processors.JavaScript.Tests;

/// <summary>
/// Comprehensive tests for the JavaScriptLanguageProcessor.
/// Tests cover: metadata, CanProcess, obfuscation of classes, functions,
/// variables (const/let), arrow function parameters, class methods,
/// destructuring, import/export bindings, import path preservation,
/// keyword/global protection, comments, string literals, round-trip
/// deobfuscation, and validation.
/// </summary>
public class JavaScriptLanguageProcessorTests
{
    private readonly JavaScriptLanguageProcessor _processor = new();

    private static ObfuscationContext CreateContext(ObfuscationLevel level = ObfuscationLevel.Full)
        => new(level);

    // ═══════════════════════════════════════════════════════════════════
    // Metadata Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void ProcessorId_Returns_JavaScript()
    {
        _processor.ProcessorId.Should().Be("javascript");
    }

    [Fact]
    public void DisplayName_Returns_JavaScriptAcornima()
    {
        _processor.DisplayName.Should().Be("JavaScript (Acornima)");
    }

    [Theory]
    [InlineData(".js")]
    [InlineData(".mjs")]
    [InlineData(".cjs")]
    public void SupportedExtensions_ContainsExpected(string ext)
    {
        _processor.SupportedExtensions.Should().Contain(ext);
    }

    // ═══════════════════════════════════════════════════════════════════
    // CanProcess Tests
    // ═══════════════════════════════════════════════════════════════════

    [Theory]
    [InlineData("test.js")]
    [InlineData("test.mjs")]
    public void CanProcess_WithSupportedExtension_ReturnsTrue(string filePath)
    {
        _processor.CanProcess(filePath, "").Should().BeTrue();
    }

    [Fact]
    public void CanProcess_WithUnsupportedExtension_ReturnsFalse()
    {
        _processor.CanProcess("test.ts", "const x = 1;").Should().BeFalse();
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Empty / Null Input
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_EmptyContent_ReturnsUnchanged()
    {
        var ctx = CreateContext();
        var result = _processor.Obfuscate("", ctx, "test.js");

        result.WasTransformed.Should().BeFalse();
    }

    [Fact]
    public void Obfuscate_NullContent_ReturnsGracefully()
    {
        var ctx = CreateContext();

        var act = () => _processor.Obfuscate(null!, ctx, "test.js");

        act.Should().NotThrow();
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Class Declarations
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ClassDeclaration_IsRenamed()
    {
        var js = "class UserService { constructor() {} }";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(js, ctx, "test.js");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("UserService");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Function Declarations
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_FunctionDeclaration_IsRenamed()
    {
        var js = "function calculateTotal(items) { return 0; }";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(js, ctx, "test.js");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("calculateTotal");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Variable Declarations (const / let)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ConstVariable_IsRenamed()
    {
        var js = "const totalAmount = 42;";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(js, ctx, "test.js");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("totalAmount");
    }

    [Fact]
    public void Obfuscate_LetVariable_IsRenamed()
    {
        var js = "let orderCount = 0;";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(js, ctx, "test.js");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("orderCount");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Arrow Function Parameters
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ArrowFunctionParameter_IsRenamed()
    {
        var js = "const fn = (orderItem) => orderItem;";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(js, ctx, "test.js");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("orderItem");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Class Methods
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ClassMethod_IsRenamed()
    {
        var js = @"class OrderHandler {
    processOrder() { return null; }
}";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(js, ctx, "test.js");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("processOrder");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Destructuring
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_DestructuringVariable_IsRenamed()
    {
        var js = "const obj = { a: 1, b: 2 }; const { customerName, orderTotal } = obj;";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(js, ctx, "test.js");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("customerName");
        result.Content.Should().NotContain("orderTotal");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Import Bindings
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ImportBinding_IsRenamed()
    {
        var js = "import { PaymentService } from './payment.js';";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(js, ctx, "test.js");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("PaymentService");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Export Bindings
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ExportBinding_IsRenamed()
    {
        var js = "const OrderManager = 1; export { OrderManager };";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(js, ctx, "test.js");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("OrderManager");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Import Paths Preserved
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ImportPath_IsPreserved()
    {
        var js = "import { x } from './utils.js';";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(js, ctx, "test.js");

        result.Content.Should().Contain("'./utils.js'");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - JS Keywords and Globals Protection
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_JsKeywordsAndGlobals_AreNotRenamed()
    {
        var js = @"function doWork(data) {
    console.log(data);
    const result = Math.round(JSON.parse(data));
    return result;
}";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(js, ctx, "test.js");

        result.Content.Should().Contain("console");
        result.Content.Should().Contain("Math");
        result.Content.Should().Contain("JSON");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Comments
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_Comments_AreReplaced()
    {
        var js = "// This is a secret comment\nconst x = 1;";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(js, ctx, "test.js");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("secret");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - String Literals
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_StringLiteral_IsObfuscated()
    {
        var js = "const msg = \"SensitiveMessage\";";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(js, ctx, "test.js");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("SensitiveMessage");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Round-Trip Test (Obfuscate -> Deobfuscate)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void RoundTrip_ModuleWithImports_RestoresAll()
    {
        var js = @"import { DataService } from './data.js';

class ReportGenerator {
    constructor() {
        this.service = new DataService();
    }

    generateReport(inputData) {
        const summary = this.service.process(inputData);
        return summary;
    }
}

function formatOutput(reportData) {
    return reportData;
}

export { ReportGenerator, formatOutput };";
        var ctx = CreateContext();

        var obfuscated = _processor.Obfuscate(js, ctx, "test.js");
        obfuscated.WasTransformed.Should().BeTrue();
        obfuscated.Content.Should().NotContain("ReportGenerator");
        obfuscated.Content.Should().NotContain("generateReport");
        obfuscated.Content.Should().NotContain("formatOutput");

        var restored = _processor.Deobfuscate(obfuscated.Content, ctx, "test.js");
        restored.WasTransformed.Should().BeTrue();
        restored.Content.Should().Contain("ReportGenerator");
        restored.Content.Should().Contain("DataService");
        restored.Content.Should().Contain("generateReport");
        restored.Content.Should().Contain("inputData");
        restored.Content.Should().Contain("formatOutput");
        restored.Content.Should().Contain("reportData");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Validate Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Validate_ObfuscatedJs_ReturnsValid()
    {
        var js = @"import { DataService } from './data.js';

class ReportGenerator {
    constructor() {
        this.service = new DataService();
    }

    generateReport(inputData) {
        const summary = this.service.process(inputData);
        return summary;
    }
}

export { ReportGenerator };";
        var ctx = CreateContext();

        var obfuscated = _processor.Obfuscate(js, ctx, "test.js");
        obfuscated.WasTransformed.Should().BeTrue();

        var validation = _processor.Validate(obfuscated.Content);
        validation.IsValid.Should().BeTrue();
    }
}
