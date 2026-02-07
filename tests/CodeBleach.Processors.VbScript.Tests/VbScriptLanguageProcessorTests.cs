using CodeBleach.Core.Models;
using CodeBleach.Processors.VbScript;
using FluentAssertions;
using Xunit;

namespace CodeBleach.Processors.VbScript.Tests;

/// <summary>
/// Comprehensive tests for the VbScriptLanguageProcessor.
/// Tests cover: metadata, CanProcess (extension matching), obfuscation of all VBScript/VBA
/// constructs (Sub, Function, Dim, Class, Property, Const, ReDim), dialect-aware built-in
/// preservation (WScript, Application, MsgBox, CreateObject, ThisWorkbook), comment and
/// string literal obfuscation, deobfuscation round-trips, and validation.
/// </summary>
public class VbScriptLanguageProcessorTests
{
    private readonly VbScriptLanguageProcessor _processor = new();

    private static ObfuscationContext CreateContext(ObfuscationLevel level = ObfuscationLevel.Full)
        => new(level);

    // ═══════════════════════════════════════════════════════════════════
    // Metadata Tests
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void ProcessorId_Returns_VbScript()
    {
        _processor.ProcessorId.Should().Be("vbscript");
    }

    [Fact]
    public void DisplayName_Returns_VbScriptVba()
    {
        _processor.DisplayName.Should().Be("VBScript/VBA");
    }

    [Theory]
    [InlineData(".vbs")]
    [InlineData(".bas")]
    [InlineData(".cls")]
    [InlineData(".frm")]
    public void SupportedExtensions_ContainsExpected(string ext)
    {
        _processor.SupportedExtensions.Should().Contain(ext);
    }

    // ═══════════════════════════════════════════════════════════════════
    // CanProcess Tests
    // ═══════════════════════════════════════════════════════════════════

    [Theory]
    [InlineData("test.vbs")]
    [InlineData("test.bas")]
    [InlineData("test.cls")]
    public void CanProcess_WithSupportedExtension_ReturnsTrue(string filePath)
    {
        var content = "Sub Test()\n  Dim x\n  x = 1\nEnd Sub";
        _processor.CanProcess(filePath, content).Should().BeTrue();
    }

    [Fact]
    public void CanProcess_WithUnsupportedExtension_ReturnsFalse()
    {
        _processor.CanProcess("test.cs", "public class Foo {}").Should().BeFalse();
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Empty / Null Input
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_EmptyContent_ReturnsUnchanged()
    {
        var ctx = CreateContext();
        var result = _processor.Obfuscate("", ctx, "test.vbs");

        result.WasTransformed.Should().BeFalse();
    }

    [Fact]
    public void Obfuscate_NullContent_ReturnsGracefully()
    {
        var ctx = CreateContext();
        var act = () => _processor.Obfuscate(null!, ctx, "test.vbs");

        act.Should().NotThrow();
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Sub Declaration
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_SubDeclaration_IsRenamed()
    {
        var code = "Sub ProcessPayment()\n  Dim x\n  x = 1\nEnd Sub";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.vbs");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("ProcessPayment");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Function Declaration
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_FunctionDeclaration_IsRenamed()
    {
        var code = "Function GetOrderTotal()\n  GetOrderTotal = 42\nEnd Function";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.vbs");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("GetOrderTotal");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Dim Variable
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_DimVariable_IsRenamed()
    {
        var code = "Dim totalPaymentCount\ntotalPaymentCount = 0";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.vbs");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("totalPaymentCount");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Class Declaration
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ClassDeclaration_IsRenamed()
    {
        var code = "Class CustomerOrder\n  Private mName\nEnd Class";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.cls");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("CustomerOrder");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Property Declaration
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_PropertyDeclaration_IsRenamed()
    {
        var code = "Class MyObj\n  Public Property Get OrderName()\n    OrderName = \"test\"\n  End Property\nEnd Class";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.cls");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("OrderName");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Const Declaration
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ConstDeclaration_IsRenamed()
    {
        var code = "Const MAX_RETRY_COUNT = 5\nDim x\nx = MAX_RETRY_COUNT";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.vbs");

        result.WasTransformed.Should().BeTrue();
        result.Content.Should().NotContain("MAX_RETRY_COUNT");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - VBScript Dialect Preserves WScript
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_VbScriptDialect_PreservesWScript()
    {
        var code = "WScript.Echo \"test\"\nDim myVar\nmyVar = 1";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.vbs");

        result.Content.Should().Contain("WScript");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - VBA Dialect Preserves Excel Objects
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_VbaDialect_PreservesExcelObjects()
    {
        var code = "Sub Test()\n  Application.Run \"macro\"\n  Dim myVar\n  myVar = 1\nEnd Sub";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.bas");

        result.Content.Should().Contain("Application");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - VBScript Built-ins Preserved
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_VbScriptBuiltins_ArePreserved()
    {
        var code = "Dim myResult\nmyResult = MsgBox(\"hello\")\nDim myObj\nSet myObj = CreateObject(\"Scripting.FSO\")";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.vbs");

        result.Content.Should().Contain("MsgBox");
        result.Content.Should().Contain("CreateObject");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - VBA Built-ins Preserved
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_VbaBuiltins_ArePreserved()
    {
        var code = "Sub Test()\n  Application.Run \"macro\"\n  Dim wb\n  Set wb = ThisWorkbook\nEnd Sub";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.bas");

        result.Content.Should().Contain("Application");
        result.Content.Should().Contain("ThisWorkbook");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - Comments Replaced
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_Comments_AreReplaced()
    {
        var code = "' This is a secret comment\nDim x\nx = 1";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.vbs");

        result.Content.Should().NotContain("secret");
        result.Content.Should().Contain("[Comment removed]");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - String Literal Obfuscated
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_StringLiteral_IsObfuscated()
    {
        var code = "Dim s\ns = \"SensitiveValue\"";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.vbs");

        result.Content.Should().NotContain("SensitiveValue");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Round-Trip Test (Obfuscate -> Deobfuscate)
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void RoundTrip_SimpleScript_RestoresAll()
    {
        var code = string.Join("\n",
            "Sub ProcessOrder()",
            "  Dim orderTotal",
            "  orderTotal = 100",
            "End Sub",
            "",
            "Function GetDiscount()",
            "  GetDiscount = 10",
            "End Function",
            "",
            "Dim myCounter",
            "myCounter = 0");
        var ctx = CreateContext();

        var obfuscated = _processor.Obfuscate(code, ctx, "test.vbs");
        obfuscated.WasTransformed.Should().BeTrue();

        // Verify originals are gone after obfuscation
        obfuscated.Content.Should().NotContain("ProcessOrder");
        obfuscated.Content.Should().NotContain("orderTotal");
        obfuscated.Content.Should().NotContain("GetDiscount");
        obfuscated.Content.Should().NotContain("myCounter");

        // Deobfuscate and verify originals are restored
        var restored = _processor.Deobfuscate(obfuscated.Content, ctx, "test.vbs");
        restored.WasTransformed.Should().BeTrue();
        restored.Content.Should().Contain("ProcessOrder");
        restored.Content.Should().Contain("orderTotal");
        restored.Content.Should().Contain("GetDiscount");
        restored.Content.Should().Contain("myCounter");
    }

    // ═══════════════════════════════════════════════════════════════════
    // Validate Test
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Validate_ObfuscatedVbScript_ReturnsValid()
    {
        var code = string.Join("\n",
            "Sub ProcessOrder()",
            "  Dim orderTotal",
            "  orderTotal = 100",
            "End Sub",
            "",
            "Dim myVar",
            "myVar = 42");
        var ctx = CreateContext();

        var obfuscated = _processor.Obfuscate(code, ctx, "test.vbs");
        var validation = _processor.Validate(obfuscated.Content);

        validation.IsValid.Should().BeTrue();
    }

    // ═══════════════════════════════════════════════════════════════════
    // Obfuscate - ReDim Variable
    // ═══════════════════════════════════════════════════════════════════

    [Fact]
    public void Obfuscate_ReDimVariable_IsRenamed()
    {
        var code = "Dim items()\nReDim items(10)";
        var ctx = CreateContext();

        var result = _processor.Obfuscate(code, ctx, "test.vbs");

        result.WasTransformed.Should().BeTrue();
        // The variable "items" should have been discovered and replaced
        result.Content.Should().NotContain("items");
    }
}
