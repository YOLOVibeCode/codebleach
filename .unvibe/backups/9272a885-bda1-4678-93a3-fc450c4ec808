# CodeBleach - Implementation Plan

**Version:** 1.0  
**Document Type:** Architecture Roadmap / Implementation Checklist  
**Status:** Approved for Development  
**Created:** January 15, 2026  
**Role:** Architect

---

## Executive Summary

This document provides a **complete implementation roadmap** for CodeBleach, a .NET 10 global tool for sanitizing code before sharing with AI assistants. The plan follows strict **TDD** (Test-Driven Development) and **ISP** (Interface Segregation Principle) methodologies.

**Estimated Total Effort:** 8-10 engineering days  
**Recommended Team Size:** 1-2 developers  
**Sprint Duration:** 5 phases, each completable in 1-2 days

---

## Table of Contents

1. [Implementation Phases Overview](#1-implementation-phases-overview)
2. [Phase 0: Project Scaffolding](#2-phase-0-project-scaffolding)
3. [Phase 1: Core Domain Models](#3-phase-1-core-domain-models)
4. [Phase 2: Sanitization Engine](#4-phase-2-sanitization-engine)
5. [Phase 3: Restoration Engine](#5-phase-3-restoration-engine)
6. [Phase 4: File Processing & Manifest](#6-phase-4-file-processing--manifest)
7. [Phase 5: CLI Commands](#7-phase-5-cli-commands)
8. [Dependency Graph](#8-dependency-graph)
9. [Interface Contracts](#9-interface-contracts)
10. [Risk Assessment](#10-risk-assessment)
11. [Definition of Done](#11-definition-of-done)

---

## 1. Implementation Phases Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        IMPLEMENTATION TIMELINE                               │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  Phase 0        Phase 1        Phase 2        Phase 3        Phase 4        │
│  ────────       ────────       ────────       ────────       ────────       │
│  Scaffolding    Models         Sanitizer      Restorer       File/CLI       │
│                                                                              │
│  [███]          [███]          [█████]        [███]          [█████]        │
│   Day 1          Day 2          Day 3-4        Day 5          Day 6-8       │
│                                                                              │
│  ───────────────────────────────────────────────────────────────────────▶   │
│                                                                              │
│  Legend:                                                                     │
│  [███] = Estimated effort (blocks ≈ 1 day)                                  │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

| Phase | Name | Duration | Dependencies | Deliverables |
|-------|------|----------|--------------|--------------|
| 0 | Project Scaffolding | 0.5 day | None | Solution, projects, CI/CD |
| 1 | Core Domain Models | 1 day | Phase 0 | Records, DTOs, MappingTable |
| 2 | Sanitization Engine | 2 days | Phase 1 | ISanitizer, IRuleRegistry |
| 3 | Restoration Engine | 1 day | Phase 1 | IRestorer |
| 4 | File Processing & Manifest | 2 days | Phase 2, 3 | IFileProcessor, IManifestManager |
| 5 | CLI Commands | 2 days | Phase 4 | Commands, global tool |

---

## 2. Phase 0: Project Scaffolding

**Duration:** 0.5 day  
**Objective:** Create project structure, configure tooling, verify CI/CD

### 2.1 Tasks

| Task ID | Task | Priority | Est. |
|---------|------|----------|------|
| P0-001 | Create solution file `CodeBleach.sln` | P0 | 5m |
| P0-002 | Create `src/CodeBleach/` project (CLI entry point) | P0 | 15m |
| P0-003 | Create `src/CodeBleach.Core/` project (business logic) | P0 | 15m |
| P0-004 | Create `tests/CodeBleach.Tests/` project | P0 | 15m |
| P0-005 | Create `tests/CodeBleach.IntegrationTests/` project | P0 | 15m |
| P0-006 | Configure global tool in `CodeBleach.csproj` | P0 | 10m |
| P0-007 | Add package references | P0 | 10m |
| P0-008 | Create `GlobalUsings.cs` files | P1 | 10m |
| P0-009 | Verify `dotnet build` succeeds | P0 | 5m |
| P0-010 | Verify `dotnet test` succeeds (empty) | P0 | 5m |
| P0-011 | Verify GitHub Actions CI runs | P1 | 10m |

### 2.2 Project Configuration

**`src/CodeBleach/CodeBleach.csproj`:**

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
    
    <!-- Global Tool Configuration -->
    <PackAsTool>true</PackAsTool>
    <ToolCommandName>codebleach</ToolCommandName>
    <PackageOutputPath>./nupkg</PackageOutputPath>
    
    <!-- NuGet Metadata -->
    <PackageId>CodeBleach</PackageId>
    <Version>1.0.0</Version>
    <Authors>YOLOVibeCode</Authors>
    <Description>Dead simple sanitization utility for sharing code with AI assistants</Description>
    <PackageTags>cli;sanitization;security;ai;privacy;codebleach</PackageTags>
    <PackageLicenseExpression>MIT</PackageLicenseExpression>
    <RepositoryUrl>https://github.com/YOLOVibeCode/codebleach</RepositoryUrl>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="System.CommandLine" Version="2.*" />
    <PackageReference Include="Microsoft.Extensions.DependencyInjection" Version="10.*" />
    <PackageReference Include="Microsoft.Extensions.Logging" Version="10.*" />
    <PackageReference Include="Microsoft.Extensions.Logging.Console" Version="10.*" />
  </ItemGroup>

  <ItemGroup>
    <ProjectReference Include="..\CodeBleach.Core\CodeBleach.Core.csproj" />
  </ItemGroup>
</Project>
```

**`src/CodeBleach.Core/CodeBleach.Core.csproj`:**

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="Microsoft.Extensions.Logging.Abstractions" Version="10.*" />
  </ItemGroup>
</Project>
```

**`tests/CodeBleach.Tests/CodeBleach.Tests.csproj`:**

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
    <IsPackable>false</IsPackable>
    <IsTestProject>true</IsTestProject>
  </PropertyGroup>

  <ItemGroup>
    <PackageReference Include="Microsoft.NET.Test.Sdk" Version="17.*" />
    <PackageReference Include="xunit" Version="2.*" />
    <PackageReference Include="xunit.runner.visualstudio" Version="2.*" />
    <PackageReference Include="FluentAssertions" Version="7.*" />
    <PackageReference Include="coverlet.collector" Version="6.*" />
  </ItemGroup>

  <ItemGroup>
    <ProjectReference Include="..\..\src\CodeBleach.Core\CodeBleach.Core.csproj" />
  </ItemGroup>
</Project>
```

### 2.3 Directory Structure to Create

```
CodeBleach/
├── src/
│   ├── CodeBleach/
│   │   ├── Commands/
│   │   ├── GlobalUsings.cs
│   │   ├── Program.cs
│   │   └── CodeBleach.csproj
│   │
│   └── CodeBleach.Core/
│       ├── Interfaces/
│       ├── Services/
│       ├── Rules/
│       ├── Models/
│       ├── GlobalUsings.cs
│       └── CodeBleach.Core.csproj
│
├── tests/
│   ├── CodeBleach.Tests/
│   │   ├── Services/
│   │   ├── Rules/
│   │   ├── Models/
│   │   ├── GlobalUsings.cs
│   │   └── CodeBleach.Tests.csproj
│   │
│   ├── CodeBleach.IntegrationTests/
│   │   ├── GlobalUsings.cs
│   │   └── CodeBleach.IntegrationTests.csproj
│   │
│   └── fixtures/
│       └── sample-project/
│
├── .github/
│   └── workflows/
│       ├── ci.yml
│       └── release.yml
│
├── .cursorrules
├── .gitignore
├── SPECIFICATION.md
├── IMPLEMENTATION_PLAN.md
├── README.md
├── LICENSE
└── CodeBleach.sln
```

### 2.4 Acceptance Criteria

- [ ] `dotnet build` completes with zero warnings
- [ ] `dotnet test` runs (0 tests initially, no failures)
- [ ] `dotnet pack` creates `.nupkg` file
- [ ] GitHub Actions CI workflow passes
- [ ] Solution opens without errors in IDE

---

## 3. Phase 1: Core Domain Models

**Duration:** 1 day  
**Objective:** Implement all data models and DTOs with full test coverage  
**TDD Approach:** Write model tests first, then implement records

### 3.1 Tasks

| Task ID | Task | Priority | Est. | Test First |
|---------|------|----------|------|------------|
| P1-001 | Write `SanitizationRuleTests` | P0 | 30m | ✅ |
| P1-002 | Implement `SanitizationRule` record | P0 | 20m | |
| P1-003 | Write `MappingTableTests` | P0 | 45m | ✅ |
| P1-004 | Implement `MappingTable` class | P0 | 30m | |
| P1-005 | Write `ManifestTests` | P0 | 30m | ✅ |
| P1-006 | Implement `Manifest` record | P0 | 20m | |
| P1-007 | Write `SanitizationResultTests` | P0 | 20m | ✅ |
| P1-008 | Implement `SanitizationResult` record | P0 | 15m | |
| P1-009 | Write `FileProcessingResultTests` | P0 | 20m | ✅ |
| P1-010 | Implement `FileProcessingResult` record | P0 | 15m | |
| P1-011 | Implement `RuleSeverity` enum | P0 | 5m | |

### 3.2 Model Specifications

#### 3.2.1 SanitizationRule

```csharp
// Location: src/CodeBleach.Core/Models/SanitizationRule.cs

/// <summary>
/// Defines a pattern for detecting and sanitizing sensitive data.
/// </summary>
public record SanitizationRule
{
    /// <summary>Unique identifier for the rule (e.g., "server_names")</summary>
    public required string RuleId { get; init; }
    
    /// <summary>Human-readable name</summary>
    public required string Name { get; init; }
    
    /// <summary>Description of what this rule detects</summary>
    public required string Description { get; init; }
    
    /// <summary>Regex pattern for detection</summary>
    public required string Pattern { get; init; }
    
    /// <summary>Alias prefix (e.g., "SERVER", "IP", "TABLE")</summary>
    public required string Prefix { get; init; }
    
    /// <summary>Severity level for reporting</summary>
    public required RuleSeverity Severity { get; init; }
    
    /// <summary>Whether this rule is active</summary>
    public bool Enabled { get; init; } = true;
    
    /// <summary>Values to exclude from matching</summary>
    public IReadOnlyList<string> Exceptions { get; init; } = [];
    
    /// <summary>Processing order (lower = earlier)</summary>
    public int Order { get; init; } = 100;
}
```

#### 3.2.2 MappingTable

```csharp
// Location: src/CodeBleach.Core/Models/MappingTable.cs

/// <summary>
/// Bidirectional mapping between original values and aliases.
/// </summary>
public sealed class MappingTable
{
    /// <summary>Original value → Alias (e.g., "ProductionDB" → "SERVER_0")</summary>
    public Dictionary<string, string> Forward { get; init; } = new();
    
    /// <summary>Alias → Original value (e.g., "SERVER_0" → "ProductionDB")</summary>
    public Dictionary<string, string> Reverse { get; init; } = new();
    
    /// <summary>Counter per prefix for alias generation</summary>
    public Dictionary<string, int> Counters { get; init; } = new();
    
    /// <summary>Gets or creates an alias for the given value.</summary>
    public string GetOrCreateAlias(string originalValue, string prefix);
    
    /// <summary>Checks if a mapping exists for the original value.</summary>
    public bool HasMapping(string originalValue);
    
    /// <summary>Gets the original value for an alias, or null if not found.</summary>
    public string? GetOriginal(string alias);
}
```

#### 3.2.3 Manifest

```csharp
// Location: src/CodeBleach.Core/Models/Manifest.cs

/// <summary>
/// Complete metadata for a sanitization run, stored in .codebleach/manifest.json
/// </summary>
public record Manifest
{
    public const string CurrentVersion = "1.0";
    public const string FileName = "manifest.json";
    public const string DirectoryName = ".codebleach";
    
    public required string Version { get; init; }
    public required string SourcePath { get; init; }
    public required string DestinationPath { get; init; }
    public required DateTime CreatedAt { get; init; }
    public DateTime? RestoredAt { get; init; }
    public required MappingTable Mappings { get; init; }
    public required IReadOnlyList<string> ProcessedFiles { get; init; }
    public required SanitizationStats Stats { get; init; }
}

public record SanitizationStats
{
    public required int TotalFiles { get; init; }
    public required int ProcessedFiles { get; init; }
    public required int SkippedFiles { get; init; }
    public required int TotalReplacements { get; init; }
    public required int UniqueValuesReplaced { get; init; }
    public required long ProcessingTimeMs { get; init; }
}
```

#### 3.2.4 SanitizationResult

```csharp
// Location: src/CodeBleach.Core/Models/SanitizationResult.cs

/// <summary>
/// Result of sanitizing content.
/// </summary>
public record SanitizationResult
{
    public required string Content { get; init; }
    public required bool WasSanitized { get; init; }
    public required IReadOnlyList<SanitizationMatch> Matches { get; init; }
}

public record SanitizationMatch
{
    public required string OriginalValue { get; init; }
    public required string Alias { get; init; }
    public required string RuleId { get; init; }
    public required int LineNumber { get; init; }
    public required int StartIndex { get; init; }
    public required int Length { get; init; }
}
```

### 3.3 Test Specifications

#### MappingTableTests (Priority: P0)

```csharp
// tests/CodeBleach.Tests/Models/MappingTableTests.cs

public class MappingTableTests
{
    [Fact]
    public void GetOrCreateAlias_NewValue_CreatesAlias()
    {
        // Arrange
        var table = new MappingTable();
        
        // Act
        var alias = table.GetOrCreateAlias("ProductionDB", "SERVER");
        
        // Assert
        alias.Should().Be("SERVER_0");
        table.Forward.Should().ContainKey("ProductionDB");
        table.Reverse.Should().ContainKey("SERVER_0");
    }
    
    [Fact]
    public void GetOrCreateAlias_ExistingValue_ReturnsSameAlias()
    {
        // Arrange
        var table = new MappingTable();
        table.GetOrCreateAlias("ProductionDB", "SERVER");
        
        // Act
        var alias = table.GetOrCreateAlias("ProductionDB", "SERVER");
        
        // Assert
        alias.Should().Be("SERVER_0");
        table.Counters["SERVER"].Should().Be(1); // Not incremented
    }
    
    [Fact]
    public void GetOrCreateAlias_MultipleValues_IncrementsCounter()
    {
        // Arrange
        var table = new MappingTable();
        
        // Act
        var alias1 = table.GetOrCreateAlias("ProductionDB", "SERVER");
        var alias2 = table.GetOrCreateAlias("StagingDB", "SERVER");
        
        // Assert
        alias1.Should().Be("SERVER_0");
        alias2.Should().Be("SERVER_1");
    }
    
    [Fact]
    public void GetOriginal_ExistingAlias_ReturnsOriginal()
    {
        // Arrange
        var table = new MappingTable();
        table.GetOrCreateAlias("ProductionDB", "SERVER");
        
        // Act
        var original = table.GetOriginal("SERVER_0");
        
        // Assert
        original.Should().Be("ProductionDB");
    }
    
    [Fact]
    public void GetOriginal_NonExistentAlias_ReturnsNull()
    {
        // Arrange
        var table = new MappingTable();
        
        // Act
        var original = table.GetOriginal("SERVER_99");
        
        // Assert
        original.Should().BeNull();
    }
    
    [Fact]
    public void HasMapping_ExistingValue_ReturnsTrue()
    {
        // Arrange
        var table = new MappingTable();
        table.GetOrCreateAlias("ProductionDB", "SERVER");
        
        // Act & Assert
        table.HasMapping("ProductionDB").Should().BeTrue();
    }
    
    [Fact]
    public void HasMapping_NonExistentValue_ReturnsFalse()
    {
        // Arrange
        var table = new MappingTable();
        
        // Act & Assert
        table.HasMapping("ProductionDB").Should().BeFalse();
    }
}
```

### 3.4 Acceptance Criteria

- [ ] All 7 model tests pass
- [ ] `MappingTable` correctly handles bidirectional mappings
- [ ] `MappingTable` counters increment correctly per prefix
- [ ] Records are immutable (except MappingTable internals)
- [ ] All public members have XML documentation
- [ ] Zero compiler warnings

---

## 4. Phase 2: Sanitization Engine

**Duration:** 2 days  
**Objective:** Implement sanitization logic with pattern matching  
**Dependencies:** Phase 1 (Models)  
**TDD Approach:** Tests first for each interface method

### 4.1 Tasks

| Task ID | Task | Priority | Est. | Test First |
|---------|------|----------|------|------------|
| P2-001 | Define `ISanitizer` interface | P0 | 10m | |
| P2-002 | Define `IRuleRegistry` interface | P0 | 10m | |
| P2-003 | Write `RuleRegistryTests` | P0 | 45m | ✅ |
| P2-004 | Implement `RuleRegistry` | P0 | 30m | |
| P2-005 | Write `BuiltInRulesTests` | P0 | 1h | ✅ |
| P2-006 | Implement `BuiltInRules` | P0 | 45m | |
| P2-007 | Write `SanitizerTests` - basic | P0 | 1h | ✅ |
| P2-008 | Implement `Sanitizer` - basic | P0 | 1h | |
| P2-009 | Write `SanitizerTests` - edge cases | P0 | 1h | ✅ |
| P2-010 | Implement `Sanitizer` - edge cases | P0 | 45m | |
| P2-011 | Write `SanitizerTests` - performance | P1 | 30m | ✅ |
| P2-012 | Optimize `Sanitizer` if needed | P1 | 30m | |

### 4.2 Interface Specifications

#### 4.2.1 ISanitizer

```csharp
// Location: src/CodeBleach.Core/Interfaces/ISanitizer.cs

/// <summary>
/// Sanitizes content by replacing sensitive values with aliases.
/// </summary>
public interface ISanitizer
{
    /// <summary>
    /// Sanitizes the given content using active rules.
    /// </summary>
    /// <param name="content">Content to sanitize</param>
    /// <param name="mappings">Mapping table to use/update</param>
    /// <returns>Sanitization result with transformed content</returns>
    SanitizationResult Sanitize(string content, MappingTable mappings);
}
```

#### 4.2.2 IRuleRegistry

```csharp
// Location: src/CodeBleach.Core/Interfaces/IRuleRegistry.cs

/// <summary>
/// Registry for sanitization rules.
/// </summary>
public interface IRuleRegistry
{
    /// <summary>Gets all active rules, ordered by priority.</summary>
    IReadOnlyList<SanitizationRule> GetActiveRules();
    
    /// <summary>Gets a rule by its ID.</summary>
    SanitizationRule? GetRule(string ruleId);
    
    /// <summary>Adds a custom rule.</summary>
    void AddRule(SanitizationRule rule);
    
    /// <summary>Disables a rule by ID.</summary>
    void DisableRule(string ruleId);
    
    /// <summary>Enables a rule by ID.</summary>
    void EnableRule(string ruleId);
}
```

### 4.3 Built-in Rules

```csharp
// Location: src/CodeBleach.Core/Rules/BuiltInRules.cs

public static class BuiltInRules
{
    public static IEnumerable<SanitizationRule> All => new[]
    {
        ServerNames,
        ProdDatabases,
        TableNamesUsers,
        PrivateIp10,
        PrivateIp172,
        PrivateIp192,
        ConnectionStrings,
        WindowsPaths,
        UncPaths,
        InternalHostnames
    };
    
    public static SanitizationRule ServerNames => new()
    {
        RuleId = "server_names",
        Name = "Server/Database Names",
        Description = "Detects server and database names like ProductionDB",
        Pattern = @"\b[A-Z][a-zA-Z]*DB\d*\b",
        Prefix = "SERVER",
        Severity = RuleSeverity.Medium,
        Order = 10
    };
    
    public static SanitizationRule PrivateIp192 => new()
    {
        RuleId = "private_ip_192",
        Name = "Private IP (192.168.x)",
        Description = "Detects 192.168.x.x private IP addresses",
        Pattern = @"\b192\.168\.\d{1,3}\.\d{1,3}\b",
        Prefix = "IP",
        Severity = RuleSeverity.High,
        Order = 20
    };
    
    // ... additional rules as per SPECIFICATION.md Section 9
}
```

### 4.4 Test Specifications

#### SanitizerTests (Priority: P0)

```csharp
// tests/CodeBleach.Tests/Services/SanitizerTests.cs

public class SanitizerTests
{
    private readonly IRuleRegistry _ruleRegistry;
    private readonly ISanitizer _sut;
    
    public SanitizerTests()
    {
        _ruleRegistry = new RuleRegistry();
        foreach (var rule in BuiltInRules.All)
            _ruleRegistry.AddRule(rule);
        
        _sut = new Sanitizer(_ruleRegistry);
    }
    
    // --- Basic Functionality ---
    
    [Fact]
    public void Sanitize_WithServerName_ReplacesWithAlias()
    {
        // Arrange
        var content = "SELECT * FROM ProductionDB.users";
        var mappings = new MappingTable();
        
        // Act
        var result = _sut.Sanitize(content, mappings);
        
        // Assert
        result.Content.Should().Be("SELECT * FROM SERVER_0.users");
        result.WasSanitized.Should().BeTrue();
        result.Matches.Should().HaveCount(1);
        result.Matches[0].OriginalValue.Should().Be("ProductionDB");
        result.Matches[0].Alias.Should().Be("SERVER_0");
    }
    
    [Fact]
    public void Sanitize_WithMultipleOccurrences_UsesSameAlias()
    {
        // Arrange
        var content = "USE ProductionDB;\nSELECT * FROM ProductionDB.users;";
        var mappings = new MappingTable();
        
        // Act
        var result = _sut.Sanitize(content, mappings);
        
        // Assert
        result.Content.Should().Contain("SERVER_0").And.NotContain("SERVER_1");
        result.Matches.Should().HaveCount(2);
        result.Matches.Should().AllSatisfy(m => m.Alias.Should().Be("SERVER_0"));
    }
    
    [Theory]
    [InlineData("192.168.1.100", "IP")]
    [InlineData("10.0.0.50", "IP")]
    [InlineData("172.16.0.1", "IP")]
    public void Sanitize_WithPrivateIP_ReplacesWithAlias(string ip, string expectedPrefix)
    {
        // Arrange
        var content = $"Host: {ip}";
        var mappings = new MappingTable();
        
        // Act
        var result = _sut.Sanitize(content, mappings);
        
        // Assert
        result.Content.Should().StartWith($"Host: {expectedPrefix}_");
        result.WasSanitized.Should().BeTrue();
    }
    
    // --- Edge Cases ---
    
    [Fact]
    public void Sanitize_WithNoSensitiveData_ReturnsUnchanged()
    {
        // Arrange
        var content = "Hello, World!";
        var mappings = new MappingTable();
        
        // Act
        var result = _sut.Sanitize(content, mappings);
        
        // Assert
        result.Content.Should().Be("Hello, World!");
        result.WasSanitized.Should().BeFalse();
        result.Matches.Should().BeEmpty();
    }
    
    [Fact]
    public void Sanitize_WithEmptyContent_ReturnsEmpty()
    {
        // Arrange
        var content = "";
        var mappings = new MappingTable();
        
        // Act
        var result = _sut.Sanitize(content, mappings);
        
        // Assert
        result.Content.Should().BeEmpty();
        result.WasSanitized.Should().BeFalse();
    }
    
    [Fact]
    public void Sanitize_WithExistingMappings_ReusesAliases()
    {
        // Arrange
        var mappings = new MappingTable();
        mappings.GetOrCreateAlias("ProductionDB", "SERVER"); // Pre-populate
        
        var content = "Connect to ProductionDB";
        
        // Act
        var result = _sut.Sanitize(content, mappings);
        
        // Assert
        result.Content.Should().Be("Connect to SERVER_0");
    }
    
    // --- Performance ---
    
    [Fact]
    public void Sanitize_WithLargeContent_CompletesWithinTimeout()
    {
        // Arrange
        var content = string.Join("\n", 
            Enumerable.Repeat("Server=ProductionDB;IP=192.168.1.100;", 10000));
        var mappings = new MappingTable();
        
        // Act
        var stopwatch = Stopwatch.StartNew();
        var result = _sut.Sanitize(content, mappings);
        stopwatch.Stop();
        
        // Assert
        stopwatch.ElapsedMilliseconds.Should().BeLessThan(1000);
        result.WasSanitized.Should().BeTrue();
    }
}
```

#### BuiltInRulesTests (Priority: P0)

```csharp
// tests/CodeBleach.Tests/Rules/BuiltInRulesTests.cs

public class BuiltInRulesTests
{
    [Theory]
    [InlineData("ProductionDB", "server_names", true)]
    [InlineData("StagingDB", "server_names", true)]
    [InlineData("TestDB123", "server_names", true)]
    [InlineData("mydb", "server_names", false)] // lowercase, no match
    [InlineData("Database", "server_names", false)] // doesn't end with DB
    public void ServerNamesRule_MatchesCorrectly(string input, string ruleId, bool shouldMatch)
    {
        // Arrange
        var rule = BuiltInRules.ServerNames;
        var regex = new Regex(rule.Pattern);
        
        // Act & Assert
        regex.IsMatch(input).Should().Be(shouldMatch);
    }
    
    [Theory]
    [InlineData("192.168.1.1", true)]
    [InlineData("192.168.255.255", true)]
    [InlineData("192.169.1.1", false)] // not 192.168.x.x
    [InlineData("8.8.8.8", false)] // public IP
    public void PrivateIp192Rule_MatchesCorrectly(string input, bool shouldMatch)
    {
        // Arrange
        var rule = BuiltInRules.PrivateIp192;
        var regex = new Regex(rule.Pattern);
        
        // Act & Assert
        regex.IsMatch(input).Should().Be(shouldMatch);
    }
    
    [Fact]
    public void AllRules_HaveUniqueIds()
    {
        // Act
        var ruleIds = BuiltInRules.All.Select(r => r.RuleId).ToList();
        
        // Assert
        ruleIds.Should().OnlyHaveUniqueItems();
    }
    
    [Fact]
    public void AllRules_HaveValidPatterns()
    {
        // Act & Assert
        foreach (var rule in BuiltInRules.All)
        {
            var action = () => new Regex(rule.Pattern);
            action.Should().NotThrow($"Rule '{rule.RuleId}' has invalid pattern");
        }
    }
}
```

### 4.5 Implementation Notes

**Sanitizer Algorithm:**

```
1. For each active rule (ordered by priority):
   a. Find all matches using compiled regex
   b. For each match:
      i.  Check if value already has mapping
      ii. If not, create new alias using MappingTable.GetOrCreateAlias()
      iii. Track match info (line number, position)
   c. Sort matches by position (descending) to avoid index shift
   d. Replace matches from end to start

2. Return SanitizationResult with:
   - Transformed content
   - WasSanitized flag
   - List of all matches with line numbers
```

### 4.6 Acceptance Criteria

- [ ] All sanitizer tests pass
- [ ] Built-in rules detect expected patterns
- [ ] Same value always maps to same alias
- [ ] Performance: <1s for 10,000 lines
- [ ] No false positives on common code
- [ ] Line numbers correctly tracked

---

## 5. Phase 3: Restoration Engine

**Duration:** 1 day  
**Objective:** Implement restoration logic to reverse sanitization  
**Dependencies:** Phase 1 (Models), Phase 2 (for integration tests)

### 5.1 Tasks

| Task ID | Task | Priority | Est. | Test First |
|---------|------|----------|------|------------|
| P3-001 | Define `IRestorer` interface | P0 | 10m | |
| P3-002 | Write `RestorerTests` - basic | P0 | 45m | ✅ |
| P3-003 | Implement `Restorer` - basic | P0 | 30m | |
| P3-004 | Write `RestorerTests` - edge cases | P0 | 30m | ✅ |
| P3-005 | Implement `Restorer` - edge cases | P0 | 20m | |
| P3-006 | Write round-trip integration tests | P0 | 45m | ✅ |

### 5.2 Interface Specification

```csharp
// Location: src/CodeBleach.Core/Interfaces/IRestorer.cs

/// <summary>
/// Restores sanitized content by replacing aliases with original values.
/// </summary>
public interface IRestorer
{
    /// <summary>
    /// Restores the given content using the mapping table.
    /// </summary>
    /// <param name="content">Sanitized content</param>
    /// <param name="mappings">Mapping table with reverse mappings</param>
    /// <returns>Restored content with original values</returns>
    RestoreResult Restore(string content, MappingTable mappings);
}

public record RestoreResult
{
    public required string Content { get; init; }
    public required bool WasRestored { get; init; }
    public required int ReplacementCount { get; init; }
    public required IReadOnlyList<string> UnmatchedAliases { get; init; }
}
```

### 5.3 Test Specifications

```csharp
// tests/CodeBleach.Tests/Services/RestorerTests.cs

public class RestorerTests
{
    private readonly IRestorer _sut = new Restorer();
    
    [Fact]
    public void Restore_WithAliases_ReplacesWithOriginals()
    {
        // Arrange
        var content = "SELECT * FROM SERVER_0.TABLE_0";
        var mappings = new MappingTable
        {
            Reverse = new Dictionary<string, string>
            {
                ["SERVER_0"] = "ProductionDB",
                ["TABLE_0"] = "users"
            }
        };
        
        // Act
        var result = _sut.Restore(content, mappings);
        
        // Assert
        result.Content.Should().Be("SELECT * FROM ProductionDB.users");
        result.WasRestored.Should().BeTrue();
        result.ReplacementCount.Should().Be(2);
    }
    
    [Fact]
    public void Restore_WithNoAliases_ReturnsUnchanged()
    {
        // Arrange
        var content = "Hello, World!";
        var mappings = new MappingTable();
        
        // Act
        var result = _sut.Restore(content, mappings);
        
        // Assert
        result.Content.Should().Be("Hello, World!");
        result.WasRestored.Should().BeFalse();
    }
    
    [Fact]
    public void Restore_WithLongerAliasFirst_RestoresCorrectly()
    {
        // Arrange - SERVER_10 should be replaced before SERVER_1
        var content = "Connect to SERVER_1 and SERVER_10";
        var mappings = new MappingTable
        {
            Reverse = new Dictionary<string, string>
            {
                ["SERVER_1"] = "StagingDB",
                ["SERVER_10"] = "ArchiveDB"
            }
        };
        
        // Act
        var result = _sut.Restore(content, mappings);
        
        // Assert
        result.Content.Should().Be("Connect to StagingDB and ArchiveDB");
        // Not "Connect to StagingDB and StagingDB0"
    }
    
    [Fact]
    public void Restore_WithUnmatchedAlias_ReportsIt()
    {
        // Arrange
        var content = "Connect to SERVER_99"; // Not in mappings
        var mappings = new MappingTable
        {
            Reverse = new Dictionary<string, string>
            {
                ["SERVER_0"] = "ProductionDB"
            }
        };
        
        // Act
        var result = _sut.Restore(content, mappings);
        
        // Assert
        result.Content.Should().Contain("SERVER_99"); // Unchanged
        result.UnmatchedAliases.Should().Contain("SERVER_99");
    }
}
```

### 5.4 Implementation Notes

**Restorer Algorithm:**

```
1. Collect all aliases from mappings.Reverse.Keys
2. Sort aliases by length (descending) to avoid partial matches
3. For each alias (longest first):
   a. Find all occurrences in content
   b. Replace with original value from mappings.Reverse[alias]
   c. Track replacement count

4. Scan for potential unmatched aliases (pattern: PREFIX_N)
5. Return RestoreResult
```

### 5.5 Acceptance Criteria

- [ ] All restorer tests pass
- [ ] Longer aliases processed before shorter ones
- [ ] Round-trip: sanitize → restore = original content
- [ ] Unmatched aliases reported but not modified
- [ ] Empty mappings handled gracefully

---

## 6. Phase 4: File Processing & Manifest

**Duration:** 2 days  
**Objective:** Implement file operations, manifest management, and cross-reference generation  
**Dependencies:** Phase 2, Phase 3

### 6.1 Tasks

| Task ID | Task | Priority | Est. | Test First |
|---------|------|----------|------|------------|
| P4-001 | Define `IFileProcessor` interface | P0 | 15m | |
| P4-002 | Define `IManifestManager` interface | P0 | 15m | |
| P4-003 | Write `FileProcessorTests` - basic | P0 | 1h | ✅ |
| P4-004 | Implement `FileProcessor` - copy | P0 | 1h | |
| P4-005 | Write `FileProcessorTests` - filtering | P0 | 45m | ✅ |
| P4-006 | Implement `FileProcessor` - filtering | P0 | 45m | |
| P4-007 | Write `ManifestManagerTests` | P0 | 45m | ✅ |
| P4-008 | Implement `ManifestManager` | P0 | 45m | |
| P4-009 | Write `XrefGeneratorTests` | P0 | 30m | ✅ |
| P4-010 | Implement `XrefGenerator` | P0 | 30m | |
| P4-011 | Integration: full sanitize workflow | P0 | 1h | ✅ |

### 6.2 Interface Specifications

#### 6.2.1 IFileProcessor

```csharp
// Location: src/CodeBleach.Core/Interfaces/IFileProcessor.cs

/// <summary>
/// Handles file system operations for sanitization.
/// </summary>
public interface IFileProcessor
{
    /// <summary>Determines if a file should be processed based on extension and path.</summary>
    bool ShouldProcess(string filePath);
    
    /// <summary>Determines if a path should be ignored (binary, .git, node_modules, etc.).</summary>
    bool ShouldIgnore(string path);
    
    /// <summary>Gets all files to process in a directory.</summary>
    IEnumerable<string> GetFilesToProcess(string directoryPath);
    
    /// <summary>Copies directory structure, skipping ignored paths.</summary>
    Task CopyDirectoryAsync(
        string sourcePath, 
        string destinationPath, 
        CancellationToken ct = default);
    
    /// <summary>Processes a single file (sanitize or restore).</summary>
    Task<FileProcessingResult> ProcessFileAsync(
        string sourcePath,
        string destinationPath,
        Func<string, string> transform,
        CancellationToken ct = default);
}
```

#### 6.2.2 IManifestManager

```csharp
// Location: src/CodeBleach.Core/Interfaces/IManifestManager.cs

/// <summary>
/// Manages manifest and cross-reference file operations.
/// </summary>
public interface IManifestManager
{
    /// <summary>Loads manifest from .codebleach directory.</summary>
    Task<Manifest?> LoadManifestAsync(string sanitizedPath, CancellationToken ct = default);
    
    /// <summary>Saves manifest to .codebleach directory.</summary>
    Task SaveManifestAsync(Manifest manifest, string sanitizedPath, CancellationToken ct = default);
    
    /// <summary>Generates and saves xref.md file.</summary>
    Task SaveXrefAsync(Manifest manifest, string sanitizedPath, CancellationToken ct = default);
    
    /// <summary>Checks if a path is a sanitized directory.</summary>
    bool IsSanitizedDirectory(string path);
}
```

### 6.3 File Filtering Rules

```csharp
// Location: src/CodeBleach.Core/Services/FileProcessor.cs

public static class FileExtensions
{
    public static readonly HashSet<string> Supported = new(StringComparer.OrdinalIgnoreCase)
    {
        // Code
        ".cs", ".fs", ".vb", ".js", ".ts", ".jsx", ".tsx", 
        ".py", ".go", ".rs", ".java", ".kt", ".swift", ".rb", ".php",
        // Config
        ".json", ".yaml", ".yml", ".xml", ".config", ".ini", ".toml", ".env",
        // Scripts
        ".sh", ".bash", ".ps1", ".psm1", ".bat", ".cmd",
        // Web
        ".html", ".htm", ".css", ".scss", ".less",
        // Data
        ".sql", ".graphql",
        // Docs
        ".md", ".txt", ".rst"
    };
}

public static class IgnoredPatterns
{
    public static readonly string[] Directories =
    {
        ".codebleach", ".git", ".svn", ".hg",
        "node_modules", "vendor", "packages", ".nuget",
        "bin", "obj", "dist", "build", "target", "out",
        ".vs", ".vscode", ".idea"
    };
    
    public static readonly string[] Extensions =
    {
        ".exe", ".dll", ".so", ".dylib", ".pdb",
        ".png", ".jpg", ".jpeg", ".gif", ".ico", ".svg",
        ".zip", ".tar", ".gz", ".rar", ".7z",
        ".pfx", ".p12", ".cer", ".crt", ".key"
    };
}
```

### 6.4 Test Specifications

```csharp
// tests/CodeBleach.Tests/Services/FileProcessorTests.cs

public class FileProcessorTests
{
    private readonly IFileProcessor _sut;
    private string _tempDir = null!;
    
    public FileProcessorTests()
    {
        _sut = new FileProcessor();
    }
    
    [Theory]
    [InlineData("file.cs", true)]
    [InlineData("file.json", true)]
    [InlineData("file.md", true)]
    [InlineData("file.dll", false)]
    [InlineData("file.png", false)]
    [InlineData("file.exe", false)]
    public void ShouldProcess_WithVariousExtensions_ReturnsCorrectly(
        string fileName, bool expected)
    {
        _sut.ShouldProcess(fileName).Should().Be(expected);
    }
    
    [Theory]
    [InlineData("node_modules/package/index.js", true)]
    [InlineData(".git/config", true)]
    [InlineData("bin/Debug/app.dll", true)]
    [InlineData("src/Program.cs", false)]
    public void ShouldIgnore_WithVariousPaths_ReturnsCorrectly(
        string path, bool expected)
    {
        _sut.ShouldIgnore(path).Should().Be(expected);
    }
    
    [Fact]
    public async Task CopyDirectoryAsync_CreatesCorrectStructure()
    {
        // Arrange
        var source = CreateTempDirectoryWithFiles();
        var dest = Path.Combine(_tempDir, "dest");
        
        // Act
        await _sut.CopyDirectoryAsync(source, dest);
        
        // Assert
        Directory.Exists(dest).Should().BeTrue();
        File.Exists(Path.Combine(dest, "src", "Program.cs")).Should().BeTrue();
    }
    
    [Fact]
    public async Task CopyDirectoryAsync_SkipsIgnoredDirectories()
    {
        // Arrange
        var source = CreateTempDirectoryWithFiles();
        Directory.CreateDirectory(Path.Combine(source, "node_modules"));
        var dest = Path.Combine(_tempDir, "dest");
        
        // Act
        await _sut.CopyDirectoryAsync(source, dest);
        
        // Assert
        Directory.Exists(Path.Combine(dest, "node_modules")).Should().BeFalse();
    }
}

// tests/CodeBleach.Tests/Services/ManifestManagerTests.cs

public class ManifestManagerTests
{
    [Fact]
    public async Task SaveAndLoad_RoundTrip_PreservesData()
    {
        // Arrange
        var manager = new ManifestManager();
        var tempDir = CreateTempDirectory();
        
        var manifest = new Manifest
        {
            Version = "1.0",
            SourcePath = "/original",
            DestinationPath = tempDir,
            CreatedAt = DateTime.UtcNow,
            Mappings = new MappingTable
            {
                Forward = { ["ProductionDB"] = "SERVER_0" },
                Reverse = { ["SERVER_0"] = "ProductionDB" },
                Counters = { ["SERVER"] = 1 }
            },
            ProcessedFiles = ["src/Program.cs"],
            Stats = new SanitizationStats
            {
                TotalFiles = 10,
                ProcessedFiles = 5,
                SkippedFiles = 5,
                TotalReplacements = 3,
                UniqueValuesReplaced = 2,
                ProcessingTimeMs = 100
            }
        };
        
        // Act
        await manager.SaveManifestAsync(manifest, tempDir);
        var loaded = await manager.LoadManifestAsync(tempDir);
        
        // Assert
        loaded.Should().NotBeNull();
        loaded!.SourcePath.Should().Be("/original");
        loaded.Mappings.Forward.Should().ContainKey("ProductionDB");
    }
    
    [Fact]
    public void IsSanitizedDirectory_WithManifest_ReturnsTrue()
    {
        // Arrange
        var tempDir = CreateTempDirectory();
        Directory.CreateDirectory(Path.Combine(tempDir, ".codebleach"));
        File.WriteAllText(
            Path.Combine(tempDir, ".codebleach", "manifest.json"), 
            "{}");
        
        var manager = new ManifestManager();
        
        // Act & Assert
        manager.IsSanitizedDirectory(tempDir).Should().BeTrue();
    }
}
```

### 6.5 Acceptance Criteria

- [ ] Files correctly filtered by extension
- [ ] Ignored directories skipped during copy
- [ ] Manifest serializes/deserializes correctly
- [ ] xref.md generated with correct format
- [ ] Large directories processed efficiently
- [ ] .gitignore patterns respected (if present)

---

## 7. Phase 5: CLI Commands

**Duration:** 2 days  
**Objective:** Implement all CLI commands with proper UX  
**Dependencies:** All previous phases

### 7.1 Tasks

| Task ID | Task | Priority | Est. | Test First |
|---------|------|----------|------|------------|
| P5-001 | Setup `System.CommandLine` root | P0 | 30m | |
| P5-002 | Write `SanitizeCommandTests` | P0 | 1h | ✅ |
| P5-003 | Implement `SanitizeCommand` | P0 | 1.5h | |
| P5-004 | Write `RestoreCommandTests` | P0 | 45m | ✅ |
| P5-005 | Implement `RestoreCommand` | P0 | 1h | |
| P5-006 | Write `StatusCommandTests` | P1 | 30m | ✅ |
| P5-007 | Implement `StatusCommand` | P1 | 30m | |
| P5-008 | Write dry-run output tests | P0 | 45m | ✅ |
| P5-009 | Implement dry-run output formatting | P0 | 1h | |
| P5-010 | Implement DI container setup | P0 | 30m | |
| P5-011 | End-to-end CLI tests | P0 | 1h | ✅ |
| P5-012 | Manual testing on Windows/macOS/Linux | P0 | 1h | |

### 7.2 Command Structure

```csharp
// Location: src/CodeBleach/Program.cs

var rootCommand = new RootCommand("Sanitize code before sharing with AI assistants")
{
    SanitizeCommand.Create(),
    RestoreCommand.Create(),
    StatusCommand.Create()
};

// Default command (no subcommand = sanitize)
rootCommand.SetHandler(async (context) =>
{
    // If first arg is a path, treat as sanitize command
    // Otherwise show help
});

return await rootCommand.InvokeAsync(args);
```

### 7.3 Command Specifications

#### 7.3.1 SanitizeCommand

```csharp
// Location: src/CodeBleach/Commands/SanitizeCommand.cs

public static class SanitizeCommand
{
    public static Command Create()
    {
        var sourceArg = new Argument<DirectoryInfo>(
            "source",
            "Directory to sanitize");
        
        var outputOption = new Option<DirectoryInfo?>(
            new[] { "--output", "-o" },
            "Output directory (default: <source>-sanitize)");
        
        var dryRunOption = new Option<bool>(
            new[] { "--dry-run", "-n" },
            "Preview changes without modifying files");
        
        var verboseOption = new Option<bool>(
            new[] { "--verbose", "-v" },
            "Show detailed output with diffs");
        
        var forceOption = new Option<bool>(
            new[] { "--force", "-f" },
            "Overwrite existing output directory");
        
        var command = new Command("sanitize", "Sanitize a directory")
        {
            sourceArg,
            outputOption,
            dryRunOption,
            verboseOption,
            forceOption
        };
        
        command.SetHandler(HandleAsync, 
            sourceArg, outputOption, dryRunOption, verboseOption, forceOption);
        
        return command;
    }
}
```

#### 7.3.2 Output Format

```csharp
// Standard output format (no emojis!)
public static class OutputFormatter
{
    public static void WriteSummary(SanitizationStats stats)
    {
        Console.WriteLine();
        Console.WriteLine("Summary:");
        Console.WriteLine($"  Files copied:     {stats.TotalFiles}");
        Console.WriteLine($"  Files processed:  {stats.ProcessedFiles}");
        Console.WriteLine($"  Files skipped:    {stats.SkippedFiles} (binary/ignored)");
        Console.WriteLine($"  Replacements:     {stats.TotalReplacements}");
        Console.WriteLine($"  Unique values:    {stats.UniqueValuesReplaced}");
        Console.WriteLine();
        Console.WriteLine($"Done in {stats.ProcessingTimeMs}ms");
    }
    
    public static void WriteDryRunPreview(IEnumerable<SanitizationMatch> matches)
    {
        Console.WriteLine("DRY RUN - No files will be modified");
        Console.WriteLine();
        
        foreach (var group in matches.GroupBy(m => m.FilePath))
        {
            Console.WriteLine($"  {group.Key}:");
            foreach (var match in group)
            {
                Console.WriteLine($"    Line {match.LineNumber}:  {match.LineContent}");
                Console.WriteLine($"             {match.OriginalValue} -> {match.Alias}");
                Console.WriteLine();
            }
        }
    }
    
    public static void WriteError(string message)
    {
        Console.Error.WriteLine($"Error: {message}");
    }
}
```

### 7.4 Integration Test Specifications

```csharp
// tests/CodeBleach.IntegrationTests/CliTests.cs

public class CliTests : IAsyncLifetime
{
    private string _tempDir = null!;
    
    public Task InitializeAsync()
    {
        _tempDir = Path.Combine(Path.GetTempPath(), $"codebleach-test-{Guid.NewGuid()}");
        Directory.CreateDirectory(_tempDir);
        return Task.CompletedTask;
    }
    
    public Task DisposeAsync()
    {
        Directory.Delete(_tempDir, recursive: true);
        return Task.CompletedTask;
    }
    
    [Fact]
    public async Task Sanitize_ValidDirectory_CreatesOutput()
    {
        // Arrange
        var sourceDir = CreateTestProject();
        
        // Act
        var result = await RunCliAsync($"sanitize {sourceDir}");
        
        // Assert
        result.ExitCode.Should().Be(0);
        Directory.Exists($"{sourceDir}-sanitize").Should().BeTrue();
        File.Exists(Path.Combine($"{sourceDir}-sanitize", ".codebleach", "manifest.json"))
            .Should().BeTrue();
    }
    
    [Fact]
    public async Task Sanitize_DryRun_NoFilesCreated()
    {
        // Arrange
        var sourceDir = CreateTestProject();
        
        // Act
        var result = await RunCliAsync($"sanitize {sourceDir} --dry-run");
        
        // Assert
        result.ExitCode.Should().Be(0);
        result.StdOut.Should().Contain("DRY RUN");
        Directory.Exists($"{sourceDir}-sanitize").Should().BeFalse();
    }
    
    [Fact]
    public async Task Restore_InSanitizedDirectory_RestoresFiles()
    {
        // Arrange
        var sourceDir = CreateTestProject();
        await RunCliAsync($"sanitize {sourceDir}");
        var sanitizedDir = $"{sourceDir}-sanitize";
        
        // Verify sanitized
        var sanitizedContent = await File.ReadAllTextAsync(
            Path.Combine(sanitizedDir, "appsettings.json"));
        sanitizedContent.Should().Contain("SERVER_0");
        
        // Act
        var result = await RunCliAsync("restore", workingDirectory: sanitizedDir);
        
        // Assert
        result.ExitCode.Should().Be(0);
        var restoredContent = await File.ReadAllTextAsync(
            Path.Combine(sanitizedDir, "appsettings.json"));
        restoredContent.Should().Contain("ProductionDB");
        restoredContent.Should().NotContain("SERVER_0");
    }
    
    [Fact]
    public async Task RoundTrip_SanitizeAndRestore_PreservesContent()
    {
        // Arrange
        var sourceDir = CreateTestProject();
        var originalContent = await File.ReadAllTextAsync(
            Path.Combine(sourceDir, "appsettings.json"));
        
        // Act - Sanitize
        await RunCliAsync($"sanitize {sourceDir}");
        var sanitizedDir = $"{sourceDir}-sanitize";
        
        // Act - Restore
        await RunCliAsync("restore", workingDirectory: sanitizedDir);
        
        // Assert
        var restoredContent = await File.ReadAllTextAsync(
            Path.Combine(sanitizedDir, "appsettings.json"));
        restoredContent.Should().Be(originalContent);
    }
    
    private string CreateTestProject()
    {
        var projectDir = Path.Combine(_tempDir, $"project-{Guid.NewGuid():N}");
        Directory.CreateDirectory(projectDir);
        
        File.WriteAllText(
            Path.Combine(projectDir, "appsettings.json"),
            """
            {
              "ConnectionStrings": {
                "Default": "Server=ProductionDB;Database=users"
              },
              "Redis": {
                "Host": "192.168.1.100"
              }
            }
            """);
        
        File.WriteAllText(
            Path.Combine(projectDir, "Program.cs"),
            """
            var server = "ProductionDB";
            var ip = "192.168.1.100";
            """);
        
        return projectDir;
    }
    
    private async Task<CliResult> RunCliAsync(string args, string? workingDirectory = null)
    {
        // Implementation using Process to run `dotnet run` or compiled exe
    }
}
```

### 7.5 Acceptance Criteria

- [ ] All CLI commands work as specified
- [ ] Exit codes: 0 = success, 1 = error, 2 = invalid args
- [ ] Errors written to stderr
- [ ] No emojis in any output
- [ ] Dry-run shows accurate preview
- [ ] Verbose mode shows unified diffs
- [ ] Works on Windows, macOS, Linux
- [ ] Global tool installs correctly via `dotnet tool install`

---

## 8. Dependency Graph

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           DEPENDENCY GRAPH                                   │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│                              ┌───────────────┐                               │
│                              │   Phase 0     │                               │
│                              │  Scaffolding  │                               │
│                              └───────┬───────┘                               │
│                                      │                                       │
│                                      ▼                                       │
│                              ┌───────────────┐                               │
│                              │   Phase 1     │                               │
│                              │    Models     │                               │
│                              └───────┬───────┘                               │
│                                      │                                       │
│                     ┌────────────────┼────────────────┐                      │
│                     │                │                │                      │
│                     ▼                ▼                ▼                      │
│              ┌───────────┐    ┌───────────┐    (shared models)               │
│              │  Phase 2  │    │  Phase 3  │                                  │
│              │ Sanitizer │    │ Restorer  │                                  │
│              └─────┬─────┘    └─────┬─────┘                                  │
│                    │                │                                        │
│                    └────────┬───────┘                                        │
│                             │                                                │
│                             ▼                                                │
│                      ┌───────────────┐                                       │
│                      │   Phase 4     │                                       │
│                      │ File/Manifest │                                       │
│                      └───────┬───────┘                                       │
│                              │                                               │
│                              ▼                                               │
│                       ┌───────────────┐                                      │
│                       │   Phase 5     │                                      │
│                       │      CLI      │                                      │
│                       └───────────────┘                                      │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

**Critical Path:** Phase 0 → Phase 1 → Phase 2 → Phase 4 → Phase 5

**Parallel Opportunities:**
- Phase 2 (Sanitizer) and Phase 3 (Restorer) can be developed in parallel after Phase 1

---

## 9. Interface Contracts

### 9.1 Complete Interface Summary

| Interface | Methods | Max | Location |
|-----------|---------|-----|----------|
| `ISanitizer` | 1 | 10 | `Core/Interfaces/ISanitizer.cs` |
| `IRestorer` | 1 | 10 | `Core/Interfaces/IRestorer.cs` |
| `IRuleRegistry` | 5 | 10 | `Core/Interfaces/IRuleRegistry.cs` |
| `IFileProcessor` | 5 | 10 | `Core/Interfaces/IFileProcessor.cs` |
| `IManifestManager` | 4 | 10 | `Core/Interfaces/IManifestManager.cs` |

### 9.2 Contract Details

```csharp
// All interfaces must follow ISP - max 10 methods

// ISanitizer - Single responsibility: sanitize content
public interface ISanitizer
{
    SanitizationResult Sanitize(string content, MappingTable mappings);
}

// IRestorer - Single responsibility: restore content
public interface IRestorer
{
    RestoreResult Restore(string content, MappingTable mappings);
}

// IRuleRegistry - Single responsibility: manage rules
public interface IRuleRegistry
{
    IReadOnlyList<SanitizationRule> GetActiveRules();
    SanitizationRule? GetRule(string ruleId);
    void AddRule(SanitizationRule rule);
    void DisableRule(string ruleId);
    void EnableRule(string ruleId);
}

// IFileProcessor - Single responsibility: file operations
public interface IFileProcessor
{
    bool ShouldProcess(string filePath);
    bool ShouldIgnore(string path);
    IEnumerable<string> GetFilesToProcess(string directoryPath);
    Task CopyDirectoryAsync(string source, string dest, CancellationToken ct = default);
    Task<FileProcessingResult> ProcessFileAsync(string source, string dest, 
        Func<string, string> transform, CancellationToken ct = default);
}

// IManifestManager - Single responsibility: manifest operations
public interface IManifestManager
{
    Task<Manifest?> LoadManifestAsync(string path, CancellationToken ct = default);
    Task SaveManifestAsync(Manifest manifest, string path, CancellationToken ct = default);
    Task SaveXrefAsync(Manifest manifest, string path, CancellationToken ct = default);
    bool IsSanitizedDirectory(string path);
}
```

---

## 10. Risk Assessment

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|------------|
| Regex performance on large files | Medium | Medium | Pre-compile patterns, use Span<T> |
| False positives (over-sanitization) | Medium | High | Comprehensive test fixtures, exception lists |
| Platform-specific path issues | Low | Medium | Use `Path.Combine()`, test on all platforms |
| Partial file writes on crash | Low | High | Write to temp file, then atomic rename |
| Manifest corruption | Low | High | Validate JSON on load, backup before modify |
| NuGet publishing failure | Low | Medium | Dry-run pack locally, verify before tag |

### 10.1 Mitigation Strategies

**Performance:**
- Pre-compile all regex patterns in `RuleRegistry`
- Use `StringBuilder` for content transformation
- Batch file I/O operations
- Add performance tests with threshold assertions

**Correctness:**
- Extensive test fixtures with real-world code samples
- Round-trip tests for every transformation
- Manual testing on production codebases before release

**Reliability:**
- Atomic file operations (write to temp, rename)
- Manifest validation on load
- Graceful error messages with recovery suggestions

---

## 11. Definition of Done

### 11.1 Per-Task DoD

- [ ] Test written first (TDD)
- [ ] Test passes
- [ ] Implementation complete
- [ ] Code reviewed (self-review minimum)
- [ ] No compiler warnings
- [ ] XML documentation on public APIs

### 11.2 Per-Phase DoD

- [ ] All tasks complete
- [ ] All tests pass
- [ ] Code coverage ≥ 80% for phase
- [ ] Integration tests pass
- [ ] Manual smoke test passes

### 11.3 Release DoD

- [ ] All phases complete
- [ ] All tests pass (unit, integration, e2e)
- [ ] Code coverage ≥ 80% overall
- [ ] Manual testing on Windows, macOS, Linux
- [ ] `dotnet pack` succeeds
- [ ] `dotnet tool install` succeeds locally
- [ ] README.md complete
- [ ] CHANGELOG.md updated
- [ ] Version tag created
- [ ] GitHub Actions release workflow passes
- [ ] Package visible on NuGet.org

---

## Quick Reference: Commands

```bash
# Phase 0: Verify setup
dotnet build
dotnet test

# Phase 1-5: Run tests
dotnet test --filter "FullyQualifiedName~MappingTable"
dotnet test --filter "FullyQualifiedName~Sanitizer"
dotnet test --collect:"XPlat Code Coverage"

# Pre-release: Full verification
dotnet clean
dotnet restore
dotnet build -c Release
dotnet test -c Release
dotnet pack -c Release -o ./artifacts

# Release
git tag -a v1.0.0 -m "Release v1.0.0"
git push origin v1.0.0
```

---

**Document Control:**
- **Version:** 1.0
- **Author:** Architecture Team
- **Status:** Approved for Development
- **Next Review:** After Phase 5 completion

---

ROLE: architect STRICT=true

