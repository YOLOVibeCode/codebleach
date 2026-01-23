# CodeBleach Sanitization Algorithm Testing Specification

**Version:** 1.0  
**Created:** January 22, 2026  
**Purpose:** Formal verification of the sanitization algorithm correctness

---

## 1. Algorithm Overview

The CodeBleach sanitization algorithm performs the following operations:

```
INPUT: Content (string) + Rules (list of patterns) + MappingTable (empty or pre-populated)
OUTPUT: Sanitized content + Updated MappingTable + Match metadata

ALGORITHM:
1. For each rule (in order of priority):
   a. Find all matches of the pattern in content
   b. For each match, check if it's in the exceptions list
   c. Get or create an alias for the matched value
   d. Record the match with position, line number, and alias
2. Sort matches by position, then by length (longest first)
3. Filter out overlapping matches (keep first/longest)
4. Build result by replacing non-overlapping matches with aliases
5. Return sanitized content and match metadata
```

---

## 2. Critical Algorithm Properties to Test

### 2.1 Determinism
- Same input MUST always produce same output
- Alias generation MUST be deterministic
- Order of rules MUST be respected consistently

### 2.2 Completeness
- All sensitive data matching a rule MUST be detected
- No false negatives for defined patterns

### 2.3 Precision
- Only data matching patterns should be sanitized
- No false positives (collateral damage)
- Word boundaries must be respected

### 2.4 Reversibility
- `Restore(Sanitize(content)) == content` MUST hold
- Bidirectional mapping MUST be consistent
- No information loss during round-trip

### 2.5 Consistency
- Same value MUST always map to same alias
- Alias counters MUST increment predictably
- Cross-file consistency MUST be maintained

---

## 3. Overlapping Pattern Test Cases

### 3.1 Test: Longer Match Takes Priority

**Scenario:** Pattern A matches "PRODSRV01", Pattern B matches "PRODSRV01.acme-corp.com"

| Input | Pattern A (SERVER) | Pattern B (HOST) | Expected Winner |
|-------|-------------------|------------------|-----------------|
| `PRODSRV01.acme-corp.com` | Matches `PRODSRV01` | Matches entire FQDN | HOST (longer) |
| `PRODSRV01 is the server` | Matches `PRODSRV01` | No match | SERVER |

**Test:**
```csharp
[Fact]
public void Sanitize_OverlappingPatterns_LongerMatchWins()
{
    // PRODSRV01.acme-corp.com should match as HOST, not SERVER
    // Because the FQDN pattern is longer/more specific
}
```

### 3.2 Test: Earlier Position Takes Priority (Same Length)

**Scenario:** Two patterns match at different positions with same start

| Input | Expected |
|-------|----------|
| `SERVER_A SERVER_B` | Both sanitized, no overlap |
| `AB` where A=1-2, B=2-3 | Earlier pattern wins |

### 3.3 Test: Nested Pattern Handling

**Scenario:** Pattern matches substring of another match

| Input | Outer Pattern | Inner Pattern | Expected |
|-------|---------------|---------------|----------|
| `[LINKEDSRV01].[DBZMEW].[dbo].[TB00123]` | 4-part name | DBZMEW alone | 4-part wins (outer) |
| `DBZMEW.dbo.TB00123` | 3-part name | DBZMEW alone | 3-part wins |

**Test:**
```csharp
[Fact]
public void Sanitize_NestedPatterns_OuterPatternWins()
{
    // When DBZMEW appears inside [LINKEDSRV01].[DBZMEW].[dbo].[TB00123]
    // The entire 4-part name should be sanitized, not just DBZMEW
}
```

---

## 4. Rule Ordering Test Cases

### 4.1 Test: Rule Order Determines Processing Sequence

**Rules (in order):**
1. Order=1: 4-part bracketed SQL names
2. Order=2: 4-part unbracketed SQL names  
3. Order=3: 3-part SQL names (DB.dbo.Table)
4. Order=4: 2-part SQL names (DB.Table)
5. Order=5: Standalone database names

**Test:**
```csharp
[Fact]
public void Sanitize_RuleOrder_HigherPriorityProcessedFirst()
{
    // Rules with lower Order value should be processed first
    // This ensures specific patterns match before general ones
}
```

### 4.2 Test: Rule Order Affects Alias Generation

**Scenario:**
- Rule 1 (Order=1): Matches `DBZMEW.dbo.TB00123` → SCHEMA_0
- Rule 2 (Order=2): Matches `DBZMEW` standalone → DB_0

If Rule 1 is processed first and matches the entire 3-part name, Rule 2 should NOT also match `DBZMEW` within it.

---

## 5. Alias Consistency Test Cases

### 5.1 Test: Same Value Always Gets Same Alias

```csharp
[Fact]
public void Sanitize_SameValueMultipleTimes_SameAlias()
{
    var content = "PRODSRV01 connects to PRODSRV01 via PRODSRV01";
    // All three occurrences should map to SERVER_0
}
```

### 5.2 Test: Different Values Get Different Aliases

```csharp
[Fact]
public void Sanitize_DifferentValues_DifferentAliases()
{
    var content = "PRODSRV01 and PRODSRV02 and STGSRV01";
    // Should produce SERVER_0, SERVER_1, SERVER_2
}
```

### 5.3 Test: Counter Increments Per Prefix

```csharp
[Fact]
public void Sanitize_DifferentPrefixes_IndependentCounters()
{
    var content = "Server: PRODSRV01, IP: 10.0.0.1, Server: PRODSRV02";
    // Should produce: SERVER_0, IP_0, SERVER_1
    // (not SERVER_0, IP_1, SERVER_2)
}
```

### 5.4 Test: Pre-populated Mappings Reused

```csharp
[Fact]
public void Sanitize_ExistingMappings_AreReused()
{
    var mappings = new MappingTable();
    mappings.GetOrCreateAlias("PRODSRV01", "SERVER"); // Pre-create
    
    var content = "Connect to PRODSRV01";
    // Should use existing SERVER_0, not create SERVER_1
}
```

---

## 6. Bidirectional Mapping Test Cases

### 6.1 Test: Forward Mapping Correct

```csharp
[Fact]
public void MappingTable_ForwardMapping_CorrectlyStored()
{
    var mappings = new MappingTable();
    mappings.GetOrCreateAlias("PRODSRV01", "SERVER");
    
    mappings.Forward["PRODSRV01"].Should().Be("SERVER_0");
}
```

### 6.2 Test: Reverse Mapping Correct

```csharp
[Fact]
public void MappingTable_ReverseMapping_CorrectlyStored()
{
    var mappings = new MappingTable();
    mappings.GetOrCreateAlias("PRODSRV01", "SERVER");
    
    mappings.Reverse["SERVER_0"].Should().Be("PRODSRV01");
}
```

### 6.3 Test: Bidirectional Consistency

```csharp
[Fact]
public void MappingTable_Bidirectional_IsConsistent()
{
    var mappings = new MappingTable();
    mappings.GetOrCreateAlias("PRODSRV01", "SERVER");
    
    var alias = mappings.Forward["PRODSRV01"];
    var original = mappings.Reverse[alias];
    
    original.Should().Be("PRODSRV01");
}
```

---

## 7. Round-Trip Integrity Test Cases

### 7.1 Test: Identity Property

```
∀ content: Restore(Sanitize(content)) ≡ content
```

```csharp
[Theory]
[InlineData("Simple text with PRODSRV01")]
[InlineData("Multiple: PRODSRV01, STGSRV01, DEVSRV01")]
[InlineData("Mixed: PRODSRV01 at 10.0.0.1 in DBZMEW")]
[InlineData("Complex SQL with [LINKEDSRV01].[DBZMEW].[dbo].[TB00123]")]
public void RoundTrip_IdentityProperty_Holds(string original)
{
    var mappings = new MappingTable();
    var sanitized = _sanitizer.Sanitize(original, mappings);
    var restored = _restorer.Restore(sanitized.Content, mappings);
    
    restored.Content.Should().Be(original);
}
```

### 7.2 Test: Multiple Cycles Preserve Identity

```csharp
[Fact]
public void RoundTrip_MultipleCycles_IdentityPreserved()
{
    var original = "Server: PRODSRV01, DB: DBZMEW";
    var current = original;
    
    for (int i = 0; i < 10; i++)
    {
        var mappings = new MappingTable();
        var sanitized = _sanitizer.Sanitize(current, mappings);
        var restored = _restorer.Restore(sanitized.Content, mappings);
        current = restored.Content;
    }
    
    current.Should().Be(original);
}
```

### 7.3 Test: Partial Restoration Correct

```csharp
[Fact]
public void Restore_PartialAliases_OnlyRestoresKnown()
{
    var content = "SERVER_0 and UNKNOWN_ALIAS";
    var mappings = new MappingTable();
    mappings.Reverse["SERVER_0"] = "PRODSRV01";
    
    var restored = _restorer.Restore(content, mappings);
    
    restored.Content.Should().Contain("PRODSRV01");
    restored.Content.Should().Contain("UNKNOWN_ALIAS"); // Unchanged
    restored.UnmatchedAliases.Should().Contain("UNKNOWN_ALIAS");
}
```

---

## 8. Boundary Condition Test Cases

### 8.1 Test: Word Boundary Handling

```csharp
[Theory]
[InlineData("PRODSRV01", true)]          // Standalone
[InlineData("PRODSRV01_BACKUP", false)]  // Underscore breaks boundary
[InlineData("MYPRODSRV01", false)]       // Prefix breaks boundary
[InlineData("PRODSRV01.domain", true)]   // Dot is boundary
[InlineData("PRODSRV01:1433", true)]     // Colon is boundary
public void Sanitize_WordBoundaries_RespectedCorrectly(string input, bool shouldMatch)
{
    // Test that word boundaries are properly handled
}
```

### 8.2 Test: Start and End of Content

```csharp
[Fact]
public void Sanitize_AtStartOfContent_Matches()
{
    var content = "PRODSRV01 is the server";
    // Should match PRODSRV01 at position 0
}

[Fact]
public void Sanitize_AtEndOfContent_Matches()
{
    var content = "Server is PRODSRV01";
    // Should match PRODSRV01 at end
}
```

### 8.3 Test: Empty and Whitespace Content

```csharp
[Theory]
[InlineData("")]
[InlineData("   ")]
[InlineData("\n\n\n")]
[InlineData("\t\t")]
public void Sanitize_EmptyOrWhitespace_ReturnsUnchanged(string content)
{
    var mappings = new MappingTable();
    var result = _sanitizer.Sanitize(content, mappings);
    
    result.Content.Should().Be(content);
    result.WasSanitized.Should().BeFalse();
}
```

### 8.4 Test: Very Long Content

```csharp
[Fact]
public void Sanitize_VeryLongContent_CompletesSuccessfully()
{
    var content = string.Join("\n", 
        Enumerable.Repeat("Server: PRODSRV01, IP: 10.0.0.1", 10000));
    
    var mappings = new MappingTable();
    var sw = Stopwatch.StartNew();
    var result = _sanitizer.Sanitize(content, mappings);
    sw.Stop();
    
    sw.ElapsedMilliseconds.Should().BeLessThan(5000);
    result.WasSanitized.Should().BeTrue();
}
```

---

## 9. Special Character Test Cases

### 9.1 Test: Unicode Content Handling

```csharp
[Theory]
[InlineData("Server PRODSRV01 🖥️")]
[InlineData("数据库: PRODSRV01")]
[InlineData("Сервер: PRODSRV01")]
public void Sanitize_UnicodeContent_HandledCorrectly(string content)
{
    var mappings = new MappingTable();
    var sanitized = _sanitizer.Sanitize(content, mappings);
    var restored = _restorer.Restore(sanitized.Content, mappings);
    
    restored.Content.Should().Be(content);
}
```

### 9.2 Test: Regex Special Characters in Content

```csharp
[Theory]
[InlineData("Server: PRODSRV01 (production)")]
[InlineData("PRODSRV01 [primary]")]
[InlineData("PRODSRV01 {server}")]
[InlineData("$PRODSRV01")]
[InlineData("PRODSRV01?")]
[InlineData("PRODSRV01*")]
[InlineData("PRODSRV01+")]
public void Sanitize_RegexSpecialChars_HandledCorrectly(string content)
{
    var mappings = new MappingTable();
    var sanitized = _sanitizer.Sanitize(content, mappings);
    
    sanitized.Content.Should().NotContain("PRODSRV01");
}
```

### 9.3 Test: Newlines and Tabs

```csharp
[Fact]
public void Sanitize_MultilineContent_AllLinesProcessed()
{
    var content = "Line1: PRODSRV01\nLine2: STGSRV01\nLine3: DEVSRV01";
    var mappings = new MappingTable();
    
    var result = _sanitizer.Sanitize(content, mappings);
    
    result.Matches.Should().HaveCount(3);
    result.Matches.Select(m => m.LineNumber).Should().Equal(1, 2, 3);
}
```

---

## 10. Exception Handling Test Cases

### 10.1 Test: Exception List Respected

```csharp
[Fact]
public void Sanitize_ExceptionListValue_NotSanitized()
{
    var rule = new SanitizationRule
    {
        Pattern = @"\b\w+DB\b",
        Prefix = "DB",
        Exceptions = ["ProductionDB", "TestDB"]
    };
    
    var content = "ProductionDB and StagingDB";
    // ProductionDB should NOT be sanitized
    // StagingDB SHOULD be sanitized
}
```

### 10.2 Test: Exception List Case Sensitivity

```csharp
[Fact]
public void Sanitize_ExceptionList_IsCaseInsensitive()
{
    var rule = new SanitizationRule
    {
        Exceptions = ["productiondb"]
    };
    
    var content = "ProductionDB"; // Different case
    // Should still be excluded due to OrdinalIgnoreCase
}
```

---

## 11. Alias Format Test Cases

### 11.1 Test: Alias Format Correct

```csharp
[Theory]
[InlineData("SERVER", 0, "SERVER_0")]
[InlineData("SERVER", 1, "SERVER_1")]
[InlineData("IP", 0, "IP_0")]
[InlineData("SCHEMA", 10, "SCHEMA_10")]
public void GetOrCreateAlias_Format_Correct(string prefix, int expectedIndex, string expectedAlias)
{
    // Verify alias format is {PREFIX}_{N}
}
```

### 11.2 Test: Alias Uniqueness

```csharp
[Fact]
public void MappingTable_AllAliases_AreUnique()
{
    var mappings = new MappingTable();
    
    for (int i = 0; i < 100; i++)
    {
        mappings.GetOrCreateAlias($"Server{i}", "SERVER");
    }
    
    mappings.Forward.Values.Should().OnlyHaveUniqueItems();
}
```

---

## 12. Performance Test Cases

### 12.1 Test: Linear Scaling

```csharp
[Fact]
public void Sanitize_Performance_ScalesLinearly()
{
    var times = new List<(int size, long ms)>();
    
    foreach (var size in new[] { 100, 1000, 10000 })
    {
        var content = GenerateContent(size);
        var sw = Stopwatch.StartNew();
        _sanitizer.Sanitize(content, new MappingTable());
        times.Add((size, sw.ElapsedMilliseconds));
    }
    
    // Verify roughly linear scaling
    // 10x more content should take ~10x more time (with tolerance)
}
```

### 12.2 Test: Many Rules Performance

```csharp
[Fact]
public void Sanitize_ManyRules_StillPerformant()
{
    // Add 100 rules
    for (int i = 0; i < 100; i++)
    {
        _ruleRegistry.AddRule(new SanitizationRule { ... });
    }
    
    var content = GenerateLargeContent();
    var sw = Stopwatch.StartNew();
    _sanitizer.Sanitize(content, new MappingTable());
    
    sw.ElapsedMilliseconds.Should().BeLessThan(10000);
}
```

---

## 13. Test Matrix Summary

| Category | Test Count | Priority | Status |
|----------|------------|----------|--------|
| Overlapping Patterns | 5 | P0 | Required |
| Rule Ordering | 3 | P0 | Required |
| Alias Consistency | 6 | P0 | Required |
| Bidirectional Mapping | 4 | P0 | Required |
| Round-Trip Integrity | 5 | P0 | Required |
| Boundary Conditions | 8 | P1 | Required |
| Special Characters | 5 | P1 | Required |
| Exception Handling | 3 | P1 | Required |
| Alias Format | 3 | P2 | Recommended |
| Performance | 4 | P2 | Recommended |
| **TOTAL** | **46** | | |

---

## 14. Formal Invariants

The following invariants MUST hold for all inputs:

### Invariant 1: Determinism
```
∀ content, rules:
  Sanitize(content, rules, empty_mappings) = Sanitize(content, rules, empty_mappings)
```

### Invariant 2: Round-Trip Identity
```
∀ content, rules:
  let (sanitized, mappings) = Sanitize(content, rules)
  Restore(sanitized, mappings) = content
```

### Invariant 3: Mapping Consistency
```
∀ value, prefix:
  let alias = GetOrCreateAlias(value, prefix)
  Forward[value] = alias ∧ Reverse[alias] = value
```

### Invariant 4: No Overlap
```
∀ match_i, match_j in result.Matches where i ≠ j:
  match_i.EndIndex ≤ match_j.StartIndex ∨ match_j.EndIndex ≤ match_i.StartIndex
```

### Invariant 5: Completeness
```
∀ pattern in rules, match in content:
  if pattern.Matches(match) ∧ match ∉ pattern.Exceptions
  then ∃ m in result.Matches: m.OriginalValue = match
```

---

## 15. Test Execution Checklist

Before release, ensure:

- [ ] All 46 algorithm tests pass
- [ ] No regressions in existing 217 tests
- [ ] Performance benchmarks within targets
- [ ] Round-trip tests pass with 10+ cycles
- [ ] Unicode content handled correctly
- [ ] Edge cases all covered
- [ ] Overlapping pattern resolution correct
- [ ] Alias generation deterministic

---

**Document Control:**
- **Author:** CodeBleach QA Team
- **Version:** 1.0
- **Status:** Specification Complete
- **Next Action:** Implement missing algorithm tests
