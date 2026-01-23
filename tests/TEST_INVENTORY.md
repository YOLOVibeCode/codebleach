# CodeBleach Test Inventory

**Last Run:** All 269 tests passed  
**Test Framework:** xUnit + FluentAssertions  
**Projects:** `CodeBleach.Tests` (Unit Tests) + `CodeBleach.IntegrationTests`

---

## Summary by Category

| Category | Test File | Test Count | Status |
|----------|-----------|------------|--------|
| **Algorithm Correctness** | `AlgorithmCorrectnessTests.cs` | 52 | PASS |
| **Sanitizer** | `SanitizerTests.cs` | 7 | PASS |
| **Restorer** | `RestorerTests.cs` | 5 | PASS |
| **Round-Trip** | `RoundTripTests.cs` | 17 | PASS |
| **Custom Rule Loader** | `CustomRuleLoaderTests.cs` | 12 | PASS |
| **Global Config Locator** | `GlobalConfigLocatorTests.cs` | 10 | PASS |
| **Enterprise Markdown** | `EnterpriseMarkdownSanitizerTests.cs` | 17 | PASS |
| **SQL Linked Server** | `SqlLinkedServerTests.cs` | 15 | PASS |
| **Built-In Rules** | `BuiltInRulesTests.cs` | 11 | PASS |
| **Rule Registry** | `RuleRegistryTests.cs` | 7 | PASS |
| **Enterprise Rules** | `EnterpriseRulesTests.cs` | 66 | PASS |
| **Mapping Table** | `MappingTableTests.cs` | 8 | PASS |
| **Integration Tests** | `EnterpriseFixtureTests.cs` | 14 | PASS |
| **Placeholder Tests** | `UnitTest1.cs` (x2) | 2 | PASS |
| **Total** | | **269** | **PASS** |

---

## Detailed Test List

### 1. Algorithm Correctness Tests (`AlgorithmCorrectnessTests.cs`)

**Purpose:** Verifies determinism, completeness, precision, reversibility, and consistency of the sanitization algorithm.

#### Section 1: Determinism Tests
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Algorithm_IsDeterministic_SameInputSameOutput` | Same input produces same output across multiple runs | PASS |
| `Algorithm_AliasGeneration_IsDeterministic` | Alias generation is consistent | PASS |

#### Section 2: Overlapping Pattern Tests
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Overlapping_LongerMatchWins` | FQDN pattern wins over server name pattern | PASS |
| `Overlapping_NonOverlappingBothMatch` | Server name and IP both matched | PASS |
| `Overlapping_AdjacentPatternsNoConflict` | Adjacent patterns (PRODSRV01:1433) work correctly | PASS |

#### Section 3: Alias Consistency Tests
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Consistency_SameValueSameAlias` | Repeated values get same alias | PASS |
| `Consistency_DifferentValuesDifferentAliases` | Different values get different aliases | PASS |
| `Consistency_DifferentPrefixesIndependentCounters` | Each prefix has independent counter | PASS |
| `Consistency_PrePopulatedMappingsReused` | Pre-populated mappings are reused | PASS |

#### Section 4: Bidirectional Mapping Tests
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Bidirectional_ForwardMappingCorrect` | Forward mapping works correctly | PASS |
| `Bidirectional_ReverseMappingCorrect` | Reverse mapping works correctly | PASS |
| `Bidirectional_ConsistencyHolds` | Forward and reverse are inverses | PASS |

#### Section 5: Round-Trip Integrity Tests
| Test Name | Description | Status |
|-----------|-------------|--------|
| `RoundTrip_IdentityPropertyHolds` (5 cases) | Restore(Sanitize(x)) = x | PASS |
| `RoundTrip_MultipleCyclesPreserveIdentity` | 10 cycles preserve identity | PASS |
| `RoundTrip_NoInformationLoss` | All original values in mappings | PASS |

#### Section 6: Boundary Condition Tests
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Boundary_WordBoundariesRespected` (6 cases) | Word boundaries handled correctly | PASS |
| `Boundary_AtStartOfContent` | Match at content start | PASS |
| `Boundary_AtEndOfContent` | Match at content end | PASS |
| `Boundary_EmptyOrWhitespaceReturnsUnchanged` (4 cases) | Empty/whitespace unchanged | PASS |

#### Section 7: Special Character Tests
| Test Name | Description | Status |
|-----------|-------------|--------|
| `SpecialChars_UnicodeHandledCorrectly` (4 cases) | Emoji, Chinese, Cyrillic, Hebrew | PASS |
| `SpecialChars_RegexSpecialCharsHandled` (6 cases) | Regex special chars don't break | PASS |
| `SpecialChars_NewlinesProcessedCorrectly` | Multi-line content with line numbers | PASS |

#### Section 8: Alias Format Tests
| Test Name | Description | Status |
|-----------|-------------|--------|
| `AliasFormat_CorrectFormat` (5 cases) | PREFIX_N format correct | PASS |
| `AliasFormat_AllAliasesUnique` | 100 aliases are unique | PASS |

#### Section 9: Performance Tests
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Performance_LargeContentCompletesInTime` | 1000 lines < 3 seconds | PASS |
| `Performance_ManyUniqueValuesHandled` | 1000 unique values < 2 seconds | PASS |

#### Section 10: Completeness Tests
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Completeness_AllMatchesFound` | 7 known sensitive values found | PASS |
| `Completeness_NoFalsePositives` | No changes to non-sensitive data | PASS |

---

### 2. Sanitizer Tests (`SanitizerTests.cs`)

**Purpose:** Core sanitizer functionality testing.

| Test Name | Description | Status |
|-----------|-------------|--------|
| `Sanitize_WithServerName_ReplacesWithAlias` | Basic server name replacement | PASS |
| `Sanitize_WithMultipleOccurrences_UsesSameAlias` | Same value = same alias | PASS |
| `Sanitize_WithPrivateIP_ReplacesWithAlias` (3 cases) | 192.168.x.x, 10.x.x.x, 172.16.x.x | PASS |
| `Sanitize_WithNoSensitiveData_ReturnsUnchanged` | No changes to safe content | PASS |
| `Sanitize_WithEmptyContent_ReturnsEmpty` | Empty string handling | PASS |
| `Sanitize_WithExistingMappings_ReusesAliases` | Pre-populated mappings reused | PASS |

---

### 3. Restorer Tests (`RestorerTests.cs`)

**Purpose:** Content restoration from aliases.

| Test Name | Description | Status |
|-----------|-------------|--------|
| `Restore_WithAliases_ReplacesWithOriginals` | Basic restoration | PASS |
| `Restore_WithNoAliases_ReturnsUnchanged` | No aliases = no changes | PASS |
| `Restore_WithLongerAliasFirst_RestoresCorrectly` | SERVER_10 before SERVER_1 | PASS |
| `Restore_WithUnmatchedAlias_ReportsIt` | Reports unmatched aliases | PASS |
| `Restore_WithEmptyContent_ReturnsEmpty` | Empty string handling | PASS |

---

### 4. Round-Trip Tests (`RoundTripTests.cs`)

**Purpose:** End-to-end sanitization and restoration verification.

#### Basic Round-Trip
| Test Name | Description | Status |
|-----------|-------------|--------|
| `RoundTrip_SimpleServerName_RestoresPerfectly` | Single server name | PASS |
| `RoundTrip_MultipleServers_RestoresPerfectly` | Multiple servers | PASS |
| `RoundTrip_PrivateIPs_RestoresPerfectly` | IP addresses | PASS |
| `RoundTrip_DatabaseNames_RestoresPerfectly` | DB prefix names | PASS |
| `RoundTrip_TableNames_RestoresPerfectly` | TB/TA prefixed tables | PASS |

#### Complex Content
| Test Name | Description | Status |
|-----------|-------------|--------|
| `RoundTrip_SqlQuery_RestoresPerfectly` | Multi-line SQL | PASS |
| `RoundTrip_MarkdownDocument_RestoresPerfectly` | Tables, code blocks, links | PASS |
| `RoundTrip_JsonConfig_RestoresPerfectly` | JSON configuration | PASS |
| `RoundTrip_YamlConfig_RestoresPerfectly` | YAML configuration | PASS |

#### AI Modification Simulation
| Test Name | Description | Status |
|-----------|-------------|--------|
| `RoundTrip_AfterAiAddsCode_PreservesAdditions` | AI adds new code | PASS |
| `RoundTrip_AfterAiModifiesLogic_PreservesChanges` | AI changes code | PASS |
| `RoundTrip_AfterAiReorganizesCode_RestoresCorrectly` | AI reorders code | PASS |

#### Edge Cases
| Test Name | Description | Status |
|-----------|-------------|--------|
| `RoundTrip_MultipleCycles_RemainsPerfect` | 5 sanitize/restore cycles | PASS |
| `RoundTrip_NoSensitiveData_RemainsUnchanged` | No sensitive data | PASS |
| `RoundTrip_EmptyContent_RemainsEmpty` | Empty string | PASS |
| `RoundTrip_WhitespaceOnly_RemainsUnchanged` | Whitespace only | PASS |
| `RoundTrip_LongDocument_RestoresPerfectly` | 100 lines | PASS |
| `RoundTrip_UnicodeContent_RestoresPerfectly` | Emoji content | PASS |
| `RoundTrip_SpecialCharacters_RestoresPerfectly` | Special chars | PASS |

---

### 5. Custom Rule Loader Tests (`CustomRuleLoaderTests.cs`)

**Purpose:** Loading and merging custom rule configurations.

| Test Name | Description | Status |
|-----------|-------------|--------|
| `LoadFromFile_WithValidJson_ReturnsRules` | Valid JSON parsing | PASS |
| `LoadFromFile_WithNonExistentFile_ReturnsEmpty` | Missing file handling | PASS |
| `LoadFromFile_WithDisabledRule_DoesNotReturnIt` | Disabled rules filtered | PASS |
| `LoadFromMultipleFiles_WithNoFiles_ReturnsEmpty` | No files = empty | PASS |
| `LoadFromMultipleFiles_WithSingleFile_ReturnsRules` | Single file loading | PASS |
| `LoadFromMultipleFiles_WithMultipleFiles_MergesRules` | Multiple file merge | PASS |
| `LoadFromMultipleFiles_WithSameRuleIdInMultipleFiles_LaterFileOverrides` | Override behavior | PASS |
| `LoadFromMultipleFiles_WithDisabledRuleInLaterFile_DisablesRule` | Late disable | PASS |
| `LoadFromMultipleFiles_WithInvalidFile_SkipsInvalidFile` | Invalid JSON skipped | PASS |
| `FindConfigFile_FromCurrentDirectory_FindsFile` | Current dir search | PASS |
| `FindConfigFile_FromParentDirectory_FindsFile` | Parent dir search | PASS |
| `FindConfigFile_WhenNotFound_ReturnsNull` | Not found = null | PASS |

---

### 6. Global Config Locator Tests (`GlobalConfigLocatorTests.cs`)

**Purpose:** Cross-platform global configuration discovery.

| Test Name | Description | Status |
|-----------|-------------|--------|
| `GetGlobalConfigDirectory_OnWindows_ReturnsAppDataPath` | Windows path | PASS |
| `GetGlobalConfigDirectory_OnLinuxMacOS_ReturnsConfigPath` | Unix path | PASS |
| `GetGlobalConfigDirectory_WithEnvironmentVariable_ReturnsEnvPath` | Env override | PASS |
| `GetGlobalRulesFilePath_ReturnsCorrectFileName` | rules.json path | PASS |
| `GlobalRulesFileExists_WhenFileDoesNotExist_ReturnsFalse` | Existence check | PASS |
| `GetConfigFilePaths_WithNoConfigFiles_ReturnsEmptyList` | No configs | PASS |
| `GetConfigFilePaths_WithProjectLocalConfig_ReturnsProjectPath` | Project config | PASS |
| `GetConfigFilePaths_WithExplicitRulesPath_IncludesExplicitPath` | Explicit path | PASS |
| `GetConfigFilePaths_WithMultipleConfigs_ReturnsInCorrectOrder` | Order priority | PASS |
| `GetConfigFilePaths_WithNonExistentExplicitPath_DoesNotIncludeIt` | Non-existent | PASS |
| `GetConfigFilePaths_WithDuplicatePaths_NoDuplicatesInResult` | Deduplication | PASS |

---

### 7. Enterprise Markdown Sanitizer Tests (`EnterpriseMarkdownSanitizerTests.cs`)

**Purpose:** Markdown-specific sanitization scenarios.

#### Markdown Table Tests
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Sanitize_MarkdownTable_SanitizesServerNamesInCells` | Table cell sanitization | PASS |
| `Sanitize_MarkdownTable_SanitizesIPsInCells` | IP in table cells | PASS |
| `Sanitize_MarkdownTable_PreservesTableStructure` | Pipe count preserved | PASS |
| `Sanitize_MarkdownTable_ConsistentAliasesAcrossRows` | Same alias per value | PASS |

#### Markdown Code Block Tests
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Sanitize_MarkdownCodeBlock_SanitizesSqlContent` | SQL in code block | PASS |
| `Sanitize_MarkdownCodeBlock_SanitizesBashContent` | Bash in code block | PASS |
| `Sanitize_MarkdownCodeBlock_SanitizesJsonContent` | JSON in code block | PASS |
| `Sanitize_MultipleCodeBlocks_SanitizesAll` | Multiple blocks | PASS |

#### Markdown Inline Code Tests
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Sanitize_InlineCode_SanitizesServerName` | Backtick server name | PASS |
| `Sanitize_InlineCode_SanitizesDatabase` | Backtick database | PASS |
| `Sanitize_MultipleInlineCodes_SanitizesAll` | Multiple inline codes | PASS |

#### Markdown Link Tests
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Sanitize_MarkdownLink_SanitizesInternalUrl` | URL sanitization | PASS |
| `Sanitize_MarkdownLink_PreservesLinkText` | Link text preserved | PASS |

#### Complex Documents
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Sanitize_ComplexMarkdownDocument_SanitizesAllSections` | Full document | PASS |
| `Sanitize_MarkdownWithDiagram_SanitizesAsciiArt` | ASCII diagrams | PASS |
| `Sanitize_EmptyMarkdown_ReturnsEmpty` | Empty markdown | PASS |
| `Sanitize_MarkdownWithNoSensitiveData_ReturnsUnchanged` | Safe markdown | PASS |
| `Sanitize_MarkdownWithMixedCase_SanitizesCorrectly` | Case sensitivity | PASS |

---

### 8. SQL Linked Server Tests (`SqlLinkedServerTests.cs`)

**Purpose:** SQL four-part naming and linked server patterns.

#### Four-Part Bracketed Names
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Sanitize_FourPartBracketed_ReplacesEntirePattern` | [Srv].[DB].[dbo].[Tbl] | PASS |
| `Sanitize_MultipleFourPartBracketed_ReplacesAll` | Multiple 4-part names | PASS |

#### Four-Part Unbracketed Names
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Sanitize_FourPartUnbracketed_ReplacesEntirePattern` | Srv.DB.dbo.Tbl | PASS |

#### Three-Part Names
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Sanitize_ThreePartDbo_ReplacesPattern` | DB.dbo.Tbl | PASS |
| `Sanitize_MultipleThreePart_ReplacesAll` | Multiple 3-part names | PASS |

#### Two-Part Names
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Sanitize_TwoPart_ReplacesPattern` | DB.Tbl | PASS |

#### Prefixed Table Names
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Sanitize_PrefixedTable_ReplacesPattern` | TB00123 | PASS |
| `Sanitize_MultiplePrefixedTables_ReplacesAll` | Multiple TB/TA | PASS |

#### Complex SQL Queries
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Sanitize_ComplexJoinQuery_SanitizesAllReferences` | Multi-join query | PASS |
| `Sanitize_InsertSelectQuery_SanitizesSourceAndDest` | INSERT SELECT | PASS |
| `Sanitize_StoredProcedureCall_SanitizesParameters` | EXEC with params | PASS |

#### Consistency & Edge Cases
| Test Name | Description | Status |
|-----------|-------------|--------|
| `Sanitize_SameTableMultipleTimes_UsesSameAlias` | Consistent alias | PASS |
| `Sanitize_DifferentTables_UseDifferentAliases` | Unique aliases | PASS |
| `Sanitize_EmptyQuery_ReturnsEmpty` | Empty SQL | PASS |
| `Sanitize_QueryWithNoSensitiveData_ReturnsUnchanged` | Safe SQL | PASS |
| `Sanitize_QueryWithComments_PreservesComments` | SQL comments | PASS |

---

### 9. Built-In Rules Tests (`BuiltInRulesTests.cs`)

**Purpose:** Validation of built-in rule patterns.

| Test Name | Description | Status |
|-----------|-------------|--------|
| `ServerNamesRule_MatchesCorrectly` (5 cases) | ProductionDB, StagingDB, etc. | PASS |
| `PrivateIp192Rule_MatchesCorrectly` (4 cases) | 192.168.x.x patterns | PASS |
| `AllRules_HaveUniqueIds` | No duplicate rule IDs | PASS |
| `AllRules_HaveValidPatterns` | All patterns compile | PASS |
| `AllRules_AreEnabledByDefault` | All rules enabled | PASS |

---

### 10. Rule Registry Tests (`RuleRegistryTests.cs`)

**Purpose:** Rule management and lifecycle.

| Test Name | Description | Status |
|-----------|-------------|--------|
| `GetActiveRules_WithNoRules_ReturnsEmpty` | Empty registry | PASS |
| `AddRule_ThenGetActiveRules_ReturnsRule` | Add and retrieve | PASS |
| `GetActiveRules_ReturnsRulesOrderedByOrder` | Order respected | PASS |
| `GetActiveRules_ExcludesDisabledRules` | Disabled filtered | PASS |
| `GetRule_WithExistingId_ReturnsRule` | Get by ID | PASS |
| `GetRule_WithNonExistentId_ReturnsNull` | Missing ID = null | PASS |
| `EnableRule_ReEnablesDisabledRule` | Re-enable works | PASS |

---

### 11. Enterprise Rules Tests (`EnterpriseRulesTests.cs`)

**Purpose:** Validation of enterprise-specific regex patterns.

#### Server FQDN Pattern (9 cases)
| Test Name | Description | Status |
|-----------|-------------|--------|
| `ServerFqdnPattern_MatchesCorrectly` | PRODSRV01.acme-corp.com, etc. | PASS |

#### Enterprise Server Name Pattern (12 cases)
| Test Name | Description | Status |
|-----------|-------------|--------|
| `ServerNamePattern_MatchesCorrectly` | PRODSRV01, STGSRV01, etc. | PASS |

#### Database Name Pattern (8 cases)
| Test Name | Description | Status |
|-----------|-------------|--------|
| `DatabaseNamePattern_MatchesCorrectly` | DBZMEW, DBZBHI, etc. | PASS |

#### SQL Four-Part Bracketed Pattern (6 cases)
| Test Name | Description | Status |
|-----------|-------------|--------|
| `FourPartBracketedPattern_MatchesCorrectly` | [Srv].[DB].[dbo].[Tbl] | PASS |

#### SQL Four-Part Unbracketed Pattern (6 cases)
| Test Name | Description | Status |
|-----------|-------------|--------|
| `FourPartUnbracketedPattern_MatchesCorrectly` | Srv.DB.dbo.Tbl | PASS |

#### SQL Three-Part Pattern (6 cases)
| Test Name | Description | Status |
|-----------|-------------|--------|
| `ThreePartDboPattern_MatchesCorrectly` | DB.dbo.Tbl | PASS |

#### SQL Prefixed Table Pattern (8 cases)
| Test Name | Description | Status |
|-----------|-------------|--------|
| `PrefixedTablePattern_MatchesCorrectly` | TB00123, TA09052 | PASS |

#### AD Service Account Pattern (8 cases)
| Test Name | Description | Status |
|-----------|-------------|--------|
| `AdServiceAccountPattern_MatchesCorrectly` | ACME\svc_sql_agent | PASS |

#### Vault Path Pattern (6 cases)
| Test Name | Description | Status |
|-----------|-------------|--------|
| `VaultPathPattern_MatchesCorrectly` | secret/data/production | PASS |

#### Private IP Patterns (12 cases)
| Test Name | Description | Status |
|-----------|-------------|--------|
| `PrivateIp10Pattern_MatchesCorrectly` | 10.x.x.x | PASS |
| `PrivateIp192Pattern_MatchesCorrectly` | 192.168.x.x | PASS |

#### UNC Path Pattern (5 cases)
| Test Name | Description | Status |
|-----------|-------------|--------|
| `UncPathPattern_MatchesCorrectly` | \\\\SERVER\\Share | PASS |

---

### 12. Mapping Table Tests (`MappingTableTests.cs`)

**Purpose:** Alias mapping table operations.

| Test Name | Description | Status |
|-----------|-------------|--------|
| `GetOrCreateAlias_NewValue_CreatesAlias` | New mapping creation | PASS |
| `GetOrCreateAlias_ExistingValue_ReturnsSameAlias` | Existing mapping reuse | PASS |
| `GetOrCreateAlias_MultipleValues_IncrementsCounter` | Counter increment | PASS |
| `GetOriginal_ExistingAlias_ReturnsOriginal` | Reverse lookup | PASS |
| `GetOriginal_NonExistentAlias_ReturnsNull` | Missing alias = null | PASS |
| `HasMapping_ExistingValue_ReturnsTrue` | Existence check (true) | PASS |
| `HasMapping_NonExistentValue_ReturnsFalse` | Existence check (false) | PASS |
| `GetOrCreateAlias_DifferentPrefixes_UseSeparateCounters` | Independent counters | PASS |

---

### 13. Integration Tests (`EnterpriseFixtureTests.cs`)

**Purpose:** Full pipeline tests with realistic fixtures.

#### Enterprise Docs Fixture
| Test Name | Description | Status |
|-----------|-------------|--------|
| `EnterpriseDocsFixture_Exists` | Fixture directory exists | PASS |
| `EnterpriseDocsFixture_ReadmeIsSanitizable` | README.md sanitization | PASS |
| `EnterpriseDocsFixture_DatabaseDesignIsSanitizable` | database-design.md | PASS |
| `EnterpriseDocsFixture_SqlMigrationIsSanitizable` | V001__initial_schema.sql | PASS |
| `EnterpriseDocsFixture_TerraformIsSanitizable` | main.tf | PASS |

#### Deep Nested Fixture
| Test Name | Description | Status |
|-----------|-------------|--------|
| `DeepNestedFixture_Exists` | Fixture directory exists | PASS |
| `DeepNestedFixture_Level10ConfigIsSanitizable` | 10 levels deep config | PASS |
| `DeepNestedFixture_FileProcessorFindsAllFiles` | Recursive file discovery | PASS |

#### CI/CD Configs Fixture
| Test Name | Description | Status |
|-----------|-------------|--------|
| `CiCdFixture_GitLabCiIsSanitizable` | .gitlab-ci.yml | PASS |
| `CiCdFixture_GitHubActionsIsSanitizable` | deploy-production.yml | PASS |

#### Round-Trip Integration
| Test Name | Description | Status |
|-----------|-------------|--------|
| `RoundTrip_EnterpriseReadme_RestoresPerfectly` | Full README round-trip | PASS |
| `RoundTrip_DeepNestedConfig_RestoresPerfectly` | Nested config round-trip | PASS |
| `RoundTrip_GitLabCi_RestoresPerfectly` | GitLab CI round-trip | PASS |

---

## Running Tests

```bash
# Run all tests
dotnet test

# Run with verbose output
dotnet test --logger "console;verbosity=detailed"

# Run specific test file
dotnet test --filter "FullyQualifiedName~SanitizerTests"

# Run specific test method
dotnet test --filter "FullyQualifiedName~Sanitize_WithServerName_ReplacesWithAlias"

# Run with coverage
dotnet test --collect:"XPlat Code Coverage"
```

---

## Test Categories Explained

| Category | Purpose |
|----------|---------|
| **Algorithm Correctness** | Mathematical properties (determinism, reversibility) |
| **Sanitizer/Restorer** | Core transformation functions |
| **Round-Trip** | End-to-end identity verification |
| **Custom Rules** | Configuration loading and merging |
| **Global Config** | Cross-platform config discovery |
| **Enterprise Markdown** | Markdown-specific patterns |
| **SQL Linked Server** | SQL four-part naming patterns |
| **Built-In Rules** | Default rule pattern validation |
| **Rule Registry** | Rule lifecycle management |
| **Enterprise Rules** | Corporate naming pattern validation |
| **Mapping Table** | Bidirectional alias storage |
| **Integration** | Full pipeline with real fixtures |

---

*Generated: January 2026*
