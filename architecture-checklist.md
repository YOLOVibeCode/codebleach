# CodeBleach Architecture Checklist: Global Rule Configuration

**Author:** Software Architect  
**Date:** 2026-01-15  
**Status:** PROPOSED  
**Issue:** Custom SQL rules not applied - no global configuration mechanism

---

## 1. Problem Statement

Custom sanitization rules (SQL database names, schema-qualified tables, etc.) are not being applied because:

1. Rules are stored in `.codebleach-rules.json` which is **project-local only**
2. The NuGet package **does NOT bundle** any custom rules
3. There is **NO global/user-level configuration** location
4. Users must manually create `.codebleach-rules.json` in every project

### Current Rule Loading Flow
```
┌─────────────────────────────────────────────────────────────┐
│  codebleach sanitize /path/to/project                       │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
        ┌─────────────────────────────────────┐
        │  1. Load BuiltInRules.All           │ ✓ Always works
        │     (11 generic patterns)           │
        └─────────────────────────────────────┘
                              │
                              ▼
        ┌─────────────────────────────────────┐
        │  2. FindConfigFile(source.FullName) │
        │     Walk UP directory tree looking  │
        │     for .codebleach-rules.json      │
        └─────────────────────────────────────┘
                              │
                     ┌────────┴────────┐
                     │                 │
                ┌────▼────┐       ┌────▼────┐
                │  Found  │       │Not Found│
                │  Load   │       │  Skip   │ ⚠️ SQL rules lost!
                └─────────┘       └─────────┘
```

---

## 2. Proposed Architecture: Multi-Level Configuration

### 2.1 Configuration Hierarchy (Priority Low → High)

```
┌─────────────────────────────────────────────────────────────┐
│                    RULE PRECEDENCE                          │
├─────────────────────────────────────────────────────────────┤
│  4. Project rules (highest)   .codebleach-rules.json        │
│     └── In source dir or parent                             │
├─────────────────────────────────────────────────────────────┤
│  3. User global rules         ~/.config/codebleach/rules.json│
│     └── Or $CODEBLEACH_CONFIG_DIR/rules.json                │
├─────────────────────────────────────────────────────────────┤
│  2. CLI-specified rules       --rules /path/to/rules.json   │
│     └── Explicit override                                   │
├─────────────────────────────────────────────────────────────┤
│  1. Built-in rules (lowest)   Compiled into assembly        │
│     └── BuiltInRules.cs                                     │
└─────────────────────────────────────────────────────────────┘
```

### 2.2 Global Configuration Locations (Cross-Platform)

| Platform | Primary Location | Fallback |
|----------|------------------|----------|
| **Linux/macOS** | `~/.config/codebleach/rules.json` | `~/.codebleach-rules.json` |
| **Windows** | `%APPDATA%\codebleach\rules.json` | `%USERPROFILE%\.codebleach-rules.json` |
| **Environment** | `$CODEBLEACH_CONFIG_DIR/rules.json` | N/A |

---

## 3. Implementation Roadmap

### Phase 1: Add CLI Option for Rules File ⬜
**Priority:** HIGH | **Effort:** Small | **Risk:** Low

Add `--rules <path>` option to explicitly specify a custom rules file:

```bash
codebleach sanitize ./my-project --rules ~/my-company-rules.json
```

**Changes Required:**
- [ ] `SanitizeCommand.cs`: Add `--rules` option
- [ ] `RestoreCommand.cs`: Add `--rules` option (if applicable)
- [ ] Update help text and README

**Interface Change:**
```csharp
// In SanitizeCommand.Create()
var rulesOption = new Option<FileInfo?>(
    new[] { "--rules", "-r" },
    "Path to custom rules file (overrides auto-discovery)");
```

---

### Phase 2: Implement Global Config Discovery ⬜
**Priority:** HIGH | **Effort:** Medium | **Risk:** Low

Create `GlobalConfigLocator` service to find user-level configuration:

```csharp
public interface IGlobalConfigLocator
{
    /// <summary>
    /// Gets all configuration file paths in priority order (lowest to highest).
    /// </summary>
    IEnumerable<string> GetConfigPaths();
    
    /// <summary>
    /// Gets the primary global config path for the current platform.
    /// </summary>
    string GetGlobalConfigPath();
}
```

**Changes Required:**
- [ ] Create `IGlobalConfigLocator` interface
- [ ] Create `GlobalConfigLocator` implementation
- [ ] Update `CustomRuleLoader.FindConfigFile()` to use hierarchy
- [ ] Unit tests for cross-platform paths

---

### Phase 3: Rule Merging Strategy ⬜
**Priority:** MEDIUM | **Effort:** Medium | **Risk:** Medium

Define how rules from multiple sources are combined:

```
MERGE STRATEGY:
┌──────────────────────────────────────────────────────────┐
│  Built-in: server_names, private_ip_10, ...              │
│            ↓ (can be overridden by same ruleId)          │
│  Global:   sql_databases, company_servers, ...           │
│            ↓ (can be overridden by same ruleId)          │
│  Project:  project_specific_tables, ...                  │
│            ↓                                             │
│  FINAL:    Merged ruleset, later sources win on conflict │
└──────────────────────────────────────────────────────────┘
```

**Behavior:**
- Rules with same `ruleId` → Later source OVERRIDES earlier
- Rules with different `ruleId` → All included (merged)
- Use `enabled: false` to disable an inherited rule

---

### Phase 4: First-Run Setup Command ⬜
**Priority:** LOW | **Effort:** Small | **Risk:** Low

Add `codebleach init` command to scaffold global configuration:

```bash
# Create global config with SQL examples
codebleach init --global

# Create project-local config
codebleach init

# List where configs are loaded from
codebleach config --list
```

---

## 4. Alternative: Promote SQL Rules to Built-In

### Option A: Add SQL Rules to BuiltInRules.cs
**Pros:**
- Immediate fix, no architecture change
- Works out of the box for everyone

**Cons:**
- Opinionated - may match things users don't want
- Harder to customize (requires code change to disable)

### Option B: Keep as Configurable Examples (Current)
**Pros:**
- Users opt-in to SQL rules explicitly
- No false positives

**Cons:**
- Requires manual setup (current pain point)

### Recommendation: **Hybrid Approach**
1. Implement global config (Phases 1-2) for power users
2. Keep SQL rules as examples in documentation
3. Provide `codebleach init --sql` to bootstrap SQL-focused rules

---

## 5. Immediate Workaround (Before Implementation)

Until global config is implemented, users can:

### Option 1: Create Global Rules Manually
```bash
# Create global config directory
mkdir -p ~/.config/codebleach

# Copy your rules file there
cp /path/to/.codebleach-rules.json ~/.config/codebleach/rules.json
```

Then modify the code to look there (requires code change).

### Option 2: Place Rules File in Home Directory
Create `~/.codebleach-rules.json` with your SQL rules.
The tool will find it if you sanitize from any subdirectory of `~`.

### Option 3: Use Symlinks
```bash
# In each project that needs SQL rules
ln -s ~/my-company-rules.json .codebleach-rules.json
```

---

## 6. Acceptance Criteria

### Phase 1 Complete When:
- [ ] `--rules` option works: `codebleach sanitize . --rules ~/rules.json`
- [ ] Verbose output shows which rules file was loaded
- [ ] Unit tests pass

### Phase 2 Complete When:
- [ ] Global config auto-discovered from `~/.config/codebleach/rules.json`
- [ ] Environment variable `CODEBLEACH_CONFIG_DIR` respected
- [ ] Cross-platform paths work (Windows/macOS/Linux)
- [ ] Integration tests pass

### Phase 3 Complete When:
- [ ] Rules from multiple sources merge correctly
- [ ] Same `ruleId` in project overrides global
- [ ] `enabled: false` can disable inherited rules
- [ ] Documentation updated

---

## 7. Decision Required

**ARCHITECT RECOMMENDATION:** Implement **Phase 1** immediately as a quick win, then **Phase 2** for proper global support.

**Question for Product Owner:**
> Should SQL database/schema rules be promoted to **built-in** rules (always active), or remain **opt-in** via configuration?

---

## 8. Appendix: Example Global Rules File

```json
{
  "rules": [
    {
      "ruleId": "sql_database_names",
      "name": "SQL Database Names",
      "description": "Masks database names in FROM clauses",
      "type": "regex",
      "pattern": "(?i)(?<=FROM\\s+)[A-Za-z_][A-Za-z0-9_]*(?=\\.)",
      "prefix": "DATABASE",
      "severity": "High",
      "order": 5
    },
    {
      "ruleId": "sql_schema_qualified",
      "name": "Schema-Qualified Tables",
      "description": "Masks schema.table references",
      "type": "regex",
      "pattern": "(?i)\\b(dbo|staging|archive)\\.[A-Za-z_][A-Za-z0-9_]*\\b",
      "prefix": "TABLE",
      "severity": "High",
      "order": 6
    },
    {
      "ruleId": "linked_servers",
      "name": "Linked Server References",
      "description": "Masks [Server].[Database].[Schema].[Table] patterns",
      "type": "regex", 
      "pattern": "\\[[A-Za-z_][A-Za-z0-9_]*\\]\\.\\[[A-Za-z_][A-Za-z0-9_]*\\]",
      "prefix": "SERVER",
      "severity": "Critical",
      "order": 1
    }
  ]
}
```

---

**STATUS:** Awaiting ENGINEER role authorization to implement Phase 1.

ROLE: architect STRICT=true