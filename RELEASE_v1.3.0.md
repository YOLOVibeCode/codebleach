# CodeBleach v1.3.0 Release Notes

**Release Date:** 2026-01-16  
**Type:** Minor Feature Release  
**Breaking Changes:** None (fully backward compatible)

---

## 🎉 What's New

### Multi-Level Global Configuration System

CodeBleach now supports a hierarchical configuration system that lets you define sanitization rules once and apply them automatically to all your projects.

#### Key Features

1. **Global User Configuration**
   - Create `~/.config/codebleach/rules.json` (Linux/macOS) or `%APPDATA%\codebleach\rules.json` (Windows)
   - Rules apply automatically to ALL projects
   - Perfect for SQL databases, company-specific patterns, etc.

2. **Configuration Hierarchy**
   ```
   Built-in → Global Config → --rules CLI → Project-Local
   (lowest priority)                    (highest priority)
   ```
   Later sources override earlier sources for rules with the same `ruleId`.

3. **New Commands**
   - `codebleach init` - Create configuration files
   - `codebleach init --global --sql` - Bootstrap global SQL rules
   - `codebleach config --list` - View active configuration hierarchy
   - `codebleach config --path` - Show global config location

4. **Enhanced `sanitize` Command**
   - New `--rules <path>` option for explicit rule file override
   - Automatic discovery and merging of multiple config sources
   - Verbose output shows which config files were loaded

---

## 🚀 Quick Start with Global Config

### For SQL/Database Projects

```bash
# One-time setup: Create global SQL rules
codebleach init --global --sql

# Now ALL sanitize operations use these rules
codebleach sanitize ~/any-project
```

### For Custom Company Patterns

```bash
# Create global config
codebleach init --global

# Edit ~/.config/codebleach/rules.json with your patterns
# Now all projects automatically use your rules
```

---

## 📋 Complete Changelog

### Added
- ✅ `IGlobalConfigLocator` interface for cross-platform config discovery
- ✅ `GlobalConfigLocator` service with environment variable support (`CODEBLEACH_CONFIG_DIR`)
- ✅ `CustomRuleLoader.LoadFromMultipleFiles()` for rule merging
- ✅ `InitCommand` for creating config files with SQL templates
- ✅ `ConfigCommand` for viewing configuration hierarchy
- ✅ `--rules` option on `sanitize` command
- ✅ Automatic global config discovery and merging
- ✅ Comprehensive documentation in README.md

### Changed
- ⚙️ `SanitizeCommand` now uses multi-level configuration system
- ⚙️ Custom rules can now override/disable inherited rules
- ⚙️ Verbose output shows all loaded configuration sources

### Fixed
- 🐛 Rule override logic now properly handles `enabled: false`
- 🐛 Duplicate config paths are automatically de-duplicated

---

## 🧪 Testing

All features have been tested with:
- ✅ Unit tests for `GlobalConfigLocator` (cross-platform paths)
- ✅ Unit tests for `CustomRuleLoader` multi-file loading
- ✅ Integration tests for new CLI commands
- ✅ End-to-end 4-roundtrip testing with SQL rules
- ✅ Cross-platform compatibility (Windows, macOS, Linux)

Test coverage maintained at >85%.

---

## 📚 Documentation

Updated documentation:
- ✅ README.md - Global Configuration section
- ✅ README.md - New commands (`init`, `config`)
- ✅ README.md - Updated Quick Start
- ✅ IMPLEMENTATION_PLAN_GLOBAL_CONFIG.md - Technical details

---

## 🔄 Migration Guide

### From v1.2.0 to v1.3.0

**No migration needed!** v1.3.0 is fully backward compatible.

**Existing projects** continue to work exactly as before:
- Project-local `.codebleach-rules.json` files are still discovered
- Built-in rules still work the same way
- All existing CLI commands work identically

**To adopt global config** (optional):
```bash
# Create global config
codebleach init --global --sql

# Your existing projects now also use global rules
# Project-local rules still override as needed
```

---

## 🎯 Use Cases

### Use Case 1: SQL Database Team
**Before v1.3.0:**
- Copy `.codebleach-rules.json` to every project
- Update manually when rules change
- Inconsistent rules across projects

**With v1.3.0:**
```bash
# One-time team setup
codebleach init --global --sql

# Everyone's projects now use consistent SQL rules
# No per-project configuration needed
```

### Use Case 2: Company-Wide Standards
**Before v1.3.0:**
- Maintain rules file in Git repo
- Developers copy/symlink to projects
- Rules diverge over time

**With v1.3.0:**
```bash
# DevOps creates global config
codebleach init --global
# Edit ~/.config/codebleach/rules.json with company patterns

# All developers automatically use company rules
# Can still add project-specific overrides
```

---

## 🔧 Technical Details

### New Interfaces (ISP Compliance)

```csharp
public interface IGlobalConfigLocator
{
    string GetGlobalConfigDirectory();
    string GetGlobalRulesFilePath();
    bool GlobalRulesFileExists();
    IEnumerable<string> GetConfigFilePaths(string projectPath, string? explicitRulesPath = null);
}
```

### Configuration Resolution Algorithm

1. Load built-in rules from `BuiltInRules.cs`
2. Discover global config (`~/.config/codebleach/rules.json`)
3. Check for `--rules` CLI option
4. Walk up directory tree for `.codebleach-rules.json`
5. Load and merge all discovered configs
6. Rules with same `ruleId` override (later wins)
7. Filter to only `enabled: true` rules

### Cross-Platform Paths

| Platform | Location |
|----------|----------|
| **Linux/macOS** | `~/.config/codebleach/rules.json` |
| **Windows** | `%APPDATA%\codebleach\rules.json` |
| **Custom** | `$CODEBLEACH_CONFIG_DIR/rules.json` |

---

## 🙏 Acknowledgments

Implementation follows:
- ✅ **TDD (Test-Driven Development)** - All tests written before implementation
- ✅ **ISP (Interface Segregation Principle)** - Small, focused interfaces
- ✅ **KISS, YAGNI, DRY** - Simple, practical design
- ✅ Architect's spec from `architecture-checklist.md`
- ✅ Implementation plan from `IMPLEMENTATION_PLAN_GLOBAL_CONFIG.md`

---

## 📦 Release Artifacts

- **NuGet Package:** CodeBleach 1.3.0
- **GitHub Tag:** v1.3.0
- **Documentation:** Updated README.md

---

## 🐛 Known Issues

None at this time.

---

## 🔮 Future Roadmap

Potential features for v1.4.0+:
- YAML configuration format support
- Rule templates library
- `codebleach rules --export` command
- Cloud-based rule sharing

---

**Full Changelog:** https://github.com/YOLOVibeCode/codebleach/compare/v1.2.0...v1.3.0

