# CodeBleach

**Industrial-grade multi-language code obfuscator for sharing code with AI assistants**

<!-- AUTO-INDEX-SECTION -->
<!--
🤖 AI ASSISTANT - PROJECT NAVIGATION GUIDE
============================================

To fully understand and work with this project, follow this systematic approach:

📚 STEP 1 - READ DOCUMENTATION: documents/INDEX.md
   This is your PRIMARY source for understanding the project:
   • Complete index of ALL project documentation
   • Organized by category (specifications, guides, architecture, etc.)
   • Read specifications FIRST to understand system design
   • Then read guides for implementation details
   • Every document is indexed with descriptions and word counts
   • This is the authoritative source - do not search randomly

💡 KEY PRINCIPLES:
   • Both indexes are auto-generated and always up-to-date
   • Never search randomly - use the indexes as navigation
   • Read documentation before writing code
   • Check scripts before running manual commands
   • Word counts help prioritize what to read first

✅ AFTER READING BOTH INDEXES:
   You will have complete knowledge of:
   • System architecture and design decisions
   • Implementation details and best practices
   • All available automation and tooling
   • Proper setup, build, test, and deployment procedures

============================================
-->

## 📚 Documentation & Scripts

Browse the complete **[Documentation Index](documents/INDEX.md)** for all project documentation.

<!-- AUTO-INDEX-SECTION -->


CodeBleach is a .NET global tool that obfuscates your code before sharing it with AI assistants like ChatGPT, Claude, or GitHub Copilot. It supports two levels of protection:

- **Level 1 (Sanitize)** - Regex-based pattern matching replaces sensitive values (database names, IPs, API keys) with safe aliases
- **Level 2 (Full Obfuscation)** - Language-aware AST/token parsing renames **all** identifiers, removes comments, obfuscates strings, and renames files so a compromised LLM context reveals no organizational fingerprint

Both levels create a sanitized copy of your project and can restore original values when AI-modified code is returned.

## 🎯 Use Cases

### Use Case 1: Share Code with AI Assistants Safely

**Problem:** You want to ask ChatGPT to refactor your code, but it contains production database names, internal IPs, and API endpoints.

**Solution:** Sanitize first, share safely, restore after.

```bash
# Level 1: Replace sensitive patterns (default)
codebleach sanitize ~/projects/my-app

# Level 2: Full obfuscation - rename ALL identifiers
codebleach sanitize ~/projects/my-app --level 2

# Share the sanitized copy with AI
# ... get AI suggestions ...

# Restore original values
cd ~/projects/my-app-sanitize
codebleach restore
```

### Use Case 2: Zero-Fingerprint LLM Context

**Problem:** Your LLM context could be compromised, and you need to ensure no organizational identifiers are exposed.

**Solution:** Level 2 obfuscation renames everything - classes, methods, variables, file names, comments - leaving only structural code.

```bash
codebleach sanitize ./my-project --level 2

# Before: public class InvoiceService { decimal CalculateTotal() ... }
# After:  public class CLS_0 { decimal MTD_0() ... }

# Before: src/Services/InvoiceService.cs
# After:  DIR_0/DIR_1/CLS_0.cs
```

### Use Case 3: Selective Obfuscation with `--scope`

**Problem:** You want to share a mixed-language codebase with an AI, but only care about protecting database schema names. You want COBOL and JCL code left readable so the AI can help with business logic.

**Solution:** Use `--scope` to selectively obfuscate only specific code domains.

```bash
# Only obfuscate database identifiers (SQL table/column names)
codebleach sanitize ~/projects/my-app --level 2 --scope database

# Only obfuscate mainframe code (COBOL, JCL)
codebleach sanitize ~/projects/my-app --level 2 --scope mainframe

# Obfuscate both database and mainframe code
codebleach sanitize ~/projects/my-app --level 2 --scope database,mainframe
```

When `--scope database` is used on a COBOL file with embedded SQL:
- COBOL identifiers (PROGRAM-ID, paragraphs, variables) remain **unchanged**
- SQL identifiers inside `EXEC SQL` blocks are **obfuscated** via cross-language delegation
- The file structure stays readable for AI analysis of business logic

### Use Case 4: Code Reviews and Documentation

**Problem:** You need to create documentation or examples but don't want to expose internal infrastructure details.

**Solution:** Create sanitized examples that are safe to share publicly.

```bash
codebleach sanitize ./examples --output ./examples-public
# Now examples-public/ is safe to commit to public repos
```

## ✨ Key Features

- **🧠 9 Language Processors** - AST-aware obfuscation for C#, VB.NET, T-SQL, JavaScript, COBOL, JCL, VBScript/VBA, Oracle SQL/PL-SQL, and F#
- **🔍 Pattern-Based Detection** - 11 built-in rules for common sensitive data (Level 1)
- **🎯 Full Identifier Obfuscation** - Renames all classes, methods, variables, tables, columns using semantic prefixes (Level 2)
- **📁 File Name Obfuscation** - Renames files and directories to prevent organizational fingerprinting (Level 2)
- **🔗 Cross-File Reference Patching** - Updates imports, project references, and solution files after renames
- **🌐 Cross-Language Delegation** - COBOL EXEC SQL and JCL instream SQL are delegated to the SQL processor
- **🎯 Selective Scope Filtering** - `--scope` flag to obfuscate only specific code domains (e.g., `database`, `mainframe`) while leaving other code intact
- **🎨 Custom Rules** - Add project-specific patterns via JSON (no recompilation)
- **🌍 Global Configuration** - Define rules once, apply to all projects automatically
- **🔄 Perfect Round-Trip** - Sanitize/Obfuscate -> AI Edit -> Restore with zero data loss
- **✅ Build Verification** - `--verify` flag runs your project's build after obfuscation to catch breakage
- **📋 Complete Audit Trail** - Manifest and cross-reference files track all changes
- **👀 Dry-Run Mode** - Preview changes before committing
- **⚡ Fast** - Processes 1000+ files in seconds
- **🛡️ Safe** - Never modifies original files (creates sanitized copy)

## 🚀 Quick Start

### Installation

```bash
# Install as .NET global tool
dotnet tool install -g CodeBleach

# Verify installation
codebleach --version
```

### Basic Usage

```bash
# Level 1: Sanitize sensitive patterns (default)
codebleach sanitize ~/projects/my-app

# Level 2: Full AST-aware obfuscation
codebleach sanitize ~/projects/my-app --level 2

# Level 2 with build verification
codebleach sanitize ~/projects/my-app --level 2 --verify

# Level 2 scoped to database identifiers only
codebleach sanitize ~/projects/my-app --level 2 --scope database

# Restore original values
cd ~/projects/my-app-sanitize
codebleach restore
```

### Preview Changes (Dry Run)

```bash
# See what would be sanitized without modifying files
codebleach sanitize ~/projects/my-app --dry-run

# With detailed output
codebleach sanitize ~/projects/my-app --dry-run --verbose
```

## 📦 Installation

### Prerequisites

- .NET 10 SDK or later
- Windows, macOS, or Linux

### Install from NuGet

```bash
dotnet tool install -g CodeBleach
```

### Update

```bash
dotnet tool update -g CodeBleach
```

### Uninstall

```bash
dotnet tool uninstall -g CodeBleach
```

## 📖 Examples

### Example 1: Level 1 - Sanitize a .NET Project

```bash
# Your project contains:
# - appsettings.json with ProductionDB connection string
# - Program.cs with internal IP addresses
# - Configuration files with API keys

# Sanitize it
codebleach sanitize ./MyProject

# Result: MyProject-sanitize/ created with:
# - ProductionDB → SERVER_0
# - 192.168.1.100 → IP_0
# - api-prod.internal.corp.com → HOST_0
```

**Before:**
```csharp
var connectionString = "Server=ProductionDB;Database=user_accounts";
var redisHost = "192.168.1.100";
var apiUrl = "https://api-prod.internal.corp.com/v1/users";
```

**After:**
```csharp
var connectionString = "Server=SERVER_0;Database=TABLE_0";
var redisHost = "IP_0";
var apiUrl = "https://HOST_0/v1/TABLE_1";
```

### Example 2: Level 2 - Full Obfuscation

```bash
codebleach sanitize ./MyProject --level 2
```

**Before:**
```csharp
namespace Acme.Billing
{
    // Calculate invoice totals with tax
    public class InvoiceService
    {
        private decimal _taxRate = 0.08m;

        public decimal CalculateTotal(decimal subtotal)
        {
            return subtotal + (subtotal * _taxRate);
        }
    }
}
```

**After:**
```csharp
namespace NS_0.NS_1
{
    // [Comment removed]
    public class CLS_0
    {
        private decimal FLD_0 = 0.08m;

        public decimal MTD_0(decimal PRM_0)
        {
            return PRM_0 + (PRM_0 * FLD_0);
        }
    }
}
```

File structure also renamed:
```
Before: src/Services/InvoiceService.cs
After:  DIR_0/DIR_1/CLS_0.cs
```

### Example 3: Custom Rules

Create `.codebleach-rules.json` in your project root:

```json
{
  "rules": [
    {
      "ruleId": "custom_api_keys",
      "name": "API Keys",
      "pattern": "api_key_[a-zA-Z0-9]{20,}",
      "prefix": "APIKEY",
      "severity": "High"
    },
    {
      "ruleId": "jwt_tokens",
      "name": "JWT Tokens",
      "pattern": "eyJ[A-Za-z0-9-_=]+\\.eyJ[A-Za-z0-9-_=]+\\.[A-Za-z0-9-_=]+",
      "prefix": "JWT",
      "severity": "Critical"
    }
  ]
}
```

CodeBleach automatically discovers and uses these rules:

```bash
codebleach sanitize ./my-project
# Custom rules loaded automatically!
```

### Example 4: Restore After AI Edits

```bash
# 1. Sanitize
codebleach sanitize ~/projects/my-app --level 2

# 2. Share with AI, get suggestions
# AI modifies ~/projects/my-app-sanitize/src/CLS_0.cs

# 3. Restore original values (including file names)
cd ~/projects/my-app-sanitize
codebleach restore

# 4. Copy restored files back to original project
cp -r src/* ~/projects/my-app/src/
```

### Example 5: Write-Back to Original Location

```bash
# Restore and write directly back to original source
cd ~/projects/my-app-sanitize
codebleach restore --writeback

# Files in ~/projects/my-app are updated with restored values
```

## 🎛️ Commands

### `sanitize` - Sanitize a Directory

```bash
codebleach sanitize <source-directory> [options]
```

**Options:**
- `--output, -o <path>` - Custom output directory (default: `<source>-sanitize`)
- `--level, -l <1|2>` - Obfuscation level: 1=sanitize patterns (default), 2=full identifier obfuscation
- `--scope <specifiers>` - Comma-separated scope filter to obfuscate only specific code domains (see [Scope Filtering](#-scope-filtering))
- `--verify` - Build the output after obfuscation to verify correctness
- `--dry-run, -n` - Preview changes without modifying files
- `--verbose, -v` - Show detailed output with diffs
- `--force, -f` - Overwrite existing output directory
- `--rules, -r <path>` - Path to custom rules file (overrides auto-discovery)

**Examples:**
```bash
# Basic sanitization (Level 1)
codebleach sanitize ./my-project

# Full obfuscation (Level 2)
codebleach sanitize ./my-project --level 2

# Full obfuscation with build verification
codebleach sanitize ./my-project --level 2 --verify

# Obfuscate only database identifiers
codebleach sanitize ./my-project --level 2 --scope database

# Obfuscate database + mainframe code
codebleach sanitize ./my-project --level 2 --scope database,mainframe

# Custom output location
codebleach sanitize ./my-project --output ./clean-version

# Preview changes
codebleach sanitize ./my-project --level 2 --dry-run --verbose
```

### `restore` - Restore Sanitized Directory

```bash
codebleach restore [options]
```

**Options:**
- `--writeback, -w` - Write restored files back to original location
- `--verify` - Build the output after restore to verify correctness
- `--yes, -y` - Skip confirmation prompts
- `--dry-run, -n` - Preview what would be restored
- `--verbose, -v` - Show detailed output

**Examples:**
```bash
# Restore in place
cd my-project-sanitize
codebleach restore

# Write back to original location
codebleach restore --writeback

# Restore with build verification
codebleach restore --verify
```

### `verify` - Verify Build

Detect the project's build system and run a build to verify the code compiles.

```bash
codebleach verify [directory] [--verbose]
```

Supported build systems: .NET (sln/csproj), TypeScript (tsconfig.json), JavaScript (package.json), Make.

**Examples:**
```bash
# Verify current directory builds
codebleach verify

# Verify specific directory
codebleach verify ./my-project-sanitize --verbose
```

### `status` - Show Sanitization Status

```bash
codebleach status [--directory <path>]
```

**Examples:**
```bash
# Check current directory
codebleach status

# Check specific directory
codebleach status --directory ./my-project-sanitize
```

### `init` - Initialize Configuration

Create a CodeBleach configuration file with example rules.

```bash
codebleach init [options]
```

**Options:**
- `--global, -g` - Create global user configuration (applies to all projects)
- `--sql` - Include SQL-focused rules (databases, schemas, tables)
- `--force, -f` - Overwrite existing configuration file

**Examples:**
```bash
# Create project-local config
codebleach init

# Create global config (applies to all projects)
codebleach init --global

# Create global config with SQL rules
codebleach init --global --sql
```

### `config` - Manage Configuration

View configuration file locations and hierarchy.

```bash
codebleach config [options]
```

**Options:**
- `--list, -l` - List all configuration file locations
- `--path, -p` - Show global configuration directory path

**Examples:**
```bash
# Show global config path
codebleach config --path

# List all config files in priority order
codebleach config --list
```

## 🧠 Supported Languages (Level 2)

Level 2 obfuscation uses language-specific parsers for accurate, structure-preserving identifier renaming:

| Language | Parser | Capabilities |
|----------|--------|-------------|
| **C#** | Roslyn | Classes, interfaces, methods, properties, fields, variables, parameters, enums, namespaces, string literals, comments |
| **VB.NET** | Roslyn | Classes, modules, subs, functions, properties, variables, parameters, comments |
| **T-SQL / DB2** | ScriptDom | Tables, columns, procedures, functions, CTEs, cursors, variables, temp tables, comments, string literals |
| **JavaScript** | Acornima | Classes, functions, variables, parameters, destructuring, imports/exports, comments, string literals |
| **COBOL** | Custom tokenizer | PROGRAM-ID, paragraph names, data items, WORKING-STORAGE, PROCEDURE DIVISION, EXEC SQL delegation |
| **JCL** | Custom parser | Job names, step names, DD names, program names, dataset names, symbolic parameters, instream SQL delegation |
| **VBScript/VBA** | Custom tokenizer | Subs, functions, classes, properties, variables, constants, Dim/ReDim, dialect-specific built-in preservation |
| **Oracle SQL/PL-SQL** | Custom tokenizer | Tables (CREATE), columns, packages, procedures, DECLARE block variables, materialized views, bind variables, optimizer hints |
| **F#** | FSharp.Compiler.Service | Modules, let bindings, functions, types, union cases, record fields, parameters, comments |

### Semantic Alias Prefixes

Level 2 uses ~58 semantic categories for readable obfuscated code:

| Prefix | Meaning | Example |
|--------|---------|---------|
| `CLS_` | Class | `CLS_0`, `CLS_1` |
| `MTD_` | Method | `MTD_0` |
| `PROP_` | Property | `PROP_0` |
| `FLD_` | Field | `FLD_0` |
| `VAR_` | Variable | `VAR_0` |
| `PRM_` | Parameter | `PRM_0` |
| `NS_` | Namespace | `NS_0` |
| `TBL_` | Table | `TBL_0` |
| `COL_` | Column | `COL_0` |
| `SP_` | Stored Procedure | `SP_0` |
| `FILE_` | File name | `FILE_0.js` |
| `DIR_` | Directory | `DIR_0/` |
| `PROJ_` | Project file | `PROJ_0.csproj` |
| ... | ~45 more | See `SemanticCategory.cs` |

## ⚙️ Global Configuration

CodeBleach supports a **multi-level configuration system** that lets you define rules once and apply them to all projects.

### Configuration Hierarchy (Priority Low -> High)

```
1. Built-in Rules       -> Always loaded (11 patterns)
2. Global User Config   -> ~/.config/codebleach/rules.json (Linux/macOS)
                          %APPDATA%\codebleach\rules.json (Windows)
3. CLI --rules Option   -> Explicit override: --rules ~/my-rules.json
4. Project-Local Config -> .codebleach-rules.json in project or parent dirs
```

**Later sources override earlier sources** for rules with the same `ruleId`.

### Quick Setup for SQL Projects

If you work with SQL databases and want to sanitize database names, schemas, and table references across ALL projects:

```bash
# One-time setup: Create global config with SQL rules
codebleach init --global --sql

# Now ALL sanitize operations will use these rules
codebleach sanitize ~/projects/my-app-1    # Uses SQL rules
codebleach sanitize ~/projects/my-app-2    # Uses SQL rules
codebleach sanitize ~/other/project        # Uses SQL rules
```

The global configuration is stored at:
- **Linux/macOS:** `~/.config/codebleach/rules.json`
- **Windows:** `%APPDATA%\codebleach\rules.json`
- **Custom:** Set `CODEBLEACH_CONFIG_DIR` environment variable

### Viewing Active Configuration

```bash
# See which config files are being loaded
codebleach config --list
```

### Per-Project Customization

You can still create project-specific rules that override or extend your global config:

```bash
# Create project-local config
cd ~/projects/special-app
codebleach init

# Edit .codebleach-rules.json to add project-specific rules
# These will merge with (and override) your global rules
```

### One-Off Rule Files

For temporary or experimental rules, use the `--rules` option:

```bash
# Use specific rules file for this run only
codebleach sanitize ./my-project --rules ~/experimental-rules.json
```

### Disabling Global Rules for a Project

To disable a specific global rule in a project, create `.codebleach-rules.json` with:

```json
{
  "rules": [
    {
      "ruleId": "sql_database_names",
      "name": "SQL Database Names",
      "type": "regex",
      "pattern": "dummy",
      "prefix": "DISABLED",
      "enabled": false
    }
  ]
}
```

The `enabled: false` flag will override and disable the global rule.

## 🔍 What Gets Sanitized

### Level 1: Built-in Rules (11 patterns)

| Pattern | Example | Alias |
|---------|---------|-------|
| **Server/Database Names** | `ProductionDB`, `StagingDB` | `SERVER_0`, `SERVER_1` |
| **Private IPs (10.x)** | `10.0.0.50` | `IP_0` |
| **Private IPs (172.x)** | `172.16.0.1` | `IP_1` |
| **Private IPs (192.168.x)** | `192.168.1.100` | `IP_2` |
| **Connection Strings** | `Server=ProductionDB;...` | `CONNSTR_0` |
| **Windows Paths** | `C:\Projects\SecretProject` | `PATH_0` |
| **UNC Paths** | `\\server\share\folder` | `PATH_1` |
| **Internal Hostnames** | `api-prod.internal.corp.com` | `HOST_0` |
| **Production Tables** | `users_prod`, `orders_prod` | `TABLE_0` |
| **User Tables** | `users`, `accounts`, `customers` | `TABLE_1` |
| **Production Databases** | `MyAppProd`, `app-production` | `SERVER_2` |

### Level 2: Full Identifier Obfuscation

In addition to Level 1 rules, Level 2 uses AST-aware parsers to:

- **Rename all user-defined identifiers** (classes, methods, variables, tables, columns, etc.)
- **Remove all comments** (replaced with `[Comment removed]`)
- **Obfuscate string literals** (replaced with `STR_N` aliases)
- **Rename files and directories** (using semantic prefixes like `CLS_0.cs`, `DIR_0/`)
- **Patch cross-file references** (imports, project references, solution files)
- **Preserve language keywords and framework types** (e.g., `public`, `Console`, `SELECT`)

### Custom Rules

Add your own patterns via `.codebleach-rules.json`:

```json
{
  "rules": [
    {
      "ruleId": "my_custom_pattern",
      "name": "Custom Pattern",
      "pattern": "your-regex-here",
      "prefix": "CUSTOM",
      "severity": "Medium"
    }
  ]
}
```

See [CUSTOM_RULES.md](CUSTOM_RULES.md) for detailed documentation.

## 📁 Project Structure

After Level 2 sanitization, your project structure looks like:

```
my-app-sanitize/
├── .codebleach/
│   ├── manifest.json      # Machine-readable mappings (identifier + file path)
│   └── xref.md            # Human-readable cross-reference
├── DIR_0/
│   ├── CLS_0.cs           # Sanitized (InvoiceService.cs → CLS_0.cs)
│   └── FILE_0.json        # Sanitized (appsettings.json → FILE_0.json)
└── PROJ_0.csproj           # Sanitized (MyApp.csproj → PROJ_0.csproj)
```

## 🔄 Workflow Example

```bash
# 1. Start with your project
~/projects/my-app/
  ├── src/
  │   ├── InvoiceService.cs   # Contains: class InvoiceService, CalculateTotal()
  │   └── appsettings.json    # Contains: Server=ProductionDB

# 2. Sanitize (Level 2)
codebleach sanitize ~/projects/my-app --level 2

# 3. Result: Fully obfuscated copy
~/projects/my-app-sanitize/
  ├── .codebleach/
  │   ├── manifest.json      # Tracks: InvoiceService ↔ CLS_0, CalculateTotal ↔ MTD_0
  │   └── xref.md
  ├── DIR_0/
  │   ├── CLS_0.cs           # class CLS_0 { MTD_0() ... }
  │   └── FILE_0.json        # Server=SERVER_0

# 4. Share with AI - zero organizational fingerprint
# AI modifies ~/projects/my-app-sanitize/DIR_0/CLS_0.cs

# 5. Restore original values + file names
cd ~/projects/my-app-sanitize
codebleach restore

# 6. Result: Everything restored
src/InvoiceService.cs   # class InvoiceService { CalculateTotal() ... }
src/appsettings.json    # Server=ProductionDB
```

## 🛠️ Advanced Usage

### Custom Output Directory

```bash
codebleach sanitize ./my-project --output ./clean-version
```

### Force Overwrite

```bash
codebleach sanitize ./my-project --force
```

### Verbose Output with Diffs

```bash
codebleach sanitize ./my-project --dry-run --verbose
```

### Restore with Write-Back

```bash
cd my-project-sanitize
codebleach restore --writeback
# Files written back to original location
```

### Build Verification

```bash
# Verify obfuscated code still compiles
codebleach sanitize ./my-project --level 2 --verify

# Verify after restore
codebleach restore --verify

# Standalone verification
codebleach verify ./my-project-sanitize
```

## 🎯 Scope Filtering

The `--scope` flag enables **selective obfuscation** — obfuscate only specific code domains while leaving the rest untouched. This is useful when you want to protect database schema names but keep business logic readable for AI analysis, or vice versa.

### How It Works

When `--scope` is active, each language processor runs in one of two modes:

- **In-scope**: Full obfuscation — identical to running without `--scope`
- **Delegation-only**: Parse the file's structure but leave its own identifiers unchanged. If the file contains embedded code from an in-scope language (e.g., SQL inside COBOL `EXEC SQL` blocks), that embedded code is still delegated and obfuscated.

Without `--scope`, everything is obfuscated (fully backward compatible).

### Groups and Processor IDs

`--scope` accepts comma-separated **group names** and/or **individual processor IDs**:

| Group | Processor IDs | Languages |
|-------|--------------|-----------|
| `database` | `tsql`, `db2sql`, `oraclesql` | T-SQL, DB2 SQL, Oracle SQL/PL-SQL |
| `mainframe` | `cobol`, `jcl`, `mainframe-utility` | COBOL, JCL, Mainframe utilities |
| `dotnet` | `csharp`, `vbnet`, `fsharp` | C#, VB.NET, F# |
| `web` | `javascript` | JavaScript |
| `scripting` | `vbscript` | VBScript/VBA |

### Examples

```bash
# Obfuscate only SQL identifiers (table names, column names, etc.)
codebleach sanitize ./my-project --level 2 --scope database

# Obfuscate only mainframe code
codebleach sanitize ./my-project --level 2 --scope mainframe

# Combine groups
codebleach sanitize ./my-project --level 2 --scope database,mainframe

# Use individual processor IDs
codebleach sanitize ./my-project --level 2 --scope tsql,cobol

# Mix groups and IDs
codebleach sanitize ./my-project --level 2 --scope database,cobol
```

### Cross-Language Delegation with Scope

The real power of `--scope` is how it interacts with cross-language delegation. When a file contains embedded code from a different language, the embedded code is still processed if its language is in scope:

**`--scope database` on a COBOL file with EXEC SQL:**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOOKUP.              <-- unchanged (COBOL out of scope)
       PROCEDURE DIVISION.
       LOOKUP-EMPLOYEE.                     <-- unchanged
           EXEC SQL
             SELECT EMPLOYEE_NAME           <-- SQL identifiers obfuscated
             FROM EMPLOYEE_MASTER           <-- SQL identifiers obfuscated
             WHERE EMP_ID = :WS-EMP-ID      <-- host var unchanged (COBOL)
           END-EXEC
```

**`--scope database` on a C# file with SQL strings:**
```csharp
namespace AccountingApp                     // unchanged (C# out of scope)
{
    public class EmployeeRepository         // unchanged
    {
        var sql = "SELECT ... FROM TBL_0";  // SQL in strings obfuscated
    }
}
```

**`--scope mainframe` on the same COBOL file:**
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM_0.                  <-- obfuscated (COBOL in scope)
       PROCEDURE DIVISION.
       PARA_0.                              <-- obfuscated
           EXEC SQL
             SELECT EMPLOYEE_NAME           <-- unchanged (SQL out of scope)
             FROM EMPLOYEE_MASTER           <-- unchanged
           END-EXEC
```

### File Renaming with Scope

When `--scope` is active, only files whose primary processor is in scope get renamed. Out-of-scope files keep their original names. This ensures you can still navigate the project structure for code that wasn't obfuscated.

### Level 1 with Scope

`--scope` also works with Level 1 (regex-based) sanitization. When combined with `--scope`, only files whose extension maps to an in-scope processor receive regex processing. Other files are copied unchanged.

```bash
# Level 1, only process SQL files
codebleach sanitize ./my-project --level 1 --scope database
```

## 📋 File Types Supported

**Language-aware processing (Level 2):**
- **C#**: `.cs`, `.csx`
- **VB.NET**: `.vb`
- **T-SQL / DB2**: `.sql`
- **JavaScript**: `.js`, `.mjs`, `.cjs`
- **COBOL**: `.cbl`, `.cob`, `.cpy`
- **JCL**: `.jcl`, `.prc`
- **VBScript/VBA**: `.vbs`, `.bas`, `.cls`, `.frm`
- **Oracle SQL/PL-SQL**: `.pls`, `.plb`, `.pks`, `.pkb`, `.fnc`, `.prc`, `.trg`
- **F#**: `.fs`, `.fsi`, `.fsx`

**Regex-based processing (Level 1, all file types):**
- **Config**: `.json`, `.yaml`, `.xml`, `.config`, `.ini`, `.env`
- **Scripts**: `.sh`, `.ps1`, `.bat`, `.cmd`
- **Web**: `.html`, `.css`, `.scss`
- **Docs**: `.md`, `.txt`, `.rst`

**Automatically skips:**
- Binary files (`.exe`, `.dll`, `.png`, etc.)
- Build artifacts (`bin/`, `obj/`, `node_modules/`, etc.)
- Version control (`.git/`, `.svn/`)
- Files larger than 10MB

## 🔒 Security & Privacy

- **Never modifies originals** - Always creates a sanitized copy
- **Complete audit trail** - Manifest tracks all substitutions
- **Fail-secure** - Errors halt processing, no partial sanitization
- **No data leakage** - Original values never logged or displayed
- **Version controlled** - `.codebleach/` directory can be committed

## 🐛 Troubleshooting

### "Source directory not found"
```bash
# Use absolute path or check current directory
codebleach sanitize /full/path/to/project
```

### "Output directory already exists"
```bash
# Use --force to overwrite
codebleach sanitize ./my-project --force
```

### "Not a sanitized directory"
```bash
# Make sure you're in a directory with .codebleach/ folder
cd my-project-sanitize
codebleach restore
```

### Custom rules not loading
```bash
# Check .codebleach-rules.json exists in project root
# Use --verbose to see which rules are loaded
codebleach sanitize ./my-project --verbose
```

## 📚 Documentation

- [CUSTOM_RULES.md](CUSTOM_RULES.md) - Custom rules configuration guide
- [SANITIZATION_DEMO.md](SANITIZATION_DEMO.md) - Step-by-step demo
- [SPECIFICATION.md](SPECIFICATION.md) - Technical specification
- [IMPLEMENTATION_PLAN.md](IMPLEMENTATION_PLAN.md) - Architecture details

## 🤝 Contributing

Contributions welcome! Please read our contributing guidelines and code of conduct.

## 📄 License

MIT License - see [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

Inspired by the need to safely share code with AI assistants while protecting sensitive infrastructure details.

---

**Made with ❤️ for developers who want to use AI safely**
