# CodeBleach

**Dead simple sanitization utility for sharing code with AI assistants**

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


CodeBleach is a .NET global tool that sanitizes sensitive data from your codebase before sharing it with AI assistants like ChatGPT, Claude, or GitHub Copilot. It creates a sanitized copy of your project, replacing sensitive values (database names, IPs, API keys, etc.) with safe aliases, and can restore original values when AI-modified code is returned.

## 🎯 Use Cases

### Use Case 1: Share Code with AI Assistants Safely

**Problem:** You want to ask ChatGPT to refactor your code, but it contains production database names, internal IPs, and API endpoints.

**Solution:** Sanitize first, share safely, restore after.

```bash
# Sanitize your project
codebleach sanitize ~/projects/my-app

# Share the sanitized copy with AI
# ... get AI suggestions ...

# Restore original values
cd ~/projects/my-app-sanitize
codebleach restore
```

### Use Case 2: Code Reviews and Documentation

**Problem:** You need to create documentation or examples but don't want to expose internal infrastructure details.

**Solution:** Create sanitized examples that are safe to share publicly.

```bash
codebleach sanitize ./examples --output ./examples-public
# Now examples-public/ is safe to commit to public repos
```

### Use Case 3: Onboarding and Training

**Problem:** New team members need example code, but you can't share production configurations.

**Solution:** Provide sanitized training materials.

```bash
codebleach sanitize ./training-materials
# Share sanitized version with new hires
```

## ✨ Key Features

- **🔍 Pattern-Based Detection** - 11 built-in rules for common sensitive data
- **🎨 Custom Rules** - Add project-specific patterns via JSON (no recompilation)
- **🔄 Perfect Round-Trip** - Sanitize → AI Edit → Restore with zero data loss
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
# 1. Sanitize a directory (creates <directory>-sanitize)
codebleach sanitize ~/projects/my-app

# 2. Share sanitized copy with AI
# ... work with AI on ~/projects/my-app-sanitize ...

# 3. Restore original values
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

### Example 1: Sanitize a .NET Project

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

### Example 2: Custom Rules

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

### Example 3: Restore After AI Edits

```bash
# 1. Sanitize
codebleach sanitize ~/projects/my-app

# 2. Share with AI, get suggestions
# AI modifies ~/projects/my-app-sanitize/src/Program.cs

# 3. Restore original values
cd ~/projects/my-app-sanitize
codebleach restore

# 4. Copy restored files back to original project
cp -r src/* ~/projects/my-app/src/
```

### Example 4: Write-Back to Original Location

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
- `--dry-run, -n` - Preview changes without modifying files
- `--verbose, -v` - Show detailed output with diffs
- `--force, -f` - Overwrite existing output directory

**Examples:**
```bash
# Basic sanitization
codebleach sanitize ./my-project

# Custom output location
codebleach sanitize ./my-project --output ./clean-version

# Preview changes
codebleach sanitize ./my-project --dry-run --verbose
```

### `restore` - Restore Sanitized Directory

```bash
codebleach restore [options]
```

**Options:**
- `--writeback, -w` - Write restored files back to original location
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

## 🔍 What Gets Sanitized

### Built-in Rules (11 patterns)

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

After sanitization, your project structure looks like:

```
my-app-sanitize/
├── .codebleach/
│   ├── manifest.json      # Machine-readable mappings
│   └── xref.md            # Human-readable cross-reference
├── src/
│   ├── Program.cs         # Sanitized (ProductionDB → SERVER_0)
│   └── appsettings.json   # Sanitized (192.168.1.100 → IP_0)
└── ...
```

## 🔄 Workflow Example

```bash
# 1. Start with your project
~/projects/my-app/
  ├── src/
  │   ├── Program.cs       # Contains: ProductionDB, 192.168.1.100
  │   └── appsettings.json # Contains: Server=ProductionDB

# 2. Sanitize
codebleach sanitize ~/projects/my-app

# 3. Result: Sanitized copy created
~/projects/my-app-sanitize/
  ├── .codebleach/
  │   ├── manifest.json    # Tracks: ProductionDB ↔ SERVER_0
  │   └── xref.md          # Shows all substitutions
  ├── src/
  │   ├── Program.cs       # Now: SERVER_0, IP_0
  │   └── appsettings.json # Now: Server=SERVER_0

# 4. Share with AI, get suggestions
# AI modifies ~/projects/my-app-sanitize/src/Program.cs

# 5. Restore original values
cd ~/projects/my-app-sanitize
codebleach restore

# 6. Result: Original values restored
src/Program.cs       # Now: ProductionDB, 192.168.1.100 (restored)
src/appsettings.json # Now: Server=ProductionDB (restored)
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

## 📋 File Types Supported

CodeBleach processes these file types:

- **Code**: `.cs`, `.js`, `.ts`, `.py`, `.go`, `.java`, `.rb`, `.php`, etc.
- **Config**: `.json`, `.yaml`, `.xml`, `.config`, `.ini`, `.env`
- **Scripts**: `.sh`, `.ps1`, `.bat`, `.cmd`
- **Web**: `.html`, `.css`, `.scss`
- **Data**: `.sql`, `.graphql`
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

