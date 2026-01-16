# CodeBleach - Technical Specification Document

**Version:** 1.0  
**Document Type:** Technical Specification  
**Status:** Draft  
**Created:** January 15, 2026  
**Classification:** Internal Engineering

---

## Table of Contents

1. [Introduction](#1-introduction)
2. [Problem Statement](#2-problem-statement)
3. [Solution Overview](#3-solution-overview)
4. [Functional Requirements](#4-functional-requirements)
5. [Non-Functional Requirements](#5-non-functional-requirements)
6. [Architecture Design](#6-architecture-design)
7. [Data Models](#7-data-models)
8. [CLI Specification](#8-cli-specification)
9. [Sanitization Rules](#9-sanitization-rules)
10. [File Processing](#10-file-processing)
11. [Testing Strategy](#11-testing-strategy)
12. [Release & Deployment](#12-release--deployment)

---

## 1. Introduction

### 1.1 Purpose

CodeBleach is a **dead simple** .NET global tool that sanitizes code directories before sharing with AI assistants. It creates a sanitized copy of any folder, replacing sensitive values (server names, database names, IPs, etc.) with safe aliases, and can restore the original values when AI-modified code is returned.

### 1.2 Design Philosophy

| Principle | Description |
|-----------|-------------|
| **Dead Simple** | One command to sanitize, one command to restore |
| **Zero Config** | Works out of the box with sensible defaults |
| **Fast** | Copy and sanitize as quickly as possible |
| **Reversible** | Perfect round-trip: sanitize → AI edit → restore |
| **Self-Contained** | Manifest file tracks everything needed for restoration |

### 1.3 Definitions

| Term | Definition |
|------|------------|
| **Sanitization** | Replacing sensitive values with safe aliases (e.g., `ProductionDB` → `SERVER_0`) |
| **Restoration** | Reversing aliases back to original values |
| **Manifest** | Machine-readable JSON file (`.codebleach/manifest.json`) storing mappings and metadata |
| **Cross-Reference** | Human-readable Markdown file (`.codebleach/xref.md`) showing substitution table |
| **Alias** | Generated placeholder (e.g., `SERVER_0`, `IP_0`, `TABLE_0`) |

---

## 2. Problem Statement

### 2.1 The Risk

Developers frequently share code with AI assistants containing:
- Production database names: `ProductionDB`, `user_accounts`
- Internal server IPs: `192.168.1.100`, `10.0.0.50`
- Internal hostnames: `db-prod-01.internal.corp.com`
- Connection strings: `Server=prod-sql;User=admin;Password=...`
- File paths revealing infrastructure: `C:\Projects\SecretProject\`

### 2.2 Current Workflow (Painful)

```
1. Developer wants AI help with code in ~/projects/my-app/
2. Developer manually copies folder
3. Developer manually searches and replaces sensitive values
4. Developer shares with AI
5. AI returns modified code with aliases
6. Developer manually reverses all replacements
7. Developer copies changes back to original
```

**Problems:**
- Manual and error-prone
- Inconsistent replacements
- Easy to miss sensitive data
- Tedious to reverse

### 2.3 CodeBleach Workflow (Simple)

```
# Sanitize (one command)
$ codebleach ~/projects/my-app/

# Result: ~/projects/my-app-sanitize/ created with all sensitive data masked
# Share sanitized folder with AI

# After AI modifications, restore (one command)
$ cd ~/projects/my-app-sanitize/
$ codebleach restore

# Result: All aliases restored to original values
# Copy files back to original project (or use --writeback)
```

---

## 3. Solution Overview

### 3.1 Core Operations

```
┌─────────────────────────────────────────────────────────────────────┐
│                          SANITIZE FLOW                               │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ~/projects/my-app/          codebleach         ~/projects/my-app-sanitize/
│  ┌─────────────────┐         ───────────────▶   ┌─────────────────┐  │
│  │ src/            │                            │ src/            │  │
│  │   Program.cs    │    Copy + Sanitize         │   Program.cs    │  │
│  │   appsettings   │    + Create Metadata       │   appsettings   │  │
│  │ tests/          │                            │ tests/          │  │
│  └─────────────────┘                            │ .codebleach/    │  │
│                                                 │   manifest.json │  │
│                                                 │   xref.md       │  │
│                                                 └─────────────────┘  │
│                                                                      │
│  Content: "Server=ProductionDB"     ──▶    Content: "Server=SERVER_0"│
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────────┐
│                          RESTORE FLOW                                │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ~/projects/my-app-sanitize/                                         │
│  ┌─────────────────┐         codebleach restore                      │
│  │ src/            │         ───────────────▶   Files updated        │
│  │   Program.cs    │    Read .codebleach/       in place OR          │
│  │   appsettings   │    manifest.json           written back to      │
│  │ .codebleach/    │    + Restore Aliases       original location    │
│  │   manifest.json │                                                 │
│  │   xref.md       │                                                 │
│  └─────────────────┘                                                 │
│                                                                      │
│  Content: "Server=SERVER_0"         ──▶    Content: "Server=ProductionDB"
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
```

### 3.2 Key Features

| Feature | Description |
|---------|-------------|
| **Auto-Detection** | Knows if you're in a sanitized directory (checks for `.codebleach`) |
| **Consistent Aliases** | Same value always gets same alias (`ProductionDB` → `SERVER_0` everywhere) |
| **Metadata Tracking** | Manifest stores source path, timestamps, file list, mappings |
| **Fast Copy** | Uses efficient file copy, skips binary files and ignored paths |
| **Writeback Option** | Can restore directly to original location |

---

## 4. Functional Requirements

### FR-001: Sanitize Directory

| ID | FR-001 |
|----|--------|
| **Description** | CLI SHALL copy a source directory to `<source>-sanitize` and sanitize all supported files |
| **Priority** | P0 (Critical) |
| **Input** | Source directory path |
| **Output** | Sanitized directory with `.codebleach/` metadata |
| **Acceptance Criteria** | <ul><li>Creates `<source>-sanitize` directory</li><li>Copies all files (respecting ignore patterns)</li><li>Sanitizes supported file types</li><li>Creates `.codebleach/manifest.json` with mappings</li><li>Creates `.codebleach/xref.md` for human reference</li><li>Reports summary statistics</li></ul> |

### FR-002: Restore Sanitized Directory

| ID | FR-002 |
|----|--------|
| **Description** | CLI SHALL restore original values in a sanitized directory using the manifest |
| **Priority** | P0 (Critical) |
| **Input** | Current directory (must contain `.codebleach/`) OR explicit path |
| **Output** | Files with aliases replaced by original values |
| **Acceptance Criteria** | <ul><li>Reads `.codebleach/manifest.json` for mappings</li><li>Replaces all aliases with original values</li><li>Updates all files in place</li><li>Updates `.codebleach/xref.md` with restore timestamp</li><li>Reports restoration summary</li></ul> |

### FR-003: Writeback to Original

| ID | FR-003 |
|----|--------|
| **Description** | CLI SHALL optionally write restored content back to the original source location |
| **Priority** | P1 (High) |
| **Input** | `--writeback` flag during restore |
| **Output** | Modified files copied to original source path |
| **Acceptance Criteria** | <ul><li>Reads original path from manifest</li><li>Only writes files that were modified</li><li>Prompts for confirmation (unless `--yes`)</li><li>Reports files written</li></ul> |

### FR-004: Detection Patterns

| ID | FR-004 |
|----|--------|
| **Description** | CLI SHALL detect sensitive data using configurable regex patterns |
| **Priority** | P0 (Critical) |
| **Patterns** | See [Section 9: Sanitization Rules](#9-sanitization-rules) |
| **Acceptance Criteria** | <ul><li>Detects server/database names</li><li>Detects private IP addresses</li><li>Detects internal hostnames</li><li>Detects connection strings</li><li>Detects file paths</li></ul> |

### FR-005: Ignore Patterns

| ID | FR-005 |
|----|--------|
| **Description** | CLI SHALL skip files/directories matching ignore patterns |
| **Priority** | P1 (High) |
| **Default Ignores** | `.git`, `node_modules`, `bin`, `obj`, `*.exe`, `*.dll`, `*.png`, `*.jpg`, etc. |
| **Acceptance Criteria** | <ul><li>Respects `.gitignore` if present</li><li>Applies default binary/artifact ignores</li><li>Supports custom ignore via config</li></ul> |

### FR-006: Auto-Detection of Context

| ID | FR-006 |
|----|--------|
| **Description** | CLI SHALL automatically detect if current directory is a sanitized copy |
| **Priority** | P1 (High) |
| **Detection** | Presence of `.codebleach/` directory with valid `manifest.json` |
| **Acceptance Criteria** | <ul><li>If `.codebleach/manifest.json` exists, running `codebleach` shows restore options</li><li>If no argument and not in sanitized dir, shows help</li><li>Generates both `manifest.json` and `xref.md` on sanitization</li></ul> |

### FR-007: Dry-Run Mode

| ID | FR-007 |
|----|--------|
| **Description** | CLI SHALL support dry-run mode showing exactly what changes would be made without modifying files |
| **Priority** | P0 (Critical) |
| **Applies To** | `sanitize`, `restore`, `restore --writeback` |
| **Acceptance Criteria** | <ul><li>Shows files that would be created/modified</li><li>Shows each sensitive value detected with line numbers</li><li>Shows mappings that would be created</li><li>Shows summary statistics</li><li>Zero file system modifications</li><li>Verbose mode shows unified diff format</li></ul> |

### FR-008: Preview with Line Context

| ID | FR-008 |
|----|--------|
| **Description** | Dry-run output SHALL show line numbers and surrounding context for each detected value |
| **Priority** | P1 (High) |
| **Acceptance Criteria** | <ul><li>1-based line numbers</li><li>Show the actual line content</li><li>Highlight the matched value</li><li>Show the mapping (original → alias)</li><li>Indicate if mapping already exists</li></ul> |

---

## 5. Non-Functional Requirements

### 5.1 Performance

| ID | Requirement | Target |
|----|-------------|--------|
| NFR-001 | Copy + sanitize 1000 files | < 5 seconds |
| NFR-002 | Restore 1000 files | < 3 seconds |
| NFR-003 | Memory usage | < 100MB for typical projects |
| NFR-004 | Startup time | < 500ms |

### 5.2 Usability

| ID | Requirement | Target |
|----|-------------|--------|
| NFR-005 | Zero configuration required | Works out of box |
| NFR-006 | Single command operation | One command to sanitize, one to restore |
| NFR-007 | Clear output | Plain text, no emojis, actionable messages |
| NFR-008 | Exit codes | 0 = success, 1 = error, 2 = invalid args |

### 5.3 Compatibility

| ID | Requirement | Target |
|----|-------------|--------|
| NFR-009 | .NET version | .NET 10 |
| NFR-010 | OS support | Windows, macOS, Linux |
| NFR-011 | Installation | `dotnet tool install -g CodeBleach` |

---

## 6. Architecture Design

### 6.1 Project Structure

```
CodeBleach/
├── src/
│   ├── CodeBleach/                     # CLI Entry Point
│   │   ├── Program.cs                  # Main + CLI setup
│   │   ├── Commands/
│   │   │   ├── SanitizeCommand.cs      # codebleach <path>
│   │   │   ├── RestoreCommand.cs       # codebleach restore
│   │   │   └── StatusCommand.cs        # codebleach status
│   │   └── CodeBleach.csproj
│   │
│   └── CodeBleach.Core/                # Business Logic
│       ├── Interfaces/
│       │   ├── ISanitizer.cs
│       │   ├── IRestorer.cs
│       │   ├── IFileProcessor.cs
│       │   ├── IRuleRegistry.cs
│       │   └── IManifestManager.cs
│       ├── Services/
│       │   ├── Sanitizer.cs
│       │   ├── Restorer.cs
│       │   ├── FileProcessor.cs
│       │   └── ManifestManager.cs
│       ├── Rules/
│       │   ├── SanitizationRule.cs
│       │   ├── BuiltInRules.cs
│       │   └── RuleRegistry.cs
│       ├── Models/
│       │   ├── Manifest.cs
│       │   ├── MappingTable.cs
│       │   ├── SanitizationResult.cs
│       │   └── FileProcessingResult.cs
│       └── CodeBleach.Core.csproj
│
├── tests/
│   ├── CodeBleach.Tests/
│   └── CodeBleach.IntegrationTests/
│
├── .cursorrules
├── SPECIFICATION.md                    # This file
├── README.md
└── CodeBleach.sln
```

### 6.2 Component Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                           CodeBleach CLI                             │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌─────────────────┐     ┌─────────────────┐     ┌───────────────┐  │
│  │ SanitizeCommand │     │ RestoreCommand  │     │ StatusCommand │  │
│  └────────┬────────┘     └────────┬────────┘     └───────┬───────┘  │
│           │                       │                      │          │
│           └───────────────────────┼──────────────────────┘          │
│                                   │                                  │
│                                   ▼                                  │
│  ┌──────────────────────────────────────────────────────────────┐   │
│  │                       Core Services                           │   │
│  │                                                               │   │
│  │  ┌─────────────┐  ┌─────────────┐  ┌───────────────────────┐ │   │
│  │  │ ISanitizer  │  │  IRestorer  │  │  IManifestManager     │ │   │
│  │  │             │  │             │  │                       │ │   │
│  │  │ Sanitize()  │  │ Restore()   │  │ Load() / Save()       │ │   │
│  │  └──────┬──────┘  └──────┬──────┘  └───────────┬───────────┘ │   │
│  │         │                │                     │             │   │
│  │         └────────────────┼─────────────────────┘             │   │
│  │                          │                                   │   │
│  │                          ▼                                   │   │
│  │  ┌──────────────────────────────────────────────────────┐   │   │
│  │  │                   IFileProcessor                      │   │   │
│  │  │                                                       │   │   │
│  │  │  CopyDirectory()  ProcessFile()  ShouldProcess()      │   │   │
│  │  └──────────────────────────┬───────────────────────────┘   │   │
│  │                             │                               │   │
│  │                             ▼                               │   │
│  │  ┌──────────────────────────────────────────────────────┐   │   │
│  │  │                   IRuleRegistry                       │   │   │
│  │  │                                                       │   │   │
│  │  │  GetActiveRules()  AddRule()  BuiltInRules           │   │   │
│  │  └──────────────────────────────────────────────────────┘   │   │
│  └──────────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────┘
```

### 6.3 Sanitize Flow

```
┌────────┐  ┌──────────────┐  ┌──────────────┐  ┌─────────────┐  ┌──────────┐
│  CLI   │  │FileProcessor │  │ IRuleRegistry│  │  ISanitizer │  │ Manifest │
└───┬────┘  └──────┬───────┘  └──────┬───────┘  └──────┬──────┘  └────┬─────┘
    │              │                 │                 │              │
    │ Sanitize(src)│                 │                 │              │
    │─────────────▶│                 │                 │              │
    │              │                 │                 │              │
    │              │ CopyDirectory() │                 │              │
    │              │─────────────────│                 │              │
    │              │                 │                 │              │
    │              │ GetActiveRules()│                 │              │
    │              │────────────────▶│                 │              │
    │              │◀────────────────│                 │              │
    │              │   rules[]       │                 │              │
    │              │                 │                 │              │
    │              │ For each file:  │                 │              │
    │              │─────────────────────────────────▶ │              │
    │              │                 Sanitize(content) │              │
    │              │◀─────────────────────────────────│              │
    │              │     SanitizationResult           │              │
    │              │     (content, mappings)          │              │
    │              │                                  │              │
    │              │ Write sanitized file             │              │
    │              │                                  │              │
    │              │ Save manifest                    │              │
    │              │────────────────────────────────────────────────▶│
    │              │                                  │              │
    │◀─────────────│                                  │              │
    │   Summary    │                                  │              │
```

---

## 7. Data Models

### 7.1 CodeBleach Directory Structure

The `.codebleach/` directory is created in the root of the sanitized directory and contains all metadata needed for restoration.

```
my-app-sanitize/
├── .codebleach/
│   ├── manifest.json      # Machine-readable metadata
│   └── xref.md            # Human-readable cross-reference
├── src/
│   └── ...
└── ...
```

### 7.2 Manifest (`.codebleach/manifest.json`)

Machine-readable JSON containing all metadata for restoration.

```csharp
public record Manifest
{
    public required string Version { get; init; }           // "1.0"
    public required string SourcePath { get; init; }        // Original directory path
    public required string DestinationPath { get; init; }   // Sanitized directory path
    public required DateTime CreatedAt { get; init; }       // When sanitization occurred
    public required DateTime? RestoredAt { get; init; }     // When last restored (null if never)
    public required MappingTable Mappings { get; init; }    // Original ↔ Alias mappings
    public required IReadOnlyList<string> ProcessedFiles { get; init; }  // List of sanitized files
    public required SanitizationStats Stats { get; init; }  // Summary statistics
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

### 7.3 Cross-Reference (`.codebleach/xref.md`)

Human-readable Markdown file showing all substitutions made. This file is for **developer reference only** - the `manifest.json` is used for actual restoration.

```markdown
# CodeBleach Cross-Reference

> **Source:** `/Users/dev/projects/my-app`  
> **Sanitized:** `2026-01-15 10:30:00`  
> **Files Processed:** 12

## Substitution Table

| Original Value | Alias | Type | Occurrences |
|----------------|-------|------|-------------|
| `ProductionDB` | `SERVER_0` | Server | 3 |
| `StagingDB` | `SERVER_1` | Server | 1 |
| `192.168.1.100` | `IP_0` | IP Address | 4 |
| `db-prod-01.internal.corp.com` | `HOST_0` | Hostname | 2 |
| `user_accounts` | `TABLE_0` | Table | 5 |

## Files Modified

| File | Replacements |
|------|--------------|
| `src/appsettings.json` | 3 |
| `src/Data/DbContext.cs` | 4 |
| `src/Program.cs` | 2 |
| `src/Services/ApiClient.cs` | 6 |

## Quick Restore

To restore original values:
\```bash
cd /Users/dev/projects/my-app-sanitize
codebleach restore
\```

To restore and write back to original location:
\```bash
codebleach restore --writeback
\```
```

**Purpose of `xref.md`:**
- Developers can quickly see what was sanitized
- Viewable directly in GitHub/GitLab/editors
- Serves as documentation when sharing code
- No need to parse JSON to understand mappings

### 7.4 MappingTable

```csharp
public class MappingTable
{
    // Original value → Alias (e.g., "ProductionDB" → "SERVER_0")
    public Dictionary<string, string> Forward { get; init; } = new();
    
    // Alias → Original value (e.g., "SERVER_0" → "ProductionDB")
    public Dictionary<string, string> Reverse { get; init; } = new();
    
    // Counter per prefix for alias generation
    public Dictionary<string, int> Counters { get; init; } = new();
}
```

### 7.5 SanitizationRule

```csharp
public record SanitizationRule
{
    public required string RuleId { get; init; }
    public required string Name { get; init; }
    public required string Description { get; init; }
    public required string Pattern { get; init; }        // Regex pattern
    public required string Prefix { get; init; }         // Alias prefix (SERVER, IP, etc.)
    public required RuleSeverity Severity { get; init; }
    public required bool Enabled { get; init; }
    public IReadOnlyList<string> Exceptions { get; init; } = [];
    public int Order { get; init; } = 100;
}

public enum RuleSeverity
{
    Low,
    Medium,
    High,
    Critical
}
```

### 7.6 Sample Manifest File (`.codebleach/manifest.json`)

```json
{
  "version": "1.0",
  "sourcePath": "/Users/dev/projects/my-app",
  "destinationPath": "/Users/dev/projects/my-app-sanitize",
  "createdAt": "2026-01-15T10:30:00Z",
  "restoredAt": null,
  "mappings": {
    "forward": {
      "ProductionDB": "SERVER_0",
      "StagingDB": "SERVER_1",
      "192.168.1.100": "IP_0",
      "db-prod-01.internal.corp.com": "HOST_0",
      "user_accounts": "TABLE_0"
    },
    "reverse": {
      "SERVER_0": "ProductionDB",
      "SERVER_1": "StagingDB",
      "IP_0": "192.168.1.100",
      "HOST_0": "db-prod-01.internal.corp.com",
      "TABLE_0": "user_accounts"
    },
    "counters": {
      "SERVER": 2,
      "IP": 1,
      "HOST": 1,
      "TABLE": 1
    }
  },
  "processedFiles": [
    "src/Program.cs",
    "src/appsettings.json",
    "src/Data/DbContext.cs"
  ],
  "stats": {
    "totalFiles": 45,
    "processedFiles": 12,
    "skippedFiles": 33,
    "totalReplacements": 27,
    "uniqueValuesReplaced": 5,
    "processingTimeMs": 342
  }
}
```

---

## 8. CLI Specification

### 8.1 Installation

```bash
# Install globally from NuGet
dotnet tool install -g CodeBleach

# Update
dotnet tool update -g CodeBleach

# Uninstall
dotnet tool uninstall -g CodeBleach
```

### 8.2 Commands

#### 8.2.1 Sanitize (Default Command)

```bash
codebleach <source-directory> [options]

# Examples:
codebleach ~/projects/my-app
codebleach ./src --output ./src-clean
codebleach . --dry-run
```

**Arguments:**

| Argument | Description | Required |
|----------|-------------|----------|
| `source-directory` | Path to directory to sanitize | Yes |

**Options:**

| Option | Short | Description | Default |
|--------|-------|-------------|---------|
| `--output <path>` | `-o` | Custom output directory | `<source>-sanitize` |
| `--dry-run` | `-n` | Show what would be done without doing it | `false` |
| `--verbose` | `-v` | Show detailed output | `false` |
| `--force` | `-f` | Overwrite existing output directory | `false` |

**Output:**

```
Sanitizing: /Users/dev/projects/my-app
Output:     /Users/dev/projects/my-app-sanitize

Copying files...
Processing files...

Summary:
  Files copied:     45
  Files processed:  12
  Files skipped:    33 (binary/ignored)
  Replacements:     27
  Unique values:    5

Mappings created:
  ProductionDB      -> SERVER_0
  StagingDB         -> SERVER_1
  192.168.1.100     -> IP_0
  db-prod-01.internal.corp.com -> HOST_0
  user_accounts     -> TABLE_0

Done in 342ms
Output: /Users/dev/projects/my-app-sanitize
```

#### 8.2.2 Restore

```bash
codebleach restore [options]

# Examples:
codebleach restore
codebleach restore --writeback
codebleach restore --writeback --yes
```

**Options:**

| Option | Short | Description | Default |
|--------|-------|-------------|---------|
| `--writeback` | `-w` | Write restored files back to original location | `false` |
| `--yes` | `-y` | Skip confirmation prompts | `false` |
| `--verbose` | `-v` | Show detailed output | `false` |
| `--dry-run` | `-n` | Show what would be done | `false` |

**Output:**

```
Restoring: /Users/dev/projects/my-app-sanitize
Source:    /Users/dev/projects/my-app

Restoring aliases in 12 files...

Summary:
  Files processed:  12
  Replacements:     27

Mappings restored:
  SERVER_0  -> ProductionDB
  SERVER_1  -> StagingDB
  IP_0      -> 192.168.1.100
  HOST_0    -> db-prod-01.internal.corp.com
  TABLE_0   -> user_accounts

Done in 156ms
```

#### 8.2.3 Status

```bash
codebleach status [directory]

# Examples:
codebleach status
codebleach status ./my-project-sanitize
```

**Output (in sanitized directory):**

```
CodeBleach Status
-----------------
Directory:  /Users/dev/projects/my-app-sanitize
Type:       Sanitized copy
Source:     /Users/dev/projects/my-app
Created:    2026-01-15 10:30:00
Restored:   Never

Mappings (5):
  SERVER_0  <-> ProductionDB
  SERVER_1  <-> StagingDB
  IP_0      <-> 192.168.1.100
  HOST_0    <-> db-prod-01.internal.corp.com
  TABLE_0   <-> user_accounts

Files processed: 12
```

**Output (not in sanitized directory):**

```
CodeBleach Status
-----------------
Directory:  /Users/dev/projects/my-app
Type:       Regular directory (not sanitized)

Run 'codebleach .' to create a sanitized copy.
```

### 8.3 Dry-Run Mode

Dry-run mode shows exactly what changes would be made without modifying any files.

#### 8.3.1 Sanitize Dry-Run

```bash
codebleach ~/projects/my-app --dry-run
```

**Output:**

```
DRY RUN - No files will be modified

Sanitizing: /Users/dev/projects/my-app
Output:     /Users/dev/projects/my-app-sanitize (would be created)

Files to copy: 45
Files to process: 12
Files to skip: 33 (binary/ignored)

Detected sensitive values:

  src/appsettings.json:
    Line 3:  "Server=ProductionDB;..."
             ProductionDB -> SERVER_0

    Line 7:  "Host": "192.168.1.100"
             192.168.1.100 -> IP_0

  src/Data/DbContext.cs:
    Line 12: .UseSqlServer("ProductionDB")
             ProductionDB -> SERVER_0 (existing mapping)

    Line 45: // Connect to db-prod-01.internal.corp.com
             db-prod-01.internal.corp.com -> HOST_0

  src/Program.cs:
    Line 8:  var endpoint = "192.168.1.100:8080";
             192.168.1.100 -> IP_0 (existing mapping)

Summary:
  Files that would be processed:  12
  Total replacements:             5
  Unique values to map:           3

Mappings that would be created:
  ProductionDB                    -> SERVER_0
  192.168.1.100                   -> IP_0
  db-prod-01.internal.corp.com    -> HOST_0

Run without --dry-run to execute.
```

#### 8.3.2 Restore Dry-Run

```bash
codebleach restore --dry-run
```

**Output:**

```
DRY RUN - No files will be modified

Restoring: /Users/dev/projects/my-app-sanitize
Source:    /Users/dev/projects/my-app

Files to process: 12

Aliases to restore:

  src/appsettings.json:
    Line 3:  "Server=SERVER_0;..."
             SERVER_0 -> ProductionDB

    Line 7:  "Host": "IP_0"
             IP_0 -> 192.168.1.100

  src/Data/DbContext.cs:
    Line 12: .UseSqlServer("SERVER_0")
             SERVER_0 -> ProductionDB

    Line 45: // Connect to HOST_0
             HOST_0 -> db-prod-01.internal.corp.com

  src/Program.cs:
    Line 8:  var endpoint = "IP_0:8080";
             IP_0 -> 192.168.1.100

Summary:
  Files that would be modified:  3
  Total restorations:            5

Run without --dry-run to execute.
```

#### 8.3.3 Writeback Dry-Run

```bash
codebleach restore --writeback --dry-run
```

**Output:**

```
DRY RUN - No files will be modified

Restoring: /Users/dev/projects/my-app-sanitize
Writeback: /Users/dev/projects/my-app

Files that would be written back:

  src/appsettings.json
    Source:      /Users/dev/projects/my-app-sanitize/src/appsettings.json
    Destination: /Users/dev/projects/my-app/src/appsettings.json
    Status:      Modified (3 aliases restored)

  src/Data/DbContext.cs
    Source:      /Users/dev/projects/my-app-sanitize/src/Data/DbContext.cs
    Destination: /Users/dev/projects/my-app/src/Data/DbContext.cs
    Status:      Modified (2 aliases restored)

  src/Program.cs
    Source:      /Users/dev/projects/my-app-sanitize/src/Program.cs
    Destination: /Users/dev/projects/my-app/src/Program.cs
    Status:      Modified (1 alias restored)

Summary:
  Files to write back:  3
  Files unchanged:      9 (would be skipped)

Run without --dry-run to execute.
```

#### 8.3.4 Dry-Run Output Format

| Element | Format |
|---------|--------|
| File paths | Relative to project root |
| Line numbers | 1-based, showing context |
| Mappings | `original -> alias` or `alias -> original` |
| Status indicators | `(existing mapping)`, `(would be created)`, etc. |

#### 8.3.5 Diff Mode (Verbose Dry-Run)

```bash
codebleach ~/projects/my-app --dry-run --verbose
```

Shows unified diff format for each file:

```
DRY RUN - No files will be modified

--- src/appsettings.json (original)
+++ src/appsettings.json (sanitized)
@@ -1,8 +1,8 @@
 {
   "ConnectionStrings": {
-    "Default": "Server=ProductionDB;Database=users;..."
+    "Default": "Server=SERVER_0;Database=TABLE_0;..."
   },
   "Redis": {
-    "Host": "192.168.1.100",
+    "Host": "IP_0",
     "Port": 6379
   }
 }

--- src/Data/DbContext.cs (original)
+++ src/Data/DbContext.cs (sanitized)
@@ -10,7 +10,7 @@
     protected override void OnConfiguring(DbContextOptionsBuilder options)
     {
-        options.UseSqlServer("ProductionDB");
+        options.UseSqlServer("SERVER_0");
     }

[... additional files ...]

Summary:
  Files: 12 would be processed
  Lines: 27 would be changed
```

### 8.4 Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Error (see stderr for details) |
| 2 | Invalid arguments / usage error |

### 8.4 Error Messages

All errors written to stderr, plain text, no emojis:

```bash
# Source not found
Error: Source directory not found: /path/to/nonexistent

# Output already exists
Error: Output directory already exists: /path/to/output
Use --force to overwrite.

# Not a sanitized directory
Error: Not a sanitized directory (no .codebleach/ directory found)
Run 'codebleach <path>' to sanitize a directory first.

# Manifest corrupted
Error: Manifest file is corrupted or invalid
```

---

## 9. Sanitization Rules

### 9.1 Built-in Rules

| Rule ID | Name | Pattern | Prefix | Severity |
|---------|------|---------|--------|----------|
| `server_names` | Server/Database Names | `\b[A-Z][a-zA-Z]*DB\d*\b` | `SERVER` | Medium |
| `prod_databases` | Production Databases | `\b[a-zA-Z]+[_-]?[Pp]rod(uction)?\b` | `SERVER` | Medium |
| `table_names_prod` | Production Tables | `\b[a-z_]+_prod\b` | `TABLE` | Medium |
| `table_names_users` | User Tables | `\b(users?|accounts?|customers?)(_\w+)?\b` | `TABLE` | Medium |
| `private_ip_10` | Private IP (10.x) | `\b10\.\d{1,3}\.\d{1,3}\.\d{1,3}\b` | `IP` | High |
| `private_ip_172` | Private IP (172.x) | `\b172\.(1[6-9]|2\d|3[01])\.\d{1,3}\.\d{1,3}\b` | `IP` | High |
| `private_ip_192` | Private IP (192.168.x) | `\b192\.168\.\d{1,3}\.\d{1,3}\b` | `IP` | High |
| `connection_string` | Connection Strings | `(?i)(server|data source|host)=[^;]+;` | `CONNSTR` | High |
| `windows_path` | Windows Paths | `[A-Za-z]:\\[^\s*?"<>|:]+` | `PATH` | Medium |
| `unc_path` | UNC Paths | `\\\\[a-zA-Z0-9._-]+\\[^\s]+` | `PATH` | Medium |
| `internal_hostname` | Internal Hostnames | `\b[a-z][a-z0-9-]*\.(internal|local|corp|lan)\b` | `HOST` | Medium |

### 9.2 Alias Generation

Aliases are generated in the format `{PREFIX}_{N}` where N is a zero-based counter per prefix:

```
First server found:    ProductionDB   -> SERVER_0
Second server found:   StagingDB      -> SERVER_1
First IP found:        192.168.1.100  -> IP_0
Second IP found:       192.168.1.101  -> IP_1
```

**Consistency Rule:** The same original value always maps to the same alias within a sanitization run.

---

## 10. File Processing

### 10.1 Supported File Extensions

| Category | Extensions |
|----------|------------|
| **Code** | `.cs`, `.fs`, `.vb`, `.js`, `.ts`, `.jsx`, `.tsx`, `.py`, `.go`, `.rs`, `.java`, `.kt`, `.swift`, `.rb`, `.php` |
| **Config** | `.json`, `.yaml`, `.yml`, `.xml`, `.config`, `.ini`, `.toml`, `.env` |
| **Scripts** | `.sh`, `.bash`, `.ps1`, `.psm1`, `.bat`, `.cmd` |
| **Web** | `.html`, `.htm`, `.css`, `.scss`, `.less` |
| **Data** | `.sql`, `.graphql` |
| **Docs** | `.md`, `.txt`, `.rst` |

### 10.2 Ignored Patterns (Default)

| Category | Patterns |
|----------|----------|
| **CodeBleach** | `.codebleach/` |
| **Version Control** | `.git/`, `.svn/`, `.hg/` |
| **Dependencies** | `node_modules/`, `vendor/`, `packages/`, `.nuget/` |
| **Build Output** | `bin/`, `obj/`, `dist/`, `build/`, `target/`, `out/` |
| **IDE** | `.vs/`, `.vscode/`, `.idea/`, `*.suo`, `*.user` |
| **Binary** | `*.exe`, `*.dll`, `*.so`, `*.dylib`, `*.pdb` |
| **Images** | `*.png`, `*.jpg`, `*.jpeg`, `*.gif`, `*.ico`, `*.svg` |
| **Archives** | `*.zip`, `*.tar`, `*.gz`, `*.rar`, `*.7z` |
| **Certificates** | `*.pfx`, `*.p12`, `*.cer`, `*.crt`, `*.key` |

### 10.3 File Size Limit

- Skip files larger than **10 MB** (likely binary or generated)
- Configurable via `--max-file-size`

---

## 11. Testing Strategy

### 11.1 Test-Driven Development (TDD)

**All features MUST follow strict TDD workflow:**

```
1. RED    → Write failing test first
2. GREEN  → Write minimum code to pass
3. REFACTOR → Clean up while keeping tests green
```

### 11.2 Test Project Structure

```
tests/
├── CodeBleach.Tests/                    # Unit tests
│   ├── Services/
│   │   ├── SanitizerTests.cs
│   │   ├── RestorerTests.cs
│   │   ├── FileProcessorTests.cs
│   │   └── ManifestManagerTests.cs
│   ├── Rules/
│   │   ├── RuleRegistryTests.cs
│   │   ├── BuiltInRulesTests.cs
│   │   └── PatternMatchingTests.cs
│   ├── Models/
│   │   ├── MappingTableTests.cs
│   │   └── ManifestTests.cs
│   └── CodeBleach.Tests.csproj
│
├── CodeBleach.IntegrationTests/         # CLI integration tests
│   ├── SanitizeCommandTests.cs
│   ├── RestoreCommandTests.cs
│   ├── StatusCommandTests.cs
│   ├── DryRunTests.cs
│   ├── RoundTripTests.cs
│   └── CodeBleach.IntegrationTests.csproj
│
└── fixtures/                            # Test data
    ├── sample-project/
    ├── edge-cases/
    └── expected-output/
```

### 11.3 Unit Test Specifications

#### 11.3.1 Sanitizer Tests

| Test Case | Description | Priority |
|-----------|-------------|----------|
| `Sanitize_WithServerName_ReplacesWithAlias` | Basic server name detection | P0 |
| `Sanitize_WithMultipleOccurrences_UsesSameAlias` | Consistency check | P0 |
| `Sanitize_WithPrivateIP_ReplacesWithAlias` | IP detection (10.x, 172.x, 192.168.x) | P0 |
| `Sanitize_WithConnectionString_ReplacesServerPart` | Connection string handling | P0 |
| `Sanitize_WithNoSensitiveData_ReturnsUnchanged` | No false positives | P0 |
| `Sanitize_WithException_SkipsExceptedValue` | Exception list works | P1 |
| `Sanitize_WithEmptyContent_ReturnsEmpty` | Edge case | P1 |
| `Sanitize_WithLargeContent_CompletesWithinTimeout` | Performance | P1 |

```csharp
[Fact]
public void Sanitize_WithServerName_ReplacesWithAlias()
{
    // Arrange
    var sanitizer = new Sanitizer(_ruleRegistry);
    var content = "SELECT * FROM ProductionDB.users";
    
    // Act
    var result = sanitizer.Sanitize(content);
    
    // Assert
    result.Content.Should().Be("SELECT * FROM SERVER_0.users");
    result.WasSanitized.Should().BeTrue();
    result.Mappings.Forward.Should().ContainKey("ProductionDB");
}

[Theory]
[InlineData("192.168.1.100", "IP_0")]
[InlineData("10.0.0.50", "IP_0")]
[InlineData("172.16.0.1", "IP_0")]
public void Sanitize_WithPrivateIP_ReplacesWithAlias(string ip, string expectedPrefix)
{
    // Arrange
    var sanitizer = new Sanitizer(_ruleRegistry);
    var content = $"Host: {ip}";
    
    // Act
    var result = sanitizer.Sanitize(content);
    
    // Assert
    result.Content.Should().StartWith($"Host: {expectedPrefix}");
    result.WasSanitized.Should().BeTrue();
}
```

#### 11.3.2 Restorer Tests

| Test Case | Description | Priority |
|-----------|-------------|----------|
| `Restore_WithAliases_ReplacesWithOriginals` | Basic restoration | P0 |
| `Restore_WithMultipleAliases_RestoresAll` | Multiple mappings | P0 |
| `Restore_WithNoAliases_ReturnsUnchanged` | No aliases present | P1 |
| `Restore_WithPartialAlias_DoesNotReplace` | SERVER_0 vs SERVER_01 | P0 |
| `Restore_WithNestedAliases_RestoresCorrectly` | Longer aliases first | P0 |

```csharp
[Fact]
public void Restore_WithAliases_ReplacesWithOriginals()
{
    // Arrange
    var restorer = new Restorer();
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
    var result = restorer.Restore(content, mappings);
    
    // Assert
    result.Should().Be("SELECT * FROM ProductionDB.users");
}
```

#### 11.3.3 FileProcessor Tests

| Test Case | Description | Priority |
|-----------|-------------|----------|
| `ShouldProcess_WithCsFile_ReturnsTrue` | Supported extension | P0 |
| `ShouldProcess_WithDllFile_ReturnsFalse` | Binary excluded | P0 |
| `ShouldProcess_WithNodeModules_ReturnsFalse` | Ignored directory | P0 |
| `ShouldProcess_WithGitignoreMatch_ReturnsFalse` | Respects .gitignore | P1 |
| `CopyDirectory_CreatesCorrectStructure` | Directory copy | P0 |
| `CopyDirectory_SkipsBinaryFiles` | Skip binaries | P0 |

#### 11.3.4 ManifestManager Tests

| Test Case | Description | Priority |
|-----------|-------------|----------|
| `Save_CreatesValidJson` | Serialization | P0 |
| `Load_ReadsValidManifest` | Deserialization | P0 |
| `Load_WithMissingFile_ThrowsException` | Error handling | P0 |
| `Load_WithCorruptedJson_ThrowsException` | Error handling | P0 |
| `GenerateXref_CreatesValidMarkdown` | Xref generation | P0 |

### 11.4 Integration Test Specifications

#### 11.4.1 Round-Trip Tests

```csharp
[Fact]
public async Task RoundTrip_SanitizeAndRestore_PreservesOriginalContent()
{
    // Arrange
    var sourceDir = CreateTempDirectory();
    var originalContent = @"
        var server = ""ProductionDB"";
        var ip = ""192.168.1.100"";
    ";
    await File.WriteAllTextAsync(Path.Combine(sourceDir, "test.cs"), originalContent);
    
    // Act - Sanitize
    var sanitizeResult = await RunCli($"codebleach {sourceDir}");
    sanitizeResult.ExitCode.Should().Be(0);
    
    var sanitizedDir = sourceDir + "-sanitize";
    var sanitizedContent = await File.ReadAllTextAsync(
        Path.Combine(sanitizedDir, "test.cs"));
    
    // Verify sanitization
    sanitizedContent.Should().NotContain("ProductionDB");
    sanitizedContent.Should().NotContain("192.168.1.100");
    sanitizedContent.Should().Contain("SERVER_0");
    sanitizedContent.Should().Contain("IP_0");
    
    // Act - Restore
    var restoreResult = await RunCli("codebleach restore", workingDir: sanitizedDir);
    restoreResult.ExitCode.Should().Be(0);
    
    var restoredContent = await File.ReadAllTextAsync(
        Path.Combine(sanitizedDir, "test.cs"));
    
    // Assert - Original values restored
    restoredContent.Should().Contain("ProductionDB");
    restoredContent.Should().Contain("192.168.1.100");
    restoredContent.Should().NotContain("SERVER_0");
    restoredContent.Should().NotContain("IP_0");
}
```

#### 11.4.2 Dry-Run Tests

```csharp
[Fact]
public async Task DryRun_DoesNotModifyFileSystem()
{
    // Arrange
    var sourceDir = CreateTempDirectory();
    await File.WriteAllTextAsync(
        Path.Combine(sourceDir, "test.cs"), 
        "var server = \"ProductionDB\";"
    );
    
    var expectedOutputDir = sourceDir + "-sanitize";
    
    // Act
    var result = await RunCli($"codebleach {sourceDir} --dry-run");
    
    // Assert
    result.ExitCode.Should().Be(0);
    result.StdOut.Should().Contain("DRY RUN");
    result.StdOut.Should().Contain("ProductionDB -> SERVER_0");
    Directory.Exists(expectedOutputDir).Should().BeFalse();
}
```

#### 11.4.3 CLI Integration Tests

| Test Case | Command | Expected |
|-----------|---------|----------|
| `Sanitize_ValidDirectory_CreatesOutput` | `codebleach ./src` | Exit 0, creates ./src-sanitize |
| `Sanitize_NonExistentPath_ReturnsError` | `codebleach ./missing` | Exit 1, error message |
| `Sanitize_OutputExists_ReturnsError` | `codebleach ./src` (2nd time) | Exit 1, use --force |
| `Sanitize_WithForce_OverwritesOutput` | `codebleach ./src --force` | Exit 0, overwrites |
| `Restore_InSanitizedDir_RestoresFiles` | `codebleach restore` | Exit 0, files restored |
| `Restore_NotInSanitizedDir_ReturnsError` | `codebleach restore` | Exit 1, error message |
| `Status_InSanitizedDir_ShowsInfo` | `codebleach status` | Exit 0, shows mappings |
| `DryRun_ShowsChanges_NoModification` | `codebleach ./src --dry-run` | Exit 0, no files created |

### 11.5 Test Coverage Requirements

| Metric | Minimum | Target |
|--------|---------|--------|
| **Line Coverage** | 80% | 90% |
| **Branch Coverage** | 75% | 85% |
| **Method Coverage** | 85% | 95% |

### 11.6 Test Fixtures

```
tests/fixtures/
├── sample-project/                      # Typical .NET project
│   ├── src/
│   │   ├── Program.cs                   # Contains server names, IPs
│   │   ├── appsettings.json             # Contains connection strings
│   │   └── Data/
│   │       └── DbContext.cs             # Contains database references
│   ├── tests/
│   │   └── UnitTest1.cs
│   ├── .gitignore
│   └── sample-project.csproj
│
├── edge-cases/
│   ├── deep-nesting/                    # 10+ levels deep
│   │   └── level1/.../level10/file.cs
│   ├── large-files/
│   │   └── large.cs                     # Near 10MB limit
│   ├── special-chars/
│   │   └── файл with spaces.cs          # Unicode + spaces
│   ├── binary-mixed/
│   │   ├── code.cs
│   │   └── image.png                    # Should be skipped
│   └── no-sensitive-data/
│       └── clean.cs                     # No matches expected
│
├── expected-output/
│   └── sample-project-sanitize/         # Expected sanitization result
│       ├── src/
│       │   ├── Program.cs
│       │   └── appsettings.json
│       └── .codebleach/
│           ├── manifest.json
│           └── xref.md
│
└── manifests/
    ├── valid-manifest.json              # For deserialization tests
    ├── corrupted-manifest.json          # For error handling tests
    └── v1-manifest.json                 # For version compatibility
```

### 11.7 Performance Tests

```csharp
[Fact]
public async Task Performance_1000Files_CompletesUnder5Seconds()
{
    // Arrange
    var sourceDir = CreateDirectoryWith1000Files();
    var stopwatch = Stopwatch.StartNew();
    
    // Act
    var result = await RunCli($"codebleach {sourceDir}");
    stopwatch.Stop();
    
    // Assert
    result.ExitCode.Should().Be(0);
    stopwatch.ElapsedMilliseconds.Should().BeLessThan(5000);
}

[Fact]
public async Task Performance_LargeFile_ProcessesEfficiently()
{
    // Arrange - 5MB file with repeated patterns
    var sourceDir = CreateTempDirectory();
    var content = string.Join("\n", 
        Enumerable.Repeat("Server=ProductionDB;IP=192.168.1.100;", 100000));
    await File.WriteAllTextAsync(Path.Combine(sourceDir, "large.sql"), content);
    
    var stopwatch = Stopwatch.StartNew();
    
    // Act
    var result = await RunCli($"codebleach {sourceDir}");
    stopwatch.Stop();
    
    // Assert
    result.ExitCode.Should().Be(0);
    stopwatch.ElapsedMilliseconds.Should().BeLessThan(3000);
}
```

### 11.8 Running Tests

```bash
# Run all tests
dotnet test

# Run with coverage
dotnet test --collect:"XPlat Code Coverage"

# Run specific test category
dotnet test --filter "Category=Unit"
dotnet test --filter "Category=Integration"

# Run with verbose output
dotnet test --logger "console;verbosity=detailed"

# Generate coverage report
dotnet tool install -g dotnet-reportgenerator-globaltool
reportgenerator -reports:coverage.cobertura.xml -targetdir:coverage-report
```

---

## 12. Release & Deployment

### 12.1 Versioning Strategy

**Semantic Versioning (SemVer):** `MAJOR.MINOR.PATCH`

| Version Component | When to Increment |
|-------------------|-------------------|
| **MAJOR** | Breaking changes (CLI args, manifest format) |
| **MINOR** | New features (new rules, new commands) |
| **PATCH** | Bug fixes, performance improvements |

### 12.2 Release Checklist

Before any release:

- [ ] All tests passing (`dotnet test`)
- [ ] Coverage meets minimum (80% line, 75% branch)
- [ ] Version updated in `CodeBleach.csproj`
- [ ] CHANGELOG.md updated
- [ ] README.md updated if needed
- [ ] No compiler warnings
- [ ] Manual smoke test on Windows, macOS, Linux

### 12.3 Git Tagging

```bash
# Create annotated tag
git tag -a v1.0.0 -m "Release v1.0.0 - Initial release"

# Push tag to remote
git push origin v1.0.0

# List tags
git tag -l "v*"
```

**Tag Format:** `v{MAJOR}.{MINOR}.{PATCH}` (e.g., `v1.0.0`, `v1.2.3`)

### 12.4 Build Artifacts

```bash
# Clean build
dotnet clean
dotnet restore

# Build release
dotnet build -c Release

# Run tests
dotnet test -c Release

# Create NuGet package
dotnet pack -c Release -o ./artifacts

# Artifacts created:
# ./artifacts/CodeBleach.{version}.nupkg
```

### 12.5 NuGet Deployment

#### Manual Deployment

```bash
# Pack
dotnet pack -c Release -o ./artifacts

# Push to NuGet.org
dotnet nuget push ./artifacts/CodeBleach.*.nupkg \
    --api-key $NUGET_API_KEY \
    --source https://api.nuget.org/v3/index.json

# Verify on NuGet.org
# https://www.nuget.org/packages/CodeBleach
```

#### Automated Deployment (GitHub Actions)

```yaml
# .github/workflows/release.yml
name: Release to NuGet

on:
  push:
    tags:
      - 'v*'

env:
  DOTNET_VERSION: '10.0.x'

jobs:
  build-and-test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup .NET
        uses: actions/setup-dotnet@v4
        with:
          dotnet-version: ${{ env.DOTNET_VERSION }}
      
      - name: Restore dependencies
        run: dotnet restore
      
      - name: Build
        run: dotnet build -c Release --no-restore
      
      - name: Test
        run: dotnet test -c Release --no-build --verbosity normal
      
      - name: Test Coverage
        run: |
          dotnet test -c Release --no-build \
            --collect:"XPlat Code Coverage" \
            --results-directory ./coverage
      
      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          directory: ./coverage

  release:
    needs: build-and-test
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup .NET
        uses: actions/setup-dotnet@v4
        with:
          dotnet-version: ${{ env.DOTNET_VERSION }}
      
      - name: Extract version from tag
        id: version
        run: echo "VERSION=${GITHUB_REF#refs/tags/v}" >> $GITHUB_OUTPUT
      
      - name: Build Release
        run: dotnet build -c Release -p:Version=${{ steps.version.outputs.VERSION }}
      
      - name: Pack
        run: dotnet pack -c Release -p:Version=${{ steps.version.outputs.VERSION }} -o ./artifacts
      
      - name: Push to NuGet
        run: |
          dotnet nuget push ./artifacts/*.nupkg \
            --api-key ${{ secrets.NUGET_API_KEY }} \
            --source https://api.nuget.org/v3/index.json \
            --skip-duplicate
      
      - name: Create GitHub Release
        uses: softprops/action-gh-release@v1
        with:
          files: ./artifacts/*.nupkg
          generate_release_notes: true
```

### 12.6 GitHub Secrets Configuration

| Secret Name | Description |
|-------------|-------------|
| `NUGET_API_KEY` | NuGet.org API key for package publishing |

**To configure:**
1. Go to GitHub repo → Settings → Secrets and variables → Actions
2. Click "New repository secret"
3. Name: `NUGET_API_KEY`
4. Value: Your NuGet.org API key

### 12.7 Release Process

```
1. Ensure all tests pass
   $ dotnet test

2. Update version in CodeBleach.csproj
   <Version>1.0.0</Version>

3. Update CHANGELOG.md
   ## [1.0.0] - 2026-01-15
   ### Added
   - Initial release
   - Sanitize command
   - Restore command
   - Dry-run mode

4. Commit changes
   $ git add -A
   $ git commit -m "Release v1.0.0"

5. Create and push tag
   $ git tag -a v1.0.0 -m "Release v1.0.0"
   $ git push origin main --tags

6. GitHub Actions automatically:
   - Runs all tests
   - Creates NuGet package
   - Publishes to NuGet.org
   - Creates GitHub release

7. Verify on NuGet.org
   https://www.nuget.org/packages/CodeBleach/1.0.0
```

### 12.8 Post-Release Verification

```bash
# Wait 5-10 minutes for NuGet indexing

# Install from NuGet
dotnet tool install -g CodeBleach

# Verify installation
codebleach --version
# Output: CodeBleach 1.0.0

# Quick smoke test
codebleach ~/test-project --dry-run
```

### 12.9 Rollback Procedure

If a release has critical issues:

```bash
# Unlist the broken version on NuGet.org
# (Go to NuGet.org → Manage Package → Unlist)

# Users can install previous version
dotnet tool install -g CodeBleach --version 0.9.0

# Create hotfix
git checkout -b hotfix/v1.0.1
# ... fix issue ...
git commit -m "Fix critical issue"
git tag -a v1.0.1 -m "Hotfix v1.0.1"
git push origin hotfix/v1.0.1 --tags
```

---

## Appendix A: Example Transformations

### Input: `appsettings.json`

```json
{
  "ConnectionStrings": {
    "Default": "Server=ProductionDB;Database=user_accounts;User=admin;Password=***"
  },
  "Redis": {
    "Host": "192.168.1.100",
    "Port": 6379
  },
  "ApiEndpoint": "https://api-prod.internal.corp.com/v1"
}
```

### Output: `appsettings.json` (sanitized)

```json
{
  "ConnectionStrings": {
    "Default": "Server=SERVER_0;Database=TABLE_0;User=admin;Password=***"
  },
  "Redis": {
    "Host": "IP_0",
    "Port": 6379
  },
  "ApiEndpoint": "https://HOST_0/v1"
}
```

### Mappings Generated

```
ProductionDB                    -> SERVER_0
user_accounts                   -> TABLE_0
192.168.1.100                   -> IP_0
api-prod.internal.corp.com      -> HOST_0
```

---

## Appendix B: Future Enhancements (Out of Scope v1.0)

| Enhancement | Description | Priority |
|-------------|-------------|----------|
| Custom rules file | Load rules from `.codebleach.yaml` | P2 |
| Git integration | Auto-stage sanitized files | P3 |
| Watch mode | Re-sanitize on file changes | P3 |
| Clipboard support | Sanitize clipboard content | P2 |
| VS Code extension | Integrated sanitization | P3 |

---

**Document Control:**
- **Version:** 1.0 (Draft)
- **Author:** Architecture Team  
- **Reviewers:** Engineering
- **Approval:** Pending

---

ROLE: architect STRICT=true

