# CodeBleach v1.1.0 Release Notes

**Release Date:** 2026-01-16

## New Features

### 1. Three Custom Rule Types

You can now define custom sanitization rules using three different methods:

#### Inline Regex (`type: "regex"`)
```json
{
  "ruleId": "tables",
  "name": "Table Names",
  "type": "regex",
  "pattern": "\\bTB\\d{5}\\b",
  "prefix": "TBL"
}
```

#### Regex From File (`type: "regexFile"`)
```json
{
  "ruleId": "servers",
  "name": "Server Names",
  "type": "regexFile",
  "patternFile": "rules/servers.regex",
  "prefix": "SRV"
}
```

**rules/servers.regex:**
```regex
# No JSON escaping needed!
\b[A-Z]+SRV\d{1,3}\b
\b[A-Z]+SVR\d{1,3}\b
```

#### JavaScript Function (`type: "javascript"`)
```json
{
  "ruleId": "databases",
  "name": "Database Names",
  "type": "javascript",
  "scriptFile": "rules/databases.js",
  "prefix": "DB"
}
```

**rules/databases.js:**
```javascript
function getPattern() {
    return '(\\b[A-Z]+DB\\b|\\bDB[0-9]*[A-Z]+\\b)';
}
```

### 2. Jint JavaScript Engine Integration

- Sandboxed JavaScript execution for custom rules
- Limited recursion and statement count for safety
- No file system or network access from scripts

## Bug Fixes

### Fixed: Overlapping Match Corruption

**Before (v1.0.0):**
```
Server=PRODSRV01  →  CONNSTR_0SRV_0  (corrupted!)
```

**After (v1.1.0):**
```
Server=PRODSRV01  →  CONNSTR_0  (correct - longer match wins)
```

The sanitizer now:
- Sorts matches by position, then by length (longer first)
- Skips overlapping matches
- Ensures lossless round-trip sanitize → restore

### Round-Trip Verification

All 4-round-trip tests now pass:
```
Original MD5:  ebeeda154b6eb9cd596ef5132b07fc1b
Restore 1 MD5: ebeeda154b6eb9cd596ef5132b07fc1b
Restore 2 MD5: ebeeda154b6eb9cd596ef5132b07fc1b
SUCCESS: All hashes match!
```

## Dependencies Added

- **Jint 4.2.1** - JavaScript interpreter for .NET

## Test Coverage

- 51 tests passing
- 9 new tests for `CustomRuleLoaderTests`

---

## Promotion Commands

Run these commands to promote to NuGet:

```bash
# 1. Stage all changes
cd /Users/admin/Dev/YOLOProjects/codebleach
git add -A

# 2. Commit
git commit -m "Release v1.1.0 - Custom rule types (regex, regexFile, javascript) and overlapping match fix"

# 3. Tag
git tag v1.1.0

# 4. Push
git push origin main
git push origin v1.1.0

# 5. Build and pack
dotnet build -c Release
dotnet pack -c Release -o ./nupkg

# 6. Push to NuGet
dotnet nuget push ./nupkg/CodeBleach.1.1.0.nupkg --api-key YOUR_API_KEY --source https://api.nuget.org/v3/index.json
```

