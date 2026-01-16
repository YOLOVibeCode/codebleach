# CodeBleach Sanitization Demo

## Sample Project Location

The sample project demonstrating sanitization is located at:

```
tests/fixtures/sample-project/
```

## What's Included

### Sample Files with Sensitive Data

1. **`src/Program.cs`** - Contains:
   - Production database name: `ProductionDB`
   - Private IP address: `192.168.1.100`
   - Internal hostname: `api-prod.internal.corp.com`
   - Windows file path: `C:\Projects\SecretProject\logs\app.log`

2. **`src/appsettings.json`** - Contains:
   - Connection string: `Server=ProductionDB;Database=user_accounts`
   - Redis host: `192.168.1.100`
   - Internal API endpoint: `https://api-prod.internal.corp.com/v1`

3. **`src/DbContext.cs`** - Contains:
   - Database connection: `ProductionDB`
   - Table name: `user_accounts`

4. **`README.md`** - Contains:
   - Database references
   - IP addresses
   - File paths

### Custom Rules Configuration

**`.codebleach-rules.json`** - Example custom rules:
- API key detection
- JWT token detection  
- Internal email domain detection

## Running the Demo

### 1. Preview Sanitization (Dry Run)

```bash
cd tests/fixtures/sample-project
dotnet run --project ../../../src/CodeBleach/CodeBleach.csproj -- sanitize . --dry-run --verbose
```

**Output:**
- Shows what files would be processed
- Shows what values would be replaced
- No files are modified

### 2. Perform Sanitization

```bash
dotnet run --project ../../../src/CodeBleach/CodeBleach.csproj -- sanitize . --force
```

**Creates:**
- `sample-project-sanitize/` directory
- `.codebleach/manifest.json` - Machine-readable mappings
- `.codebleach/xref.md` - Human-readable cross-reference

### 3. View Sanitized Results

```bash
cat sample-project-sanitize/src/Program.cs
cat sample-project-sanitize/src/appsettings.json
cat sample-project-sanitize/.codebleach/xref.md
```

**Example Output:**

**Before:**
```csharp
var connectionString = "Server=ProductionDB;Database=user_accounts;User=admin";
var redisHost = "192.168.1.100";
var apiUrl = "https://api-prod.internal.corp.com/v1/users";
```

**After:**
```csharp
var connectionString = "CONNSTR_0SERVER_0;Database=TABLE_0;User=admin";
var redisHost = "IP_0";
var apiUrl = "https://SERVER_1HOST_0.corp.com/v1/TABLE_1";
```

### 4. Restore Original Values

```bash
cd sample-project-sanitize
dotnet run --project ../../../src/CodeBleach/CodeBleach.csproj -- restore
```

**Result:** All aliases restored to original values

## Custom Rules Example

The sample project includes `.codebleach-rules.json`:

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
      "ruleId": "custom_jwt_tokens",
      "name": "JWT Tokens",
      "pattern": "eyJ[A-Za-z0-9-_=]+\\.eyJ[A-Za-z0-9-_=]+\\.[A-Za-z0-9-_=]+",
      "prefix": "JWT",
      "severity": "Critical"
    }
  ]
}
```

**Benefits:**
- No recompilation needed
- Project-specific patterns
- Version controlled with your code
- Automatically discovered

## What Gets Sanitized

### Built-in Patterns (11 rules)

| Pattern | Example | Alias |
|---------|---------|-------|
| Server names | `ProductionDB` | `SERVER_0` |
| Private IPs | `192.168.1.100` | `IP_0` |
| Internal hostnames | `api-prod.internal.corp.com` | `HOST_0` |
| Windows paths | `C:\Projects\SecretProject` | `PATH_0` |
| Connection strings | `Server=ProductionDB` | `CONNSTR_0` |
| Table names | `user_accounts` | `TABLE_0` |

### Custom Patterns (from `.codebleach-rules.json`)

| Pattern | Example | Alias |
|---------|---------|-------|
| API keys | `api_key_abc123...` | `APIKEY_0` |
| JWT tokens | `eyJhbGciOiJIUzI1NiIs...` | `JWT_0` |
| Internal emails | `user@internal.com` | `EMAIL_0` |

## Key Features Demonstrated

✅ **Pattern-based detection** - Regex rules for flexible matching  
✅ **Custom rules** - No recompilation needed  
✅ **Bidirectional mappings** - Perfect round-trip restoration  
✅ **Manifest tracking** - Complete audit trail  
✅ **Human-readable xref** - Easy to review changes  
✅ **Dry-run mode** - Preview before committing  
✅ **File filtering** - Skips binaries and ignored paths  

## Next Steps

1. **Add your own patterns** - Edit `.codebleach-rules.json`
2. **Test with your codebase** - Run on real projects
3. **Share sanitized code** - Safe to share with AI assistants
4. **Restore after AI edits** - Get original values back

See `CUSTOM_RULES.md` for detailed custom rules documentation.

