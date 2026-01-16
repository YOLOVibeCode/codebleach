# CodeBleach Sanitization Demo

This sample project demonstrates CodeBleach sanitization capabilities.

## What Gets Sanitized

### Built-in Rules
- **Server/Database Names**: `ProductionDB` → `SERVER_0`
- **Private IPs**: `192.168.1.100` → `IP_0`
- **Internal Hostnames**: `api-prod.internal.corp.com` → `HOST_0`
- **Windows Paths**: `C:\Projects\SecretProject` → `PATH_0`
- **Connection Strings**: `Server=ProductionDB` → `CONNSTR_0`
- **Table Names**: `user_accounts` → `TABLE_0`

### Custom Rules (from `.codebleach-rules.json`)
- **API Keys**: `api_key_xxxxx` → `APIKEY_0`
- **JWT Tokens**: `eyJ...` → `JWT_0`
- **Internal Emails**: `user@internal.com` → `EMAIL_0`

## Running Sanitization

```bash
# Preview changes
codebleach sanitize . --dry-run

# Actually sanitize
codebleach sanitize . --force

# With verbose output
codebleach sanitize . --dry-run --verbose
```

## Restoring

After sanitization, restore original values:

```bash
cd sample-project-sanitize
codebleach restore
```

## Custom Rules

Edit `.codebleach-rules.json` to add project-specific patterns. See `CUSTOM_RULES.md` for details.

