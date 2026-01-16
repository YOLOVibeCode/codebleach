# Custom Rules Configuration

CodeBleach supports custom regex patterns via a `.codebleach-rules.json` configuration file. This allows you to add project-specific sanitization rules without recompiling the tool.

## Configuration File Location

Place `.codebleach-rules.json` in your project root (or any parent directory). CodeBleach will automatically search up the directory tree to find it.

## Configuration Format

```json
{
  "rules": [
    {
      "ruleId": "unique_rule_id",
      "name": "Human Readable Name",
      "description": "What this rule detects",
      "pattern": "regex pattern here",
      "prefix": "PREFIX",
      "severity": "Low|Medium|High|Critical",
      "enabled": true,
      "exceptions": ["value1", "value2"],
      "order": 100
    }
  ]
}
```

## Field Descriptions

| Field | Required | Description |
|-------|----------|-------------|
| `ruleId` | Yes | Unique identifier (e.g., "custom_api_keys") |
| `name` | Yes | Human-readable name |
| `description` | No | Description of what the rule detects |
| `pattern` | Yes | Regex pattern (standard .NET regex) |
| `prefix` | Yes | Alias prefix (e.g., "APIKEY", "JWT", "EMAIL") |
| `severity` | No | Severity level (default: Medium) |
| `enabled` | No | Whether rule is active (default: true) |
| `exceptions` | No | Array of values to exclude from matching |
| `order` | No | Processing order - lower numbers run first (default: 100) |

## Example: Custom API Key Detection

```json
{
  "rules": [
    {
      "ruleId": "custom_api_keys",
      "name": "API Keys",
      "description": "Detects API keys in the format 'api_key_xxxxx'",
      "pattern": "api_key_[a-zA-Z0-9]{20,}",
      "prefix": "APIKEY",
      "severity": "High",
      "enabled": true,
      "order": 5
    }
  ]
}
```

## Example: JWT Token Detection

```json
{
  "rules": [
    {
      "ruleId": "custom_jwt_tokens",
      "name": "JWT Tokens",
      "description": "Detects JWT tokens",
      "pattern": "eyJ[A-Za-z0-9-_=]+\\.eyJ[A-Za-z0-9-_=]+\\.[A-Za-z0-9-_=]+",
      "prefix": "JWT",
      "severity": "Critical",
      "enabled": true,
      "order": 1
    }
  ]
}
```

## Example: Internal Email Domains

```json
{
  "rules": [
    {
      "ruleId": "custom_email_domains",
      "name": "Internal Email Domains",
      "description": "Detects internal corporate email addresses",
      "pattern": "\\b[a-zA-Z0-9._%+-]+@(internal|corp|company)\\.(com|org|net)\\b",
      "prefix": "EMAIL",
      "severity": "Medium",
      "enabled": true,
      "exceptions": ["admin@internal.com"],
      "order": 15
    }
  ]
}
```

## Example: Custom Database Names

```json
{
  "rules": [
    {
      "ruleId": "company_db_names",
      "name": "Company Database Names",
      "description": "Detects company-specific database naming patterns",
      "pattern": "\\b(MyCompany|AcmeCorp)[A-Z][a-zA-Z]*DB\\b",
      "prefix": "DB",
      "severity": "High",
      "enabled": true,
      "order": 10
    }
  ]
}
```

## Rule Processing Order

Rules are processed in order of their `order` field (ascending). Lower numbers run first. Built-in rules have orders like:
- Server names: 10
- IP addresses: 20-22
- Connection strings: 30
- Paths: 40-41
- Hostnames: 50

Set custom rule orders accordingly:
- `< 10`: Run before built-in rules
- `10-50`: Run alongside built-in rules
- `> 50`: Run after built-in rules

## Exceptions

Use the `exceptions` array to exclude specific values from matching:

```json
{
  "ruleId": "internal_emails",
  "pattern": "\\b[a-z]+@internal\\.com\\b",
  "exceptions": ["admin@internal.com", "support@internal.com"]
}
```

## Testing Custom Rules

Use `--dry-run` to preview what custom rules will match:

```bash
codebleach sanitize ./my-project --dry-run --verbose
```

## Best Practices

1. **Use specific patterns**: Avoid overly broad patterns that might match legitimate code
2. **Test thoroughly**: Use `--dry-run` to verify rules work as expected
3. **Order matters**: Set appropriate `order` values to avoid conflicts
4. **Use exceptions**: Exclude known-safe values rather than disabling rules
5. **Document patterns**: Add clear descriptions for team members

## Regex Tips

- Use word boundaries `\b` to avoid partial matches
- Escape special characters: `\.` for literal dot, `\\` for backslash
- Use character classes: `[a-zA-Z0-9]` for alphanumeric
- Use quantifiers: `{20,}` for "20 or more characters"
- Use groups: `(option1|option2)` for alternatives

## Troubleshooting

**Rule not matching?**
- Check regex syntax (test in regex tester)
- Verify `enabled: true`
- Check if value is in `exceptions` list
- Use `--verbose` to see which rules are loaded

**Too many matches?**
- Make pattern more specific
- Add exceptions for false positives
- Increase `order` to run after other rules

**Rule conflicts?**
- Adjust `order` values
- Make patterns more specific
- Use exceptions strategically

