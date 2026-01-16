# Quick Start: SQL Sanitization

## 30 Second Setup

```bash
# 1. Install CodeBleach
dotnet tool install -g CodeBleach

# 2. Copy SQL rules to your project
cd your-project
curl -o .codebleach-rules.json \
  https://raw.githubusercontent.com/YOLOVibeCode/codebleach/main/.codebleach-rules-sql.json

# 3. Sanitize
codebleach sanitize ./sql-scripts
```

Done! Your SQL files are now safe to share.

## What Just Happened?

**Before** (`migration.sql`):
```sql
SELECT * FROM DBZMEW.DW_PRIME_MCEF WHERE SERVER = 'PRODSRV01'
```

**After** (`sql-scripts-sanitize/migration.sql`):
```sql
SELECT * FROM SCHEMA_0 WHERE SERVER = 'SRV_0'
```

**Cross-Reference** (`.codebleach/xref.md`):
```markdown
| Original | Alias |
|----------|-------|
| DBZMEW.DW_PRIME_MCEF | SCHEMA_0 |
| PRODSRV01 | SRV_0 |
```

## Restore Original Names

```bash
cd sql-scripts-sanitize
codebleach restore
```

Your files are back to their original state!

## Next Steps

- Read [SQL_SANITIZATION.md](SQL_SANITIZATION.md) for full guide
- Customize [.codebleach-rules.json](.codebleach-rules-sql.json) for your schema
- See [CUSTOM_RULES.md](CUSTOM_RULES.md) for advanced patterns

