# SQL Schema Sanitization Guide

CodeBleach includes pre-configured rules for sanitizing SQL database schemas, perfect for sharing SQL queries, migration scripts, and database documentation with AI assistants or external teams.

## Quick Start

### Option 1: Use the Pre-Configured SQL Rules

Copy the SQL rules file to your project:

```bash
cd your-project
curl -o .codebleach-rules.json https://raw.githubusercontent.com/YOLOVibeCode/codebleach/main/.codebleach-rules-sql.json
```

Or manually copy `.codebleach-rules-sql.json` to `.codebleach-rules.json` in your project root.

### Option 2: Customize for Your Schema

Edit `.codebleach-rules.json` to match your naming conventions:

```json
{
  "rules": [
    {
      "ruleId": "my_schema_tables",
      "name": "My Schema Tables",
      "type": "regex",
      "pattern": "\\b(SCHEMA1|SCHEMA2)\\.\\w+\\b",
      "prefix": "SCHEMA",
      "order": 1
    }
  ]
}
```

## What Gets Sanitized

### ✅ Sanitized (Hidden)

| Pattern | Example | Sanitized To |
|---------|---------|--------------|
| `DATABASE.dbo.TABLE` | `DBZBHI.dbo.TA09052` | `SCHEMA_0` |
| `DATABASE.TABLE` | `DBZMEW.DW_PRIME_MCEF` | `SCHEMA_1` |
| Server names | `PRODSRV01`, `SQLSVR02` | `SRV_0`, `SRV_1` |
| DB prefixes | `DBZMEW`, `DB2MEW` | `DB_0`, `DB_1` |
| Table names | `TA09052`, `TB00123` | `TBL_0`, `TBL_1` |

### ❌ NOT Sanitized (Visible)

| Type | Example | Why |
|------|---------|-----|
| Column names | `EMP_ID`, `CUSTOMER_NAME` | Business logic visibility |
| SQL keywords | `SELECT`, `FROM`, `WHERE` | Standard SQL |
| Functions | `MAX()`, `COUNT()`, `SUM()` | Standard SQL |
| String literals | `'ACTIVE'`, `'2024-01-01'` | Data values |

## Example

### Before Sanitization

```sql
-- Pull pharmacy data from DBZMEW to DBZBHI
SELECT 
    A.RXCLAIMNBRID AS CLM_CNTL_ID,
    A.MEMBERID AS MEM_ID,
    (
        SELECT MAX(C.GRP_ID) 
        FROM DBZMEW.MEMBERSHIP_OUT C 
        WHERE C.TRANS_TYP_CD = 'PLN'
    ) AS MSTR_PLCY_ID
FROM DBZMEW.DW_PRIME_MCEF A
JOIN DBZMEW.MEMBERSHIP_OUT C ON C.MEM_ID = A.MEMBERID
WHERE A.STATUS = 'ACTIVE'
```

### After Sanitization

```sql
-- Pull pharmacy data from DB_0 to DB_1
SELECT 
    A.RXCLAIMNBRID AS CLM_CNTL_ID,
    A.MEMBERID AS MEM_ID,
    (
        SELECT MAX(C.GRP_ID) 
        FROM SCHEMA_1 C 
        WHERE C.TRANS_TYP_CD = 'PLN'
    ) AS MSTR_PLCY_ID
FROM SCHEMA_0 A
JOIN SCHEMA_1 C ON C.MEM_ID = A.MEMBERID
WHERE A.STATUS = 'ACTIVE'
```

### Cross-Reference (Stored in `.codebleach/xref.md`)

```markdown
| Original | Alias | Type |
|----------|-------|------|
| DBZMEW | DB_0 | Database |
| DBZBHI | DB_1 | Database |
| DBZMEW.DW_PRIME_MCEF | SCHEMA_0 | Schema.Table |
| DBZMEW.MEMBERSHIP_OUT | SCHEMA_1 | Schema.Table |
```

## Usage

### Sanitize SQL Files

```bash
# Sanitize a single file
codebleach sanitize migration.sql

# Sanitize a directory of SQL scripts
codebleach sanitize ./sql-migrations

# Preview changes first
codebleach sanitize ./sql-migrations --dry-run

# Sanitize with verbose output
codebleach sanitize ./sql-migrations --verbose
```

### Restore Original Names

```bash
# Restore from within the sanitized directory
cd sql-migrations-sanitize
codebleach restore
```

## Common SQL Patterns

### Pattern 1: Schema-Qualified Names

**Pattern:** `\\b[A-Z][A-Z0-9_]+\\.[A-Z][A-Z0-9_]+\\b`

Matches:
- `DBZMEW.MEMBERSHIP_OUT`
- `SCHEMA1.TABLE_NAME`
- `DB2.VIEW_SALES`

### Pattern 2: Three-Part Names

**Pattern:** `\\b[A-Z][A-Z0-9_]+\\.dbo\\.[A-Z][A-Z0-9_]+\\b`

Matches:
- `DBZBHI.dbo.TA09052`
- `MASTER.dbo.SYSCOLUMNS`

### Pattern 3: Prefixed Tables

**Pattern:** `\\b[A-Z]{2}\\d{4,6}\\b`

Matches:
- `TA09052`
- `TB00123`
- `XX123456`

### Pattern 4: Server Names

**Pattern:** `\\b[A-Z]+S(RV|VR|ERVER)\\d{1,3}\\b`

Matches:
- `PRODSRV01`
- `SQLSVR02`
- `DBSERVER123`

## Custom Patterns

### Example 1: Specific Schema Names

```json
{
  "ruleId": "our_schemas",
  "name": "Our Schemas",
  "type": "regex",
  "pattern": "\\b(HR_SCHEMA|FINANCE_SCHEMA|SALES_SCHEMA)\\b",
  "prefix": "SCHEMA"
}
```

### Example 2: Stored Procedures

```json
{
  "ruleId": "stored_procs",
  "name": "Stored Procedures",
  "type": "regex",
  "pattern": "\\bsp_[A-Za-z0-9_]+\\b",
  "prefix": "PROC"
}
```

### Example 3: Using External File

**`.codebleach-rules.json`:**
```json
{
  "rules": [
    {
      "ruleId": "our_tables",
      "name": "Our Table Names",
      "type": "regexFile",
      "patternFile": "rules/tables.regex",
      "prefix": "TBL"
    }
  ]
}
```

**`rules/tables.regex`:**
```regex
# Our table naming patterns
\bCUST_\w+\b
\bORDER_\w+\b
\bPROD_\w+\b
```

## Best Practices

### 1. Test with Dry Run First

```bash
codebleach sanitize ./sql --dry-run
```

### 2. Check the Cross-Reference

```bash
codebleach sanitize ./sql --output ./sql-safe
cat ./sql-safe/.codebleach/xref.md
```

### 3. Order Matters

Rules are applied in order. More specific patterns should have lower order numbers:

```json
{
  "rules": [
    {"ruleId": "three_part", "pattern": "DB\\.dbo\\.TABLE", "order": 1},
    {"ruleId": "two_part", "pattern": "DB\\.TABLE", "order": 2},
    {"ruleId": "table_only", "pattern": "TABLE", "order": 3}
  ]
}
```

### 4. Verify Round-Trip

```bash
# Sanitize
codebleach sanitize ./original --output ./sanitized

# Restore
cd ./sanitized
codebleach restore

# Verify
diff -r ../original ./
```

## Troubleshooting

### Issue: Too Many False Positives

**Solution:** Add exceptions to your rule:

```json
{
  "ruleId": "my_rule",
  "pattern": "\\bDB\\w+\\b",
  "prefix": "DB",
  "exceptions": ["DATABASE", "DBNULL", "DBMS"]
}
```

### Issue: Pattern Not Matching

**Solution:** Test your regex pattern separately:

```bash
echo "SELECT * FROM DBZMEW.TABLE1" | grep -oE '\b[A-Z]+\.\w+\b'
```

### Issue: Overlapping Matches

CodeBleach automatically handles overlapping matches by preferring longer matches first.

```
DBZMEW.DW_PRIME_MCEF  →  SCHEMA_0  ✅ (longer match wins)
DBZMEW                →  (skipped, already part of SCHEMA_0)
```

## Resources

- [Main README](README.md)
- [Custom Rules Guide](CUSTOM_RULES.md)
- [Example Rules](.codebleach-rules-example.json)
- [SQL Rules Template](.codebleach-rules-sql.json)

