# CodeBleach v1.2.0 Release Notes

**Release Date:** 2026-01-16

## New Features

### Pre-Configured SQL Schema Rules

Added **ready-to-use** SQL sanitization rules as a configuration file (`.codebleach-rules-sql.json`):

| Rule | Pattern | Example | Sanitized |
|------|---------|---------|-----------|
| **Schema.dbo.Table** | `DATABASE.dbo.TABLE` | `DBZBHI.dbo.TA09052` | `SCHEMA_0` |
| **Schema.Table** | `DATABASE.TABLE` | `DBZMEW.DW_PRIME_MCEF` | `SCHEMA_1` |
| **SQL Servers** | `*SRV##`, `*SVR##`, `*SERVER##` | `PRODSRV01` | `SRV_0` |
| **DB Prefix** | `DB*` databases | `DBZMEW`, `DB2MEW` | `DB_0` |
| **Table Prefix** | `XX#####` tables | `TA09052`, `TB00123` | `TBL_0` |

### Quick Start for SQL Users

```bash
# Copy the SQL rules to your project
curl -o .codebleach-rules.json https://raw.githubusercontent.com/YOLOVibeCode/codebleach/main/.codebleach-rules-sql.json

# Or manually copy the file from the repo
cp .codebleach-rules-sql.json .codebleach-rules.json

# Sanitize your SQL files
codebleach sanitize ./sql-scripts
```

See [SQL_SANITIZATION.md](SQL_SANITIZATION.md) for full guide.

### Use Cases

Perfect for sanitizing:
- SQL migration scripts
- Database schema documentation
- ETL/data warehouse queries
- Stored procedures
- Table mapping documents

### Example

**Before:**
```sql
SELECT * 
FROM DBZMEW.DW_PRIME_MCEF A
JOIN DBZMEW.MEMBERSHIP_OUT C ON C.MEM_ID = A.MEMBERID
WHERE A.STATUS = 'ACTIVE'
```

**After:**
```sql
SELECT * 
FROM SCHEMA_0 A
JOIN SCHEMA_1 C ON C.MEM_ID = A.MEMBERID
WHERE A.STATUS = 'ACTIVE'
```

## What's Included

### From v1.1.0
- Three custom rule types (regex, regexFile, javascript)
- Jint JavaScript engine for advanced rules
- Lossless 4-round-trip guarantee
- Overlapping match protection

### New in v1.2.0
- **SQL rules template** (`.codebleach-rules-sql.json`)
- **General examples** (`.codebleach-rules-example.json`)
- **Comprehensive SQL guide** (`SQL_SANITIZATION.md`)
- **.gitignore** to protect custom rules from being committed

---

## Built-In Rules: 11

1. Server/Database Names
2. Production Databases
3. Production Tables
4. User Tables
5. Private IP (10.x)
6. Private IP (172.x)
7. Private IP (192.168.x)
8. Connection Strings
9. Windows Paths
10. UNC Paths
11. Internal Hostnames

**Plus:** 5 optional SQL rules available in `.codebleach-rules-sql.json`

---

## Upgrade Instructions

```bash
dotnet tool update -g CodeBleach
```

## Breaking Changes

None - fully backward compatible with v1.0.0 and v1.1.0

