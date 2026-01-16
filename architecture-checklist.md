# CodeBleach Architecture: Schema-Aware Sanitization

## Problem Statement

Enterprise SQL code contains sensitive schema information that reveals:
1. **Internal table naming conventions** (e.g., `TA09063`, `TB00123`)
2. **Database names** (e.g., `DB2MEW`, `SRCDB`, `PRODDB`)
3. **Server names** (e.g., `PRODSRV01`, `STGSRV01`)
4. **Schema-qualified references** (e.g., `DB2BHI.dbo.TA09052`, `SRCDB.CLAIMS_DATA`)

**Column names are NOT sensitive** - they can remain as-is.

These patterns are **deterministic** and must be detected via **regex rules in code**, not LLM inference.

---

## Scope

### IN SCOPE (Must Sanitize)
- Table names (e.g., `TB00123`, `TA09063`)
- Database names (e.g., `SRCDB`, `PRODDB`, `DB2MEW`)
- Server names (e.g., `PRODSRV01`, `FILESRV01`)
- Schema-qualified names (e.g., `SRCDB.CLAIMS_DATA`, `DESTDB.dbo.TB00999`)

### OUT OF SCOPE (Leave As-Is)
- Column names (`EMP_ID`, `FIRST_NM`, `STATUS_CD`, etc.)
- SQL keywords
- String literals (already handled by existing rules)

---

## Detection Strategy

### Rule 1: Prefixed Table Names

Pattern: Tables with prefix + numbers (common enterprise convention)

| Pattern | Regex | Example | Alias |
|---------|-------|---------|-------|
| `TB#####` | `\bTB\d{4,6}\b` | `TB00123` | `TBL_0` |
| `TA#####` | `\bTA\d{4,6}\b` | `TA09063` | `TBL_1` |
| Generic | `\b[A-Z]{2}\d{4,6}\b` | `XX12345` | `TBL_N` |

### Rule 2: Server Names

Pattern: Server naming conventions (NAME + NUMBER suffix)

| Pattern | Regex | Example | Alias |
|---------|-------|---------|-------|
| `*SRV##` | `\b[A-Z]+SRV\d{1,3}\b` | `PRODSRV01` | `SRV_0` |
| `*SVR##` | `\b[A-Z]+SVR\d{1,3}\b` | `SQLSVR02` | `SRV_1` |
| `*SERVER##` | `\b[A-Z]+SERVER\d{1,3}\b` | `DBSERVER01` | `SRV_2` |

### Rule 3: Database Names

Pattern: Database naming conventions

| Pattern | Regex | Example | Alias |
|---------|-------|---------|-------|
| `*DB` suffix | `\b[A-Z][A-Z0-9]*DB\b` | `PRODDB`, `SRCDB` | `DB_0` |
| `DB*` prefix | `\bDB[A-Z0-9]+\b` | `DB2MEW`, `DB2BHI` | `DB_1` |
| `*_DB` suffix | `\b[A-Z][A-Z0-9_]*_DB\b` | `REPORTING_DB` | `DB_2` |

### Rule 4: Schema-Qualified Names (Must Apply FIRST)

Pattern: `DATABASE.TABLE` or `DATABASE.SCHEMA.TABLE`

| Pattern | Regex | Example | Alias |
|---------|-------|---------|-------|
| `DB.dbo.TABLE` | `\b[A-Z][A-Z0-9_]+\.dbo\.[A-Z][A-Z0-9_]+\b` | `DESTDB.dbo.TB00999` | `SCHEMA_0` |
| `DB.TABLE` | `\b[A-Z][A-Z0-9_]+\.[A-Z][A-Z0-9_]+\b` | `SRCDB.CLAIMS_DATA` | `SCHEMA_1` |

---

## Implementation Checklist

### New Rules (Add to `BuiltInRules.cs`)

- [ ] `SqlSchemaDbQualified` - `DATABASE.dbo.TABLE` (Order: 1)
- [ ] `SqlSchemaQualified` - `DATABASE.TABLE` (Order: 2)
- [ ] `SqlTablePrefixed` - `TB#####`, `TA#####` tables (Order: 10)
- [ ] `SqlServerNames` - `*SRV##`, `*SVR##` servers (Order: 20)
- [ ] `SqlDatabaseNames` - `*DB`, `DB*` databases (Order: 30)

### Rule Ordering (Critical)

```csharp
// Most specific first to prevent partial matches
public static IEnumerable<SanitizationRule> SqlSchemaRules => new[]
{
    SqlSchemaDbQualified,  // Order: 1  - DESTDB.dbo.TB00999
    SqlSchemaQualified,    // Order: 2  - SRCDB.CLAIMS_DATA
    SqlTablePrefixed,      // Order: 10 - TB00123
    SqlServerNames,        // Order: 20 - PRODSRV01
    SqlDatabaseNames,      // Order: 30 - PRODDB
}.OrderBy(r => r.Order);
```

---

## Example Transformation

### Before Sanitization:
```sql
SELECT EMP_ID, FIRST_NM, LAST_NM, STATUS_CD
FROM TB00123 e
JOIN TB00456 d ON e.EMP_ID = d.EMP_ID
WHERE e.ACTIVE_IND = 'Y';

-- Load from SRCDB.CLAIMS_DATA into DESTDB.dbo.TB00999
INSERT INTO DESTDB.dbo.TB00999 (CLAIM_ID, STATUS_CD)
SELECT CLAIM_ID, STATUS_CD FROM SRCDB.CLAIMS_DATA;

-- Server: PRODSRV01, Database: PRODDB
```

### After Sanitization:
```sql
SELECT EMP_ID, FIRST_NM, LAST_NM, STATUS_CD
FROM TBL_0 e
JOIN TBL_1 d ON e.EMP_ID = d.EMP_ID
WHERE e.ACTIVE_IND = 'Y';

-- Load from SCHEMA_0 into SCHEMA_1
INSERT INTO SCHEMA_1 (CLAIM_ID, STATUS_CD)
SELECT CLAIM_ID, STATUS_CD FROM SCHEMA_0;

-- Server: SRV_0, Database: DB_0
```

### Cross-Reference Generated:
```markdown
| Original | Alias | Type |
|----------|-------|------|
| TB00123 | TBL_0 | Table |
| TB00456 | TBL_1 | Table |
| SRCDB.CLAIMS_DATA | SCHEMA_0 | Schema.Table |
| DESTDB.dbo.TB00999 | SCHEMA_1 | Schema.Table |
| PRODSRV01 | SRV_0 | Server |
| PRODDB | DB_0 | Database |
```

**Note:** Column names (`EMP_ID`, `FIRST_NM`, `STATUS_CD`, etc.) remain unchanged.

---

## Non-Goals (Explicit)

- ❌ **No column name sanitization** - Column names are safe to expose
- ❌ **No LLM inference** - All detection is regex-based
- ❌ **No semantic analysis** - We don't parse SQL AST
- ❌ **No external API calls** - Everything runs locally

---

## Next Steps

1. **ENGINEER**: Implement 5 new rules in `BuiltInRules.cs`
2. **ENGINEER**: Write tests for each pattern
3. **QA**: Validate against `tests/fixtures/sql-schema-test/`

---

**Author**: Architect  
**Date**: 2026-01-16  
**Status**: PROPOSED - Awaiting Engineer Implementation

