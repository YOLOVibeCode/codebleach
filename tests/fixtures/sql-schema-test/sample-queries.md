# Sample SQL Queries

> Fictional test data for CodeBleach table/database/server sanitization.

## Query 1: Basic Table Join

```sql
SELECT EMP_ID, FIRST_NM, LAST_NM
FROM TB00123 e
JOIN TB00456 d ON e.EMP_ID = d.EMP_ID
JOIN TB00789 g ON e.DEPT_ID = g.DEPT_ID
JOIN TB00999 m ON e.EMP_ID = m.EMP_ID
WHERE e.ACTIVE_IND = 'Y';
```

## Query 2: Update with Table References

```sql
UPDATE TB00123
SET STATUS_CD = 'ACTIVE'
FROM TB00456
WHERE TB00123.EMP_ID = TB00456.EMP_ID
AND TB00456.ADDR_TYPE_CD = 'HOME';
```

## Query 3: Subquery

```sql
SELECT MAX(EFFECTIVE_DT)
FROM TB00456 innerTbl
WHERE TB00123.EMP_ID = innerTbl.EMP_ID;
```
