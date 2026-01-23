# Database Architecture

## Overview

The Enterprise Platform uses a multi-database architecture with cross-database queries via linked servers.

## Database Topology

```
┌─────────────────────┐     ┌─────────────────────┐
│  PRODSRV01          │     │  LINKEDSRV01        │
│  (Primary SQL)      │────▶│  (Remote SQL)       │
│  10.50.100.10       │     │  10.50.200.10       │
├─────────────────────┤     ├─────────────────────┤
│  DBZMEW             │     │  DBZBHI             │
│  HRDatabase         │     │  ReportingDB        │
│  FinanceDB          │     │                     │
└─────────────────────┘     └─────────────────────┘
         │
         ▼
┌─────────────────────┐
│  PRODSRV02          │
│  (DR Replica)       │
│  10.50.100.11       │
└─────────────────────┘
```

## Schema Details

### DBZMEW (Main Data Warehouse)

| Table | Description | Row Count |
|-------|-------------|-----------|
| TB00123 | Employee master data | ~50,000 |
| TB00456 | Department hierarchy | ~500 |
| TB00789 | Job classifications | ~1,000 |
| TB00999 | Salary history | ~500,000 |

### Cross-Database Queries

Joining data across servers:

```sql
-- Employee with BI data
SELECT 
    e.EMP_ID,
    e.FIRST_NM,
    e.LAST_NM,
    bi.PERFORMANCE_SCORE
FROM DBZMEW.dbo.TB00123 e
JOIN [LINKEDSRV01].[DBZBHI].[dbo].[BI_EMPLOYEE_METRICS] bi 
    ON e.EMP_ID = bi.EMP_ID
WHERE e.DEPT_ID IN (
    SELECT DEPT_ID 
    FROM DBZMEW.dbo.TB00456 
    WHERE ACTIVE_IND = 'Y'
);
```

### Complex Join Example

```sql
-- Multi-database financial report
INSERT INTO DBZBHI.dbo.FINANCIAL_SUMMARY (
    FISCAL_YEAR,
    DEPT_ID,
    TOTAL_SALARY,
    HEADCOUNT
)
SELECT 
    YEAR(s.EFFECTIVE_DT) AS FISCAL_YEAR,
    e.DEPT_ID,
    SUM(s.SALARY_AMT) AS TOTAL_SALARY,
    COUNT(DISTINCT e.EMP_ID) AS HEADCOUNT
FROM PRODSRV01.DBZMEW.dbo.TB00123 e
JOIN PRODSRV01.DBZMEW.dbo.TB00999 s ON e.EMP_ID = s.EMP_ID
JOIN [LINKEDSRV01].[HRDatabase].[dbo].[DEPT_BUDGET] b ON e.DEPT_ID = b.DEPT_ID
WHERE s.EFFECTIVE_DT >= '2024-01-01'
GROUP BY YEAR(s.EFFECTIVE_DT), e.DEPT_ID;
```

## Connection Information

| Database | Server | Port | Auth |
|----------|--------|------|------|
| DBZMEW | PRODSRV01.acme-corp.com | 1433 | Windows Auth |
| DBZBHI | LINKEDSRV01.acme-corp.com | 1433 | Windows Auth |
| HRDatabase | PRODSRV01.acme-corp.com | 1433 | Windows Auth |
| FinanceDB | PRODSRV01.acme-corp.com | 1433 | Windows Auth |

## Backup Locations

- Primary backups: `\\FILESRV01\SQLBackups\Production`
- DR backups: `\\FILESRV02\SQLBackups\DR`
- Archive: `\\FILESRV-BACKUP\Archive\SQL`

## Monitoring

Database health is monitored via:
- Grafana: https://grafana.internal.acme.com/d/sql-health
- Prometheus endpoint: http://PRODSRV01.acme-corp.com:9090/metrics
