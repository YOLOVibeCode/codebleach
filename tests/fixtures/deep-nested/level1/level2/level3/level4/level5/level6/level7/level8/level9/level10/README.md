# Deep Nested Component

This component is at the deepest level of the directory structure.

## Configuration

- **Database Server**: PRODSRV01.acme-corp.com
- **Database Name**: DBZMEW
- **Cache Server**: 10.50.100.50
- **Linked Server**: LINKEDSRV01

## SQL Access

```sql
SELECT * FROM [LINKEDSRV01].[DBZBHI].[dbo].[METRICS]
WHERE SOURCE_DB = 'DBZMEW';
```

## Service Account

Use `ACME\svc_deepnested` for authentication.
