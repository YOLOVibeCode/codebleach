# Enterprise Application Platform

> Internal documentation for the ACME Corporation Enterprise Platform

## Quick Links

- **Production API**: https://api-prod.internal.acme.com/v1
- **Staging API**: https://api-staging.corp.acme.net/v1
- **Documentation**: https://confluence.internal.acme.com/display/PLATFORM

## Infrastructure Overview

| Environment | Database Server | Application Server | Redis |
|-------------|-----------------|-------------------|-------|
| Production | PRODSRV01.acme-corp.com | APPSRV01.acme-corp.com | 10.50.100.10 |
| Production DR | PRODSRV02.acme-corp.com | APPSRV02.acme-corp.com | 10.50.100.11 |
| Staging | STGSRV01.internal.net | STGAPP01.internal.net | 192.168.1.100 |
| Development | DEVSRV01.corp.acme.net | DEVAPP01.corp.acme.net | 192.168.1.200 |

## Database Information

Primary databases:
- `DBZMEW` - Main data warehouse
- `DBZBHI` - Business intelligence
- `HRDatabase` - Human resources
- `FinanceDB` - Financial records

## Connection Strings

Production connection (read-only access):
```
Server=PRODSRV01;Database=DBZMEW;Integrated Security=true
```

For linked server queries:
```sql
SELECT * FROM [LINKEDSRV01].[DBZMEW].[dbo].[Employees]
```

## Service Accounts

| Service | Account | Purpose |
|---------|---------|---------|
| SQL Agent | ACME\svc_sql_agent | Database maintenance |
| Deployment | ACME\svc_deployment | CI/CD deployments |
| Monitoring | ACME\svc_monitoring | Health checks |

## Secrets Management

All secrets are stored in HashiCorp Vault:
- Production DB: `secret/data/production/database`
- API Keys: `secret/data/production/api-keys`
- Certificates: `secret/data/production/certs`

## Contact

For access requests, contact the Platform Team via `platform-team@internal.acme.com`.
