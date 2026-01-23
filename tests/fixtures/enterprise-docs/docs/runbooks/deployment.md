# Deployment Runbook

## Prerequisites

1. VPN connection to `vpn.acme-corp.com`
2. Access to `ACME\svc_deployment` service account
3. Permissions on `PRODSRV01` and `APPSRV01`

## Pre-Deployment Checklist

- [ ] Verify staging tests passed on `STGSRV01`
- [ ] Confirm backup completed on `\\FILESRV01\SQLBackups\Production`
- [ ] Notify team in #deployments Slack channel
- [ ] Check Grafana dashboards: https://grafana.internal.acme.com/d/deployments

## Deployment Steps

### Step 1: Database Migration

Connect to the production database:

```sql
-- Connect to PRODSRV01.acme-corp.com
USE DBZMEW;
GO

-- Run migration scripts
:r \\FILESRV01\Deployments\migrations\V042__add_indexes.sql
```

Verify the migration:

```sql
SELECT TOP 10 * FROM DBZMEW.dbo.TB00123 WHERE MODIFIED_DT > GETDATE() - 1;
```

### Step 2: Application Deployment

SSH to the application server:

```bash
ssh ACME\\svc_deployment@APPSRV01.acme-corp.com
```

Deploy the application:

```bash
cd /opt/enterprise-app
./deploy.sh --environment production --version 2.5.0
```

### Step 3: Verify Deployment

Check the health endpoint:

```bash
curl https://api-prod.internal.acme.com/health
```

Expected response:
```json
{
  "status": "healthy",
  "database": "PRODSRV01",
  "version": "2.5.0"
}
```

### Step 4: Update Linked Server Data

If the deployment includes BI changes:

```sql
-- Refresh data from linked server
INSERT INTO DBZBHI.dbo.SYNC_LOG (SYNC_DT, SOURCE_DB)
SELECT GETDATE(), 'DBZMEW';

EXEC [LINKEDSRV01].[DBZBHI].[dbo].[sp_RefreshMetrics];
```

## Rollback Procedure

If issues are detected:

1. Restore from backup:
   ```sql
   RESTORE DATABASE DBZMEW 
   FROM DISK = '\\FILESRV01\SQLBackups\Production\DBZMEW_pre_deploy.bak'
   WITH REPLACE;
   ```

2. Rollback application:
   ```bash
   ssh ACME\\svc_deployment@APPSRV01.acme-corp.com
   ./rollback.sh --to-version 2.4.9
   ```

## Post-Deployment

- [ ] Monitor Grafana for 30 minutes
- [ ] Verify linked server queries work
- [ ] Check error rates at https://grafana.internal.acme.com/d/errors
- [ ] Update deployment log in Confluence

## Emergency Contacts

| Role | Name | Contact |
|------|------|---------|
| DBA On-Call | Rotate | dba-oncall@internal.acme.com |
| Platform Lead | John Smith | jsmith@internal.acme.com |
| Security | Security Team | security@internal.acme.com |
