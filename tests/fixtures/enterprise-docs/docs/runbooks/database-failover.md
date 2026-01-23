# Database Failover Runbook

## Overview

This runbook covers failover procedures from `PRODSRV01` (primary) to `PRODSRV02` (DR).

## Failover Scenarios

### Scenario 1: Planned Maintenance

For planned maintenance windows:

1. **Notify stakeholders** via platform-team@internal.acme.com
2. **Drain connections** from `PRODSRV01.acme-corp.com`
3. **Initiate failover** to `PRODSRV02.acme-corp.com`

### Scenario 2: Unplanned Outage

If `PRODSRV01` (10.50.100.10) becomes unavailable:

1. **Verify outage** - ping 10.50.100.10, check Grafana
2. **Check DR server** - verify PRODSRV02 (10.50.100.11) is healthy
3. **Execute failover** (see steps below)

## Failover Steps

### Step 1: Verify DR Server Health

```bash
# Check SQL Server on DR
sqlcmd -S PRODSRV02.acme-corp.com -E -Q "SELECT @@SERVERNAME, @@VERSION"

# Verify databases are synchronized
sqlcmd -S PRODSRV02.acme-corp.com -E -Q "
    SELECT database_name, synchronization_state_desc 
    FROM sys.dm_hadr_database_replica_states
"
```

### Step 2: Failover Always On AG

```sql
-- Connect to PRODSRV02.acme-corp.com
USE master;
GO

-- Failover the availability group
ALTER AVAILABILITY GROUP [AG_Enterprise] FAILOVER;
GO
```

### Step 3: Update DNS

Update the CNAME record for `db-primary.internal.acme.com`:

| Before | After |
|--------|-------|
| PRODSRV01.acme-corp.com | PRODSRV02.acme-corp.com |

### Step 4: Update Application Configuration

On `APPSRV01` and `APPSRV02`:

```bash
# Update connection string to point to DR
sed -i 's/PRODSRV01/PRODSRV02/g' /opt/enterprise-app/config/production.json

# Restart application
systemctl restart enterprise-app
```

### Step 5: Update Linked Server

If linked server queries are failing:

```sql
-- On LINKEDSRV01, update linked server definition
EXEC sp_dropserver 'PRODSRV01', 'droplogins';
GO

EXEC sp_addlinkedserver 
    @server = 'PRODSRV_PRIMARY',
    @srvproduct = '',
    @provider = 'SQLNCLI',
    @datasrc = 'PRODSRV02.acme-corp.com';
GO
```

### Step 6: Verify Failover

```sql
-- Verify we're on PRODSRV02
SELECT @@SERVERNAME;  -- Should return PRODSRV02

-- Verify databases are accessible
SELECT COUNT(*) FROM DBZMEW.dbo.TB00123;
SELECT COUNT(*) FROM DBZBHI.dbo.BI_EMPLOYEE_METRICS;

-- Test linked server
SELECT TOP 1 * FROM [LINKEDSRV01].[DBZBHI].[dbo].[BI_EMPLOYEE_METRICS];
```

## Post-Failover Tasks

- [ ] Update monitoring to point to PRODSRV02
- [ ] Verify backups are running on new primary
- [ ] Test all critical queries
- [ ] Update runbooks if needed
- [ ] Schedule failback window

## Failback Procedure

When `PRODSRV01` is repaired:

1. Rebuild as secondary replica
2. Wait for synchronization
3. Schedule maintenance window
4. Repeat failover steps (in reverse)

## Monitoring URLs

- Primary Health: https://grafana.internal.acme.com/d/sql-primary
- DR Health: https://grafana.internal.acme.com/d/sql-dr
- Replication Lag: https://grafana.internal.acme.com/d/replication

## Emergency Contacts

| Role | Primary Server | DR Server |
|------|---------------|-----------|
| DBA | PRODSRV01 @ 10.50.100.10 | PRODSRV02 @ 10.50.100.11 |
| Network | Contact NOC | Contact NOC |
| Management | CTO direct line | CTO direct line |
