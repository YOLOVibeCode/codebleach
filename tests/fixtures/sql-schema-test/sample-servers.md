# Server and Database Names

> Fictional test data for CodeBleach server/database sanitization.

## Server Names

| Environment | Server | Database |
|-------------|--------|----------|
| Production | PRODSRV01 | PRODDB |
| Production | PRODSRV02 | PRODDB |
| Staging | STGSRV01 | STGDB |
| Development | DEVSRV01 | DEVDB |
| File Server | FILESRV01 | N/A |
| SQL Server | SQLSVR01 | MAINDB |
| DB Server | DBSERVER01 | DATADB |

## Connection Strings

```
Server=PRODSRV01;Database=PRODDB;
Server=PRODSRV02;Database=PRODDB;
Server=STGSRV01;Database=STGDB;
Data Source=SQLSVR01;Initial Catalog=MAINDB;
```

## Database References

- Source: SRCDB
- Destination: DESTDB  
- Production: PRODDB
- Staging: STGDB
- Development: DEVDB
- Analytics: ANALYTICSDB
- Reporting: REPORTINGDB
- Data Warehouse: DB2MEW
- Business Intelligence: DB2BHI

