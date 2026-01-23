# Network Topology

## VLAN Configuration

| VLAN | Subnet | Purpose |
|------|--------|---------|
| 100 | 10.50.100.0/24 | Production Database |
| 101 | 10.50.101.0/24 | Production Application |
| 200 | 10.50.200.0/24 | Production BI/Reporting |
| 300 | 192.168.1.0/24 | Staging |
| 400 | 192.168.2.0/24 | Development |
| 500 | 172.16.0.0/24 | Management |

## Server IP Assignments

### Production (VLAN 100-101)

| Server | IP Address | Role |
|--------|------------|------|
| PRODSRV01 | 10.50.100.10 | Primary SQL Server |
| PRODSRV02 | 10.50.100.11 | DR SQL Server |
| APPSRV01 | 10.50.101.10 | Primary App Server |
| APPSRV02 | 10.50.101.11 | Secondary App Server |
| LINKEDSRV01 | 10.50.200.10 | BI SQL Server |

### Staging (VLAN 300)

| Server | IP Address | Role |
|--------|------------|------|
| STGSRV01 | 192.168.1.100 | Staging SQL |
| STGAPP01 | 192.168.1.101 | Staging App |
| STGREDIS | 192.168.1.102 | Staging Redis |

### Development (VLAN 400)

| Server | IP Address | Role |
|--------|------------|------|
| DEVSRV01 | 192.168.2.100 | Dev SQL |
| DEVAPP01 | 192.168.2.101 | Dev App |

## Load Balancer Configuration

```
                    ┌──────────────────┐
                    │   External LB    │
                    │   (F5 BigIP)     │
                    │  api.acme.com    │
                    └────────┬─────────┘
                             │
              ┌──────────────┼──────────────┐
              ▼              ▼              ▼
    ┌─────────────┐ ┌─────────────┐ ┌─────────────┐
    │  APPSRV01   │ │  APPSRV02   │ │  APPSRV03   │
    │ 10.50.101.10│ │ 10.50.101.11│ │ 10.50.101.12│
    └─────────────┘ └─────────────┘ └─────────────┘
```

## Firewall Rules

| Source | Destination | Port | Protocol |
|--------|-------------|------|----------|
| 10.50.101.0/24 | 10.50.100.10 | 1433 | TCP (SQL) |
| 10.50.101.0/24 | 10.50.100.11 | 1433 | TCP (SQL) |
| 10.50.100.10 | 10.50.200.10 | 1433 | TCP (Linked Server) |
| 0.0.0.0/0 | 10.50.101.0/24 | 443 | TCP (HTTPS) |

## DNS Records

| Hostname | Type | Value |
|----------|------|-------|
| api-prod.internal.acme.com | A | 10.50.101.10 |
| db-primary.internal.acme.com | CNAME | PRODSRV01.acme-corp.com |
| db-dr.internal.acme.com | CNAME | PRODSRV02.acme-corp.com |
| bi-server.internal.acme.com | CNAME | LINKEDSRV01.acme-corp.com |

## VPN Access

Connect to corporate VPN at `vpn.acme-corp.com` using your AD credentials.

After connecting, you can access:
- SQL Management Studio: Connect to `PRODSRV01.acme-corp.com`
- Grafana: https://grafana.internal.acme.com
- Jenkins: https://jenkins.internal.acme.com
