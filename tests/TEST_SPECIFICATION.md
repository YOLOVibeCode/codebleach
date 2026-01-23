# CodeBleach Enterprise Test Specification

**Version:** 1.0  
**Created:** January 22, 2026  
**Status:** Implementation Ready

---

## 1. Overview

This specification defines comprehensive test scenarios for CodeBleach targeting enterprise/corporate environments using GitFlow/GitLab workflows. Tests focus on:

- Deep nested directory structures (up to 10 levels)
- Markdown documentation with sensitive data
- SQL linked servers and four-part naming
- CI/CD configuration files
- Round-trip sanitization and restoration

---

## 2. Test Categories

### 2.1 Unit Tests (CodeBleach.Tests)

| Test Class | Purpose | Priority |
|------------|---------|----------|
| `EnterpriseMarkdownSanitizerTests` | Markdown table, code block, link sanitization | P0 |
| `SqlLinkedServerRulesTests` | Four-part SQL naming patterns | P0 |
| `CloudResourceRulesTests` | AWS ARN, S3, Azure patterns | P1 |
| `RoundTripTests` | Sanitize → Restore verification | P0 |
| `DeepNestingTests` | 10-level directory processing | P1 |
| `CrossReferenceTests` | Document cross-references | P1 |

### 2.2 Integration Tests (CodeBleach.IntegrationTests)

| Test Class | Purpose | Priority |
|------------|---------|----------|
| `EnterpriseFixtureTests` | Full enterprise fixture sanitization | P0 |
| `MarkdownRoundTripTests` | Markdown-specific round-trip | P0 |
| `CiCdConfigTests` | GitLab CI/GitHub Actions sanitization | P1 |
| `PerformanceTests` | Large structure performance | P2 |

---

## 3. Test Fixtures Required

### 3.1 Fixture: `enterprise-docs/` (Level 3-5)

```
tests/fixtures/enterprise-docs/
├── .codebleach-rules.json
├── README.md                          # Project overview with server refs
├── CONTRIBUTING.md                    # CI/CD URLs
├── docs/
│   ├── architecture/
│   │   ├── overview.md                # Server names, IPs, schemas
│   │   ├── database-design.md         # Full DB documentation
│   │   └── network-topology.md        # Network diagrams with IPs
│   ├── runbooks/
│   │   ├── deployment.md              # Deployment procedures
│   │   ├── database-failover.md       # Recovery procedures
│   │   └── troubleshooting.md         # Debug with server refs
│   └── api/
│       └── endpoints.md               # Internal API URLs
├── infrastructure/
│   └── terraform/
│       └── main.tf                    # Resource names
└── sql/
    └── migrations/
        └── V001__initial.sql          # Linked server refs
```

### 3.2 Fixture: `deep-nested/` (Level 10)

```
tests/fixtures/deep-nested/
├── .codebleach-rules.json
└── level1/
    └── level2/
        └── level3/
            └── level4/
                └── level5/
                    └── level6/
                        └── level7/
                            └── level8/
                                └── level9/
                                    └── level10/
                                        ├── config.json
                                        └── README.md
```

### 3.3 Fixture: `ci-cd-configs/` (Level 2-3)

```
tests/fixtures/ci-cd-configs/
├── .gitlab-ci.yml
├── .github/
│   └── workflows/
│       ├── deploy-prod.yml
│       └── deploy-staging.yml
├── Jenkinsfile
├── azure-pipelines.yml
└── docs/
    └── pipeline-guide.md
```

---

## 4. Detailed Test Specifications

### 4.1 Markdown Sanitization Tests

#### Test: `Sanitize_MarkdownTable_SanitizesAllCells`

**Input:**
```markdown
| Environment | Server | IP |
|-------------|--------|-----|
| Production | PRODSRV01 | 10.50.100.10 |
| Staging | STGSRV01 | 10.50.100.20 |
```

**Expected Output:**
```markdown
| Environment | Server | IP |
|-------------|--------|-----|
| Production | SERVER_0 | IP_0 |
| Staging | SERVER_1 | IP_1 |
```

**Assertions:**
- Table structure preserved
- All server names replaced
- All IPs replaced
- Consistent alias assignment

#### Test: `Sanitize_MarkdownCodeBlock_SanitizesContent`

**Input:**
```markdown
Connect using:
```sql
Server=PRODSRV01;Database=DBZMEW;
```
```

**Expected Output:**
```markdown
Connect using:
```sql
CONNSTR_0
```
```

#### Test: `Sanitize_MarkdownInlineCode_SanitizesContent`

**Input:**
```markdown
The server `PRODSRV01` hosts the database `DBZMEW`.
```

**Expected Output:**
```markdown
The server `SERVER_0` hosts the database `DB_0`.
```

#### Test: `Sanitize_MarkdownLink_SanitizesUrl`

**Input:**
```markdown
See [documentation](https://wiki.internal.acme.com/database)
```

**Expected Output:**
```markdown
See [documentation](HOST_0)
```

### 4.2 SQL Linked Server Tests

#### Test: `Sanitize_FourPartName_Bracketed`

**Input:**
```sql
SELECT * FROM [LINKEDSRV01].[DBZMEW].[dbo].[Employees]
```

**Expected Output:**
```sql
SELECT * FROM LINKED_0
```

#### Test: `Sanitize_FourPartName_Unbracketed`

**Input:**
```sql
SELECT * FROM LINKEDSRV01.DBZMEW.dbo.Employees
```

**Expected Output:**
```sql
SELECT * FROM LINKED_0
```

#### Test: `Sanitize_MixedSchemaPatterns`

**Input:**
```sql
-- Cross-database query
SELECT a.ID, b.NAME
FROM SRCDB.dbo.TB00123 a
JOIN DESTDB.dbo.TB00456 b ON a.ID = b.ID
JOIN [LINKEDSRV01].[REMOTEDB].[dbo].[TB00789] c ON a.ID = c.ID
WHERE a.STATUS IN (SELECT STATUS FROM PRODDB.Staging.StatusCodes)
```

**Expected Output:**
```sql
-- Cross-database query
SELECT a.ID, b.NAME
FROM SCHEMA_0 a
JOIN SCHEMA_1 b ON a.ID = b.ID
JOIN LINKED_0 c ON a.ID = c.ID
WHERE a.STATUS IN (SELECT STATUS FROM SCHEMA_2)
```

### 4.3 Round-Trip Tests

#### Test: `RoundTrip_MarkdownDocument_PerfectRestoration`

**Steps:**
1. Load original markdown with server names, IPs, tables
2. Sanitize → verify all sensitive data replaced
3. Restore → verify exact match with original

**Assertions:**
- `originalContent == restoredContent`
- `sanitizedContent != originalContent`
- All mappings bidirectional

#### Test: `RoundTrip_DeepNestedStructure_AllFilesRestored`

**Steps:**
1. Create 10-level deep structure with config files
2. Sanitize entire structure
3. Restore entire structure
4. Compare file-by-file

**Assertions:**
- All files at all levels restored
- Directory structure preserved
- File hashes match original

#### Test: `RoundTrip_AfterAiModification_PreservesChanges`

**Steps:**
1. Sanitize content
2. Simulate AI edit (add new code that doesn't contain aliases)
3. Restore
4. Verify AI changes preserved, sensitive data restored

### 4.4 Cloud Resource Tests

#### Test: `Sanitize_AwsArn_ReplacesArn`

**Input:**
```
arn:aws:rds:us-east-1:123456789012:db:production-db
```

**Expected Output:**
```
ARN_0
```

#### Test: `Sanitize_S3Bucket_ReplacesBucket`

**Input:**
```
s3://acme-production-data-lake/analytics/
```

**Expected Output:**
```
S3_0
```

#### Test: `Sanitize_AzureResourceGroup_ReplacesRg`

**Input:**
```
/subscriptions/12345678-1234-1234-1234-123456789012/resourceGroups/rg-acme-prod-eastus
```

**Expected Output:**
```
AZRG_0
```

### 4.5 CI/CD Configuration Tests

#### Test: `Sanitize_GitLabCi_SanitizesVariables`

**Input:**
```yaml
deploy_production:
  script:
    - kubectl config use-context production-cluster
    - helm upgrade --install app ./chart \
        --set database.host=PRODSRV01.acme-corp.com \
        --set database.name=DBZMEW
  environment:
    name: production
    url: https://api.acme-prod.com
```

**Expected Output:**
```yaml
deploy_production:
  script:
    - kubectl config use-context production-cluster
    - helm upgrade --install app ./chart \
        --set database.host=HOST_0 \
        --set database.name=DB_0
  environment:
    name: production
    url: HOST_1
```

### 4.6 Enterprise Rule Tests

#### Test: `Rule_EnterpriseServerFqdn_MatchesPatterns`

| Input | Should Match |
|-------|--------------|
| `PRODSRV01.acme-corp.com` | Yes |
| `DB-EAST-01.internal.net` | Yes |
| `localhost` | No |
| `google.com` | No |

#### Test: `Rule_AdServiceAccount_MatchesPatterns`

| Input | Should Match |
|-------|--------------|
| `ACME\svc_deployment` | Yes |
| `CORP\svc_sql_agent` | Yes |
| `john.doe` | No |
| `admin` | No |

---

## 5. Test Data Requirements

### 5.1 Server Names Pool

```
PRODSRV01, PRODSRV02, PRODSRV-EAST-01, PRODSRV-WEST-01
STGSRV01, STGSRV02
DEVSRV01, DEVSRV02
SQLSVR01, SQLSVR02
DBSERVER01, DBSERVER-EAST
FILESRV01, FILESRV-BACKUP
LINKEDSRV01, LINKEDSRV02
```

### 5.2 Database Names Pool

```
DBZMEW, DBZBHI, DB2MEW, DB2BHI
PRODDB, STGDB, DEVDB
HRDatabase, FinanceDB, SalesDB
UserAccounts, OrderHistory
```

### 5.3 IP Address Pool

```
10.50.100.10, 10.50.100.20, 10.50.100.30
192.168.1.100, 192.168.1.101, 192.168.1.102
172.16.0.1, 172.16.0.2
```

### 5.4 Internal Hostname Pool

```
api-prod.internal.acme.com
db-primary.corp.acme.net
jenkins.internal.acme.com
gitlab.corp.acme.net
confluence.internal.acme.com
vault.secure.acme.net
```

---

## 6. Acceptance Criteria

### 6.1 All Tests Must:

- [ ] Follow Arrange-Act-Assert pattern
- [ ] Use FluentAssertions
- [ ] Have descriptive names: `Method_Scenario_ExpectedBehavior`
- [ ] Include both positive and negative cases
- [ ] Be deterministic (no random data in assertions)

### 6.2 Coverage Targets:

| Metric | Minimum | Target |
|--------|---------|--------|
| Line Coverage | 80% | 90% |
| Branch Coverage | 75% | 85% |
| Mutation Score | 70% | 80% |

### 6.3 Performance Targets:

| Scenario | Maximum Time |
|----------|--------------|
| Single file sanitize | < 100ms |
| 100 files sanitize | < 2s |
| 1000 files sanitize | < 10s |
| 10-level deep structure | < 5s |

---

## 7. Implementation Order

### Phase 1 (Immediate)
1. Create `enterprise-docs/` fixture
2. Implement `EnterpriseMarkdownSanitizerTests`
3. Implement `SqlLinkedServerRulesTests`
4. Implement basic `RoundTripTests`

### Phase 2 (Short-term)
1. Create `deep-nested/` fixture
2. Create `ci-cd-configs/` fixture
3. Implement `CloudResourceRulesTests`
4. Implement `DeepNestingTests`

### Phase 3 (Medium-term)
1. Implement `EnterpriseFixtureTests` (integration)
2. Implement `PerformanceTests`
3. Add mutation testing

---

## 8. Test Execution Commands

```bash
# Run all unit tests
dotnet test tests/CodeBleach.Tests

# Run all integration tests
dotnet test tests/CodeBleach.IntegrationTests

# Run with coverage
dotnet test --collect:"XPlat Code Coverage"

# Run specific test category
dotnet test --filter "Category=Markdown"
dotnet test --filter "Category=SQL"
dotnet test --filter "Category=RoundTrip"

# Run with verbose output
dotnet test --logger "console;verbosity=detailed"
```

---

**Document Control:**
- **Author:** CodeBleach QA Team
- **Reviewers:** Engineering
- **Status:** Ready for Implementation
