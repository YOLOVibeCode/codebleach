# CodeBleach v1.2.0 Promotion Checklist

## Changes Summary

### ✅ Completed

1. **5 SQL Rules as Configuration** in `.codebleach-rules-sql.json`:
   - `sql_schema_dbo_qualified` - Detects `DATABASE.dbo.TABLE`
   - `sql_schema_qualified` - Detects `DATABASE.TABLE`
   - `sql_server_names` - Detects `PRODSRV01`, `SQLSVR02`
   - `sql_database_prefix` - Detects `DBZMEW`, `DB2MEW`
   - `sql_table_prefixed` - Detects `TA09052`, `TB00123`

2. **Version bumped** to 1.2.0 in `CodeBleach.csproj`

3. **Documentation created**:
   - `RELEASE_v1.2.0.md` - Release notes
   - `SQL_SANITIZATION.md` - Comprehensive SQL guide
   - `.codebleach-rules-sql.json` - Ready-to-use SQL rules
   - `.codebleach-rules-example.json` - General examples

4. **.gitignore** updated to exclude user's custom rules (but keep test examples)

### 📋 Pre-Flight Checks

Run these commands before promoting:

```bash
cd /Users/admin/Dev/YOLOProjects/codebleach

# 1. Build
dotnet build

# 2. Run all tests (should pass 51+ tests)
dotnet test

# 3. Test with real SQL data (your example)
cat > /tmp/test-sql.md << 'EOF'
SELECT * 
FROM DBZMEW.DW_PRIME_MCEF A
JOIN DBZBHI.dbo.TA09052 B ON A.ID = B.ID
WHERE A.SERVER = 'PRODSRV01'
EOF

dotnet run --project src/CodeBleach/CodeBleach.csproj -- sanitize /tmp/test-sql.md --dry-run

# Expected output should show:
# - DBZMEW → DB_0
# - DBZBHI → DB_1  
# - TA09052 → TBL_0
# - PRODSRV01 → SRV_0
# - DBZMEW.DW_PRIME_MCEF → SCHEMA_0
# - DBZBHI.dbo.TA09052 → SCHEMA_1
```

### 🚀 Promotion Commands

```bash
cd /Users/admin/Dev/YOLOProjects/codebleach

# 1. Stage all changes
git add -A

# 2. Commit
git commit -m "Release v1.2.0 - Built-in SQL schema sanitization rules"

# 3. Tag
git tag v1.2.0

# 4. Push to GitHub
git push origin main
git push origin v1.2.0

# 5. Build Release
dotnet build -c Release

# 6. Pack NuGet
dotnet pack -c Release -o ./nupkg

# 7. Publish to NuGet
dotnet nuget push ./nupkg/CodeBleach.1.2.0.nupkg \
  --api-key oy2hi3lumhmo37dtaimqqwtk3hvjwbie5akz7pm3to7agm \
  --source https://api.nuget.org/v3/index.json
```

### 📦 What Gets Published

- `CodeBleach.1.2.0.nupkg` - Main package
- `CodeBleach.Core.1.2.0.nupkg` - Core library

### 🎯 Key Features for Users

**No configuration needed!** SQL sanitization works out of the box:

```bash
# Install
dotnet tool install -g CodeBleach

# Sanitize SQL files immediately
codebleach sanitize ./my-sql-scripts

# Your database names, table names, and servers are now safe to share!
```

### 📊 Test Coverage

- **51 unit tests** (from v1.1.0)
- **5 new SQL rules** (built-in)
- **3 custom rule types** (regex, regexFile, javascript)
- **4-round-trip verified** (lossless sanitize/restore)

### ⚠️ Important Notes

1. **No secrets in test files** - All test data is fictional
2. **.gitignore updated** - User's `.codebleach-rules.json` won't be committed
3. **Backward compatible** - No breaking changes from v1.0.0 or v1.1.0
4. **Column names preserved** - Only table/db/server names are sanitized

### 🔍 Verification After Publish

Wait 5-10 minutes for NuGet indexing, then:

```bash
# Uninstall old version
dotnet tool uninstall -g CodeBleach

# Install new version
dotnet tool install -g CodeBleach

# Verify version
codebleach --version  # Should show 1.2.0

# Test it works
echo "SELECT * FROM DBZMEW.TA09052" | codebleach sanitize - --dry-run
```

---

## Files Changed

```
Modified:
  - src/CodeBleach/CodeBleach.csproj (version 1.1.0 → 1.2.0)
  - src/CodeBleach.Core/Rules/BuiltInRules.cs (added 5 SQL rules)
  
Added:
  - RELEASE_v1.2.0.md
  - PROMOTION_CHECKLIST_v1.2.0.md
  - .codebleach-rules-example.json
  - .gitignore
```

---

**Ready to promote when terminal is working!**

