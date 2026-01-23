#!/bin/bash
set -e

echo "=========================================="
echo "CodeBleach v1.3.0 - Global Config E2E Test"
echo "=========================================="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test counter
TESTS_PASSED=0
TESTS_FAILED=0

# Function to print test results
test_result() {
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✓ PASS${NC}: $2"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    else
        echo -e "${RED}✗ FAIL${NC}: $2"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    fi
}

# Cleanup function
cleanup() {
    echo ""
    echo "Cleaning up test artifacts..."
    rm -rf ~/codebleach-test-project
    rm -rf ~/codebleach-test-project-sanitize
    rm -f ~/.config/codebleach/rules.json 2>/dev/null || true
    echo "Cleanup complete."
}

# Set trap to cleanup on exit
trap cleanup EXIT

# Step 1: Build and install the tool
echo "Step 1: Building CodeBleach..."
cd /Users/admin/Dev/YOLOProjects/codebleach
dotnet build --configuration Release > /dev/null 2>&1
test_result $? "Build succeeded"

echo ""
echo "Step 2: Creating test project with SQL content..."
mkdir -p ~/codebleach-test-project/src

# Create test SQL file
cat > ~/codebleach-test-project/src/queries.sql << 'EOF'
-- Sample SQL queries with sensitive data
SELECT * FROM ProductionDB.dbo.CustomerOrders
WHERE ServerName = 'SQLPROD01'
AND DatabaseName = 'FinanceDB';

-- Linked server query
SELECT *
FROM [SQLPROD01].[FinanceDB].dbo.Transactions t
JOIN [SQLPROD02].[ReportingDB].dbo.Reports r
ON t.ReportID = r.ID;

-- Schema-qualified references
INSERT INTO staging.temp_customers
SELECT * FROM dbo.customers
WHERE status = 'active';

EOF

test_result $? "Created test SQL file"

echo ""
echo "Step 3: Creating global SQL rules config..."
mkdir -p ~/.config/codebleach
cat > ~/.config/codebleach/rules.json << 'EOF'
{
  "rules": [
    {
      "ruleId": "sql_database_names",
      "name": "SQL Database Names",
      "description": "Detects database names in FROM clauses",
      "type": "regex",
      "pattern": "(?i)(?<=FROM\\s+)[A-Za-z_][A-Za-z0-9_]*(?=\\.)",
      "prefix": "DATABASE",
      "severity": "High",
      "order": 5
    },
    {
      "ruleId": "sql_schema_qualified",
      "name": "Schema-Qualified Tables",
      "description": "Detects schema.table patterns",
      "type": "regex",
      "pattern": "(?i)\\b(dbo|staging|archive|etl)\\.[A-Za-z_][A-Za-z0-9_]*\\b",
      "prefix": "TABLE",
      "severity": "High",
      "order": 6
    },
    {
      "ruleId": "sql_linked_servers",
      "name": "Linked Server References",
      "description": "Detects [Server].[Database] patterns",
      "type": "regex",
      "pattern": "\\[[A-Za-z_][A-Za-z0-9_]*\\]\\.\\[[A-Za-z_][A-Za-z0-9_]*\\]",
      "prefix": "SERVER",
      "severity": "Critical",
      "order": 1
    }
  ]
}
EOF

test_result $? "Created global rules config"

echo ""
echo "Step 4: Testing sanitize with global config..."
cd /Users/admin/Dev/YOLOProjects/codebleach
dotnet run --project src/CodeBleach/CodeBleach.csproj -- sanitize ~/codebleach-test-project --verbose > /tmp/sanitize-output.txt 2>&1
test_result $? "Sanitize command executed"

# Check if output was created
if [ -d ~/codebleach-test-project-sanitize ]; then
    test_result 0 "Sanitized directory created"
else
    test_result 1 "Sanitized directory created"
fi

# Check if SQL content was sanitized
if [ -f ~/codebleach-test-project-sanitize/src/queries.sql ]; then
    SANITIZED_CONTENT=$(cat ~/codebleach-test-project-sanitize/src/queries.sql)
    
    # Check for DATABASE_ aliases
    if echo "$SANITIZED_CONTENT" | grep -q "DATABASE_"; then
        test_result 0 "Database names sanitized (DATABASE_ aliases found)"
    else
        test_result 1 "Database names sanitized"
    fi
    
    # Check for SERVER_ aliases
    if echo "$SANITIZED_CONTENT" | grep -q "SERVER_"; then
        test_result 0 "Server names sanitized (SERVER_ aliases found)"
    else
        test_result 1 "Server names sanitized"
    fi
    
    # Check for TABLE_ aliases
    if echo "$SANITIZED_CONTENT" | grep -q "TABLE_"; then
        test_result 0 "Table names sanitized (TABLE_ aliases found)"
    else
        test_result 1 "Table names sanitized"
    fi
    
    # Verify original values are gone
    if ! echo "$SANITIZED_CONTENT" | grep -q "ProductionDB"; then
        test_result 0 "Original database names removed"
    else
        test_result 1 "Original database names removed"
    fi
else
    test_result 1 "Sanitized SQL file exists"
fi

echo ""
echo "Step 5: Testing restore (1st round)..."
cd ~/codebleach-test-project-sanitize
dotnet run --project /Users/admin/Dev/YOLOProjects/codebleach/src/CodeBleach/CodeBleach.csproj -- restore > /tmp/restore1-output.txt 2>&1
test_result $? "First restore completed"

# Verify restore brought back original values
RESTORED_CONTENT=$(cat ~/codebleach-test-project-sanitize/src/queries.sql)
if echo "$RESTORED_CONTENT" | grep -q "ProductionDB"; then
    test_result 0 "Original database names restored (1st restore)"
else
    test_result 1 "Original database names restored (1st restore)"
fi

echo ""
echo "Step 6: Testing 4-roundtrip (sanitize -> restore -> sanitize -> restore)..."

# 2nd sanitize
cd ~/codebleach-test-project-sanitize
dotnet run --project /Users/admin/Dev/YOLOProjects/codebleach/src/CodeBleach/CodeBleach.csproj -- sanitize . --output ~/test-round2 > /dev/null 2>&1
test_result $? "2nd sanitize completed"

# 2nd restore
cd ~/test-round2
dotnet run --project /Users/admin/Dev/YOLOProjects/codebleach/src/CodeBleach/CodeBleach.csproj -- restore > /dev/null 2>&1
test_result $? "2nd restore completed"

# Verify content matches original after 2 full cycles
FINAL_CONTENT=$(cat ~/test-round2/src/queries.sql)
ORIGINAL_CONTENT=$(cat ~/codebleach-test-project/src/queries.sql)

if [ "$FINAL_CONTENT" == "$ORIGINAL_CONTENT" ]; then
    test_result 0 "4-roundtrip maintains perfect fidelity"
else
    test_result 1 "4-roundtrip maintains perfect fidelity"
fi

# Cleanup round 2
rm -rf ~/test-round2

echo ""
echo "Step 7: Testing config commands..."

# Test config --path
dotnet run --project /Users/admin/Dev/YOLOProjects/codebleach/src/CodeBleach/CodeBleach.csproj -- config --path > /tmp/config-path.txt 2>&1
if [ $? -eq 0 ] && grep -q "Global Configuration" /tmp/config-path.txt; then
    test_result 0 "config --path command works"
else
    test_result 1 "config --path command works"
fi

# Test config --list
dotnet run --project /Users/admin/Dev/YOLOProjects/codebleach/src/CodeBleach/CodeBleach.csproj -- config --list > /tmp/config-list.txt 2>&1
if [ $? -eq 0 ] && grep -q "Configuration Files" /tmp/config-list.txt; then
    test_result 0 "config --list command works"
else
    test_result 1 "config --list command works"
fi

echo ""
echo "=========================================="
echo "Test Summary"
echo "=========================================="
echo -e "${GREEN}Passed: $TESTS_PASSED${NC}"
echo -e "${RED}Failed: $TESTS_FAILED${NC}"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ ALL TESTS PASSED!${NC}"
    echo ""
    echo "CodeBleach v1.3.0 global configuration is working correctly."
    exit 0
else
    echo -e "${RED}✗ SOME TESTS FAILED${NC}"
    echo ""
    echo "Check the output above for details."
    exit 1
fi

