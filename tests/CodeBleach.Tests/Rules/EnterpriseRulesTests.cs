using System.Text.RegularExpressions;
using CodeBleach.Core.Models;

namespace CodeBleach.Tests.Rules;

/// <summary>
/// Tests for enterprise-specific sanitization rules.
/// Validates regex patterns for corporate naming conventions.
/// </summary>
public class EnterpriseRulesTests
{
    // ========================================
    // Server FQDN Pattern Tests
    // ========================================

    [Theory]
    [InlineData("PRODSRV01.acme-corp.com", true)]
    [InlineData("STGSRV01.internal.net", true)]
    [InlineData("DEVSRV01.corp.net", true)]
    [InlineData("DB-EAST-01.acme-corp.com", true)]
    [InlineData("LINKEDSRV01.internal.com", true)]
    [InlineData("localhost", false)]
    [InlineData("google.com", false)]
    [InlineData("server", false)]
    [InlineData("192.168.1.1", false)]
    public void ServerFqdnPattern_MatchesCorrectly(string input, bool shouldMatch)
    {
        // Arrange
        var pattern = @"\b[A-Z][A-Z0-9-]+\.(acme-corp|internal|corp)\.(com|net)\b";
        var regex = new Regex(pattern);

        // Act & Assert
        regex.IsMatch(input).Should().Be(shouldMatch);
    }

    // ========================================
    // Enterprise Server Name Pattern Tests
    // ========================================

    [Theory]
    [InlineData("PRODSRV01", true)]
    [InlineData("PRODSRV02", true)]
    [InlineData("STGSRV01", true)]
    [InlineData("DEVSRV01", true)]
    [InlineData("SQLSVR01", true)]
    [InlineData("DBSVR01", true)]
    [InlineData("FILESVR01", true)]
    [InlineData("LINKEDSRV01", true)]
    [InlineData("SERVER01", false)]
    [InlineData("PRODSERVER", false)]
    [InlineData("prodsrv01", false)] // Case sensitive
    [InlineData("PROD", false)]
    public void ServerNamePattern_MatchesCorrectly(string input, bool shouldMatch)
    {
        // Arrange
        var pattern = @"\b(PROD|STG|DEV|SQL|DB|FILE|LINKED)S(RV|VR|ERVER)\d{1,2}\b";
        var regex = new Regex(pattern);

        // Act & Assert
        regex.IsMatch(input).Should().Be(shouldMatch);
    }

    // ========================================
    // Database Name Pattern Tests
    // ========================================

    [Theory]
    [InlineData("DBZMEW", true)]
    [InlineData("DBZBHI", true)]
    [InlineData("DB2MEW", false)] // Pattern requires letter after DB, not digit
    [InlineData("DBPROD", true)]
    [InlineData("DBA", false)] // Too short (needs at least 2 chars after DB)
    [InlineData("DB", false)] // No suffix
    [InlineData("DATABASE", false)] // Doesn't start with DB prefix
    [InlineData("dbzmew", false)] // Lowercase
    public void DatabaseNamePattern_MatchesCorrectly(string input, bool shouldMatch)
    {
        // Arrange
        var pattern = @"\bDB[A-Z][A-Z0-9]+\b";
        var regex = new Regex(pattern);

        // Act & Assert
        regex.IsMatch(input).Should().Be(shouldMatch);
    }

    // ========================================
    // SQL Four-Part Name Pattern Tests (Bracketed)
    // ========================================

    [Theory]
    [InlineData("[LINKEDSRV01].[DBZMEW].[dbo].[TB00123]", true)]
    [InlineData("[SERVER01].[DATABASE].[dbo].[TABLE]", true)]
    [InlineData("[SRV].[DB].[dbo].[TBL]", true)]
    [InlineData("LINKEDSRV01.DBZMEW.dbo.TB00123", false)] // No brackets
    [InlineData("[LINKEDSRV01].[DBZMEW].[DBO].[TB00123]", false)] // DBO uppercase
    [InlineData("[linkedsrv01].[dbzmew].[dbo].[tb00123]", false)] // Lowercase
    public void FourPartBracketedPattern_MatchesCorrectly(string input, bool shouldMatch)
    {
        // Arrange
        var pattern = @"\[[A-Z][A-Z0-9_]+\]\.\[[A-Z][A-Z0-9_]+\]\.\[[a-z]+\]\.\[[A-Z][A-Z0-9_]+\]";
        var regex = new Regex(pattern);

        // Act & Assert
        regex.IsMatch(input).Should().Be(shouldMatch);
    }

    // ========================================
    // SQL Four-Part Name Pattern Tests (Unbracketed)
    // ========================================

    [Theory]
    [InlineData("LINKEDSRV01.DBZMEW.dbo.TB00123", true)]
    [InlineData("SERVER01.DATABASE.dbo.TABLE", true)]
    [InlineData("PRODSRV01.DBZMEW.dbo.EMPLOYEES", true)]
    [InlineData("[LINKEDSRV01].[DBZMEW].[dbo].[TB00123]", false)] // Bracketed
    [InlineData("LINKEDSRV01.DBZMEW.DBO.TB00123", false)] // DBO uppercase
    [InlineData("DBZMEW.dbo.TB00123", false)] // Only 3 parts
    public void FourPartUnbracketedPattern_MatchesCorrectly(string input, bool shouldMatch)
    {
        // Arrange
        var pattern = @"\b[A-Z][A-Z0-9_]+\.[A-Z][A-Z0-9_]+\.dbo\.[A-Z][A-Z0-9_]+\b";
        var regex = new Regex(pattern);

        // Act & Assert
        regex.IsMatch(input).Should().Be(shouldMatch);
    }

    // ========================================
    // SQL Three-Part Name Pattern Tests
    // ========================================

    [Theory]
    [InlineData("DBZMEW.dbo.TB00123", true)]
    [InlineData("DATABASE.dbo.TABLE", true)]
    [InlineData("PROD_DB.dbo.EMPLOYEES", true)]
    [InlineData("DBZMEW.DBO.TB00123", false)] // DBO uppercase
    [InlineData("DBZMEW.TB00123", false)] // Only 2 parts
    [InlineData("dbzmew.dbo.tb00123", false)] // Lowercase
    public void ThreePartDboPattern_MatchesCorrectly(string input, bool shouldMatch)
    {
        // Arrange
        var pattern = @"\b[A-Z][A-Z0-9_]+\.dbo\.[A-Z][A-Z0-9_]+\b";
        var regex = new Regex(pattern);

        // Act & Assert
        regex.IsMatch(input).Should().Be(shouldMatch);
    }

    // ========================================
    // SQL Prefixed Table Pattern Tests
    // ========================================

    [Theory]
    [InlineData("TB00123", true)]
    [InlineData("TB00456", true)]
    [InlineData("TA09052", true)]
    [InlineData("TB123456", true)]
    [InlineData("TB0012", false)] // Only 4 digits
    [InlineData("TB1234567", false)] // 7 digits
    [InlineData("TC00123", false)] // Wrong prefix
    [InlineData("tb00123", false)] // Lowercase
    public void PrefixedTablePattern_MatchesCorrectly(string input, bool shouldMatch)
    {
        // Arrange
        var pattern = @"\b(TB|TA)\d{5,6}\b";
        var regex = new Regex(pattern);

        // Act & Assert
        regex.IsMatch(input).Should().Be(shouldMatch);
    }

    // ========================================
    // AD Service Account Pattern Tests
    // ========================================

    [Theory]
    [InlineData(@"ACME\svc_sql_agent", true)]
    [InlineData(@"ACME\svc_deployment", true)]
    [InlineData(@"CORP\svc_monitoring", true)]
    [InlineData(@"DOMAIN\svc_backup_job", true)]
    [InlineData(@"acme\svc_test", false)] // Lowercase domain
    [InlineData(@"ACME\john.doe", false)] // Not service account
    [InlineData(@"svc_test", false)] // No domain
    [InlineData(@"ACME\SVC_TEST", false)] // Uppercase svc
    public void AdServiceAccountPattern_MatchesCorrectly(string input, bool shouldMatch)
    {
        // Arrange
        var pattern = @"\b[A-Z]+\\svc_[a-z0-9_]+\b";
        var regex = new Regex(pattern);

        // Act & Assert
        regex.IsMatch(input).Should().Be(shouldMatch);
    }

    // ========================================
    // Vault Path Pattern Tests
    // ========================================

    [Theory]
    [InlineData("secret/data/production/database", true)]
    [InlineData("secret/data/staging/api-keys", true)]
    [InlineData("secret/data/prod/certs/main", true)]
    [InlineData("vault/data/production", false)] // Wrong prefix
    [InlineData("secret/production", false)] // Missing /data/
    [InlineData("SECRET/DATA/PRODUCTION", false)] // Uppercase
    public void VaultPathPattern_MatchesCorrectly(string input, bool shouldMatch)
    {
        // Arrange
        var pattern = @"secret/data/[a-z0-9/-]+";
        var regex = new Regex(pattern);

        // Act & Assert
        regex.IsMatch(input).Should().Be(shouldMatch);
    }

    // ========================================
    // Private IP Pattern Tests
    // ========================================

    [Theory]
    [InlineData("10.50.100.10", true)]
    [InlineData("10.0.0.1", true)]
    [InlineData("10.255.255.255", true)]
    [InlineData("192.168.1.100", false)] // 192.168.x.x is different rule
    [InlineData("172.16.0.1", false)] // 172.x.x.x is different rule
    [InlineData("8.8.8.8", false)] // Public IP
    [InlineData("256.1.1.1", false)] // Invalid
    public void PrivateIp10Pattern_MatchesCorrectly(string input, bool shouldMatch)
    {
        // Arrange
        var pattern = @"\b10\.\d{1,3}\.\d{1,3}\.\d{1,3}\b";
        var regex = new Regex(pattern);

        // Act & Assert
        regex.IsMatch(input).Should().Be(shouldMatch);
    }

    [Theory]
    [InlineData("192.168.1.100", true)]
    [InlineData("192.168.0.1", true)]
    [InlineData("192.168.255.255", true)]
    [InlineData("192.169.1.1", false)] // Not 192.168.x.x
    [InlineData("10.0.0.1", false)] // 10.x.x.x
    public void PrivateIp192Pattern_MatchesCorrectly(string input, bool shouldMatch)
    {
        // Arrange
        var pattern = @"\b192\.168\.\d{1,3}\.\d{1,3}\b";
        var regex = new Regex(pattern);

        // Act & Assert
        regex.IsMatch(input).Should().Be(shouldMatch);
    }

    // ========================================
    // UNC Path Pattern Tests
    // ========================================

    [Theory]
    [InlineData(@"\\FILESRV01\SQLBackups\Production", true)]
    [InlineData(@"\\SERVER\Share\Folder", true)]
    [InlineData(@"\\server-01\data", true)]
    [InlineData(@"\FILESRV01\Share", false)] // Single backslash
    [InlineData(@"C:\Folder\File", false)] // Local path
    public void UncPathPattern_MatchesCorrectly(string input, bool shouldMatch)
    {
        // Arrange
        var pattern = @"\\\\[a-zA-Z0-9._-]+\\[^\s]+";
        var regex = new Regex(pattern);

        // Act & Assert
        regex.IsMatch(input).Should().Be(shouldMatch);
    }
}
