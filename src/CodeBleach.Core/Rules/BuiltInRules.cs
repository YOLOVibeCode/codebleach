using CodeBleach.Core.Models;

namespace CodeBleach.Core.Rules;

/// <summary>
/// Built-in sanitization rules for common sensitive data patterns.
/// </summary>
public static class BuiltInRules
{
    public static IEnumerable<SanitizationRule> All => new[]
    {
        ServerNames,
        ProdDatabases,
        TableNamesProd,
        TableNamesUsers,
        PrivateIp10,
        PrivateIp172,
        PrivateIp192,
        ConnectionStrings,
        WindowsPaths,
        UncPaths,
        InternalHostnames
    };
    
    public static SanitizationRule ServerNames => new()
    {
        RuleId = "server_names",
        Name = "Server/Database Names",
        Description = "Detects server and database names like ProductionDB",
        Pattern = @"\b[A-Z][a-zA-Z]*DB\d*\b",
        Prefix = "SERVER",
        Severity = RuleSeverity.Medium,
        Order = 10
    };
    
    public static SanitizationRule ProdDatabases => new()
    {
        RuleId = "prod_databases",
        Name = "Production Databases",
        Description = "Detects production database names",
        Pattern = @"\b[a-zA-Z]+[_-]?[Pp]rod(uction)?\b",
        Prefix = "SERVER",
        Severity = RuleSeverity.Medium,
        Order = 11
    };
    
    public static SanitizationRule TableNamesProd => new()
    {
        RuleId = "table_names_prod",
        Name = "Production Tables",
        Description = "Detects production table names",
        Pattern = @"\b[a-z_]+_prod\b",
        Prefix = "TABLE",
        Severity = RuleSeverity.Medium,
        Order = 12
    };
    
    public static SanitizationRule TableNamesUsers => new()
    {
        RuleId = "table_names_users",
        Name = "User Tables",
        Description = "Detects user/account table names",
        Pattern = @"\b(users?|accounts?|customers?)(_\w+)?\b",
        Prefix = "TABLE",
        Severity = RuleSeverity.Medium,
        Order = 13
    };
    
    public static SanitizationRule PrivateIp10 => new()
    {
        RuleId = "private_ip_10",
        Name = "Private IP (10.x)",
        Description = "Detects 10.x.x.x private IP addresses",
        Pattern = @"\b10\.\d{1,3}\.\d{1,3}\.\d{1,3}\b",
        Prefix = "IP",
        Severity = RuleSeverity.High,
        Order = 20
    };
    
    public static SanitizationRule PrivateIp172 => new()
    {
        RuleId = "private_ip_172",
        Name = "Private IP (172.x)",
        Description = "Detects 172.16-31.x.x private IP addresses",
        Pattern = @"\b172\.(1[6-9]|2\d|3[01])\.\d{1,3}\.\d{1,3}\b",
        Prefix = "IP",
        Severity = RuleSeverity.High,
        Order = 21
    };
    
    public static SanitizationRule PrivateIp192 => new()
    {
        RuleId = "private_ip_192",
        Name = "Private IP (192.168.x)",
        Description = "Detects 192.168.x.x private IP addresses",
        Pattern = @"\b192\.168\.\d{1,3}\.\d{1,3}\b",
        Prefix = "IP",
        Severity = RuleSeverity.High,
        Order = 22
    };
    
    public static SanitizationRule ConnectionStrings => new()
    {
        RuleId = "connection_string",
        Name = "Connection Strings",
        Description = "Detects connection string server/host parts",
        Pattern = @"(?i)(server|data source|host)=[^;]+",
        Prefix = "CONNSTR",
        Severity = RuleSeverity.High,
        Order = 30
    };
    
    public static SanitizationRule WindowsPaths => new()
    {
        RuleId = "windows_path",
        Name = "Windows Paths",
        Description = "Detects Windows file paths",
        Pattern = @"[A-Za-z]:\\[^\s*?""<>|:]+",
        Prefix = "PATH",
        Severity = RuleSeverity.Medium,
        Order = 40
    };
    
    public static SanitizationRule UncPaths => new()
    {
        RuleId = "unc_path",
        Name = "UNC Paths",
        Description = "Detects UNC network paths",
        Pattern = @"\\\\[a-zA-Z0-9._-]+\\[^\s]+",
        Prefix = "PATH",
        Severity = RuleSeverity.Medium,
        Order = 41
    };
    
    public static SanitizationRule InternalHostnames => new()
    {
        RuleId = "internal_hostname",
        Name = "Internal Hostnames",
        Description = "Detects internal hostnames (.internal, .local, .corp, .lan)",
        Pattern = @"\b[a-z][a-z0-9-]*\.(internal|local|corp|lan)\b",
        Prefix = "HOST",
        Severity = RuleSeverity.Medium,
        Order = 50
    };
}

