using System.CommandLine;
using CodeBleach.Core.Services;

namespace CodeBleach.Commands;

public static class InitCommand
{
    public static Command Create()
    {
        var globalOption = new Option<bool>(
            new[] { "--global", "-g" },
            "Create global user configuration");
        
        var sqlOption = new Option<bool>(
            "--sql",
            "Include SQL-focused rules (databases, tables, schemas)");
        
        var forceOption = new Option<bool>(
            new[] { "--force", "-f" },
            "Overwrite existing configuration file");
        
        var command = new Command("init", "Initialize CodeBleach configuration")
        {
            globalOption,
            sqlOption,
            forceOption
        };
        
        command.SetHandler(async (global, sql, force) =>
        {
            await HandleAsync(global, sql, force);
        }, globalOption, sqlOption, forceOption);
        
        return command;
    }
    
    private static async Task HandleAsync(bool global, bool sql, bool force)
    {
        string targetPath;
        
        if (global)
        {
            var locator = new GlobalConfigLocator();
            var configDir = locator.GetGlobalConfigDirectory();
            
            // Ensure directory exists
            Directory.CreateDirectory(configDir);
            
            targetPath = locator.GetGlobalRulesFilePath();
        }
        else
        {
            targetPath = Path.Combine(Directory.GetCurrentDirectory(), ".codebleach-rules.json");
        }
        
        if (File.Exists(targetPath) && !force)
        {
            Console.Error.WriteLine($"Error: Configuration file already exists: {targetPath}");
            Console.Error.WriteLine("Use --force to overwrite.");
            Environment.Exit(1);
            return;
        }
        
        // Create example configuration
        var exampleConfig = sql 
            ? GetSqlExampleConfig() 
            : GetBasicExampleConfig();
        
        await File.WriteAllTextAsync(targetPath, exampleConfig);
        
        Console.WriteLine($"Created configuration file: {targetPath}");
        Console.WriteLine();
        
        if (sql)
        {
            Console.WriteLine("SQL-focused rules have been configured.");
            Console.WriteLine("These rules will detect:");
            Console.WriteLine("  - Database names in FROM clauses");
            Console.WriteLine("  - Schema-qualified table references (dbo.TableName)");
            Console.WriteLine("  - Linked server references ([Server].[Database])");
        }
        else
        {
            Console.WriteLine("Basic example configuration created.");
        }
        
        Console.WriteLine();
        Console.WriteLine("Edit this file to customize your sanitization rules.");
        Console.WriteLine("Documentation: https://github.com/YOLOVibeCode/codebleach#custom-rules");
    }
    
    private static string GetBasicExampleConfig()
    {
        return """
        {
          "rules": [
            {
              "ruleId": "custom_example",
              "name": "Custom Example Rule",
              "description": "Example custom rule - replace with your patterns",
              "type": "regex",
              "pattern": "\\bEXAMPLE_PATTERN\\b",
              "prefix": "CUSTOM",
              "severity": "Medium",
              "enabled": false,
              "order": 100
            }
          ]
        }
        """;
    }
    
    private static string GetSqlExampleConfig()
    {
        return """
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
              "description": "Detects schema.table patterns (dbo.TableName)",
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
            },
            {
              "ruleId": "sql_server_names",
              "name": "SQL Server Names",
              "description": "Detects SQL Server instance names",
              "type": "regex",
              "pattern": "(?i)\\b[A-Z][A-Z0-9_-]*(?:SQL|DB|SERVER)[A-Z0-9_-]*\\b",
              "prefix": "SERVER",
              "severity": "High",
              "order": 2
            }
          ]
        }
        """;
    }
}

