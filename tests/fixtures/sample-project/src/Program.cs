using System;

namespace MyApp
{
    class Program
    {
        static void Main(string[] args)
        {
            // Connect to production database
            var connectionString = "Server=ProductionDB;Database=user_accounts;User=admin";
            Console.WriteLine($"Connecting to {connectionString}");
            
            // Internal API endpoint
            var apiUrl = "https://api-prod.internal.corp.com/v1/users";
            var redisHost = "192.168.1.100";
            var redisPort = 6379;
            
            // File path on Windows server
            var logPath = @"C:\Projects\SecretProject\logs\app.log";
            
            Console.WriteLine($"API: {apiUrl}");
            Console.WriteLine($"Redis: {redisHost}:{redisPort}");
            Console.WriteLine($"Logs: {logPath}");
        }
    }
}

