using Microsoft.EntityFrameworkCore;

namespace MyApp.Data
{
    public class AppDbContext : DbContext
    {
        protected override void OnConfiguring(DbContextOptionsBuilder options)
        {
            options.UseSqlServer("CONNSTR_0SERVER_0;Database=TABLE_0");
        }
        
        public DbSet<User> Users { get; set; }
    }
    
    public class User
    {
        public int Id { get; set; }
        public string Name { get; set; }
    }
}

