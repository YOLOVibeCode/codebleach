# Terraform configuration for Enterprise Platform
# Environment: Production
# Region: US-East

terraform {
  required_version = ">= 1.0"
  
  backend "s3" {
    bucket = "acme-terraform-state-prod"
    key    = "enterprise-platform/production/terraform.tfstate"
    region = "us-east-1"
  }
}

provider "aws" {
  region = "us-east-1"
  
  default_tags {
    tags = {
      Environment = "production"
      Project     = "enterprise-platform"
      ManagedBy   = "terraform"
    }
  }
}

# Database subnet group
resource "aws_db_subnet_group" "main" {
  name       = "acme-prod-db-subnet"
  subnet_ids = ["subnet-0abc123def456789a", "subnet-0abc123def456789b"]
  
  tags = {
    Name = "ACME Production DB Subnet Group"
  }
}

# RDS instance for DBZMEW
resource "aws_db_instance" "primary" {
  identifier     = "prodsrv01-dbzmew"
  engine         = "sqlserver-ee"
  engine_version = "15.00.4236.7.v1"
  instance_class = "db.r5.2xlarge"
  
  allocated_storage     = 500
  max_allocated_storage = 1000
  storage_type          = "gp3"
  storage_encrypted     = true
  
  db_name  = "DBZMEW"
  username = "admin"
  password = var.db_password  # From Vault: secret/data/production/database
  
  vpc_security_group_ids = [aws_security_group.db.id]
  db_subnet_group_name   = aws_db_subnet_group.main.name
  
  backup_retention_period = 30
  backup_window          = "03:00-04:00"
  maintenance_window     = "Sun:04:00-Sun:05:00"
  
  multi_az               = true
  publicly_accessible    = false
  deletion_protection    = true
  
  tags = {
    Name        = "PRODSRV01"
    Database    = "DBZMEW"
    Environment = "production"
  }
}

# RDS instance for DR (PRODSRV02)
resource "aws_db_instance" "replica" {
  identifier     = "prodsrv02-dbzmew-dr"
  replicate_source_db = aws_db_instance.primary.identifier
  instance_class = "db.r5.2xlarge"
  
  vpc_security_group_ids = [aws_security_group.db.id]
  
  backup_retention_period = 7
  
  tags = {
    Name        = "PRODSRV02"
    Database    = "DBZMEW"
    Environment = "production-dr"
  }
}

# Security group for database
resource "aws_security_group" "db" {
  name        = "acme-prod-db-sg"
  description = "Security group for production database"
  vpc_id      = var.vpc_id
  
  ingress {
    description = "SQL Server from app servers"
    from_port   = 1433
    to_port     = 1433
    protocol    = "tcp"
    cidr_blocks = ["10.50.101.0/24"]  # App server subnet
  }
  
  ingress {
    description = "SQL Server from linked server"
    from_port   = 1433
    to_port     = 1433
    protocol    = "tcp"
    cidr_blocks = ["10.50.200.0/24"]  # BI server subnet (LINKEDSRV01)
  }
  
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
  
  tags = {
    Name = "ACME Production DB Security Group"
  }
}

# Route53 record for primary database
resource "aws_route53_record" "db_primary" {
  zone_id = var.private_zone_id
  name    = "db-primary.internal.acme.com"
  type    = "CNAME"
  ttl     = 300
  records = [aws_db_instance.primary.address]
}

# Route53 record for DR database
resource "aws_route53_record" "db_dr" {
  zone_id = var.private_zone_id
  name    = "db-dr.internal.acme.com"
  type    = "CNAME"
  ttl     = 300
  records = [aws_db_instance.replica.address]
}

# S3 bucket for backups
resource "aws_s3_bucket" "backups" {
  bucket = "acme-prod-sql-backups"
  
  tags = {
    Name        = "SQL Backups - Production"
    Environment = "production"
  }
}

# Outputs
output "primary_db_endpoint" {
  description = "Primary database endpoint (PRODSRV01)"
  value       = aws_db_instance.primary.endpoint
}

output "replica_db_endpoint" {
  description = "Replica database endpoint (PRODSRV02)"
  value       = aws_db_instance.replica.endpoint
}

output "backup_bucket" {
  description = "S3 bucket for SQL backups"
  value       = aws_s3_bucket.backups.id
}
