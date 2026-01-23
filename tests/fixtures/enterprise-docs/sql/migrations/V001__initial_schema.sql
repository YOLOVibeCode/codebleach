-- Migration: V001__initial_schema.sql
-- Description: Initial schema setup for Enterprise Platform
-- Server: PRODSRV01.acme-corp.com
-- Database: DBZMEW

USE DBZMEW;
GO

-- ============================================
-- Employee Master Table
-- ============================================
CREATE TABLE dbo.TB00123 (
    EMP_ID INT PRIMARY KEY IDENTITY(1,1),
    FIRST_NM NVARCHAR(100) NOT NULL,
    LAST_NM NVARCHAR(100) NOT NULL,
    EMAIL NVARCHAR(255),
    DEPT_ID INT,
    HIRE_DT DATE,
    ACTIVE_IND CHAR(1) DEFAULT 'Y',
    CREATED_DT DATETIME DEFAULT GETDATE(),
    MODIFIED_DT DATETIME DEFAULT GETDATE()
);
GO

-- ============================================
-- Department Hierarchy Table
-- ============================================
CREATE TABLE dbo.TB00456 (
    DEPT_ID INT PRIMARY KEY IDENTITY(1,1),
    DEPT_NM NVARCHAR(200) NOT NULL,
    PARENT_DEPT_ID INT,
    COST_CENTER VARCHAR(20),
    ACTIVE_IND CHAR(1) DEFAULT 'Y',
    CREATED_DT DATETIME DEFAULT GETDATE()
);
GO

-- ============================================
-- Job Classifications Table
-- ============================================
CREATE TABLE dbo.TB00789 (
    JOB_ID INT PRIMARY KEY IDENTITY(1,1),
    JOB_TITLE NVARCHAR(200) NOT NULL,
    JOB_LEVEL INT,
    MIN_SALARY DECIMAL(12,2),
    MAX_SALARY DECIMAL(12,2),
    ACTIVE_IND CHAR(1) DEFAULT 'Y'
);
GO

-- ============================================
-- Salary History Table
-- ============================================
CREATE TABLE dbo.TB00999 (
    SALARY_ID INT PRIMARY KEY IDENTITY(1,1),
    EMP_ID INT NOT NULL,
    SALARY_AMT DECIMAL(12,2) NOT NULL,
    EFFECTIVE_DT DATE NOT NULL,
    END_DT DATE,
    CREATED_BY NVARCHAR(100) DEFAULT SYSTEM_USER,
    CREATED_DT DATETIME DEFAULT GETDATE(),
    CONSTRAINT FK_SALARY_EMP FOREIGN KEY (EMP_ID) REFERENCES dbo.TB00123(EMP_ID)
);
GO

-- ============================================
-- Create linked server view for BI data
-- ============================================
CREATE VIEW dbo.VW_EMPLOYEE_WITH_METRICS AS
SELECT 
    e.EMP_ID,
    e.FIRST_NM,
    e.LAST_NM,
    e.DEPT_ID,
    d.DEPT_NM,
    s.SALARY_AMT AS CURRENT_SALARY,
    bi.PERFORMANCE_SCORE,
    bi.LAST_REVIEW_DT
FROM DBZMEW.dbo.TB00123 e
JOIN DBZMEW.dbo.TB00456 d ON e.DEPT_ID = d.DEPT_ID
JOIN DBZMEW.dbo.TB00999 s ON e.EMP_ID = s.EMP_ID AND s.END_DT IS NULL
LEFT JOIN [LINKEDSRV01].[DBZBHI].[dbo].[BI_EMPLOYEE_METRICS] bi ON e.EMP_ID = bi.EMP_ID
WHERE e.ACTIVE_IND = 'Y';
GO

-- ============================================
-- Stored procedure for cross-database report
-- ============================================
CREATE PROCEDURE dbo.sp_GenerateFinancialReport
    @FiscalYear INT,
    @DeptID INT = NULL
AS
BEGIN
    SET NOCOUNT ON;
    
    -- Insert into reporting database on linked server
    INSERT INTO [LINKEDSRV01].[DBZBHI].[dbo].[FINANCIAL_REPORT] (
        REPORT_DT,
        FISCAL_YEAR,
        DEPT_ID,
        TOTAL_HEADCOUNT,
        TOTAL_SALARY,
        AVG_SALARY
    )
    SELECT 
        GETDATE(),
        @FiscalYear,
        e.DEPT_ID,
        COUNT(DISTINCT e.EMP_ID),
        SUM(s.SALARY_AMT),
        AVG(s.SALARY_AMT)
    FROM PRODSRV01.DBZMEW.dbo.TB00123 e
    JOIN PRODSRV01.DBZMEW.dbo.TB00999 s ON e.EMP_ID = s.EMP_ID
    WHERE YEAR(s.EFFECTIVE_DT) = @FiscalYear
      AND s.END_DT IS NULL
      AND (@DeptID IS NULL OR e.DEPT_ID = @DeptID)
    GROUP BY e.DEPT_ID;
    
    -- Log the execution
    INSERT INTO DBZMEW.dbo.REPORT_LOG (REPORT_NM, EXEC_DT, EXEC_BY)
    VALUES ('FinancialReport', GETDATE(), SYSTEM_USER);
END;
GO

-- ============================================
-- Grant permissions to service account
-- ============================================
GRANT SELECT ON dbo.TB00123 TO [ACME\svc_sql_agent];
GRANT SELECT ON dbo.TB00456 TO [ACME\svc_sql_agent];
GRANT SELECT ON dbo.TB00789 TO [ACME\svc_sql_agent];
GRANT SELECT ON dbo.TB00999 TO [ACME\svc_sql_agent];
GRANT EXECUTE ON dbo.sp_GenerateFinancialReport TO [ACME\svc_deployment];
GO

PRINT 'Migration V001 completed successfully on ' + @@SERVERNAME;
GO
