# Mainframe Sanitization Improvements

## What This Branch Does

Fixes CodeBleach's handling of mainframe codebases (COBOL + JCL + CARD utility files).
Previously, only 76/436 files (17%) were processed when sanitizing a typical mainframe
codebase. With these changes, 410/436 files (94%) are processed with 95,000+ replacements.

## Changes

### New: MainframeUtility Processor (`CodeBleach.Processors.MainframeUtility`)

A catch-all language processor for extensionless mainframe utility control cards that
neither the COBOL nor JCL processor claims. Detects and sanitizes 8 sub-types:

| Sub-type | Detection | What it sanitizes |
|----------|-----------|-------------------|
| **DB2 BIND** | `DSN SYSTEM(` / `BIND` | System names, program names, plan names, library DSNs |
| **IDCAMS** | `DELETE`/`DEFINE`/`REPRO` with dataset syntax | Dataset names, volume serials, cluster names |
| **DFSORT** | `SORT FIELDS=` / `MERGE FIELDS=` | Comment lines, inline field description comments |
| **FTP** | Line-structure analysis + FTP command detection | Hostnames, passwords, remote paths, dataset names |
| **SMTP** | `HELO` / `MAIL FROM:` / `RCPT TO:` | Server names, email addresses, subject lines |
| **MFS/IMS** | `MSG TYPE=` / `MFLD` / `DFLD` / `FMT` | Screen labels, field names, map names |
| **DL/I Control** | `CONTROL CNTL` / `OPTION PRINT` | Minimal (syntax is not sensitive) |
| **Parameter** | Catch-all for short non-binary files | Token-level replacement using shared context |

### Fixed: COBOL Copybook Identifier Leakage (Pass 1.5)

Added a discovery pass between Pass 1 (local definitions) and Pass 2 (replacement) that
scans procedure/data/SQL lines for identifiers not in the alias map. These are identifiers
from COPY members that the processor can't see. Any non-keyword identifier used in code
gets aliased, closing the copybook leak.

### Fixed: COBOL EXEC SQL Qualified Host Variables

Changed `HostVariablePattern` to capture `:RECORD.FIELD` qualified references and replace
both the record and field parts independently.

### Fixed: COBOL String Literal Obfuscation

Added `ObfuscateStringLiterals` pass that replaces non-trivial string content (4+ chars,
contains letters) in VALUE clauses, MOVE/DISPLAY statements with `STR_N` aliases.

### Fixed: New Built-in Regex Rules

- `EmailAddresses` - catches `user@domain.com` patterns
- `MvsDatasetQuoted` - catches `'HLQ.QUAL.NAME'` patterns
- `MvsDatasetBare` - catches `HLQ.QUAL.NAME` patterns

### Fixed: DIRECTORY_STRUCTURE.txt Content Sanitization

Phase 3.5 post-processing replaces original file/directory names in `.txt` files containing
directory tree listings with their mapped aliases.

## Test Fixture: MiniIron

`tests/fixtures/MiniIron/` - A compact 15-file mainframe codebase for fast iteration (~3s):

```
MiniIron/
├── CARD/ACME/PROD/CARDLIB/   (8 files: DB2 BIND, IDCAMS, SORT, FTP, SMTP, MFS, DL/I, PARM)
├── COBOL/ACME/PROD/SOURCLIB/ (3 files: main program, subroutine, copybook)
├── JCL/ACME/PROD/JCLLIB/     (3 files: batch job, bind job, FTP job)
└── DIRECTORY_STRUCTURE.txt
```

All files cross-reference each other (JCL runs COBOL programs, references CARD members;
COBOL programs CALL each other, COPY shared copybooks, use EXEC SQL with DB2 tables;
CARD files reference program names, datasets, DB2 objects).

Run: `dotnet run --project src/CodeBleach/CodeBleach.csproj -- sanitize "tests/fixtures/MiniIron" -l 2 -f -v`

## Remaining Leaks (TODO)

These are visible in MiniIron output and need fixing:

| Leak | Location | Example | Root Cause |
|------|----------|---------|------------|
| INSTALLATION/AUTHOR paragraph | COBOL ID DIVISION | `ACME INSURANCE COMPANY` | Lines classified as ParagraphHeader, fix is in ReplaceProcedureStatement |
| SQL table names | COBOL EXEC SQL | `S05201SA`, `S08708SA` | SQL delegation not replacing embedded SQL identifiers |
| SQL column names | COBOL EXEC SQL | `ACCT_CNTL_ID`, `CLM_STAT_CD` | Same - SQL delegation gap |
| JCL instream DSN commands | JCL `//SYSTSIN DD *` | `PROGRAM(AR1000)`, `PLAN(ARPLAN01)` | JCL instream handler not parsing DB2 DSN syntax |
| DB2 BIND identifiers | CARD DB2 BIND | `ACMECOL`, `ACMEBAT`, `MEMBER(AR1000)` | Db2Bind handler not catching PACKAGE/MEMBER/OWNER/QUALIFIER |
| SMTP body text | CARD SMTP | `Acme Insurance Company`, `ACME-CLM-01` | SMTP body handler not sanitizing free text |
| DL/I control identifiers | CARD DL/I | `PROGRAM(AR1000)`, `PLAN(ARPLAN01)` | DlIControl handler does minimal processing |
| PROCEDURE DIVISION USING | COBOL | `LS-CLAIM-KEY LS-RETURN-CODE` | Linkage identifiers after USING not replaced |
| Mangled qualified host vars | COBOL EXEC SQL | `:DCLSCLM-DSN_6-PMT-ID` | Partial alias collision in compound identifiers |
