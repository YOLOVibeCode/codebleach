using System.Collections.Frozen;
using System.Text;
using System.Text.RegularExpressions;
using CodeBleach.Core.Interfaces;
using CodeBleach.Core.Models;

namespace CodeBleach.Processors.MainframeUtility;

/// <summary>
/// Catch-all language processor for mainframe utility control cards (CARD files).
/// Handles DB2 BIND, IDCAMS, DFSORT, FTP, SMTP, MFS, and generic parameter cards.
/// Registered with lower priority than COBOL and JCL so it only claims extensionless
/// files those processors rejected.
/// </summary>
public sealed class MainframeUtilityLanguageProcessor : ILanguageProcessor
{
    public string ProcessorId => "mainframe-utility";
    public string DisplayName => "Mainframe Utility Cards";
    public IReadOnlySet<string> SupportedExtensions { get; } = FrozenSet<string>.Empty;
    public int Priority => 200; // Lower priority = runs after COBOL (100) and JCL (100)

    // ── FTP commands used for detection ──────────────────────────────
    private static readonly FrozenSet<string> FtpCommands = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
    {
        "cd", "get", "put", "ascii", "binary", "quit", "bye", "mget", "mput",
        "lcd", "delete", "mkdir", "rmdir", "rename", "ls", "dir", "pwd", "open",
        "close", "disconnect", "type", "prompt", "hash", "bell", "glob"
    }.ToFrozenSet(StringComparer.OrdinalIgnoreCase);

    private static readonly Regex EmailPattern = new(
        @"[\w.+-]+@[\w.-]+\.\w{2,}",
        RegexOptions.Compiled);

    private static readonly Regex DatasetInQuotesPattern = new(
        @"'([A-Z][A-Z0-9]{0,7}(?:\.[A-Z0-9@#$]{1,8})+(?:\([^)]+\))?)'",
        RegexOptions.Compiled);

    private static readonly Regex DatasetBarePattern = new(
        @"\b([A-Z]{2,8}\.[A-Z][A-Z0-9]{1,7}(?:\.[A-Z0-9]{1,8}){0,5})\b",
        RegexOptions.Compiled);

    private static readonly Regex Db2ProgramPattern = new(
        @"(?:PROGRAM|PLAN)\s*\(\s*(\w+)\s*\)",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private static readonly Regex Db2SystemPattern = new(
        @"(?:SYSTEM|S)\s*\(\s*(\w+)\s*\)",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private static readonly Regex Db2LibraryPattern = new(
        @"(?:LIBRARY|LIB)\s*\(\s*'([^']+)'\s*\)",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private static readonly Regex IdcamsNamePattern = new(
        @"NAME\s*\(\s*([A-Z][A-Z0-9.]+)\s*\)",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private static readonly Regex IdcamsVolumesPattern = new(
        @"VOLUMES?\s*\(\s*(\w+)\s*\)",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private static readonly Regex MfsFieldPattern = new(
        @"MFLD\s+\(\s*(\w+)",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private static readonly Regex MfsSorPattern = new(
        @"SOR\s*=\s*\(\s*(\w+)",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private static readonly Regex MfsStringPattern = new(
        @"'([^']{2,})'",
        RegexOptions.Compiled);

    // ─────────────────────────────────────────────────────────────────

    public bool CanProcess(string filePath, string content)
    {
        if (string.IsNullOrWhiteSpace(content))
            return false;

        var extension = Path.GetExtension(filePath);
        if (!string.IsNullOrEmpty(extension))
            return false; // Only handle extensionless files

        // If content looks like binary, skip
        if (content.Any(c => c < 0x09 || (c > 0x0D && c < 0x20 && c != 0x1A)))
            return false;

        return DetectCardType(content) != null;
    }

    public LanguageProcessingResult Obfuscate(string content, ObfuscationContext context, string? filePath = null)
    {
        var cardType = DetectCardType(content);
        if (cardType == null)
        {
            return new LanguageProcessingResult
            {
                Content = content,
                WasTransformed = false,
                ReplacementCount = 0,
                ProcessorId = ProcessorId
            };
        }

        var (result, count) = cardType.Value switch
        {
            CardType.Db2Bind => ObfuscateDb2Bind(content, context, filePath),
            CardType.Idcams => ObfuscateIdcams(content, context, filePath),
            CardType.Sort => ObfuscateSort(content, context, filePath),
            CardType.Ftp => ObfuscateFtp(content, context, filePath),
            CardType.Smtp => ObfuscateSmtp(content, context, filePath),
            CardType.Mfs => ObfuscateMfs(content, context, filePath),
            CardType.DlIControl => (content, 0), // Leave as-is, not sensitive
            CardType.Parameter => ObfuscateParameter(content, context, filePath),
            _ => (content, 0)
        };

        return new LanguageProcessingResult
        {
            Content = result,
            WasTransformed = count > 0,
            ReplacementCount = count,
            ProcessorId = ProcessorId
        };
    }

    public LanguageProcessingResult Deobfuscate(string content, ObfuscationContext context, string? filePath = null)
    {
        // Reverse all known aliases in content
        var result = content;
        var count = 0;
        foreach (var (alias, original) in context.Mappings.Reverse)
        {
            var before = result;
            result = result.Replace(alias, original, StringComparison.Ordinal);
            if (result != before) count++;
        }

        return new LanguageProcessingResult
        {
            Content = result,
            WasTransformed = count > 0,
            ReplacementCount = count,
            ProcessorId = ProcessorId
        };
    }

    public ValidationResult Validate(string obfuscatedContent)
    {
        return ValidationResult.Valid();
    }

    // ── Card type detection ──────────────────────────────────────────

    internal static CardType? DetectCardType(string content)
    {
        var upper = content.ToUpperInvariant();

        // DB2 BIND: DSN SYSTEM( or DSN S( with RUN/BIND/END
        if ((upper.Contains("DSN SYSTEM(") || upper.Contains("DSN S(")) &&
            (upper.Contains("RUN ") || upper.Contains("BIND ") || upper.Contains("END")))
            return CardType.Db2Bind;

        // SMTP: HELO or MAIL FROM: or RCPT TO:
        if (upper.Contains("HELO ") || upper.Contains("MAIL FROM:") || upper.Contains("RCPT TO:"))
            return CardType.Smtp;

        // MFS/IMS: MSG TYPE= or MFLD or DFLD with macro syntax
        if ((upper.Contains("MSG TYPE=") || upper.Contains("MSG  TYPE=")) &&
            (upper.Contains("MFLD") || upper.Contains("DFLD")))
            return CardType.Mfs;

        // IDCAMS: DELETE/DEFINE/REPRO/LISTCAT with cluster/dataset patterns
        if (upper.Contains("DEFINE") && (upper.Contains("CLUSTER") || upper.Contains("ALIAS")))
            return CardType.Idcams;
        if (upper.TrimStart().StartsWith("DELETE", StringComparison.Ordinal) &&
            (upper.Contains(".") || upper.Contains("PURGE")))
            return CardType.Idcams;
        if (upper.TrimStart().StartsWith("REPRO", StringComparison.Ordinal) &&
            (upper.Contains("INFILE") || upper.Contains("INDATASET")))
            return CardType.Idcams;

        // DFSORT/SYNCSORT: SORT FIELDS= or MERGE FIELDS= or INCLUDE COND=
        if (upper.Contains("SORT FIELDS=") || upper.Contains("MERGE FIELDS=") ||
            upper.Contains("INCLUDE COND=") || upper.Contains("OUTREC"))
            return CardType.Sort;

        // FTP: Detect by looking at line structure - hostname on line 1, FTP commands following
        if (LooksLikeFtp(content))
            return CardType.Ftp;

        // IMS/DL1 control
        if (upper.Contains("CONTROL  CNTL") || upper.Contains("OPTION   PRINT"))
            return CardType.DlIControl;

        // Parameter: catch-all for short files with non-binary content
        var lineCount = content.Split('\n').Length;
        if (lineCount <= 50)
            return CardType.Parameter;

        return CardType.Parameter; // Still catch-all for longer files
    }

    private static bool LooksLikeFtp(string content)
    {
        var lines = content.Split('\n', StringSplitOptions.RemoveEmptyEntries);
        if (lines.Length < 3) return false;

        // FTP scripts: line 1 = hostname (or _FTP_xxx), followed by FTP commands
        var ftpCommandCount = 0;
        var hasLocsite = false;

        for (var i = 0; i < lines.Length; i++)
        {
            var trimmed = lines[i].Trim().TrimEnd('\r');
            var firstWord = trimmed.Split(' ', 2)[0];

            if (FtpCommands.Contains(firstWord))
                ftpCommandCount++;
            if (trimmed.StartsWith("LOCSITE", StringComparison.OrdinalIgnoreCase))
                hasLocsite = true;
        }

        return ftpCommandCount >= 2 || hasLocsite;
    }

    // ── Sub-type handlers ────────────────────────────────────────────

    private (string Content, int Count) ObfuscateDb2Bind(string content, ObfuscationContext context, string? filePath)
    {
        var result = content;
        var count = 0;

        // Replace PROGRAM(xxx) and PLAN(xxx)
        result = Db2ProgramPattern.Replace(result, m =>
        {
            var name = m.Groups[1].Value;
            var alias = context.GetOrCreateAlias(name, SemanticCategory.Program, filePath);
            count++;
            return m.Value.Replace(name, alias);
        });

        // Replace SYSTEM(xxx) or S(xxx)
        result = Db2SystemPattern.Replace(result, m =>
        {
            var name = m.Groups[1].Value;
            var alias = context.GetOrCreateAlias(name, SemanticCategory.Db2System, filePath);
            count++;
            return m.Value.Replace(name, alias);
        });

        // Replace LIBRARY('xxx') or LIB('xxx')
        result = Db2LibraryPattern.Replace(result, m =>
        {
            var dsn = m.Groups[1].Value;
            var alias = context.GetOrCreateAlias(dsn, SemanticCategory.MvsDataset, filePath);
            count++;
            return m.Value.Replace(dsn, alias);
        });

        return (result, count);
    }

    private (string Content, int Count) ObfuscateIdcams(string content, ObfuscationContext context, string? filePath)
    {
        var result = content;
        var count = 0;

        // Replace NAME(dataset.name) patterns
        result = IdcamsNamePattern.Replace(result, m =>
        {
            var dsn = m.Groups[1].Value;
            var alias = context.GetOrCreateAlias(dsn, SemanticCategory.MvsDataset, filePath);
            count++;
            return m.Value.Replace(dsn, alias);
        });

        // Replace VOLUMES(xxx)
        result = IdcamsVolumesPattern.Replace(result, m =>
        {
            var vol = m.Groups[1].Value;
            var alias = context.GetOrCreateAlias(vol, SemanticCategory.VolumeSerial, filePath);
            count++;
            return m.Value.Replace(vol, alias);
        });

        // Replace bare dataset names on standalone DELETE lines
        var lines = result.Split('\n');
        var sb = new StringBuilder();
        for (var i = 0; i < lines.Length; i++)
        {
            var line = lines[i];
            var trimmed = line.TrimStart().TrimEnd('\r', ' ');

            // Bare dataset on a line by itself (continuation of DELETE)
            if (!trimmed.StartsWith("*", StringComparison.Ordinal) &&
                !trimmed.Contains('(') &&
                DatasetBarePattern.IsMatch(trimmed))
            {
                line = DatasetBarePattern.Replace(line, dm =>
                {
                    var dsn = dm.Groups[1].Value;
                    var alias = context.GetOrCreateAlias(dsn, SemanticCategory.MvsDataset, filePath);
                    count++;
                    return alias;
                });
            }

            sb.Append(line);
            if (i < lines.Length - 1) sb.Append('\n');
        }
        result = sb.ToString();

        // Replace quoted dataset names
        result = DatasetInQuotesPattern.Replace(result, m =>
        {
            // Only replace if it looks like a real dataset (has dots)
            var dsn = m.Groups[1].Value;
            if (!dsn.Contains('.')) return m.Value;
            var alias = context.GetOrCreateAlias(dsn, SemanticCategory.MvsDataset, filePath);
            count++;
            return $"'{alias}'";
        });

        return (result, count);
    }

    /// <summary>
    /// Pattern matching SORT/MERGE field definitions with trailing inline comments.
    /// Captures: field-spec (number,number,type,order) then optional comma, then spaces, then comment text.
    /// </summary>
    private static readonly Regex SortFieldCommentPattern = new(
        @"(\d+\s*,\s*\d+\s*,\s*[A-Z]{2}\s*,\s*[A-Z]\s*[,)]?\s{2,})(\S.+?)(\s*)$",
        RegexOptions.Compiled | RegexOptions.IgnoreCase);

    private (string Content, int Count) ObfuscateSort(string content, ObfuscationContext context, string? filePath)
    {
        // Sort cards: strip comment lines, sanitize inline comments after field specs
        var lines = content.Split('\n');
        var sb = new StringBuilder();
        var count = 0;

        for (var i = 0; i < lines.Length; i++)
        {
            var line = lines[i];
            var trimmed = line.TrimStart();

            if (trimmed.StartsWith("*", StringComparison.Ordinal))
            {
                // Replace comment content with generic comment
                sb.Append("*     [Comment removed]");
                count++;
            }
            else
            {
                // Check for inline comments after sort field definitions
                var fieldMatch = SortFieldCommentPattern.Match(line);
                if (fieldMatch.Success)
                {
                    var commentText = fieldMatch.Groups[2].Value.Trim();
                    if (commentText.Length >= 3 && commentText.Any(char.IsLetter))
                    {
                        var alias = context.GetOrCreateAlias(commentText, SemanticCategory.StringLiteral, filePath);
                        sb.Append(line[..fieldMatch.Groups[2].Index]);
                        sb.Append(alias);
                        sb.Append(fieldMatch.Groups[3].Value);
                        count++;
                    }
                    else
                    {
                        sb.Append(line);
                    }
                }
                else
                {
                    sb.Append(line);
                }
            }

            if (i < lines.Length - 1) sb.Append('\n');
        }

        return (sb.ToString(), count);
    }

    private (string Content, int Count) ObfuscateFtp(string content, ObfuscationContext context, string? filePath)
    {
        var lines = content.Split('\n');
        var sb = new StringBuilder();
        var count = 0;

        for (var i = 0; i < lines.Length; i++)
        {
            var line = lines[i];
            var trimmed = line.Trim().TrimEnd('\r');

            if (i == 0)
            {
                // Line 1: hostname or FTP target
                var alias = context.GetOrCreateAlias(trimmed, SemanticCategory.Hostname, filePath);
                sb.Append(alias);
                count++;
            }
            else if (i == 1 && !string.IsNullOrWhiteSpace(trimmed) &&
                     !trimmed.Contains(' ') && !FtpCommands.Contains(trimmed.Split(' ')[0]))
            {
                // Line 2: likely password (single token, no spaces, not an FTP command)
                var alias = context.GetOrCreateAlias(trimmed, SemanticCategory.FtpCredential, filePath);
                sb.Append(alias);
                count++;
            }
            else if (trimmed.StartsWith("cd ", StringComparison.OrdinalIgnoreCase))
            {
                // cd /path/to/directory
                var path = trimmed[3..].Trim();
                var alias = context.GetOrCreateAlias(path, SemanticCategory.UnixPath, filePath);
                sb.Append("cd ").Append(alias);
                count++;
            }
            else if (trimmed.StartsWith("get ", StringComparison.OrdinalIgnoreCase) ||
                     trimmed.StartsWith("put ", StringComparison.OrdinalIgnoreCase))
            {
                // GET/PUT remoteFile 'dataset' or GET remoteFile
                var cmd = trimmed[..3];
                var args = trimmed[4..].Trim();
                var parts = SplitFtpArgs(args);
                var sbLine = new StringBuilder(cmd).Append(' ');

                foreach (var part in parts)
                {
                    if (part.StartsWith("'", StringComparison.Ordinal) && part.EndsWith("'", StringComparison.Ordinal))
                    {
                        var dsn = part[1..^1];
                        var alias = context.GetOrCreateAlias(dsn, SemanticCategory.MvsDataset, filePath);
                        sbLine.Append('\'').Append(alias).Append('\'');
                        count++;
                    }
                    else
                    {
                        var alias = context.GetOrCreateAlias(part, SemanticCategory.SourceFile, filePath);
                        sbLine.Append(alias);
                        count++;
                    }
                    sbLine.Append(' ');
                }
                sb.Append(sbLine.ToString().TrimEnd());
            }
            else if (trimmed.StartsWith("LOCSITE", StringComparison.OrdinalIgnoreCase))
            {
                // LOCSITE commands: leave as-is (configuration, not sensitive)
                sb.Append(line.TrimEnd('\r'));
            }
            else
            {
                // Other FTP commands (ascii, binary, quit, etc.): leave as-is
                sb.Append(line.TrimEnd('\r'));
            }

            if (i < lines.Length - 1) sb.Append('\n');
        }

        return (sb.ToString(), count);
    }

    private static List<string> SplitFtpArgs(string args)
    {
        var parts = new List<string>();
        var inQuote = false;
        var current = new StringBuilder();

        foreach (var c in args)
        {
            if (c == '\'' && !inQuote)
            {
                inQuote = true;
                current.Append(c);
            }
            else if (c == '\'' && inQuote)
            {
                inQuote = false;
                current.Append(c);
                parts.Add(current.ToString());
                current.Clear();
            }
            else if (c == ' ' && !inQuote)
            {
                if (current.Length > 0)
                {
                    parts.Add(current.ToString());
                    current.Clear();
                }
            }
            else
            {
                current.Append(c);
            }
        }
        if (current.Length > 0) parts.Add(current.ToString());
        return parts;
    }

    private (string Content, int Count) ObfuscateSmtp(string content, ObfuscationContext context, string? filePath)
    {
        var lines = content.Split('\n');
        var sb = new StringBuilder();
        var count = 0;
        var inBody = false;

        for (var i = 0; i < lines.Length; i++)
        {
            var line = lines[i];
            var trimmed = line.Trim().TrimEnd('\r');

            if (trimmed.StartsWith("HELO ", StringComparison.OrdinalIgnoreCase))
            {
                var server = trimmed[5..].Trim();
                var alias = context.GetOrCreateAlias(server, SemanticCategory.Hostname, filePath);
                sb.Append("HELO ").Append(alias);
                count++;
            }
            else if (trimmed.StartsWith("MAIL FROM:", StringComparison.OrdinalIgnoreCase) ||
                     trimmed.StartsWith("RCPT TO:", StringComparison.OrdinalIgnoreCase))
            {
                // Replace email addresses
                var replaced = EmailPattern.Replace(line.TrimEnd('\r'), m =>
                {
                    var email = m.Value;
                    var alias = context.GetOrCreateAlias(email, SemanticCategory.Email, filePath);
                    count++;
                    return alias;
                });
                sb.Append(replaced);
            }
            else if (trimmed.StartsWith("Subject:", StringComparison.OrdinalIgnoreCase))
            {
                var subjectContent = trimmed[8..].Trim();
                var alias = context.GetOrCreateAlias(subjectContent, SemanticCategory.StringLiteral, filePath);
                sb.Append("Subject: ").Append(alias);
                count++;
            }
            else if (trimmed.Equals("DATA", StringComparison.OrdinalIgnoreCase))
            {
                sb.Append(trimmed);
                inBody = true;
            }
            else if (inBody)
            {
                // In email body: replace emails, sanitize comment-style lines, replace identifiers
                var replaced = line.TrimEnd('\r');

                // Replace email addresses in body
                replaced = EmailPattern.Replace(replaced, m =>
                {
                    var email = m.Value;
                    var alias = context.GetOrCreateAlias(email, SemanticCategory.Email, filePath);
                    count++;
                    return alias;
                });

                // Replace TO: header line email addresses
                if (trimmed.StartsWith("TO:", StringComparison.OrdinalIgnoreCase))
                {
                    // Already handled by EmailPattern above
                }

                // Replace dataset names in body
                replaced = DatasetBarePattern.Replace(replaced, m =>
                {
                    var dsn = m.Groups[1].Value;
                    if (!dsn.Contains('.')) return m.Value;
                    var alias = context.GetOrCreateAlias(dsn, SemanticCategory.MvsDataset, filePath);
                    count++;
                    return alias;
                });

                // Strip comment-style body lines preserving structure
                if (trimmed.StartsWith("*", StringComparison.Ordinal) && trimmed.EndsWith("*", StringComparison.Ordinal))
                {
                    // Sanitize the content between the asterisks
                    var inner = trimmed[1..^1].Trim();
                    if (!string.IsNullOrWhiteSpace(inner))
                    {
                        var innerAlias = context.GetOrCreateAlias(inner, SemanticCategory.StringLiteral, filePath);
                        replaced = $"* {innerAlias} *";
                        count++;
                    }
                }

                sb.Append(replaced);
            }
            else
            {
                sb.Append(line.TrimEnd('\r'));
            }

            if (i < lines.Length - 1) sb.Append('\n');
        }

        return (sb.ToString(), count);
    }

    private (string Content, int Count) ObfuscateMfs(string content, ObfuscationContext context, string? filePath)
    {
        var result = content;
        var count = 0;

        // Replace SOR=(mapname,...) transaction/map references
        result = MfsSorPattern.Replace(result, m =>
        {
            var name = m.Groups[1].Value;
            var alias = context.GetOrCreateAlias(name, SemanticCategory.MfsMap, filePath);
            count++;
            return m.Value.Replace(name, alias);
        });

        // Replace MFLD (fieldname,...) field references
        result = MfsFieldPattern.Replace(result, m =>
        {
            var name = m.Groups[1].Value;
            // Skip if it looks like a system-level label (single char like SCA)
            if (name.Length <= 3) return m.Value;
            var alias = context.GetOrCreateAlias(name, SemanticCategory.Variable, filePath);
            count++;
            return m.Value.Replace(name, alias);
        });

        // Replace string literals in DFLD and MFLD
        result = MfsStringPattern.Replace(result, m =>
        {
            var str = m.Groups[1].Value;
            // Skip very short strings or purely numeric ones
            if (str.Length < 3 || str.All(c => char.IsDigit(c) || c == ' ')) return m.Value;
            var alias = context.GetOrCreateAlias(str, SemanticCategory.ScreenLabel, filePath);
            count++;
            return $"'{alias}'";
        });

        // Strip comment lines
        var lines = result.Split('\n');
        var sb = new StringBuilder();
        for (var i = 0; i < lines.Length; i++)
        {
            var trimmed = lines[i].TrimStart();
            if (trimmed.StartsWith("*", StringComparison.Ordinal))
            {
                sb.Append("*     [Comment removed]");
                count++;
            }
            else
            {
                sb.Append(lines[i].TrimEnd('\r'));
            }
            if (i < lines.Length - 1) sb.Append('\n');
        }

        return (sb.ToString(), count);
    }

    private (string Content, int Count) ObfuscateParameter(string content, ObfuscationContext context, string? filePath)
    {
        var lines = content.Split('\n');
        var sb = new StringBuilder();
        var count = 0;

        for (var i = 0; i < lines.Length; i++)
        {
            var line = lines[i];
            var trimmed = line.Trim().TrimEnd('\r');

            // Strip comment lines
            if (trimmed.StartsWith("*", StringComparison.Ordinal))
            {
                sb.Append("*     [Comment removed]");
                count++;
            }
            else if (string.IsNullOrWhiteSpace(trimmed))
            {
                sb.Append(line.TrimEnd('\r'));
            }
            else
            {
                // Replace each significant token on the line
                var (replaced, tokenCount) = ReplaceTokens(trimmed, context, filePath);
                count += tokenCount;
                sb.Append(replaced);
            }

            if (i < lines.Length - 1) sb.Append('\n');
        }

        return (sb.ToString(), count);
    }

    private static (string Result, int Count) ReplaceTokens(string line, ObfuscationContext context, string? filePath)
    {
        var localCount = 0;

        // Replace email addresses first
        var result = EmailPattern.Replace(line, m =>
        {
            var email = m.Value;
            var alias = context.GetOrCreateAlias(email, SemanticCategory.Email, filePath);
            localCount++;
            return alias;
        });

        // Replace quoted dataset names
        result = DatasetInQuotesPattern.Replace(result, m =>
        {
            var dsn = m.Groups[1].Value;
            if (!dsn.Contains('.')) return m.Value;
            var alias = context.GetOrCreateAlias(dsn, SemanticCategory.MvsDataset, filePath);
            localCount++;
            return $"'{alias}'";
        });

        // Replace bare dataset names
        result = DatasetBarePattern.Replace(result, m =>
        {
            var dsn = m.Groups[1].Value;
            if (!dsn.Contains('.')) return m.Value;
            var alias = context.GetOrCreateAlias(dsn, SemanticCategory.MvsDataset, filePath);
            localCount++;
            return alias;
        });

        // For remaining multi-character alphabetic tokens that don't look like syntax,
        // check if they match any already-known alias in the context
        result = Regex.Replace(result, @"\b([A-Za-z][\w-]{2,})\b", m =>
        {
            var token = m.Groups[1].Value;
            var upper = token.ToUpperInvariant();

            // If this token is already an alias, leave it
            if (context.Mappings.Reverse.ContainsKey(upper) ||
                context.Mappings.Reverse.ContainsKey(token))
                return m.Value;

            // If this token has an existing alias from COBOL/JCL processing, reuse it
            if (context.Mappings.Forward.TryGetValue(upper, out var existingAlias) ||
                context.Mappings.Forward.TryGetValue(token, out existingAlias))
                return existingAlias;

            // For standalone parameter files, replace significant tokens
            // Skip very short tokens or purely numeric suffixed ones
            if (token.Length < 4) return m.Value;

            var alias = context.GetOrCreateAlias(upper, SemanticCategory.StringLiteral, filePath);
            localCount++;
            return alias;
        });

        return (result, localCount);
    }
}
