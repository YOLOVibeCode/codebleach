using CodeBleach.Core.Interfaces;

namespace CodeBleach.Core.Services;

/// <summary>
/// Registry for language processors. Selects the best processor for a file
/// based on extension, priority, and content heuristics.
/// </summary>
public sealed class LanguageProcessorRegistry : ILanguageProcessorRegistry
{
    private readonly List<ILanguageProcessor> _processors = [];
    private readonly Dictionary<string, List<ILanguageProcessor>> _byExtension = new(StringComparer.OrdinalIgnoreCase);

    public void Register(ILanguageProcessor processor)
    {
        _processors.Add(processor);

        foreach (var ext in processor.SupportedExtensions)
        {
            if (!_byExtension.TryGetValue(ext, out var list))
            {
                list = [];
                _byExtension[ext] = list;
            }
            list.Add(processor);
            // Keep sorted by priority (lower = higher priority)
            list.Sort((a, b) => a.Priority.CompareTo(b.Priority));
        }
    }

    public ILanguageProcessor? GetProcessor(string filePath, string? content = null)
    {
        var extension = Path.GetExtension(filePath);
        if (string.IsNullOrEmpty(extension))
        {
            return null;
        }

        if (!_byExtension.TryGetValue(extension, out var candidates))
        {
            return null;
        }

        // If content is available, use CanProcess for disambiguation
        if (content != null)
        {
            foreach (var processor in candidates)
            {
                if (processor.CanProcess(filePath, content))
                {
                    return processor;
                }
            }
        }

        // Fall back to highest-priority processor for this extension
        return candidates.Count > 0 ? candidates[0] : null;
    }

    public IReadOnlyList<ILanguageProcessor> GetAll() => _processors.AsReadOnly();
}
