using CodeBleach.Core.Models;
using CodeBleach.Core.Services;

namespace CodeBleach.Tests.Services;

public class FileReferencePatcherJavaScriptTests : IDisposable
{
    private readonly string _tempDir;

    public FileReferencePatcherJavaScriptTests()
    {
        _tempDir = Path.Combine(Path.GetTempPath(), $"codebleach_js_{Guid.NewGuid():N}");
        Directory.CreateDirectory(_tempDir);
    }

    public void Dispose()
    {
        if (Directory.Exists(_tempDir))
        {
            Directory.Delete(_tempDir, recursive: true);
        }
    }

    private void CreateFile(string relativePath, string content)
    {
        var fullPath = Path.Combine(_tempDir, relativePath.Replace('/', Path.DirectorySeparatorChar));
        var dir = Path.GetDirectoryName(fullPath);
        if (dir != null) Directory.CreateDirectory(dir);
        File.WriteAllText(fullPath, content);
    }

    private string ReadFile(string relativePath)
    {
        return File.ReadAllText(Path.Combine(_tempDir, relativePath.Replace('/', Path.DirectorySeparatorChar)));
    }

    [Fact]
    public async Task PatchJsImports_ImportFromStatement_UpdatesPath()
    {
        CreateFile("src/app.js", "import { helper } from './utils/helper';");
        CreateFile("src/utils/helper.js", "export const helper = () => {};");

        var mappings = new MappingTable();
        mappings.FilePathForward["src/app.js"] = "src/FILE_0.js";
        mappings.FilePathForward["src/utils/helper.js"] = "src/DIR_0/FILE_1.js";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("src/app.js");
        patched.Should().Contain("DIR_0/FILE_1");
        patched.Should().NotContain("utils/helper");
    }

    [Fact]
    public async Task PatchJsImports_RequireStatement_UpdatesPath()
    {
        CreateFile("src/index.js", "const helper = require('./utils/helper');");
        CreateFile("src/utils/helper.js", "module.exports = {};");

        var mappings = new MappingTable();
        mappings.FilePathForward["src/index.js"] = "src/FILE_0.js";
        mappings.FilePathForward["src/utils/helper.js"] = "src/DIR_0/FILE_1.js";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("src/index.js");
        patched.Should().Contain("DIR_0/FILE_1");
        patched.Should().NotContain("utils/helper");
    }

    [Fact]
    public async Task PatchJsImports_DynamicImport_UpdatesPath()
    {
        CreateFile("src/app.js", "const lazy = import('./modules/lazy');");
        CreateFile("src/modules/lazy.js", "export default {};");

        var mappings = new MappingTable();
        mappings.FilePathForward["src/app.js"] = "src/FILE_0.js";
        mappings.FilePathForward["src/modules/lazy.js"] = "src/DIR_0/FILE_1.js";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("src/app.js");
        patched.Should().Contain("DIR_0/FILE_1");
        patched.Should().NotContain("modules/lazy");
    }

    [Fact]
    public async Task PatchJsImports_ExportFromStatement_UpdatesPath()
    {
        CreateFile("src/index.js", "export { foo } from '../shared/types';");
        CreateFile("shared/types.js", "export const foo = 42;");

        var mappings = new MappingTable();
        mappings.FilePathForward["src/index.js"] = "src/FILE_0.js";
        mappings.FilePathForward["shared/types.js"] = "DIR_0/FILE_1.js";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("src/index.js");
        patched.Should().Contain("FILE_1");
        patched.Should().NotContain("shared/types");
    }

    [Fact]
    public async Task PatchJsImports_NpmPackageImport_LeftUnchanged()
    {
        CreateFile("src/app.js", "import React from 'react';\nimport { useState } from 'react';");

        var mappings = new MappingTable();
        mappings.FilePathForward["src/app.js"] = "src/FILE_0.js";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        // File was processed but no relative imports found → no changes
        count.Should().Be(0);
        var patched = ReadFile("src/app.js");
        patched.Should().Contain("'react'");
    }

    [Fact]
    public async Task PatchJsImports_ExtensionOmitted_ResolvesAndPatches()
    {
        // Import without .js extension → should resolve via extension probing
        CreateFile("src/app.js", "import { helper } from './helper';");
        CreateFile("src/helper.js", "export const helper = () => {};");

        var mappings = new MappingTable();
        mappings.FilePathForward["src/app.js"] = "src/FILE_0.js";
        mappings.FilePathForward["src/helper.js"] = "src/FILE_1.js";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("src/app.js");
        // Extension should be omitted in output (matching original convention)
        patched.Should().Contain("'./FILE_1'");
    }

    [Fact]
    public async Task PatchJsImports_ExtensionIncluded_PatchesWithExtension()
    {
        CreateFile("src/app.ts", "import { helper } from './helper.ts';");
        CreateFile("src/helper.ts", "export const helper = () => {};");

        var mappings = new MappingTable();
        mappings.FilePathForward["src/app.ts"] = "src/FILE_0.ts";
        mappings.FilePathForward["src/helper.ts"] = "src/FILE_1.ts";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("src/app.ts");
        // Extension included in output since original had it
        patched.Should().Contain("./FILE_1.ts");
    }

    [Fact]
    public async Task PatchJsImports_DirectoryImport_ResolvesIndexFile()
    {
        CreateFile("src/app.js", "import { Button } from './components';");
        CreateFile("src/components/index.js", "export const Button = {};");

        var mappings = new MappingTable();
        mappings.FilePathForward["src/app.js"] = "src/FILE_0.js";
        mappings.FilePathForward["src/components/index.js"] = "src/DIR_0/FILE_1.js";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("src/app.js");
        // Directory import resolved to DIR_0 directory
        patched.Should().Contain("DIR_0");
        patched.Should().NotContain("components");
    }

    [Fact]
    public async Task PatchJsImports_ParentTraversal_RecomputesCorrectly()
    {
        CreateFile("src/features/list.js", "import { utils } from '../../shared/utils';");
        CreateFile("shared/utils.js", "export const utils = {};");

        var mappings = new MappingTable();
        mappings.FilePathForward["src/features/list.js"] = "src/DIR_0/FILE_0.js";
        mappings.FilePathForward["shared/utils.js"] = "DIR_1/FILE_1.js";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("src/features/list.js");
        // From src/DIR_0/ to DIR_1/ requires going up to root: ../../DIR_1/FILE_1
        patched.Should().Contain("DIR_1/FILE_1");
        patched.Should().NotContain("shared/utils");
    }

    [Fact]
    public async Task PatchJsImports_TypeScriptFile_Patched()
    {
        CreateFile("src/app.ts", "import { service } from './service';");
        CreateFile("src/service.ts", "export class service {}");

        var mappings = new MappingTable();
        mappings.FilePathForward["src/app.ts"] = "src/FILE_0.ts";
        mappings.FilePathForward["src/service.ts"] = "src/FILE_1.ts";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("src/app.ts");
        patched.Should().Contain("FILE_1");
    }

    [Fact]
    public async Task PatchJsImports_MjsFile_Patched()
    {
        CreateFile("src/app.mjs", "import { helper } from './helper.mjs';");
        CreateFile("src/helper.mjs", "export const helper = () => {};");

        var mappings = new MappingTable();
        mappings.FilePathForward["src/app.mjs"] = "src/FILE_0.mjs";
        mappings.FilePathForward["src/helper.mjs"] = "src/FILE_1.mjs";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("src/app.mjs");
        patched.Should().Contain("FILE_1.mjs");
    }

    [Fact]
    public async Task PatchJsImports_MultipleImportsInOneFile_AllPatched()
    {
        var jsContent = @"import { a } from './moduleA';
const b = require('./moduleB');
const c = import('./moduleC');
";
        CreateFile("src/app.js", jsContent);
        CreateFile("src/moduleA.js", "export const a = 1;");
        CreateFile("src/moduleB.js", "module.exports = {};");
        CreateFile("src/moduleC.js", "export default {};");

        var mappings = new MappingTable();
        mappings.FilePathForward["src/app.js"] = "src/FILE_0.js";
        mappings.FilePathForward["src/moduleA.js"] = "src/FILE_1.js";
        mappings.FilePathForward["src/moduleB.js"] = "src/FILE_2.js";
        mappings.FilePathForward["src/moduleC.js"] = "src/FILE_3.js";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("src/app.js");
        patched.Should().Contain("FILE_1");
        patched.Should().Contain("FILE_2");
        patched.Should().Contain("FILE_3");
        patched.Should().NotContain("moduleA");
        patched.Should().NotContain("moduleB");
        patched.Should().NotContain("moduleC");
    }

    [Fact]
    public async Task PatchJsImports_SingleAndDoubleQuotes_BothPatched()
    {
        var jsContent = "import { a } from \"./foo\";\nimport { b } from './bar';";
        CreateFile("src/app.js", jsContent);
        CreateFile("src/foo.js", "export const a = 1;");
        CreateFile("src/bar.js", "export const b = 2;");

        var mappings = new MappingTable();
        mappings.FilePathForward["src/app.js"] = "src/FILE_0.js";
        mappings.FilePathForward["src/foo.js"] = "src/FILE_1.js";
        mappings.FilePathForward["src/bar.js"] = "src/FILE_2.js";

        var patcher = new FileReferencePatcher();
        var count = await patcher.PatchReferencesAsync(_tempDir, mappings);

        count.Should().Be(1);
        var patched = ReadFile("src/app.js");
        patched.Should().Contain("FILE_1");
        patched.Should().Contain("FILE_2");
        patched.Should().NotContain("foo");
        patched.Should().NotContain("bar");
    }
}
