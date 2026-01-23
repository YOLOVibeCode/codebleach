# Deep Nested Test Fixture

This fixture tests CodeBleach's ability to process deeply nested directory structures.

## Structure

```
deep-nested/
└── level1/
    └── level2/
        └── level3/
            └── level4/
                └── level5/
                    └── level6/
                        └── level7/
                            └── level8/
                                └── level9/
                                    └── level10/
                                        ├── config.json
                                        └── README.md
```

## Sensitive Data Locations

| Level | File | Contains |
|-------|------|----------|
| 10 | config.json | PRODSRV01, DBZMEW, 10.50.100.50, LINKEDSRV01 |
| 10 | README.md | Server names, SQL references |
| 1 | README.md | Overview only |

## Expected Behavior

CodeBleach should:
1. Traverse all 10 levels
2. Process files at every level
3. Create consistent mappings across all files
4. Maintain directory structure in output
