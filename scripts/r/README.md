# Macrobase COMAP parser

Run from the repository root:

```sh
Rscript scripts/r/parse_macrobase.R
```

The script recursively searches `data/` for two Excel workbooks:

- a filename containing `presentad`
- a filename containing `recomend`

It treats Macrobase Presentados as the registry of presented projects, then enriches those rows with Macrobase Recomendados using:

1. `rut + codigo_comap + tramo`, when `tramo` exists on both sides
2. `rut + codigo_comap`, only when that recommended key is unique

The generated files are written to `data/processed/`:

- `macrobase_presentados_normalized.csv`
- `macrobase_recomendados_normalized.csv`
- `comap_projects_authoritative.csv`
- `summary_by_ministerio.csv`
- `linkage_audit.csv`

`comap_projects_authoritative.csv` includes both original and benefit-specific amount fields:

- `monto_ui_presentado`
- `monto_ui_recomendado`
- `monto_ui_beneficios`

Rows are marked with `has_benefits` when a `fecha_recomendado` was linked or came from a recommendation-only row. `es_ampliacion` is inferred from the `Tramo` column.
