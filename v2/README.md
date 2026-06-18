# V2 Analysis Workflow

This folder contains a second analysis workflow for `Data/Data-ext-65.csv`.

## Structure

- `scripts2/`: v2 R scripts.
- `outputs2/tables/`: v2 CSV outputs.
- `outputs2/plots/`: v2 plot outputs.

## Run

From the repository root:

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "v2\scripts2\99_run_all.R"
```

`Rscript` is not currently on the PowerShell PATH in this environment, so the full executable path is used.

## Data Quality Gate

The workflow runs `scripts2/00_data_quality_check.R` before cleaning or plotting. It writes:

- `outputs2/tables/data_quality_ext65.csv`
- `outputs2/tables/data_quality_ext65_summary.csv`

The current dataset runs, but the quality gate reports that `Data-ext-65.csv` has 62 rows instead of the expected 65. The missing papers are:

- `Malakhov2014.pdf`
- `Fujii2015.pdf`
- `Woorons2017.pdf`

## Meta-Analysis Note

`Data-ext-65.csv` contains narrative effect fields but not the old intervention/control mean-SD fields required for Hedges' g calculation. The v2 meta-analysis script therefore writes exclusion and empty subgroup summary tables until group-level numeric extraction fields are added.
