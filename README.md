# BIOS 731 – Homework 2 Simulation Study

This repository implements a full-factorial simulation study comparing Wald, percentile bootstrap, and bootstrap-t confidence intervals under varying sample sizes, treatment effects, and error distributions.

## Project structure

- `code/`  
  Core simulation functions and scripts used to generate Monte Carlo replicates.

- `analysis/`  
  Post-processing scripts that aggregate simulation outputs and compute summary metrics.

- `data/` *(ignored via .gitignore)*  
  Intermediate simulation outputs saved as `.rds` files. These files are generated during the simulation and are not tracked in version control.

- `HW_simulation-2.qmd`  
  Quarto document that loads summarized results, produces figures and tables, and contains the written analysis.

## Workflow

1. Run simulation scenarios (parallelized across scenarios and replicates):
   - Source `code/sim_functions.R`
   - Execute `code/run_rep.R` using array jobs on a computing cluster
   - Each replicate is saved as an `.rds` file under `data`

2. Aggregate simulation results:
   - Run `analysis/summarize_sim.R` to combine replicate-level outputs into scenario-level summaries
   - Summary results are saved to `out/summary/`

3. Generate figures and tables:
   - Render `HW_simulation-2.qmd`, which reads from `out/summary/` and produces all reported results

## Reproducibility

All results are fully reproducible by rerunning the simulation and aggregation scripts. Intermediate simulation outputs are excluded from version control to keep the repository lightweight.
