# Tobacco Tax Microsimulation for Belize — README

Documentation for the R script pipeline that estimates the effect of tobacco tax reforms in Belize on consumption, mortality from non-communicable diseases (NCDs), second-hand smoke, illicit trade, and tax revenue, through Monte Carlo microsimulation over a synthetic population built from the 2015 MICS5 survey.

## Software and versions

- **Language:** R 4.5.2. 
- Tidyverse follows the policy of supporting the 5 most recent R releases.

## Execution order

`0_Master.R` sources `1_Inputs.R` → `2_Synthetic.R` → `3_Simu.R` → `4_Outputs.R` and runs every scenario; **those four are not meant to be run on their own**. `5_Figures.R` **run separately**, as standalone script, after `0_Master.R` finishes.

1. **Edit the project path** — in `0_Master.R` (line 12), change `old_wd <- "/Belize Tobacco"` to the local path.
2. **Install the packages** tidyverse, readxl, parallel.
3. **Place the input data** exactly where the scripts expect it.
4. **Run `0_Master.R` in full.** This loads the libraries, sources the 4 scripts, defines `tobacco_taxes()`, and runs the nested loop over elasticities, with Monte Carlo replicates parallelized. This is computationally intensive; for testing, reduce `NSim` and/or the elasticity/tax/market vectors before running the full pipeline.
6. **Run `5_Figures.R` separately**, in the same R session (it reuses `old_wd`, `tidyverse`, and `scales` already loaded), once every `lbecea_*/` folder with its `TD_Nal.rds` exists. This script builds `tab_sum` (one summary row per scenario) and produces exploratory charts across the scenarios.

### Inputs
#### Parameters (in `0_Master.R`)

| Parameter | Values in the script | Meaning |
|---|---|---|
| `porc_tax` | `seq(0.30, 0.70, 0.05)` | % increase targeted in the final retail price |
| `elasticity` | `c(-0.3785, -0.77)` | Price elasticity of cigarette demand |
| `porc_market` | `c(0.045, 0.02, 0)` | Shift toward the illicit market per unit increase in the specific tax |
| `porc_illicit` | `0.13` | Baseline illicit-trade rate (pre-reform) |
| `NSim` | `200` | Monte Carlo replicates per scenario |
| `parallel_sims` | `TRUE` | Whether replicates run in parallel |
| `ncores` | `8L` | Cores used by `parallel::makeCluster()` |

Each output folder's name (`` lbecea_<porc_tax*100>_<porc_illicit*100>_<abs(elasticity)>_<porc_market*100> ``) encodes these four parameters. `5_Figures.R` rebuilds that same name **independently**.

#### Structural parameters (`fn_par()` function in `1_Inputs.R`)

| Parameter | Value / formula | Description |
|---|---|---|
| `CQuitYouth` | 2.0 | Extensive-margin (quitting) multiplier for `CAgeGroup` ≤ 5 (ages 0-24) | 
| `ECopayInsu` | 0.67 | Share of the population with health insurance | 
| `ECopayReim` | 1.0 | Insurance reimbursement rate | 
| `ECost1`–`ECost4` | 5,701.16 / 8,924.35 / 2,529.76 / 64,275.37 (US$/person/year) | Annual cost of heart disease, stroke, COPD, and cancer, respectively | 
| `HDeathTob` | 0.5 | Share of smoker deaths attributable to tobacco |
| `HShareCanc/Copd/Hear/Stro` | 6.8% / 18.5% / 52.3% / 22.4% | Share of each NCD among deaths averted by quitting | 
| `HShsShareCanc/Copd/Hear/Stro` | 8.3% / 41.3% / 39.6% / 10.8% | Same, for deaths averted from second-hand smoke | 
| `SCigaImpo` | `69560/(1000*20)*1e6` | Cigarette imports, in packs of 20 | 
| `SExwManCoPre` | `0.10*1.2713` | Manufacturing cost | 
| `SExwMinProRaPre` | 0.2 | Manufacturing profit rate | 
| `SIlliTraPre` | `porc_illicit` (scenario parameter) | Pre-reform illicit trade | 
| `SImpaElasExtensive` / `SImpaElasIntensive` | 0.5 / -0.5 | Split of the elasticity between the extensive and intensive margins | 
| `SPriDisMaPre` / `SPriRetMaPre` | 0.15 / 0.15 | Distribution and retail margins | 
| `SPriElas` | `elasticity` (scenario parameter) | Price elasticity of demand | 
| `STaxPassSpePre` | 1.2 | Pass-through of the specific excise to price |
| `STaxRaAdvPre` | 0 | Ad valorem rate | 
| `STaxRaVatPre` | 0.125 | VAT/GST rate | 
| `STaxVaSpePre` | `7.5*0.1733` | Specific excise value | 
| `STaxVaVatPre` | `7.5*0.125` | VAT value |

The pipeline depends on two external sources, with different access conditions.

| Expected file/folder | Used in | Redistributable? |
|---|---|---|
| `data/MICS/MICS 5 (2015)/Belize_MICS5_Datasets/{hh,hl,wm,bh,ch,mn}.sav` | `2_Synthetic.R` (`TD21SynCoreData()`) | **No** |
| `data/MidYear_Estimates_AgeGroup_Sex_2015-2025.xlsx` | `1_Inputs.R` (`popBelize`, `pop50Belize`) | Yes (aggregate, open access) |

## Note

- The `lbecea_*` folders are regenerable results.

