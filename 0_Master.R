# Libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(parallel)
})

# Remove variables in enviroment
rm(list = ls());gc()

# Set working directory and load functions
old_wd <- "/Belize Tobacco"
lco    <- paste0(old_wd,"/scripts")
message("Project folder (lco): ", lco)
setwd(lco)
source("1_Inputs.R")
source("2_Synthetic.R")
source("3_Simu.R")
source("4_Outputs.R")

# Function to extract the final table according to the parameters
tobacco_taxes <- function(porc_tax,porc_illicit,elasticity,porc_market,
                          Sim = TRUE,NSim = 200,parallel_sims = TRUE,ncores = 8L){
  
  lbecea  <- paste0(old_wd,"/lbecea_",porc_tax*100,'_',porc_illicit*100,'_',abs(elasticity),'_',porc_market*100)
  if (!dir.exists(lbecea)) dir.create(lbecea, recursive = TRUE)
  par <- fn_par(porc_tax,porc_illicit,elasticity,porc_market)
  
  saveRDS(HRiskRedu,file.path(lbecea, "HRiskRedu.rds"))
  saveRDS(HLiYeGain,file.path(lbecea, "HLiYeGain.rds"))
  saveRDS(HLiYeGainCubSpli,file.path(lbecea, "HLiYeGainCubSpli.rds"))
  
  # CORE DATASET: MICS
  syn_base_path <- file.path(lbecea, "synthetic_population.rds")
  if (!file.exists(syn_base_path)) {
    message("NOTE: synthetic_population.rds not found in lbecea.")
    p_out <- TD21SynCoreData(old_wd)
    saveRDS(p_out, file.path(lbecea, "synthetic_population.rds"))
  } else {
    message("synthetic_population.rds found in lbecea")
  }
  
  sim_ids <- seq.int(1,NSim)
  if(Sim){
    message("Simulations s = ", 1, " ... ", NSim)
    message("ncores = ", ncores, " | parallel = ", parallel_sims)
    
    if (!parallel_sims) {
      times <- lapply(sim_ids, run_one_pop, par = par,lbecea = lbecea)
      message("2_Synthetic: completed all replicates.")
      times <- lapply(sim_ids, run_one_sim, par = par,lbecea = lbecea)
      message("3_Simu: completed all replicates.")
    } else {
      n_use <- min(ncores, length(sim_ids))
      cl <- makeCluster(n_use)
      on.exit(stopCluster(cl), add = TRUE)
      
      # Ensure workers have needed functions + base packages
      wd <- getwd()
      clusterExport(cl, varlist = c("iid_vars","hid_vars",
                                    "AdjNumSmo","wd","lbecea","par",
                                    "pop50Belize","HLiYeGainCubSpli","HRiskRedu",
                                    "run_one_pop","run_one_sim","packs_factor",
                                    "TD31SimConsu","TD32SimHealth"),
                    envir = environment())
      
      # Important: set worker working directory so relative paths in source() work
      clusterEvalQ(cl, setwd(wd))
      clusterEvalQ(cl, {suppressPackageStartupMessages({
        library(tidyverse)
        library(readxl)})})
      
      times <- parLapply(cl, sim_ids, run_one_pop, par = par,lbecea = lbecea)
      message("2_Synthetic: completed all replicates.")
      times <- parLapply(cl, sim_ids, run_one_sim, par = par,lbecea = lbecea)
      message("3_Simu: completed all replicates.")
    }
  }
  
  TDd <- Reduce(bind_rows,lapply(sim_ids, run_one_res, par = par,lbecea = lbecea))
  message("4_Outputs: completed all Outputs.")
  
  TDd <- TDd %>% mutate(TRevPosSpe = CPosTaxCigTot * par$STaxVaSpePos) %>%
    mutate(prevPreTax = CPreTaxSmok/CPopulation,
           prevPosTax = CPosTaxSmok/CPopulation)
  saveRDS(TDd, file.path(lbecea, paste0("TD_Nal.rds")))
  writexl::write_xlsx(x = TDd,path = file.path(lbecea, "TD_Nal.xlsx"))
  message("Export: File saved.")
  invisible(TRUE)
}

# Execute function
# porc_illicit <- 0.13
for (e in c(-0.3785,-0.77)){
for (t in seq(0.3,0.7,0.05)){
  for (m in c(0.045,0.02,0)){
porc_tax <- t
elasticity <- e
porc_market <- m
tobacco_taxes(porc_tax,porc_illicit,elasticity,porc_market,TRUE,200,TRUE,8L)
print(paste(e,t,m))
}
}
}
# tobacco_taxes(0.5,0.13,-0.3785)
# tobacco_taxes(0.65,0.13,-0.3785)
# tobacco_taxes(0.5,0.16,-0.3785)
# tobacco_taxes(0.5,0.11,-0.3785)
# tobacco_taxes(0.65,0.16,-0.3785)
# tobacco_taxes(0.65,0.11,-0.3785)