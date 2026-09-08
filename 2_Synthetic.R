#   Construction of the synthetic population base used in the microsimulation.

#========================
# MICS DATASET
#========================

TD21SynCoreData <- function(old_wd){
  # ---- Directories (from options, or fallback to working directory) ----
  lpmic15 <- paste0(old_wd,"/data/MICS/MICS 5 (2015)/Belize_MICS5_Datasets")  # input folder with .dta files
  message("TD21SynCoreData: lpmic15=", normalizePath(lpmic15))
  
  # All tables
  #Households - hh.sav 
  hh <- haven::read_sav(paste0(lpmic15,"/hh.sav"))
  #Household members - hl.sav
  hl <- haven::read_sav(paste0(lpmic15,"/hl.sav"))
  #Women in reproductive age (15-49 years of age) – wm.sav
  wm <- haven::read_sav(paste0(lpmic15,"/wm.sav"))
  #Birth history - bh.sav 
  bh <- haven::read_sav(paste0(lpmic15,"/bh.sav"))
  #Mothers or primary caretakers of children under the age of five – ch.sav
  ch <- haven::read_sav(paste0(lpmic15,"/ch.sav"))
  #Men (15-59 years of age) – mn.sav
  mn <- haven::read_sav(paste0(lpmic15,"/mn.sav"))
  
  # Unified individual datasets
  # Basically, males is the same but add the prefix "M" 
  # to each variable name
  
  ## 1. Build a rename map for all overlapping variables ----
  # women's variable names are the "canonical" names
  wm_names <- names(wm)
  wm_names
  # what those names look like in the men's file (according to the questionnaires)
  mn_prefixed <- paste0("M", wm_names)
  mn_prefixed
  # keep only those that actually exist in mn
  mn_has <- intersect(names(mn), mn_prefixed)
  mn_has
  # corresponding target names (strip the initial "M")
  target_names <- sub("^M", "", mn_has)
  target_names
  
  # CORRECT direction: new_name = old_name
  # e.g. c("WM1" = "MWM1", "WB2" = "MWB2", "TA3" = "MTA3", ...)
  rename_vec <- setNames(mn_has, target_names)
  
  mn2 <- mn %>%
    rename(!!!rename_vec) %>%   # apply ALL mappings
    mutate(CSex = 1) #male
  
  wm2 <- wm %>%
    mutate(CSex = 2) #female
  
  all_individuals <- bind_rows(mn2, wm2) %>%
    mutate(weights = ifelse(is.na(mnweight),wmweight,mnweight))
  rm(mn2,wm2)
  
  # Add HH-level key variables
  all_individuals <- all_individuals %>% left_join(hh, by = c("HH1", "HH2"))
  
  # Extract variables
  ISynthetic <- all_individuals %>% 
    rename(
      vid  = WM1,
      hid  = WM2,
      iid  = WM4,
      HFex = hhweight,
      CFex = weights) %>%
    mutate(
      CAge = WB2,
      CAgeGroup = case_when(
        is.na(CAge) ~ NA_real_,
        CAge <= 4 & CAge > 0   ~ 1,
        CAge <= 9 & CAge >= 5  ~ 2,
        CAge <= 14 & CAge >= 10 ~ 3,
        CAge <= 19 & CAge >= 15 ~ 4,
        CAge <= 24 & CAge >= 20 ~ 5,
        CAge <= 29 & CAge >= 25 ~ 6,
        CAge <= 34 & CAge >= 30 ~ 7,
        CAge <= 39 & CAge >= 35 ~ 8,
        CAge <= 44 & CAge >= 40 ~ 9,
        CAge <= 49 & CAge >= 45 ~ 10,
        CAge <= 54 & CAge >= 50 ~ 11,
        CAge <= 59 & CAge >= 55 ~ 12,
        CAge <= 64 & CAge >= 60 ~ 13,
        CAge <= 69 & CAge >= 65 ~ 14,
        CAge <= 74 & CAge >= 70 ~ 15,
        CAge <= 79 & CAge >= 75 ~ 16,
        CAge <= 84 & CAge >= 80 ~ 17,
        CAge <= 89 & CAge >= 85 ~ 18,
        CAge <= 94 & CAge >= 90 ~ 19,
        CAge <= 99 & CAge >= 95 ~ 20,
        CAge >= 100~ 21,
        TRUE ~ NA_real_
      ),
      CAgeGroup95 = case_when(
        is.na(CAge) ~ NA_real_,
        CAge <= 4 & CAge > 0   ~ 1,
        CAge <= 9 & CAge >= 5  ~ 2,
        CAge <= 14 & CAge >= 10 ~ 3,
        CAge <= 19 & CAge >= 15 ~ 4,
        CAge <= 24 & CAge >= 20 ~ 5,
        CAge <= 29 & CAge >= 25 ~ 6,
        CAge <= 34 & CAge >= 30 ~ 7,
        CAge <= 39 & CAge >= 35 ~ 8,
        CAge <= 44 & CAge >= 40 ~ 9,
        CAge <= 49 & CAge >= 45 ~ 10,
        CAge <= 54 & CAge >= 50 ~ 11,
        CAge <= 59 & CAge >= 55 ~ 12,
        CAge <= 64 & CAge >= 60 ~ 13,
        CAge <= 69 & CAge >= 65 ~ 14,
        CAge <= 74 & CAge >= 70 ~ 15,
        CAge <= 79 & CAge >= 75 ~ 16,
        CAge <= 84 & CAge >= 80 ~ 17,
        CAge <= 89 & CAge >= 85 ~ 18,
        CAge <= 94 & CAge >= 90 ~ 19,
        CAge >= 95 ~ 20,
        TRUE ~ NA_real_
      )
    ) %>%
    # Correct
    mutate(TA4_c = ifelse(TA4 == 99,NA_real_,TA4),
           TA5_c = ifelse((TA5 == 0|TA5==99|is.na(TA5))&!is.na(TA4_c)&TA4_c!=0,1,
                          ifelse(TA5 == 99,NA_real_,TA5))) %>%
    # Smoke
    mutate(
      SFumaSino = case_when(
        
        ## (1) Positive if ANY use in the last 30 days
        TA5_c > 0                ~ 1,
        TA7 == 1               ~ 1,
        TA11 == 1              ~ 1,
        
        ## (2) Explicit last-month NO for all products
        TA5_c == 0 & TA7 == 2 & TA11 == 2 ~ 0,
        
        ## (4) Only the 9's are missing
        TA1==9 | TA2==99 | TA3==9 | TA5_c==99 | TA6==9 | TA9==99 ~ NA_real_,
        
        ## (4) Everything else should be a zero
        TRUE ~ 0
      ),
      SFumaCuan = TA4_c,
      SFumaCuan = if_else(SFumaCuan == 99,NA_real_,as.numeric(SFumaCuan)),
      SFumaFrec = case_when(TA5_c == 30 ~ 1,
                            TA5_c < 30 & TA5_c > 4 ~ 2,
                            TA5_c <= 4 & TA5_c > 0 ~ 3,
                            is.na(TA5_c)|TA5_c == 99 ~ NA_real_),
    ) %>%
    mutate(
      SFumaSino = if_else(is.na(SFumaSino), 0, as.numeric(SFumaSino))
    ) %>%
    # People in household
    rename(
      RCanPerso = HH11
    )
  
  p <- ISynthetic %>%
    mutate(
      id = paste0(as.character(vid), "-", as.character(hid), " ", as.character(iid))
    )
  
  #========================
  # KEEP variables
  #========================
  vmicro <- c(
    "id", iid_vars,"CFex",
    "CAge","CAgeGroup","CAgeGroup80","CAgeGroup95","CAgeGroup_dec","CSex",
    grep("^SFuma", names(p), value = TRUE),
    "RCanPerso"
  )
  
  vmicro <- vmicro[vmicro %in% names(p)]
  p_out <- p %>% select(all_of(vmicro))
  
  # Corrections
  # Reescale weigths for reaching Belize population
  p_out <- p_out %>% filter(CFex != 0) %>%
    left_join(popBelize,by = c('CAgeGroup','CSex')) %>%
    rename(CFex_old = CFex) %>%
    group_by(CSex,CAgeGroup) %>% 
    mutate(Tot_CFex_old = sum(CFex_old)) %>% ungroup %>%
    mutate(CFex = CFex_old/Tot_CFex_old*Population) %>% select(-Population)
  
  # Compute weighted medians among smokers in-group (after adjustment)
  med_frec <- Hmisc::wtd.quantile(
    x = p_out %>% filter(SFumaSino == 1,!is.na(SFumaFrec)) %>% pull(SFumaFrec),
    weights = p_out %>% filter(SFumaSino == 1,!is.na(SFumaFrec)) %>% pull(CFex),
    probs = 0.5, na.rm = TRUE
  )
  
  med_cuan <- Hmisc::wtd.quantile(
    x = p_out %>% filter(SFumaSino == 1,!is.na(SFumaCuan),SFumaCuan >0) %>% pull(SFumaCuan),
    weights = p_out %>% filter(SFumaSino == 1,!is.na(SFumaCuan),SFumaCuan >0) %>% pull(CFex),
    probs = 0.5, na.rm = TRUE
  )
  
  # Assign median frequency/intensity to NA data but smoke
  p_out <- p_out %>%
    mutate(
      SFumaFrec = if_else(SFumaSino == 1 & is.na(SFumaFrec), as.numeric(med_frec), SFumaFrec),
      SFumaCuan = if_else(SFumaSino == 1 & (is.na(SFumaCuan)|SFumaCuan == 0), as.numeric(med_cuan), SFumaCuan)
    )
  return(p_out)
}

#========================
# SYNTHETIC DATASET
#========================

run_one_pop <- function(simu,par,lbecea) {
  t_start <- proc.time()
  # ---- Load synthetic datasets ----
  in_base <- file.path(lbecea, "synthetic_population")
  p <- readRDS(paste0(in_base, ".rds"))
  
  #===========================================================
  # Intensity
  #===========================================================
  
  SFumaSino_frec <- p %>% filter(!is.na(SFumaCuan),SFumaCuan != 0) %>% 
    group_by(CSex,SFumaFrec) %>% 
    summarise(num = sum(CFex*SFumaCuan),.groups = 'drop') %>% left_join(
      p %>% filter(!is.na(SFumaCuan),SFumaCuan != 0) %>% 
        group_by(CSex,SFumaFrec) %>% 
        summarise(denom = sum(CFex),.groups = 'drop')
    ) %>% mutate(cuan_imp = num/denom) %>% select(-num,-denom)
  
  p <- p %>% left_join(SFumaSino_frec,by = c('CSex','SFumaFrec')) %>%
    mutate(
      SFumaCuan = case_when(
        SFumaSino == 1&(is.na(SFumaCuan)|SFumaCuan == 0)~ cuan_imp,
        TRUE ~ SFumaCuan),
      SFumaCuan = par$adj_fac_illi*SFumaCuan
    ) %>% select(-cuan_imp)
  
  #===========================================================
  # Adjustment of number of smokers 15-19, 20-24
  #===========================================================
  
  # Check prevalence
  # with(p, sum(CFex * (SFumaSino == 1), na.rm = TRUE) / sum(CFex, na.rm = TRUE))
  
  # Computation of adj_fact
  p <- p %>%
    { AdjNumSmo(., 15, 19, 1, 2.783758) } %>%
    { AdjNumSmo(., 15, 19, 2, 21.859167) } %>%
    { AdjNumSmo(., 20, 24, 2, 1.088885) }
  
  # Check prevalence after adjust
  # with(p, sum(CFex * (SFumaSino == 1), na.rm = TRUE) / sum(CFex, na.rm = TRUE))
  
  #===========================================================
  # Population 50+
  #===========================================================
  # Smoke
  SFumaSino_49 <- p %>% filter(CAge %in% 45:49,SFumaSino == 1) %>% group_by(CSex) %>% 
    summarise(num = sum(CFex),.groups = 'drop') %>% left_join(
      p %>% filter(CAge %in% 45:49) %>% group_by(CSex) %>% 
        summarise(denom = sum(CFex),.groups = 'drop')
    ) %>% mutate(prop = num/denom) %>% select(-num,-denom)
  target_prev <- expand.grid(CAgeGroup95 = 1:20,CSex = c(1,2)) %>% 
    left_join(SFumaSino_49,by = c('CSex'))
  # Frequency
  SFumaFrec_49 <- p %>% filter(CAge %in% 45:49,SFumaSino == 1) %>% group_by(CSex,SFumaFrec) %>% 
    summarise(num = sum(CFex),.groups = 'drop') %>% left_join(
      p %>% filter(CAge %in% 45:49,SFumaSino == 1) %>% group_by(CSex) %>% 
        summarise(denom = sum(CFex),.groups = 'drop')
    ) %>% mutate(prop = num/denom) %>% select(-num,-denom)
  target_frec <- expand.grid(CSex = c(1,2)) %>% 
    left_join(SFumaFrec_49,by = c('CSex'))
  target_wide <- target_frec %>%
    mutate(SFumaFrec = as.integer(SFumaFrec)) %>%
    tidyr::pivot_wider(names_from = SFumaFrec, values_from = prop, names_prefix = "p") %>%
    mutate(across(starts_with("p"), ~replace_na(.x, 0)))
  # Quantity
  SFumaCuan_49 <- p %>% filter(CAge %in% 45:49,SFumaSino == 1) %>% group_by(CSex) %>%
    summarise(SFumaCuan49 = Hmisc::wtd.quantile(SFumaCuan,weights = CFex,probs = 0.5,na.rm = TRUE),
              .groups = 'drop')
  # Household
  hid_miss <- p %>% group_by_at(c(hid_vars,'RCanPerso')) %>% summarise(n = n(),.groups = 'drop') %>% 
    filter(n != RCanPerso) %>% select(vid,hid,RCanPerso)
  
  # Simulation
  n_total_micro <- 3500  # choose size; larger = smoother
  targets_pop <- pop50Belize %>%
    mutate(n_micro = pmax(1, round(N / sum(N) * n_total_micro)))
  p50 <- targets_pop %>% tidyr::uncount(n_micro) %>%
    group_by(CAge, CSex) %>%
    mutate(w_raw = rgamma(n(), shape = 2, scale = 1),
           CFex = w_raw / sum(w_raw) * first(N)) %>%
    ungroup() %>%
    select(-w_raw,-N) %>%
    mutate(CAgeGroup95 = case_when(
      is.na(CAge) ~ NA_real_,
      CAge <= 4 & CAge > 0   ~ 1,
      CAge <= 9 & CAge >= 5  ~ 2,
      CAge <= 14 & CAge >= 10 ~ 3,
      CAge <= 19 & CAge >= 15 ~ 4,
      CAge <= 24 & CAge >= 20 ~ 5,
      CAge <= 29 & CAge >= 25 ~ 6,
      CAge <= 34 & CAge >= 30 ~ 7,
      CAge <= 39 & CAge >= 35 ~ 8,
      CAge <= 44 & CAge >= 40 ~ 9,
      CAge <= 49 & CAge >= 45 ~ 10,
      CAge <= 54 & CAge >= 50 ~ 11,
      CAge <= 59 & CAge >= 55 ~ 12,
      CAge <= 64 & CAge >= 60 ~ 13,
      CAge <= 69 & CAge >= 65 ~ 14,
      CAge <= 74 & CAge >= 70 ~ 15,
      CAge <= 79 & CAge >= 75 ~ 16,
      CAge <= 84 & CAge >= 80 ~ 17,
      CAge <= 89 & CAge >= 85 ~ 18,
      CAge <= 94 & CAge >= 90 ~ 19,
      CAge >= 95 ~ 20,
      TRUE ~ NA_real_
    ))
  # Smoke
  p50 <- p50 %>%
    left_join(target_prev,by = c('CSex','CAgeGroup95')) %>%
    mutate(SFumaSino = 0) %>% group_by(CSex,CAgeGroup95) %>%
    mutate(RanU = runif(n())) %>% 
    arrange(desc(RanU)) %>%
    mutate(RanPop = cumsum(replace_na(CFex, 0))) %>% ungroup
  RanTot <- p50 %>% group_by(CSex,CAgeGroup95) %>% summarise(RanTot = sum(CFex),.groups = 'drop')
  p50 <- p50 %>% left_join(RanTot,by = c('CSex','CAgeGroup95')) %>%
    mutate(RanPro = if_else(RanTot > 0, RanPop / RanTot, NA_real_)) %>%
    mutate(SFumaSino = if_else(!is.na(RanPro) & RanPro <= prop, 1L, SFumaSino)) %>% 
    select(-RanU, -RanPop, -RanTot, -RanPro,-prop)
  # Those who Smoke - Frec and Cuan
  p50 <- p50 %>%
    left_join(target_wide,by = c('CSex')) %>%
    mutate(SFumaFrec = if_else(SFumaSino == 1, 0L, NA_integer_)) %>% 
    group_by(CSex) %>% 
    mutate(RanU = if_else(SFumaSino == 1, runif(n()), NA_real_)) %>% 
    arrange(desc(RanU)) %>%
    mutate(RanPop = if_else(SFumaSino == 1, cumsum(replace_na(CFex, 0)), NA_real_)) %>%
    ungroup
  RanTot_smok <- p50 %>% filter(SFumaSino == 1) %>% 
    group_by(CSex) %>% summarise(RanTot = sum(CFex),.groups = 'drop')
  p50 <- p50 %>% left_join(RanTot_smok,by = c('CSex')) %>%
    mutate(RanPro = if_else(SFumaSino == 1 & RanTot > 0, RanPop / RanTot, NA_real_)) %>%
    mutate(q1 = p1,
           q2 = (p1 + p2),
           q3 = (p1 + p2 + p3),
           SFumaFrec = case_when(
             SFumaSino == 1 & RanPro <= q1 ~ 1L,
             SFumaSino == 1 & RanPro <= q2 ~ 2L,
             SFumaSino == 1 & RanPro <= q3 ~ 3L,
             TRUE       ~ SFumaFrec
           )) %>% 
    select(-starts_with("p"),-starts_with("q"),-RanU, -RanPop, -RanTot, -RanPro)
  p50 <- p50 %>% left_join(SFumaCuan_49,by = 'CSex') %>%
    mutate(SFumaCuan = if_else(SFumaSino == 1,SFumaCuan49,NA_real_)) %>%
    select(-SFumaCuan49)
  # Households
  ## New households 
  # 1 person (20) # 2 people both 50+ (10-10)
  rantot <- p50 %>% summarise(RanTot = sum(CFex),.groups = 'drop') %>% pull(RanTot)
  p50 <- p50 %>% mutate(RanU = runif(n())) %>% 
    arrange(desc(RanU)) %>%
    mutate(RanPop = cumsum(replace_na(CFex, 0)),RanTot = rantot) %>% 
    mutate(RanPro = if_else(RanTot > 0, RanPop / RanTot, NA_real_)) %>%
    mutate(q1 = 0.2,
           q2 = 0.4,
           hid = case_when(
             RanPro <= q1 ~ 1000,
             RanPro <= q2 ~ 2000,
             TRUE       ~ NA
           )) %>% 
    select(-starts_with("q"),-RanU, -RanPop, -RanTot, -RanPro) %>%
    group_by(hid) %>%
    mutate(
      idx = row_number(),
      vid = case_when(
        hid == 1000 ~ idx,
        hid == 2000 ~ (idx + 1L) %/% 2L,   # 1,1,2,2,3,3,...
        TRUE ~ NA_integer_
      )
    ) %>%
    group_by(vid,hid) %>% mutate(iid = row_number()) %>% 
    mutate(RCanPerso = n()) %>% ungroup %>%
    select(-idx)
  ## Current households
  # >=2 people (60)
  p50 <- bind_rows(p50 %>% filter(is.na(hid)) %>% select(-vid,-hid,-iid,-RCanPerso) %>% 
                     bind_cols(hid_miss %>% mutate(RanU = runif(n())) %>%
                                 arrange(desc(RanU)) %>% 
                                 slice(1:nrow(p50 %>% filter(is.na(hid))))) %>%
                     mutate(iid = 1000),
                   p50 %>% filter(!is.na(hid))) %>%
    mutate(
    CAgeGroup = case_when(
      is.na(CAge) ~ NA_real_,
      CAge <= 4 & CAge > 0   ~ 1,
      CAge <= 9 & CAge >= 5  ~ 2,
      CAge <= 14 & CAge >= 10 ~ 3,
      CAge <= 19 & CAge >= 15 ~ 4,
      CAge <= 24 & CAge >= 20 ~ 5,
      CAge <= 29 & CAge >= 25 ~ 6,
      CAge <= 34 & CAge >= 30 ~ 7,
      CAge <= 39 & CAge >= 35 ~ 8,
      CAge <= 44 & CAge >= 40 ~ 9,
      CAge <= 49 & CAge >= 45 ~ 10,
      CAge <= 54 & CAge >= 50 ~ 11,
      CAge <= 59 & CAge >= 55 ~ 12,
      CAge <= 64 & CAge >= 60 ~ 13,
      CAge <= 69 & CAge >= 65 ~ 14,
      CAge <= 74 & CAge >= 70 ~ 15,
      CAge <= 79 & CAge >= 75 ~ 16,
      CAge <= 84 & CAge >= 80 ~ 17,
      CAge <= 89 & CAge >= 85 ~ 18,
      CAge <= 94 & CAge >= 90 ~ 19,
      CAge <= 99 & CAge >= 95 ~ 20,
      CAge >= 100~ 21,
      TRUE ~ NA_real_
    ))
  
  # Append
  p <- p %>% bind_rows(p50) %>%
    mutate(id = paste0(as.character(vid), "-", as.character(hid), " ", as.character(iid)))
  
  #===========================================================
  # Household with smokers
  #===========================================================
  
  p <- p %>%
    group_by(across(all_of(hid_vars))) %>%
    mutate(
      SFumaHSino = as.integer(sum(SFumaSino, na.rm = TRUE) > 0)
    ) %>%
    ungroup()

  # -----------------------------------------
  # Second-hand smoke pre-tax (your block)
  # -----------------------------------------
  
  p <- p %>%
    mutate(
      SShsPre = if_else(SFumaHSino == 1, 0L, as.integer(NA))
    )
  
  # Save
  in_base <- file.path(lbecea, paste0("RSynS", simu))
  saveRDS(p, paste0(in_base, ".rds"))
  
  # Time
  elapsed <- unname((proc.time() - t_start)["elapsed"])
  list(simu = simu, elapsed = elapsed)
}
