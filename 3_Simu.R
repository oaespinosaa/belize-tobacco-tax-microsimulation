# Consumption simulation
TD31SimConsu <- function(df,par){
  # ---- QUITTING (weighted random selection ) ----
  df <- df %>%
    mutate(CQuit = dplyr::if_else(SFumaSino == 1, 0L, NA_integer_)) %>%
    mutate(RanU = if_else(SFumaSino == 1, runif(n()), NA_real_))
  
  # Sort descending RanU (within the full df) and compute cumulative weighted share
  # only for eligible smokers (others keep NA for RanPro)
  df <- df %>% arrange(desc(RanU)) %>%
    mutate(RanPop = if_else(SFumaSino == 1,cumsum(replace_na(CFex, 0)),NA_real_))
  # Total weight among smokers
  RanTot_i <- df %>% filter(SFumaSino == 1) %>%
    summarise(tot = sum(CFex, na.rm = TRUE), .groups = "drop") %>% pull(tot)
  if (length(RanTot_i) == 0 || is.na(RanTot_i) || RanTot_i <= 0) {
    # No smokers : nothing to do
    df <- df %>% mutate(RanPro = NA_real_)
  } else {
    df <- df %>% mutate(RanPro = RanPop / RanTot_i)
    # Quitting rule: replace CQuit = 1 if RanPro_i <= CQuitProp
    df <- df %>%
      mutate(CQuit = if_else(!is.na(RanPro) & RanPro <= CQuitProp, 1L, CQuit)) %>%
      select(-RanU, -RanPop, -RanPro) %>%
      arrange(., across(all_of(iid_vars)))
  }
  
  # ---- REDUCTION IN SMOKING INTENSITY  ----
  df <- df %>%
    mutate(SFumaCuanPos = if_else(CQuit == 0L, SFumaCuan * (1 + par$CIntenProp), NA_real_))
  return(df)  
}

#   Health outcomes simulation.
#   Applies smoking-related risk reductions and life-years gains to the
#   simulated population, producing health outcome indicators.
TD32SimHealth <- function(df,par){
  # 1) DEATHS PRE TAX (smokers)
  df <- df %>%
    mutate(HDiePreTax = if_else(SFumaSino == 1, 0L, NA_integer_)) %>% 
    arrange(iid) %>% 
    mutate(RanU = if_else(SFumaSino == 1, runif(n()), NA_real_)) %>% 
    arrange(desc(RanU)) %>%
    mutate(RanPop = if_else(SFumaSino == 1, cumsum(replace_na(CFex, 0)), NA_real_)) %>%
    ungroup
  RanTot_smok <- df %>% summarise(RanTot = sum(CFex[SFumaSino == 1], na.rm = TRUE),.groups = 'drop') %>%
    pull(RanTot)
  df <- df %>% 
    mutate(RanTot = if_else(SFumaSino == 1, RanTot_smok, NA_real_)) %>% 
    mutate(RanPro = if_else(SFumaSino == 1 & RanTot > 0, RanPop / RanTot, NA_real_)) %>%
    mutate(HDiePreTax = if_else(!is.na(RanPro) & RanPro <= par$HDeathTob, 1L, HDiePreTax)) %>% 
    select(-RanU, -RanPop, -RanTot, -RanPro) %>% 
    arrange(iid)
  
  # 2) DEATHS FROM SECOND-HAND SMOKE (SHS) PRE-TAX
  df <- df %>%
    group_by_at(hid_vars) %>%
    mutate(SFumaSmoHh = sum(SFumaSino, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(SmoNonUni = if_else(SFumaSino == 1 & RCanPerso < 2, 0L, NA_integer_)) %>%
    mutate(SmoNonUni = if_else(SFumaSino == 1 & RCanPerso >= 2 & SFumaSmoHh < RCanPerso, 1L, SmoNonUni))
  
  TSmoNonUni <- sum(df$CFex[df$SmoNonUni == 1], na.rm = TRUE)
  TDeShsPre <- TSmoNonUni / 56.1
  
  df <- df %>%
    group_by_at(hid_vars) %>%
    mutate(SExpoShs0 = sum(SmoNonUni, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(SExpoShs1 = if_else(!is.na(SExpoShs0) & SExpoShs0 > 0 & !is.na(SFumaSino) & SFumaSino == 0, 1L, NA_integer_)) %>%
    mutate(RanU = if_else(SExpoShs1 == 1, runif(n()), NA_real_)) %>% 
    arrange(desc(RanU)) %>%
    mutate(RanPop = if_else(SExpoShs1 == 1, cumsum(replace_na(CFex, 0)), NA_real_)) %>%
    mutate(HDiePreShs = if_else(SExpoShs1 == 1, 0L, NA_integer_)) %>%
    mutate(HDiePreShs = if_else(!is.na(RanPop) & RanPop <= TDeShsPre, 1L, HDiePreShs)) %>%
    select(-SFumaSmoHh, -SmoNonUni, -SExpoShs0, -SExpoShs1, -RanPop, -RanU) %>% 
    arrange(iid)
  
  # 3) DEATHS POST-TAX (quitters & risk reduction)
  df <- df %>% left_join(HRiskRedu, by = "CAgeGroup") %>% 
    mutate(HRiskRedu = if_else(SFumaSino == 1, HRiskRedu, NA_real_)) %>% 
    mutate(HQuitDie = if_else(CQuit == 1, 0L, NA_integer_))
  
  # For age group
  for (k in 1:21) {
    
    df <- df %>% arrange(iid) %>%
      mutate(RanU = if_else(CQuit == 1 & CAgeGroup == k, runif(n()), NA_real_)) %>% 
      arrange(desc(RanU)) %>%
      mutate(RanPop = if_else(CQuit == 1 & CAgeGroup == k,
                              cumsum(if_else(CQuit == 1 & CAgeGroup == k, replace_na(CFex, 0), 0)),
                              NA_real_))
    
    RanTot_k <- df %>%
      summarise(x = sum(CFex[CQuit == 1 & CAgeGroup == k], na.rm = TRUE)) %>%
      pull(x)
    
    df <- df %>%
      mutate(RanTot = if_else(CQuit == 1 & CAgeGroup == k, RanTot_k, NA_real_)) %>%
      mutate(RanPro = if_else(CQuit == 1 & CAgeGroup == k & RanTot > 0, RanPop / RanTot, NA_real_)) %>%
      mutate(HQuitDie = if_else(CQuit == 1 & CAgeGroup == k & HDiePreTax == 1 &
                                  !is.na(RanPro) & RanPro <= (1 - HRiskRedu),
                                1L, HQuitDie)) %>% select(-RanU, -RanPop, -RanTot, -RanPro) %>% 
      arrange(iid)
  }
  
  # 4) NON-SMOKERS SHS POST-TAX + deaths averted
  df <- df %>%
    mutate(HQuitDieShs = if_else(HDiePreShs == 1, 1L, NA_integer_)) %>%
    group_by_at(hid_vars) %>%
    mutate(TSmoHh = sum(SFumaSino == 1 & CQuit == 0, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(HQuitDieShs = if_else(HQuitDieShs == 1 & TSmoHh == 0, 0L, HQuitDieShs)) %>%
    mutate(HDeathShsAver = if_else(HDiePreShs == 1, 0L, NA_integer_),
           HDeathShsPosTax = if_else(HDiePreShs == 1, 0L, NA_integer_)) %>%
    mutate(HDeathShsAver = if_else(HDiePreShs == 1 & HQuitDieShs == 0, 1L, HDeathShsAver),
           HDeathShsPosTax = if_else(HDiePreShs == 1 & HQuitDieShs == 1, 1L, HDeathShsPosTax))
  
  # 5) LIFE YEARS GAINED
  df <- df %>% left_join(HLiYeGainCubSpli, by = "CAge") %>% 
    mutate(HAgeLiYe = if_else(SFumaSino == 1, HAgeLiYe, NA_real_)) %>% 
    mutate(HLiYeGainPosTaxQuit = if_else(CQuit == 1, HAgeLiYe, NA_real_))
  
  # 6) DEATHS AVERTED (smokers)
  df <- df %>% mutate(HDiePosTax = if_else(!is.na(HDiePreTax), 0L, NA_integer_)) %>%
    mutate(HDiePosTax = if_else(HQuitDie == 1 | (CQuit == 0 & HDiePreTax == 1), 1L, HDiePosTax)) %>% 
    mutate(HDeathAver = if_else(HDiePreTax == 1, 0L, NA_integer_)) %>%
    mutate(HDeathAver = if_else(HDiePreTax == 1 & HDiePosTax == 0, 1L, HDeathAver))
  #(is.na(HQuitDie) | HQuitDie == 0) 
  df <- df %>% mutate(Nal = 1L) %>% arrange(across(all_of(iid_vars)))
  return(df)
}

# Function that runs one replicate
run_one_sim <- function(s,par,lbecea) {
  message("---- Simulation replicate s = ", s, " ----")
  # Load database
  in_base <- file.path(lbecea, paste0("RSynS", s))
  df <- readRDS(paste0(in_base, ".rds"))
  # Quit smoking
  df <- df %>%
    mutate(
      CQuitProp = if_else(CAgeGroup <= 5, par$CQuitProp * par$CQuitYouth, par$CQuitProp)
    )
  # Simulation consumption, health and costs
  out31 <- TD31SimConsu(df,par)
  message("TD31SimConsu: finished.")
  p_1 <- TD32SimHealth(out31,par)
  message("TD32SimHealth: finished.")
  # Save
  out_base <- file.path(lbecea, paste0("SSimS",s))
  saveRDS(p_1, paste0(out_base, ".rds"))
}
