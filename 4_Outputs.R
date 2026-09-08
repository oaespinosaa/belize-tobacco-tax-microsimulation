# Consolidate simulation outputs across replicates
run_one_res <- function(s,par,lbecea) {
  message("SIMULACION S = ", s)
  p <- readRDS(file = file.path(lbecea, paste0("SSimS", s,".rds")))
  # Population
  CPopulation <- p %>% group_by(Nal) %>%
    summarise(CPopulation = sum(CFex, na.rm = TRUE),.groups = "drop")
  # CONSUMPTION – NUMBER OF SMOKERS
  #Smokers Pre-Tax
  CPreTaxSmok <- p %>% filter(SFumaSino == 1) %>% group_by(Nal) %>%
    summarise(CPreTaxSmok = sum(CFex, na.rm = TRUE),.groups = "drop")
  #Smokers Post-Tax
  CPosTaxSmok <- p %>% filter(SFumaSino == 1, CQuit == 0) %>% group_by(Nal) %>%
    summarise(CPosTaxSmok = sum(CFex, na.rm = TRUE),.groups = "drop")
  
  # SMOKING INTENSITY (weighted median)
  #Pre-Tax
  CPreInteMedian <- p %>% filter(SFumaSino == 1) %>% group_by(Nal) %>%
    summarise(CPreInteMedian = Hmisc::wtd.quantile(SFumaCuan,weights = CFex,probs = 0.5,na.rm = TRUE),
              .groups = "drop")
  
  #Post-Tax
  CPosInteMedian <- tryCatch(
    p %>% filter(SFumaSino == 1, CQuit == 0) %>% group_by(Nal) %>%
      summarise(CPosInteMedian = Hmisc::wtd.quantile(SFumaCuanPos,weights = CFex,probs = 0.5,na.rm = TRUE),
        .groups = "drop")
    ,error = function(e){
      p %>% group_by(Nal) %>% summarise(CPosInteMedian = 0,.groups = 'drop')})
  # Cigarettes smoked (20-Sticks packs)
  #Pre-Tax
  CPreTaxCigSmo <- p %>% mutate(Cig = CFex * SFumaCuan * packs_factor) %>%
    group_by(Nal) %>%
    summarise(CPreTaxCigSmo = sum(Cig, na.rm = TRUE),.groups = "drop")
  #Post-Tax
  CPosTaxCigSmo <- p %>% filter(SFumaSino == 1, CQuit == 0) %>%
    mutate(Cig = CFex * SFumaCuanPos * packs_factor) %>%
    group_by(Nal) %>%
    summarise(CPosTaxCigSmo = sum(Cig, na.rm = TRUE),.groups = "drop")
  
  # REDUCTIONS (intensive & extensive margin)
  #Intensive margin
  CPrePosCigSmoInDif <- p %>% filter(SFumaSino == 1, CQuit == 0) %>%
    mutate(Cig = CFex * (SFumaCuan - SFumaCuanPos) * packs_factor) %>%
    group_by(Nal) %>%
    summarise(CPrePosCigSmoInDif = sum(Cig, na.rm = TRUE),.groups = "drop")
  
  #Extensive margin
  CPrePosCigSmoExDif <- p %>% filter(SFumaSino == 1, CQuit == 1) %>%
    mutate(Cig = CFex * SFumaCuan * packs_factor) %>%
    group_by(Nal) %>%
    summarise(CPrePosCigSmoExDif = sum(Cig, na.rm = TRUE),.groups = "drop")
  
  # TOTAL PACKS PAYING TAXES (imports)
  #Pre-Tax
  CPreTaxCigTot <- tibble(Nal = 1L,CPreTaxCigTot = par$SCigaImpo)
  #Post-Tax
  CPosTaxCigTot <- p %>%
    mutate(
      CigPos = if_else(SFumaSino == 1 & CQuit == 0,
                       CFex * SFumaCuanPos * packs_factor,
                       0)
    ) %>%
    group_by(Nal) %>%
    summarise(
      TCigPos = sum(CigPos, na.rm = TRUE),
      CPosTaxCigTot = TCigPos*(1 - par$SIlliTraPos),.groups = "drop") %>%
    select(Nal,CPosTaxCigTot)
  
  # ILLICIT TRADE
  CPreCigIllTra <- p %>%
    mutate(
      TIlli = CFex * SFumaCuan * packs_factor * par$SIlliTraPre
    ) %>%
    group_by(Nal) %>%
    summarise(CPreCigIllTra = sum(TIlli, na.rm = TRUE),.groups = "drop")
  CPosCigIllTra <- p %>%
    mutate(
      TIlli = if_else(SFumaSino == 1 & CQuit == 0,
                      CFex * SFumaCuanPos * packs_factor * par$SIlliTraPos,
                      NA_real_)
    ) %>%
    group_by(Nal) %>%
    summarise(CPosCigIllTra = sum(TIlli, na.rm = TRUE),.groups = "drop")
  
  #========================
  # Deaths of smokers Pre-Tax
  HDeathsPreTax <- p %>%
    filter(HDiePreTax == 1) %>%
    group_by(Nal) %>%
    summarise(
      HDeathsPreTax = sum(CFex, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(HDeathsPreTaxCanc = HDeathsPreTax*par$HShareCanc,
           HDeathsPreTaxHear = HDeathsPreTax*par$HShareHear,
           HDeathsPreTaxStro = HDeathsPreTax*par$HShareStro,
           HDeathsPreTaxCopd = HDeathsPreTax*par$HShareCopd)
  
  # Deaths of non-smokers Pre-Tax due to SHS
  HDeathPreShs <- p %>%
    filter(HDiePreShs == 1) %>%
    group_by(Nal) %>%
    summarise(
      HDeathPreShs = sum(CFex, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(HCancShsDeaPreTax = HDeathPreShs*par$HShsShareCanc,
           HHearShsDeaPreTax = HDeathPreShs*par$HShsShareHear,
           HStroShsDeaPreTax = HDeathPreShs*par$HShsShareStro,
           HCopdShsDeaPreTax = HDeathPreShs*par$HShsShareCopd)
  
  # Years of life Gained from quitting Post-Tax
  HLiYearGainPosTaxQuit <- p %>%
    mutate(LiYe = CFex * HLiYeGainPosTaxQuit) %>%
    group_by(Nal) %>%
    summarise(
      HLiYearGainPosTaxQuit = sum(LiYe, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Deaths of smokers who quit b/c tax but still die
  HDeathPosTaxQuit <- p %>%
    filter(HQuitDie == 1) %>%
    group_by(Nal) %>%
    summarise(
      HDeathPosTaxQuit = sum(CFex, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Deaths of those who keep smoking Post-Tax
  HDeathPosTaxSmokQuit <- p %>%
    filter(HDiePosTax == 1 & CQuit == 0) %>%
    group_by(Nal) %>%
    summarise(
      HDeathPosTaxSmokQuit = sum(CFex, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Deaths Post-Tax
  HDeathsPosTax <- p %>%
    filter(HDiePosTax == 1) %>%
    group_by(Nal) %>%
    summarise(
      HDeathsPosTax = sum(CFex, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(HDeathsPosTaxCanc = HDeathsPosTax*par$HShareCanc,
           HDeathsPosTaxHear = HDeathsPosTax*par$HShareHear,
           HDeathsPosTaxStro = HDeathsPosTax*par$HShareStro,
           HDeathsPosTaxCopd = HDeathsPosTax*par$HShareCopd)
  

  # Deaths of smokers averted by the tax
  HDeathsAver <- p %>%
    filter(HDeathAver == 1) %>%
    group_by(Nal) %>%
    summarise(
      HDeathsAver = sum(CFex, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(HDeathsAverCanc = HDeathsAver*par$HShareCanc,
           HDeathsAverHear = HDeathsAver*par$HShareHear,
           HDeathsAverStro = HDeathsAver*par$HShareStro,
           HDeathsAverCopd = HDeathsAver*par$HShareCopd)

  # Deaths of Non-Smokers for SHS Post-Tax
  HDeathsShsPosTax <- p %>%
    filter(HDeathShsPosTax == 1) %>%
    group_by(Nal) %>%
    summarise(
      HDeathsShsPosTax = sum(CFex, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(HCancShsDeaPosTax = HDeathsShsPosTax*par$HShsShareCanc,
           HHearShsDeaPosTax = HDeathsShsPosTax*par$HShsShareHear,
           HStroShsDeaPosTax = HDeathsShsPosTax*par$HShsShareStro,
           HCopdShsDeaPosTax = HDeathsShsPosTax*par$HShsShareCopd)  
  
  # Deaths of Non-Smokers for SHS averted by the tax
  HDeathsShsAver <- p %>%
    filter(HDeathShsAver == 1) %>%
    group_by(Nal) %>%
    summarise(
      HDeathsShsAver = sum(CFex, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(HCancShsDeaAver = HDeathsShsAver*par$HShsShareCanc,
           HHearShsDeaAver = HDeathsShsAver*par$HShsShareHear,
           HStroShsDeaAver = HDeathsShsAver*par$HShsShareStro,
           HCopdShsDeaAver = HDeathsShsAver*par$HShsShareCopd)  
  
  #========================
  # Pre-Tax
  ECostPreTax <- HDeathsPreTax %>% left_join(HDeathPreShs,by = 'Nal') %>%
    mutate(ECostHeartPreTax = (HDeathsPreTaxHear+HHearShsDeaPreTax)*par$ECost1,
           ECostStroPreTax = (HDeathsPreTaxStro+HStroShsDeaPreTax)*par$ECost2,
           ECostCopdPreTax = (HDeathsPreTaxCopd+HCopdShsDeaPreTax)*par$ECost3,
           ECostCancPreTax = (HDeathsPreTaxCanc+HCancShsDeaPreTax)*par$ECost4,
           ECostPreTax = ECostHeartPreTax+ECostStroPreTax+ECostCopdPreTax+ECostCancPreTax) %>%
    mutate(ECostOopPre  = par$ECopayProOop * ECostPreTax,
           ECostOohsPre = (1 - par$ECopayProOop) * ECostPreTax) %>%
    select(Nal,ECostPreTax,ECostOopPre,ECostOohsPre,
           ECostHeartPreTax,ECostStroPreTax,ECostCopdPreTax,ECostCancPreTax)
  # Post-Tax
  ECostPosTax <- HDeathsPosTax %>% left_join(HDeathsShsPosTax,by = 'Nal') %>%
    mutate(ECostHeartPosTax = (HDeathsPosTaxHear+HHearShsDeaPosTax)*par$ECost1,
           ECostStroPosTax = (HDeathsPosTaxStro+HStroShsDeaPosTax)*par$ECost2,
           ECostCopdPosTax = (HDeathsPosTaxCopd+HCopdShsDeaPosTax)*par$ECost3,
           ECostCancPosTax = (HDeathsPosTaxCanc+HCancShsDeaPosTax)*par$ECost4,
           ECostPosTax = ECostHeartPosTax+ECostStroPosTax+ECostCopdPosTax+ECostCancPosTax) %>%
    mutate(ECostOopPos  = par$ECopayProOop * ECostPosTax,
           ECostOohsPos = (1 - par$ECopayProOop) * ECostPosTax) %>%
    select(Nal,ECostPosTax,ECostOopPos,ECostOohsPos,
           ECostHeartPosTax,ECostStroPosTax,ECostCopdPosTax,ECostCancPosTax)
  
  # Averted
  ECostAver <- HDeathsAver %>% left_join(HDeathsShsAver,by = 'Nal') %>%
    mutate(ECostHeartAver = (HDeathsAverHear+HHearShsDeaAver)*par$ECost1,
           ECostStroAver = (HDeathsAverStro+HStroShsDeaAver)*par$ECost2,
           ECostCopdAver = (HDeathsAverCopd+HCopdShsDeaAver)*par$ECost3,
           ECostCancAver = (HDeathsAverCanc+HCancShsDeaAver)*par$ECost4,
           ECostAver = ECostHeartAver+ECostStroAver+ECostCopdAver+ECostCancAver) %>%
    mutate(ECostOopAver  = par$ECopayProOop * ECostAver,
           ECostOohsAver = (1 - par$ECopayProOop) * ECostAver) %>%
    select(Nal,ECostAver,ECostOopAver,ECostOohsAver,
           ECostHeartAver,ECostStroAver,ECostCopdAver,ECostCancAver)
  
  
  #========================
  # Cigarettes smoked
  
  # Tax Revenues Specific Pre-Tax (licit consumption net of illicit trade)
  TRevPreSpeSmo <- p %>%
    mutate(Con = CFex * SFumaCuan * packs_factor) %>%
    group_by(Nal) %>%
    summarise(
      TCon = sum(Con, na.rm = TRUE),
      TRevPreSpeSmo = (TCon * (1 - par$SIlliTraPre) * par$STaxVaSpePre),
      .groups = "drop"
    ) %>%
    select(Nal, TRevPreSpeSmo)
  # Tax Revenues Specific Post-Tax (licit consumption = post-tax minus illicit level fixed at pre-tax)
  TRevPosSpeSmo <- p %>%
    mutate(
      ConsPos = if_else(SFumaSino == 1 & CQuit == 0,
                        CFex * SFumaCuanPos * packs_factor,
                        NA_real_)
    ) %>%
    group_by(Nal) %>%
    summarise(
      TConsPos = sum(ConsPos, na.rm = TRUE),
      TRevPosSpeSmo = (TConsPos*(1 - par$SIlliTraPos) * par$STaxVaSpePos),
      .groups = "drop"
    ) %>%
    select(Nal, TRevPosSpeSmo)

  td_ds <- Reduce(function(a,b) left_join(a, b, by = 'Nal'),
                  list(CPopulation,CPreTaxSmok,CPosTaxSmok,CPreInteMedian,CPosInteMedian,
                       CPreTaxCigSmo,CPosTaxCigSmo,CPrePosCigSmoInDif,CPrePosCigSmoExDif,
                       CPreTaxCigTot,CPosTaxCigTot,CPreCigIllTra,CPosCigIllTra,
                       HDeathsPreTax,HDeathPreShs,ECostPreTax,
                       HDeathsPosTax,HDeathPosTaxQuit,HDeathPosTaxSmokQuit,HLiYearGainPosTaxQuit,
                       HDeathsShsPosTax,ECostPosTax,HDeathsAver,HDeathsShsAver,ECostAver,
                       TRevPreSpeSmo,TRevPosSpeSmo))
  
  td_ds <- td_ds %>% mutate(S = s)
  return(td_ds)
}