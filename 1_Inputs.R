# This script defines the function for structural parameters (taxes/prices), 
# builds key lookup tables and baseline inputs used by the microsimulation pipeline.

fn_par <- function(porc_tax,porc_illicit,elasticity,porc_market){
  
  # ---- PARAMETERS 
  par <- list(
    CQuitYouth = 2.0, #Increase Impact Extensive Margin Youth
    ECopayInsu = 0.67, # Fraction of population insured https://apps.who.int/gb/statements/WHA76/PDF/Belize-13.1-13.2.pdf
    ECopayReim = 1.0, # Reimbursement
    # https://doi.org/10.1093/heapol/czae068
    ECost1 = 5701.16, # Cost (US$ per year per peson - Annual) - Heart disease
    ECost2 = 8924.35, # Cost (US$ per year per peson - Annual) - Stroke
    ECost3 = 2529.76, # Cost (US$ per year per peson - Annual) - Copd
    ECost4 = 64275.37, # Cost (US$ per year per peson - Annual) - Cancer
    HDeathTob = 0.5, # Smokers deaths caused by tobacco
    # Shares of Averted deaths by NCD
    HShareCanc = 0.067657767,
    HShareCopd = 0.185072816,
    HShareHear = 0.52336165,
    HShareStro = 0.223907767,
    # Second-Hand Smoke Shares of Averted deaths by NCD
    HShsShareCanc = 0.083336059,
    HShsShareCopd = 0.412755519,
    HShsShareHear = 0.39574816,
    HShsShareStro = 0.108160262,
    # Imports packs x 20
    SCigaImpo = 69560/(1000*20)*1e6,
    # Population https://sib.org.bz/statistics/population/
    # Exwork
    # Inflation https://sib.org.bz/statistics/economic-statistics/consumer-price-index/
    SExwManCoPre = 0.10*1.2713, # Manufacturing Cost # https://tobaccocontrol.bmj.com/content/tobaccocontrol/21/2/230.full.pdf
    SExwMinProRaPre = 0.2, # Manufacturing Profit Rate
    SIlliTraPre = porc_illicit, # ILLICIT TRADE (PRE-TAX)
    # Impact on Elasticities (kind of half of elasticity for quitting, half for intensity)
    SImpaElasExtensive = 0.5, # At the extensive margin 
    SImpaElasIntensive = -0.5, # At the intensive margin
    SPriDisMaPre = 0.15, # Distribution Margin
    # Elasticities
    SPriElas = elasticity,
    # Price
    SPriRetMaPre = 0.15, # Retail Margin
    # Rates
    STaxPassSpePre = 1.2, # Pass-Through Specific (excise)
    STaxRaAdvPre = 0, # AdValorem (excise)
    STaxRaVatPre = 0.125, # Value Added Vat https://bts.gov.bz/gst-faq/
    # Values
    STaxVaAdvPre = 0, # AdValorem (excise)
    STaxVaSpePre = 7.5*0.1733, # Specific (excise) # https://www.numbeo.com/cost-of-living/country_result.jsp?country=Belize
    STaxVaVatPre = 7.5*0.125 # Value Added Vat
  )
  par$adj_fac_illi = (par$SCigaImpo/(1-par$SIlliTraPre))/2560000
  val_final = 7.5*(1+porc_tax)
  price_ini = 7.5-par$STaxVaSpePre-par$STaxVaVatPre
  # AdValorem (excise)
  par$STaxVaAdvPos = par$STaxVaAdvPre
  # Value Added Vat
  par$STaxVaVatPos = val_final*par$STaxRaVatPre
  par$STaxVaSpePos = val_final - price_ini - par$STaxVaVatPos
  par$SIlliTraPos = par$SIlliTraPre + (par$STaxVaSpePos/par$STaxVaSpePre-1)*porc_market
  # Total
  par$STaxVaTotPre = par$STaxVaSpePre + par$STaxVaAdvPre + par$STaxVaVatPre
  par$STaxVaTotPos = par$STaxVaSpePos + par$STaxVaAdvPos + par$STaxVaVatPos
  # Rates
  # AdValorem (excise)
  par$STaxRaAdvPos = par$STaxRaAdvPre
  # Value Added Vat
  par$STaxRaVatPos = par$STaxRaVatPre
  # Pass-Through Specific (excise)
  par$STaxPassSpePos = par$STaxPassSpePre
  # Implicit Baseline Prices
  # Ibp Vat
  par$STaxIbpVatPre = par$STaxVaVatPre/par$STaxRaVatPre
  licit_price = par$STaxVaVatPos/par$STaxRaVatPos
  par$STaxIbpVatPos = (1-par$SIlliTraPos)*licit_price + par$SIlliTraPos*(1/3*licit_price)
  # Ibp AdValorem
  par$STaxIbpAdvPre = par$STaxVaAdvPre/par$STaxRaAdvPre
  par$STaxIbpAdvPos = par$STaxVaAdvPos/par$STaxRaAdvPos
  
  par$SPriceIncrease = (par$STaxIbpVatPos - par$STaxIbpVatPre)/par$STaxIbpVatPre
  
  # COSTS (Million COP$ per year per peson - Annual)
  # Proportion of healthcare expenditures as Out Of Pocket
  par$ECopayProOop = par$ECopayInsu*(1-par$ECopayReim) + (1- par$ECopayInsu)*par$ECopayReim
  
  par$CQuitProp  = -par$SPriElas * par$SImpaElasExtensive * par$SPriceIncrease
  par$CIntenProp = -par$SPriElas * par$SImpaElasIntensive * par$SPriceIncrease
  return(par)
}

hid_vars <- c("vid","hid")
iid_vars <- c("vid","hid","iid")
# NUMBER OF CIGARETTES (packs 20)
packs_factor <- (30 * 12) / 20

# ----LOOKUP TABLES (from Stata) ----
# ---- HRiskRedu
HRiskRedu <- read.table(textConnection("1\t1\n2\t1\n3\t1\n4\t0.968811722\n5\t0.947662258\n6\t0.92098104\n7\t0.892470978\n8\t0.865640471\n9\t0.836784324\n10\t0.794983743\n11\t0.729006299\n12\t0.628344949\n13\t0.499176461\n14\t0.364361418\n15\t0.246916804\n16\t0.156773014\n17\t0.090773853\n18\t0.045194143\n19\t0.016308707\n20\t0.000392369\n21\t0.000392369"), header = FALSE, sep = "\t", stringsAsFactors = FALSE)
names(HRiskRedu) <- c("CAgeGroup", "HRiskRedu")

# ---- HLiYeGain
HLiYeGain <- read.table(textConnection("0\t0\n15\t10\n25\t9\n45\t6\n65\t3\n105\t0"), header = FALSE, sep = "\t", stringsAsFactors = FALSE)
names(HLiYeGain) <- c("HAgeLiYe", "HLiYeGain")

# ---- HLiYeGainCubSpli
HLiYeGainCubSpli <- read.table(textConnection("0\t0\n1\t0.90168\n2\t1.79706\n3\t2.67985\n4\t3.54376\n5\t4.38248\n6\t5.18973\n7\t5.95922\n8\t6.68464\n9\t7.3597\n10\t7.9781\n11\t8.53356\n12\t9.01977\n13\t9.43045\n14\t9.75929\n15\t10\n16\t10.14911\n17\t10.21446\n18\t10.20669\n19\t10.13645\n20\t10.01442\n21\t9.85122\n22\t9.65753\n23\t9.44399\n24\t9.22127\n25\t9\n26\t8.78894\n27\t8.58918\n28\t8.39992\n29\t8.22034\n30\t8.04962\n31\t7.88695\n32\t7.73152\n33\t7.58252\n34\t7.43914\n35\t7.30056\n36\t7.16596\n37\t7.03454\n38\t6.90549\n39\t6.77798\n40\t6.65122\n41\t6.52438\n42\t6.39665\n43\t6.26722\n44\t6.13527\n45\t6\n46\t5.8608\n47\t5.71788\n48\t5.57169\n49\t5.42264\n50\t5.27117\n51\t5.11771\n52\t4.96268\n53\t4.80651\n54\t4.64964\n55\t4.49248\n56\t4.33548\n57\t4.17906\n58\t4.02364\n59\t3.86966\n60\t3.71755\n61\t3.56774\n62\t3.42064\n63\t3.2767\n64\t3.13635\n65\t3\n66\t2.868\n67\t2.74034\n68\t2.61689\n69\t2.49754\n70\t2.38219\n71\t2.27073\n72\t2.16304\n73\t2.05902\n74\t1.95854\n75\t1.86151\n76\t1.76781\n77\t1.67732\n78\t1.58995\n79\t1.50557\n80\t1.42408\n81\t1.34536\n82\t1.2693\n83\t1.1958\n84\t1.12474\n85\t1.05601\n86\t0.9895\n87\t0.9251\n88\t0.8627\n89\t0.80219\n90\t0.74345\n91\t0.68637\n92\t0.63085\n93\t0.57678\n94\t0.52403\n95\t0.47251\n96\t0.42209\n97\t0.37268\n98\t0.32415\n99\t0.2764\n100\t0.22932\n101\t0.18279\n102\t0.1367\n103\t0.09095\n104\t0.04542\n105\t0\n106\t0\n107\t0\n108\t0\n109\t0\n110\t0\n111\t0\n112\t0\n113\t0\n114\t0\n115\t0\n116\t0\n117\t0"), header = FALSE, sep = "\t", stringsAsFactors = FALSE)
names(HLiYeGainCubSpli) <- c("CAge", "HAgeLiYe")

# Belize population by age group and sex
# Population 15-49
popBelize <- read_excel(paste0(old_wd,"/data/MidYear_Estimates_AgeGroup_Sex_2015-2025.xlsx"))
# Population 50-95+
pop50Belize <- read_excel(paste0(old_wd,"/data/MidYear_Estimates_AgeGroup_Sex_2015-2025.xlsx"),
                          sheet = 'Hoja2') %>% select(CAge,Male_2025,Female_2025) %>%
  pivot_longer(-CAge,names_to = 'sex',values_to = 'N') %>%
  mutate(CSex = ifelse(grepl('Female',sex),2,1),CAge = as.numeric(CAge)) %>% select(-sex)

# Adjustment of smoker according to the parameter of adj_factor
AdjNumSmo <- function(df, age_low, age_up, sex, adj_factor) {
  base_smokers <- sum(df[["CFex"]][df[["CAge"]] >= age_low &df[["CAge"]] <= age_up &
                                      df[["CSex"]] == sex & df[["SFumaSino"]] == 1], na.rm = TRUE)
  nsmo <- base_smokers * (adj_factor - 1)
  message(sprintf("Extra Smokers (age %s-%s, sex %s): %.3f",age_low, age_up, sex, nsmo))
  
  # If nothing to add, return unchanged
  if (!is.finite(nsmo) || nsmo <= 0) return(df)
  
  # Assign random numbers to eligible non-smokers in group
  df2 <- df %>%
    mutate(
      RanU = if_else(CAge >= age_low & CAge <= age_up & CSex == sex &
                       SFumaSino == 0, 8 + runif(n()), NA_real_)
    )
  
  # Sort by descending RanU (higher first), compute cumulative sum of CFex
  df2 <- df2 %>%
    arrange(desc(RanU)) %>%
    mutate(
      RanPop = if_else(CAge >= age_low & CAge <= age_up & CSex == sex &
                         SFumaSino == 0,
                       cumsum(replace_na(CFex, 0)),
                       NA_real_),
      RanW = if_else(!is.na(RanPop) & RanPop <= nsmo, 1L, 0L)
    ) %>%
    mutate(SFumaSino= if_else(RanW == 1L, 1L, SFumaSino))
  med_frec <- Hmisc::wtd.quantile(
      x = df2 %>% filter(CAge >= age_low,CAge <= age_up,CSex == sex, SFumaSino == 1) %>% 
        pull(SFumaFrec),
      weights = df2 %>% filter(CAge >= age_low,CAge <= age_up,CSex == sex, SFumaSino == 1) %>% 
        pull(CFex),
      probs = 0.5, na.rm = TRUE
    )
    
  med_cuan <- Hmisc::wtd.quantile(
    x = df2 %>% filter(CAge >= age_low,CAge <= age_up,CSex == sex, SFumaSino == 1) %>% 
      pull(SFumaCuan),
    weights = df2 %>% filter(CAge >= age_low,CAge <= age_up,CSex == sex, SFumaSino == 1) %>% 
      pull(CFex),
    probs = 0.5, na.rm = TRUE
  )
  # Assign median frequency/intensity to the newly created smokers (RanW==1)
  df2 <- df2 %>%
    mutate(
      SFumaFrec= if_else(RanW == 1L, as.numeric(med_frec),SFumaFrec),
      SFumaCuan= if_else(RanW == 1L, as.numeric(med_cuan),SFumaCuan)
    ) %>%
    select(-RanU, -RanPop, -RanW) %>%
    arrange(iid)
  return(df2)
}