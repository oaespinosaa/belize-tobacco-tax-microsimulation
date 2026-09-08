tab_sum <- NULL
aux_name <- expand.grid(x1 = seq(0.30,0.7,0.05)*100,
                        x2 = c(0.13)*100,
                        x3 = c(0.3785,0.77),
                        x4 = c(0,0.02,0.045)*100) %>%
  mutate(name = paste0(x1,'_',x2,'_',x3,'_',x4))
names <- aux_name$name
# names <- names[c(4:9)]
for (name in names){
  lbecea  <- paste0(old_wd,"/lbecea_",name)
  namespl <- strsplit(name,'_')[[1]]
  val_f <- (7.5*(1+as.numeric(namespl[1])/100))
  aux_tax <- ((val_f - 5.26275 - val_f*0.125)/val_f + 0.125)*100
  name2 <- paste0("Elasticity: ",namespl[3],"\nTax reform: ",round(aux_tax,1),"%\nIllicit trade: ",namespl[2],"%\n",
                  "Market penetration: ",namespl[4],"%")
  name3 <- namespl[3]
  df <- readRDS(file.path(lbecea, "TD_Nal.rds")) %>%
    select(-S) %>% summarise_all(median,na.rm = T) %>% 
    mutate(scenario = name2,reform = paste0("Tax reform:",round(aux_tax,1),"%"),
           elasticity = name3,
           tax = as.numeric(namespl[1])/100,market = as.numeric(namespl[4])/100)
  tab_sum <- bind_rows(tab_sum,df)
}

fig_var <- function(tab_sum,varPre,varPos,label_x, type){
  if (type %in% c('G','TD')){
  df <- tab_sum %>% group_by(reform) %>% summarise_if(is.numeric,mean) %>%
    pivot_longer(-reform,names_to = 'var',values_to = 'val') %>%
    filter(var %in% c(varPos)) %>% replace_na(list(val = 0)) %>%
    bind_rows(tab_sum %>% summarise_if(is.numeric,mean) %>%
                pivot_longer(-c(Nal),names_to = 'var',values_to = 'val') %>%
                filter(var %in% c(varPre)) %>% mutate(reform = 'Base scenario:29.83%')) %>%
    mutate(var = factor(var,levels = c(varPre,varPos)))
  var_fill = var_y <- 'reform'
  if(type == 'TD'){
    df <- df %>% mutate(var = case_when(var == 'HDeathsPreTax'~"Original smokers",
                                                    var == 'HDeathPosTaxQuit'~"Quitters",
                                                    var == 'HDeathPosTaxSmokQuit'~"Non-quitters"),
                                    var = factor(var,levels = c("Original smokers","Quitters","Non-quitters")))
    var_fill <- 'reform'
    var_y <- 'var'
    }
  }
  if (type == 'YLG'){
    df <- tab_sum %>% group_by(reform) %>% summarise_if(is.numeric,mean) %>%
      pivot_longer(-reform,names_to = 'var',values_to = 'val') %>%
      filter(var %in% c(varPos)) %>% replace_na(list(val = 0)) %>%
      mutate(var = factor(var,levels = c(varPos)))
    var_fill = var_y <- 'reform' 
  }
  if (type %in% c('D','C','DA','CA')){
    if(type == 'D'){aux1 <- "All deaths"}
    if(type == 'C'){aux1 <- "All costs"}
    if(type == 'DA'){aux1 <- "All deaths averted"}
    if(type == 'CA'){aux1 <- "All costs averted"}
    df <- tab_sum %>% group_by(reform) %>% summarise_if(is.numeric,mean) %>%
      select_at(c('reform','Nal',varPos)) %>% 
      pivot_longer(-c(reform,Nal),names_to = 'var',values_to = 'val') %>%
      replace_na(list(val = 0))
    if (type %in% c('D','C')){
    df <- df %>%
      bind_rows(tab_sum %>% summarise_if(is.numeric,mean) %>%
                  select_at(c('Nal',varPre)) %>% 
                  pivot_longer(-Nal,names_to = 'var',values_to = 'val') %>%
                  replace_na(list(val = 0)) %>% mutate(reform = 'Base scenario:29.83%'))
    }
    df <- df %>%
      mutate(var = gsub('PreTax','',gsub('PosTax','',gsub('Aver','',gsub('HDeaths','',gsub('ECost','',var))))),
        var = case_when(var == ""~aux1,
                       var %in% c("Heart","Hear")~"Heart disease",
                       var == "Stro"~"Stroke",
                       var == "Copd"~"COPD",
                       var == "Canc"~"Cancer"),
           var = factor(var,levels = c(aux1,"Heart disease","Stroke","COPD","Cancer")))
    if(type %in% c('D','DA')){
      df <- df %>% mutate(val = val/1e3) 
    }
    if(type %in% c('C','CA')){
      df <- df %>% mutate(val = val/1e6)
    }
    var_fill <- 'reform'
    var_y <- 'var'
  }
  if (type == 'I'){
    df <- tab_sum %>%
      pivot_longer(-c(Nal,scenario,market,reform,tax),names_to = 'var',values_to = 'val') %>%
      filter(var %in% c(varPos)) %>% replace_na(list(val = 0)) %>%
      bind_rows(tab_sum %>% summarise_if(is.numeric,mean) %>%
                  pivot_longer(-c(Nal),names_to = 'var',values_to = 'val') %>%
                  filter(var %in% c(varPre)) %>% mutate(scenario = 'Base scenario:29.83%')) %>%
      mutate(var = factor(var,levels = c(varPre,varPos)))
    var_fill = var_y <- 'scenario'    
  }
  if (type == 'I2'){
    df <- tab_sum %>%
      pivot_longer(-c(Nal,scenario,market,reform,tax),names_to = 'var',values_to = 'val') %>%
      filter(var %in% varPos) %>% replace_na(list(val = 0)) %>%
      bind_rows(tab_sum %>% summarise_if(is.numeric,mean) %>%
                  pivot_longer(-c(Nal),names_to = 'var',values_to = 'val') %>%
                  filter(var %in% c(varPre)) %>% mutate(scenario = 'Base scenario:29.83%')) %>%
      mutate(var = case_when(var == "CPreTaxCigTot"~"Pay tax",
                             var == "CPreCigIllTra"~"Illicit cig",
                             var == "CPosTaxCigTot"~"Pay tax",
                             var == "CPosCigIllTra"~"Illicit cig"),
             var = factor(var,levels = c("Pay tax","Illicit cig")))
  }
  if (type != 'I2'){
gg <- ggplot(df, aes(x = val, y = !!sym(var_y), fill = !!sym(var_fill))) +
  geom_col(
    position = position_dodge2(width = 0.9, preserve = "single")
  ) +
  geom_text(
    aes(label = scales::comma(val, accuracy = 0.1)),
    position = position_dodge2(width = 0.9, preserve = "single"),
    hjust = 1.05, 
    size = 3
  ) +
  labs(
    x = label_x,
    y = "",
    fill = ""
  ) +
  scale_x_continuous(n.breaks = 10, expand = expansion(mult = c(0, 0.1)),
                     labels = scales::label_number(big.mark = ',')) +
  theme_minimal()
  }else{
  gg <- ggplot(df, aes(x = val, y = scenario, fill = var)) +
    geom_col() +
    geom_text(
      aes(label = scales::comma(val, accuracy = 0.1)),
      hjust = 1.05, 
      size = 3
    ) +
    labs(
      x = label_x,
      y = "",
      fill = ""
    ) +
    scale_x_continuous(n.breaks = 10, expand = expansion(mult = c(0, 0.1)),
                       labels = scales::label_number(big.mark = ',')) +
    theme_minimal()
}
if (type %in% c('TD','D','C','DA','CA','I2')){
  gg <- gg + theme(legend.position = "bottom")
}else{
  gg <- gg + theme(legend.position = "bottom",axis.text.y = element_blank(),axis.ticks.y = element_blank())
}
return(gg)
}

# Outcomes independent from illicit trade
fig_var(tab_sum,"CPreTaxSmok","CPosTaxSmok","Number of smokers","G")
fig_var(tab_sum,"CPreInteMedian","CPosInteMedian","Smoking intensity (cigs x day)","G")
fig_var(tab_sum,"CPreTaxCigSmo","CPosTaxCigSmo","Cigarettes smoked in Belize (20-Sticks packs)","G")
fig_var(tab_sum,"HDeathsPreTax",c('HDeathPosTaxQuit','HDeathPosTaxSmokQuit'),"Deaths of smokers","TD")
fig_var(tab_sum,"","HLiYearGainPosTaxQuit","Years of life Gained from quitting Post-Tax","YLG")
fig_var(tab_sum,c("HDeathsPreTax","HDeathsPreTaxHear","HDeathsPreTaxStro","HDeathsPreTaxCopd","HDeathsPreTaxCanc"),
        c("HDeathsPosTax","HDeathsPosTaxHear","HDeathsPosTaxStro","HDeathsPosTaxCopd","HDeathsPosTaxCanc"),"Deaths of smokers (x 1,000)","D")
fig_var(tab_sum,c("ECostPreTax","ECostHeartPreTax","ECostStroPreTax","ECostCopdPreTax","ECostCancPreTax"),
        c("ECostPosTax","ECostHeartPosTax","ECostStroPosTax","ECostCopdPosTax","ECostCancPosTax"),"Total costs (millions BZD)","C")
fig_var(tab_sum,"",c("HDeathsAver","HDeathsAverHear","HDeathsAverStro","HDeathsAverCopd","HDeathsAverCanc"),"Premature deaths adverted (x 1,000)","DA")
fig_var(tab_sum,"",c("ECostAver","ECostHeartAver","ECostStroAver","ECostCopdAver","ECostCancAver"),"Averted total costs (millions BZD)","CA")

# Outcomes with illicit trade
fig_var(tab_sum,"CPreCigIllTra","CPosCigIllTra","Cigarettes illicit trade","I")
fig_var(tab_sum,"TRevPreSpeSmo","TRevPosSpeSmo","Tax revenues specific","I")
fig_var(tab_sum,"TProfPreSmo","TProfPosSmo","Industry profits","I")
fig_var(tab_sum,c("CPreTaxCigTot","CPreCigIllTra"),
        c("CPosTaxCigTot","CPosCigIllTra"),"Packs","I2")

