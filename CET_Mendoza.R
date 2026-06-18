# Load libraries
library(tidyverse)
library(readxl)
library(writexl)
library(car)
library(stargazer)
library(sandwich)
library(AER)
library(zoo)
library(scales)

# Clean workspace
rm(list = ls());gc()

# Control variables and IV
demog_xs <- c('lpobl_total','prop_m5','prop_M65','prop_muj')
macro_xs <- c('tasa_desempleo','inflacion','TRM')
total_xs <- c(demog_xs,macro_xs,'GS_LAC')
total_iv <- c('PBG_minero_2024','PBG_pc','Prop_PBGagro',
              'Regalias_2024','Coparticipacion_2024',
              'Origenpc_2024')

# Data
file <- 'Data/Regression data.xlsx'
gs_pc <- read_excel(file,sheet = 'GS_pc') %>% 
  select(Año,afiliados = `Afiliados total`,gs = `Total 2024`,gs_pc = PC_total,GS_LAC,prop = Proporcion)
tasa_desempleo <- read_excel(file,sheet = 'Desempleo') %>% group_by(Año) %>% summarise(tasa_desempleo = mean(Tasa),.groups = 'drop')
inflacion <- read_excel(file,sheet = 'Inflacion') %>% slice(-1) %>% select(Año,inflacion = `Inflación Mendoza`)
trm <- read_excel(file,sheet = 'TRM')
poblacion <- read_excel(file, sheet = 'Poblacion') %>% select(Año,pobl_total = Total,prop_m5,prop_M65,prop_muj) %>%
  mutate(lpobl_total = log(pobl_total))
mortalidad <- read_excel(file,sheet = 'Mortalidad') %>% select(Año,prop_ajuste)
instrumentos <- read_excel(file,sheet = 'Instrumentos') %>% 
  select(Año,PBG_minero_2024,PBG_pc,Regalias_2024,Coparticipacion_2024,Origenpc_2024)

# Regression variables
regresion <- gs_pc %>% left_join(tasa_desempleo,by = 'Año') %>%
  left_join(inflacion,by = 'Año') %>% left_join(trm,by = 'Año') %>%
  left_join(poblacion,by = 'Año') %>% left_join(instrumentos,by = 'Año') %>%
  left_join(mortalidad,by = 'Año') %>% 
  mutate(lgs_pc = log(gs_pc))

# Health outcomes
qaly <- readRDS('Data/QALY_mendoza_year.rds')
yll <- readRDS('Data/YLL_mendoza_year.rds')

# Consolidated table
df_uce <- qaly %>% full_join(yll,by = c('anio')) %>%
  left_join(regresion,by = c('anio' = 'Año')) %>% 
  mutate(YLL_pc = YLL*prop_ajuste/afiliados,lYLL_pc = log(YLL_pc), 
         QALY_pc = QALY*prop_ajuste/afiliados,lQALY_pc = log(QALY_pc))

# Add the lagged variables
df_uce <- df_uce %>%
  arrange(anio) %>%
  mutate(
    across(
      .cols = -anio,
      .fns = ~ lag(.x),
      .names = "lag_{.col}"
    )
  ) %>% filter(anio != 2014) %>%
  mutate(dummy = ifelse(anio %in% 2020:2021,1,0),
         dummy2020 = ifelse(anio %in% 2020,1,0),
         dummy2021 = ifelse(anio %in% 2021,1,0))

# Considered models
reg_posibles <- tibble(controls_list = list(c('lpobl_total','GS_LAC'),
                                            c('tasa_desempleo','inflacion'),
                                            c('tasa_desempleo','inflacion'),
                                            c('tasa_desempleo','inflacion'),
                                            c('tasa_desempleo','inflacion')),
                       instruments_list = list(c('PBG_minero_2024','Regalias_2024'),
                                               c('PBG_minero_2024','Regalias_2024','Coparticipacion_2024'),
                                               c('PBG_minero_2024','Regalias_2024','Coparticipacion_2024'),
                                               c('PBG_minero_2024','Regalias_2024','Origenpc_2024'),
                                               c('PBG_minero_2024','Regalias_2024')),
                       tend_dum = c('+dummy2021','+dummy','+dummy2020','+dummy2020','+dummy2021'),
                       modelo = paste0('E',1:5))

# Function for extract F-stat first stage
extract_iv_fstat <- function(model) {
  V_HAC <- NeweyWest(model,lag = 1,prewhite = FALSE,adjust = TRUE)
  s <- suppressMessages(summary(model,V_HAC, diagnostics = TRUE))
  aux <- s$coefficients['lag_lgs_pc',c('Std. Error','Pr(>|t|)')]
  diag_tab <- s$diagnostics
  
  row_f <- grep("Weak instruments", rownames(diag_tab), ignore.case = TRUE)
  
  # VIF
  X_reg <- model.matrix(model, component = "regressors")
  Z_inst <- model.matrix(model, component = "instruments")
  
  reg_names <- colnames(X_reg)
  inst_names <- colnames(Z_inst)
  reg_names_no_intercept <- setdiff(reg_names, "(Intercept)")
  inst_names_no_intercept <- setdiff(inst_names, "(Intercept)")
  endog_vars <- setdiff(reg_names_no_intercept, inst_names_no_intercept)
  
  if (length(endog_vars) == 0) {
    warning("No se detectaron variables endógenas. No se calcula VIF de primera etapa.")
    vif_max <- NA_real_
  } else {
    
    vif_values_all <- list()
    
    for (endog in endog_vars) {
      fs_data <- as.data.frame(Z_inst[, inst_names_no_intercept, drop = FALSE])
      fs_data$.endog <- X_reg[, endog]
      fs_model <- lm(.endog ~ ., data = fs_data)
      vif_values <- car::vif(fs_model)
      vif_values_all[[endog]] <- vif_values
    }
    vif_max <- max(unlist(vif_values_all), na.rm = TRUE)
  }
  out <- data.frame(
    statistic = rownames(diag_tab)[row_f],
    df1       = diag_tab[row_f, "df1"],
    df2       = diag_tab[row_f, "df2"],
    f_stat    = diag_tab[row_f, "statistic"],
    p_value   = diag_tab[row_f, "p-value"],
    vif_max   = vif_max,
    std_error = aux[1],
    pval = aux[2],
    row.names = NULL
  )
  return(out)
}

# Compute CET =========================================
MPBG_pc <- (df_uce$PBG_pc[df_uce$anio == 2023])*1000
Mgs_pc <- mean(df_uce$gs_pc)

MYLL_pc  <- mean(df_uce$YLL_pc, na.rm = TRUE)
MQALY_pc  <- mean(df_uce$QALY_pc, na.rm = TRUE)

reg_posibles <- reg_posibles %>%
    mutate(
      beta_yll = NA_real_,
      beta_qaly = NA_real_,
      UCE_yll = NA_real_,
      UCE_qaly = NA_real_,
      prop_UCE_yll = NA_real_,
      prop_UCE_qaly = NA_real_,
      pval_yll = NA_real_,
      std_yll = NA_real_,
      ci_yll= NA_real_,
      pval_qaly = NA_real_,
      std_qaly = NA_real_,
      ci_qaly= NA_real_,
      F_yll = NA_real_,
      F_qaly = NA_real_,
      vif_yll = NA_real_,
      vif_qaly = NA_real_,
      ci_UCE_yll = NA_real_,
      ci_UCE_qaly = NA_real_,
      ci_prop_UCE_yll = NA_real_,
      ci_prop_UCE_qaly = NA_real_)

for (i in seq_len(nrow(reg_posibles))) {
  Xs <- reg_posibles$controls_list[[i]]
  IV <- reg_posibles$instruments_list[[i]]
  tend_dum <- reg_posibles$tend_dum[i]
  
  # IV and controls
  Xs <- paste0("lag_", Xs)
  IV <- paste0("log(lag_", IV, ")")
  formula_yll <- as.formula(
    paste0(  
      "lYLL_pc"," ~ lag_lgs_pc + ",
      paste(Xs, collapse = " + "),
      tend_dum,"| ",
      paste(IV, collapse = " + "),
      " + ",
      paste(Xs, collapse = " + "),
      tend_dum
    )
  )
  
  modelo_yll <- ivreg(formula_yll, data = df_uce)
  ci_yll <- coefci(modelo_yll, vcov. = vcovHC(modelo_yll, type = "HC1"), level = 0.90)['lag_lgs_pc',]
  beta_yll <- coef(modelo_yll)["lag_lgs_pc"]
  
  F_yll = tryCatch(extract_iv_fstat(modelo_yll)$f_stat,error=function(e) NULL)
  vif_yll = tryCatch(extract_iv_fstat(modelo_yll)$vif_max,error=function(e) NULL)
  pval_yll = tryCatch(extract_iv_fstat(modelo_yll)$pval,error=function(e) NULL)
  std_yll = tryCatch(extract_iv_fstat(modelo_yll)$std_error,error=function(e) NULL)
  
  formula_qaly <- as.formula(
    paste0(
      "lQALY_pc"," ~ lag_lgs_pc + ",
      paste(Xs, collapse = " + "),
      tend_dum,"| ",
      paste(IV, collapse = " + "),
      " + ",
      paste(Xs, collapse = " + "),
      tend_dum
    )
  )
  
  modelo_qaly <- ivreg(formula_qaly, data = df_uce)
  ci_qaly <- coefci(modelo_qaly, vcov. = vcovHC(modelo_qaly, type = "HC1"), level = 0.90)['lag_lgs_pc',]
  beta_qaly <- coef(modelo_qaly)["lag_lgs_pc"]
  
  F_qaly = tryCatch(extract_iv_fstat(modelo_qaly)$f_stat,error=function(e) NULL)
  vif_qaly = tryCatch(extract_iv_fstat(modelo_qaly)$vif_max,error=function(e) NULL)
  pval_qaly = tryCatch(extract_iv_fstat(modelo_qaly)$pval,error=function(e) NULL)
  std_qaly = tryCatch(extract_iv_fstat(modelo_qaly)$std_error,error=function(e) NULL)
  
  UCE_yll  <- Mgs_pc / (beta_yll  * -MYLL_pc)
  UCE_qaly <- Mgs_pc / (beta_qaly * -MQALY_pc)
  ci_UCE_yll <- Mgs_pc / (ci_yll * -MYLL_pc)
  ci_UCE_qaly <- Mgs_pc / (ci_qaly * -MQALY_pc)
  
  prop_UCE_yll  <- UCE_yll  / MPBG_pc
  prop_UCE_qaly <- UCE_qaly / MPBG_pc
  
  ci_prop_UCE_yll  <- ci_UCE_yll  / MPBG_pc
  ci_prop_UCE_qaly <- ci_UCE_qaly / MPBG_pc
  
  reg_posibles$beta_yll[i] <- beta_yll
  reg_posibles$beta_qaly[i] <- beta_qaly
  reg_posibles$UCE_yll[i] <- UCE_yll
  reg_posibles$UCE_qaly[i] <- UCE_qaly
  reg_posibles$prop_UCE_yll[i] <- prop_UCE_yll
  reg_posibles$prop_UCE_qaly[i] <- prop_UCE_qaly
  reg_posibles$ci_yll[i] <- paste0(round(ci_yll,2),collapse = '; ')
  reg_posibles$ci_qaly[i] <- paste0(round(ci_qaly,2),collapse = '; ')
  reg_posibles$ci_prop_UCE_yll[i] <- paste0(round(ci_prop_UCE_yll,2),collapse = '; ')
  reg_posibles$ci_prop_UCE_qaly[i] <- paste0(round(ci_prop_UCE_qaly,2),collapse = '; ')
  reg_posibles$ci_UCE_yll[i] <- paste0(round((ci_UCE_yll)/1e6,2),collapse = '; ')
  reg_posibles$ci_UCE_qaly[i] <- paste0(round((ci_UCE_qaly)/1e6,2),collapse = '; ')
  
  if (!is.null(F_yll)) reg_posibles$F_yll[i]  =F_yll
  if (!is.null(F_qaly)) reg_posibles$F_qaly[i] = F_qaly
  if (!is.null(vif_yll)) reg_posibles$vif_yll[i]  =vif_yll
  if (!is.null(vif_qaly)) reg_posibles$vif_qaly[i] = vif_qaly
  if (!is.null(pval_yll)) reg_posibles$pval_yll[i] = pval_yll
  if (!is.null(pval_qaly)) reg_posibles$pval_qaly[i] = pval_qaly
  if (!is.null(std_yll)) reg_posibles$std_yll[i] = std_yll
  if (!is.null(std_qaly)) reg_posibles$std_qaly[i] = std_qaly
}

tabF <- reg_posibles %>% rowwise() %>%
  mutate(controles = paste0(controls_list,collapse = ';'),instrumentos =paste0(instruments_list,collapse = ';')) %>%
  select(-controls_list,-instruments_list) %>%
  arrange(modelo)
tabF
