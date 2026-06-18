# Load libraries
library(tidyverse)
library(readxl)
library(AER)
library(zoo)

# Clean workspace
rm(list = ls());gc()

# Control variables and IV
demog_xs=c('lpobl_total','prop_m5','prop_M65','prop_muj')
macro_xs=c('tasa_desempleo','inflacion','TRM')
total_xs <- c(demog_xs,macro_xs,'GS_LAC')

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

# Adjustment coverage by 5-year age group
age_groups5 <- c('0-4','5-9',paste0(seq(10,75,5),'-',seq(14,79,5)),'80+')
poblacion_grupo5 <- read_excel('Data/PopulationAdjust_AgeGroup5.xlsx') %>%
  mutate(grupo_edad = case_when(age_group5 %in% age_groups5[1:3]~'0-14',
                                age_group5 %in% age_groups5[4:13]~'15-64',
                                age_group5 %in% age_groups5[14:17]~'65+'))
afiliados_grupo5 <- read_excel(file,sheet = 'Afil_grupoedad') %>%
  select(Año,grupo_edad,proporcion_grupo) %>% 
  left_join(poblacion_grupo5,by = c('Año' = 'anio','grupo_edad')) %>%
  mutate(afiliados_totalgrupo = adj*proporcion_grupo/100) %>% 
  left_join(gs_pc %>% select(Año,prop),by = 'Año') %>%
  mutate(afiliados_grupoedad = afiliados_totalgrupo*prop) %>%
  select(Año, age_group5,afiliados_grupoedad)
aux_afilgrupo5 <- afiliados_grupo5 %>% group_by(age_group5) %>%
  summarise(val2016 = afiliados_grupoedad[Año == 2016],.groups = 'drop')
afiliados_grupo5 <- afiliados_grupo5 %>%
  bind_rows(expand.grid(Año = 2014:2015,age_group5 = age_groups5) %>%
              left_join(aux_afilgrupo5 %>% rename(afiliados_grupoedad = val2016),by = 'age_group5')) %>% 
  arrange(Año) %>% select(Año,age_group5,afiliados_grupoedad) %>%
  mutate(age_group5_2 = ifelse(age_group5 %in% c('0-4','5-9'),'0-9',age_group5)) %>%
  group_by(Año,age_group5_2) %>% summarise(afiliados_grupoedad = sum(afiliados_grupoedad),.groups = 'drop')

# Regression variables
regresion <- gs_pc %>% left_join(tasa_desempleo,by = 'Año') %>%
  left_join(inflacion,by = 'Año') %>% left_join(trm,by = 'Año') %>%
  left_join(poblacion,by = 'Año') %>% left_join(instrumentos,by = 'Año') %>%
  left_join(mortalidad,by = 'Año') %>% 
  mutate(lgs_pc = log(gs_pc))

# Health outcomes
qaly <- readRDS('Data/QALY_mendoza_year_age.rds') %>%
  mutate(age_group5_2 = ifelse(age_group5 %in% c('0','1-9'),'0-9',age_group5)) %>%
  group_by(anio,age_group5_2) %>% summarise(QALY = sum(QALY),.groups = 'drop')
yll <- readRDS('Data/YLL_mendoza_year_age.rds') %>% 
  mutate(age_group5_2 = ifelse(age_group5 %in% c('0','1-9'),'0-9',age_group5)) %>%
  group_by(anio,age_group5_2) %>% summarise(YLL = sum(YLL),.groups = 'drop')

# Consolidated table
df_uce_tot <- qaly %>% full_join(yll,by = c('anio','age_group5_2')) %>%
  left_join(afiliados_grupo5,by = c('anio' = 'Año','age_group5_2')) %>% 
  left_join(regresion,by = c('anio' = 'Año')) %>% 
  mutate(YLL_pc = YLL*prop_ajuste/afiliados_grupoedad,lYLL_pc = log(YLL_pc), 
         QALY_pc = QALY*prop_ajuste/afiliados_grupoedad,lQALY_pc = log(QALY_pc))

# Add the lagged variables
df_uce_tot <- df_uce_tot %>%
  group_by(age_group5_2) %>% arrange(anio) %>%
  mutate(
    across(
      .cols = -anio,
      .fns = ~ lag(.x),
      .names = "lag_{.col}"
    )
  ) %>% ungroup %>% 
  mutate(dummy = ifelse(anio %in% 2020:2021,1,0),
         dummy2020 = ifelse(anio %in% 2020,1,0),
         dummy2021 = ifelse(anio %in% 2021,1,0)) %>% 
  filter(anio != 2014,!age_group5_2 %in% c('75-79','80+')) %>% ungroup()

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

# Compute CET by 5-year age group =========================================
aux <- NULL
for (age in c('0-9',age_groups5[-c(1:2,16:17)])){
  df_uce <- df_uce_tot %>% filter(age_group5_2 == age)
  MPBG_pc <- (df_uce$PBG_pc[df_uce$anio == 2023])*1000
  Mgs_pc <- mean(df_uce$gs_pc)
  MYLL_pc  <- mean(df_uce$YLL_pc)
  MQALY_pc  <- mean(df_uce$QALY_pc)
reg_posibles <- reg_posibles %>%
  mutate(
    beta_yll = NA_real_,
    beta_qaly = NA_real_,
    UCE_yll = NA_real_,
    UCE_qaly = NA_real_,
    prop_UCE_yll = NA_real_,
    prop_UCE_qaly = NA_real_
  )
for (i in seq_len(nrow(reg_posibles))) {
  Xs <- reg_posibles$controls_list[[i]]
  IV <- reg_posibles$instruments_list[[i]]
  tend_dum <- reg_posibles$tend_dum[i]
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
    beta_yll <- coef(modelo_yll)["lag_lgs_pc"]
    
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
    beta_qaly <- coef(modelo_qaly)["lag_lgs_pc"]
    
    UCE_yll  <- Mgs_pc / (beta_yll  * -MYLL_pc)
    UCE_qaly <- Mgs_pc / (beta_qaly * -MQALY_pc)
    
    prop_UCE_yll  <- UCE_yll  / MPBG_pc
    prop_UCE_qaly <- UCE_qaly / MPBG_pc
    
    reg_posibles$beta_yll[i] <- beta_yll
    reg_posibles$beta_qaly[i] <- beta_qaly
    reg_posibles$UCE_yll[i] <- UCE_yll
    reg_posibles$UCE_qaly[i] <- UCE_qaly
    reg_posibles$prop_UCE_yll[i] <- prop_UCE_yll
    reg_posibles$prop_UCE_qaly[i] <- prop_UCE_qaly
  }
  aux <- bind_rows(aux,reg_posibles %>% mutate(age_group5 = age))
}

tabF <- aux %>% rowwise() %>%
  mutate(controles = paste0(controls_list,collapse = ';'),instrumentos =paste0(instruments_list,collapse = ';')) %>%
  select(-controls_list,-instruments_list) %>%
  arrange(modelo)
tabF

tabla_age5 <-  tabF %>% mutate(
  modelo = factor(modelo, levels = paste0("E", 1:5)),
  age_group5 = factor(
    age_group5,
    levels = c(
      "0-9", "10-14", "15-19", "20-24", "25-29",
      "30-34", "35-39", "40-44", "45-49", "50-54",
      "55-59", "60-64", "65-69", "70-74"
    )
  ))

plot_qaly <- ggplot(
  tabla_age5,
  aes(
    x = age_group5,
    y = prop_UCE_qaly,
    group = modelo,
    linewidth = modelo == "E1",
    alpha = modelo == "E1",
    linetype = modelo,
    color = modelo
  )
) +
  geom_line() +
  geom_point(aes(size = modelo == "E1")) +
  scale_linewidth_manual(values = c(`TRUE` = 1.3, `FALSE` = 0.55), guide = "none") +
  scale_size_manual(values = c(`TRUE` = 2.4, `FALSE` = 1.4), guide = "none") +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.45), guide = "none") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  labs(
    x = "Age group",
    y = "CET / GGP per capita",
    color = "",
    linetype = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

plot_qaly
