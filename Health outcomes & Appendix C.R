# Load libraries
library(tidyverse)
library(scales)
library(readxl)

# Data
file <- 'Data/Datos_regresiones.xlsx'
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

qaly_all <- readRDS('Data/QALY_mendoza.rds')
yll_all <- readRDS('Data/YLL_mendoza.rds')

# Consolidated table
df_uce <- qaly %>% full_join(yll,by = c('anio')) %>%
  left_join(regresion,by = c('anio' = 'Año')) %>% 
  mutate(YLL_pc = YLL*prop_ajuste/afiliados,lYLL_pc = log(YLL_pc), 
         QALY_pc = QALY*prop_ajuste/afiliados,lQALY_pc = log(QALY_pc))

# FIGURES ---------------------------------------------------------------------

# Health outcomes density
qaly_all %>% mutate(QALY = log(QALY)) %>%
  group_by(anio) %>%
  group_modify(~{
    d <- density(
      .x$QALY,
      kernel = "epanechnikov",
      na.rm = TRUE
    )
    data.frame(
      x = d$x,
      y = d$y
    )
  }) %>% ungroup() %>% 
  ggplot(aes(x = x,y = y,colour = factor(anio))) +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 12) +
  labs(x = "Logarithm QALY",y = "Density",col = '') + 
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(n.breaks = 8) +
  theme(legend.position = "bottom",panel.grid.minor = element_blank())

yll_all %>% mutate(YLL = log(YLL)) %>%
  group_by(anio) %>%
  group_modify(~{
    d <- density(
      .x$YLL,
      kernel = "epanechnikov",
      na.rm = TRUE
    )
    data.frame(
      x = d$x,
      y = d$y
    )
  }) %>% ungroup() %>% 
  ggplot(aes(x = x,y = y,colour = factor(anio))) +
  geom_line(linewidth = 1) +
  theme_minimal(base_size = 12) +
  labs(x = "Logarithm YLL",y = "Density",col = '') + 
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(n.breaks = 8) +
  theme(legend.position = "bottom",panel.grid.minor = element_blank())

# Appendix C
# Figure C1
factor_gs <- max(c(df_uce$YLL_pc, df_uce$QALY_pc), na.rm = TRUE) / 
  max(df_uce$gs_pc, na.rm = TRUE)
ggplot(df_uce, aes(x = anio)) +
  geom_line(aes(y = YLL_pc, color = "YLL per-capita"), linewidth = 1.4) +
  geom_point(aes(y = YLL_pc, color = "YLL per-capita"), size = 3) +
  geom_line(aes(y = QALY_pc, color = "QALY losses per-capita"),
            linewidth = 1.4, linetype = "dashed") +
  geom_point(aes(y = QALY_pc, color = "QALY losses per-capita"),
             size = 3, shape = 17) +
  geom_line(aes(y = gs_pc * factor_gs, color = "Per-capita public health expenditure"), 
            linewidth = 1.4, linetype = "dotdash") +
  geom_point(aes(y = gs_pc * factor_gs, color = "Per-capita public health expenditure"), 
             size = 3, shape = 15) +
  scale_y_continuous(
    name = "Health losses per-capita",
    labels = label_number(big.mark = ",", accuracy = 0.001),
    sec.axis = sec_axis(
      ~ . / factor_gs,
      name = "Per-capita public health expenditure",
      labels = label_number(big.mark = ",")
    )
  ) +
  scale_x_continuous(
    breaks = sort(unique(df_uce$anio))
  ) +
  labs(
    x = "Year",
    color = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y.left = element_text(size = 16, face = "bold"),
    axis.title.y.right = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Figure C2 Instruments
ggplot(df_uce, aes(x = anio)) +
  geom_line(aes(y = PBG_minero_2024/1e6, color = "Mining gross geographic product"), linewidth = 1.4) +
  geom_point(aes(y = PBG_minero_2024/1e6, color = "Mining gross geographic product"), size = 3) +
  scale_y_continuous(
    name = "Mining gross geographic product\n(x 1,000,000 ARS of 2024)",
    labels = label_number(big.mark = ","),n.breaks = 10) +
  scale_x_continuous(
    breaks = sort(unique(df_uce$anio))
  ) +
  labs(
    x = "Year",
    color = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y.left = element_text(size = 16, face = "bold"),
    axis.title.y.right = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.position = "none",
    legend.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggplot(df_uce, aes(x = anio)) +
  geom_line(aes(y = Regalias_2024/1e5, color = "Energy royalties"), linewidth = 1.4) +
  geom_point(aes(y = Regalias_2024/1e5, color = "Energy royalties"), size = 3) +
  scale_y_continuous(
    name = "Energy royalties\n(x 100,000 ARS of 2024)",
    labels = label_number(big.mark = ","),n.breaks = 10) +
  scale_x_continuous(
    breaks = sort(unique(df_uce$anio))
  ) +
  labs(
    x = "Year",
    color = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y.left = element_text(size = 16, face = "bold"),
    axis.title.y.right = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.position = "none",
    legend.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggplot(df_uce, aes(x = anio)) +
  geom_line(aes(y = Coparticipacion_2024, color = "Intergovernmental transfers per-capita"), linewidth = 1.4) +
  geom_point(aes(y = Coparticipacion_2024, color = "Intergovernmental transfers per-capita"), size = 3) +
  scale_y_continuous(
    name = "Intergovernmental transfers per-capita (ARS of 2024)",
    labels = label_number(big.mark = ",",accuracy = 0.01),n.breaks = 10) +
  scale_x_continuous(
    breaks = sort(unique(df_uce$anio))
  ) +
  labs(
    x = "Year",
    color = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y.left = element_text(size = 16, face = "bold"),
    axis.title.y.right = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.position = "none",
    legend.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggplot(df_uce, aes(x = anio)) +
  geom_line(aes(y = Origenpc_2024, color = "Origin-based per-capita fiscal resources"), linewidth = 1.4) +
  geom_point(aes(y = Origenpc_2024, color = "Origin-based per-capita fiscal resources"), size = 3) +
  scale_y_continuous(
    name = "Origin-based per-capita fiscal resources (ARS of 2024)",
    labels = label_number(big.mark = ",",accuracy = 0.01),n.breaks = 10) +
  scale_x_continuous(
    breaks = sort(unique(df_uce$anio))
  ) +
  labs(
    x = "Year",
    color = NULL
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y.left = element_text(size = 16, face = "bold"),
    axis.title.y.right = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    legend.position = "none",
    legend.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
