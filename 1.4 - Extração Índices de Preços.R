############################################################################################################
#                             EXTRAÇÃO DE ÍNDICES DE PREÇOS PARA DEFLACIONAMENTO
############################################################################################################

rm(list = ls())

# Pacotes:
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(stringr)
library(htmltools)
library(htmlwidgets)
library(zeallot)
library(readxl)
library(gdata)
library(stringi)
library(Hmisc)



# Opções:
options(scipen = 999) # Desabilita notação scientífica. Para voltar ao padrão -> options(scipen = 1)

# -----------------------------------------------------------------------------------------------------------

# Extraindo os dados históricos do IPCA:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/IPCA")
ipca <- read.csv2("Série Histórica IPCA.csv", skip = 6, stringsAsFactors = FALSE,
                  col.names = c('ano', 'mes', 'indice_ipca', 'variacao_mes','variacao_3_meses',
                                'variacao_6_meses)', 'variacao_ano', 'variacao_12_meses'))
str(ipca)

# Arrumando o Data Frame:
ipca <- ipca %>%
  fill(ano) %>% 
  group_by(ano) %>% 
  mutate(num_mes = row_number()) %>%
  ungroup() %>% 
  select(ano, num_mes, mes, indice_ipca) %>% 
  group_by(ano) %>%
  summarise(ipca = round(mean(indice_ipca, na.rm = TRUE), digits = 2)) %>%
  ungroup() %>% 
  mutate(deflator_ipca_base_2016 = ipca[ano == 2016]/ipca)

ipca$ano <- as.character(ipca$ano)
# ---------------------------------------------------------------------------------------------------------


# Extraindo Dados do IGP-DI da FGV:
setwd('C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/FGV-IBRE')
dir()

# IGP-DI Anual:
igp_di_anual <- read.csv2("Série IGP-DI Anual.csv", col.names = c('ano', 'igp_di'))
igp_di_anual$igp_di <- round(igp_di_anual$igp_di, digits = 2)
rm(igp_di_anual)

# IGP-DI Mensal:
igp_di <- read.csv2("Série IGP-DI Mensal.csv", col.names = c('ano.mes', 'igp_di'))
igp_di$igp_di <- round(igp_di$igp_di, digits = 2)
igp_di$ano.mes <- as.character(igp_di$ano.mes)
igp_di <- igp_di %>% 
  separate(col = ano.mes, into = c('ano', 'mes'), sep = '\\.') %>%
  group_by(ano) %>%
  summarise(igp_di = round(mean(igp_di), digits = 2)) %>% 
  ungroup() %>% 
  mutate(deflator_igpdi_base_2016 = igp_di[ano == 2016]/igp_di)

igp_di$ano <- as.character(igp_di$ano)
# --------------------------------------------------------------------------------------------------------

# Extraindo Dados do IGP-M da FGV:
setwd('C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/FGV-IBRE')
dir()

# IGP-M Mensal:
igp_m <- read.csv2("Série IGP-M Mensal.csv", col.names = c('ano.mes', 'igp_m'))
igp_m$igp_m <- round(igp_m$igp_m, digits = 2)
igp_m$ano.mes <- as.character(igp_m$ano.mes)
igp_m <- igp_m %>% 
  separate(col = ano.mes, into = c('ano', 'mes'), sep = '\\.') %>%
  group_by(ano) %>%
  summarise(igp_m = round(mean(igp_m), digits = 2)) %>% 
  ungroup() %>% 
  mutate(deflator_igpm_base_2016 = igp_m[ano == 2016]/igp_m)

igp_m$ano <- as.character(igp_m$ano)
# -------------------------------------------------------------------------------------------------------------

# Unindo os índices em uma só base:
rm(igp_di_anual)
deflatores <- ipca %>% 
  full_join(igp_di) %>% 
  full_join(igp_m) %>% 
  arrange(ano)

# -----------------------------------------------------------------------------------------------------------

# Salvando a base de dados:
setwd('C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R')
keep(ipca, all = TRUE, sure = TRUE)   # ADAPTAR COMANDO CASO SE QUEIRA DEFLACIONAR USANDO OUTRO ÍNDICE
save(ipca, file = 'ipca')
rm(list = ls())
load("ipca")




