############################################################################################################
#                                   MANIPULAÇÃO BASE DE DADOS FINAL
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
library(foreign)


# Opções:
options(scipen = 999) # Desabilita notação scientífica. Para voltar ao padrão -> options(scipen = 1)

setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
load("eleicoes")
load("finbra_munic")

# -----------------------------------------------------------------------------------------------------------------


# Unindo a base de dados de Outcomes e de variáveis eleitorais:
base_pronta <- inner_join(finbra_munic, eleicao_pronta %>% select(-c(sigla_uf, MUNICIPIO))) %>% 
  mutate(partido_de_esquerda = as.numeric(ideologia_partido_eleito == "esquerda"))

# -----------------------------------------------------------------------------------------------------------------

# Base de Dados que mostra somente os outcomes para o último ano de mandato:
finbra_munic_last_year <- finbra_munic %>% filter(ano %in% c(2004, 2008, 2012, 2016))
base_pronta_last_year <- inner_join(finbra_munic_last_year %>% select(-c(sigla_uf, MUNICIPIO)), eleicao_pronta) %>% 
  mutate(partido_de_esquerda = as.numeric(ideologia_partido_eleito == "esquerda"))


# -----------------------------------------------------------------------------------------------------------------

# Uma outra alternativa é apresentar a base de dados ao nível Município-Mandato (como Brollo 2016),
# agregando as variáveis como sendo a média ao longo dos anos do mandato:

finbra_munic_mandato <- full_join(finbra_munic %>% group_by(cod_municipio_ibge, mandato) %>% summarise_at(vars(cod_uf:MUNICIPIO), first),
                                  finbra_munic %>% group_by(cod_municipio_ibge, mandato) %>% summarise_at(vars(pop:proporcao_brancos), mean, na.rm = T))

  
# Substituindo os valores NaN por NA. Como o método is.nan() não funciona para data frames, criamos primeiramente
# uma função que torna isto possível:
is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

finbra_munic_mandato[is.nan(finbra_munic_mandato)] <- NA
# rm(is.nan.data.frame)

# Agora unindo a base de dados de outcomes a nivel de municipio-mandato com os dados eleitorais:
base_pronta_mandato <- inner_join(finbra_munic_mandato, eleicao_pronta %>% select(-c(sigla_uf, MUNICIPIO))) %>% 
  mutate(partido_de_esquerda = as.numeric(ideologia_partido_eleito == "esquerda")) %>% 
  ungroup()


# -----------------------------------------------------------------------------------------------------------------

# Base de dados no nível município-mandato que apresenta as variáveis como
# a taxa de crescimento entre o primeiro e último ano de mandato:
finbra_munic_first_last_year <- finbra_munic %>%
  filter(ano %in% c(2001, 2004, 2005, 2008, 2009, 2012, 2013, 2016)) %>%
  arrange(cod_municipio_ibge, ano)
  
finbra_munic_growth <- finbra_munic_first_last_year %>%
  select(-c(pop, fracao_pop_masculina:proporcao_brancos)) %>% 
  group_by(cod_municipio_ibge, mandato) %>%
  mutate_at(vars(despesa_geral:servidores_comissionados_mil), function(x) {(x - lag(x))/lag(x)}) %>%
  ungroup() %>% 
  filter(ano %in% c(2004, 2008, 2012, 2016)) %>% 
  select(-ano)
# rename_at(vars(despesa_geral:servidores_comissionados_mil), function(x) paste0(x, "_gw")) %>%

# Substituindo os valores NaN e Inf por NA:
finbra_munic_growth[is.nan(finbra_munic_growth)] <- NA
finbra_munic_growth[finbra_munic_growth == Inf] <- NA
rm(is.nan.data.frame)


# Unindo com a base de dados eleitorais:
base_pronta_growth <- inner_join(finbra_munic_growth, eleicao_pronta %>% select(-c(sigla_uf, MUNICIPIO))) %>% 
  mutate(partido_de_esquerda = as.numeric(ideologia_partido_eleito == "esquerda")) %>% 
  ungroup()

keep(list = ls(pattern = "base"), all = T, sure = T)

# --------------------------------------------------------------------------------------------------------------------

# Diretório para salvar as bases de dados finais prontas para análise:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases de Dados Prontas")

# Salvando as bases de dados:
save(list = ls(), file = "bases_prontas_ECD")   # Salvando em formato R
# save(list = ls(), file = "bases_prontas_ED")   # Salvando em formato R

# Salvando a base de dados também em arquivo dta para se utilizar no Stata:
write.dta(base_pronta_mandato, file = "base_pronta_mandato_ECD.dta")
write.dta(base_pronta_last_year, file = "base_pronta_last_year_ECD.dta")
write.dta(base_pronta_growth, file = "base_pronta_growth_ECD.dta")

# write.dta(base_pronta_mandato, file = "base_pronta_mandato_ED.dta")
# write.dta(base_pronta_last_year, file = "base_pronta_last_year_ED.dta")
# write.dta(base_pronta_growth, file = "base_pronta_growth_ED.dta")

# -------------------------------------------------------------------------------------------------------------------
is_grouped_df(base_pronta)
is_grouped_df(base_pronta_mandato)
is_grouped_df(base_pronta_last_year)
is_grouped_df(base_pronta_growth)

# Analisando quais partidos estão sendo considerados:
table(base_pronta_mandato$ideologia_partido_eleito)
summarytools::freq(base_pronta_mandato$ideologia_partido_eleito)
summarytools::descr(base_pronta_mandato$margem_vitoria_esquerda, transpose = T, stats = "common", round.digits = 3)