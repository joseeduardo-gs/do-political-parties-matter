############################################################################################################
#                                   EXTRAÇÃO DADOS CENSO
############################################################################################################

rm(list = ls())

# PACOTES
# install.packages(c("httr", "usethis", "backports"))
# install.packages("backports")
# ?library(httr)
# library(usethis)
# library(backports)
# install.packages("devtools")
# library(devtools)
# devtools::install_github("lucasmation/microdadosBrasil", force = TRUE)
# library(microdadosBrasil)

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

# PNAD ---------------------------------------------------------------------------------------------------

# Baixando dados da PNAD:

# Testando especificações adicionais:
# download_sourceData("PNAD", 2001, unzip = T, replace = T, root_path = "C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PNAD")
# pnad_dom_2001 <- read_PNAD("domicilios", 2001)
# pnad_pes_2001 <- read_PNAD("pessoas", 2001)
# 
# download_sourceData("PNAD", 2002, unzip = T, replace = T, root_path = "C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PNAD")
# pnad_dom_2002 <- read_PNAD("domicilios", 2002)
# pnad_pes_2002 <- read_PNAD("pessoas", 2002)
# 
# download_sourceData("PNAD", 2003, unzip = T, replace = T, root_path = "C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PNAD")
# pnad_dom_2003 <- read_PNAD("domicilios", 2003)
# pnad_pes_2003 <- read_PNAD("pessoas", 2003)
# 
# download_sourceData("PNAD", 2004, unzip = T, replace = T, root_path = "C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PNAD")
# pnad_dom_2004 <- read_PNAD("domicilios", 2004)
# pnad_pes_2004 <- read_PNAD("pessoas", 2004)
# 
# download_sourceData("PNAD", 2005, unzip = T, replace = T, root_path = "C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PNAD")
# pnad_dom_2005 <- read_PNAD("domicilios", 2005)
# pnad_pes_2005 <- read_PNAD("pessoas", 2005)
# 
# download_sourceData("PNAD", 2006, unzip = T, replace = T, root_path = "C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PNAD")
# pnad_dom_2006 <- read_PNAD("domicilios", 2006)
# pnad_pes_2006 <- read_PNAD("pessoas", 2006)
# 
# download_sourceData("PNAD", 2007, unzip = T, replace = T, root_path = "C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PNAD")
# pnad_dom_2007 <- read_PNAD("domicilios", 2007)
# pnad_pes_2007 <- read_PNAD("pessoas", 2007)
# 
# download_sourceData("PNAD", 2008, unzip = T, replace = T, root_path = "C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PNAD")
# pnad_dom_2008 <- read_PNAD("domicilios", 2008)
# pnad_pes_2008 <- read_PNAD("pessoas", 2008)
# 
# download_sourceData("PNAD", 2009, unzip = T, replace = T, root_path = "C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PNAD")
# pnad_dom_2009 <- read_PNAD("domicilios", 2009)
# pnad_pes_2009 <- read_PNAD("pessoas", 2009)
# 
# download_sourceData("PNAD", 2010, unzip = T, replace = T, root_path = "C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PNAD")
# pnad_dom_2010 <- read_PNAD("domicilios", 2010)
# pnad_pes_2010 <- read_PNAD("pessoas", 2010)
# 
# download_sourceData("PNAD", 2011, unzip = T, replace = T, root_path = "C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PNAD")
# pnad_dom_2011 <- read_PNAD("domicilios", 2011)
# pnad_pes_2011 <- read_PNAD("pessoas", 2011)
# 
# download_sourceData("PNAD", 2012, unzip = T, replace = T, root_path = "C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PNAD")
# pnad_dom_2012 <- read_PNAD("domicilios", 2012)
# pnad_pes_2012 <- read_PNAD("pessoas", 2012)
# 
# download_sourceData("PNAD", 2013, unzip = T, replace = T, root_path = "C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PNAD")
# pnad_dom_2013 <- read_PNAD("domicilios", 2013)
# pnad_pes_2013 <- read_PNAD("pessoas", 2013)
# 
# download_sourceData("PNAD", 2014, unzip = T, replace = T, root_path = "C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PNAD")
# pnad_dom_2014 <- read_PNAD("domicilios", 2014)
# pnad_pes_2014 <- read_PNAD("pessoas", 2014)
# 
# download_sourceData("PNAD", 2015, unzip = T, replace = T, root_path = "C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PNAD")
# pnad_dom_2015 <- read_PNAD("domicilios", 2015)
# pnad_pes_2015 <- read_PNAD("pessoas", 2015)



# CENSO - GRUPOS DE IDADE ----------------------------------------------------------------------

# Dados do Censo extraídos através do sistema SIDRA do IBGE:

# População de acordo com faixas de idade:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/Censo")



# Censo 2000

pop_idade_2000 <- read.csv2('Populacao por grupos de idade 2000.csv', skip = 5, stringsAsFactors = FALSE, dec = ",")

pop_idade_2000$Total <- as.numeric(str_replace_all(pop_idade_2000$Total, ",", "\\."))

pop_idade_2000 <- pop_idade_2000 %>%
  select(cod_municipio_ibge = Cód., municipio = Município, grupo_idade = Grupo.de.idade, proporcao = Total) %>%
  separate(col = municipio, into = c("municipio", "sigla_uf"), sep = " \\(") %>% 
  mutate_at("sigla_uf", str_remove_all, pattern = "\\)") %>%
  mutate(proporcao = proporcao/100) %>% 
  filter(grupo_idade != "Total") %>% 
  mutate(faixa_etaria = case_when(grupo_idade %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos") ~ "jovem",
                                  grupo_idade %in% c("65 a 69 anos", "70 a 74 anos",
                                                     "75 a 79 anos", "80 a 84 anos", "85 a 89 anos",
                                                     "90 a 94 anos", "95 a 99 anos", "100 anos ou mais")
                                  ~ "idoso")) %>% 
  filter(nchar(cod_municipio_ibge) == 7)

pop_idade_2000$faixa_etaria = ifelse(! (pop_idade_2000$faixa_etaria %in% c('jovem', 'idoso')), "Adulto", pop_idade_2000$faixa_etaria)

pop_idade_2000 <- pop_idade_2000 %>%
  group_by(cod_municipio_ibge, faixa_etaria) %>% 
  summarise(proporcao = sum(proporcao)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = cod_municipio_ibge, names_from = faixa_etaria,
              names_prefix = "proporcao_", values_from = proporcao) %>% 
  mutate(ano = 2000) %>% 
  mutate(proporcao_idoso = 1 - proporcao_Adulto - proporcao_jovem)

# Censo 2010:

pop_idade_2010 <- read.csv2('Populacao por grupos de idade 2010.csv', skip = 5, stringsAsFactors = FALSE, dec = ",")

pop_idade_2010$Total <- as.numeric(str_replace_all(pop_idade_2010$Total, ",", "\\."))

pop_idade_2010 <- pop_idade_2010 %>%
  select(cod_municipio_ibge = Cód., municipio = Município, grupo_idade = Idade, proporcao = Total) %>%
  separate(col = municipio, into = c("municipio", "sigla_uf"), sep = " \\(") %>% 
  mutate_at("sigla_uf", str_remove_all, pattern = "\\)") %>%
  mutate(proporcao = proporcao/100) %>% 
  filter(grupo_idade != "Total") %>% 
  mutate(faixa_etaria = case_when(grupo_idade %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos") ~ "jovem",
                                  grupo_idade %in% c("65 a 69 anos", "70 anos ou mais") ~ "idoso")) %>% 
  filter(nchar(cod_municipio_ibge) == 7)

pop_idade_2010$faixa_etaria = ifelse(! (pop_idade_2010$faixa_etaria %in% c('jovem', 'idoso')), "Adulto", pop_idade_2010$faixa_etaria)

pop_idade_2010 <- pop_idade_2010 %>%
  group_by(cod_municipio_ibge, faixa_etaria) %>% 
  summarise(proporcao = sum(proporcao)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = cod_municipio_ibge, names_from = faixa_etaria,
              names_prefix = "proporcao_", values_from = proporcao) %>% 
  mutate(ano = 2010)


pop_idade <- full_join(pop_idade_2000, pop_idade_2010) %>% 
  select(cod_municipio_ibge, ano, everything()) %>% 
  arrange(cod_municipio_ibge, ano)


rm(pop_idade_2000, pop_idade_2010)
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
save(pop_idade, file = "pop_idade")



# CENSO - TAXA DE ALFABETIZAÇÃO -------------------------------------------
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/Censo")


# Taxa 2000:

alfabetizacao_2000 <- read.csv2('Taxa de Alfabetização 2000.csv', skip = 4, stringsAsFactors = FALSE, dec = ",")
alfabetizacao_2000 <- alfabetizacao_2000[1:77098, ]

alfabetizacao_2000 <- alfabetizacao_2000 %>%
  select(cod_municipio_ibge = Cód., municipio = Município, grupo_idade = Grupo.de.idade,
         alfabetizadas = Alfabetizadas, nao_alfabetizadas = Não.alfabetizadas) %>%
  separate(col = municipio, into = c("municipio", "sigla_uf"), sep = " \\(") %>% 
  mutate_at("sigla_uf", str_remove_all, pattern = "\\)") %>%
  filter(grupo_idade != "Total") %>% 
  filter(nchar(cod_municipio_ibge) == 7)


alfabetizacao_2000 <- alfabetizacao_2000 %>%
  mutate_at(vars(alfabetizadas, nao_alfabetizadas), as.integer) %>% 
  group_by(cod_municipio_ibge) %>% 
  summarise(alfabetizadas = sum(alfabetizadas), nao_alfabetizadas = sum(nao_alfabetizadas)) %>% 
  ungroup() %>% 
  transmute(cod_municipio_ibge, ano = 2000, taxa_alfabetizacao = alfabetizadas/(alfabetizadas + nao_alfabetizadas)) %>% 
  mutate_at("cod_municipio_ibge", as.character)


# Taxa 2010:

alfabetizacao_2010 <- read.csv2('Taxa de Alfabetização 2010.csv', skip = 5, stringsAsFactors = FALSE, dec = ",")
alfabetizacao_2010 <- alfabetizacao_2010[1:66780, ]

alfabetizacao_2010 <- alfabetizacao_2010 %>%
  select(cod_municipio_ibge = Cód., municipio = Município, grupo_idade = Idade,
         alfabetizadas = Alfabetizadas, nao_alfabetizadas = Não.alfabetizadas) %>%
  separate(col = municipio, into = c("municipio", "sigla_uf"), sep = " \\(") %>% 
  mutate_at("sigla_uf", str_remove_all, pattern = "\\)") %>%
  filter(grupo_idade != "Total") %>% 
  filter(nchar(cod_municipio_ibge) == 7)


alfabetizacao_2010 <- alfabetizacao_2010 %>%
  mutate_at(vars(alfabetizadas, nao_alfabetizadas), as.integer) %>% 
  group_by(cod_municipio_ibge) %>% 
  summarise(alfabetizadas = sum(alfabetizadas), nao_alfabetizadas = sum(nao_alfabetizadas)) %>% 
  ungroup() %>% 
  transmute(cod_municipio_ibge, ano = 2010, taxa_alfabetizacao = alfabetizadas/(alfabetizadas + nao_alfabetizadas)) %>% 
  mutate_at("cod_municipio_ibge", as.character)


alfabetizacao <- bind_rows(alfabetizacao_2000, alfabetizacao_2010) %>% 
  arrange(cod_municipio_ibge, ano)

setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
save(alfabetizacao, file = "alfabetizacao")
rm(alfabetizacao_2000, alfabetizacao_2010)



# CENSO - COR/RAÇA -------------------------------------------------------------

setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/Censo")

# Censo 2000:

cor_2000 <- read.csv2('População por cor 2000.csv', skip = 4, stringsAsFactors = FALSE, dec = ",", na.strings = "-")
cor_2000 <- cor_2000[-5566, ]

cor_2000 <- cor_2000 %>% 
  select(cod_municipio_ibge = Cód., municipio = Município, populacao_total = Total, brancas = Branca) %>% 
  transmute(cod_municipio_ibge, municipio, proporcao_brancos = brancas/populacao_total, ano = 2000) %>% 
  mutate_at("cod_municipio_ibge", as.character)


# Censo 2010:

cor_2010 <- read.csv2('Populacão por cor 2010.csv', skip = 4, stringsAsFactors = FALSE, dec = ",", na.strings = "-")
cor_2010 <- cor_2010[1:5565, ]

cor_2010 <- cor_2010 %>% 
  select(cod_municipio_ibge = Cód., municipio = Município, populacao_total = Total, brancas = Branca) %>% 
  transmute(cod_municipio_ibge, municipio, proporcao_brancos = brancas/populacao_total, ano = 2010)


cor_censo <- bind_rows(cor_2000, cor_2010) %>%
  select(cod_municipio_ibge, ano, everything()) %>% 
  arrange(cod_municipio_ibge, ano) %>% 
  separate(col = municipio, into = c("municipio", "sigla_uf"), sep = " \\(") %>% 
  mutate_at("sigla_uf", str_remove_all, pattern = "\\)") %>% 
  select(-municipio, -sigla_uf) %>%
  filter(nchar(cod_municipio_ibge) == 7)


setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
save(cor_censo, file = "cor_censo")
rm(cor_2000, cor_2010)


# Taxas de Homicídios -----------------------------------------------------

# Base extraída do IPEADATA:

# OBS: Taxa de homicídio X representa a taxa de homicídios de X por 100 mil pessoas.

setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IPEADATA")

tx_homicidios <- read.csv2("Taxas de Homicidio.csv", skip = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
  
tx_homicidios <- tx_homicidios %>% 
  rename(sigla_uf = Sigla, cod_municipio_ibge = Código, municipio = Município) %>% 
  rename_at(vars(starts_with('X2')), str_replace_all, pattern = "X", replacement = "taxa_") %>% 
  select(-X) %>% 
  pivot_longer(cols = starts_with("taxa"), names_to = 'ano', names_prefix = 'taxa_', values_to = 'taxa_homicidios') %>% 
  arrange(sigla_uf, municipio, ano) %>% 
  mutate_at("cod_municipio_ibge", as.character)

  

setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
save(tx_homicidios, file = "tx_homicidios")
  










