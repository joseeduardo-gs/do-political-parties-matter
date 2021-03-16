############################################################################################################
#                     CÓDIGOS DOS MUNICÍPIOS - IBGE
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
library(stringi)
library(Hmisc)


# Extraindo a base de dados do IBGE que lista os códigos dos municípios:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/Código dos Municípios")
municipios_ibge <- read_xls("RELATORIO_DTB_BRASIL_MUNICIPIO.xls")

municipios_ibge <- municipios_ibge %>%
  select(cod_uf = UF, uf = Nome_UF, cod_municipio_ibge = `Código Município Completo`,
         cod_mun_0 = Município, municipio = Nome_Município) %>% 
  mutate(MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
         MUNICIPIO = str_replace_all(MUNICIPIO, "Ç", 'C'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "'", ''),
         MUNICIPIO = str_replace_all(MUNICIPIO, "D |D   ", 'D'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "SUDMENNUCCI", 'SUD MENNUCCI'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "DAVIDCANABARRO", 'DAVID CANABARRO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "BELFORDROXO", 'BELFORD ROXO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "OLHO D AGUA DO CASADO", 'OLHO DAGUA DO CASADO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "PINGO D AGUA", 'PINGO DAGUA'),
         MUNICIPIO = str_trim(MUNICIPIO, side = "both"),
         MUNICIPIO = str_squish(MUNICIPIO))

municipios_ibge$cod_mun <- as.character(as.integer(municipios_ibge$cod_mun_0))
municipios_ibge$MUNICIPIO <- str_replace_all(municipios_ibge$MUNICIPIO, "-", " ")
#label(municipios_ibge$MUNICIPIO) <- "Nome simplificado do município (sem acentos ou hífens)"


# Corrigindo erros nos nomes de municípios:
municipios_ibge$MUNICIPIO[municipios_ibge$MUNICIPIO == "SANTO ANTONIO DO ICA	"] <- "SANTO ANTONIO DO ICA"
municipios_ibge$MUNICIPIO[municipios_ibge$MUNICIPIO == "ACU"] <- "ASSU"
municipios_ibge$MUNICIPIO[municipios_ibge$MUNICIPIO == "JANUARIO CICCO"] <- "BOA SAUDE"
# municipios_ibge$MUNICIPIO[municipios_ibge$MUNICIPIO == "AUGUSTO SEVERO"] <- "CAMPO GRANDE"


# Salvando a base de dados:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
save(municipios_ibge, file = "municipios_ibge")


rm(list = ls())
load("municipios_ibge")
