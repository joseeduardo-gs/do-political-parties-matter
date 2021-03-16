############################################################################################################
#                                   MANIPULAÇÃO DADOS FINBRA
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
# library(deflateBR)
# library(BETS)


# Opções:
options(scipen = 999) # Desabilita notação scientífica. Para voltar ao padrão -> options(scipen = 1)

# Carregando as bases de dados extraídas nas etapas anteriores:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
dir()
load("bases_finbra_siconfi")
load("finbra_antigos")

# ---- FINBRA SICONFI

# Restringindo as Categorias de Despesa por Função:
finbra_desp_funcao_siconfi_restrita <- finbra_desp_funcao_siconfi %>% 
  select(uf:despesas_exceto_intraorcamentarias, assistencia_social:educacao,
         urbanismo, transporte, desporto_e_lazer, seguranca_publica, gestao_ambiental)

# Unindo as bases de dados FINBRA recentes (SICONFI):
finbra_siconfi <- finbra_desp_orcamentarias_siconfi %>%
  full_join(finbra_receitas_orcamentarias_siconfi %>% select(-municipio, -pop, -uf), by = c("cod_municipio_ibge", "ano")) %>% 
  full_join(finbra_desp_funcao_siconfi_restrita %>% select(-municipio, -pop, -uf), by = c("cod_municipio_ibge", "ano"))

# Criando Variáveis:
finbra_siconfi$cod_uf <- str_extract_all(finbra_siconfi$cod_municipio_ibge, "^..")
finbra_siconfi$cod_mun <- str_remove_all(finbra_siconfi$cod_municipio_ibge, "^..")


# Criando uma variável com os nomes dos municípios com letras maiúsculas, sem acentos e sem hifens:
finbra_siconfi <- finbra_siconfi %>% 
  mutate(MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
         MUNICIPIO = str_replace_all(MUNICIPIO, "Ç", 'C'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "'", ''),
         MUNICIPIO = str_replace_all(MUNICIPIO, "D |D   ", 'D'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "SUDMENNUCCI", 'SUD MENNUCCI'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "DAVIDCANABARRO", 'DAVID CANABARRO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "BELFORDROXO", 'BELFORD ROXO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "OLHO D AGUA DO CASADO", 'OLHO DAGUA DO CASADO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "PINGO D AGUA", 'PINGO DAGUA'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "´", ''),
         MUNICIPIO = str_trim(MUNICIPIO, side = "both"),
         MUNICIPIO = str_squish(MUNICIPIO))

finbra_siconfi$cod_uf <- as.numeric(finbra_siconfi$cod_uf)

# Salvando:
save(finbra_siconfi, file = "finbra_siconfi")

# --------------------------------------------------------------------------------------------------------

# ---- FINBRA ANTIGOS

# Restringindo as Categorias de Despesa por Função:
finbra_despesas_por_funcao_antigas_restrita <- finbra_despesas_por_funcao_antigas %>% 
  select(uf:despesas_exceto_intraorcamentarias, assistencia_social:educacao,
         urbanismo, transporte, desporto_e_lazer, seguranca_publica, gestao_ambiental) %>%  
  mutate(MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
          MUNICIPIO = str_replace_all(MUNICIPIO, "Ç", 'C'),
          MUNICIPIO = str_replace_all(MUNICIPIO, "'", ''),
          MUNICIPIO = str_replace_all(MUNICIPIO, "D |D   ", 'D'),
          MUNICIPIO = str_replace_all(MUNICIPIO, "SUDMENNUCCI", 'SUD MENNUCCI'),
          MUNICIPIO = str_replace_all(MUNICIPIO, "DAVIDCANABARRO", 'DAVID CANABARRO'),
          MUNICIPIO = str_replace_all(MUNICIPIO, "BELFORDROXO", 'BELFORD ROXO'),
          MUNICIPIO = str_replace_all(MUNICIPIO, "OLHO D AGUA DO CASADO", 'OLHO DAGUA DO CASADO'),
          MUNICIPIO = str_replace_all(MUNICIPIO, "PINGO D AGUA", 'PINGO DAGUA'),
          MUNICIPIO = str_replace_all(MUNICIPIO, "´", ''),
          MUNICIPIO = str_trim(MUNICIPIO, side = "both"),
          MUNICIPIO = str_squish(MUNICIPIO))

finbra_despesas_orcamentarias_antigas <- finbra_despesas_orcamentarias_antigas %>%
  mutate(MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
         MUNICIPIO = str_replace_all(MUNICIPIO, "Ç", 'C'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "'", ''),
         MUNICIPIO = str_replace_all(MUNICIPIO, "D |D   ", 'D'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "SUDMENNUCCI", 'SUD MENNUCCI'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "DAVIDCANABARRO", 'DAVID CANABARRO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "BELFORDROXO", 'BELFORD ROXO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "OLHO D AGUA DO CASADO", 'OLHO DAGUA DO CASADO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "PINGO D AGUA", 'PINGO DAGUA'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "´", ''),
         MUNICIPIO = str_trim(MUNICIPIO, side = "both"),
         MUNICIPIO = str_squish(MUNICIPIO))

finbra_receitas_orcamentarias_antigas <- finbra_receitas_orcamentarias_antigas %>%
  mutate(MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
         MUNICIPIO = str_replace_all(MUNICIPIO, "Ç", 'C'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "'", ''),
         MUNICIPIO = str_replace_all(MUNICIPIO, "D |D   ", 'D'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "SUDMENNUCCI", 'SUD MENNUCCI'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "DAVIDCANABARRO", 'DAVID CANABARRO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "BELFORDROXO", 'BELFORD ROXO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "OLHO D AGUA DO CASADO", 'OLHO DAGUA DO CASADO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "PINGO D AGUA", 'PINGO DAGUA'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "´", ''),
         MUNICIPIO = str_trim(MUNICIPIO, side = "both"),
         MUNICIPIO = str_squish(MUNICIPIO))

# Unindo as bases de dados FINBRA antigas:
finbra_antigos <- finbra_despesas_orcamentarias_antigas %>%
  full_join(finbra_receitas_orcamentarias_antigas %>% select(-c(uf, municipio, pop, MUNICIPIO)), by = c("cod_uf", "cod_mun", "ano")) %>% 
  full_join(finbra_despesas_por_funcao_antigas_restrita %>% select(-c(uf, municipio, pop, MUNICIPIO)), by = c("cod_uf", "cod_mun", "ano"))

# Criando uma variável com os nomes dos municípios com letras maiúsculas e sem acentos:
finbra_antigos <- finbra_antigos %>% 
  mutate(MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
         MUNICIPIO = str_replace_all(MUNICIPIO, "Ç", 'C'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "'|`|,", ''),
         MUNICIPIO = str_replace_all(MUNICIPIO, "_", ' '),
         MUNICIPIO = str_replace_all(MUNICIPIO, "D |D   ", 'D'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "SUDMENNUCCI", 'SUD MENNUCCI'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "DAVIDCANABARRO", 'DAVID CANABARRO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "BELFORDROXO", 'BELFORD ROXO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "OLHO D AGUA DO CASADO", 'OLHO DAGUA DO CASADO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "PINGO D AGUA", 'PINGO DAGUA'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "´", ''),
         MUNICIPIO = str_trim(MUNICIPIO, side = "both"),
         MUNICIPIO = str_squish(MUNICIPIO))

finbra_antigos$cod_mun <- as.character(finbra_antigos$cod_mun)

# Salvando:
save(finbra_antigos, file = "finbra_antigos_juntos")


keep(finbra_antigos, finbra_siconfi, all = TRUE, sure = TRUE)



# # Realizando um full_join entre as bases (Append e Merge "ao mesmo tempo")
# finbra <- full_join(finbra_antigos, finbra_siconfi) %>%
#   arrange(uf, MUNICIPIO, ano)

# Unindo as bases:
finbra <- bind_rows(finbra_antigos, finbra_siconfi) %>%
  arrange(uf, MUNICIPIO, ano) %>% 
  mutate_at(vars(cod_uf), as.character)

# Removendo linhas que contém NAs demais:
finbra <- finbra[!is.na(finbra$uf), ]


# Corrigindo o nome de alguns municípios:
finbra$MUNICIPIO <- gsub(' +', ' ', finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "NOVO HORIZONTE DOESTE"] <- "NOVO HORIZONTE DO OESTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "ELDORADO DOS CARAJAS"] <- "ELDORADO DO CARAJAS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SALINOPLIS"] <- "SALINOPOLIS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SANTA ISABEL DO PARA"] <- "SANTA IZABEL DO PARA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "COUTO DE MAGALHAES"] <- "COUTO MAGALHAES"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO VALERIO DA NATIVIDADE"] <- "SAO VALERIO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "GOVERNADOR LUIS ROCHA"] <- "GOVERNADOR LUIZ ROCHA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SENADOR LA ROQUE"] <- "SENADOR LA ROCQUE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "COCAL DO ALVES"] <- "COCAL DOS ALVES"
finbra$MUNICIPIO[finbra$MUNICIPIO == "LAGOA DO SAO FRANCISCO"] <- "LAGOA DE SAO FRANCISCO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "PALMERAIS"] <- "PALMEIRAIS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO FRANCISCO DE ASSIS DO PIA"] <- "SAO FRANCISCO DE ASSIS DO PIAUI"
finbra$MUNICIPIO[finbra$MUNICIPIO == "DEPUTADO IRAPUAN RIBEIRO"] <- "DEPUTADO IRAPUAN PINHEIRO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "DEPUTADO IRAPUAN RIBEIRO"] <- "DEPUTADO IRAPUAN PINHEIRO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "ITAPAGE"] <- "ITAPAJE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "PENA FORTE"] <- "PENAFORTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO LUIZ DO CURU"] <- "SAO LUIS DO CURU"
finbra$MUNICIPIO[finbra$MUNICIPIO == "ALTO DOS RODRIGUES"] <- "ALTO DO RODRIGUES"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 24 & finbra$MUNICIPIO == "PRESIDENTE JUSCELINO", "SERRA CAIADA", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "BELEM DO BREJO DA CRUZ"] <- "BELEM DO BREJO DO CRUZ"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 25 & finbra$MUNICIPIO == "SANTAREM", "JOCA CLAUDINO", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO DOMINGOS DE POMBAL"] <- "SAO DOMINGOS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO JOSE DO BREJO DA CRUZ"] <- "SAO JOSE DO BREJO DO CRUZ"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO SEBASTIAO DE LAGOA DE ROC"] <- "SAO SEBASTIAO DE LAGOA DE ROCA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SERIDO"] <- "SAO VICENTE DO SERIDO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CAMPO DE SANTANA"] <- "TACIMA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "VIEROPOLIS"] <- "VIEIROPOLIS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "BELEM DE SAO FRANCISCO"] <- "BELEM DO SAO FRANCISCO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CARNAUBEIRAS DA PENHA"] <- "CARNAUBEIRA DA PENHA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "IGUARACI"] <- "IGUARACY"
finbra$MUNICIPIO[finbra$MUNICIPIO == "LAGOA DO ITAENGA"] <- "LAGOA DE ITAENGA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SANTANA DE IPANEMA"] <- "SANTANA DO IPANEMA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SANTANA DE SAO FRANCISCO"] <- "SANTANA DO SAO FRANCISCO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "GOVERNADOR LOMANTO JUNIOR"] <- "BARRO PRETO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "MUQUEM DE SAO FRANCISCO"] <- "MUQUEM DO SAO FRANCISCO"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 29 & finbra$MUNICIPIO == "SANTA TERESINHA", "SANTA TEREZINHA", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "AMPARO DA SERRA"] <- "AMPARO DO SERRA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "BRASOPOLIS"] <- "BRAZOPOLIS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CAPURITA"] <- "CAPUTIRA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "DONA EUZEBIA"] <- "DONA EUSEBIA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "ITABIRINHA DE MANTENA"] <- "ITABIRINHA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "LUIZBURGO"] <- "LUISBURGO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "MORRO DO GARCA"] <- "MORRO DA GARCA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SANTA RITA DO JACUTINGA"] <- "SANTA RITA DE JACUTINGA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SANTA RITA DO IBITIPOCA"] <- "SANTA RITA DE IBITIPOCA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO SEBASTIAO DA VARGEM ALEGR"] <- "SAO SEBASTIAO DA VARGEM ALEGRE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO TOME DAS LETRAS"] <- "SAO THOME DAS LETRAS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CACHOEIRO DO ITAPEMIRIM"] <- "CACHOEIRO DE ITAPEMIRIM"
finbra$MUNICIPIO[finbra$MUNICIPIO == "ARMACAO DE BUZIOS"] <- "ARMACAO DOS BUZIOS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "PARATI"] <- "PARATY"
finbra$MUNICIPIO[finbra$MUNICIPIO == "TRAJANO DE MORAIS"] <- "TRAJANO DE MORAES"
finbra$MUNICIPIO[finbra$MUNICIPIO == "VARRE E SAI"] <- "VARRE SAI"
finbra$MUNICIPIO[finbra$MUNICIPIO == "BRODOSQUI"] <- "BRODOWSKI"
finbra$MUNICIPIO[finbra$MUNICIPIO == "EMBU"] <- "EMBU DAS ARTES"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO MIGUEL DE TOUROS"] <- "SAO MIGUEL DO GOSTOSO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "FLORINIA"] <- "FLORINEA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "GUARAPARES"] <- "GUARARAPES"
finbra$MUNICIPIO[finbra$MUNICIPIO == "MOJI DAS CRUZES"] <- "MOGI DAS CRUZES"
finbra$MUNICIPIO[finbra$MUNICIPIO == "MOJI GUACU"] <- "MOGI GUACU"
finbra$MUNICIPIO[finbra$MUNICIPIO == "MOJI MIRIM"] <- "MOGI MIRIM"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO LUIS DO PARAITINGA"] <- "SAO LUIZ DO PARAITINGA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SERRO AZUL"] <- "CERRO AZUL"
finbra$MUNICIPIO[finbra$MUNICIPIO == "VILA ALTA"] <- "ALTO PARAISO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "PICARRAS"] <- "BALNEARIO PICARRAS"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 42 & finbra$MUNICIPIO == "PRESIDENTE CASTELO BRANCO", "PRESIDENTE CASTELLO BRANCO", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO MIGUEL DOESTE"] <- "SAO MIGUEL DO OESTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "WITTMARSUM"] <- "WITMARSUM"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SANT ANA DO LIVRAMENTO"] <- "SANTANA DO LIVRAMENTO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "TAQUARACU DO SUL"] <- "TAQUARUCU DO SUL"
finbra$MUNICIPIO[finbra$MUNICIPIO == "DEADAPOLIS"] <- "DEODAPOLIS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "VILA BELA DA SANTISSIMA TRIND"] <- "VILA BELA DA SANTISSIMA TRINDADE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "POXOREO"] <- "POXOREU"
finbra$MUNICIPIO[finbra$MUNICIPIO == "ROSARIO DO OESTE"] <- "ROSARIO OESTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "AGUA LINDAS DE GOIAS"] <- "AGUAS LINDAS DE GOIAS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "PALMEIRA DE GOIAS"] <- "PALMEIRAS DE GOIAS"

finbra$MUNICIPIO[finbra$MUNICIPIO == "MANUEL URBANO"] <- "MANOEL URBANO"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 12 & finbra$MUNICIPIO == "SANTA ROSA", "SANTA ROSA DO PURUS", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "SENADOR TEOTONIO VILELA"] <- "TEOTONIO VILELA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "BOA VISTA DE RAMOS"] <- "BOA VISTA DO RAMOS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "NOVO AYRAO"] <- "NOVO AIRAO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "AGUA BRANCA DO AMAPARI"] <- "PEDRA BRANCA DO AMAPARI"
finbra$MUNICIPIO[finbra$MUNICIPIO == "AMAPARI"] <- "PEDRA BRANCA DO AMAPARI"
finbra$MUNICIPIO[finbra$MUNICIPIO == "AGUA QUENTE"] <- "ERICO CARDOSO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "GOVERNADOR LOMANTO JUNIOR"] <- "BARRO PRETO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "QUINJINGUE"] <- "QUIJINGUE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "DEP IRAPUAN PINHEIRO"] <- "DEPUTADO IRAPUAN PINHEIRO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "EUZEBIO"] <- "EUSEBIO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "GRANGEIRO"] <- "GRANJEIRO"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 29 & finbra$MUNICIPIO == "OURO BRANCO", "PINDAI", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "ANSELMO DA FONSECA"] <- "CAEM"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 21 & finbra$MUNICIPIO == "AGUA DOCE", "AGUA DOCE DO MARANHAO", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "AGUAS LINDAS"] <- "AGUAS LINDAS DE GOIAS"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 22 & finbra$MUNICIPIO == "ALAGOINHA", "ALAGOINHA DO PIAUI", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "ALMERIM"] <- "ALMEIRIM"
finbra$MUNICIPIO[finbra$MUNICIPIO == "ALTA FLORESTA DO OESTE"] <- "ALTA FLORESTA DOESTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "ALTO DA BOA VISTA"] <- "ALTO BOA VISTA"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 52 & finbra$MUNICIPIO == "ALTO PARAISO", "ALTO PARAISO DE GOIAS", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "ALVORADA DO OESTE"] <- "ALVORADA DOESTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "AMPARO DO SAO FRANCISCO"] <- "AMPARO DE SAO FRANCISCO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "APARECIDA DO TABUADO"] <- "APARECIDA DO TABOADO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SENADOR CATUNDA"] <- "CATUNDA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "TEJUSSUOCA"] <- "TEJUCUOCA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SANTA MARIA DO JETIBA"] <- "SANTA MARIA DE JETIBA"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 32 & finbra$MUNICIPIO == "SAO DOMINGOS", "SAO DOMINGOS DO NORTE", finbra$MUNICIPIO)
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 52 & finbra$MUNICIPIO == "BOM JESUS", "BOM JESUS DE GOIAS", finbra$MUNICIPIO)
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 52 & finbra$MUNICIPIO == "MUNDO NOVO DE GOIAS", "MUNDO NOVO", finbra$MUNICIPIO)
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 52 & finbra$MUNICIPIO == "VALPARAISO", "VALPARAISO DE GOIAS", finbra$MUNICIPIO)
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 21 & finbra$MUNICIPIO == "BELA VISTA", "BELA VISTA DO MARANHAO", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "GOVERNADOR EDSON LOBAO"] <- "GOVERNADOR EDISON LOBAO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "LUIS DOMINGUES DO MARANHAO"] <- "LUIS DOMINGUES"	
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO RAIMUNDO DA DOCA BEZERRA"] <- "SAO RAIMUNDO DO DOCA BEZERRA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CONCEICAO DA PEDRA"] <- "CONCEICAO DAS PEDRAS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "GOUVEA"] <- "GOUVEIA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "ITAMOJI"] <- "ITAMOGI"
finbra$MUNICIPIO[finbra$MUNICIPIO == "BATAGUACU"] <- "BATAGUASSU"
finbra$MUNICIPIO[finbra$MUNICIPIO == "BATAIPORA"] <- "BATAYPORA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "JUTY"] <- "JUTI"
finbra$MUNICIPIO[finbra$MUNICIPIO == "FIGUEIROPOLES DOESTE"] <- "FIGUEIROPOLIS DOESTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "NOVA BANDEIRANTE"] <- "NOVA BANDEIRANTES"
finbra$MUNICIPIO[finbra$MUNICIPIO == "PORTO ESPEREDIAO"] <- "PORTO ESPERIDIAO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "QUATRO MARCOS"] <- "SAO JOSE DOS QUATRO MARCOS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "TAPURA"] <- "TAPURAH"
finbra$MUNICIPIO[finbra$MUNICIPIO == "VILA BELA STSSMA TRINDADE"] <- "VILA BELA DA SANTISSIMA TRINDADE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "VIZEU"] <- "VISEU"
finbra$MUNICIPIO[finbra$MUNICIPIO == "BARAUNAS"] <- "AREIA DE BARAUNAS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SANTA CECILIA DE UMBUZEIRO"] <- "SANTA CECILIA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO JOSE DO BREJO CRUZ"] <- "SAO JOSE DO BREJO DO CRUZ"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO SEB. DE LAGOA DE ROCA"] <- "SAO SEBASTIAO DE LAGOA DE ROCA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO SEBASTIAO DE LAGOA DE ROCA"] <- "SAO SEBASTIAO DE LAGOA DE ROCA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CABO"] <- "CABO DE SANTO AGOSTINHO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "ITAMARACA"] <- "ILHA DE ITAMARACA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "JABOATAO"] <- "JABOATAO DOS GUARARAPES"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO CAETANO"] <- "SAO CAITANO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CONSELHEIRO MAYRINCK"] <- "CONSELHEIRO MAIRINCK"
finbra$MUNICIPIO[finbra$MUNICIPIO == "DIAMANTE DO OESTE"] <- "DIAMANTE DOESTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "ITAGUAGE"] <- "ITAGUAJE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "ITAPEJARA DO OESTE"] <- "ITAPEJARA DOESTE"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 41 & finbra$MUNICIPIO == "LUISIANIA", "LUIZIANA", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "MOREIRA SALLES"] <- "MOREIRA SALES"
finbra$MUNICIPIO[finbra$MUNICIPIO == "MUNHOZ DE MELLO"] <- "MUNHOZ DE MELO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SANTA ANA DO ITARARE"] <- "SANTANA DO ITARARE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SANTA CRUZ DO MONTE CASTELO"] <- "SANTA CRUZ DE MONTE CASTELO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SANTA IZABEL DO IVAI"] <- "SANTA ISABEL DO IVAI"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CAMPOS"] <- "CAMPOS DOS GOYTACAZES"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 33 & finbra$MUNICIPIO == "CAMPO GRANDE", "PATY DO ALFERES", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "PATI DO ALFERES"] <- "PATY DO ALFERES"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 24 & finbra$MUNICIPIO == "CAMPO GRANDE", "AUGUSTO SEVERO", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "ESPIRITO SANTO DO OESTE"] <- "PARAU"
finbra$MUNICIPIO[finbra$MUNICIPIO == "FERNANDO PEDROSA"] <- "FERNANDO PEDROZA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "JANUARIO CICCO"] <- "BOA SAUDE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO JOSE DE CAMPESTRE"] <- "SAO JOSE DO CAMPESTRE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "ESPIGAO DO OESTE"] <- "ESPIGAO DOESTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "MACHADINHO DO OESTE"] <- "MACHADINHO DOESTE"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 11 & finbra$MUNICIPIO == "NOVA BRASILANDIA", "NOVA BRASILANDIA DOESTE", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "NOVA BRASILANDIA DO OESTE"] <- "NOVA BRASILANDIA DOESTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SANTA LUZIA DO OESTE"] <- "SANTA LUZIA DOESTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO FELIPE DO OESTE"] <- "SAO FELIPE DOESTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO LUIZ DO ANAUA"] <- "SAO LUIZ"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CHIAPETA"] <- "CHIAPETTA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO LUIS GONZAGA"] <- "SAO LUIZ GONZAGA"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 43 & finbra$MUNICIPIO == "TRINDADE", "TRINDADE DO SUL", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "BALNEARIO DE BARRA DO SUL"] <- "BALNEARIO BARRA DO SUL"
finbra$MUNICIPIO[finbra$MUNICIPIO == "BALNEARIO DE CAMBORIU"] <- "BALNEARIO CAMBORIU"
finbra$MUNICIPIO[finbra$MUNICIPIO == "HERVAL DO OESTE"] <- "HERVAL DOESTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "LAGEADO GRANDE"] <- "LAJEADO GRANDE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "LUIS ALVES"] <- "LUIZ ALVES"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CANINDE DO SAO FRANCISCO"] <- "CANINDE DE SAO FRANCISCO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "GRACCHO CARDOSO"] <- "GRACHO CARDOSO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "BADY BASSIT"] <- "BADY BASSITT"
finbra$MUNICIPIO[finbra$MUNICIPIO == "BERNADINO DE CAMPOS"] <- "BERNARDINO DE CAMPOS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "IPAUCU"] <- "IPAUSSU"
finbra$MUNICIPIO[finbra$MUNICIPIO == "LUISIANIA"] <- "LUIZIANIA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "PIRACUNUNGA"] <- "PIRASSUNUNGA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SALMORAO"] <- "SALMOURAO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SANTO ANTONIO DA POSSE"] <- "SANTO ANTONIO DE POSSE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SUDMENUCCI"] <- "SUD MENNUCCI"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SUZANOPOLIS"] <- "SUZANAPOLIS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "VALPARAIZO"] <- "VALPARAISO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CHAPADA DA AREIA"] <- "CHAPADA DE AREIA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "DARCYNOPOLIS"] <- "DARCINOPOLIS"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 17 & finbra$MUNICIPIO == "DIVINOPOLIS", "DIVINOPOLIS DO TOCANTINS", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "LAGEADO"] <- "LAJEADO"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 17 & finbra$MUNICIPIO == "MONTE SANTO", "MONTE SANTO DO TOCANTINS", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "OLIVEIRA DO TOCANTINS"] <- "OLIVEIRA DE FATIMA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "PINDORAMA DE GOIAS"] <- "PINDORAMA DO TOCANTINS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO VALERIO DO TOCANTINS"] <- "SAO VALERIO"
finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 17 & finbra$MUNICIPIO == "BANDEIRANTE", "BANDEIRANTES DO TOCANTINS", finbra$MUNICIPIO)

finbra$MUNICIPIO <- ifelse(finbra$cod_uf == 12 & finbra$MUNICIPIO == "SANTA ROSA_DO PURUS", "SANTA ROSA DO PURUS", finbra$MUNICIPIO)
finbra$MUNICIPIO[finbra$MUNICIPIO == "MARECHAL TAUMATURGO"] <- "MARECHAL THAUMATURGO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CARREIRO DA VARZEA"] <- "CAREIRO DA VARZEA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SERRA DO NAVIO (EX AGUA BRANCA DO AMAPARI)"] <- "SERRA DO NAVIO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "LIVRAMENTO DO BRUMADO"] <- "LIVRAMENTO DE NOSSA SENHORA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "GOUVERLANDIA"] <- "GOUVELANDIA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "ALTO JEQUITIBA (PRESIDENTE SOARES)"] <- "ALTO JEQUITIBA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CACHOEIRA DE PAJEU (EX ANDRE FERNANDES)"] <- "CACHOEIRA DE PAJEU"
finbra$MUNICIPIO[finbra$MUNICIPIO == "MATHIAS LOBATO (VILA MATIAS)"] <- "MATHIAS LOBATO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "PIUI"] <- "PIUMHI"
finbra$MUNICIPIO[finbra$MUNICIPIO == "QUELUZITA"] <- "QUELUZITO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO GONCALO DO RIO PRETO (EX FELISB.CALDEIRA)"] <- "SAO GONCALO DO RIO PRETO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SANTA CARMEN"] <- "SANTA CARMEM"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CAMPO DE SANTANA (EX TACIMA)"] <- "TACIMA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "RIACHAO DO BACAMARTE (EX ASSIS CHATEAUBRIAND)	"] <- "RIACHAO DO BACAMARTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "RIACHAO DO BACAMARTE (EX ASSIS CHATEAUBRIAND)	"] <- "RIACHAO DO BACAMARTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO BENTINHO (EX SAO BENTO DE POMBAL)"] <- "SAO BENTINHO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO BENTO DE POMBAL"] <- "SAO BENTINHO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "MOREILANDIA (EX SITIO DOS MOREIRAS)"] <- "MOREILANDIA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "LOGOA ALEGRE"] <- "LAGOA ALEGRE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "NOVA SANTA RITA (EX PETRONIO PORTELA)"] <- "NOVA SANTA RITA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "PAU DARCO DO PAIUI"] <- "PAU DARCO DO PIAUI"
finbra$MUNICIPIO[finbra$MUNICIPIO == "BELA VISTA DO CAROBA"] <- "BELA VISTA DA CAROBA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "BOA ESPERANACA DO IGUACU"] <- "BOA ESPERANCA DO IGUACU"
finbra$MUNICIPIO[finbra$MUNICIPIO == "GOIO ERE"] <- "GOIOERE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "NOVA LARANJEIRA"] <- "NOVA LARANJEIRAS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "ACU"] <- "ASSU"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CAMPO GRANDE (EX AUGUSTO SEVERO)"] <- "AUGUSTO SEVERO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CAMPO GRANDE ANTIGO AUGUSTO SEVERO"] <- "AUGUSTO SEVERO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "ESPIRITO SANTO DO OESTE (EX PARAU)"] <- "PARAU"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SERRA CAIADA (EX PRESIDENTE JUSCELINO)"] <- "SERRA CAIADA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "JAMARI"] <- "CANDEIAS DO JAMARI"
finbra$MUNICIPIO[finbra$MUNICIPIO == "NOVO HORIZONTE DOESTE (EX CACAIEIROS)"] <- "NOVO HORIZONTE DO OESTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "CAPITAO DO CIPO"] <- "CAPAO DO CIPO"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO DOMINGIS DO SUL"] <- "SAO DOMINGOS DO SUL"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO JOAO DO URTIGA"] <- "SAO JOAO DA URTIGA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "IPORA DOESTE"] <- "IPORA DO OESTE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "BOM SUCESSO DO ITARARE"] <- "BOM SUCESSO DE ITARARE"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SAO JOAO DO IRACEMA"] <- "SAO JOAO DE IRACEMA"
finbra$MUNICIPIO[finbra$MUNICIPIO == "AXIXA DE TOCANTINS"] <- "AXIXA DO TOCANTINS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "COLINAS DE TOCANTINS"] <- "COLINAS DO TOCANTINS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "JAU DO TOCANTIS"] <- "JAU DO TOCANTINS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "MOSQUITO"] <- "PALMEIRAS DO TOCANTINS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "SANTA TERESA DO TOCANTINS"] <- "SANTA TEREZA DO TOCANTINS"
finbra$MUNICIPIO[finbra$MUNICIPIO == "RIACHAO DO BACAMARTE (EX ASSIS CHATEAUBRIAND)"] <- "RIACHAO DO BACAMARTE"

finbra <- finbra %>% filter(municipio != "JAMARI")

# ----------------------------------------------------------------------------------------------------------------
# Verificando se existe alguma inconsistência entre os nomes dos municípios na base MUNIC e os nomes oficiais do IBGE:
load("municipios_ibge")
finbra_anti <- finbra %>%
  anti_join(municipios_ibge %>% select(cod_uf, MUNICIPIO, cod_municipio_ibge, cod_mun),
            by = c("cod_uf", "MUNICIPIO")) %>%
  select(cod_municipio_ibge, cod_uf, cod_mun, uf, MUNICIPIO, everything())
keep(finbra, all = TRUE, sure = TRUE)
# ----------------------------------------------------------------------------------------------------------------



# Corrigindo o problema com o código dos municípios. Uma vez que consertamos os nomes dos municípios, podemos fazer um
# merge com a base oficial do IBGE para recuperar os códigos corretos:
load("municipios_ibge")
finbra <- finbra %>% select(-c(cod_mun, municipio, cod_municipio_ibge)) %>% rename(sigla_uf = uf) %>% 
  left_join(municipios_ibge %>% select(-cod_mun_0), by = c("cod_uf", "MUNICIPIO")) %>% 
  select(cod_municipio_ibge, cod_uf, cod_mun, sigla_uf, uf, municipio, MUNICIPIO, ano, everything())  

# Adicionando um Label para a variável MUNICIPIO:
#label(finbra$MUNICIPIO) <- "Nome simplificado do município (sem acentos ou hífens)"


keep(finbra, all = TRUE, sure = TRUE)
save(finbra, file = "finbra")







# ---------------------------------------------------------------------------------------------------------------

# Adicionando Informações de PIB dos Municipios
load("pib_municipios")

# Unindo as bases de dados do Finbra e do PIB dos municípios:
finbra <- finbra %>%
  full_join(pib_municipios_2000_2016_tidy %>% select(cod_municipio_ibge, ano, pib), by = c("cod_municipio_ibge", "ano"))

# Arrumando e criando novas variáveis (PIB per capta):
# finbra <- finbra %>%
#   mutate(pib_pc = pib/pop)


# ----------------------------------------------------------------------------------------------------------------

# DEFLACIONAMENTO

# Carregando a base de dados de índices de preços para realizar o deflacionamento dos valores nominais:
setwd('C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R')
load("ipca")

# Unindo à base de dados do FINBRA com as informações de índices de preços:
finbra$ano <- as.character(finbra$ano)
ipca$ano <- as.character(ipca$ano)
finbra <- left_join(finbra, ipca)

# Deflacionando as variáveis de receitas e despesas, que se encontram em nível nominal, e adotando 2016 como ano base
# (para fazer isto, basta multiplicar o valor nominal pelo deflator previamente criado)
finbra_real <- finbra %>%
  mutate_at(vars(despesa_geral:pib), function(x) {x * finbra$deflator_ipca_base_2016}) %>% 
  select(-ipca, -deflator_ipca_base_2016)

rm(ipca, pib_municipios_2000_2016_tidy)


save(finbra_real, file = "finbra")




