############################################################################################################
#                                   EXTRAÇÃO DADOS MUNIC
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



setwd('C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/MUNIC')
dir()


# MUNIC 2015

# Extraindo a base:
munic_2015 <- read_xls("Base_MUNIC_2015.xls", sheet = "Recursos humanos", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável', 'NÃ£o soube informar*'))

# Renomeando variáveis:
munic_2015 <- munic_2015 %>% 
  transmute(ano = 2015,
            codigo_municipio_ibge = A1,
            cod_uf = Codigouf,
            cod_mun = Codigomunicipio,
            municipio = Nome,
            MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
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
            MUNICIPIO = str_squish(MUNICIPIO),
            total_funcionarios_adm_direta = A2, 
            funcionarios_adm_direta_estatutario = A3,
            funcionarios_adm_direta_clt = A4,
            funcionarios_adm_direta_comissionados = A5,
            funcionarios_adm_direta_estagiarios = A6,
            funcionarios_adm_direta_nao_permanentes = A7,
            adm_indireta = A8,
            total_funcionarios_adm_indireta = A9,
            funcionarios_adm_indireta_estatutario = A10,
            funcionarios_adm_indireta_clt = A11,
            funcionarios_adm_indireta_comissionados = A12,
            funcionarios_adm_indireta_estagiarios = A13,
            funcionarios_adm_indireta_nao_permanentes = A14)


  


# --------------------------------------------------------------------------------------------------------------------------------------------------------

# MUNIC 2014

# Variáveis Externas:
variaveis_externas_2014 <- read_xls("Base_MUNIC_2014.xls", sheet = "Variáveis externas", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))

variaveis_externas_2014 <- variaveis_externas_2014 %>% 
  transmute(codigo_municipio_ibge = A1,
            cod_uf = A1022,
            cod_mun = A1023,
            municipio = A1027,
            MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
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



# Dados sobre funcionários públicos:
munic_2014 <- read_xls("Base_MUNIC_2014.xls", sheet = "Recursos humanos", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))

# Renomeando variáveis:
munic_2014 <- munic_2014 %>% 
    transmute(ano = 2014,
             codigo_municipio_ibge = A1,
             total_funcionarios_adm_direta = A2,
             funcionarios_adm_direta_estatutario = A8,
             funcionarios_adm_direta_clt = A14,
             funcionarios_adm_direta_comissionados = A20,
             funcionarios_adm_direta_estagiarios = A26,
             funcionarios_adm_direta_nao_permanentes = A29,
             adm_indireta = A37,
             total_funcionarios_adm_indireta = A38,
             funcionarios_adm_indireta_estatutario = A44,
             funcionarios_adm_indireta_clt = A50,
             funcionarios_adm_indireta_comissionados = A56,
             funcionarios_adm_indireta_estagiarios = A62,
             funcionarios_adm_indireta_nao_permanentes = A65)

# Merge:
munic_2014 <- left_join(munic_2014, variaveis_externas_2014) %>%
  select(ano, codigo_municipio_ibge, cod_uf, cod_mun, municipio, MUNICIPIO, everything())

# --------------------------------------------------------------------------------------------------------------------------------------------------------

# MUNIC 2013

# Variáveis Externas:
variaveis_externas_2013 <- read_xls("Base_MUNIC_2013.xls", sheet = "Variáveis externas", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))

variaveis_externas_2013 <- variaveis_externas_2013 %>% 
  transmute(codigo_municipio_ibge = A1,
            cod_uf = A391,
            cod_mun = A392,
            municipio = A394,
            MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
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



# Dados sobre funcionários públicos:
munic_2013 <- read_xls("Base_MUNIC_2013.xls", sheet = "Recursos humanos", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))
munic_2013[munic_2013 == "Não aplicável"] <- NA

# Renomeando variáveis:
munic_2013 <- munic_2013 %>% 
  transmute(ano = 2013,
           codigo_municipio_ibge = A1,
           total_funcionarios_adm_direta = A21,
           funcionarios_adm_direta_estatutario = A22,
           funcionarios_adm_direta_clt = A23,
           funcionarios_adm_direta_comissionados = A24,
           funcionarios_adm_direta_estagiarios = A25,
           funcionarios_adm_direta_nao_permanentes = A26,
           adm_indireta = A27,
           total_funcionarios_adm_indireta = A28,
           funcionarios_adm_indireta_estatutario = A29,
           funcionarios_adm_indireta_clt = A30,
           funcionarios_adm_indireta_comissionados = A31,
           funcionarios_adm_indireta_estagiarios = A32,
           funcionarios_adm_indireta_nao_permanentes = A33)


# Merge:
munic_2013 <- left_join(munic_2013, variaveis_externas_2013) %>%
  select(ano, codigo_municipio_ibge, cod_uf, cod_mun, municipio, MUNICIPIO, everything())

# -------------------------------------------------------------------------------------------------------------------


# MUNIC 2012

# Variáveis Externas:
variaveis_externas_2012 <- read_xls("Base_MUNIC_2012.xls", sheet = "Variaveis externas", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))

variaveis_externas_2012 <- variaveis_externas_2012 %>% 
  transmute(codigo_municipio_ibge = A1,
            cod_uf = A535,
            cod_mun = NA,
            municipio = A537,
            MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
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



# Dados sobre funcionários públicos:
munic_2012 <- read_xls("Base_MUNIC_2012.xls", sheet = "Recursos humanos", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))
munic_2012[munic_2012 == "Não aplicável"] <- NA
munic_2012[munic_2012 == "Ignorado"] <- NA

# Renomeando variáveis:
munic_2012 <- munic_2012 %>% 
  transmute(ano = 2012,
            codigo_municipio_ibge = A1,
            total_funcionarios_adm_direta = A2, 
            funcionarios_adm_direta_estatutario = A3,
            funcionarios_adm_direta_clt = A4,
            funcionarios_adm_direta_comissionados = A5,
            funcionarios_adm_direta_estagiarios = A6,
            funcionarios_adm_direta_nao_permanentes = A7,
            adm_indireta = A8,
            total_funcionarios_adm_indireta = A9,
            funcionarios_adm_indireta_estatutario = A10,
            funcionarios_adm_indireta_clt = A11,
            funcionarios_adm_indireta_comissionados = A12,
            funcionarios_adm_indireta_estagiarios = A13,
            funcionarios_adm_indireta_nao_permanentes = A14)

# Merge:
munic_2012 <- left_join(munic_2012, variaveis_externas_2012) %>%
  select(ano, codigo_municipio_ibge, cod_uf, cod_mun, municipio, MUNICIPIO, everything())

# -------------------------------------------------------------------------------------------------------------------


# MUNIC 2011

# Variáveis Externas:
variaveis_externas_2011 <- read_xls("Base_MUNIC_2011.xls", sheet = "Variáveis externas", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))

variaveis_externas_2011 <- variaveis_externas_2011 %>% 
  transmute(codigo_municipio_ibge = A1,
            cod_uf = A568,
            cod_mun = NA,
            municipio = A570,
            MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
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



# Dados sobre funcionários públicos:
munic_2011 <- read_xls("Base_MUNIC_2011.xls", sheet = "Recursos humanos", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))
munic_2011[munic_2011 == "Não aplicável"] <- NA

# Renomeando variáveis:
munic_2011 <- munic_2011 %>% 
  transmute(ano = 2011,
            codigo_municipio_ibge = A1,
            total_funcionarios_adm_direta = A2,
            funcionarios_adm_direta_estatutario = A8,
            funcionarios_adm_direta_clt = A14,
            funcionarios_adm_direta_comissionados = A20,
            funcionarios_adm_direta_estagiarios = A26,
            funcionarios_adm_direta_nao_permanentes = A29,
            adm_indireta = A35,
            total_funcionarios_adm_indireta = A36,
            funcionarios_adm_indireta_estatutario = A42,
            funcionarios_adm_indireta_clt = A48,
            funcionarios_adm_indireta_comissionados = A54,
            funcionarios_adm_indireta_estagiarios = A60,
            funcionarios_adm_indireta_nao_permanentes = A63)

# Merge:
munic_2011 <- left_join(munic_2011, variaveis_externas_2011) %>%
  select(ano, codigo_municipio_ibge, cod_uf, cod_mun, municipio, MUNICIPIO, everything())

# -------------------------------------------------------------------------------------------------------------------


# MUNIC 2009

# Variáveis Externas:
variaveis_externas_2009 <- read_xls("Base_MUNIC_2009.xls", sheet = "Variáveis externas", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))

variaveis_externas_2009 <- variaveis_externas_2009 %>% 
  transmute(codigo_municipio_ibge = A1,
            cod_uf = A738,
            cod_mun = NA,
            municipio = A741,
            MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
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


# Dados sobre funcionários públicos:
munic_2009 <- read_xls("Base_MUNIC_2009.xls", sheet = "Recursos humanos", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))
munic_2009[munic_2009 == "Não aplicável"] <- NA

# Renomeando variáveis:
munic_2009 <- munic_2009 %>% 
  transmute(ano = 2009,
            codigo_municipio_ibge = A1,
            total_funcionarios_adm_direta = A8,
            funcionarios_adm_direta_estatutario = A9,
            funcionarios_adm_direta_clt = A10,
            funcionarios_adm_direta_comissionados = A11,
            funcionarios_adm_direta_estagiarios = A12,
            funcionarios_adm_direta_nao_permanentes = A13,
            adm_indireta = A14,
            total_funcionarios_adm_indireta = A15,
            funcionarios_adm_indireta_estatutario = A16,
            funcionarios_adm_indireta_clt = A17,
            funcionarios_adm_indireta_comissionados = A18,
            funcionarios_adm_indireta_estagiarios = A19,
            funcionarios_adm_indireta_nao_permanentes = A20)


# Merge:
munic_2009 <- left_join(munic_2009, variaveis_externas_2009) %>%
  select(ano, codigo_municipio_ibge, cod_uf, cod_mun, municipio, MUNICIPIO, everything())


# -------------------------------------------------------------------------------------------------------------------


# MUNIC 2008

# Variáveis Externas:
variaveis_externas_2008 <- read_xls("Base_MUNIC_2008.xls", sheet = "Variáveis externas", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))

variaveis_externas_2008 <- variaveis_externas_2008 %>% 
  transmute(codigo_municipio_ibge = A1,
            cod_uf = A336,
            cod_mun = NA,
            municipio = A338,
            MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
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


# ADMINISTRAÇÃO DIRETA:
# Extraindo a base:
munic_2008_adm_direta <- read_xls("Base_MUNIC_2008.xls", sheet = "Adm direta", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))
munic_2008_adm_direta[munic_2008_adm_direta == "Não aplicável"] <- NA

# Renomeando variáveis:
munic_2008_adm_direta <- munic_2008_adm_direta %>% 
  transmute(ano = 2008,
            codigo_municipio_ibge = A1,
            total_funcionarios_adm_direta = A2,
            funcionarios_adm_direta_estatutario = A8,
            funcionarios_adm_direta_clt = A14,
            funcionarios_adm_direta_comissionados = A20,
            funcionarios_adm_direta_estagiarios = A26,
            funcionarios_adm_direta_nao_permanentes = A29)


# ADMINISTRAÇÃO INDIRETA:
# Extraindo a base:
munic_2008_adm_indireta <- read_xls("Base_MUNIC_2008.xls", sheet = "Adm indireta", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável', '	Não aplicável'))
munic_2008_adm_indireta[munic_2008_adm_indireta == "Não aplicável"] <- NA

# Renomeando variáveis:
munic_2008_adm_indireta <- munic_2008_adm_indireta %>% 
  transmute(ano = 2008,
            codigo_municipio_ibge = A1,
            adm_indireta = A35,
            total_funcionarios_adm_indireta = A36,
            funcionarios_adm_indireta_estatutario = A42,
            funcionarios_adm_indireta_clt = A48,
            funcionarios_adm_indireta_comissionados = A54,
            funcionarios_adm_indireta_estagiarios = A60,
            funcionarios_adm_indireta_nao_permanentes = A63)


# Juntando as informações da Administração Direta e Indireta na mesma base:
munic_2008 <- full_join(munic_2008_adm_direta, munic_2008_adm_indireta)
rm(munic_2008_adm_direta, munic_2008_adm_indireta)

# Merge:
munic_2008 <- left_join(munic_2008, variaveis_externas_2008) %>%
  select(ano, codigo_municipio_ibge, cod_uf, cod_mun, municipio, MUNICIPIO, everything())


# -------------------------------------------------------------------------------------------------------------------


# MUNIC 2006

# Variáveis Externas:
variaveis_externas_2006 <- read_xls("Base_MUNIC_2006.xls", sheet = "Variáveis externas", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))

variaveis_externas_2006 <- variaveis_externas_2006 %>% 
  transmute(codigo_municipio_ibge = A1,
            cod_uf = A356,
            cod_mun = NA,
            municipio = A357,
            MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
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



# ADMINISTRAÇÃO DIRETA:
# Extraindo a base:
munic_2006_adm_direta <- read_xls("Base_MUNIC_2006.xls", sheet = "Adm direta", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))
munic_2006_adm_direta[munic_2006_adm_direta == "Não aplicável"] <- NA

# Renomeando variáveis:
munic_2006_adm_direta <- munic_2006_adm_direta %>% 
  transmute(ano = 2006,
            codigo_municipio_ibge = A1,
            total_funcionarios_adm_direta = A41,
            funcionarios_adm_direta_estatutario = A42,
            funcionarios_adm_direta_clt = A43,
            funcionarios_adm_direta_comissionados = A44,
            funcionarios_adm_direta_nao_permanentes = A45)


# ADMINISTRAÇÃO INDIRETA:
# Extraindo a base:
munic_2006_adm_indireta <- read_xls("Base_MUNIC_2006.xls", sheet = "Adm indireta", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))
munic_2006_adm_indireta[munic_2006_adm_indireta == "Não aplicável"] <- NA

# Renomeando variáveis:
munic_2006_adm_indireta <- munic_2006_adm_indireta %>% 
  transmute(ano = 2006,
            codigo_municipio_ibge = A1,
            adm_indireta = A46,
            total_funcionarios_adm_indireta = A47,
            funcionarios_adm_indireta_estatutario = A48,
            funcionarios_adm_indireta_clt = A49,
            funcionarios_adm_indireta_comissionados = A50,
            funcionarios_adm_indireta_nao_permanentes = A51)


# Juntando as informações da Administração Direta e Indireta na mesma base:
munic_2006 <- full_join(munic_2006_adm_direta, munic_2006_adm_indireta)
rm(munic_2006_adm_direta, munic_2006_adm_indireta)

# Merge:
munic_2006 <- left_join(munic_2006, variaveis_externas_2006) %>%
  select(ano, codigo_municipio_ibge, cod_uf, cod_mun, municipio, MUNICIPIO, everything())

# -------------------------------------------------------------------------------------------------------------------


# MUNIC 2005


# Variáveis Externas:
variaveis_externas_2005 <- read_xls("Base_MUNIC_2005.xls", sheet = "Variáveis externas", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))

variaveis_externas_2005 <- variaveis_externas_2005 %>% 
  transmute(codigo_municipio_ibge = A1,
            cod_uf = A325,
            cod_mun = NA,
            municipio = A326,
            MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
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


# ADMINISTRAÇÃO DIRETA:
# Extraindo a base:
munic_2005_adm_direta <- read_xls("Base_MUNIC_2005.xls", sheet = "Adm Direta", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))
munic_2005_adm_direta[munic_2005_adm_direta == "Não aplicável"] <- NA

# Renomeando variáveis:
munic_2005_adm_direta <- munic_2005_adm_direta %>% 
  transmute(ano = 2005,
            codigo_municipio_ibge = A1,
            total_funcionarios_adm_direta = A9,
            funcionarios_adm_direta_estatutario = A14,
            funcionarios_adm_direta_clt = A19,
            funcionarios_adm_direta_comissionados = A24,
            funcionarios_adm_direta_nao_permanentes = A29)


# ADMINISTRAÇÃO INDIRETA:
# Extraindo a base:
munic_2005_adm_indireta <- read_xls("Base_MUNIC_2005.xls", sheet = "Adm Indireta", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))
munic_2005_adm_indireta[munic_2005_adm_indireta == "Não aplicável"] <- NA

# Renomeando variáveis:
munic_2005_adm_indireta <- munic_2005_adm_indireta %>% 
  transmute(ano = 2005,
            codigo_municipio_ibge = A1,
            adm_indireta = A34,
            total_funcionarios_adm_indireta = A39,
            funcionarios_adm_indireta_estatutario = A44,
            funcionarios_adm_indireta_clt = A49,
            funcionarios_adm_indireta_comissionados = A54,
            funcionarios_adm_indireta_nao_permanentes = A59)


# Juntando as informações da Administração Direta e Indireta na mesma base:
munic_2005 <- full_join(munic_2005_adm_direta, munic_2005_adm_indireta)
rm(munic_2005_adm_direta, munic_2005_adm_indireta)

# Merge:
munic_2005 <- left_join(munic_2005, variaveis_externas_2005) %>%
  select(ano, codigo_municipio_ibge, cod_uf, cod_mun, municipio, MUNICIPIO, everything())


# -------------------------------------------------------------------------------------------------------------------


# MUNIC 2004

variaveis_externas_2004 <- read_xls("Base_MUNIC_2004.xls", sheet = "Variáveis externas", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))

variaveis_externas_2004 <- variaveis_externas_2004 %>% 
  transmute(codigo_municipio_ibge = A1,
            cod_uf = as.numeric(str_extract_all(codigo_municipio_ibge, "^..")),
            cod_mun = NA,
            municipio = A164,
            MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
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


# ADMINISTRAÇÃO DIRETA:
# Extraindo a base:
munic_2004_adm_direta <- read_xls("Base_MUNIC_2004.xls", sheet = "Quadro adm direta", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))
munic_2004_adm_direta[munic_2004_adm_direta == "Não aplicável"] <- NA

# Renomeando variáveis:
munic_2004_adm_direta <- munic_2004_adm_direta %>% 
  transmute(ano = 2004,
            codigo_municipio_ibge = A1,
            total_funcionarios_adm_direta = A4,
            funcionarios_adm_direta_estatutario = A5,
            funcionarios_adm_direta_clt = A6,
            funcionarios_adm_direta_comissionados = A7,
            funcionarios_adm_direta_nao_permanentes = A8)


# ADMINISTRAÇÃO INDIRETA:
# Extraindo a base:
munic_2004_adm_indireta <- read_xls("Base_MUNIC_2004.xls", sheet = "Quadro adm indireta", na = c('', '-', 'Ignorado', 'Recusa', 'Não aplicável', 'Não Aplicável'))
munic_2004_adm_indireta[munic_2004_adm_indireta == "Não aplicável"] <- NA

# Renomeando variáveis:
munic_2004_adm_indireta <- munic_2004_adm_indireta %>% 
  transmute(ano = 2004,
            codigo_municipio_ibge = A1,
            adm_indireta = A12,
            total_funcionarios_adm_indireta = A17,
            funcionarios_adm_indireta_estatutario = A18,
            funcionarios_adm_indireta_clt = A19,
            funcionarios_adm_indireta_comissionados = A20,
            funcionarios_adm_indireta_nao_permanentes = A21)


# Juntando as informações da Administração Direta e Indireta na mesma base:
munic_2004 <- full_join(munic_2004_adm_direta, munic_2004_adm_indireta)
rm(munic_2004_adm_direta, munic_2004_adm_indireta)

# Merge:
munic_2004 <- left_join(munic_2004, variaveis_externas_2004) %>%
  select(ano, codigo_municipio_ibge, cod_uf, cod_mun, municipio, MUNICIPIO, everything())


# ----------------------------------------------------------------------------------------------------------

rm(list = ls(pattern = "^v"))
#termos_na <- c("ignorado", "Não Aplicável")


# Padronizando os tipos das variáveis:
padronizar_tipos <- function(base) {
  base <- base %>%
    mutate(adm_indireta = ifelse(adm_indireta == "Sim", 1, 0)) %>% 
    mutate_at(vars(ano:MUNICIPIO), as.character) %>%
    mutate_at(vars(adm_indireta), as.logical) %>% 
    mutate_at(vars(total_funcionarios_adm_direta:funcionarios_adm_direta_nao_permanentes,
                   total_funcionarios_adm_indireta:funcionarios_adm_indireta_nao_permanentes), as.integer)
    
}

munic_2004 <- padronizar_tipos(munic_2004)
munic_2005 <- padronizar_tipos(munic_2005)
munic_2006 <- padronizar_tipos(munic_2006)
munic_2008 <- padronizar_tipos(munic_2008)
munic_2009 <- padronizar_tipos(munic_2009)
munic_2011 <- padronizar_tipos(munic_2011)
munic_2012 <- padronizar_tipos(munic_2012)
munic_2013 <- padronizar_tipos(munic_2013)
munic_2014 <- padronizar_tipos(munic_2014)
munic_2015 <- padronizar_tipos(munic_2015)


# Salvando:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
save(list = ls(pattern = 'munic'), file = 'munics')
rm(list = ls())
load('munics')

######################################################################################################

# # Empilhando a base de cada ano
# munic <- munic_2004 %>%
#   full_join(munic_2005, by = "codigo_municipio_ibge", suffix = c("_2004", "_2005"))
# 
# # Empilhando a base de cada ano
# munic <- munic_2004 %>%
#   full_join(munic_2005, by = "codigo_municipio_ibge", suffix = c("_2004", "_2005")) %>%
#   full_join(munic_2006, by = "codigo_municipio_ibge", suffix = c("", "_2006")) %>%
#   full_join(munic_2008, by = "codigo_municipio_ibge", suffix = c("", "_2008")) %>%
#   full_join(munic_2009, by = "codigo_municipio_ibge", suffix = c("", "_2009")) %>%
#   full_join(munic_2011, by = "codigo_municipio_ibge", suffix = c("", "_2011")) %>%
#   full_join(munic_2012, by = "codigo_municipio_ibge", suffix = c("", "_2012")) %>%
#   full_join(munic_2013, by = "codigo_municipio_ibge", suffix = c("", "_2013")) %>%
#   full_join(munic_2014, by = "codigo_municipio_ibge", suffix = c("", "_2012")) %>%
#   full_join(munic_2015, by = "codigo_municipio_ibge", suffix = c("", "_2015"))
# 
# 
# 
# # O comando full_join não está funcionando adequadamente.
# # Para não perder muito tempo com isso, irei utilizaro comando mais simples rbind().
# # Porém, para isto terei que deixar todas as bases com exatamente as mesmas colunas:
# 
# # Mantendo todas as bases com as mesmas colunas:
# munic_2004 <- munic_2004 %>% select(-adm_indireta)
# munic_2005 <- munic_2005 %>% select(-adm_indireta)
# munic_2006 <- munic_2006 %>% select(-adm_indireta)
# munic_2008 <- munic_2008 %>% select(-adm_indireta, -funcionarios_adm_direta_estagiarios, -funcionarios_adm_indireta_estagiarios)
# munic_2009 <- munic_2009 %>% select(-adm_indireta, -funcionarios_adm_direta_estagiarios, -funcionarios_adm_indireta_estagiarios)
# munic_2011 <- munic_2011 %>% select(-adm_indireta, -funcionarios_adm_direta_estagiarios, -funcionarios_adm_indireta_estagiarios)
# munic_2012 <- munic_2012 %>% select(-adm_indireta, -funcionarios_adm_direta_estagiarios, -funcionarios_adm_indireta_estagiarios)
# munic_2013 <- munic_2013 %>% select(-adm_indireta, -funcionarios_adm_direta_estagiarios, -funcionarios_adm_indireta_estagiarios)
# munic_2014 <- munic_2014 %>% select(-adm_indireta, -funcionarios_adm_direta_estagiarios, -funcionarios_adm_indireta_estagiarios)
# munic_2015 <- munic_2015 %>% select(-adm_indireta, -funcionarios_adm_direta_estagiarios, -funcionarios_adm_indireta_estagiarios)


munic <- bind_rows(munic_2004, munic_2005, munic_2006, munic_2008, munic_2009,
                    munic_2011, munic_2012, munic_2013, munic_2014, munic_2015) %>% 
  select(codigo_municipio_ibge, ano, everything()) %>% 
  arrange(codigo_municipio_ibge, ano) %>% 
  select(-c(adm_indireta, funcionarios_adm_direta_estagiarios, funcionarios_adm_indireta_estagiarios))
         

munic[munic == "Não soube informar*"] <- NA

keep(munic, all = TRUE, sure = TRUE)

# Agregando as variáveis da administração direta e indireta:
munic <- munic %>%
  mutate(total_servidores = rowSums(select(., total_funcionarios_adm_direta, total_funcionarios_adm_indireta), na.rm = TRUE),
         servidores_estatutarios = rowSums(select(., funcionarios_adm_direta_estatutario, funcionarios_adm_indireta_estatutario), na.rm = TRUE),
         servidores_comissionados = rowSums(select(., funcionarios_adm_direta_comissionados, funcionarios_adm_indireta_comissionados), na.rm = TRUE),
         servidores_clt = rowSums(select(., funcionarios_adm_direta_clt, funcionarios_adm_indireta_clt), na.rm = TRUE),
         servidores_nao_permanentes = rowSums(select(., funcionarios_adm_direta_nao_permanentes, funcionarios_adm_indireta_nao_permanentes), na.rm = TRUE)) %>% 
  select(codigo_municipio_ibge:MUNICIPIO, total_servidores:servidores_nao_permanentes) %>%
  rename(cod_municipio_ibge = codigo_municipio_ibge)



# Corrigindo o nome de alguns municípios:
munic$MUNICIPIO <- gsub(' +', ' ', munic$MUNICIPIO) 
munic$MUNICIPIO[munic$MUNICIPIO == "NOVO HORIZONTE DOESTE"] <- "NOVO HORIZONTE DO OESTE"
munic$MUNICIPIO[munic$MUNICIPIO == "ELDORADO DOS CARAJAS"] <- "ELDORADO DO CARAJAS"
munic$MUNICIPIO[munic$MUNICIPIO == "SALINOPLIS"] <- "SALINOPOLIS"
munic$MUNICIPIO[munic$MUNICIPIO == "SANTA ISABEL DO PARA"] <- "SANTA IZABEL DO PARA"
munic$MUNICIPIO[munic$MUNICIPIO == "COUTO DE MAGALHAES"] <- "COUTO MAGALHAES"
munic$MUNICIPIO[munic$MUNICIPIO == "SAO VALERIO DA NATIVIDADE"] <- "SAO VALERIO"
munic$MUNICIPIO[munic$MUNICIPIO == "GOVERNADOR LUIS ROCHA"] <- "GOVERNADOR LUIZ ROCHA"
munic$MUNICIPIO[munic$MUNICIPIO == "SENADOR LA ROQUE"] <- "SENADOR LA ROCQUE"
munic$MUNICIPIO[munic$MUNICIPIO == "COCAL DO ALVES"] <- "COCAL DOS ALVES"
munic$MUNICIPIO[munic$MUNICIPIO == "LAGOA DO SAO FRANCISCO"] <- "LAGOA DE SAO FRANCISCO"
munic$MUNICIPIO[munic$MUNICIPIO == "PALMERAIS"] <- "PALMEIRAIS"
munic$MUNICIPIO[munic$MUNICIPIO == "SAO FRANCISCO DE ASSIS DO PIA"] <- "SAO FRANCISCO DE ASSIS DO PIAUI"
munic$MUNICIPIO[munic$MUNICIPIO == "DEPUTADO IRAPUAN RIBEIRO"] <- "DEPUTADO IRAPUAN PINHEIRO"
munic$MUNICIPIO[munic$MUNICIPIO == "DEPUTADO IRAPUAN RIBEIRO"] <- "DEPUTADO IRAPUAN PINHEIRO"
munic$MUNICIPIO[munic$MUNICIPIO == "ITAPAGE"] <- "ITAPAJE"
munic$MUNICIPIO[munic$MUNICIPIO == "PENA FORTE"] <- "PENAFORTE"
munic$MUNICIPIO[munic$MUNICIPIO == "SAO LUIZ DO CURU"] <- "SAO LUIS DO CURU"
munic$MUNICIPIO[munic$MUNICIPIO == "ALTO DOS RODRIGUES"] <- "ALTO DO RODRIGUES"
munic$MUNICIPIO <- ifelse(munic$cod_uf == 24 & munic$MUNICIPIO == "PRESIDENTE JUSCELINO", "SERRA CAIADA", munic$MUNICIPIO)
munic$MUNICIPIO[munic$MUNICIPIO == "BELEM DO BREJO DA CRUZ"] <- "BELEM DO BREJO DO CRUZ"
munic$MUNICIPIO <- ifelse(munic$cod_uf == 25 & munic$MUNICIPIO == "SANTAREM", "JOCA CLAUDINO", munic$MUNICIPIO)
munic$MUNICIPIO[munic$MUNICIPIO == "SAO DOMINGOS DE POMBAL"] <- "SAO DOMINGOS"
munic$MUNICIPIO[munic$MUNICIPIO == "SAO JOSE DO BREJO DA CRUZ"] <- "SAO JOSE DO BREJO DO CRUZ"
munic$MUNICIPIO[munic$MUNICIPIO == "SAO SEBASTIAO DE LAGOA DE ROC"] <- "SAO SEBASTIAO DE LAGOA DE ROCA"
munic$MUNICIPIO[munic$MUNICIPIO == "SERIDO"] <- "SAO VICENTE DO SERIDO"
munic$MUNICIPIO[munic$MUNICIPIO == "CAMPO DE SANTANA"] <- "TACIMA"
munic$MUNICIPIO[munic$MUNICIPIO == "VIEROPOLIS"] <- "VIEIROPOLIS"
munic$MUNICIPIO[munic$MUNICIPIO == "BELEM DE SAO FRANCISCO"] <- "BELEM DO SAO FRANCISCO"
munic$MUNICIPIO[munic$MUNICIPIO == "CARNAUBEIRAS DA PENHA"] <- "CARNAUBEIRA DA PENHA"
munic$MUNICIPIO[munic$MUNICIPIO == "IGUARACI"] <- "IGUARACY"
munic$MUNICIPIO[munic$MUNICIPIO == "LAGOA DO ITAENGA"] <- "LAGOA DE ITAENGA"
munic$MUNICIPIO[munic$MUNICIPIO == "SANTANA DE IPANEMA"] <- "SANTANA DO IPANEMA"
munic$MUNICIPIO[munic$MUNICIPIO == "SANTANA DE SAO FRANCISCO"] <- "SANTANA DO SAO FRANCISCO"
munic$MUNICIPIO[munic$MUNICIPIO == "GOVERNADOR LOMANTO JUNIOR"] <- "BARRO PRETO"
munic$MUNICIPIO[munic$MUNICIPIO == "MUQUEM DE SAO FRANCISCO"] <- "MUQUEM DO SAO FRANCISCO"
munic$MUNICIPIO <- ifelse(munic$cod_uf == 29 & munic$MUNICIPIO == "SANTA TERESINHA", "SANTA TEREZINHA", munic$MUNICIPIO)
munic$MUNICIPIO[munic$MUNICIPIO == "AMPARO DA SERRA"] <- "AMPARO DO SERRA"
munic$MUNICIPIO[munic$MUNICIPIO == "BRASOPOLIS"] <- "BRAZOPOLIS"
munic$MUNICIPIO[munic$MUNICIPIO == "CAPURITA"] <- "CAPUTIRA"
munic$MUNICIPIO[munic$MUNICIPIO == "DONA EUZEBIA"] <- "DONA EUSEBIA"
munic$MUNICIPIO[munic$MUNICIPIO == "ITABIRINHA DE MANTENA"] <- "ITABIRINHA"
munic$MUNICIPIO[munic$MUNICIPIO == "LUIZBURGO"] <- "LUISBURGO"
munic$MUNICIPIO[munic$MUNICIPIO == "MORRO DO GARCA"] <- "MORRO DA GARCA"
munic$MUNICIPIO[munic$MUNICIPIO == "SANTA RITA DO JACUTINGA"] <- "SANTA RITA DE JACUTINGA"
munic$MUNICIPIO[munic$MUNICIPIO == "SANTA RITA DO IBITIPOCA"] <- "SANTA RITA DE IBITIPOCA"
munic$MUNICIPIO[munic$MUNICIPIO == "SAO SEBASTIAO DA VARGEM ALEGR"] <- "SAO SEBASTIAO DA VARGEM ALEGRE"
munic$MUNICIPIO[munic$MUNICIPIO == "SAO TOME DAS LETRAS"] <- "SAO THOME DAS LETRAS"
munic$MUNICIPIO[munic$MUNICIPIO == "CACHOEIRO DO ITAPEMIRIM"] <- "CACHOEIRO DE ITAPEMIRIM"
munic$MUNICIPIO[munic$MUNICIPIO == "ARMACAO DE BUZIOS"] <- "ARMACAO DOS BUZIOS"
munic$MUNICIPIO[munic$MUNICIPIO == "PARATI"] <- "PARATY"
munic$MUNICIPIO[munic$MUNICIPIO == "TRAJANO DE MORAIS"] <- "TRAJANO DE MORAES"
munic$MUNICIPIO[munic$MUNICIPIO == "VARRE E SAI"] <- "VARRE SAI"
munic$MUNICIPIO[munic$MUNICIPIO == "BRODOSQUI"] <- "BRODOWSKI"
munic$MUNICIPIO[munic$MUNICIPIO == "EMBU"] <- "EMBU DAS ARTES"
munic$MUNICIPIO[munic$MUNICIPIO == "SAO MIGUEL DE TOUROS"] <- "SAO MIGUEL DO GOSTOSO"
munic$MUNICIPIO[munic$MUNICIPIO == "FLORINIA"] <- "FLORINEA"
munic$MUNICIPIO[munic$MUNICIPIO == "GUARAPARES"] <- "GUARARAPES"
munic$MUNICIPIO[munic$MUNICIPIO == "MOJI DAS CRUZES"] <- "MOGI DAS CRUZES"
munic$MUNICIPIO[munic$MUNICIPIO == "MOJI GUACU"] <- "MOGI GUACU"
munic$MUNICIPIO[munic$MUNICIPIO == "MOJI MIRIM"] <- "MOGI MIRIM"
munic$MUNICIPIO[munic$MUNICIPIO == "SAO LUIS DO PARAITINGA"] <- "SAO LUIZ DO PARAITINGA"
munic$MUNICIPIO[munic$MUNICIPIO == "SERRO AZUL"] <- "CERRO AZUL"
munic$MUNICIPIO[munic$MUNICIPIO == "VILA ALTA"] <- "ALTO PARAISO"
munic$MUNICIPIO[munic$MUNICIPIO == "PICARRAS"] <- "BALNEARIO PICARRAS"
munic$MUNICIPIO <- ifelse(munic$cod_uf == 42 & munic$MUNICIPIO == "PRESIDENTE CASTELO BRANCO", "PRESIDENTE CASTELLO BRANCO", munic$MUNICIPIO)
munic$MUNICIPIO[munic$MUNICIPIO == "SAO MIGUEL DOESTE"] <- "SAO MIGUEL DO OESTE"
munic$MUNICIPIO[munic$MUNICIPIO == "WITTMARSUM"] <- "WITMARSUM"
munic$MUNICIPIO[munic$MUNICIPIO == "SANT ANA DO LIVRAMENTO"] <- "SANTANA DO LIVRAMENTO"
munic$MUNICIPIO[munic$MUNICIPIO == "TAQUARACU DO SUL"] <- "TAQUARUCU DO SUL"
munic$MUNICIPIO[munic$MUNICIPIO == "DEADAPOLIS"] <- "DEODAPOLIS"
munic$MUNICIPIO[munic$MUNICIPIO == "VILA BELA DA SANTISSIMA TRIND"] <- "VILA BELA DA SANTISSIMA TRINDADE"
munic$MUNICIPIO[munic$MUNICIPIO == "POXOREO"] <- "POXOREU"
munic$MUNICIPIO[munic$MUNICIPIO == "ROSARIO DO OESTE"] <- "ROSARIO OESTE"
munic$MUNICIPIO[munic$MUNICIPIO == "AGUA LINDAS DE GOIAS"] <- "AGUAS LINDAS DE GOIAS"
munic$MUNICIPIO[munic$MUNICIPIO == "PALMEIRA DE GOIAS"] <- "PALMEIRAS DE GOIAS"
munic$MUNICIPIO[munic$MUNICIPIO == "ACU"] <- "ASSU"
munic$MUNICIPIO[munic$MUNICIPIO == "JANUARIO CICCO"] <- "BOA SAUDE"


# ----------------------------------------------------------------------------------------------------------------
# Verificando se existe alguma inconsistência entre os nomes dos municípios na base MUNIC e os nomes oficiais do IBGE:
load("municipios_ibge")
munic_anti <- munic %>%
  anti_join(municipios_ibge %>% select(cod_uf, MUNICIPIO, cod_municipio_ibge, cod_mun),
             by = c("cod_uf", "MUNICIPIO"))
rm(munic_anti)
# ----------------------------------------------------------------------------------------------------------------

# Corrigindo os códigos de municípios. Faremos isso unindo a base MUNIC à base MUNICIPIOS IBGE a partir do
# código da UF e nome do municipio. Assim extraímos os códigos corretos e adicionamos à base, podendo então nos livrar
# do código defeituos que há.

munic <- munic %>%
  full_join(municipios_ibge %>% select(cod_uf, MUNICIPIO, cod_municipio_ibge, cod_mun),
            by = c("cod_uf", "MUNICIPIO"), suffix = c("_incorreto", "")) %>% 
  select(-cod_municipio_ibge_incorreto, -cod_mun_incorreto) %>% 
  select(cod_municipio_ibge, ano, cod_uf, cod_mun, everything()) %>% 
  mutate_at(vars(total_servidores:servidores_nao_permanentes), as.integer)



# Salvando a base de dados
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
save(munic, file = "munic")
