############################################################################################################
#                                   EXTRAÇÃO PIB MUNICÍPIOS
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


# -----------------------------------------------------------------------------------------------------------------

# PERÍODO 2002-2016

# Extraindo dados do PIB dos municípios. Base extraída do IBGE (SIDRA).
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PIB dos Municipios")
pib_municipios_2002_2016 <- read.csv2("PIB Municipios 2002-2016 (IBGE-SIDRA).csv", skip = 3, na.strings = c("NA", "..."), stringsAsFactors = FALSE)
pib_municipios_2002_2016 <- pib_municipios_2002_2016[1:5570, ] # Eliminando as últimas linhas (possui apenas comentários)
names(pib_municipios_2002_2016) <- str_replace_all(names(pib_municipios_2002_2016), pattern = 'X', replacement = 'pib_')

pib_municipios_2002_2016 <- pib_municipios_2002_2016 %>%
  separate(col = Município, into = c("municipio", "uf"), sep = " [(]") %>%
  select(-Nível)

pib_municipios_2002_2016$uf <- str_remove_all(pib_municipios_2002_2016$uf, pattern = "[)]")

# Multiplicando ass variaveis PIB por mil (pois estão em milhares):
pib_municipios_2002_2016 <- pib_municipios_2002_2016 %>%
  rename(cod_municipio_ibge = Cód.) %>% 
  select(cod_municipio_ibge, uf, everything()) %>%
  mutate_at(vars(pib_2002:pib_2016), function(x) {x * 1000}) %>% 
  mutate(cod_uf = as.character(str_extract_all(cod_municipio_ibge, "^.."))) %>% 
  mutate(MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
         MUNICIPIO = str_trim(MUNICIPIO, side = "both"),
         MUNICIPIO = str_squish(MUNICIPIO),
         MUNICIPIO = str_replace_all(MUNICIPIO, "Ç", 'C'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "´", ''),
         MUNICIPIO = str_replace_all(MUNICIPIO, "'", ''),
         MUNICIPIO = str_replace_all(MUNICIPIO, "D |D   ", 'D'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "´", '')) %>%
  select(cod_municipio_ibge, cod_uf, uf, municipio, MUNICIPIO, everything())


# Transformando em formato Tidy:
pib_municipios_2002_2016_tidy <- pib_municipios_2002_2016 %>%
  pivot_longer(cols = pib_2002:pib_2016, names_to = 'ano', names_prefix = 'pib_', values_to = 'pib') %>% 
  mutate(MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
         MUNICIPIO = str_trim(MUNICIPIO, side = "both"),
         MUNICIPIO = str_squish(MUNICIPIO),
         MUNICIPIO = str_replace_all(MUNICIPIO, "Ç", 'C'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "´", ''),
         MUNICIPIO = str_replace_all(MUNICIPIO, "'", ''),
         MUNICIPIO = str_replace_all(MUNICIPIO, "D |D   ", 'D'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "´", '')) %>% 
  select(cod_municipio_ibge, cod_uf, uf, municipio, MUNICIPIO, ano, pib)






  
pib_municipios_2002_2016_tidy$ano <- as.numeric(pib_municipios_2002_2016_tidy$ano)

# -----------------------------------------------------------------------------------------------------------------

# PERÍODO 2000-2001

# Extraindo dados do PIB dos municípios. Base extraída do IBGE (SIDRA).
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/PIB dos Municipios")
pib_municipios_1999_2012 <- read.csv2("PIB Municipios 1999-2012 (IBGE-SIDRA).csv", skip = 3, na.strings = c("NA", "..."), stringsAsFactors = FALSE)
pib_municipios_1999_2012 <- pib_municipios_1999_2012[1:5565, ] # Eliminando as últimas linhas (possui apenas comentários)
names(pib_municipios_1999_2012) <- str_replace_all(names(pib_municipios_1999_2012), pattern = 'X', replacement = 'pib_')

pib_municipios_1999_2012 <- pib_municipios_1999_2012 %>%
  separate(col = Município, into = c("municipio", "uf"), sep = " [(]") %>%
  select(-Nível)

pib_municipios_1999_2012$uf <- str_remove_all(pib_municipios_1999_2012$uf, pattern = "[)]")

# Multiplicando as variaveis PIB por mil (pois estão em milhares):
pib_municipios_1999_2012 <- pib_municipios_1999_2012 %>%
  rename(cod_municipio_ibge = Cód.) %>% 
  select(cod_municipio_ibge, uf, everything()) %>%
  mutate_at(vars(pib_1999:pib_2012), function(x) {x * 1000}) %>% 
  mutate(cod_uf = as.character(str_extract_all(cod_municipio_ibge, "^.."))) %>% 
  mutate(MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
         MUNICIPIO = str_trim(MUNICIPIO, side = "both"),
         MUNICIPIO = str_squish(MUNICIPIO),
         MUNICIPIO = str_replace_all(MUNICIPIO, "Ç", 'C'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "´", ''),
         MUNICIPIO = str_replace_all(MUNICIPIO, "'", ''),
         MUNICIPIO = str_replace_all(MUNICIPIO, "D |D   ", 'D'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "´", ''))


# Restringindo apenas para os anos de interesse (2000 e 2001):
pib_municipios_2000_2001 <- pib_municipios_1999_2012 %>%
  select(cod_municipio_ibge, cod_uf, uf, municipio, MUNICIPIO, pib_2000, pib_2001)

rm(pib_municipios_1999_2012)

# Transformando em formato Tidy:
pib_municipios_2000_2001_tidy <- pib_municipios_2000_2001 %>%
  pivot_longer(cols = pib_2000:pib_2001, names_to = 'ano', names_prefix = 'pib_', values_to = 'pib') %>%
  mutate(MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
         MUNICIPIO = str_trim(MUNICIPIO, side = "both"),
         MUNICIPIO = str_squish(MUNICIPIO),
         MUNICIPIO = str_replace_all(MUNICIPIO, "Ç", 'C'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "´", ''),
         MUNICIPIO = str_replace_all(MUNICIPIO, "'", ''),
         MUNICIPIO = str_replace_all(MUNICIPIO, "D |D   ", 'D'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "´", '')) %>%
  select(cod_municipio_ibge, cod_uf, uf, municipio, MUNICIPIO, ano, pib)

pib_municipios_2000_2001_tidy$ano <- as.numeric(pib_municipios_2000_2001_tidy$ano)

# ----------------------------------------------------------------------------------------------------------

# OBS: Os dados da base 1999-2012 não batem com os dados da base 2002-2016. Por enquanto, creio que seja
# melhor utilizar apenas os dados de PIB entre 2002 e 2016, ignorando os anos de 2000 e 2001.

# Unindo as bases para termos observações de PIB entre 2000 e 2016:
pib_municipios_2000_2016 <- full_join(pib_municipios_2000_2001 %>% select(-municipio, -MUNICIPIO), pib_municipios_2002_2016,
                                      by = c("cod_municipio_ibge", "cod_uf", "uf")) %>%
  select(cod_municipio_ibge, cod_uf, uf, municipio, MUNICIPIO, everything()) %>%
  mutate(MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
          MUNICIPIO = str_trim(MUNICIPIO, side = "both"),
          MUNICIPIO = str_squish(MUNICIPIO),
          MUNICIPIO = str_replace_all(MUNICIPIO, "'", ""),
          MUNICIPIO = str_replace_all(MUNICIPIO, "Ç", 'C'),
          MUNICIPIO = str_replace_all(MUNICIPIO, "'", ''),
          MUNICIPIO = str_replace_all(MUNICIPIO, "D |D   ", 'D'),
          MUNICIPIO = str_replace_all(MUNICIPIO, "SUDMENNUCCI", 'SUD MENNUCCI'),
          MUNICIPIO = str_replace_all(MUNICIPIO, "DAVIDCANABARRO", 'DAVID CANABARRO'),
          MUNICIPIO = str_replace_all(MUNICIPIO, "BELFORDROXO", 'BELFORD ROXO'),
          MUNICIPIO = str_replace_all(MUNICIPIO, "OLHO D AGUA DO CASADO", 'OLHO DAGUA DO CASADO'),
          MUNICIPIO = str_replace_all(MUNICIPIO, "PINGO D AGUA", 'PINGO DAGUA'),
          MUNICIPIO = str_replace_all(MUNICIPIO, "´", ''))
            
  
  

# Transformando em formato Tidy:
pib_municipios_2000_2016_tidy <- pib_municipios_2000_2016 %>%
  pivot_longer(cols = pib_2000:pib_2016, names_to = 'ano', names_prefix = 'pib_', values_to = 'pib') %>%
  mutate(MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " ")) %>% 
  mutate(MUNICIPIO = str_replace_all(MUNICIPIO, "Ç", 'C')) %>%
  select(cod_municipio_ibge:municipio, MUNICIPIO, ano, pib) %>% 
  mutate(MUNICIPIO = str_replace_all(toupper(stri_trans_general(municipio, "Latin-ASCII")), "-", " "),
         MUNICIPIO = str_trim(MUNICIPIO, side = "both"),
         MUNICIPIO = str_squish(MUNICIPIO),
         MUNICIPIO = str_replace_all(MUNICIPIO, "'", ""),
         MUNICIPIO = str_replace_all(MUNICIPIO, "Ç", 'C'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "'", ''),
         MUNICIPIO = str_replace_all(MUNICIPIO, "D |D   ", 'D'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "SUDMENNUCCI", 'SUD MENNUCCI'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "DAVIDCANABARRO", 'DAVID CANABARRO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "BELFORDROXO", 'BELFORD ROXO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "OLHO D AGUA DO CASADO", 'OLHO DAGUA DO CASADO'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "PINGO D AGUA", 'PINGO DAGUA'),
         MUNICIPIO = str_replace_all(MUNICIPIO, "´", ''))

pib_municipios_2000_2016_tidy$ano <- as.numeric(pib_municipios_2000_2016_tidy$ano)

# Corrigindo o nome de alguns municípios:
pib_municipios_2000_2016_tidy$MUNICIPIO[pib_municipios_2000_2016_tidy$MUNICIPIO == "ACU"] <- "ASSU"
pib_municipios_2000_2016_tidy$MUNICIPIO[pib_municipios_2000_2016_tidy$MUNICIPIO == "JANUARIO CICCO"] <- "BOA SAUDE"


# Salvando:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
keep(pib_municipios_2000_2016_tidy, all = TRUE, sure = TRUE)
save(pib_municipios_2000_2016_tidy, file = 'pib_municipios')



# ----------------------------------------------------------------------------------------------------------------
# Verificando se existe alguma inconsistência entre os nomes dos municípios na base MUNIC e os nomes oficiais do IBGE:
load("municipios_ibge")
pib_anti <- pib_municipios_2000_2016_tidy %>%
  anti_join(municipios_ibge %>% select(cod_uf, MUNICIPIO, cod_municipio_ibge, cod_mun),
            by = c("cod_uf", "MUNICIPIO"))
rm(pib_anti)
# ----------------------------------------------------------------------------------------------------------------

