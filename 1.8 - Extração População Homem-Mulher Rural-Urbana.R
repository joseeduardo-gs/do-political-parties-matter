############################################################################################################
#                     EXTRAÇÃO DADOS POPULAÇÃO MASCULINA, FEMININA, RURAL E URBANA
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


# -----------------------------------------------------------------------------------------------------------------

setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/IBGE/População dos Municípios")
dir()

funcao_extrair_base <- function(base, variavel_interesse) {

base <- read.csv2(base, skip = 1)

base <- base %>% 
  rename(sigla_uf = Sigla, cod_municipio_ibge = Código, municipio = Município) %>% 
  pivot_longer(cols = starts_with("X"), names_to = 'ano', names_prefix = "X", values_to = variavel_interesse) %>% 
  select(cod_municipio_ibge, sigla_uf, municipio, ano, everything()) %>% 
  arrange(sigla_uf, municipio, ano)

}


# Aplicando esta função nas quatro bases de dados:

masculino <- funcao_extrair_base("populacao_homens.csv", variavel_interesse = "populacao_homens")
feminino <- funcao_extrair_base("populacao_mulheres.csv", variavel_interesse = "populacao_mulheres")
urbana <- funcao_extrair_base("populacao_urbana.csv", variavel_interesse = "populacao_urbana")
rural <- funcao_extrair_base("populacao_rural.csv", variavel_interesse = "populacao_rural")


# Unindo as bases:
variaveis_municipais <- masculino %>% 
  full_join(feminino %>% select(-sigla_uf, -municipio), by = c("cod_municipio_ibge", "ano")) %>% 
  full_join(urbana %>% select(-sigla_uf, -municipio), by = c("cod_municipio_ibge", "ano")) %>% 
  full_join(rural %>% select(-sigla_uf, -municipio), by = c("cod_municipio_ibge", "ano")) 


# Criando novas variáveis:
variaveis_municipais <- variaveis_municipais %>% 
  mutate(fracao_pop_masculina = populacao_homens/(populacao_homens + populacao_mulheres),
         fracao_pop_urbana = populacao_urbana/(populacao_urbana + populacao_rural)) %>% 
  mutate_at('cod_municipio_ibge', as.character)



setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
save(variaveis_municipais, file = "variaveis_municipais")
      