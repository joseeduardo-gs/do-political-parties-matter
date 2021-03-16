############################################################################################################
#                                   ANÁLISE EMPÍRICA - ESTIMAÇÃO RDD NÃO-PARAMÉTRICO
############################################################################################################

rm(list = ls())
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
library(gmodels)
library(broom)

# Pacotes para estatísticas descritivas:
library(psych)
library(summarytools)

# Pacotes Específicos para se trabalhar com RDD:
library(rdd) # Pacote mais básico
library(rddtools)  # Pacote com muitas opções de testes de placebo e sensibilidade das estimações e estimação paramétrica
library(rdrobust)  # Pacote mais abrangente
library(rddapp)    # Pacote com interface em Shiny, reduz a necessidade de se saber programar.
library(rddensity)

# Pacotes para se visualizar resultados de modelos:
library(stargazer)
library(jtools)
library(huxtable)
library(texreg)
library(export)

# Pacote para regressões de Dados em Painel:
library(plm)


# Opções:
options(scipen = 999) # Desabilita notação scientífica. Para voltar ao padrão -> options(scipen = 1)

# Baixando as bases de dados de interesse:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
load("bases_prontas")

# Definindo o diretório para o R salvar as tabelas geradas:
setwd("C:/Users/joseg_000.PC-JE/Google Drive/FGV-EPGE/Dissertação/Tabelas Geradas no R/Análise com partidos de esquerda, centro e direita")


# Analisando quais partidos estão sendo considerados:
table(base_pronta_mandato$ideologia_partido_eleito)
summarytools::freq(base_pronta_mandato$ideologia_partido_eleito)
summarytools::descr(base_pronta_mandato$margem_vitoria_esquerda, transpose = T, stats = "common", round.digits = 3)



# Lista de variáveis outcome de interesse:
# outcomes <- c("despesa_geral_pc", "despesa_geral_pib",
#               "total_receitas_pc", "total_receitas_pib",
#               "receita_tributaria_pc", "receita_tributaria_pib",
#               "educacao_pc", "educacao_pib", "educacao_prop"
#               "saude_pc", "saude_pib", "saude_prop"
#               "assistencia_social_pc", "assistencia_social_pib", "assistencia_social_prop"
#               "total_servidores_pc",
#               "servidores_comissionados_pc")



# Lista de variáveis de controle de interesse:
# covariates <- c("regiao", "pop", "fracao_pop_masculina", "fracao_pop_urbana",
#                 "proporcao_idoso", "proporcao_jovem", "proporcao_brancos", "pib_pc")



# -------------------------------------------------------------------------------------------------------------
attach(base_pronta_mandato)

# -------------------------------------------------------------------------------------------------------------
# Estimação RDD não-paramétrica SEM COVARIATES usando o pacote rdrobust: (np significa NÃO-PARAMÉTRICO):

np_rdd_unconditional_1  <- rdrobust(y = log(despesa_geral_pc), x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T)
np_rdd_unconditional_2  <- rdrobust(y = log(despesa_geral_pib), x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T)
np_rdd_unconditional_3  <- rdrobust(y = log(total_receitas_pc), x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T)
np_rdd_unconditional_4  <- rdrobust(y = log(total_receitas_pib), x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T)
np_rdd_unconditional_5  <- rdrobust(y = log(receita_tributaria_pc), x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T)
np_rdd_unconditional_6  <- rdrobust(y = log(receita_tributaria_pib), x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T)
np_rdd_unconditional_7  <- rdrobust(y = log(servidores_comissionados_mil + 0.0001), x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T)
np_rdd_unconditional_8  <- rdrobust(y = educacao_prop, x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T)
np_rdd_unconditional_9  <- rdrobust(y = saude_prop, x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T)
np_rdd_unconditional_10 <- rdrobust(y = assistencia_social_prop + 0.00001, x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T)

summary(np_rdd_unconditional_1)
summary(np_rdd_unconditional_2)
summary(np_rdd_unconditional_3)
summary(np_rdd_unconditional_4)
summary(np_rdd_unconditional_5)
summary(np_rdd_unconditional_6)
summary(np_rdd_unconditional_7)
summary(np_rdd_unconditional_8)
summary(np_rdd_unconditional_9)
summary(np_rdd_unconditional_10)




# ------------------------------------------------------------------------------------------------

# Estimação RDD não-paramétrica COM COVARIATES usando o pacote rdrobust: (np significa NÃO-PARAMÉTRICO):
covariates <- cbind(pop, fracao_pop_masculina, fracao_pop_urbana, pib_pc)
np_rdd_conditional_1  <- rdrobust(y = log(despesa_geral_pc), x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T, covs = covariates)
np_rdd_conditional_2  <- rdrobust(y = log(despesa_geral_pib), x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T, covs = covariates)
np_rdd_conditional_3  <- rdrobust(y = log(total_receitas_pc), x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T, covs = covariates)
np_rdd_conditional_4  <- rdrobust(y = log(total_receitas_pib), x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T, covs = covariates)
np_rdd_conditional_5  <- rdrobust(y = log(receita_tributaria_pc), x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T, covs = covariates)
np_rdd_conditional_6  <- rdrobust(y = log(receita_tributaria_pib), x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T, covs = covariates)
np_rdd_conditional_7  <- rdrobust(y = log(servidores_comissionados_mil + 0.0001), x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T, covs = covariates)
np_rdd_conditional_8  <- rdrobust(y = educacao_prop, x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T, covs = covariates)
np_rdd_conditional_9  <- rdrobust(y = saude_prop, x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T, covs = covariates)
np_rdd_conditional_10 <- rdrobust(y = assistencia_social_prop + 0.00001, x = margem_vitoria_esquerda, c = 0, vce = "nn", cluster = cod_municipio_ibge, all = T, covs = covariates)


summary(np_rdd_conditional_1)
summary(np_rdd_conditional_2)
summary(np_rdd_conditional_3)
summary(np_rdd_conditional_4)
summary(np_rdd_conditional_5)
summary(np_rdd_conditional_6)
summary(np_rdd_conditional_7)
summary(np_rdd_conditional_8)
summary(np_rdd_conditional_9)
summary(np_rdd_conditional_10)



detach(base_pronta_mandato)