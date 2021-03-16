############################################################################################################
#                                   ANÁLISE EMPÍRICA - ESTIMAÇÃO RDD PARAMÉTRICO
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



# -----------------------------------------------------------------------------------------------------------------------

# Estimação RDD não-paramétrica usando o pacote rddtools


# Usando a função rddtools::rdd_data() para construir os RDD objects de interesse:

# Primeiramente, devemos armazenar os modelos RDD como rdd objects: (SEM COVARIATES)
# O prefixo p_ significa "PARAMÉTRICO":
np_rdd_object_unconditional_1  <- rdd_data(y = log(despesa_geral_pc), x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
np_rdd_object_unconditional_2  <- rdd_data(y = log(despesa_geral_pib), x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
np_rdd_object_unconditional_3  <- rdd_data(y = log(total_receitas_pc), x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
np_rdd_object_unconditional_4  <- rdd_data(y = log(total_receitas_pib), x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
np_rdd_object_unconditional_5  <- rdd_data(y = log(receita_tributaria_pc), x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
np_rdd_object_unconditional_6  <- rdd_data(y = log(receita_tributaria_pib), x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
np_rdd_object_unconditional_7  <- rdd_data(y = log(servidores_comissionados_mil + 0.0001), x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
np_rdd_object_unconditional_8  <- rdd_data(y = educacao_prop, x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
np_rdd_object_unconditional_9  <- rdd_data(y = saude_prop, x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
np_rdd_object_unconditional_10 <- rdd_data(y = assistencia_social_prop, x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)



# ------------------------------------------------------------------------------------------------------------

# REGRESSÕES RDD NÃO-PARAMÉTRICAS SEM COVARIATES:

# Regressões sem covariates, bandwidth IK:
# OBS: Não entendi por que os optimal bandwidths calculados são tão grandes!? Não deveriam ser pequenos para
# capturar somente observações que se encontram próximas ao cutoff?
np_ik_rdd_unconditional_1   <- rdd_reg_np(np_rdd_object_unconditional_1,  bw = rdd_bw_ik(np_rdd_object_unconditional_1,  kernel = "Uniform"), slope = "separate", inference = "np")
np_ik_rdd_unconditional_2   <- rdd_reg_np(np_rdd_object_unconditional_2,  bw = rdd_bw_ik(np_rdd_object_unconditional_2,  kernel = "Uniform"), slope = "separate", inference = "np")
np_ik_rdd_unconditional_3   <- rdd_reg_np(np_rdd_object_unconditional_3,  bw = rdd_bw_ik(np_rdd_object_unconditional_3,  kernel = "Uniform"), slope = "separate", inference = "np")
np_ik_rdd_unconditional_4   <- rdd_reg_np(np_rdd_object_unconditional_4,  bw = rdd_bw_ik(np_rdd_object_unconditional_4,  kernel = "Uniform"), slope = "separate", inference = "np")
np_ik_rdd_unconditional_5   <- rdd_reg_np(np_rdd_object_unconditional_5,  bw = rdd_bw_ik(np_rdd_object_unconditional_5,  kernel = "Uniform"), slope = "separate", inference = "np")
np_ik_rdd_unconditional_6   <- rdd_reg_np(np_rdd_object_unconditional_6,  bw = rdd_bw_ik(np_rdd_object_unconditional_6,  kernel = "Uniform"), slope = "separate", inference = "np")
np_ik_rdd_unconditional_7   <- rdd_reg_np(np_rdd_object_unconditional_7,  bw = rdd_bw_ik(np_rdd_object_unconditional_7,  kernel = "Uniform"), slope = "separate", inference = "np")
np_ik_rdd_unconditional_8   <- rdd_reg_np(np_rdd_object_unconditional_8,  bw = rdd_bw_ik(np_rdd_object_unconditional_8,  kernel = "Uniform"), slope = "separate", inference = "np")
np_ik_rdd_unconditional_9   <- rdd_reg_np(np_rdd_object_unconditional_9,  bw = rdd_bw_ik(np_rdd_object_unconditional_9,  kernel = "Uniform"), slope = "separate", inference = "np")
np_ik_rdd_unconditional_10  <- rdd_reg_np(np_rdd_object_unconditional_10, bw = rdd_bw_ik(np_rdd_object_unconditional_10, kernel = "Uniform"), slope = "separate", inference = "np")


summary(np_ik_rdd_unconditional_1)
summary(np_ik_rdd_unconditional_2)
summary(np_ik_rdd_unconditional_3)
summary(np_ik_rdd_unconditional_4)
summary(np_ik_rdd_unconditional_5)
summary(np_ik_rdd_unconditional_6)
summary(np_ik_rdd_unconditional_7)
summary(np_ik_rdd_unconditional_8)
summary(np_ik_rdd_unconditional_9)
summary(np_ik_rdd_unconditional_10)


# coeftest(np_ik_rdd_1, vcov = vcovHC(np_ik_rdd_1, type = "HC0", cluster = "group"))

# ------------------------------------------------------------------------------------------------------------------------

# # Regressões sem covariates, bandwidth = 0.04:
# np_rdd_1   <- rdd_reg_np(np_rdd_object_1,  bw = 0.04, slope = "separate", inference = "np")
# np_rdd_2   <- rdd_reg_np(np_rdd_object_2,  bw = 0.04, slope = "separate", inference = "np")
# np_rdd_3   <- rdd_reg_np(np_rdd_object_3,  bw = 0.04, slope = "separate", inference = "np")
# np_rdd_4   <- rdd_reg_np(np_rdd_object_4,  bw = 0.04, slope = "separate", inference = "np")
# np_rdd_5   <- rdd_reg_np(np_rdd_object_5,  bw = 0.04, slope = "separate", inference = "np")
# np_rdd_6   <- rdd_reg_np(np_rdd_object_6,  bw = 0.04, slope = "separate", inference = "np")
# np_rdd_7   <- rdd_reg_np(np_rdd_object_7,  bw = 0.04, slope = "separate", inference = "np")
# np_rdd_8   <- rdd_reg_np(np_rdd_object_8,  bw = 0.04, slope = "separate", inference = "np")
# np_rdd_9   <- rdd_reg_np(np_rdd_object_9,  bw = 0.04, slope = "separate", inference = "np")
# np_rdd_10  <- rdd_reg_np(np_rdd_object_10, bw = 0.04, slope = "separate", inference = "np")
# np_rdd_11  <- rdd_reg_np(np_rdd_object_11, bw = 0.04, slope = "separate", inference = "np")
# np_rdd_12  <- rdd_reg_np(np_rdd_object_12, bw = 0.04, slope = "separate", inference = "np")
# 
# 
# summary(np_rdd_1)
# summary(np_rdd_2)
# summary(np_rdd_3)
# summary(np_rdd_4)
# summary(np_rdd_5)
# summary(np_rdd_6)
# summary(np_rdd_7)
# summary(np_rdd_8)
# summary(np_rdd_9)
# summary(np_rdd_10)
# summary(np_rdd_11)
# summary(np_rdd_12)





####################################################################################################################
# -----------------------------------------------------------------------------------------------------------------
####################################################################################################################



# REGRESSÕES RDD NÃO-PARAMÉTRICAS COM COVARIATES:

# Primeiramente, devemos armazenar os modelos RDD como rdd objects: (COM COVARIATES)
# O prefixo p_ significa "PARAMÉTRICO":
covariates_df <- base_pronta_mandato %>% select(pop, fracao_pop_masculina, fracao_pop_urbana, pib_pc)

p_rdd_object_conditional_1  <- rdd_data(y = log(despesa_geral_pc), x = margem_vitoria_esquerda, covar = covariates_df, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_conditional_2  <- rdd_data(y = log(despesa_geral_pib), x = margem_vitoria_esquerda, covar = covariates_df, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_conditional_3  <- rdd_data(y = log(total_receitas_pc), x = margem_vitoria_esquerda, covar = covariates_df, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_conditional_4  <- rdd_data(y = log(total_receitas_pib), x = margem_vitoria_esquerda, covar = covariates_df, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_conditional_5  <- rdd_data(y = log(receita_tributaria_pc), x = margem_vitoria_esquerda, covar = covariates_df, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_conditional_6  <- rdd_data(y = log(receita_tributaria_pib), x = margem_vitoria_esquerda, covar = covariates_df, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_conditional_7  <- rdd_data(y = log(servidores_comissionados_mil + 0.0001), x = margem_vitoria_esquerda, covar = covariates_df, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_conditional_8  <- rdd_data(y = educacao_prop, x = margem_vitoria_esquerda, covar = covariates_df, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_conditional_9  <- rdd_data(y = saude_prop, x = margem_vitoria_esquerda, covar = covariates_df, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_conditional_10 <- rdd_data(y = assistencia_social_prop, x = margem_vitoria_esquerda, covar = covariates_df, cutpoint = 0, data = base_pronta_mandato)

# -------------------------------------------------------------------------------------------------------------------------

# Regressões com covariates, bandiwidth IK:
np_ik_rdd_conditional_1    <- rdd_reg_np(p_rdd_object_conditional_1,   bw = rdd_bw_ik(p_rdd_object_conditional_1,  kernel = "Uniform"),  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
np_ik_rdd_conditional_2    <- rdd_reg_np(p_rdd_object_conditional_2,   bw = rdd_bw_ik(p_rdd_object_conditional_2,  kernel = "Uniform"),  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
np_ik_rdd_conditional_3    <- rdd_reg_np(p_rdd_object_conditional_3,   bw = rdd_bw_ik(p_rdd_object_conditional_3,  kernel = "Uniform"),  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
np_ik_rdd_conditional_4    <- rdd_reg_np(p_rdd_object_conditional_4,   bw = rdd_bw_ik(p_rdd_object_conditional_4,  kernel = "Uniform"),  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
np_ik_rdd_conditional_5    <- rdd_reg_np(p_rdd_object_conditional_5,   bw = rdd_bw_ik(p_rdd_object_conditional_5,  kernel = "Uniform"),  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
np_ik_rdd_conditional_6    <- rdd_reg_np(p_rdd_object_conditional_6,   bw = rdd_bw_ik(p_rdd_object_conditional_6,  kernel = "Uniform"),  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
np_ik_rdd_conditional_7    <- rdd_reg_np(p_rdd_object_conditional_7,   bw = rdd_bw_ik(p_rdd_object_conditional_7,  kernel = "Uniform"),  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
np_ik_rdd_conditional_8    <- rdd_reg_np(p_rdd_object_conditional_8,   bw = rdd_bw_ik(p_rdd_object_conditional_8,  kernel = "Uniform"),  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
np_ik_rdd_conditional_9    <- rdd_reg_np(p_rdd_object_conditional_9,   bw = rdd_bw_ik(p_rdd_object_conditional_9,  kernel = "Uniform"),  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
np_ik_rdd_conditional_10   <- rdd_reg_np(p_rdd_object_conditional_10,  bw = rdd_bw_ik(p_rdd_object_conditional_10, kernel = "Uniform"),  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))

summary(np_ik_rdd_conditional_1)
summary(np_ik_rdd_conditional_2)
summary(np_ik_rdd_conditional_3)
summary(np_ik_rdd_conditional_4)
summary(np_ik_rdd_conditional_5)
summary(np_ik_rdd_conditional_6)
summary(np_ik_rdd_conditional_7)
summary(np_ik_rdd_conditional_8)
summary(np_ik_rdd_conditional_9)
summary(np_ik_rdd_conditional_10)


# ----------------------------------------------------------------------------------------------------------------------

# # Regressões com covariates, bandwidth = 0.04:
# np_rdd_conditional_1    <- rdd_reg_np(p_rdd_object_conditional_1,   bw = 0.04,  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
# np_rdd_conditional_2    <- rdd_reg_np(p_rdd_object_conditional_2,   bw = 0.04,  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
# np_rdd_conditional_3    <- rdd_reg_np(p_rdd_object_conditional_3,   bw = 0.04,  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
# np_rdd_conditional_4    <- rdd_reg_np(p_rdd_object_conditional_4,   bw = 0.04,  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
# np_rdd_conditional_5    <- rdd_reg_np(p_rdd_object_conditional_5,   bw = 0.04,  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
# np_rdd_conditional_6    <- rdd_reg_np(p_rdd_object_conditional_6,   bw = 0.04,  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
# np_rdd_conditional_7    <- rdd_reg_np(p_rdd_object_conditional_7,   bw = 0.04,  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
# np_rdd_conditional_8    <- rdd_reg_np(p_rdd_object_conditional_8,   bw = 0.04,  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
# np_rdd_conditional_9    <- rdd_reg_np(p_rdd_object_conditional_9,   bw = 0.04,  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
# np_rdd_conditional_10   <- rdd_reg_np(p_rdd_object_conditional_10,  bw = 0.04,  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
# np_rdd_conditional_11   <- rdd_reg_np(p_rdd_object_conditional_11,  bw = 0.04,  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
# np_rdd_conditional_12   <- rdd_reg_np(p_rdd_object_conditional_12,  bw = 0.04,  slope = "separate", inference = "np", covariates = covariates_df, covar.opt = list("include"))
# 
# summary(np_rdd_conditional_1)
# summary(np_rdd_conditional_2)
# summary(np_rdd_conditional_3)
# summary(np_rdd_conditional_4)
# summary(np_rdd_conditional_5)
# summary(np_rdd_conditional_6)
# summary(np_rdd_conditional_7)
# summary(np_rdd_conditional_8)
# summary(np_rdd_conditional_9)
# summary(np_rdd_conditional_10)
# summary(np_rdd_conditional_11)
# summary(np_rdd_conditional_12)




# Antes de me preocupar em como exportar os resultados e como aplicar erros padrão clusterizados irei
# tentar rodar os modelos RDD não paramétricos a partir do pacote rdrobust:

# Bandwidth recomendado por Hahn, Todd, and van der Klaauw (2001) (segundo o artigo RDD in Economics):
# bandwidth = 0.9 * sd(base_pronta_mandato$margem_vitoria_esquerda) * nrow(base_pronta_mandato)^(-1/5)
