############################################################################################################
#                                   ANÁLISE EMPÍRICA - ESTIMAÇÃO PARAMÉTRICO (MANDATO)
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

# Definindo o diretório onde se encontram as bases de dados prontas para análise empírica:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases de Dados Prontas")

# ----------------------------------------------------------------------------------------------------------------------------------

# Baixando as bases de dados (ESCOLHER A QUE FOR USAR)
load("bases_prontas_ECD")
# load("bases_prontas_ED")

# Definindo o diretório para o R salvar as tabelas geradas:
setwd("C:/Users/joseg_000.PC-JE/Google Drive/FGV-EPGE/Dissertação/Tabelas Geradas no R/Análise com partidos de esquerda, centro e direita")
# setwd("C:/Users/joseg_000.PC-JE/Google Drive/FGV-EPGE/Dissertação/Tabelas Geradas no R/Análise com partidos de esquerda e direita")

# ----------------------------------------------------------------------------------------------------------------------------------


# Analisando quais partidos estão sendo considerados:
table(base_pronta_mandato$ideologia_partido_eleito)
summarytools::freq(base_pronta_mandato$ideologia_partido_eleito)
summarytools::descr(base_pronta_mandato$margem_vitoria_esquerda, transpose = T, stats = "common", round.digits = 3)



# Lista de variáveis outcome de interesse:
# outcomes <- c("despesa_geral_pc", "despesa_geral_pib",
#               "total_receitas_pc", "total_receitas_pib",
#               "receita_tributaria_pc", "receita_tributaria_pib",
#               "educacao_prop", "saude_prop", "assistencia_social_prop",
#               "urbanismo_prop", "transporte_prop", "desporto_e_lazer_prop",
#               "seguranca_publica_prop", "gestao_ambiental_prop",
#               "servidores_comissionados_mil")



# Lista de variáveis de controle de interesse:
# covariates <- c("regiao", "pop", "fracao_pop_masculina", "fracao_pop_urbana",
#                 "proporcao_idoso", "proporcao_jovem", "proporcao_brancos", "pib_pc")


# -----------------------------------------------------------------------------------------------------------------------

# Estimação RDD Paramétrica usando o pacote rddtools


# Usando a função rddtools::rdd_data() para construir os RDD objects de interesse:



# Primeiramente, devemos armazenar os modelos RDD como rdd objects: (SEM COVARIATES)
# O prefixo p_ significa "PARAMÉTRICO":
p_rdd_object_unconditional_1  <- rdd_data(y = log(despesa_geral_pc), x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_unconditional_2  <- rdd_data(y = log(despesa_geral_pib), x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_unconditional_3  <- rdd_data(y = log(total_receitas_pc), x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_unconditional_4  <- rdd_data(y = log(total_receitas_pib), x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_unconditional_5  <- rdd_data(y = log(receita_tributaria_pc), x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_unconditional_6  <- rdd_data(y = log(receita_tributaria_pib), x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_unconditional_7  <- rdd_data(y = log(servidores_comissionados_mil + 0.0001), x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_unconditional_8  <- rdd_data(y = educacao_prop, x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_unconditional_9  <- rdd_data(y = saude_prop, x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_unconditional_10 <- rdd_data(y = assistencia_social_prop, x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_unconditional_11 <- rdd_data(y = urbanismo_prop, x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_unconditional_12 <- rdd_data(y = transporte_prop, x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_unconditional_13 <- rdd_data(y = desporto_e_lazer_prop, x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_unconditional_14 <- rdd_data(y = seguranca_publica_prop, x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_unconditional_15 <- rdd_data(y = gestao_ambiental_prop, x = margem_vitoria_esquerda, cutpoint = 0, data = base_pronta_mandato)

# ------------------------------------------------------------------------------------------------------------

# REGRESSÕES RDD PARAMÉTRICAS SEM COVARIATES:

# Regressões sem covariates, polinômio de ordem 3:
p_rdd_unconditional_1  <- rdd_reg_lm(p_rdd_object_unconditional_1,  order = 1, slope = "separate")
p_rdd_unconditional_2  <- rdd_reg_lm(p_rdd_object_unconditional_2,  order = 1, slope = "separate")
p_rdd_unconditional_3  <- rdd_reg_lm(p_rdd_object_unconditional_3,  order = 1, slope = "separate")
p_rdd_unconditional_4  <- rdd_reg_lm(p_rdd_object_unconditional_4,  order = 1, slope = "separate")
p_rdd_unconditional_5  <- rdd_reg_lm(p_rdd_object_unconditional_5,  order = 1, slope = "separate")
p_rdd_unconditional_6  <- rdd_reg_lm(p_rdd_object_unconditional_6,  order = 1, slope = "separate")
p_rdd_unconditional_7  <- rdd_reg_lm(p_rdd_object_unconditional_7,  order = 1, slope = "separate")
p_rdd_unconditional_8  <- rdd_reg_lm(p_rdd_object_unconditional_8,  order = 1, slope = "separate")
p_rdd_unconditional_9  <- rdd_reg_lm(p_rdd_object_unconditional_9,  order = 1, slope = "separate")
p_rdd_unconditional_10 <- rdd_reg_lm(p_rdd_object_unconditional_10, order = 1, slope = "separate")
p_rdd_unconditional_11 <- rdd_reg_lm(p_rdd_object_unconditional_10, order = 1, slope = "separate")
p_rdd_unconditional_12 <- rdd_reg_lm(p_rdd_object_unconditional_10, order = 1, slope = "separate")
p_rdd_unconditional_13 <- rdd_reg_lm(p_rdd_object_unconditional_10, order = 1, slope = "separate")
p_rdd_unconditional_14 <- rdd_reg_lm(p_rdd_object_unconditional_10, order = 1, slope = "separate")
p_rdd_unconditional_15 <- rdd_reg_lm(p_rdd_object_unconditional_10, order = 1, slope = "separate")


stargazer(p_rdd_unconditional_1, p_rdd_unconditional_2,
          type = "text", digits = 3, digits.extra = 2, digit.separator = "", keep = "D", single.row = F, align = T, table.placement = "h!",
          covariate.labels = "Left-Wing Party", title = "Parametric RDD",
          column.labels = c("Total expenditures (per capita)", "Total Expeditures (share of income)"),
          se = list(sqrt(diag(vcovHC(p_rdd_unconditional_1, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_2, type = "HC0", cluster = "group")))))

stargazer(p_rdd_unconditional_3, p_rdd_unconditional_4, p_rdd_unconditional_5, p_rdd_unconditional_6,
          type = "text", digits = 3, digits.extra = 2, digit.separator = "", keep = "D", single.row = F, align = T, table.placement = "h!",
          covariate.labels = "Left-Wing Party", title = "Parametric RDD",
          column.labels = c("Total Revenues (per capita)", "Total Revenues (share of income)",
                            "Tax Revenues (per capita)", "Tax Revenues (share of income)"),
          se = list(sqrt(diag(vcovHC(p_rdd_unconditional_3, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_4, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_5, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_6, type = "HC0", cluster = "group")))))

stargazer(p_rdd_unconditional_7, p_rdd_unconditional_8, p_rdd_unconditional_9, p_rdd_unconditional_10, 
          type = "text", digits = 3, digits.extra = 2, digit.separator = "", keep = "D", single.row = F, align = T, table.placement = "h!",
          covariate.labels = "Left-Wing Party", title = "Parametric RDD",
          column.labels = c("Comission Employees (per 1000 residents)", "Spending on Education (share of total)",
                            "Spending on Health (share of total)", "Spending on Social Assistance (share of total)"),
          se = list(sqrt(diag(vcovHC(p_rdd_unconditional_7,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_8,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_9,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_10, type = "HC0", cluster = "group")))))


# Todas as regressões em uma só tabela, exportada para excel xls, e erros padrão com cluster:
# (Abrir os arquivos em html, copiar e colar (transpondo) no excel e arrumar os dados na tabela)
column_labels_vector <- c("Total expenditures (per capita)", "Total Expeditures (share of income)",
                          "Total Revenues (per capita)", "Total Revenues (share of income)",
                          "Tax Revenues (per capita)", "Tax Revenues (share of income)",
                          "Comission Employees (per 1000 residents)", "Spending on Education (share of total)",
                          "Spending on Health (share of total)", "Spending on Social Assistance (share of total)",
                          "Spending on Urbanism (Share of Total)", "Spending on Transportation (Share of Total)",
                          "Spending on Sports and Leisure (Share of Total)",
                          "Spending on Public Safety (Share of Total)",
                          "Spending on Environmental management (Share of Total)")

stargazer(p_rdd_unconditional_1, p_rdd_unconditional_2, p_rdd_unconditional_3, p_rdd_unconditional_4,
          p_rdd_unconditional_5, p_rdd_unconditional_6, p_rdd_unconditional_7, p_rdd_unconditional_8,
          p_rdd_unconditional_9, p_rdd_unconditional_10, p_rdd_unconditional_11, p_rdd_unconditional_12, 
          p_rdd_unconditional_13, p_rdd_unconditional_14, p_rdd_unconditional_15,
          digits = 3, digits.extra = 2, digit.separator = "", keep = "D", single.row = F, align = T, table.placement = "h!",
          covariate.labels = "Left-Wing Party", title = "Parametric RDD. No covariates. Polymonial of order 3",
          column.labels = column_labels_vector,
          se = list(sqrt(diag(vcovHC(p_rdd_unconditional_1,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_2,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_3,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_4,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_5,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_6,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_7,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_8,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_9,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_10, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_11, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_12, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_13, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_14, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_unconditional_15, type = "HC0", cluster = "group")))),
          type = "html", out = "p_rdd_unconditional.html")



plot(p_rdd_unconditional_1)
plot(p_rdd_unconditional_2)
plot(p_rdd_unconditional_3)
plot(p_rdd_unconditional_4)
plot(p_rdd_unconditional_5)
plot(p_rdd_unconditional_6)
plot(p_rdd_unconditional_7)
plot(p_rdd_unconditional_8)
plot(p_rdd_unconditional_9)
plot(p_rdd_unconditional_10)




# ------------------------------------------------------------------------------------------------------------

# REGRESSÕES RDD PARAMÉTRICAS COM COVARIATES:

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
p_rdd_object_conditional_11 <- rdd_data(y = urbanismo_prop, x = margem_vitoria_esquerda, covar = covariates_df, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_conditional_12 <- rdd_data(y = transporte_prop, x = margem_vitoria_esquerda, covar = covariates_df, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_conditional_13 <- rdd_data(y = desporto_e_lazer_prop, x = margem_vitoria_esquerda, covar = covariates_df, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_conditional_14 <- rdd_data(y = seguranca_publica_prop, x = margem_vitoria_esquerda, covar = covariates_df, cutpoint = 0, data = base_pronta_mandato)
p_rdd_object_conditional_15 <- rdd_data(y = gestao_ambiental_prop, x = margem_vitoria_esquerda, covar = covariates_df, cutpoint = 0, data = base_pronta_mandato)



# Regressões com covariates, polinômio de ordem 3:
attach(base_pronta_mandato)
p_rdd_conditional_1  <- rdd_reg_lm(p_rdd_object_conditional_1,   order = 1, slope = "separate", covariates = "pop", covar.opt = list("include"))
p_rdd_conditional_2  <- rdd_reg_lm(p_rdd_object_conditional_2,   order = 1, slope = "separate", covariates = "pop", covar.opt = list("include"))
p_rdd_conditional_3  <- rdd_reg_lm(p_rdd_object_conditional_3,   order = 1, slope = "separate", covariates = "pop", covar.opt = list("include"))
p_rdd_conditional_4  <- rdd_reg_lm(p_rdd_object_conditional_4,   order = 1, slope = "separate", covariates = "pop", covar.opt = list("include"))
p_rdd_conditional_5  <- rdd_reg_lm(p_rdd_object_conditional_5,   order = 1, slope = "separate", covariates = "pop", covar.opt = list("include"))
p_rdd_conditional_6  <- rdd_reg_lm(p_rdd_object_conditional_6,   order = 1, slope = "separate", covariates = "pop", covar.opt = list("include"))
p_rdd_conditional_7  <- rdd_reg_lm(p_rdd_object_conditional_7,   order = 1, slope = "separate", covariates = "pop", covar.opt = list("include"))
p_rdd_conditional_8  <- rdd_reg_lm(p_rdd_object_conditional_8,   order = 1, slope = "separate", covariates = "pop", covar.opt = list("include"))
p_rdd_conditional_9  <- rdd_reg_lm(p_rdd_object_conditional_9,   order = 1, slope = "separate", covariates = "pop", covar.opt = list("include"))
p_rdd_conditional_10 <- rdd_reg_lm(p_rdd_object_conditional_10,  order = 1, slope = "separate", covariates = "pop", covar.opt = list("include"))
p_rdd_conditional_11 <- rdd_reg_lm(p_rdd_object_conditional_11,  order = 1, slope = "separate", covariates = "pop", covar.opt = list("include"))
p_rdd_conditional_12 <- rdd_reg_lm(p_rdd_object_conditional_12,  order = 1, slope = "separate", covariates = "pop", covar.opt = list("include"))
p_rdd_conditional_13 <- rdd_reg_lm(p_rdd_object_conditional_13,  order = 1, slope = "separate", covariates = "pop", covar.opt = list("include"))
p_rdd_conditional_14 <- rdd_reg_lm(p_rdd_object_conditional_14,  order = 1, slope = "separate", covariates = "pop", covar.opt = list("include"))
p_rdd_conditional_15 <- rdd_reg_lm(p_rdd_object_conditional_15,  order = 1, slope = "separate", covariates = "pop", covar.opt = list("include"))



stargazer(p_rdd_conditional_1, p_rdd_conditional_2,
          type = "text", digits = 3, digits.extra = 2, digit.separator = "", keep = "D", single.row = F, align = T, table.placement = "h!",
          covariate.labels = "Left-Wing Party", title = "Parametric RDD",
          column.labels = c("Total expenditures (per capita)", "Total Expeditures (share of income)"),
          se = list(sqrt(diag(vcovHC(p_rdd_conditional_1, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_2, type = "HC0", cluster = "group")))))

stargazer(p_rdd_conditional_3, p_rdd_conditional_4, p_rdd_conditional_5, p_rdd_conditional_6,
          type = "text", digits = 3, digits.extra = 2, digit.separator = "", keep = "D", single.row = F, align = T, table.placement = "h!",
          covariate.labels = "Left-Wing Party", title = "Parametric RDD",
          column.labels = c("Total Revenues (per capita)", "Total Revenues (share of income)",
                            "Tax Revenues (per capita)", "Tax Revenues (share of income)"),
          se = list(sqrt(diag(vcovHC(p_rdd_conditional_3, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_4, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_5, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_6, type = "HC0", cluster = "group")))))

stargazer(p_rdd_conditional_7, p_rdd_conditional_8, p_rdd_conditional_9, p_rdd_conditional_10, 
          type = "text", digits = 3, digits.extra = 2, digit.separator = "", keep = "D", single.row = F, align = T, table.placement = "h!",
          covariate.labels = "Left-Wing Party", title = "Parametric RDD",
          column.labels = c("Comission Employees (per 1000 residents)", "Spending on Education (share of total)",
                            "Spending on Health (share of total)", "Spending on Social Assistance (share of total)"),
          se = list(sqrt(diag(vcovHC(p_rdd_conditional_7,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_8,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_9,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_10, type = "HC0", cluster = "group")))))


# Todas as regressões em uma só tabela, exportada para excel xls, e erros padrão com cluster:
# (Abrir os arquivos em html, copiar e colar (transpondo) no excel e arrumar os dados na tabela)
stargazer(p_rdd_conditional_1, p_rdd_conditional_2, p_rdd_conditional_3, p_rdd_conditional_4,
          p_rdd_conditional_5, p_rdd_conditional_6, p_rdd_conditional_7, p_rdd_conditional_8,
          p_rdd_conditional_9, p_rdd_conditional_10, p_rdd_conditional_11, p_rdd_conditional_12,
          p_rdd_conditional_13, p_rdd_conditional_14, p_rdd_conditional_15, 
          digits = 3, digits.extra = 2, digit.separator = "", keep = "D", single.row = F, align = T, table.placement = "h!",
          covariate.labels = "Left-Wing Party", title = "Parametric RDD. Four covariates. Polymonial of order 3",
          column.labels = column_labels_vector,
          se = list(sqrt(diag(vcovHC(p_rdd_conditional_1,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_2,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_3,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_4,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_5,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_6,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_7,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_8,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_9,  type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_10, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_11, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_12, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_13, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_14, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(p_rdd_conditional_15, type = "HC0", cluster = "group")))),
          type = "html", out = "p_rdd_conditional.html")



# plot(p_rdd_conditional_1)
# plot(p_rdd_conditional_2)
# plot(p_rdd_conditional_3)
# plot(p_rdd_conditional_4)
# plot(p_rdd_conditional_5)
# plot(p_rdd_conditional_6)
# plot(p_rdd_conditional_7)
# plot(p_rdd_conditional_8)
# plot(p_rdd_conditional_9)
# plot(p_rdd_conditional_10)
# plot(p_rdd_conditional_11)
# plot(p_rdd_conditional_12)



detach(base_pronta_mandato)

