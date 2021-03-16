############################################################################################################
#                                   ANÁLISE EMPÍRICA - ESTIMAÇÃO OLS E EFEITOS FIXOS
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
# setwd("C:/Users/joseg_000.PC-JE/Google Drive/FGV-EPGE/Dissertação/Tabelas Geradas no R/Análise com partidos de esquerda, centro e direita")
setwd("C:/Users/joseg_000.PC-JE/Google Drive/FGV-EPGE/Dissertação/Tabelas Geradas no R/Análise com partidos de esquerda e direita")


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


# -------------------------------------------------------------------------------------------------------------------

# Estimação OLS com efeitos fixos de municipio e mandato:

# Para isso, vamos utilizar o pacote plm que é usado para regressões com dados em Painel:
# total_receitas_pc, total_receitas_pib

fe_unconditional_1  <- plm(log(despesa_geral_pc) ~ partido_de_esquerda, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")
fe_unconditional_2  <- plm(log(despesa_geral_pib) ~ partido_de_esquerda, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")

fe_unconditional_3  <- plm(log(total_receitas_pc) ~ partido_de_esquerda, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")
fe_unconditional_4  <- plm(log(total_receitas_pib) ~ partido_de_esquerda, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")

fe_unconditional_5  <- plm(log(receita_tributaria_pc) ~ partido_de_esquerda, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")
fe_unconditional_6  <- plm(log(receita_tributaria_pib) ~ partido_de_esquerda, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")

fe_unconditional_7  <- plm(log(servidores_comissionados_mil + 0.0001) ~ partido_de_esquerda, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")

fe_unconditional_8  <- plm(educacao_prop ~ partido_de_esquerda, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")
fe_unconditional_9  <- plm(saude_prop ~ partido_de_esquerda, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")
fe_unconditional_10 <- plm(assistencia_social_prop ~ partido_de_esquerda, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")


# Reportando os resultados com erros padrão robustos usando cluster de municipio:
stargazer(fe_unconditional_1, fe_unconditional_2,
          type = "text", digits = 3, digits.extra = 2, digit.separator = "", omit = c("Constant"),
          covariate.labels = "Left-Wing Party",
          dep.var.labels = c("Total expenditures (per capita)", "Total Expeditures (share of income)"),
          se = list(sqrt(diag(vcovHC(fe_unconditional_1, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_unconditional_2, type = "HC0", cluster = "group")))))


stargazer(fe_unconditional_3, fe_unconditional_4, fe_unconditional_5, fe_unconditional_6,
          type = "text", digits = 3, digits.extra = 2, digit.separator = "", omit = c("Constant"),
          covariate.labels = "Left-Wing Party",
          dep.var.labels = c("Total Revenues (per capita)", "Total Revenues (share of income)",
                             "Tax Revenues (per capita)", "Tax Revenues (share of income)"),
          se = list(sqrt(diag(vcovHC(fe_unconditional_3, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_unconditional_4, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_unconditional_5, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_unconditional_6, type = "HC0", cluster = "group")))))

stargazer(fe_unconditional_7, fe_unconditional_8, fe_unconditional_9, fe_unconditional_10,
          type = "text", digits = 3, digits.extra = 2, digit.separator = "", omit = c("Constant"),
          covariate.labels = "Left-Wing Party",
          dep.var.labels = c("Comission Employees (per 1000 residents)", "Spending on Education (share of total)",
                             "Spending on Health (share of total)", "Spending on Social Assistance (share of total)"),
          se = list(sqrt(diag(vcovHC(fe_unconditional_7, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_unconditional_8, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_unconditional_9, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_unconditional_10, type = "HC0", cluster = "group")))))




# Todas as regressões em uma só tabela, exportada para excel xls:
stargazer(fe_unconditional_1, fe_unconditional_2, fe_unconditional_3, fe_unconditional_4,
          fe_unconditional_5, fe_unconditional_6, fe_unconditional_7, fe_unconditional_8,
          fe_unconditional_9, fe_unconditional_10, single.row = F,
          digits = 3, digits.extra = 2, digit.separator = "",
          omit = c("Constant", "pop", "fracao_pop_masculina", "fracao_pop_urbana", "pib_pc"),
          covariate.labels = "Left-Wing Party",
          dep.var.labels = c("Total expenditures (per capita)", "Total Expeditures (share of income)",
                             "Total Revenues (per capita)", "Total Revenues (share of income)",
                             "Tax Revenues (per capita)", "Tax Revenues (share of income)",
                             "Comission Employees (per 1000 residents)", "Spending on Education (share of total)",
                             "Spending on Health (share of total)", "Spending on Social Assistance (share of total)"),
          se = list(sqrt(diag(vcovHC(fe_unconditional_1, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_unconditional_2, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_unconditional_3, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_unconditional_4, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_unconditional_5, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_unconditional_6, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_unconditional_7, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_unconditional_8, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_unconditional_9, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_unconditional_10, type = "HC0", cluster = "group")))),
          out = "fe_unconditional.xls", type = "html")



# Reportando os resultados com erros padrão corrigidos usando clusters de municípios:
# coeftest(fe_unconditional_1,  vcov = vcovHC(fe_unconditional_1, type = "HC0", cluster = "group"))
# coeftest(fe_unconditional_2,  vcov = vcovHC(fe_unconditional_2, type = "HC0", cluster = "group"))
# coeftest(fe_unconditional_3,  vcov = vcovHC(fe_unconditional_3, type = "HC0", cluster = "group"))
# coeftest(fe_unconditional_4,  vcov = vcovHC(fe_unconditional_4, type = "HC0", cluster = "group"))
# coeftest(fe_unconditional_5,  vcov = vcovHC(fe_unconditional_5, type = "HC0", cluster = "group"))
# coeftest(fe_unconditional_6,  vcov = vcovHC(fe_unconditional_6, type = "HC0", cluster = "group"))
# coeftest(fe_unconditional_7,  vcov = vcovHC(fe_unconditional_7, type = "HC0", cluster = "group"))
# coeftest(fe_unconditional_8,  vcov = vcovHC(fe_unconditional_8, type = "HC0", cluster = "group"))
# coeftest(fe_unconditional_9,  vcov = vcovHC(fe_unconditional_9, type = "HC0", cluster = "group"))
# coeftest(fe_unconditional_10, vcov = vcovHC(fe_unconditional_10, type = "HC0", cluster = "group"))



# outcomes <- c("despesa_geral_pc", "despesa_geral_pib",
#               "total_receitas_pc", "total_receitas_pib",
#               "receita_tributaria_pc", "receita_tributaria_pib",
#               "educacao_pc", "educacao_pib", "educacao_prop"
#               "saude_pc", "saude_pib", "saude_prop"
#               "assistencia_social_pc", "assistencia_social_pib", "assistencia_social_prop"
#               "total_servidores_pc",
#               "servidores_comissionados_pc")




# -----------------------------------------------------------------------------------------------

# Estimação OLS com efeitos fixos de municipio e mandato, e variáveis de controle:

# Para isso, vamos utilizar o pacote plm que é usado para regressões com dados em Painel:

fe_conditional_1  <- plm(log(despesa_geral_pc) ~ partido_de_esquerda + pop + fracao_pop_masculina + fracao_pop_urbana + pib_pc, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")
fe_conditional_2  <- plm(log(despesa_geral_pib) ~ partido_de_esquerda + pop + fracao_pop_masculina + fracao_pop_urbana + pib_pc, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")

fe_conditional_3  <- plm(log(total_receitas_pc) ~ partido_de_esquerda + pop + fracao_pop_masculina + fracao_pop_urbana + pib_pc, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")
fe_conditional_4  <- plm(log(total_receitas_pib) ~ partido_de_esquerda + pop + fracao_pop_masculina + fracao_pop_urbana + pib_pc, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")

fe_conditional_5  <- plm(log(receita_tributaria_pc) ~ partido_de_esquerda + pop + fracao_pop_masculina + fracao_pop_urbana + pib_pc, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")
fe_conditional_6  <- plm(log(receita_tributaria_pib) ~ partido_de_esquerda + pop + fracao_pop_masculina + fracao_pop_urbana + pib_pc, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")

fe_conditional_7  <- plm(log(servidores_comissionados_mil + 0.0001) ~ partido_de_esquerda + pop + fracao_pop_masculina + fracao_pop_urbana + pib_pc, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")

fe_conditional_8  <- plm(educacao_prop ~ partido_de_esquerda + pop + fracao_pop_masculina + fracao_pop_urbana + pib_pc, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")
fe_conditional_9  <- plm(saude_prop ~ partido_de_esquerda + pop + fracao_pop_masculina + fracao_pop_urbana + pib_pc, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")
fe_conditional_10 <- plm(assistencia_social_prop ~ partido_de_esquerda + pop + fracao_pop_masculina + fracao_pop_urbana + pib_pc, data = base_pronta_mandato, index = c("cod_municipio_ibge", "mandato"), model = "within", effect = "twoways")



# Reportando resultados com erros padrão usuais:


# Reportando os resultados com erros padrão robustos usando cluster de municipio:
stargazer(fe_conditional_1, fe_conditional_2,
          type = "text", digits = 3, digits.extra = 2, digit.separator = "",
          omit = c("Constant", "pop", "fracao_pop_masculina", "fracao_pop_urbana", "pib_pc"),
          covariate.labels = "Left-Wing Party",
          dep.var.labels = c("Total expenditures (per capita)", "Total Expeditures (share of income)"),
          se = list(sqrt(diag(vcovHC(fe_conditional_1, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_conditional_2, type = "HC0", cluster = "group")))))


stargazer(fe_conditional_3, fe_conditional_4, fe_conditional_5, fe_conditional_6,
          type = "text", digits = 3, digits.extra = 2, digit.separator = "",
          omit = c("Constant", "pop", "fracao_pop_masculina", "fracao_pop_urbana", "pib_pc"),
          covariate.labels = "Left-Wing Party",
          dep.var.labels = c("Total Revenues (per capita)", "Total Revenues (share of income)",
                             "Tax Revenues (per capita)", "Tax Revenues (share of income)"),
          se = list(sqrt(diag(vcovHC(fe_conditional_3, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_conditional_4, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_conditional_5, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_conditional_6, type = "HC0", cluster = "group")))))

stargazer(fe_conditional_7, fe_conditional_8, fe_conditional_9, fe_conditional_10, single.row = F,
          type = "text", digits = 3, digits.extra = 2, digit.separator = "",
          omit = c("Constant", "pop", "fracao_pop_masculina", "fracao_pop_urbana", "pib_pc"),
          covariate.labels = "Left-Wing Party",
          dep.var.labels = c("Comission Employees (per 1000 residents)", "Spending on Education (share of total)",
                             "Spending on Health (share of total)", "Spending on Social Assistance (share of total)"),
          se = list(sqrt(diag(vcovHC(fe_conditional_7, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_conditional_8, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_conditional_9, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_conditional_10, type = "HC0", cluster = "group")))))


# Todas as regressões em uma só tabela, exportada para excel xls:
stargazer(fe_conditional_1, fe_conditional_2, fe_conditional_3, fe_conditional_4,
          fe_conditional_5, fe_conditional_6, fe_conditional_7, fe_conditional_8,
          fe_conditional_9, fe_conditional_10, single.row = F,
          digits = 3, digits.extra = 2, digit.separator = "",
          omit = c("Constant", "pop", "fracao_pop_masculina", "fracao_pop_urbana", "pib_pc"),
          covariate.labels = "Left-Wing Party",
          dep.var.labels = c("Total expenditures (per capita)", "Total Expeditures (share of income)",
                             "Total Revenues (per capita)", "Total Revenues (share of income)",
                             "Tax Revenues (per capita)", "Tax Revenues (share of income)",
                             "Comission Employees (per 1000 residents)", "Spending on Education (share of total)",
                             "Spending on Health (share of total)", "Spending on Social Assistance (share of total)"),
          se = list(sqrt(diag(vcovHC(fe_conditional_1, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_conditional_2, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_conditional_3, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_conditional_4, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_conditional_5, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_conditional_6, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_conditional_7, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_conditional_8, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_conditional_9, type = "HC0", cluster = "group"))),
                    sqrt(diag(vcovHC(fe_conditional_10, type = "HC0", cluster = "group")))),
          out = "fe_conditional.xls", type = "html")



# htmlreg(list(fe_conditional_9, fe_conditional_10, fe_conditional_11, fe_conditional_12), single.row = F, file = "tabela_zzz.xls",
#           stars = c(0.01, 0.05, 0.10), digits = 3, center = T, dcolumn = T,
#           custom.model.names = c("Comission Employees (per 1000 residents)", "Spending on Education (share of total)",
#                                  "Spending on Health (share of total)", "Spending on Social Assistance (share of total)"),
#           omit.coef = "pop",
#           custom.coef.names = c("Left-Wing Party", "Income per Capita"),
#           override.se = list(sqrt(diag(vcovHC(fe_conditional_9, type = "HC0", cluster = "group"))),
#                              sqrt(diag(vcovHC(fe_conditional_10, type = "HC0", cluster = "group"))),
#                              sqrt(diag(vcovHC(fe_conditional_11, type = "HC0", cluster = "group"))),
#                              sqrt(diag(vcovHC(fe_conditional_12, type = "HC0", cluster = "group")))))
# 
# htmlreg(list(fe_conditional_9, fe_conditional_10, fe_conditional_11, fe_conditional_12), single.row = F, file = "tabela_12.xls",
#         stars = c(0.01, 0.05, 0.10), digits = 3)

# Para quebrar linhas no formato html, usar o separador <br>:



# # Reportando os resultados com erros padrão corrigidos usando clusters de municípios:
# coeftest(fe_conditional_1,  vcov = vcovHC(fe_conditional_1, type = "HC0", cluster = "group"))
# coeftest(fe_conditional_2,  vcov = vcovHC(fe_conditional_2, type = "HC0", cluster = "group"))
# coeftest(fe_conditional_3,  vcov = vcovHC(fe_conditional_3, type = "HC0", cluster = "group"))
# coeftest(fe_conditional_4,  vcov = vcovHC(fe_conditional_4, type = "HC0", cluster = "group"))
# coeftest(fe_conditional_5,  vcov = vcovHC(fe_conditional_5, type = "HC0", cluster = "group"))
# coeftest(fe_conditional_6,  vcov = vcovHC(fe_conditional_6, type = "HC0", cluster = "group"))
# coeftest(fe_conditional_7,  vcov = vcovHC(fe_conditional_7, type = "HC0", cluster = "group"))
# coeftest(fe_conditional_8,  vcov = vcovHC(fe_conditional_8, type = "HC0", cluster = "group"))
# coeftest(fe_conditional_9,  vcov = vcovHC(fe_conditional_9, type = "HC0", cluster = "group"))
# coeftest(fe_conditional_10, vcov = vcovHC(fe_conditional_10, type = "HC0", cluster = "group"))
# coeftest(fe_conditional_11, vcov = vcovHC(fe_conditional_11, type = "HC0", cluster = "group"))
# coeftest(fe_conditional_12, vcov = vcovHC(fe_conditional_12, type = "HC0", cluster = "group"))
# coeftest(fe_conditional_13, vcov = vcovHC(fe_conditional_13, type = "HC0", cluster = "group"))
# coeftest(fe_conditional_14, vcov = vcovHC(fe_conditional_14, type = "HC0", cluster = "group"))
# # coeftest(fe_conditional_15, vcov = vcovHC(fe_conditional_15, type = "HC0", cluster = "group"))
# # coeftest(fe_conditional_16, vcov = vcovHC(fe_conditional_16, type = "HC0", cluster = "group"))
# coeftest(fe_conditional_17, vcov = vcovHC(fe_conditional_17, type = "HC0", cluster = "group"))
# coeftest(fe_conditional_18, vcov = vcovHC(fe_conditional_18, type = "HC0", cluster = "group"))
# coeftest(fe_conditional_19, vcov = vcovHC(fe_conditional_19, type = "HC0", cluster = "group"))