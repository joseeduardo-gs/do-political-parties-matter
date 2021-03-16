############################################################################################################
#                                   ANÁLISE EMPÍRICA
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

# Pacotes para estatísticas descritivas:
library(psych)
library(summarytools)
library(stargazer)
library(qwraps2)
# library(tabyls)

# Pacotes Específicos para se trabalhar com RDD:
library(rdd) # Pacote mais básico
library(rddtools)  # Pacote com muitas opções de testes de placebo e sensibilidade das estimações e estimação paramétrica
library(rdrobust)  # Pacote mais abrangente
library(rddapp)    # Pacote com interface em Shiny, reduz a necessidade de se saber programar.

# Opções:
options(scipen = 999) # Desabilita notação scientífica. Para voltar ao padrão -> options(scipen = 1)


# Definindo o diretório onde se encontram as bases de dados prontas para análise empírica:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases de Dados Prontas")

# Baixando as bases de dados (ESCOLHER A QUE FOR USAR)
load("bases_prontas_ECD")
# load("bases_prontas_ED")

# Definindo o diretório para o R salvar as tabelas geradas:
setwd("C:/Users/joseg_000.PC-JE/Google Drive/FGV-EPGE/Dissertação/Tabelas Geradas no R/Análise com partidos de esquerda, centro e direita")
# setwd("C:/Users/joseg_000.PC-JE/Google Drive/FGV-EPGE/Dissertação/Tabelas Geradas no R/Análise com partidos de esquerda e direita")

# --------------------------------------------------------------------------------------------------------------------------

# Analisando quais partidos estão sendo considerados:
table(base_pronta_mandato$ideologia_partido_eleito)
summarytools::freq(base_pronta_mandato$ideologia_partido_eleito)
summarytools::descr(base_pronta_mandato$margem_vitoria_esquerda, transpose = T, stats = "common", round.digits = 3)

# ------------------------------------------------------------------------------------------------------------------

# Selecionando variáveis de interesse para análise descritiva:



base_pronta_summary <- base_pronta %>% select(despesa_geral_pc, despesa_geral_pib, 
                                              total_receitas_pc, total_receitas_pib,
                                              receita_tributaria_pc, receita_tributaria_pib,
                                              servidores_comissionados_mil,
                                              educacao_prop, saude_prop, assistencia_social_prop,
                                              urbanismo_prop, transporte_prop, desporto_e_lazer_prop,
                                              seguranca_publica_prop, gestao_ambiental_prop,
                                              margem_vitoria_esquerda,
                                              pib_pc, pop, proporcao_jovem, proporcao_idoso, fracao_pop_masculina, fracao_pop_urbana,
                                              cod_municipio_ibge, ano, mandato, partido_de_esquerda) %>% 
  mutate_at(vars(margem_vitoria_esquerda, despesa_geral_pib,
                 total_receitas_pib, receita_tributaria_pib,
                 educacao_prop, saude_prop, assistencia_social_prop,
                 urbanismo_prop, transporte_prop, desporto_e_lazer_prop,
                 seguranca_publica_prop, gestao_ambiental_prop,
                      proporcao_jovem, proporcao_idoso, fracao_pop_masculina, fracao_pop_urbana),
            function(x) {x * 100})


base_pronta_mandato_summary <- base_pronta_mandato %>% select(despesa_geral_pc, despesa_geral_pib, 
                                                              total_receitas_pc, total_receitas_pib,
                                                              receita_tributaria_pc, receita_tributaria_pib,
                                                              servidores_comissionados_mil,
                                                              educacao_prop, saude_prop, assistencia_social_prop,
                                                              urbanismo_prop, transporte_prop, desporto_e_lazer_prop,
                                                              seguranca_publica_prop, gestao_ambiental_prop,
                                                              margem_vitoria_esquerda,
                                                              pib_pc, pop, proporcao_jovem, proporcao_idoso, fracao_pop_masculina, fracao_pop_urbana,
                                                              cod_municipio_ibge, mandato, partido_de_esquerda) %>% 
  mutate_at(vars(margem_vitoria_esquerda, despesa_geral_pib,
                 total_receitas_pib, receita_tributaria_pib,
                 educacao_prop, saude_prop, assistencia_social_prop,
                 urbanismo_prop, transporte_prop, desporto_e_lazer_prop,
                 seguranca_publica_prop, gestao_ambiental_prop,
                 proporcao_jovem, proporcao_idoso, fracao_pop_masculina, fracao_pop_urbana),
            function(x) {x * 100})

# Estatísticas Descritivas: -----------------------------------------------

# Estatísticas descritivas para a amostra inteira table(base_pronta_mandato$ideologia_partido_eleito)
summarytools::freq(base_pronta_mandato$ideologia_partido_eleito)
summarytools::descr(base_pronta_mandato_summary$margem_vitoria_esquerda, transpose = T, stats = "common", round.digits = 3)


# summary(base_pronta_mandato_summary)
psych::describe(base_pronta_mandato_summary, skew = F, ranges = F, quant = NULL, IQR = F)

covariates_labels_vector <- c("Total Expeditures per capita", "Total Expeditures as a share of income (%)",
                              "Total Revenues per capita", "Total Revenues as a share of income (%)",
                              "Tax Revenues per capita", "Tax Revenues as a share of income (%)",
                              "Comission Employees (per 1000 residents)",
                              "% spent on Education",
                              "% spent on Health",
                              "% spent on Social Assistance",
                              "% spent on Urbanism",
                              "% spent on transportation",
                              "% spent on Leisure",
                              "% spent on Public Safety",
                              "% spent on Environmental Management",
                              "Left Vote Share Margin of victory (%)",
                              "Income per capita",
                              "Population Size",
                              "Proportion of young (0-15) (%)",
                              "Proportion of old (65+) (%)",
                              "Proportion Male Population (%)",
                              "Proportion Urban Population (%)")

# Estatísticas Descritivas da base inteira: (definir type = "htlm", converter para excel e depois para latex)
stargazer(data.frame(base_pronta_mandato_summary), summary = T, align = T,
          digits = 2, digits.extra = 2, rownames = T, digit.separator = "",
          omit = c("cod_municipio_ibge", "mandato", "partido_de_esquerda"),
          covariate.labels = covariates_labels_vector,
          summary.stat = c("mean", "sd", "min", "max"),
          out = "estatisticas_descritivas.html", type = "text")














# --------------------------------------------------------------------------------------------------------------


# NÃO PRETENDO MAIS USAR ESTA PARTE QUE SEGUE ABAIXO:


# # Estatísticas Descritivas comparando partidos de esquerda e direita:
# # Usando o pacote psych:
# psych::describeBy(base_pronta_mandato_summary, group = base_pronta_mandato_summary$partido_de_esquerda, digits = 2, mat = F,
#                   skew = F, ranges = F, quant = NULL, IQR = F)
# 
# # Usando o Pacote summarytools:
# # st_options(descr.stats = "common", descr.transpose = T)
# # print(stby(base_pronta_mandato_summary, base_pronta_mandato_summary$partido_de_esquerda, descr))
# 
# # Usando o pacote Stargazer: (Adicionar as variáveis no Excel, e completar a tabela "manualmente")
# # Estatísticas descritivas para partidos de esquerda:
# stargazer(data.frame(base_pronta_mandato_summary %>% filter(partido_de_esquerda == 1)), summary = T, align = T,
#           digits = 2, digits.extra = 2, rownames = T, digit.separator = "", title = "Left-Wing Parties",
#           omit = c("cod_municipio_ibge", "mandato", "partido_de_esquerda", "margem_vitoria_esquerda"),
#           summary.stat = c("mean"),
#           covariate.labels = covariates_labels_vector,
#           type = "text")
# 
# # Estatísticas descritivas para partidos de centro e/ou direita:
# stargazer(data.frame(base_pronta_mandato_summary %>% filter(partido_de_esquerda == 0)), summary = T, align = T,
#           digits = 2, digits.extra = 2, rownames = T, digit.separator = "", title = "Center and/or Right-Wing Parties",
#           omit = c("cod_municipio_ibge", "mandato", "partido_de_esquerda", "margem_vitoria_esquerda"),
#           summary.stat = c("mean"),
#           covariate.labels = covariates_labels_vector,
#           type = "text")
# 
# 
# 
# # Testes de Diferença de Médias (importantes para se observar o p-valor dos testes):
# attach(base_pronta_mandato_summary)
# t.test(despesa_geral_pc ~ partido_de_esquerda)
# t.test(despesa_geral_pib ~ partido_de_esquerda)
# # t.test(despesas_correntes_pc ~ partido_de_esquerda)
# # t.test(despesas_correntes_pib ~ partido_de_esquerda)
# t.test(total_receitas_pc ~ partido_de_esquerda)
# t.test(total_receitas_pib ~ partido_de_esquerda)
# t.test(receita_tributaria_pc ~ partido_de_esquerda)
# t.test(receita_tributaria_pib ~ partido_de_esquerda)
# t.test(assistencia_social_prop ~ partido_de_esquerda)
# t.test(saude_prop ~ partido_de_esquerda)
# t.test(educacao_prop ~ partido_de_esquerda)
# t.test(servidores_comissionados_mil ~ partido_de_esquerda)
# # t.test(taxa_homicidios ~ partido_de_esquerda)
# # t.test(margem_vitoria_esquerda ~ partido_de_esquerda)
# t.test(pib_pc ~ partido_de_esquerda)
# t.test(pop ~ partido_de_esquerda)
# t.test(proporcao_jovem ~ partido_de_esquerda)
# t.test(proporcao_idoso ~ partido_de_esquerda)
# t.test(fracao_pop_masculina ~ partido_de_esquerda)
# t.test(fracao_pop_urbana ~ partido_de_esquerda)
# 
# detach(base_pronta_mandato_summary)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # ------------------------------------------------------------------------------------------------------------------------------
# 
# 
# # Testes de Diferença de média para eleições vencidas por pequena margem (margem menos que 4 p.p.)
# 
# 
# # Selecionando variáveis de interesse para análise descritiva:
# 
# base_pronta_summary_close <- base_pronta %>% 
#   filter(abs(margem_vitoria_esquerda) <= 0.04) %>% 
#   select(despesa_geral_pc, despesa_geral_pib,
#          total_receitas_pc, total_receitas_pib,
#          receita_tributaria_pc, receita_tributaria_pib,
#          assistencia_social_prop, saude_prop, educacao_prop,
#          servidores_comissionados_mil,
#          margem_vitoria_esquerda,
#          pib_pc, pop, proporcao_jovem, proporcao_idoso, fracao_pop_masculina, fracao_pop_urbana,
#          cod_municipio_ibge, ano, mandato, partido_de_esquerda) %>% 
#   mutate_at(vars(margem_vitoria_esquerda,
#                  despesa_geral_pib,
#                  total_receitas_pib, receita_tributaria_pib,
#                  assistencia_social_prop, saude_prop, educacao_prop,
#                  proporcao_jovem, proporcao_idoso, fracao_pop_masculina, fracao_pop_urbana),
#             function(x) {x * 100})
# 
# base_pronta_mandato_summary_close <- base_pronta_mandato %>% 
#   filter(abs(margem_vitoria_esquerda) <= 0.04) %>% 
#   select(despesa_geral_pc, despesa_geral_pib, 
#          total_receitas_pc, total_receitas_pib,
#          receita_tributaria_pc, receita_tributaria_pib,
#          assistencia_social_prop, saude_prop, educacao_prop,
#          servidores_comissionados_mil,
#          margem_vitoria_esquerda,
#          pib_pc, pop, proporcao_jovem, proporcao_idoso, fracao_pop_masculina, fracao_pop_urbana,
#          cod_municipio_ibge, mandato, partido_de_esquerda) %>% 
#   mutate_at(vars(margem_vitoria_esquerda,
#                  despesa_geral_pib,
#                  total_receitas_pib, receita_tributaria_pib,
#                  assistencia_social_prop, saude_prop, educacao_prop,
#                  proporcao_jovem, proporcao_idoso, fracao_pop_masculina, fracao_pop_urbana),
#             function(x) {x * 100})
# 
# 
# 
# 
# stargazer(data.frame(base_pronta_summary_close %>% filter(partido_de_esquerda == 1)), summary = T, align = T,
#           digits = 2, digits.extra = 2, rownames = T, digit.separator = "", title = "Left-Wing Parties",
#           omit = c("cod_municipio_ibge", "mandato", "partido_de_esquerda", "margem_vitoria_esquerda"),
#           summary.stat = c("mean"),
#           covariate.labels = covariates_labels_vector,
#           type = "text")
# 
# # Estatísticas descritivas para partidos de centro e/ou direita:
# stargazer(data.frame(base_pronta_mandato_summary_close %>% filter(partido_de_esquerda == 0)), summary = T, align = T,
#           digits = 2, digits.extra = 2, rownames = T, digit.separator = "", title = "Center and/or Right-Wing Parties",
#           omit = c("cod_municipio_ibge", "mandato", "partido_de_esquerda", "margem_vitoria_esquerda"),
#           summary.stat = c("mean"),
#           covariate.labels = covariates_labels_vector,
#           type = "text")
# 
# 
# 
# 
# 
# attach(base_pronta_mandato_summary_close)
# # Estatísticas Descritivas comparando partidos de esquerda e direita:
# t.test(despesa_geral_pc ~ partido_de_esquerda)
# t.test(despesa_geral_pib ~ partido_de_esquerda)
# # t.test(despesas_correntes_pc ~ partido_de_esquerda)
# # t.test(despesas_correntes_pib ~ partido_de_esquerda)
# t.test(total_receitas_pc ~ partido_de_esquerda)
# t.test(total_receitas_pib ~ partido_de_esquerda)
# t.test(receita_tributaria_pc ~ partido_de_esquerda)
# t.test(receita_tributaria_pib ~ partido_de_esquerda)
# t.test(assistencia_social_prop ~ partido_de_esquerda)
# t.test(saude_prop ~ partido_de_esquerda)
# t.test(educacao_prop ~ partido_de_esquerda)
# t.test(servidores_comissionados_mil ~ partido_de_esquerda)
# # t.test(taxa_homicidios ~ partido_de_esquerda)
# # t.test(margem_vitoria_esquerda ~ partido_de_esquerda)
# t.test(pib_pc ~ partido_de_esquerda)
# t.test(pop ~ partido_de_esquerda)
# t.test(proporcao_jovem ~ partido_de_esquerda)
# t.test(proporcao_idoso ~ partido_de_esquerda)
# t.test(fracao_pop_masculina ~ partido_de_esquerda)
# t.test(fracao_pop_urbana ~ partido_de_esquerda)
# 
# detach(base_pronta_mandato_summary_close)
# 
# 
# 
