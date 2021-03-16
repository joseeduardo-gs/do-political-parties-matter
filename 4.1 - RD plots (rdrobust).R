############################################################################################################
#                                   ANÁLISE EMPÍRICA - GRÁFICOS RDPLOTS
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
library(export)
library(ggpubr)

# Pacotes Específicos para se trabalhar com RDD:
library(rdd) # Pacote mais básico
library(rddtools)  # Pacote com muitas opções de testes de placebo e sensibilidade das estimações e estimação paramétrica
library(rdrobust)  # Pacote mais abrangente
library(rddapp)    # Pacote com interface em Shiny, reduz a necessidade de se saber programar.

# Opções:
options(scipen = 999) # Desabilita notação scientífica. Para voltar ao padrão -> options(scipen = 1)

# Definindo o diretório onde se encontram as bases de dados prontas para análise empírica:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases de Dados Prontas")

# ----------------------------------------------------------------------------------------------------------------------------------

# Baixando as bases de dados (ESCOLHER A QUE FOR USAR)
# load("bases_prontas_ECD")
load("bases_prontas_ED")

# Definindo o diretório para o R salvar as imagens geradas:
# setwd("C:/Users/joseg_000.PC-JE/Google Drive/FGV-EPGE/Dissertação/Imagens/Partidos de Esquerda, Centro e Direita/Rdplots")
setwd("C:/Users/joseg_000.PC-JE/Google Drive/FGV-EPGE/Dissertação/Imagens/Partidos de Esquerda e Direita/Rdplots")


# Analisando quais partidos estão sendo considerados:
table(base_pronta_mandato$ideologia_partido_eleito)
summarytools::freq(base_pronta_mandato$ideologia_partido_eleito)
summarytools::descr(base_pronta_mandato$margem_vitoria_esquerda, transpose = T, stats = "common", round.digits = 3)


# -------------------------------------------------------------------------------------------------------
#                                 ANÁLISE GRÁFICA DO EFEITO CAUSAL
# -------------------------------------------------------------------------------------------------------



# RDD: Análise Gráfica - RD Plots ---------------------------------------------------

# Primeiramente, iremos plotar um gráfico que relaciona a forcing variable com os outcomes de interesse, para
# termos uma primeira ideia se há indícios de efeito causal dos partidos de esquerda:



# Realizando a análise gráfica utilizando o Pacote rdrobust:
attach(base_pronta_mandato)


# RDPLOTS

# Outcome: despesa geral:
png("despesa_geral_pc.png", width = 1600, height = 837)
rdplot(y = despesa_geral_pc, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Total expenditures per capita", title = "") # Polinômio de quarta ordem
dev.off()

png("despesa_geral_pib.png", width = 1600, height = 837)
rdplot(y = despesa_geral_pib, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Total expenditures as a share of income", title = "") # Polinômio de quarta ordem
dev.off()

# ------------------------------------------------------------------------------------------------------

# Outcome: Receitas Totais per capita:
png("total_receitas_pc.png", width = 1600, height = 837)
rdplot(y = total_receitas_pc, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Current revenues per capita", title = "") # Polinômio de quarta ordem
dev.off()

png("total_receitas_pib.png", width = 1600, height = 837)
rdplot(y = total_receitas_pib, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Current revenues as a share of income", title = "") # Polinômio de quarta ordem
dev.off()


# ----------------------------------------------------------------------------------------------------------
# Outcome: Receita tributária per capita
png("receita_tributaria_pc.png", width = 1600, height = 837)
rdplot(y = receita_tributaria_pc, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Tax revenues per capita", title = "") # Polinômio de quarta ordem
dev.off()

png("receita_tributaria_pib.png", width = 1600, height = 837)
rdplot(y = receita_tributaria_pib, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Tax revenues as a share of income", title = "") # Polinômio de quarta ordem
dev.off()


# ----------------------------------------------------------------------------------------------------------
# Outcome: Total de servidores públicos por mil habitantes:
png("total_servidores_mil.png", width = 1600, height = 837)
rdplot(y = total_servidores_mil, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Total Public Employees (per 1000 residents)", title = "") # Polinômio de quarta ordem
dev.off()

# ---------------------------------------------------------------------------------------------------------
# Outcome: Servidores comissionados por mil habitantes:
png("servidores_comissionados_mil.png", width = 1600, height = 837)
rdplot(y = servidores_comissionados_mil, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Employees holding commissioned positions (per 1000 residents)", title = "") # Polinômio de quarta ordem
dev.off()

# Outcome: despesa com educação
png("educacao_pc.png", width = 1600, height = 837)
rdplot(y = educacao_pc, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with education per capita", title = "") # Polinômio de quarta ordem
dev.off()

png("educacao_pib.png", width = 1600, height = 837)
rdplot(y = educacao_pib, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with education as a share of income", title = "") # Polinômio de quarta ordem
dev.off()

png("educacao_prop.png", width = 1600, height = 837)
rdplot(y = educacao_prop, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with education (share of total)", title = "") # Polinômio de quarta ordem
dev.off()


# ----------------------------------------------------------------------------------------------------------
# Outcome: despesa com saúde
png("saude_pc.png", width = 1600, height = 837)
rdplot(y = saude_pc, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with health per capita", title = "") # Polinômio de quarta ordem
dev.off()

png("saude_pib.png", width = 1600, height = 837)
rdplot(y = saude_pib, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with health as a share of income", title = "") # Polinômio de quarta ordem
dev.off()

png("saude_prop.png", width = 1600, height = 837)
rdplot(y = saude_prop, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with health (share of total)", title = "") # Polinômio de quarta ordem
dev.off()


# ----------------------------------------------------------------------------------------------------------
# Outcome: despesa com assistência social
png("assistencia_social_pc.png", width = 1600, height = 837)
rdplot(y = assistencia_social_pc, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with social assistance per capita", title = "") # Polinômio de quarta ordem
dev.off()

png("assistencia_social_pib.png", width = 1600, height = 837)
rdplot(y = assistencia_social_pib, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with social assistance as a share of income", title = "") # Polinômio de quarta ordem
dev.off()

png("assistencia_social_prop.png", width = 1600, height = 837)
rdplot(y = assistencia_social_prop, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with social assistance (share of total)", title = "") # Polinômio de quarta ordem
dev.off()



#########################################################################################################
#########################################################################################################
#########################################################################################################


# RDPLOTS - Agora tranasformando as variáveis que representam o tamanho de governo em log:

# Outcome: despesa geral:
png("log_despesa_geral_pc.png", width = 1600, height = 837)
rdplot(y = log(despesa_geral_pc), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log total expenditures per capita", title = "") # Polinômio de quarta ordem
dev.off()

png("log_despesa_geral_pib.png", width = 1600, height = 837)
rdplot(y = log(despesa_geral_pib), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log total expenditures as a share of income", title = "") # Polinômio de quarta ordem
dev.off()

# --------------------------------------------------------------------------------------------------------
# Outcome: Receitas Totais per capita:
png("log_total_receitas_pc.png", width = 1600, height = 837)
rdplot(y = log(total_receitas_pc), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log total revenues per capita", title = "") # Polinômio de quarta ordem
dev.off()

png("log_total_receitas_pib.png", width = 1600, height = 837)
rdplot(y = log(total_receitas_pib), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log total revenues as a share of income", title = "") # Polinômio de quarta ordem
dev.off()

# -------------------------------------------------------------------------------------------------------
# Outcome: Receita tributária per capita
png("log_receita_tributaria_pc.png", width = 1600, height = 837)
rdplot(y = log(receita_tributaria_pc), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log tax revenues per capita", title = "") # Polinômio de quarta ordem
dev.off()

png("log_receita_tributaria_pib.png", width = 1600, height = 837)
rdplot(y = log(receita_tributaria_pib), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log tax revenues as a share of income", title = "") # Polinômio de quarta ordem
dev.off()

# ------------------------------------------------------------------------------------------------------
# Outcome: Total de servidores públicos por mil habitantes:
png("log_total_servidores_mil.png", width = 1600, height = 837)
rdplot(y = log(total_servidores_mil + 0.00001), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log total public employees (per 1000 residents)", title = "") # Polinômio de quarta ordem
dev.off()

# ------------------------------------------------------------------------------------------------------
# Outcome: Servidores comissionados por mil habitantes:
png("log_servidores_comissionados_mil.png", width = 1600, height = 837)
rdplot(y = log(servidores_comissionados_mil + 0.00001), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log total employees holding commissioned positions (per 1000 residents)", title = "") # Polinômio de quarta ordem
dev.off()

# ------------------------------------------------------------------------------------------------------
# Outcome: despesa com educação
png("log_educacao_pc.png", width = 1600, height = 837)
rdplot(y = log(educacao_pc), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log expenditures with education per capita", title = "") # Polinômio de quarta ordem
dev.off()

png("log_educacao_pib.png", width = 1600, height = 837)
rdplot(y = log(educacao_pib), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log expenditures with education as a share of income", title = "") # Polinômio de quarta ordem
dev.off()

png("log_educacao_prop.png", width = 1600, height = 837)
rdplot(y = educacao_prop, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with education (share of total)", title = "") # Polinômio de quarta ordem
dev.off()


# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Outcome: despesa com saúde
png("log_saude_pc.png", width = 1600, height = 837)
rdplot(y = log(saude_pc), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log expenditures with health per capita", title = "") # Polinômio de quarta ordem
dev.off()

png("log_saude_pib.png", width = 1600, height = 837)
rdplot(y = log(saude_pib), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log expenditures with health as a share of income", title = "") # Polinômio de quarta ordem
dev.off()

png("log_saude_prop.png", width = 1600, height = 837)
rdplot(y = saude_prop, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with health (share of total)", title = "") # Polinômio de quarta ordem
dev.off()

# -----------------------------------------------------------------------------------------------------------
# Outcome: despesa com assistência social
png("log_assistencia_social_pc.png", width = 1600, height = 837)
rdplot(y = log(assistencia_social_pc + 0.00001), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log expenditures with social assistance per capita", title = "") # Polinômio de quarta ordem
dev.off()

png("log_assistencia_social_pib.png", width = 1600, height = 837)
rdplot(y = log(assistencia_social_pib + 0.00001), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log expenditures with social assistance as a share of income", title = "") # Polinômio de quarta ordem
dev.off()

png("log_assistencia_social_prop.png", width = 1600, height = 837)
rdplot(y = assistencia_social_prop, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with social assistance (share of total)", title = "") # Polinômio de quarta ordem
dev.off()





##########################################################################################################
##########################################################################################################
##########################################################################################################


# Gerando somente os RDplots que serão adicionados ao trabalho final:

# ECD
rdplot(y = log(despesa_geral_pc), x = margem_vitoria_esquerda, subset = log(despesa_geral_pc) > 7.15, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log total expenditures per capita", title = "")
rdplot(y = log(total_receitas_pc), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log total revenues per capita", title = "") 
rdplot(y = log(receita_tributaria_pc), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log tax revenues per capita", title = "")
rdplot(y = log(servidores_comissionados_mil + 0.00001), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log commission employees (per 1000 residents)", title = "")
rdplot(y = educacao_prop, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with education (share of total)", title = "")
rdplot(y = saude_prop, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with health (share of total)", title = "")
rdplot(y = assistencia_social_prop, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with social assistance (share of total)", title = "")
rdplot(y = urbanismo_prop, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with urbanism (share of total)", title = "")


# ED
rdplot(y = log(despesa_geral_pc), x = margem_vitoria_esquerda, subset = log(despesa_geral_pc) > 7.15, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log total expenditures per capita", title = "")
rdplot(y = log(total_receitas_pc), x = margem_vitoria_esquerda, subset = log(total_receitas_pc) > 7.88, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log total revenues per capita", title = "") 
rdplot(y = log(receita_tributaria_pc), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log tax revenues per capita", title = "")
rdplot(y = log(servidores_comissionados_mil + 0.00001), x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Log commission employees (per 1000 residents)", title = "")
rdplot(y = educacao_prop, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with education (share of total)", title = "")
rdplot(y = saude_prop, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with health (share of total)", title = "")
rdplot(y = assistencia_social_prop, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with social assistance (share of total)", title = "")
rdplot(y = urbanismo_prop, x = margem_vitoria_esquerda, nbins = 30, p = 3, col.lines = "red", x.label = "Left Vote share margin of victory", y.label = "Expenditures with urbanism (share of total)", title = "")








detach(base_pronta_mandato)





