############################################################################################################
#                                   ANÁLISE EMPÍRICA - TESTES DE ROUBUSTEZ
############################################################################################################


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

# Pacotes para estatísticas descritivas:
library(psych)
library(summarytools)

# Pacotes Específicos para se trabalhar com RDD:
library(rdd) # Pacote mais básico
library(rddtools)  # Pacote com muitas opções de testes de placebo e sensibilidade das estimações e estimação paramétrica
library(rdrobust)  # Pacote mais abrangente
library(rddapp)    # Pacote com interface em Shiny, reduz a necessidade de se saber programar.
library(rddensity)

# Opções:
options(scipen = 999) # Desabilita notação scientífica. Para voltar ao padrão -> options(scipen = 1)

# -----------------------------------------------------------------------------------------------------

# Definindo o diretório onde se encontram as bases de dados prontas para análise empírica:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases de Dados Prontas")

# Baixando as bases de dados (ESCOLHER A QUE FOR USAR)
load("bases_prontas_ECD")
# load("bases_prontas_ED")

# Definindo o diretório para o R salvar as imagens geradas:
setwd("C:/Users/joseg_000.PC-JE/Google Drive/FGV-EPGE/Dissertação/Imagens/Partidos de Esquerda, Centro e Direita/Teste de McCrary")
# setwd("C:/Users/joseg_000.PC-JE/Google Drive/FGV-EPGE/Dissertação/Imagens/Partidos de Esquerda e Direita/Teste de McCrary")


# Analisando quais partidos estão sendo considerados:
table(base_pronta_mandato$ideologia_partido_eleito)
summarytools::freq(base_pronta_mandato$ideologia_partido_eleito)
summarytools::descr(base_pronta_mandato$margem_vitoria_esquerda, transpose = T, stats = "common", round.digits = 3)



### TESTES DE ROBUSTEZ E ASSUMPTION CHECKS:


# Testando se existe manipulação na forcing variable ----------------------

# Teste de McCrary de descontinuidade na forcing variable:
# H0: Não há descontinuidade da forcing variable no cuttof
# H1: Existe descontinuidade da forcing variable no cutoff


png("maccrary_test.png", width = 1600, height = 837)
rdd::DCdensity(base_pronta_mandato$margem_vitoria_esquerda, cutpoint = 0)
dev.off()

rdd::DCdensity(base_pronta_mandato$margem_vitoria_esquerda, ext.out = T, cutpoint = 0)

# Como o p-valor do teste de McCrary para a forcing variable margem_vitoria_esquerda é de 0.8499,
# não podemos rejeitar a hipótese nula de que não há descontinuidade. Portanto, a conclusão é de
# que assumimos que de fato não existe descontinuidade da variável no cutoff.




#######################################################################################################
#######################################################################################################
#######################################################################################################


# Realizando o teste da densidade da forcing variable a partir do pacote rddensity:
density_test <- rddensity(base_pronta_mandato$margem_vitoria_esquerda, c = 0, all = T)
summary(density_test)
rdplotdensity(density_test, base_pronta_mandato$margem_vitoria_esquerda,
              lcol = c("black", "black"), plotRange = c(-1, 1), plotN = 1000,
              CItype = "region",
              xlabel = "Left vote share margin of victory",
              ylabel = "Density",
              title = "Forcing Variable Density Test (P-value = 0.77)")

















