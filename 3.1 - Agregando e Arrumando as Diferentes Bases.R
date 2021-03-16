############################################################################################################
#                                   MANIPULAÇÃO DADOS FINBRA
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


# Opções:
options(scipen = 999) # Desabilita notação scientífica. Para voltar ao padrão -> options(scipen = 1)

setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
load("munic")
load("finbra")

# Unindo a base de dados que contém as variáveis do FINBRA e a base MUNIC:
finbra_munic <- finbra_real %>% 
  full_join(munic %>% select(-c(cod_uf, cod_mun, municipio, MUNICIPIO)), by = c("cod_municipio_ibge", "ano"))

# Criando as variáveis medidas per capta:
finbra_munic_pc <- finbra_munic %>% 
  select(-c(cod_uf:municipio)) %>%
  mutate_at(vars(despesa_geral:servidores_nao_permanentes), function(x) { x/finbra_munic$pop }) %>% 
  rename_at(vars(despesa_geral:servidores_nao_permanentes), function(x) { paste0(x, "_pc") } ) %>% 
  select(-MUNICIPIO, -pop)

# Criando as variáveis medidas como proporção do PIB:
finbra_munic_pib <- finbra_munic %>% 
  select(-c(cod_uf:municipio), -pop, -c(total_servidores:servidores_nao_permanentes)) %>%
  mutate_at(vars(despesa_geral:gestao_ambiental), function(x) { x/finbra_munic$pib }) %>%
  rename_at(vars(despesa_geral:gestao_ambiental), function(x) { paste0(x, "_pib") } ) %>%
  select(-MUNICIPIO, -pib)

# Juntando as bases de dados:
finbra_munic <- finbra_munic %>% 
  full_join(finbra_munic_pc) %>% 
  full_join(finbra_munic_pib)


# Criando as variáveis de gasto em saúde, educação e assistência social como proporção das despesas correntes:
finbra_munic <- finbra_munic %>% 
  mutate(educacao_prop = educacao/despesa_geral,
         saude_prop = saude/despesa_geral,
         assistencia_social_prop = assistencia_social/despesa_geral,
         urbanismo_prop = urbanismo/despesa_geral,
         transporte_prop = transporte/despesa_geral,
         desporto_e_lazer_prop = desporto_e_lazer/despesa_geral,
         seguranca_publica_prop = seguranca_publica/despesa_geral,
         gestao_ambiental_prop = gestao_ambiental/despesa_geral)


# Criando as variáveis que representam o total de servidores e servidores comissionados para cada 1000 habitantes:
finbra_munic <- finbra_munic %>% 
  mutate(total_servidores_mil = total_servidores/(pop/1000),
         servidores_comissionados_mil = servidores_comissionados/(pop/1000))

# Adicionando variável de região e de mandato:
finbra_munic <- finbra_munic %>% 
  mutate(regiao = case_when(sigla_uf %in% c('AL', 'BA', 'CE', 'MA', 'PI', 'PE', 'PB', 'RN', 'SE') ~ 'nordeste',
                            sigla_uf %in% c('GO', 'MT', 'MS', 'DF') ~ 'centro-oeste',
                            sigla_uf %in% c('AC', 'AM', 'AP', 'PA', 'RO', 'RR', 'TO') ~ 'norte',
                            sigla_uf %in% c('PR', 'RS', 'SC') ~ 'sul',
                            sigla_uf %in% c('ES', 'MG', 'RJ', 'SP') ~ 'sudeste')) %>%
  mutate(mandato = case_when(ano == 2000 ~ "2000",
                             ano >= 2001 & ano <= 2004 ~ "2001 a 2004",
                             ano >= 2005 & ano <= 2008 ~ "2005 a 2008",
                             ano >= 2009 & ano <= 2012 ~ "2009 a 2012",
                             ano >= 2013 & ano <= 2016 ~ "2013 a 2016")) %>% 
  select(cod_municipio_ibge:cod_mun, regiao, sigla_uf:ano, mandato, everything())


keep(finbra_munic, all = TRUE, sure = TRUE)



# ----------------------------------------------------------------------------------------------------

# Adicionando as variáveis que indicam o percentual da população masculina e urbana:
load("variaveis_municipais")

finbra_munic <- finbra_munic %>% 
  left_join(variaveis_municipais %>% select(cod_municipio_ibge, ano, fracao_pop_masculina, fracao_pop_urbana))

# ----------------------------------------------------------------------------------------------------

# Adicionando a variável Taxa de homicídios por 100 mil pessoas:
load('tx_homicidios')

finbra_munic <- finbra_munic %>% 
  left_join(tx_homicidios %>% select(-municipio))


# -----------------------------------------------------------------------------------------------------

# Adicionando a variável proporção de jovens e idosos:
load('pop_idade')

finbra_munic <- finbra_munic %>% 
  left_join(pop_idade %>% mutate_at("ano", as.character))

rm(pop_idade, tx_homicidios)


# ----------------------------------------------------------------------------------------------------

# Adicionando a variável Taxa de alfabetização (de pessoas acima de 15 anos):

load('alfabetizacao')

finbra_munic <- left_join(finbra_munic, alfabetizacao %>% mutate_at("ano", as.character))


# ----------------------------------------------------------------------------------------------

load("cor_censo")

finbra_munic <- left_join(finbra_munic, cor_censo %>% mutate_at("ano", as.character))

keep(finbra_munic, all = T, sure = T)


save(finbra_munic, file = "finbra_munic")




# Diretório para salvar as bases de dados finais prontas para análise:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Base de Dados - Clara")

# Salvando as bases de dados:
save(finbra_munic, file = "finbra_munic.R")   # Salvando em formato R

# Salvando a base de dados também em arquivo dta para se utilizar no Stata:
write.dta(finbra_munic, file = "finbra_munic.dta")


