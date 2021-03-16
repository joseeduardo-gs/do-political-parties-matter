############################################################################################################
#                     EXTRAÇÃO DADOS FINBRA 2000-2012 (ACCESS) - FINANÇAS PÚBLICAS MUNICIPAIS
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

# Opções:
options(scipen = 999) # Desabilita notação scientífica. Para voltar ao padrão -> options(scipen = 1)


##################################################################################################################


# DESPESAS ORÇAMENTÁRIAS:

# Função:
funcao_finbra_despesas_orcamentarias <- function(base_finbra, ano) {
  
  f_desp_orcamentarias <- read_xlsx(base_finbra)
  
  f_desp_orcamentarias <- f_desp_orcamentarias %>%
    mutate(ano = ano) %>% 
    select(uf = UF, cod_uf = CD_UF, cod_mun = CD_MUN, municipio = MUNICIPIO,
           ano, pop = Populacao, despesa_geral = `Despesas Orçamentárias`,
           despesas_correntes = `Desp Correntes`, despesas_capital = `Despesas de Capital`)
  
}


# Aplicando a função:
setwd("~/Dissertação - Dados/FINBRA/FINBRA 2000-2012/Despesas Orçamentárias")
f_desp_orcamentarias_2012 <- funcao_finbra_despesas_orcamentarias("finbra_despesas_orcamentarias_2012.xlsx", ano = 2012)
f_desp_orcamentarias_2011 <- funcao_finbra_despesas_orcamentarias("finbra_despesas_orcamentarias_2011.xlsx", ano = 2011)
f_desp_orcamentarias_2010 <- funcao_finbra_despesas_orcamentarias("finbra_despesas_orcamentarias_2010.xlsx", ano = 2010)
f_desp_orcamentarias_2009 <- funcao_finbra_despesas_orcamentarias("finbra_despesas_orcamentarias_2009.xlsx", ano = 2009)
f_desp_orcamentarias_2008 <- funcao_finbra_despesas_orcamentarias("finbra_despesas_orcamentarias_2008.xlsx", ano = 2008)
f_desp_orcamentarias_2007 <- funcao_finbra_despesas_orcamentarias("finbra_despesas_orcamentarias_2007.xlsx", ano = 2007)
f_desp_orcamentarias_2006 <- funcao_finbra_despesas_orcamentarias("finbra_despesas_orcamentarias_2006.xlsx", ano = 2006)
f_desp_orcamentarias_2005 <- funcao_finbra_despesas_orcamentarias("finbra_despesas_orcamentarias_2005.xlsx", ano = 2005)
f_desp_orcamentarias_2004 <- funcao_finbra_despesas_orcamentarias("finbra_despesas_orcamentarias_2004.xlsx", ano = 2004)
f_desp_orcamentarias_2003 <- funcao_finbra_despesas_orcamentarias("finbra_despesas_2003.xlsx", ano = 2003)
f_desp_orcamentarias_2002 <- funcao_finbra_despesas_orcamentarias("finbra_despesas_2002.xlsx", ano = 2002)
f_desp_orcamentarias_2001 <- funcao_finbra_despesas_orcamentarias("finbra_despesas_2001.xlsx", ano = 2001)
f_desp_orcamentarias_2000 <- funcao_finbra_despesas_orcamentarias("finbra_despesas_2000.xlsx", ano = 2000)

finbra_despesas_orcamentarias_antigas <- rbind(f_desp_orcamentarias_2000, f_desp_orcamentarias_2001, f_desp_orcamentarias_2002,
                                       f_desp_orcamentarias_2003, f_desp_orcamentarias_2004, f_desp_orcamentarias_2005,
                                       f_desp_orcamentarias_2006, f_desp_orcamentarias_2007, f_desp_orcamentarias_2008,
                                       f_desp_orcamentarias_2009, f_desp_orcamentarias_2010, f_desp_orcamentarias_2011,
                                       f_desp_orcamentarias_2012) %>%
  arrange(uf, municipio)







# -----------------------------------------------------------------------------------------------------------------------


# RECEITAS ORÇAMENTÁRIAS:

# Função:
funcao_finbra_receitas_orcamentarias <- function(base_finbra, ano) {
  
  f_receitas_orcamentarias <- read_xlsx(base_finbra)
  
  f_receitas_orcamentarias <- f_receitas_orcamentarias %>%
    mutate(ano = ano) %>% 
    select(uf = UF, cod_uf = CD_UF, cod_mun = CD_MUN, municipio = MUNICIPIO,
           ano, pop = Populacao, total_receitas = `Rec Orçamentária`, receitas_correntes = `Rec Correntes`,
           receita_tributaria = `Rec Tributária`, impostos = Impostos,
           receita_transferencias_correntes = `Rec Transf Correntes`, receitas_capital = `Rec de Capital`)
  
}


# Aplicando a função:
setwd("~/Dissertação - Dados/FINBRA/FINBRA 2000-2012/receitas Orçamentárias")
f_receitas_orcamentarias_2012 <- funcao_finbra_receitas_orcamentarias("finbra_receitas_orcamentarias_2012.xlsx", ano = 2012)
f_receitas_orcamentarias_2011 <- funcao_finbra_receitas_orcamentarias("finbra_receitas_orcamentarias_2011.xlsx", ano = 2011)
f_receitas_orcamentarias_2010 <- funcao_finbra_receitas_orcamentarias("finbra_receitas_orcamentarias_2010.xlsx", ano = 2010)
f_receitas_orcamentarias_2009 <- funcao_finbra_receitas_orcamentarias("finbra_receitas_orcamentarias_2009.xlsx", ano = 2009)
f_receitas_orcamentarias_2008 <- funcao_finbra_receitas_orcamentarias("finbra_receitas_orcamentarias_2008.xlsx", ano = 2008)
f_receitas_orcamentarias_2007 <- funcao_finbra_receitas_orcamentarias("finbra_receitas_orcamentarias_2007.xlsx", ano = 2007)
f_receitas_orcamentarias_2006 <- funcao_finbra_receitas_orcamentarias("finbra_receitas_orcamentarias_2006.xlsx", ano = 2006)
f_receitas_orcamentarias_2005 <- funcao_finbra_receitas_orcamentarias("finbra_receitas_orcamentarias_2005.xlsx", ano = 2005)
f_receitas_orcamentarias_2004 <- funcao_finbra_receitas_orcamentarias("finbra_receitas_orcamentarias_2004.xlsx", ano = 2004)
f_receitas_orcamentarias_2003 <- funcao_finbra_receitas_orcamentarias("finbra_receitas_2003.xlsx", ano = 2003)
f_receitas_orcamentarias_2002 <- funcao_finbra_receitas_orcamentarias("finbra_receitas_2002.xlsx", ano = 2002)
f_receitas_orcamentarias_2001 <- funcao_finbra_receitas_orcamentarias("finbra_receitas_2001.xlsx", ano = 2001)
f_receitas_orcamentarias_2000 <- funcao_finbra_receitas_orcamentarias("finbra_receitas_2000.xlsx", ano = 2000)

finbra_receitas_orcamentarias_antigas <- rbind(f_receitas_orcamentarias_2000, f_receitas_orcamentarias_2001, f_receitas_orcamentarias_2002,
                                       f_receitas_orcamentarias_2003, f_receitas_orcamentarias_2004, f_receitas_orcamentarias_2005,
                                       f_receitas_orcamentarias_2006, f_receitas_orcamentarias_2007, f_receitas_orcamentarias_2008,
                                       f_receitas_orcamentarias_2009, f_receitas_orcamentarias_2010, f_receitas_orcamentarias_2011,
                                       f_receitas_orcamentarias_2012) %>%
  arrange(uf, municipio)







# ---------------------------------------------------------------------------------------------------------------------


# DESPESAS POR FUNÇÃO:

# Função: (Para anos de 2012 a 2004)
funcao_finbra_despesas_funcao <- function(base_finbra, ano) {
  
  f_desp_funcao <- read_xlsx(base_finbra)
  
  f_desp_funcao <- f_desp_funcao %>%
    mutate(ano = ano) %>% 
    select(uf = UF, cod_uf = CdUF, cod_mun = CdMun, municipio = MUNICIPIO,
           ano, pop = Populacao, despesas_exceto_intraorcamentarias = `Despesas por Função`,
           administracao = `Administração Geral`, assistencia_social = `Assistência Social`, saude = Saúde,
           educacao = Educação, gestao_ambiental = `Gestão Ambiental`, agricultura = Agricultura,
           desporto_e_lazer = `Desporto e Lazer`, urbanismo = Urbanismo,
           encargos_especiais = `Encargos Especiais`, previdencia_social = `Previdência Social`,
           cultura = Cultura, energia = Energia, transporte = Transporte, habitacao = Habitação,
           legislativa = Legislativa, saneamento = Saneamento, direitos_da_cidadania = `Direitos da Cidadania`,
           essencial_a_justica = `Essencial à Justiça`, seguranca_publica = `Segurança Pública`,
           industria = Indústria, trabalho = Trabalho, comunicacoes = Comunicações,
           defesa_nacional = `Defesa Nacional`, comercio_e_servicos = `Comércio e Serviços`,
           organizacao_agraria = `Organização Agrária`, ciencia_e_tecnologia = `Ciência e Tecnologia`,
           relacoes_exteriores = `Relações Exteriores`)
  
}


# Aplicando a função:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/FINBRA/FINBRA 2000-2012/Despesas por Função")
f_desp_funcao_2012 <- funcao_finbra_despesas_funcao("finbra_despesas_por_funcao_2012.xlsx", ano = 2012)
f_desp_funcao_2011 <- funcao_finbra_despesas_funcao("finbra_despesas_por_funcao_2011.xlsx", ano = 2011)
f_desp_funcao_2010 <- funcao_finbra_despesas_funcao("finbra_despesas_por_funcao_2010.xlsx", ano = 2010)
f_desp_funcao_2009 <- funcao_finbra_despesas_funcao("finbra_despesas_por_funcao_2009.xlsx", ano = 2009)
f_desp_funcao_2008 <- funcao_finbra_despesas_funcao("finbra_despesas_por_funcao_2008.xlsx", ano = 2008)
f_desp_funcao_2007 <- funcao_finbra_despesas_funcao("finbra_despesas_por_funcao_2007.xlsx", ano = 2007)
f_desp_funcao_2006 <- funcao_finbra_despesas_funcao("finbra_despesas_por_funcao_2006.xlsx", ano = 2006)
f_desp_funcao_2005 <- funcao_finbra_despesas_funcao("finbra_despesas_por_funcao_2005.xlsx", ano = 2005)
f_desp_funcao_2004 <- funcao_finbra_despesas_funcao("finbra_despesas_por_funcao_2004.xlsx", ano = 2004)




# Função: (Para anos de 2003 a 2002)
funcao_finbra_despesas_funcao <- function(base_finbra, ano) {
  
  f_desp_funcao <- read_xlsx(base_finbra)
  
  f_desp_funcao <- f_desp_funcao %>%
    mutate(ano = ano) %>% 
    select(uf = UF, cod_uf = CD_UF, cod_mun = CD_MUN, municipio = MUNICIPIO,
           ano, pop = Populacao, despesas_exceto_intraorcamentarias = `Despesas Orçamentárias`,
           administracao = `Administração`, assistencia_social = `Assistência Social`, saude = Saúde,
           educacao = Educação, gestao_ambiental = `Gestão Ambiental`, agricultura = Agricultura,
           desporto_e_lazer = `Desporto e Lazer`, urbanismo = Urbanismo,
           encargos_especiais = `Encargos Especiais`, previdencia_social = `Previdência Social`,
           cultura = Cultura, energia = Energia, transporte = Transporte, habitacao = Habitação,
           legislativa = Legislativa, saneamento = Saneamento, direitos_da_cidadania = `Direitos da Cidadania`,
           essencial_a_justica = `Essencial à Justiça`, seguranca_publica = `Segurança Pública`,
           industria = Indústria, trabalho = Trabalho, comunicacoes = Comunicações,
           defesa_nacional = `Defesa Nacional`, comercio_e_servicos = `Comércio e Serviços`,
           organizacao_agraria = `Organização Agrária`, ciencia_e_tecnologia = `Ciência e Tecnologia`,
           relacoes_exteriores = `Relações Exteriores`)
  
}


# Aplicando a função:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/FINBRA/FINBRA 2000-2012/Despesas por Função")
f_desp_funcao_2003 <- funcao_finbra_despesas_funcao("finbra_despesas_2003.xlsx", ano = 2003)
f_desp_funcao_2002 <- funcao_finbra_despesas_funcao("finbra_despesas_2002.xlsx", ano = 2002)





# Função: (Para anos de 2001 a 2000)
funcao_finbra_despesas_funcao <- function(base_finbra, ano) {
  
  f_desp_funcao <- read_xlsx(base_finbra)
  
  f_desp_funcao <- f_desp_funcao %>%
    mutate(ano = ano) %>% 
    select(uf = UF, cod_uf = CD_UF, cod_mun = CD_MUN, municipio = MUNICIPIO,
           ano, pop = Populacao, despesas_exceto_intraorcamentarias = `Despesas Orçamentárias`,
           legislativa = Legislativa, judiciaria = Judiciária, plaejamento = Planejamento,
           agricultura = Agricultura, educacao_e_cultura = `Educação e Cultura`,
           habitacao_e_urbanismo = `Habitação e Urbanismo`, industria_e_comercio = `Indústria e Comércio`,
           saude_e_saneamento = `Saúde e Saneamento`,
           assistencia_e_previdencia = `Assistência e Previdência`, transporte = Transporte,
           seguranca_publica = `Segurança Pública`, desenvolvimento_regional = `Desenvolvimento Regional`,
           comunicacoes = Comunicações, outras = Outras)
  }


# Aplicando a função:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/FINBRA/FINBRA 2000-2012/Despesas por Função")
f_desp_funcao_2001 <- funcao_finbra_despesas_funcao("finbra_despesas_2001.xlsx", ano = 2001)
f_desp_funcao_2000 <- funcao_finbra_despesas_funcao("finbra_despesas_2000.xlsx", ano = 2000)



finbra_despesas_por_funcao_antigas <- rbind(f_desp_funcao_2002,
                                    f_desp_funcao_2003, f_desp_funcao_2004, f_desp_funcao_2005,
                                    f_desp_funcao_2006, f_desp_funcao_2007, f_desp_funcao_2008,
                                    f_desp_funcao_2009, f_desp_funcao_2010, f_desp_funcao_2011,
                                    f_desp_funcao_2012) %>%
  arrange(uf, municipio)




# Mantendo somente as bases de dados empilhadas:
keep(finbra_despesas_orcamentarias_antigas, finbra_receitas_orcamentarias_antigas,
     finbra_despesas_por_funcao_antigas, all = TRUE, sure = TRUE)

# Salvando:
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
save(finbra_despesas_orcamentarias_antigas, finbra_receitas_orcamentarias_antigas,
     finbra_despesas_por_funcao_antigas, file = "finbra_antigos")
rm(list = ls())
load("finbra_antigos")



# # Consertando as diferenças de nome do municipio entre os anos:
# finbra_despesas_orcamentarias_antigas_2 <- finbra_despesas_orcamentarias_antigas %>%
#   group_by(cod_uf, cod_mun) %>%
#   mutate(municipio2 = case_when(ano <= 2004 ~ tolower(municipio)))
  
  
  
  
  
  
  
  


