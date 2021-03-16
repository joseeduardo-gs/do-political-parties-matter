############################################################################################################
#                     EXTRAÇÃO DADOS FINBRA 2013-2016 (SICONFI) - FINANÇAS PÚBLICAS MUNICIPAIS
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
library(gdata)

# Opções:
options(scipen = 999) # Desabilita notação scientífica. Para voltar ao padrão -> options(scipen = 1)


##################################################################################################################


# DESPESAS POR FUNÇÃO

# Função que realiza a limpeza e manipulação dos dados FINBRA para cada ano:
funcao_finbra_despesas_por_funcao <- function(base_finbra, ano) {
  
  finbra <- read.csv2(base_finbra, skip = 3, encoding = "Windows-1252", stringsAsFactors = FALSE)
  
  # Trabalhando apenas com a Categoria Despesas Empenhadas:
  finbra <- finbra %>% filter(Coluna == "Despesas Empenhadas")
  
  # Arrumando a coluna Instituição, que deve indicar somente o nome do muncípio:
  finbra$Instituição <- str_remove_all(finbra$Instituição, 'Prefeitura Municipal d. | - ..')
  
  # Alterando a coluna Conta:
  finbra$Conta <- gsub("Intra-Orçamentárias", "Intraorçamentárias", finbra$Conta, fixed = TRUE)
  finbra$Conta <- gsub("Despesas (Exceto Intraorçamentárias)", "A - Despesas (Exceto Intraorçamentárias)", finbra$Conta, fixed = TRUE)
  finbra$Conta <- gsub("Despesas (Intraorçamentárias)", "B - Despesas (Intraorçamentárias)", finbra$Conta, fixed = TRUE)
  
  # Renomeando e reordenando as variáveis:
  finbra <- finbra %>%
    mutate(ano = ano) %>%
    rename(uf = UF, municipio = Instituição, pop = População, cod_municipio_ibge = Cod.IBGE, conta = Conta, tipo_despesa = Coluna, valor = Valor) %>%
    select(uf, cod_municipio_ibge, municipio, ano, pop:valor, -tipo_despesa)
  
  # Separando a Conta e Número da Conta:
  finbra <- finbra %>% 
    separate(col = conta, into = c('numero_conta', 'conta'), sep = ' - ')
  
  # Selecionando apenas categorias de despesa de interesse (No caso as contas agregadas de cada área,
  # mas talvez eu ainda restrinja mais as variáveis)
  finbra <- finbra %>%
    filter(nchar(numero_conta) <= 2) %>%
    arrange(uf, municipio)
  
  # # Data frame que ajuda a visualizar as categorias de despesa:
  # funcoes_despesa <- finbra %>%
  #   group_by(numero_conta, conta) %>%
  #   summarise(quantidade = n(), soma_municipios = sum(valor))
  
  # Tidying a base de dados (Desse modo mantém a numeração das contas):
  finbra_tidy <- finbra %>%
    pivot_wider(names_from = c(numero_conta, conta), values_from = valor)
  
  # Tidying a base de dados (Sem numeração das contas):
  finbra_tidy <- finbra %>%
    pivot_wider(id_cols = -numero_conta, names_from = conta, values_from = valor) %>%
    rename(despesas_exceto_intraorcamentarias = `Despesas (Exceto Intraorçamentárias)`,legislativa = Legislativa,
           judiciaria = Judiciária, administracao = Administração, seguranca_publica = `Segurança Pública`,
           assistencia_social = `Assistência Social`, saude = Saúde, educacao = Educação, cultura = Cultura,
           direitos_da_cidadania = `Direitos da Cidadania`, urbanismo = Urbanismo, saneamento = Saneamento,
           gestao_ambiental = `Gestão Ambiental`, agricultura = Agricultura,
           comercio_e_servicos = `Comércio e Serviços`,comunicacoes = Comunicações, energia = Energia,
           transporte = Transporte, desporto_e_lazer = `Desporto e Lazer`, encargos_especiais = `Encargos Especiais`,
           encargos_especiais = `Encargos Especiais`, habitacao = Habitação, industria = Indústria,
           ciencia_e_tecnologia = `Ciência e Tecnologia`,essencial_a_justica = `Essencial à Justiça`,
           previdencia_social = `Previdência Social`, despesas_intraorcamentarias = `Despesas (Intraorçamentárias)`,
           trabalho = Trabalho, defesa_nacional = `Defesa Nacional`, relacoes_exteriores = `Relações Exteriores`,
           organizacao_agraria = `Organização Agrária`) %>%
    arrange(uf, municipio)

  list(finbra, finbra_tidy)
}

# -------------------------------------------------------------------------------------------------------------

# Aplicando esta função para arrumar a base de dados de cada ano:
setwd('C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/FINBRA/FINBRA 2013-2016 - SICONFI/Despesas por Função')
c(f_2016_desp_funcao, f_2016_desp_funcao_tidy) %<-% funcao_finbra_despesas_por_funcao('finbra_2016_despesa_por_funcao.csv', ano = 2016)
c(f_2015_desp_funcao, f_2015_desp_funcao_tidy) %<-% funcao_finbra_despesas_por_funcao('finbra_2015_despesa_por_funcao.csv', ano = 2015)
c(f_2014_desp_funcao, f_2014_desp_funcao_tidy) %<-% funcao_finbra_despesas_por_funcao('finbra_2014_despesa_por_funcao.csv', ano = 2014)
c(f_2013_desp_funcao, f_2013_desp_funcao_tidy) %<-% funcao_finbra_despesas_por_funcao('finbra_2013_despesa_por_funcao.csv', ano = 2013)















##################################################################################################################


# DESPESAS ORÇAMENTÁRIAS:

# Função que realiza a limpeza e manipulação dos dados FINBRA para cada ano:
funcao_finbra_despesas_orcamentarias <- function(base_finbra, ano) {
  
  finbra <- read.csv2(base_finbra, skip = 3, encoding = "Windows-1252", stringsAsFactors = FALSE)
  
  # Trabalhando apenas com a Categoria Despesas Empenhadas:
  finbra <- finbra %>% filter(Coluna == "Despesas Empenhadas")
  
  # Arrumando a coluna Instituição, que deve indicar somente o nome do muncípio:
  finbra$Instituição <- str_remove_all(finbra$Instituição, 'Prefeitura Municipal d. | - ..')
  
  # Alterando a coluna Conta:
  finbra$Conta <- gsub("Total Geral da Despesa", "A - Total Geral da Despesa", finbra$Conta, fixed = TRUE)
  finbra$Conta <- gsub("Total Despesa", "A - Total Geral da Despesa", finbra$Conta, fixed = TRUE)
  
  # Renomeando e reordenando as variáveis:
  finbra <- finbra %>%
    mutate(ano = ano) %>%
    rename(uf = UF, municipio = Instituição, pop = População, cod_municipio_ibge = Cod.IBGE, conta = Conta, tipo_despesa = Coluna, valor = Valor) %>%
    select(uf, cod_municipio_ibge, municipio, ano, pop:valor, -tipo_despesa)
  
  # Separando a Conta e Número da Conta:
  finbra <- finbra %>% 
    separate(col = conta, into = c('numero_conta', 'conta'), sep = ' - ', extra = "merge")
  
  # Selecionando apenas categorias de despesa de interesse (No caso as contas agregadas de cada área,
  # mas talvez eu ainda restrinja mais as variáveis)
  finbra <- finbra %>%
    filter(conta %in% c("Total Geral da Despesa", "Despesas Correntes", "Despesas de Capital")) %>%
    arrange(uf, municipio)
  
  # # Data frame que ajuda a visualizar as categorias de despesa:
  # funcoes_despesa <- finbra %>%
  #   group_by(numero_conta, conta) %>%
  #   summarise(quantidade = n(), soma_municipios = sum(valor))
  
  # # Tidying a base de dados (Desse modo mantém a numeração das contas):
  # finbra_tidy <- finbra %>%
  #   pivot_wider(names_from = c(numero_conta, conta), values_from = valor)
  
  # Tidying a base de dados (Sem numeração das contas):
  finbra_tidy <- finbra %>%
    pivot_wider(id_cols = -numero_conta, names_from = conta, values_from = valor) %>%
    arrange(uf, municipio) %>%
    rename(despesa_geral = `Total Geral da Despesa`, despesas_correntes = `Despesas Correntes`,
           despesas_capital = `Despesas de Capital`)
  
  list(finbra, finbra_tidy)
}

# -------------------------------------------------------------------------------------------------------------

# Aplicando esta função para arrumar a base de dados de cada ano:
setwd('C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/FINBRA/FINBRA 2013-2016 - SICONFI/Despesas Orçamentárias')
c(f_2016_desp_orcamentarias, f_2016_desp_orcamentarias_tidy) %<-% funcao_finbra_despesas_orcamentarias('finbra_despesas_orcamentarias_2016.csv', ano = 2016)
c(f_2015_desp_orcamentarias, f_2015_desp_orcamentarias_tidy) %<-% funcao_finbra_despesas_orcamentarias('finbra_despesas_orcamentarias_2015.csv', ano = 2015)
c(f_2014_desp_orcamentarias, f_2014_desp_orcamentarias_tidy) %<-% funcao_finbra_despesas_orcamentarias('finbra_despesas_orcamentarias_2014.csv', ano = 2014)
c(f_2013_desp_orcamentarias, f_2013_desp_orcamentarias_tidy) %<-% funcao_finbra_despesas_orcamentarias('finbra_despesas_orcamentarias_2013.csv', ano = 2013)







##################################################################################################################


# RECEITAS ORÇAMENTÁRIAS:

# Função que realiza a limpeza e manipulação dos dados FINBRA para cada ano:
funcao_finbra_receitas_orcamentarias <- function(base_finbra, ano) {
  
  finbra <- read.csv2(base_finbra, skip = 3, encoding = "Windows-1252", stringsAsFactors = FALSE)
  
  # Trabalhando apenas com a Categoria "Receitas Brutas Realizadas":
  # finbra <- finbra %>% filter(Coluna == "Receitas Brutas Realizadas")
  # finbra <- finbra %>% filter(Coluna == "Receitas Realizadas")
  
  # Arrumando a coluna Instituição, que deve indicar somente o nome do muncípio:
  finbra$Instituição <- str_remove_all(finbra$Instituição, 'Prefeitura Municipal d. | - ..')
  
  # Alterando a coluna Conta:
  finbra$Conta <- ifelse(finbra$Coluna == "Receitas Brutas Realizadas" & finbra$Conta == "Total Receitas", "A - Total Receitas", finbra$Conta)
  finbra$Conta <- ifelse(finbra$Coluna == "Deduções - Transferências Constitucionais" & finbra$Conta == "Total Receitas", "B - Total Receitas", finbra$Conta)
  finbra$Conta <- ifelse(finbra$Coluna == "Deduções - FUNDEB" & finbra$Conta == "Total Receitas", "C - Total Receitas", finbra$Conta)
  finbra$Conta <- ifelse(finbra$Coluna == "Outras Deduções da Receita" & finbra$Conta == "Total Receitas", "D - Total Receitas", finbra$Conta)
  finbra$Conta <- ifelse(finbra$Coluna == "Receitas Realizadas" & finbra$Conta == "Total Receitas", "A - Total Receitas", finbra$Conta)
  finbra$Conta <- ifelse(finbra$Coluna == "Deduções da Receita" & finbra$Conta == "Total Receitas", "B - Total Receitas", finbra$Conta)
  
  
  # Renomeando e reordenando as variáveis:
  finbra <- finbra %>%
    mutate(ano = ano) %>%
    rename(uf = UF, municipio = Instituição, pop = População, cod_municipio_ibge = Cod.IBGE, conta = Conta, tipo_receita = Coluna, valor = Valor) %>%
    select(uf, cod_municipio_ibge, municipio, ano, pop:valor)
  
  # Separando a Conta e Número da Conta:
  finbra <- finbra %>% 
    separate(col = conta, into = c('numero_conta', 'conta'), sep = ' - ', extra = "merge")
  
  # Selecionando apenas categorias da receita de interesse (No caso as contas agregadas de cada área,
  # mas talvez eu ainda restrinja mais as variáveis)
  finbra <- finbra %>%
    filter(conta %in% c("Total Receitas", "Receitas Correntes", "Receita Tributária", "Impostos",
                        "Transferências Correntes", "Receitas de Capital")) %>%
    arrange(uf, municipio)
  
  # Tornando as contas de deduções em números negativos:
  finbra$valor <- ifelse(finbra$tipo_receita %in% c("Receitas Brutas Realizadas", "Receitas Realizadas"), finbra$valor, -finbra$valor)
  
  
  # Mostrando apenas as receitas líquidas (subtraídas as deduções)
  finbra <- finbra %>%
    group_by(cod_municipio_ibge, conta) %>%
    mutate(valor_liquido = sum(valor)) %>%
    ungroup() %>%
    filter(tipo_receita %in% c("Receitas Brutas Realizadas", "Receitas Realizadas")) %>%
    mutate(tipo_receita = "Receitas Líquidas (Após Deduções)") %>%
    select(-valor)
  
  # # Data frame que ajuda a visualizar as categorias de despesa:
  # funcoes_despesa <- finbra %>%
  #   group_by(numero_conta, conta) %>%
  #   summarise(quantidade = n(), soma_municipios = sum(valor))
  
  # # Tidying a base de dados (Desse modo mantém a numeração das contas):
  # finbra_tidy <- finbra %>%
  #   pivot_wider(names_from = c(numero_conta, conta), values_from = valor)
  
  # Tidying a base de dados (Sem numeração das contas):
  finbra_tidy <- finbra %>%
    pivot_wider(id_cols = -c(numero_conta, tipo_receita), names_from = conta, values_from = valor_liquido) %>%
    arrange(uf, municipio) %>%
    rename(total_receitas = `Total Receitas`, receitas_correntes = `Receitas Correntes`,
           receita_tributaria = `Receita Tributária`, impostos = Impostos,
           receita_transferencias_correntes = `Transferências Correntes`, receitas_capital = `Receitas de Capital`)
  
  list(finbra, finbra_tidy)
}

# -------------------------------------------------------------------------------------------------------------

# Aplicando esta função para arrumar a base de dados de cada ano:
setwd('C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/FINBRA/FINBRA 2013-2016 - SICONFI/Receitas Orçamentárias')
c(f_2016_receitas_orcamentarias, f_2016_receitas_orcamentarias_tidy) %<-% funcao_finbra_receitas_orcamentarias('finbra_receitas_orcamentarias_2016.csv', ano = 2016)
c(f_2015_receitas_orcamentarias, f_2015_receitas_orcamentarias_tidy) %<-% funcao_finbra_receitas_orcamentarias('finbra_receitas_orcamentarias_2015.csv', ano = 2015)
c(f_2014_receitas_orcamentarias, f_2014_receitas_orcamentarias_tidy) %<-% funcao_finbra_receitas_orcamentarias('finbra_receitas_orcamentarias_2014.csv', ano = 2014)
c(f_2013_receitas_orcamentarias, f_2013_receitas_orcamentarias_tidy) %<-% funcao_finbra_receitas_orcamentarias('finbra_receitas_orcamentarias_2013.csv', ano = 2013)





##############################################################################################################

# SALVANDO AS BASES EXTRAÍDAS:

# Mantendo somente as bases em formato tidy, e simplificando seus nomes:
f_desp_funcao_2013 <- f_2013_desp_funcao_tidy
f_desp_funcao_2014 <- f_2014_desp_funcao_tidy
f_desp_funcao_2015 <- f_2015_desp_funcao_tidy
f_desp_funcao_2016 <- f_2016_desp_funcao_tidy
finbra_desp_funcao_siconfi <- rbind(f_desp_funcao_2013, f_desp_funcao_2014, f_desp_funcao_2015, f_desp_funcao_2016) %>%
  arrange(uf, municipio)

f_desp_orcamentarias_2013 <- f_2013_desp_orcamentarias_tidy
f_desp_orcamentarias_2014 <- f_2014_desp_orcamentarias_tidy
f_desp_orcamentarias_2015 <- f_2015_desp_orcamentarias_tidy
f_desp_orcamentarias_2016 <- f_2016_desp_orcamentarias_tidy
finbra_desp_orcamentarias_siconfi <- rbind(f_desp_orcamentarias_2013, f_desp_orcamentarias_2014,
                                      f_desp_orcamentarias_2015, f_desp_orcamentarias_2016) %>%
  arrange(uf, municipio)


f_receitas_orcamentarias_2013 <- f_2013_receitas_orcamentarias_tidy
f_receitas_orcamentarias_2014 <- f_2014_receitas_orcamentarias_tidy
f_receitas_orcamentarias_2015 <- f_2015_receitas_orcamentarias_tidy
f_receitas_orcamentarias_2016 <- f_2016_receitas_orcamentarias_tidy
finbra_receitas_orcamentarias_siconfi <- rbind(f_receitas_orcamentarias_2013, f_receitas_orcamentarias_2014,
                                               f_receitas_orcamentarias_2015, f_receitas_orcamentarias_2016) %>%
  arrange(uf, municipio)


# Mantendo apenas as bases de interesse:
keep(f_desp_funcao_2013, f_desp_funcao_2014, f_desp_funcao_2015, f_desp_funcao_2016,
     f_desp_orcamentarias_2013, f_desp_orcamentarias_2014, f_desp_orcamentarias_2015, f_desp_orcamentarias_2016,
     f_receitas_orcamentarias_2013, f_receitas_orcamentarias_2014, f_receitas_orcamentarias_2015, f_receitas_orcamentarias_2016,
     finbra_desp_funcao_siconfi, finbra_desp_orcamentarias_siconfi, finbra_receitas_orcamentarias_siconfi,
     all = TRUE, sure = TRUE)

keep(finbra_desp_funcao_siconfi, finbra_desp_orcamentarias_siconfi, finbra_receitas_orcamentarias_siconfi,
     all = TRUE, sure = TRUE)


# Salvando
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R")
rm(list=lsf.str())        # Removendo as funções
save(list = objects(), file = 'bases_finbra_siconfi')
rm(list = ls())
load("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Geradas no R/bases_finbra_siconfi")


# Exportando Bases para formato CSV:
getwd()
setwd("C:/Users/joseg_000.PC-JE/Documents/Dissertação - Dados/Bases Exportadas para CSV")

finbra_desp_funcao_siconfi <- write.csv2(finbra_desp_funcao_siconfi, file = "finbra_desp_funcao_siconfi.csv")
finbra_desp_orcamentarias_siconfi <- write.csv2(finbra_desp_orcamentarias_siconfi, file = "finbra_desp_orcamentarias_siconfi.csv")
finbra_receitas_orcamentarias_siconfi <- write.csv2(finbra_receitas_orcamentarias_siconfi, file = "finbra_receitas_orcamentarias_siconfi.csv")
