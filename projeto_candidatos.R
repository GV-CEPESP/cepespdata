rm(list = ls())

library(tidyverse)
library(httr)
library(rvest)


# 1. Carregando Banco - Candidatos ----------------------------------------

anos <- c(seq(1998, 2014, by = 4))

cargos <- c(1,3,5,6,7,8,11,13)

colunas <- c("ANO_ELEICAO",
             "NUM_TURNO",
             "DESCRICAO_ELEICAO",
             "SIGLA_UF",
             "NOME_CANDIDATO",
             "DES_SITUACAO_CANDIDATURA",
             "NUMERO_PARTIDO",
             "SIGLA_PARTIDO",
             "CODIGO_LEGENDA",
             "SIGLA_LEGENDA",
             "COMPOSICAO_LEGENDA",
             "NOME_COLIGACAO")
  
get_candidato <- function(ano, cargo, colunas){
  url_base <- "http://cepesp.io/api/consulta/candidatos"
  
  params <- list(
    `cargo`              = cargo,
    `ano`                = ano)
  
  for(coluna in colunas){
    i = length(params) + 1
    
    params[[i]] <- coluna
    
    names(params)[i] <- "selected_columns[]"
  }

  data_frame <- GET(url_base, query = params) %>% 
    content(type = "text/csv")
}

candidatos_ls <- lst()

for(ano in anos){
    i = length(candidatos_ls) + 1
    
    candidatos_ls[[i]] <- get_candidato(ano, cargo = 6, colunas)
}

candidatos_df <- bind_rows(candidatos_ls)

candidatos_df <- candidatos_df %>% 
  filter(ANO_ELEICAO %in% c(2014, 2010, 2006))

# 2. Carregando Banco - Legandas ------------------------------------------

colunas <- c("DATA_GERACAO",
             "HORA_GERACAO",
             "ANO_ELEICAO",
             "NUM_TURNO",
             "DESCRICAO_ELEICAO",
             "SIGLA_UF",
             "SIGLA_UE",
             "CODIGO_CARGO",
             "DESCRICAO_CARGO",
             "TIPO_LEGENDA",
             "NUMERO_PARTIDO",
             "SIGLA_PARTIDO",
             "SIGLA_COLIGACAO",
             "NOME_COLIGACAO",
             "COMPOSICAO_COLIGACAO",
             "SEQUENCIAL_COLIGACAO")

legendas_ls <- lst()

get_legenda <- function(ano, cargo, colunas){
  url_base <- "http://cepesp.io/api/consulta/legendas"
  
  params <- list(
    `cargo`              = cargo,
    `ano`                = ano)
  
  for(coluna in colunas){
    i = length(params) + 1
    
    params[[i]] <- coluna
    
    names(params)[i] <- "selected_columns[]"
  }
  
  data_frame <- GET(url_base, query = params) %>% 
    content(type = "text/csv")
}

for(ano in anos){
  i = length(legendas_ls) + 1
  
  legendas_ls[[i]] <- get_legenda(ano, cargo = 6, colunas)

}

for(i in seq_along(legendas_ls)){
  legendas_ls[[i]] <- legendas_ls[[i]] %>% 
    mutate(SEQUENCIAL_COLIGACAO = parse_character(SEQUENCIAL_COLIGACAO))
}

legendas_df <- bind_rows(legendas_ls)

legendas_df %>% 
  count(TIPO_LEGENDA)

legendas_df <- legendas_df %>% 
  mutate(TIPO_LEGENDA = case_when(TIPO_LEGENDA == "PARTIDO ISOLADO" ~ "PARTIDO_ISOLADO",
                                  T                                 ~ TIPO_LEGENDA))

legendas_df %>% 
  count(TIPO_LEGENDA)

# 3. Consistência dos Dados -----------------------------------------------

#'Verificando quais são os partidos com SEQUENCIAL_COLIGACAO e NOME_COLIGACAO %in% c("#NE#", "#NULO#")
#'
#' Aparenmente é possível agrupar por coligação mesmo quando o sequencial é inválido 
#' 

col_teste <- legendas_df %>% 
  group_by(TIPO_LEGENDA) %>% 
  count(SEQUENCIAL_COLIGACAO, nome_valido = !(NOME_COLIGACAO %in% c("#NE#", "#NULO#")))

col_teste %>% 
  arrange(desc(n))

col_teste %>% 
  filter(!nome_valido)


