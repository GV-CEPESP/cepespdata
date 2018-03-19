rm(list = ls())

library(tidyverse)
library(httr)

# 1. Carregando Banco -----------------------------------------------------

anos <- c(seq(1998, 2014, by = 4))

cargos <- c(1,3,5,6,7,8,11,13)

colunas <- c("ANO_ELEICAO",
             "NUM_TURNO",
             "DESCRICAO_ELEICAO",
             "NOME_UF",
             "CODIGO_CARGO",
             "NOME_CANDIDATO",
             "DES_SITUACAO_CANDIDATURA",
             "NUMERO_PARTIDO",
             "SIGLA_PARTIDO",
             "CODIGO_LEGENDA",
             "SIGLA_LEGENDA",
             "COMPOSICAO_LEGENDA",
             "NOME_COLIGACAO",
             "CODIGO_OCUPACAO",
             "DESCRICAO_OCUPACAO",
             "IDADE_DATA_ELEICAO",
             "DESCRICAO_SEXO",
             "DESCRICAO_GRAU_INSTRUCAO",
             "DESCRICAO_ESTADO_CIVIL",
             "DESCRICAO_COR_RACA",
             "QTDE_VOTOS",
             "TIPO_LEGENDA",
             "DESC_SIT_TOT_TURNO")

get_eleicao <- function(ano, cargo, agreg_reg, agreg_pol, colunas){
  url_base <- "http://cepesp.io/api/consulta/tse"
  
  params <- list(
    `cargo`              = cargo,
    `ano`                = ano,
    `agregacao_regional` = agreg_reg,
    `agregacao_politica` = agreg_pol)
  
  for(coluna in colunas){
    i = length(params) + 1
    
    params[[i]] <- coluna
    
    names(params)[i] <- "selected_columns[]"
  }
  
  data_frame <- GET(url_base, query = params) %>% 
    content(type = "text/csv")
}

eleicao_ls <- lst()

for(ano in anos){
  i = length(eleicao_ls) + 1
  
  eleicao_ls[[i]] <- get_eleicao(ano,agreg_reg = 2, agreg_pol = 2, cargo = 6, colunas = colunas)
}

eleicao_df <- bind_rows(eleicao_ls)


# 2. Consistência ---------------------------------------------------------

candidatos_df %>% 
  count(DESC_SIT_TOT_TURNO)

candidatos_df %>% 
  filter(DES_SITUACAO_CANDIDATURA == "DEFERIDO") %>% 
  count(DESC_SIT_TOT_TURNO)

candidatos_df %>% 
  filter(DES_SITUACAO_CANDIDATURA == "DEFERIDO") %>% 
  count(SIGLA_PARTIDO) %>% 
  print(n = Inf)

candidatos_df %>% 
  filter(DES_SITUACAO_CANDIDATURA == "DEFERIDO") %>% 
  filter(DESC_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR QP", "ELEITO POR MÉDIA", "MÉDIA")) %>% 
  count(ANO_ELEICAO)
  count(SIGLA_PARTIDO)
