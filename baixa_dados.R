rm(list = ls())

library(tidyverse)
library(httr)

anos <- c(seq(1998, 2016, by = 2))

cargos <- c(1,3,5,6,7,8,11,13)

args_candi <- expand.grid(anos, cargos)

args_candi <- args_candi %>% 
  filter((anos %in% seq(2000, 2016, by = 4) & (Var2 %in% c(11, 13))) |
           (anos %in% seq(1998, 2014, by = 4) & !(Var2 %in% c(11,13))))

# 1. Carregando Banco - Candidatos ----------------------------------------

colunas <- c("ANO_ELEICAO",
             "NOME_CANDIDATO",
             "NUM_TITULO_ELEITORAL_CANDIDATO",
             "CPF_CANDIDATO",
             "IDADE_DATA_ELEICAO",
             "NUM_TURNO",
             "DESCRICAO_ELEICAO",
             "SIGLA_UF",
             "DES_SITUACAO_CANDIDATURA",
             "CODIGO_CARGO",
             "NUMERO_PARTIDO",
             "SIGLA_PARTIDO",
             "CODIGO_LEGENDA",
             "SIGLA_LEGENDA",
             "COMPOSICAO_LEGENDA",
             "NOME_COLIGACAO",
             "DESCRICAO_SEXO",
             "DESCRICAO_GRAU_INSTRUCAO",
             "DESCRICAO_COR_RACA",
             "DESCRICAO_ESTADO_CIVIL")

get_candidato <- function(ano, cargo, colunas){
  message(str_c("Lendo", ano, cargo, sep = " "))
  
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

candidatos_ls <- map2(args_candi$Var1, args_candi$Var2, ~get_candidato(ano = .x, cargo = .y, colunas = colunas) %>% 
                        mutate(cargo = .y))

candidatos_df <- bind_rows(candidatos_ls)

write_rds(candidatos_df, "candidatos.rds")

# 2. Carregando Banco - Legendas ------------------------------------------

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

get_legenda <- function(ano, cargo, colunas){
  message(str_c("Lendo", ano, cargo, sep = " "))
  
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

legendas_ls <- map2(args_candi$Var1, args_candi$Var2, ~get_legenda(ano = .x, cargo = .y, colunas = colunas))

for(i in seq_along(legendas_ls)){
  legendas_ls[[i]] <- legendas_ls[[i]] %>% 
    mutate(SIGLA_UE = parse_character(SIGLA_UE))
}

legendas_df <- bind_rows(legendas_ls)

legendas_df %>% 
  count(TIPO_LEGENDA)

legendas_df <- legendas_df %>% 
  mutate(TIPO_LEGENDA = case_when(TIPO_LEGENDA == "PARTIDO ISOLADO" ~ "PARTIDO_ISOLADO",
                                  T                                 ~ TIPO_LEGENDA))

legendas_df %>% 
  count(TIPO_LEGENDA)

write_rds(legendas_df, "legendas.rds")

rm(legendas_ls, candidatos_ls)


# 3. Carrengado Banco - Votos ---------------------------------------------


