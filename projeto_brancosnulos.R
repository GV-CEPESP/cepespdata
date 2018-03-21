rm(list = ls())

library(tidyverse)
library(rgdal)
library(httr)


# 1. Carregando Banco -----------------------------------------------------

anos <- c(seq(1998, 2014, by = 4))

cargos <- c(1,3,5,6)

get_voto <- function(ano, cargo){
  url_base <- "http://cepesp.io/api/consulta/tse"
  
  params <- list(
    `cargo`                = cargo,
    `ano`                  = ano,
    `agregacao_regional`   = 2,
    `agregacao_politica`   = 2,
    `brancos`              = 1,
    `nulos`                = 1,
    
    `selected_columns[]`   = "ANO_ELEICAO",
    `selected_columns[]`   = "SIGLA_PARTIDO",
    `selected_columns[]`   = "NUMERO_PARTIDO",
    `selected_columns[]`   = "CODIGO_CARGO",
    `selected_columns[]`   = "DESCRICAO_CARGO",
    `selected_columns[]`   = "NUM_TITULO_ELEITORAL_CANDIDATO",
    `selected_columns[]`   = "UF",
    `selected_columns[]`   = "QTDE_VOTOS")
  
  data_frame <- GET(url_base, query = params) %>% 
    content(type = "text/csv")
}

votos_ls <- lst()

for(ano in anos){
  for(cargo in cargos){
    i = length(votos_ls) + 1
    
    votos_ls[[i]] <- get_voto(ano, cargo)
  }
}

for(i in seq_along(votos_ls)){
  votos_ls[[i]] <- votos_ls[[i]] %>% 
    mutate(NUM_TITULO_ELEITORAL_CANDIDATO = as.character(NUM_TITULO_ELEITORAL_CANDIDATO))
}

votos_df <- bind_rows(votos_ls)


# 2. Consistência ---------------------------------------------------------

votos_df %>% 
  count(NUMERO_PARTIDO, SIGLA_PARTIDO) %>% 
  print(n = Inf)

votos_df %>% 
  group_by(ANO_ELEICAO, DESCRICAO_CARGO) %>% 
  summarise(sum(NUMERO_PARTIDO %in% c(95,96,97)))

votos_df %>% 
  count()

# 3. Gráficos -------------------------------------------------------------

votos_df %>% 
  group_by(ANO_ELEICAO, DESCRICAO_CARGO) %>% 
  summarise(votos = sum(QTDE_VOTOS)) %>% 
  ggplot(mapping = aes(x = ANO_ELEICAO)) +
  geom_bar()
