rm(list = ls())

library(tidyverse)
library(rgdal)
library(httr)
source("FUN.R")


# 1. Carregando Banco -----------------------------------------------------

anos <- c(seq(2002, 2014, by = 4))

cargos <- c(1,3,5,6)

args <- expand.grid(cargo = cargos, ano = anos) %>% 
  as.list()

colunas <- c("ANO_ELEICAO",
             "SIGLA_PARTIDO",
             "NUMERO_PARTIDO",
             "CODIGO_CARGO",
             "DESCRICAO_CARGO",
             "NUM_TITULO_ELEITORAL_CANDIDATO",
             "UF",
             "QTDE_VOTOS")

votos_ls <- pmap(args, get_votos, colunas = colunas, agre_reg = 2, agre_pol = 2)

votos_ls <- votos_ls %>% 
  map(~mutate(.data = ., 
              NUM_TITULO_ELEITORAL_CANDIDATO = as.character(NUM_TITULO_ELEITORAL_CANDIDATO)))

votos_df <- bind_rows(votos_ls)

rm(args, votos_ls)

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
