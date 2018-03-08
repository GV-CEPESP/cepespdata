rm(list = ls())

library(tidyverse)
library(httr)
library(rvest)

# 1. Carregando Banco -----------------------------------------------------

get_dep_fed <- function(ano, cargo){
  url_base <- "http://cepesp.io/api/consulta/candidatos"
  
  params <- list(
    `cargo`              = cargo,
    `ano`                = ano
  )

  data_frame <- GET(url_base, query = params) %>% 
    content(type = "text/csv")
}

anos <- c(seq(1998, 2014, by = 4))

cargos <- c(1,3,5,6,7,8,11,13)

candidatos_ls <- lst()

for(ano in anos){
  for(cargo in cargos){
    i = length(candidatos_ls) + 1
    
    candidatos_ls[[i]] <- get_dep_fed(ano, cargo)
  }
}

candidatos_df <- bind_rows(candidatos_ls)

glimpse(candidatos_ls)

# 2. Consistência ---------------------------------------------------------

candidatos_federais_df <- candidatos_federais_df %>% 
  mutate(sit_canditado = case_when(DESC_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP") ~ "Eleito",
                                   T                                                                        ~ "Não eleito"))

candidatos_federais_df %>% 
  count(DESC_SIT_TOT_TURNO)

candidatos_federais_df %>% 
  count(sit_canditado)

candidatos_federais_df %>% 
  group_by(ANO_ELEICAO,SIGLA_UF) %>% 
  count(sit_canditado) 

# 3. Análise Exploratória -------------------------------------------------

candidatos_federais_df %>% 
  group_by(ANO_ELEICAO,SIGLA_UF) %>% 
  count(sit_canditado) %>% 
  spread(key = sit_canditado, value = n) %>% 
  mutate(total = Eleito + `Não eleito`) %>% 
  gather(Eleito:`Não eleito`, key = sit_canditado, value = n) %>% 
  mutate(prop = n / total) %>% 
  filter(ANO_ELEICAO == 2014 & sit_canditado == "Eleito") %>% 
  ggplot(mapping = aes(x = prop, y = reorder(SIGLA_UF, prop), color = sit_canditado)) +
  geom_point()

prop_uf_df <- candidatos_federais_df %>% 
  filter(DES_SITUACAO_CANDIDATURA == "DEFERIDO") %>% 
  group_by(ANO_ELEICAO,SIGLA_UF) %>% 
  count(sit_canditado) %>% 
  spread(key = sit_canditado, value = n) %>% 
  mutate(total = Eleito + `Não eleito`) %>% 
  gather(Eleito:`Não eleito`, key = sit_canditado, value = n) %>% 
  filter(ANO_ELEICAO == 2014) %>%
  filter(sit_canditado == "Eleito") %>% 
  mutate(prop       = n / total)

prop_uf_df %>%
  mutate(media_prop = mean(prop_uf_df$prop)) %>% 
  gather(prop:media_prop, key = props,value = value) %>% 
  ggplot(mapping = aes(x = value, y = reorder(SIGLA_UF, value), group = SIGLA_UF, color = props)) +
  geom_point(size = 3) +
  geom_line(size = 3, alpha = 0.3)
