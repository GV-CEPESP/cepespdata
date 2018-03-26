rm(list = ls())

library(tidyverse)
library(httr)
library(rgdal)
library(ggthemes)
source("FUN.R")

# 1. Download Banco de Dados ----------------------------------------------

cargos = c(3, 5, 6, 7)

anos   = seq(1998, 2014, by = 4)

arg <- expand.grid(cargos, anos)

variaveis <- c("ANO_ELEICAO",
               "CPF_CANDIDATO",
               "NUM_TITULO_ELEITORAL_CANDIDATO",
               "NOME_CANDIDATO",
               "NUM_TURNO",
               "DESCRICAO_CARGO",
               "CODIGO_CARGO",
               "DESCRICAO_ELEICAO",        
               "SIGLA_UF",
               "DES_SITUACAO_CANDIDATURA",
               "NUMERO_PARTIDO",
               "SIGLA_PARTIDO",
               "CODIGO_LEGENDA",
               "SIGLA_LEGENDA",
               "COMPOSICAO_LEGENDA",
               "NOME_COLIGACAO",
               "DESC_SIT_TOT_TURNO")

candidatos_ls <- map2(arg$Var1, arg$Var2, ~get_candidatos(cargo = .x, ano = .y, colunas = variaveis))

candidatos_ls <- map(candidatos_ls, ~mutate(.data = .,
                                           CPF_CANDIDATO                  = as.character(CPF_CANDIDATO),
                                           NUM_TITULO_ELEITORAL_CANDIDATO = as.character(NUM_TITULO_ELEITORAL_CANDIDATO)))

candidatos_df <- bind_rows(candidatos_ls)

rm(candidatos_ls, arg)

# 2. Consistência Geral ---------------------------------------------------

glimpse(candidatos_df)

##2.1. Seleção de candidatos deferidos, ou seja, que de fato concorreram a aleição.

candidatos_df %>% 
  count(DES_SITUACAO_CANDIDATURA) %>% 
  arrange(desc(n))

candidatos_df <- candidatos_df %>% 
  filter(DES_SITUACAO_CANDIDATURA %in% c("DEFERIDO", "DEFERIDO COM RECURSO", "SUB JUDICE", "SUB-JÚDICE"))


##2.3. Criação de uma variável única para candidatos eleitos

candidatos_df %>% 
  count(DESC_SIT_TOT_TURNO) %>% 
  arrange(desc(n))

candidatos_df %>% 
  count(ANO_ELEICAO, DESC_SIT_TOT_TURNO) %>% 
  arrange(desc(n)) %>% 
  print(n = Inf)

candidatos_df <- candidatos_df %>% 
  mutate(ELEITO = ifelse(DESC_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR QP", "MÉDIA", "ELEITO POR MÉDIA"), T, F))

##2.4. Recodificando as Variáveis de CPF e de Título de Eleitor

#' Duas opções:
#'  - CPF
#'  - Título de Eleitor

candidatos_df %>% 
  count(CPF_CANDIDATO) %>% 
  arrange(desc(n))

candidatos_df %>% 
  count(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  arrange(desc(n))

candidatos_df <- candidatos_df %>% 
  mutate(CPF_CANDIDATO                  = ifelse(CPF_CANDIDATO == "#NULO#", NA, CPF_CANDIDATO),
         NUM_TITULO_ELEITORAL_CANDIDATO = ifelse(NUM_TITULO_ELEITORAL_CANDIDATO == "#NI#", NA, NUM_TITULO_ELEITORAL_CANDIDATO),
         CPF_CANDIDATO                  = parse_number(CPF_CANDIDATO),
         NUM_TITULO_ELEITORAL_CANDIDATO = parse_number(NUM_TITULO_ELEITORAL_CANDIDATO))

# 3. Consistência Deputado Federal ----------------------------------------

dep_federais_df <- candidatos_df %>% 
  filter(DESCRICAO_CARGO == "DEPUTADO FEDERAL")

dep_federais_df <- dep_federais_df %>% 
  filter(!is.na(NUM_TITULO_ELEITORAL_CANDIDATO)) %>% 
  filter(NUM_TITULO_ELEITORAL_CANDIDATO != 29427820370)

##3.1. Quantidade de vezes concorridas

dep_federais_df <- dep_federais_df %>% 
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  arrange(NUM_TITULO_ELEITORAL_CANDIDATO, ANO_ELEICAO) %>%
  mutate(CONC_VEZES = 1,
         CONC_VEZES = cumsum(CONC_VEZES),
         CAND_NOVO = ifelse(CONC_VEZES == 1, T, F)) %>% 
  ungroup()

dep_federais_df %>% 
  count(CONC_VEZES)

##3.2. Criação da variável de duas variáveis: 1) Concorreu na eleição passada; 2) Incumbente

serie_longa <- dep_federais_df %>% 
  mutate(eleicao_atual = T) %>% 
  select(ANO_ELEICAO, NUM_TITULO_ELEITORAL_CANDIDATO, eleicao_atual) %>% 
  spread(ANO_ELEICAO, eleicao_atual, fill = FALSE) %>% 
  gather(`1998`:`2014`, key = "ANO_ELEICAO", value = "eleicao_atual", convert = T)

dep_federais_df <- serie_longa %>% 
  left_join(dep_federais_df)

dep_federais_df %>% 
  count(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  filter(n != 5)

dep_federais_df <- dep_federais_df %>% 
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  arrange(NUM_TITULO_ELEITORAL_CANDIDATO, ANO_ELEICAO) %>% 
  mutate(CONC_ELEI_PASSADA = lag(eleicao_atual),
         CONC_ELEI_PASSADA = ifelse(is.na(CONC_ELEI_PASSADA), F, CONC_ELEI_PASSADA),
         INCUMBENTE        = lag(ELEITO),
         INCUMBENTE        = ifelse(is.na(INCUMBENTE), F, INCUMBENTE)) %>% 
  ungroup()

dep_federais_df <- dep_federais_df %>% 
  filter(eleicao_atual == TRUE)

dep_federais_df %>% 
  count(INCUMBENTE)

dep_federais_df %>% 
  filter(ELEITO == T) %>% 
  count(INCUMBENTE)

rm(serie_longa)

# 4. Consistência Governador ----------------------------------------------

governador_df <- candidatos_df %>% 
  filter(DESCRICAO_CARGO == "GOVERNADOR")

##4.1. Avaliando Repetições

governador_df %>% 
  count(ANO_ELEICAO, NUM_TURNO, NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  filter(n > 1)

governador_df %>% 
  filter(ANO_ELEICAO == 2014,
         NUM_TITULO_ELEITORAL_CANDIDATO == 2430682216) %>% 
  View()

governador_df <- governador_df %>% 
  filter(DESCRICAO_ELEICAO != "Eleição Suplementar Governador AM")

##4.2. Criando Variável Quantidade de Vezes Concorridas

governador_df <- governador_df %>% 
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  arrange(NUM_TITULO_ELEITORAL_CANDIDATO, ANO_ELEICAO) %>%
  mutate(CONC_VEZES = 1,
         CONC_VEZES = cumsum(CONC_VEZES),
         CAND_NOVO = ifelse(CONC_VEZES == 1, T, F)) %>% 
  ungroup()

governador_df %>% 
  count(CONC_VEZES)

##4.3. Criação da variável de duas variáveis: 1) Concorreu na eleição passada; 2) Incumbente

serie_longa <- governador_df %>% 
  mutate(eleicao_atual = T) %>% 
  select(ANO_ELEICAO, NUM_TITULO_ELEITORAL_CANDIDATO, eleicao_atual) %>% 
  spread(ANO_ELEICAO, eleicao_atual, fill = FALSE) %>% 
  gather(`1998`:`2014`, key = "ANO_ELEICAO", value = "eleicao_atual", convert = T)

governador_df <- serie_longa %>% 
  left_join(governador_df)

governador_df %>% 
  count(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  filter(n != 5)

governador_df <- governador_df %>% 
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  arrange(NUM_TITULO_ELEITORAL_CANDIDATO, ANO_ELEICAO) %>% 
  mutate(CONC_ELEI_PASSADA = lag(eleicao_atual),
         CONC_ELEI_PASSADA = ifelse(is.na(CONC_ELEI_PASSADA), F, CONC_ELEI_PASSADA),
         INCUMBENTE        = lag(ELEITO),
         INCUMBENTE        = ifelse(is.na(INCUMBENTE), F, INCUMBENTE)) %>% 
  ungroup()

governador_df <- governador_df %>% 
  filter(eleicao_atual == TRUE)

governador_df %>% 
  count(INCUMBENTE)

governador_df %>% 
  filter(ELEITO == T) %>% 
  count(INCUMBENTE)

rm(serie_longa)


# 5. Consistência Deputado Estadual ---------------------------------------

##5.1. Recodigicando a Variável DESCRICAO_CARGO

dep_estadual_df <- candidatos_df %>% 
  filter(DESCRICAO_CARGO %in% c("DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL")) %>% 
  mutate(DESCRICAO_CARGO = ifelse(DESCRICAO_CARGO == "DEPUTADO DISTRITAL", "DEPUTADO ESTADUAL", DESCRICAO_CARGO))

dep_estadual_df %>% 
  count(DESCRICAO_CARGO)

##5.2. Avaliando Reptções de Candidatos

dep_estadual_df %>% 
  count(ANO_ELEICAO, NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  filter(n > 1)

dep_estadual_df %>% 
  count(ANO_ELEICAO, CPF_CANDIDATO) %>% 
  filter(n > 1)

candi_rep <- dep_estadual_df %>% 
  count(ANO_ELEICAO, NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  filter(n > 1) %>% 
  .$NUM_TITULO_ELEITORAL_CANDIDATO

dep_estadual_df %>% 
  filter(CPF_CANDIDATO %in% c(11184310149, 8942260888)) %>% 
  View()

dep_estadual_df <- dep_estadual_df %>% 
  filter(!is.na(NUM_TITULO_ELEITORAL_CANDIDATO)) %>% 
  filter(!(NUM_TITULO_ELEITORAL_CANDIDATO %in% candi_rep))
  
##5.3. Criando Variável Quantidade de Vezes Concorridas

dep_estadual_df <- dep_estadual_df %>% 
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  arrange(NUM_TITULO_ELEITORAL_CANDIDATO, ANO_ELEICAO) %>%
  mutate(CONC_VEZES = 1,
         CONC_VEZES = cumsum(CONC_VEZES),
         CAND_NOVO = ifelse(CONC_VEZES == 1, T, F)) %>% 
  ungroup()

dep_estadual_df %>% 
  count(CONC_VEZES)

##5.4. Criação da variável de duas variáveis: 1) Concorreu na eleição passada; 2) Incumbente

serie_longa <- dep_estadual_df %>% 
  mutate(eleicao_atual = T) %>% 
  select(ANO_ELEICAO, NUM_TITULO_ELEITORAL_CANDIDATO, eleicao_atual) %>% 
  spread(ANO_ELEICAO, eleicao_atual, fill = FALSE) %>% 
  gather(`1998`:`2014`, key = "ANO_ELEICAO", value = "eleicao_atual", convert = T)

dep_estadual_df <- serie_longa %>% 
  left_join(dep_estadual_df)

dep_estadual_df %>% 
  count(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  filter(n != 5)

dep_estadual_df <- dep_estadual_df %>% 
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  arrange(NUM_TITULO_ELEITORAL_CANDIDATO, ANO_ELEICAO) %>% 
  mutate(CONC_ELEI_PASSADA = lag(eleicao_atual),
         CONC_ELEI_PASSADA = ifelse(is.na(CONC_ELEI_PASSADA), F, CONC_ELEI_PASSADA),
         INCUMBENTE        = lag(ELEITO),
         INCUMBENTE        = ifelse(is.na(INCUMBENTE), F, INCUMBENTE)) %>% 
  ungroup()

dep_estadual_df <- dep_estadual_df %>% 
  filter(eleicao_atual == TRUE)

dep_estadual_df %>% 
  count(INCUMBENTE)

dep_estadual_df %>% 
  filter(ELEITO == T) %>% 
  count(INCUMBENTE)

rm(serie_longa)


# 6. Consistência Senador -------------------------------------------------

senador_df <- candidatos_df %>% 
  filter(DESCRICAO_CARGO == "SENADOR")

##6.1. Avaliando Repetições de Candidatos

senador_df %>% 
  count(ANO_ELEICAO, NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  filter(n > 1)

senador_df <- senador_df %>% 
  filter(!is.na(NUM_TITULO_ELEITORAL_CANDIDATO))

##6.2. Criando Variável Quantidade de Vezes Concorridas

senador_df <- senador_df %>% 
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  arrange(NUM_TITULO_ELEITORAL_CANDIDATO, ANO_ELEICAO) %>%
  mutate(CONC_VEZES = 1,
         CONC_VEZES = cumsum(CONC_VEZES),
         CAND_NOVO = ifelse(CONC_VEZES == 1, T, F)) %>% 
  ungroup()

senador_df %>% 
  count(CONC_VEZES)

##6.4. Criação da variável de duas variáveis: 1) Concorreu na eleição passada; 2) Incumbente

serie_longa <- senador_df %>% 
  mutate(eleicao_atual = T) %>% 
  select(ANO_ELEICAO, NUM_TITULO_ELEITORAL_CANDIDATO, eleicao_atual) %>% 
  spread(ANO_ELEICAO, eleicao_atual, fill = FALSE) %>% 
  gather(`1998`:`2014`, key = "ANO_ELEICAO", value = "eleicao_atual", convert = T)

senador_df <- serie_longa %>% 
  left_join(senador_df)

senador_df %>% 
  count(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  filter(n != 5)

senador_df <- senador_df %>% 
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  arrange(NUM_TITULO_ELEITORAL_CANDIDATO, ANO_ELEICAO) %>% 
  mutate(CONC_ELEI_PASSADA = lag(eleicao_atual),
         CONC_ELEI_PASSADA = ifelse(is.na(CONC_ELEI_PASSADA), F, CONC_ELEI_PASSADA),
         INCUMBENTE        = lag(ELEITO),
         INCUMBENTE        = ifelse(is.na(INCUMBENTE), F, INCUMBENTE)) %>% 
  ungroup()

senador_df <- senador_df %>% 
  filter(eleicao_atual == TRUE)

senador_df %>% 
  count(INCUMBENTE)

senador_df %>% 
  filter(ELEITO == T) %>% 
  count(INCUMBENTE)

rm(serie_longa)

# 7. Gráficos -------------------------------------------------------------

##7.1. Deputados Federais

dep_federais_df %>% 
  filter(ELEITO == T) %>% 
  filter(ANO_ELEICAO > 1998) %>% 
  ggplot(mapping = aes(x = ANO_ELEICAO, fill = INCUMBENTE)) +
  geom_bar(position = "fill") +
  scale_x_continuous(breaks = anos)

dep_federais_df %>% 
  filter(ANO_ELEICAO > 1998) %>% 
  ggplot(mapping = aes(x = ANO_ELEICAO, fill = CAND_NOVO)) +
  geom_bar(position = "fill") +
  scale_x_continuous(breaks = anos)

dep_federais_df %>% 
  filter(ANO_ELEICAO == 2014) %>% 
  filter(ELEITO == T) %>% 
  ggplot(mapping = aes(x = SIGLA_PARTIDO, fill = INCUMBENTE)) +
  geom_bar(position = "fill") +
  coord_flip()

##7.2. Deputados Estaduais

dep_estadual_df %>% 
  filter(ELEITO == T) %>% 
  filter(ANO_ELEICAO > 1998) %>% 
  ggplot(mapping = aes(x = SIGLA_UF, fill = INCUMBENTE)) +
  geom_bar(position = "fill")

##7.3. Governadores

governador_df %>% 
  filter(ELEITO == T) %>% 
  filter(ANO_ELEICAO > 1998) %>% 
  ggplot(mapping = aes(x = SIGLA_UF, fill = INCUMBENTE)) +
  geom_bar(position = "fill")

##7.4. Senadores

senador_df %>% 
  filter(ELEITO == T) %>% 
  filter(ANO_ELEICAO > 1998) %>% 
  ggplot(mapping = aes(x = ANO_ELEICAO, fill = INCUMBENTE)) +
  geom_bar(position = "fill")

# 8. Mapas ----------------------------------------------------------------

## Base para mapas

brasil <- readOGR("[SP]Brasil", 'Brasil')

brasil_df <- brasil@data

brasil_df$id <- 0:26

brasil_pl <- fortify(brasil)

brasil_pl$id <- as.integer(brasil_pl$id)

##

dados_renovacao_df <- candidatos_df %>% 
  filter(ELEITO == T) %>% 
  filter(ANO_ELEICAO > 1998) %>% 
  group_by(ANO_ELEICAO, SIGLA_UF) %>% 
  summarise(n = n(),
            INCUMBENTE = sum(INCUMBENTE == T)) %>% 
  mutate(taxa_renovacao = 1 - (INCUMBENTE / n))

dados_renovacao_mapa_df <- brasil_df %>% 
  left_join(dados_renovacao_df, by = c("UF" = "SIGLA_UF"))

for(ano in anos[-1]){
  dados_uso <- dados_renovacao_mapa_df %>% 
    filter(ANO_ELEICAO == ano)
  
  brasil_pl %>% 
    left_join(dados_uso) %>% 
    ggplot(mapping = aes(x = long, y = lat, group = group, fill = taxa_renovacao)) +
    geom_polygon(color = "white") +
    coord_map() +
    theme_map()
  
  ggsave(str_c("projeto_renovacao/", ano, ".png"), height = 8.0, width = 8.0)
}

