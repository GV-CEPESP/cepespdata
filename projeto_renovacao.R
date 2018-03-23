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

# 2. Consistência  --------------------------------------------------------

glimpse(candidatos_df)

##2.1. Seleção de candidatos deferidos, ou seja, que de fato concorreram a aleição.

candidatos_df %>% 
  count(DES_SITUACAO_CANDIDATURA) %>% 
  arrange(desc(n))

candidatos_df <- candidatos_df %>% 
  filter(DES_SITUACAO_CANDIDATURA %in% c("DEFERIDO", "DEFERIDO COM RECURSO", "SUB JUDICE", "SUB-JÚDICE"))

##2.2. Criação de uma variável única para candidatos eleitos

candidatos_df %>% 
  count(DESC_SIT_TOT_TURNO) %>% 
  arrange(desc(n))

candidatos_df %>% 
  count(ANO_ELEICAO, DESC_SIT_TOT_TURNO) %>% 
  arrange(desc(n)) %>% 
  print(n = Inf)

candidatos_df <- candidatos_df %>% 
  mutate(ELEITO = ifelse(DESC_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR QP", "MÉDIA", "ELEITO POR MÉDIA"), T, F))

##2.3. Escolhendo uma variável para comprar a candidatura ao longo dos anos

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

candidatos_df %>% 
  count(ANO_ELEICAO, CPF_CANDIDATO) %>% 
  filter(n > 1)

candidatos_df %>% 
  count(ANO_ELEICAO, NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  filter(n > 1)

candidatos_df %>% 
  count(ANO_ELEICAO, SIGLA_UF, NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  filter(n > 1)

#' Vamos ficar com o Título de Eleitor já que a quantidade de missings é menor
#' Podemos pensar em alternativas para não perder esses dados

# Filtra dados com missings ou o caso duplicado 29427820370

candidatos_df <- candidatos_df %>% 
  filter(!is.na(NUM_TITULO_ELEITORAL_CANDIDATO)) %>% 
  filter(NUM_TITULO_ELEITORAL_CANDIDATO != 29427820370)

##2.4. Quantidade de vezes concorridas

candidatos_df <- candidatos_df %>% 
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  arrange(NUM_TITULO_ELEITORAL_CANDIDATO, ANO_ELEICAO) %>%
  mutate(CONC_VEZES = 1,
         CONC_VEZES = cumsum(CONC_VEZES),
         CAND_NOVO = ifelse(CONC_VEZES == 1, T, F)) %>% 
  ungroup()

candidatos_df %>% 
  count(CONC_VEZES) %>% 
  filter(n > 5)

##2.5. Criação da variável de duas variáveis: 1) Concorreu na eleição passada; 2) Incumbente

serie_longa <- candidatos_df %>% 
  mutate(eleicao_atual = T) %>% 
  select(ANO_ELEICAO, NUM_TITULO_ELEITORAL_CANDIDATO, eleicao_atual) %>% 
  spread(ANO_ELEICAO, eleicao_atual, fill = FALSE) %>% 
  gather(`1998`:`2014`, key = "ANO_ELEICAO", value = "eleicao_atual", convert = T)

candidatos_df <- serie_longa %>% 
  left_join(candidatos_df)

candidatos_df %>% 
  count(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  filter(n != 5)

candidatos_df <- candidatos_df %>% 
  group_by(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  arrange(NUM_TITULO_ELEITORAL_CANDIDATO, ANO_ELEICAO) %>% 
  mutate(CONC_ELEI_PASSADA = lag(eleicao_atual),
         CONC_ELEI_PASSADA = ifelse(is.na(CONC_ELEI_PASSADA), F, CONC_ELEI_PASSADA),
         INCUMBENTE        = lag(ELEITO),
         INCUMBENTE        = ifelse(is.na(INCUMBENTE), F, INCUMBENTE)) %>% 
  ungroup()

candidatos_df <- candidatos_df %>% 
  filter(eleicao_atual == TRUE)

candidatos_df %>% 
  count(INCUMBENTE)

candidatos_df %>% 
  filter(ELEITO == T) %>% 
  count(INCUMBENTE)

rm(serie_longa)

# 3. Gráficos -------------------------------------------------------------

candidatos_df %>% 
  filter(ELEITO == T) %>% 
  filter(ANO_ELEICAO > 1998) %>% 
  ggplot(mapping = aes(x = ANO_ELEICAO, fill = INCUMBENTE)) +
  geom_bar(position = "fill") +
  scale_x_continuous(breaks = anos)

candidatos_df %>% 
  filter(ANO_ELEICAO > 1998) %>% 
  ggplot(mapping = aes(x = ANO_ELEICAO, fill = CAND_NOVO)) +
  geom_bar(position = "fill") +
  scale_x_continuous(breaks = anos)

candidatos_df %>% 
  filter(ANO_ELEICAO == 2014) %>% 
  filter(ELEITO == T) %>% 
  ggplot(mapping = aes(x = SIGLA_PARTIDO, fill = INCUMBENTE)) +
  geom_bar(position = "fill") +
  coord_flip()


# 4. Mapas ----------------------------------------------------------------

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

