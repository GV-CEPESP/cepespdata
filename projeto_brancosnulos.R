rm(list = ls())

library(tidyverse)
library(rgdal)
library(httr)
source("FUN.R")


# 1. Carregando Banco -----------------------------------------------------

anos <- c(seq(2002, 2014, by = 4))

cargos <- c(1,3,5,6)

args <- expand.grid(cargo = cargos, ano = anos, agre_reg = 2)

args$colunas <- list(colunas = c("ANO_ELEICAO",
                     "SIGLA_UE",
                     "UF",
                     "NUM_TURNO",
                     "DESCRICAO_ELEICAO",
                     "CODIGO_CARGO",
                     "DESCRICAO_CARGO",
                     "NUMERO_CANDIDATO",
                     "QTDE_VOTOS"))

votos_ls <- pmap(args, get_votos_only)

votos_df <- bind_rows(votos_ls)

rm(args, votos_ls)

# 2. Consistência ---------------------------------------------------------

votos_df %>% 
  count(NUMERO_CANDIDATO) %>% 
  arrange(desc(n))

votos_df <- votos_df %>% 
  mutate(TIPO_VOTO = ifelse(NUMERO_CANDIDATO %in% c(96,96,97), "VOTO BRANCO/NULO", "VOTO VÁLIDO"))

# 3. Gráficos -------------------------------------------------------------

votos_pres_df %>% 
  group_by(ANO_ELEICAO, NUM_TURNO, TIPO_VOTO) %>% 
  summarise(votos = sum(QTDE_VOTOS)) %>% 
  group_by(ANO_ELEICAO, NUM_TURNO) %>% 
  mutate(TOTAL = sum(votos)) %>% 
  mutate(PROP  = votos/TOTAL) %>% 
  filter(TIPO_VOTO == "VOTO BRANCO/NULO" & NUM_TURNO == 1) %>% 
  ggplot(mapping = aes(x = ANO_ELEICAO, y = PROP)) +
  geom_line()

votos_df %>% 
  filter(NUM_TURNO == 1) %>% 
  filter(CODIGO_CARGO == 3) %>% 
  group_by(ANO_ELEICAO, NUM_TURNO, TIPO_VOTO) %>% 
  summarise(votos = sum(QTDE_VOTOS)) %>% 
  mutate(TOTAL = sum(votos),
         PROP  = round(votos/TOTAL, 3)) %>% 
  filter(TIPO_VOTO == "VOTO BRANCO/NULO") %>% 
  ggplot(mapping = aes(x = ANO_ELEICAO, y = PROP)) +
  geom_line() +
  scale_x_continuous(breaks = anos)

votos_df %>% 
  filter(NUM_TURNO == 1 & CODIGO_CARGO == 6) %>% 
  group_by(ANO_ELEICAO, TIPO_VOTO) %>% 
  summarise(votos = sum(QTDE_VOTOS)) %>% 
  mutate(TOTAL = sum(votos),
         PROP  = votos/TOTAL) %>% 
  filter(TIPO_VOTO == "VOTO BRANCO/NULO") %>% 
  ggplot(mapping = aes(x = ANO_ELEICAO, y = PROP)) +
  geom_line() +
  scale_x_continuous(breaks = anos)


# 4. Mapas ----------------------------------------------------------------

brasil <- readOGR("[SP]Brasil", "Brasil", encoding = "UTF-8")

brasil@data

