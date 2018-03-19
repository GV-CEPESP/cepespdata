rm(list = ls())

library(tidyverse)
library(httr)
library(rgdal)
library(ggthemes)

# 1. Carregando Banco -----------------------------------------------------

anos <- c(seq(1998, 2014, by = 4))

cargos <- c(1,3,5,6,7,8,11,13)

colunas <- c("ANO_ELEICAO",
             "NUM_TURNO",
             "DESCRICAO_ELEICAO",
             "SIGLA_UF",
             "DESCRICAO_UE",
             "NOME_CANDIDATO",
             "DES_SITUACAO_CANDIDATURA",
             "NUMERO_PARTIDO",
             "SIGLA_PARTIDO",
             "CODIGO_LEGENDA",
             "SIGLA_LEGENDA",
             "COMPOSICAO_LEGENDA",
             "NOME_COLIGACAO",
             "DESC_SIT_TOT_TURNO")

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
  mutate(ELEITO = if_else(DESC_SIT_TOT_TURNO %in% c("ELEITO", "ELEITO POR MÉDIA", "ELEITO POR QP", "MÉDIA"), T, F))
  

# 2. Consistência ---------------------------------------------------------

candidatos_df %>% 
  filter(ELEITO == T) %>% 
  count(DES_SITUACAO_CANDIDATURA)

candidatos_df %>% 
  count(DESC_SIT_TOT_TURNO)

candidatos_df %>% 
  filter(DES_SITUACAO_CANDIDATURA %in% c("DEFERIDO", "DEFERIDO COM RECURSO", "SUB-JÚDICE", "SUB JUDICE")) %>% 
  count(DESC_SIT_TOT_TURNO)

candidatos_df %>% 
  filter(DES_SITUACAO_CANDIDATURA %in% c("DEFERIDO", "DEFERIDO COM RECURSO", "SUB-JÚDICE", "SUB JUDICE")) %>% 
  count(SIGLA_PARTIDO) %>% 
  print(n = Inf)

candidatos_df %>% 
  filter(ELEITO == T) %>% 
  filter(DES_SITUACAO_CANDIDATURA %in% c("DEFERIDO", "DEFERIDO COM RECURSO", "SUB-JÚDICE", "SUB JUDICE")) %>% 
  count(ANO_ELEICAO)


# 3. Calculo do NEP -------------------------------------------------------

##NEP por UF (faz sentido?)
eleicao_ufs <- candidatos_df %>% 
  filter(ELEITO == T) %>% 
  group_by(ANO_ELEICAO, SIGLA_UF, SIGLA_PARTIDO) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n))

nep_ufs <- eleicao_ufs %>% 
  group_by(ANO_ELEICAO, SIGLA_UF) %>% 
  summarise(NEP = 1 / sum(freq^2))


##3.1. NEP Nacional 
eleicao_nacional <- candidatos_df %>% 
  filter(ELEITO == T) %>% 
  group_by(ANO_ELEICAO, SIGLA_PARTIDO) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/ sum(n))

nep_nacional <- eleicao_nacional %>% 
  group_by(ANO_ELEICAO) %>% 
  summarise(NEP = 1 / sum(freq^2))


brasil@data %>% 
  left_join(nep_ufs %>% 
              filter(ANO_ELEICAO == 2014), by = c("NM_ESTADO" = "DESCRICAO_UE"))
# 4. Gráficos -------------------------------------------------------------

brasil <- readOGR("[SP]Brasil", "Brasil", encoding = "UTF-8")

plot(brasil)

brasil_pl <- fortify(brasil)

brasil_pl %>% 
  count(id) %>% 
  print(n = Inf)

brasil_df <- brasil@data

brasil_df$id <- as.character(0:26) 

brasil_df <- brasil_df %>% 
  left_join(nep_ufs %>% filter(ANO_ELEICAO == 2014), by = c("UF" = "SIGLA_UF"))

brasil_pl <- brasil_pl %>% 
  left_join(brasil_df)

brasil_pl %>% 
  ggplot(mapping = aes(x = long, y = lat, gruop = group, fill = NEP)) +
  geom_polygon(color = "white") +
  coord_map() +
  theme_map()
