rm(list = ls())

library(tidyverse)
library(httr)

# 1. Carregando Banco - Candidatos ----------------------------------------

anos <- c(seq(1998, 2014, by = 4))

cargos <- c(1,3,5,6,7,8,11,13)

args <- expand.grid()

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

rm(legendas_ls, candidatos_ls)

legendas_df <- read_rds("legendas.rds")

legendas_df <- legendas_df %>% 
  filter(CODIGO_CARGO == 6)

candidatos_df <- read_rds("candidatos.csv")

candidatos_df <- candidatos_df %>% 
  filter(cargo == 6)

# 3. Consistência dos Dados -----------------------------------------------

#' Antes de criar um banco de dados das coligações vou criar um banco de dados dos partidos.
#' Realizei isso em dois passos: 1) Criei um banco de partidos a partir do banco de legendas;
#' 2) criei um banco de partidos a partir do banco de candidatos. Como nem todos os partidos lançam
#' candidatos, utilizar o banco legendas é mais interessante. Porém, esse banco possui alguns problemas de 
#' observações repetidas. O TSE não informa qual a legenda que de fato concorreu nas eleições. 
#' Utilizei o banco de partidos provenientes do banco de candidatos para arrumar essa informação.

partidos1_df <- legendas_df %>% 
  select(SIGLA_PARTIDO, NOME_COLIGACAO, SIGLA_UF, ANO_ELEICAO, TIPO_LEGENDA) %>% 
  arrange(ANO_ELEICAO, SIGLA_UF, NOME_COLIGACAO) %>% 
  distinct()

# Tentando entender o que a categoria #NE# correspode
partidos1_df %>% 
  count(NOME_COLIGACAO, TIPO_LEGENDA) %>% 
  arrange(desc(n))

partidos1_df %>% 
  filter(NOME_COLIGACAO %in% c("#NE#", "#NULO#")) %>% 
  count(TIPO_LEGENDA)


#' Como essa categoria só possui TIPO_LEGENDA == PARTIDO_ISOLADO,
#' vou transformar o #NE# no nome do partido para facilitar a criação
#' de um banco de coligações.

partidos1_df <- partidos1_df %>% 
  mutate(NOME_COLIGACAO = case_when(NOME_COLIGACAO %in% c("#NE#", "#NULO#", "PARTIDO ISOLADO") ~ SIGLA_PARTIDO,
                                    T                                                ~ NOME_COLIGACAO))

#' Agora, eu explorei o banco de candidatos. Esse banco é essencial para
#' a criação do banco coligações já que me permite comparar os resultados.
#' Como eu só me interesso pelos candidatos que de fato concorreram, filtrei 
#' o banco para os candidados DEFERIDOS.

partidos2_df <- candidatos_df %>% 
  filter(DES_SITUACAO_CANDIDATURA == "DEFERIDO") %>% 
  select(SIGLA_PARTIDO, NOME_COLIGACAO, SIGLA_UF, ANO_ELEICAO) %>% 
  arrange(ANO_ELEICAO, SIGLA_UF, SIGLA_PARTIDO) %>% 
  distinct()

partidos2_df %>% 
  count(NOME_COLIGACAO) %>% 
  arrange(desc(n))

#' Novamente, temos NOME_COLIGACAO == #NE#. 
#' Vou verificar no banco de legendas para ver como esses partidos estão classificados.

partidos2_NE <- partidos2_df %>% 
  filter(NOME_COLIGACAO %in% c("#NE#", "#NULO#", "PARTIDO ISOLADO", NA)) %>% 
  select(-NOME_COLIGACAO)

partidos1_df %>% 
  inner_join(partidos2_NE) 

partidos1_df %>% 
  inner_join(partidos2_NE) %>%
  count(TIPO_LEGENDA)

partidos1_df %>% 
  inner_join(partidos2_NE) %>%
  filter(TIPO_LEGENDA == "COLIGACAO")

rm(partidos2_NE)

#' A maioria se classifica como PARTIDO_ISOLADO, mas 6 casos constam como coligação.
#' A fim de verificar qual a situação desses partidos nas eleições de 2010, utilizei o 
#' <http://eleicoes.terra.com.br/apuracao/2010/1turno/> e pesquisei um por um. Aparentemente,
#' todos as 6 observações são de concorrência individual.
#' 
#' Tendo em vista isso, podemos assumir com certa segurança que os #NE# do NOME_COLIGACAO no
#' banco de candidatos são de partidos que concorreram isoladamente. 

partidos2_df <- partidos2_df %>% 
  mutate(NOME_COLIGACAO = case_when(is.na(NOME_COLIGACAO)                                      ~ SIGLA_PARTIDO,
                                    NOME_COLIGACAO %in% c("#NE#", "#NULO#", "PARTIDO ISOLADO") ~ SIGLA_PARTIDO,
                                    T                                                          ~ NOME_COLIGACAO),
         TIPO_LEGENDA   = case_when(SIGLA_PARTIDO == NOME_COLIGACAO ~ "PARTIDO_ISOLADO",
                                    T                               ~ "COLIGACAO"))

partidos2_df %>% 
  count(NOME_COLIGACAO) %>% 
  arrange(desc(n))
#' Verifiquei se há repetições de partidos nos dois bancos.

partidos1_df %>% 
  count(ANO_ELEICAO, SIGLA_UF, SIGLA_PARTIDO) %>% 
  filter(n > 1)

#' Temos bastantes observações que se repetem no banco proveniente das legendas

partidos2_df %>% 
  count(ANO_ELEICAO, SIGLA_UF, SIGLA_PARTIDO) %>% 
  filter(n > 1)

#' No banco proveniente dos candidatos temos apenas uma repetição. 
#' Se olharmos especificamente vemos que provavelmente houve um erro
#' na construção de banco de tal maneira que o NOME_COLIGACAO era um
#' missing.

partidos2_df %>% 
  filter(ANO_ELEICAO   == 2010,
         SIGLA_UF      == "MA",
         SIGLA_PARTIDO == "PSL")

candidatos_df %>% 
  filter(DES_SITUACAO_CANDIDATURA == "DEFERIDO") %>% 
  select(ANO_ELEICAO, SIGLA_UF, SIGLA_PARTIDO, NOME_COLIGACAO) %>% 
  filter(ANO_ELEICAO   == 2010,
         SIGLA_UF      == "MA",
         SIGLA_PARTIDO == "PSL")

partidos2_df <- partidos2_df %>% #Arrumando uma observação específica
  mutate(NOME_COLIGACAO = case_when(ANO_ELEICAO == 2010 & SIGLA_UF == "MA" & SIGLA_PARTIDO == "PSL" ~ "O MARANHÃO NÃO PODE PARAR - F2",
                                    T                                                               ~ NOME_COLIGACAO),
         TIPO_LEGENDA   = case_when(ANO_ELEICAO == 2010 & SIGLA_UF == "MA" & SIGLA_PARTIDO == "PSL" ~ "COLIGACAO",
                                    T                                                               ~ TIPO_LEGENDA)) %>% 
  distinct()

partidos2_df %>% 
  count(ANO_ELEICAO, SIGLA_UF, SIGLA_PARTIDO) %>% 
  filter(n > 1)

#' Agora, o banco partidos2_df não possui repetições. Resta arrumar o partidos1_df

#' Primeiro, vou comparar o partidos1_df e o partidos2_df. Vou dar preferência para a classificação
#' proveniente do partidos2_df já que, a princípio, ela foi a utilizada na eleição. 
#' 
#' Quando o NOME_COLIGACAO é NA no partidos2_df, significa que o partido não apresentou candidatos.
#' Logo prevalece a classificação do partidos1_df. Caso sejam diferentes, prevalece a classificação do 
#' partidos2_df. Por fim, se for igual, tanto faz e repete a classificação do partidos1_df.

partidos_df <- partidos1_df %>% 
  left_join(partidos2_df, by =  c("SIGLA_PARTIDO", "SIGLA_UF", "ANO_ELEICAO")) %>% 
  mutate(TIPO_LEGENDA   = case_when(is.na(TIPO_LEGENDA.y)                ~ TIPO_LEGENDA.x,
                                    TIPO_LEGENDA.x != TIPO_LEGENDA.y     ~ TIPO_LEGENDA.y,
                                    T                                    ~ TIPO_LEGENDA.x),
         NOME_LEGENDA = case_when(is.na(NOME_COLIGACAO.y)              ~ NOME_COLIGACAO.x,
                                  NOME_COLIGACAO.x != NOME_COLIGACAO.y ~ NOME_COLIGACAO.y,
                                  T                                    ~ NOME_COLIGACAO.x)) %>% 
  select(-NOME_COLIGACAO.x, -NOME_COLIGACAO.y, -TIPO_LEGENDA.x, -TIPO_LEGENDA.y) %>% 
  distinct()

partidos_df %>% 
  group_by(SIGLA_UF, ANO_ELEICAO) %>% 
  count(NOME_LEGENDA)

#' Partidos que com classificação repetida, mas que não apresentaram candidatos nas coligações.

partidos_prob <- partidos_df %>% 
  count(SIGLA_PARTIDO, SIGLA_UF,ANO_ELEICAO) %>% 
  filter(n > 1) %>% 
  select(-n)

partidos_class <- partidos_df %>% 
  inner_join(partidos_prob)

partidos_class$teste <- NA

#' A solução foi verificiar quais NOME_COLIGACAO estão presentes na SIGLA_UF ANO_ELEICAO do
#' partidos2_df. Prevalece o NOME_COLIGACAO que estiver no partidos2_df.

for(i in seq_along(partidos_class$teste)){
  legendas <- partidos2_df$NOME_COLIGACAO[partidos2_df$ANO_ELEICAO == partidos_class$ANO_ELEICAO[i] & partidos2_df$SIGLA_UF == partidos_class$SIGLA_UF[i]]

  partidos_class$teste[i] <- partidos_class$NOME_LEGENDA[i] %in% legendas
}

partidos_class %>% 
  filter(teste)

#' Retirei as observações que não bateram do partidos_df

partidos_df <- partidos_df %>% 
  anti_join(partidos_class %>% 
               filter(!teste))

#' Verificação final para ver se há repetição de partidos em partidos_df
partidos_df %>% 
  count(SIGLA_PARTIDO, SIGLA_UF, ANO_ELEICAO) %>% 
  filter(n > 1)

# 4. Criando banco de Coligações ------------------------------------------

coligacoes_df <- partidos_df %>% 
  select(NOME_LEGENDA, SIGLA_UF, ANO_ELEICAO, TIPO_LEGENDA) %>% 
  distinct()

coligacoes_df %>% 
  count(NOME_LEGENDA, SIGLA_UF, ANO_ELEICAO) %>% 
  filter(n > 1)

# 5. Candidatos disponíveis por distrito ----------------------------------

# Banco com as magnitudes
# source("magnitudes.R")

mag_df <- read_csv("magnitudes.csv")

mag_df <- mag_df %>% 
  filter(cargo == "DEPUTADO FEDERAL")

#' Criação do quantidade possível de candidatos por coligação
coligacoes_df <- coligacoes_df %>% 
  left_join(mag_df) %>% 
  mutate(CAND_POSSIVEIS = case_when(TIPO_LEGENDA == "COLIGACAO"       & vagas >= 20 ~ vagas * 2,
                                    TIPO_LEGENDA == "COLIGACAO"       & vagas <  20 ~ vagas * 3,
                                    TIPO_LEGENDA == "PARTIDO_ISOLADO" & vagas >= 20 ~ vagas * 1.5,
                                    TIPO_LEGENDA == "PARTIDO_ISOLADO" & vagas <  20 ~ vagas * 2))

#" Banco com a quantidade de candidatos apresentados por coligação
quanti_candi <- candidatos_df %>% 
  filter(DES_SITUACAO_CANDIDATURA == "DEFERIDO") %>% 
  mutate(NOME_LEGENDA = case_when(is.na(NOME_COLIGACAO)                                           ~ SIGLA_PARTIDO,
                                  NOME_COLIGACAO == "#NE#"                                        ~ SIGLA_PARTIDO,
                                  NOME_COLIGACAO == "PARTIDO ISOLADO"                             ~ SIGLA_PARTIDO,
                                  NOME_COLIGACAO == "#NULO#"                                      ~ SIGLA_PARTIDO,
                                  T                                                               ~ NOME_COLIGACAO)) %>% 
  count(NOME_LEGENDA, SIGLA_UF, ANO_ELEICAO) %>% 
  mutate(NOME_LEGENDA = case_when(ANO_ELEICAO == 2010 & SIGLA_UF == "MA" & NOME_LEGENDA == "PSL" ~ "O MARANHÃO NÃO PODE PARAR - F2",
         T                                                                             ~ NOME_LEGENDA)) %>% 
  rename(CAND_APRES = n)

quanti_candi %>% 
  count(NOME_LEGENDA) %>% 
  arrange(desc(n))

#' Adicionando quantidade de candidatos apresentados por coligação
coligacoes_df <- coligacoes_df %>% 
  left_join(quanti_candi) %>% 
  mutate(CAND_APRES = ifelse(is.na(CAND_APRES), 0, CAND_APRES))

#' Verificando se a quantidade de coligações com candidatos apresentados é igual
#' nos dois bancos.
#'  
sum(coligacoes_df$CAND_APRES > 0) 

sum(quanti_candi$CAND_APRES > 0)

# 6. Gráficos -------------------------------------------------------------

coligacoes_df %>% 
  group_by(ANO_ELEICAO) %>% 
  summarise(apres = sum(CAND_APRES)) %>% 
  left_join(mag_df %>% 
              group_by(ANO_ELEICAO) %>% 
              summarise(vagas = sum(vagas))) %>% 
  mutate(can_vaga = apres/vagas) %>% 
  ggplot(mapping = aes(x = ANO_ELEICAO, y = can_vaga)) +
  geom_line(color = "tomato3", size = 2) +
  geom_point(size = 3) +
  theme_minimal() +
  scale_x_continuous(breaks = c(1998, 2002, 2006, 2010, 2014))

coligacoes_df %>% 
  filter(ANO_ELEICAO == 2014) %>% 
  filter(SIGLA_UF == "SP") %>% 
  gather(CAND_POSSIVEIS,CAND_APRES, key = "apres", value = "quanti") %>% 
  ggplot(mapping = aes(x = NOME_LEGENDA, y = quanti, color = apres, group = NOME_LEGENDA)) +
  geom_segment(aes(xend = NOME_LEGENDA, y = min(quanti), yend = max(quanti)),
               linetype= "dashed",
               size = 0.1,
               color = "black") +
  geom_line(size = 5, alpha = 0.3, color = "black") +
  geom_point(size = 5) +
  scale_y_continuous(breaks = seq(0, 200, by = 25)) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom")


for(i in seq(0, 12, by = 4)){
  dupla_ano = c(1998, 2002)
  
  dupla_ano = dupla_ano + i
  
  coligacoes_df %>% 
    group_by(ANO_ELEICAO, SIGLA_UF) %>% 
    summarise(cand = sum(CAND_APRES)) %>% 
    left_join(mag_df) %>%
    group_by(SIGLA_UF) %>%
    mutate(can_vaga = cand / vagas,
           can_vaga1 = lag(can_vaga),
           maior     = case_when(can_vaga > can_vaga1 ~ T,
                                 can_vaga < can_vaga1 ~ F)) %>%
    ungroup() %>% 
    mutate(SIGLA_UF = reorder(SIGLA_UF, can_vaga)) %>% 
    filter(ANO_ELEICAO %in% dupla_ano) %>%
    ggplot() +
    geom_segment(aes(x    = SIGLA_UF,
                     xend = SIGLA_UF,
                     y    = 2,
                     yend = 20),
                 linetype= "dashed", 
                 size    = 0.1) +
    geom_line(aes(group = SIGLA_UF,
                  x = SIGLA_UF,
                  y = can_vaga,
                  color = maior),
              size = 4,
              alpha = 0.4) +
    geom_point(aes(x = SIGLA_UF,
                   y = can_vaga,
                   color = as.factor(ANO_ELEICAO)),
               stat = "identity",
               size = 4) +
    coord_flip() +
    scale_y_continuous(limits = c(2, 20), breaks = seq(2,20, by = 2)) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(str_c("cand_vaga", str_c(dupla_ano, collapse = "-"), ".png"))
}

