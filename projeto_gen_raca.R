rm(list = ls())

library(tidyverse)
library(httr)
library(genderBR)

# 1. Carregando Bancos ----------------------------------------------------

anos <- c(seq(1998, 2014, by = 4))

args <- expand.grid()

colunas <- c("ANO_ELEICAO",
             "NUM_TURNO",
             "DESCRICAO_ELEICAO",
             "SIGLA_UF",
             "NOME_CANDIDATO",
             "NUM_TITULO_ELEITORAL_CANDIDATO",
             "DES_SITUACAO_CANDIDATURA",
             "NUMERO_PARTIDO",
             "SIGLA_PARTIDO",
             "CODIGO_LEGENDA",
             "SIGLA_LEGENDA",
             "COMPOSICAO_LEGENDA",
             "NOME_COLIGACAO",
             "CODIGO_SEXO",
             "DESCRICAO_SEXO",
             "CODIGO_ESTADO_CIVIL",
             "DESCRICAO_ESTADO_CIVIL",
             "COD_GRAU_INSTRUCAO",
             "DESCRICAO_GRAU_INSTRUCAO",
             "CODIGO_COR_RACA",
             "DESCRICAO_COR_RACA",
             "DESPESA_MAX_CAMPANHA")

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

for(i in seq_along(candidatos_ls)){
  candidatos_ls[[i]] <- candidatos_ls[[i]] %>% 
    mutate(NUM_TITULO_ELEITORAL_CANDIDATO = parse_character(NUM_TITULO_ELEITORAL_CANDIDATO))
}

candidatos_df <- bind_rows(candidatos_ls)

get_voto <- function(ano){
  url_base <- "http://cepesp.io/api/consulta/tse"
  
  params <- list(
    `cargo`              = 6,
    `ano`                = ano,
    `agregacao_regional` = 2,
    `agregacao_politica` = 2,
    `selected_columns[]`   = "ANO_ELEICAO",
    `selected_columns[]`   = "NUM_TITULO_ELEITORAL_CANDIDATO",
    `selected_columns[]`   = "UF",
    `selected_columns[]`   = "QTDE_VOTOS")
  
  data_frame <- GET(url_base, query = params) %>% 
    content(type = "text/csv")
}

votos_ls <- lst()

for(ano in anos){
  i = length(votos_ls) + 1
  
  votos_ls[[i]] <- get_voto(ano)
}

votos_df <- bind_rows(votos_ls)

# 2. Criando Variáveis ----------------------------------------------------

#2.1. Banco de Votos
votos_df %>% 
  count(NUM_TITULO_ELEITORAL_CANDIDATO) %>% 
  arrange(desc(n))

votos_df <- votos_df %>%
  mutate(NUM_TITULO_ELEITORAL_CANDIDATO = ifelse(NUM_TITULO_ELEITORAL_CANDIDATO %in% c("#NULO#", "#NI#"),
                                                 NA, NUM_TITULO_ELEITORAL_CANDIDATO))

#2.2. Merge

candidatos_df <- candidatos_df %>% 
  left_join(votos_df, by = c("ANO_ELEICAO", "SIGLA_UF" = "UF", "NUM_TITULO_ELEITORAL_CANDIDATO"))


candidatos_df %>% 
  count(DES_SITUACAO_CANDIDATURA) %>% 
  arrange(n)

candidatos_df %>% 
  count(DESCRICAO_SEXO)

candidatos_df <- candidatos_df %>% 
  mutate(sexo = fct_recode(DESCRICAO_SEXO,
                           "Mulheres" = "FEMININO",
                           "Homens"   = "MASCULINO",
                           NULL       = "NÃO INFORMADO"))

candidatos_df %>% 
  count(sexo)

candidatos_df %>% 
  count(DESCRICAO_COR_RACA)

candidatos_df <- candidatos_df %>% 
  mutate(raca = fct_recode(DESCRICAO_COR_RACA,
                           "Branca"  = "BRANCA",
                           "Amarela" = "AMARELA",
                           "Indígena"= "INDÍGENA",
                           "Parda"   = "PARDA",
                           "Preta"   = "PRETA",
                           NULL      = "#NE#"))

candidatos_df %>% 
  count(raca)

candidatos_df %>% 
  count(DESCRICAO_GRAU_INSTRUCAO)

candidatos_df <- candidatos_df %>%
  mutate(escolaridade = fct_collapse(DESCRICAO_GRAU_INSTRUCAO,
                                     "Não Superior" = c("1º GRAU COMPLETO",
                                                        "1º GRAU INCOMPLETO", 
                                                        "2º GRAU COMPLETO",
                                                        "2º GRAU INCOMPLETO",
                                                        "ANALFABETO",
                                                        "ENSINO FUNDAMENTAL COMPLETO",
                                                        "ENSINO FUNDAMENTAL INCOMPLETO",
                                                        "ENSINO MÉDIO COMPLETO",
                                                        "ENSINO MÉDIO INCOMPLETO",  
                                                        "FUNDAMENTAL COMPLETO",    
                                                        "FUNDAMENTAL INCOMPLETO",     
                                                        "LÊ E ESCREVE",
                                                        "MÉDIO COMPLETO",            
                                                        "MÉDIO INCOMPLETO",          
                                                        "NÃO INFORMADO"),
                                     "Superior (Incompleto + Completo)" = c("SUPERIOR COMPLETO",
                                                                            "SUPERIOR INCOMPLETO")))

candidatos_df %>% 
  count(escolaridade)

candidatos_df %>% 
  count(DESPESA_MAX_CAMPANHA)

candidatos_df %>% 
  count(DESPESA_MAX_CAMPANHA) %>% 
  arrange(desc(DESPESA_MAX_CAMPANHA))

candidatos_df %>% 
  ggplot(mapping = aes(x = DESPESA_MAX_CAMPANHA)) +
  geom_histogram()

# 3. Gráficos -------------------------------------------------------------

candidatos_df %>% 
  filter(!is.na(sexo)) %>% 
  ggplot(mapping = aes(x = ANO_ELEICAO, fill = sexo)) +
  geom_bar(position = "fill") +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(breaks = anos) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Candidatos em função de sexo",
       fill = "Sexo",
       x = "Ano",
       y = "Proporção")
  
for(ano in anos){
  candidatos_df %>% 
    filter(ANO_ELEICAO == ano) %>% 
    group_by(SIGLA_PARTIDO, ANO_ELEICAO) %>% 
    summarise(n   = n(),
              fem = sum(sexo == "Mulheres", na.rm = T),
              mas = sum(sexo == "Homens", na.rm = T)) %>% 
    mutate(prop_m = fem/n) %>%
    gather(fem:mas, key = "gen", value = "quanti") %>% 
    ggplot(mapping = aes(x = reorder(SIGLA_PARTIDO, prop_m), y = quanti, fill = gen)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_hline(yintercept = 0.5) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_fill_discrete(labels = c("Mulheres", "Homens")) +
    labs(title = str_c("Proporção de Homens e Mulheres por Partido: ", ano),
         fill = "Sexo",
         x = "Partido",
         y = "Proporção")
  
  ggsave(str_c("diff_gen_", ano,".png"), width = 8.0, height = 5.0)
}

candidatos_df %>% 
  filter(ANO_ELEICAO == 2014) %>% 
  group_by(SIGLA_PARTIDO) %>% 
  summarise(n = n(),
            amarela  = sum(raca == "Amarela"),
            branca   = sum(raca == "Branca"),
            indigena = sum(raca == "Indígena"),
            parda    = sum(raca == "Parda"),
            preta    = sum(raca == "Preta"),
            ppi      = sum(raca %in% c("Indígena", "Parda", "Preta"))) %>% 
  mutate(prop_ppi = ppi / n) %>% 
  gather(amarela:preta, key = "raca", value = "quanti") %>% 
  ggplot(mapping = aes(reorder(SIGLA_PARTIDO, prop_ppi), quanti, fill = raca)) +
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() +
  scale_fill_discrete(labels = c("Amarela", "Branca", "Indígena", "Parda", "Preta")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Proporção de Raças",
       x = "Partido",
       y = "Proporção",
       fill = "Cor/Raça")
  
ggsave("diff_rac.png", width = 8.0, height = 5.0)

candidatos_df %>% 
  filter(!is.na(escolaridade)) %>% 
  ggplot(mapping = aes(x = ANO_ELEICAO, fill =  escolaridade)) +
  geom_bar(position = "fill")

candidatos_df %>% 
  filter(DES_SITUACAO_CANDIDATURA == "DEFERIDO") %>% 
  filter(ANO_ELEICAO == 2014) %>% 
  group_by(SIGLA_PARTIDO) %>% 
  summarise(ppi   = sum(raca %in% c("Indígena", "Preta", "Parda")),
            n     = n(),
            votos = sum(QTDE_VOTOS, na.rm = T)) %>% 
  mutate(prop_ppi = ppi / n) %>% 
  ggplot(mapping = aes(x = prop_ppi, y = votos)) +
  geom_point()

candidatos_df %>% 
  filter(DES_SITUACAO_CANDIDATURA == "DEFERIDO") %>% 
  filter(ANO_ELEICAO == 2014) %>% 
  group_by(SIGLA_PARTIDO) %>% 
  summarise(mulh = sum(sexo == "Mulheres"),
            n    = n(),
            votos= sum(QTDE_VOTOS, na.rm = T)) %>% 
  mutate(prop_m = mulh / n) %>% 
  arrange(desc(prop_m)) %>% 
  ggplot(mapping = aes(x = prop_m, y = votos)) +
  geom_point()

candidatos_df %>% 
  filter(DES_SITUACAO_CANDIDATURA == "DEFERIDO") %>% 
  filter(ANO_ELEICAO == 2014) %>% 
  group_by(SIGLA_PARTIDO) %>% 
  summarise(mulh = sum(escolaridade == "Não Superior"),
            n    = n(),
            votos= sum(QTDE_VOTOS, na.rm = T)) %>% 
  mutate(prop_m = mulh / n) %>% 
  arrange(desc(prop_m)) %>% 
  ggplot(mapping = aes(x = prop_m, y = votos)) +
  geom_point()

