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


# 2. Consistência ---------------------------------------------------------

candidatos_df %>% 
  count(DES_SITUACAO_CANDIDATURA) %>% 
  arrange(n)

candidatos_df %>% 
  count(DESCRICAO_SEXO)

candidatos_df$DESCRICAO_SEXO[candidatos_df$DESCRICAO_SEXO == "NÃO INFORMADO"] <- NA

candidatos_df %>% 
  count(DESCRICAO_GRAU_INSTRUCAO)

candidatos_df %>% 
  count(DESCRICAO_COR_RACA)

candidatos_df %>% 
  ggplot(mapping = aes(ANO_ELEICAO, fill = DESCRICAO_COR_RACA)) +
  geom_bar()

candidatos_df %>% 
  count(DESPESA_MAX_CAMPANHA)

candidatos_df <- candidatos_df %>% 
  mutate(DESPESA_MAX_CAMPANHA = ifelse(DESPESA_MAX_CAMPANHA == -1, NA, DESPESA_MAX_CAMPANHA))

candidatos_df %>% 
  ggplot(mapping = aes(x = DESPESA_MAX_CAMPANHA)) +
  geom_histogram()

candidatos_df %>% 
  count(DESPESA_MAX_CAMPANHA) %>% 
  arrange(desc(DESPESA_MAX_CAMPANHA))

# 3. Gráficos -------------------------------------------------------------
candidatos_df %>% 
  filter(!is.na(DESCRICAO_SEXO)) %>% 
  ggplot(mapping = aes(x = ANO_ELEICAO, fill = DESCRICAO_SEXO)) +
  geom_bar(position = "fill") +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(breaks = anos) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(labels = c("Mulheres", "Homens")) +
  labs(title = "Proporção de Mulheres Candidatas",
       fill = "Sexo",
       x = "Ano",
       y = "Proporção")
  
for(ano in anos){
  candidatos_df %>% 
    filter(ANO_ELEICAO == ano) %>% 
    group_by(SIGLA_PARTIDO, ANO_ELEICAO) %>% 
    summarise(n   = n(),
              fem = sum(DESCRICAO_SEXO == "FEMININO", na.rm = T),
              mas = sum(DESCRICAO_SEXO == "MASCULINO", na.rm = T)) %>% 
    mutate(prop_m = fem/n) %>%
    gather(fem:mas, key = "gen", value = "quanti") %>% 
    ggplot(mapping = aes(x = reorder(SIGLA_PARTIDO, prop_m), y = quanti, fill = gen)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_hline(yintercept = 0.5) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_fill_discrete(labels = c("Mulheres", "Homens")) +
    labs(title = str_c("Proporção de Mulheres por Partido: ", ano),
         fill = "Sexo",
         x = "Partido",
         y = "Proporção")
  
  ggsave(str_c("diff_gen_", ano,".png"), width = 8.0, height = 5.0)
}

candidatos_df %>% 
  count(DESCRICAO_COR_RACA)

candidatos_df %>% 
  filter(ANO_ELEICAO == 2014) %>% 
  group_by(SIGLA_PARTIDO) %>% 
  summarise(n = n(),
            amarela  = sum(DESCRICAO_COR_RACA == "AMARELA"),
            branca   = sum(DESCRICAO_COR_RACA == "BRANCA"),
            indigena = sum(DESCRICAO_COR_RACA == "INDÍGENA"),
            parda    = sum(DESCRICAO_COR_RACA == "PARDA"),
            preta    = sum(DESCRICAO_COR_RACA == "PRETA"),
            ppi      = sum(DESCRICAO_COR_RACA %in% c("INDÍGENA", "PARDA", "PRETA"))) %>% 
  mutate(prop_ppi = ppi / n) %>% 
  gather(amarela:preta, key = "DESCRICAO_COR_RACA", value = "quanti") %>% 
  ggplot(mapping = aes(reorder(SIGLA_PARTIDO, prop_ppi), quanti, fill = DESCRICAO_COR_RACA)) +
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
