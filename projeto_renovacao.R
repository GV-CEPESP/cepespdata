rm(list = ls())

library(tidyverse)
library(httr)
source("FUN.R")

# 1. Download Banco de Dados ----------------------------------------------

cargos = 6

anos   = seq(1998, 2014, by = 4)

colunas = c("ANO_ELEICAO",
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

get_candidatos(cargos, anos)
