library(tidyverse)

base_url <- "http://agencia.tse.jus.br/estatistica/sead/odsele/consulta_vagas/consulta_vagas_ANO.zip"

base_dest <- "NOME.zip"

dir.create("~/cepespdata/temp")

get_vagas <- function(ano){
  uso_url <- str_replace(base_url, "ANO", str_c(ano))
  
  uso_dest <- str_replace(base_dest, "NOME", str_c("~/cepespdata/temp/vagas_", ano))
  
  download.file(uso_url, uso_dest)
}

walk(seq(1998, 2014, by = 4), get_vagas)

zip_files <- list.files(path = "~/cepespdata/temp/", pattern = "vagas_\\d{4}.zip")

walk(zip_files, ~unzip(zipfile = str_c("~/cepespdata/temp/", .), exdir = "~/cepespdata/temp"))

vagas_files <- str_c("/temp/", list.files("/temp", pattern = "consulta_vagas_\\d{4}_.{2}.txt"))

vagas_ls <- map(vagas_files, ~read_delim(file = .,delim = ";", col_names = F, locale = locale(encoding = "ISO-8859-1")))

vagas_df <- bind_rows(vagas_ls)

vagas_df <- vagas_df %>% 
  rename(data    = X1,
         hora    = X2,
         ANO_ELEICAO     = X3,
         eleicao = X4,
         SIGLA_UF      = X5,
         uf_2    = X6,
         nome_uf = X7,
         x8      = X8,
         cargo   = X9,
         vagas   = X10)

vagas_df %>% 
  count(eleicao)

vagas_df %>% 
  count(cargo)

vagas_df <- vagas_df %>% 
  mutate(cargo = str_to_upper(cargo))

vagas_df %>% 
  count(cargo)

vagas_df %>% 
  count(ANO_ELEICAO)

vagas_df %>% 
  count(SIGLA_UF) %>% 
  count(SIGLA_UF) %>% 
  filter(nn > 1)

write_csv(vagas_df, "magnitudes.csv")

rm(vagas_df, vagas_ls)

