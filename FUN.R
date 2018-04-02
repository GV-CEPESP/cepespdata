get_params <- function(params, colunas){
  
  for(coluna in colunas){
    params <- append(params, c("selected_columns[]" = coluna))
  }
  
  return(params)
}

get_candidatos <- function(ano, cargo, colunas){
  url_base <- "http://cepesp.io/api/consulta/candidatos"
  
  params <- lst(
    `cargo` = cargo,
    `ano`   = ano
  )
  
  params <- get_params(params, colunas)
  
  GET(url_base, query = params) %>% 
    content(type = "text/csv")
}

get_votos <- function(ano, cargo, colunas, agre_reg, agre_pol){
  url_base <- "http://cepesp.io/api/consulta/tse"
  
  params <- lst(
    `cargo`              = cargo,
    `ano`                = ano,
    `agregacao_regional` = agre_reg,
    `agregacao_politica` = agre_pol,
    `brancos`            = 1,
    `nulos`              = 1
  )
  
  params <- get_params(params, colunas)
  
  data <- GET(url_base, query = params) %>% 
    content(type = "text/csv")
}

get_votos_only <- function(ano, cargo, agre_reg, colunas){
  url_base <- "http://cepesp.io/api/consulta/votos"
  
  params <- lst(
    `cargo` = cargo,
    `ano`   = ano,
    `agregacao_regional` = agre_reg
  )
  
  params <- get_params(params, colunas)
  
  GET(url_base, query = params) %>% 
    content(type = "text/csv")
}