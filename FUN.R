get_params <- function(params, colunas){
  
  for(coluna in colunas){
    params <- append(params, c("selected_columns[]" = coluna))
  }
  
  return(params)
}

get_candidatos <- function(cargo, ano, colunas){
  url_base <- "http://cepesp.io/api/consulta/candidatos"
  
  params <- lst(
    `cargo` = cargo,
    `ano`   = ano
  )
  
  params <- get_params(params, colunas)
  
  GET(url_base, query = params) %>% 
    content(type = "text/csv")
}

get_votos <- function(cargo, ano, colunas, agre_reg, agre_pol){
  url_base <- "http://cepesp.io/api/consulta/tse"
  
  params <- lst(
    `cargo`              = cargo,
    `ano`                = ano,
    `agregacao_regional` = agre_reg,
    `agregacao_politica` = agre_pol
  )
  
  params <- get_params(params, colunas)
  
  GET(url_base, query = params) %>% 
    content(type = "text/csv")
}