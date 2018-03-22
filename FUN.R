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

get_params <- function(params, colunas){
  
  for(coluna in colunas){
    params <- append(params, c("selected_columns[]" = coluna))
  }
  
  return(params)
}
