get_candidatos <- function(cargo, ano, colunas = NULL){
  url_base <- "http://cepesp.io/api/consulta/candidatos"
  
  params <- lst(
    `cargo` = cargo,
    `ano`   = ano
  )
  
  for(coluna in colunas){
    params <- append(params, c("selected_columns[]" = coluna))
  }
  
  if(length(ano) > 1 | length(ano) > 1){
    
  }
  
  GET(url_base, body = params) %>% 
    content(type = "text/csv")
}
