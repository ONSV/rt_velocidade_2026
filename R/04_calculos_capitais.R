filtra_capitais <- function(ano){
  
  nome_df <- paste0("municipios_", ano, "_fixo")
  
  capitais <- data.frame(municipio = c(
    "rio branco", "maceio", "macapa", "manaus", "salvador", "fortaleza", 
    "brasilia", "vitoria", "goiania", "sao luis", "cuiaba", "campo grande", 
    "belo horizonte", "belem", "joao pessoa", "curitiba", "recife", 
    "teresina", "rio de janeiro", "natal", "porto alegre", "porto velho", 
    "boa vista", "florianopolis", "sao paulo", "aracaju", "palmas"
  ),
  uf = c(
    "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
    "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", 
    "RS", "RO", "RR", "SC", "SP", "SE", "TO"
  ))
  
  
  base <- get(nome_df) %>% 
    semi_join(capitais, by = c("municipio", "uf"))
  
  nome_variavel <- paste0("capitais_", ano, "_fixo")
  assign(nome_variavel, base, envir = .GlobalEnv)
}


join_capitais_dist_vias <- function(df){
  base <- get(df) %>% 
    left_join(dist_vias, by =  "municipio")
  
  assign(df, base, envir = .GlobalEnv)
  
}


calculo_i6_capitais <- function(df){
  base <- get(df) %>% 
    mutate(
      i6 = total_faixas_aprv_rprd/dist_vias*100) %>% 
    relocate(i6, .before = dist_vias) %>% 
    relocate(dist_vias, .before = i1) 
    
  
  assign(df, base, envir = .GlobalEnv)
}
