importar_frota <- function(ano){
  nome_arquivo <- paste0("data-raw/FrotapormunicipioetipoDezembro", ano, ".xlsx")
  base <- read_excel(nome_arquivo, skip = 3)
  
  nome_variavel <- paste0("frota_", ano)
  assign(nome_variavel, base, .GlobalEnv)
}
