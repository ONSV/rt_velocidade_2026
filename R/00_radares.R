
importar_dados <- function(ano, mes){
  
  nome_variavel <- paste0("radares_", ano, "_", mes)
  nome_arquivo <- paste0("data/", nome_variavel, ".rda")
  
  
  resultado <- tryCatch({
    radares <- read_excel(paste0("data-raw/Dados Radares Puerta - ", ano, ".xlsx"), sheet = mes)
    assign(nome_variavel, radares)
    
    save(list = nome_variavel, file = nome_arquivo)
    message("Sucesso: ", nome_variavel)
    
  }, error = function(e) {
    message(paste("Não encontrado:", nome_variavel))
  })
  
}



