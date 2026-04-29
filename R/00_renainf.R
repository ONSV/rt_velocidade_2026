importar_infracoes <- function(link, mes, ano){
  temp1 <- tempfile(fileext = ".csv")
  
  try(curl_download(
    url = link,
    destfile = temp1,
    quiet = FALSE,
    handle = new_handle(timeout = 3600) 
  ))
  
  if (ano == 2023){
    infracoes <- readr::read_csv2(temp1, locale = locale(encoding = "UTF-16")) %>% 
    mutate(mes = {{mes}}) %>% 
    slice(-1)
  } else {
    infracoes <- readr::read_csv(temp1, locale = locale(encoding = "UTF-16")) %>% 
    mutate(mes = {{mes}}) %>% 
    slice(-1)
    }
  
  nome_arquivo <- paste0("data/infracoes_", ano, "_", mes, ".rds")
  
  saveRDS(infracoes, file = nome_arquivo)
}





