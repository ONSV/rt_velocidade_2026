
join_municipios_mortes <- function(){
  municipios_2024_fixo %>% 
    select(uf, municipio, frota, i1, i2, i3, i4) %>% 
    left_join(rtdeaths, 
              by = c("uf", "municipio")) %>% 
    mutate(n_mortes = replace_na(n_mortes, 0),
           n_mortes_10mil_veiculos = n_mortes/frota*10000)
}


calcular_correlacao <- function(){
  teste <- cor.test(municipios_mortes_2024$n_mortes_10mil_veiculos, municipios_mortes_2024$i1, method = "spearman")
  
  p_valor <- teste$p.value
  rho <- teste$estimate
  
  return(data.frame(p_valor = p_valor, rho = rho))
}



calcular_correlacao_uf <- function(uf){
  base <- municipios_mortes_2024 %>% 
    filter(uf == {{uf}})
  
  n_linhas <- nrow(base)
  
  if (n_linhas > 3){
    teste <- cor.test(base$n_mortes_10mil_veiculos, base$i3, method = "spearman")
    
    p_valor <- teste$p.value
    rho <- teste$estimate
  } else {
    p_valor <- 0
    rho <-  0
  }
  return(data.frame(uf = {{uf}},
                    p_valor = p_valor,
                    rho = rho))
}
