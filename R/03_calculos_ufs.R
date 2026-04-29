agrupar_fixos_uf <- function(ano){
  base <- get(paste0("radares_", ano)) %>%  
    filter(tipo_medidor == "FIXO" & ultimo_resultado != "REPROVADO") %>% 
    
    group_by(uf) %>% 
    summarise(qt_radares_federais = sum(tipo_via == "Rodovia Federal", na.rm = TRUE),
              total_faixas_federais = sum(if_else(tipo_via == "Rodovia Federal", qt_faixas, 0), na.rm = TRUE),
              qt_radares_estaduais = sum(tipo_via == "Rodovia Estadual", na.rm = TRUE),
              total_faixas_estaduais = sum(if_else(tipo_via == "Rodovia Estadual", qt_faixas, 0), na.rm = TRUE),
              total_radares_rodovias = qt_radares_federais + qt_radares_estaduais,
              total_faixas_rodovias = total_faixas_federais + total_faixas_estaduais,
              qt_radares_urbanos = sum(tipo_via == "Via Urbana", na.rm = T),
              total_faixas_urbanos = sum(if_else(tipo_via == "Via Urbana", qt_faixas, 0), na.rm = TRUE),
              total_faixas_aprv_rprd = sum(qt_faixas, na.rm = T))
  
  nome_variavel <- paste0("ufs_", ano, "_fixo")
  assign(nome_variavel, base, envir = .GlobalEnv)
}

contar_ultimo_resultado_uf <- function(ano){
  base <- get(paste0("radares_", ano)) %>% 
    group_by(uf) %>% 
    summarise(
      reparados = sum(ultimo_resultado == "REPARADO", na.rm = TRUE),
      aprovados = sum(ultimo_resultado == "APROVADO", na.rm = TRUE),
      total_radares_aprv_rprd = reparados + aprovados,
      reprovados = sum(ultimo_resultado == "REPROVADO", na.rm = TRUE),
      total_radares_geral = aprovados + reparados + reprovados,
      total_faixas_geral = sum(qt_faixas, na.rm = T)
    ) %>% 
    mutate(ano = {{ano}})
  nome_variavel <- paste0("reprovados_ufs_", ano)
  assign(nome_variavel, base, envir = .GlobalEnv)
}

join_ufs_frota <- function(ano){
  ufs <- get(paste0("ufs_", ano, "_fixo"))
  frota <- get(paste0("frota_", ano)) %>% 
    group_by(uf) %>% 
    summarise(frota = sum(frota, na.rm = T))
  
  join <- left_join(ufs, frota, by = c("uf"))
  
  assign(paste0("ufs_", ano, "_fixo"), join, envir = .GlobalEnv)
}

join_contagem_ultimo_resultado_uf <- function(ano){
  nome_variavel <- paste0("ufs_", ano, "_fixo")
  base <- get(nome_variavel) %>% 
    left_join(get(paste0("reprovados_ufs_", ano)), by = "uf")
  
  assign(nome_variavel, base, envir = .GlobalEnv)
}

join_bilhao_km_percorrido <- function(ano){
  nome_variavel <- paste0("ufs_", ano, "_fixo")
  base <- get(nome_variavel) %>% 
    left_join(get(paste0("bilhao_km_percorrido_", ano)), by = "uf")
  
  assign(nome_variavel, base, envir = .GlobalEnv)
}

calculo_br_2023 <- function(){
  br_2023_fixo <<- ufs_2023_fixo %>% 
    summarise(
      qt_radares_federais = sum(qt_radares_federais, na.rm = T),
      total_faixas_federais = sum(total_faixas_federais, na.rm = T),
      qt_radares_estaduais = sum(qt_radares_estaduais, na.rm = T),
      total_faixas_estaduais = sum(total_faixas_estaduais),
      total_radares_rodovias = sum(total_radares_rodovias),
      total_faixas_rodovias = sum(total_faixas_rodovias),
      qt_radares_urbanos = sum(qt_radares_urbanos),
      total_faixas_urbanos = sum(total_faixas_urbanos),
      total_faixas_aprv_rprd = sum(total_faixas_aprv_rprd),
      frota = sum(frota),
      reparados = sum(reparados),
      aprovados = sum(aprovados),
      total_radares_aprv_rprd = sum(total_radares_aprv_rprd),
      reprovados = sum(reprovados),
      total_radares_geral = sum(total_radares_geral),
      total_faixas_geral = sum(total_faixas_geral),
      soma_vl_extensa = sum(soma_vl_extensa),
      bilhao_km_percorrido = sum(bilhao_km_percorrido)
     # n_infracoes = sum(n_infracoes)
    ) %>% 
    mutate(ano = 2023,
           uf = "BR",
           i1 = total_faixas_aprv_rprd/frota*10000,
           i2 = total_radares_geral/frota*10000,
           i3 = total_faixas_urbanos/frota*10000,
           i4 = total_faixas_rodovias/frota*10000,
           i5 = total_faixas_aprv_rprd/total_faixas_geral,
           i6 = total_faixas_federais/soma_vl_extensa*100,
           i7 = total_faixas_aprv_rprd/bilhao_km_percorrido) %>% 
    relocate(uf, .before = qt_radares_federais)
}

calculo_br_2024 <- function(){
  br_2024_fixo <<- ufs_2024_fixo %>% 
    summarise(
      qt_radares_federais = sum(qt_radares_federais, na.rm = T),
      total_faixas_federais = sum(total_faixas_federais, na.rm = T),
      qt_radares_estaduais = sum(qt_radares_estaduais, na.rm = T),
      total_faixas_estaduais = sum(total_faixas_estaduais),
      total_radares_rodovias = sum(total_radares_rodovias),
      total_faixas_rodovias = sum(total_faixas_rodovias),
      qt_radares_urbanos = sum(qt_radares_urbanos),
      total_faixas_urbanos = sum(total_faixas_urbanos),
      total_faixas_aprv_rprd = sum(total_faixas_aprv_rprd),
      frota = sum(frota),
      reparados = sum(reparados),
      aprovados = sum(aprovados),
      total_radares_aprv_rprd = sum(total_radares_aprv_rprd),
      reprovados = sum(reprovados),
      total_radares_geral = sum(total_radares_geral),
      total_faixas_geral = sum(total_faixas_geral),
      soma_vl_extensa = sum(soma_vl_extensa),
      bilhao_km_percorrido = sum(bilhao_km_percorrido)
      # n_infracoes = sum(n_infracoes)
    ) %>% 
    mutate(ano = 2024,
           uf = "BR",
           i1 = total_faixas_aprv_rprd/frota*10000,
           i2 = total_radares_geral/frota*10000,
           i3 = total_faixas_urbanos/frota*10000,
           i4 = total_faixas_rodovias/frota*10000,
           i5 = total_faixas_aprv_rprd/total_faixas_geral,
           i6 = total_faixas_federais/soma_vl_extensa*100,
           i7 = total_faixas_aprv_rprd/bilhao_km_percorrido) %>% 
    relocate(uf, .before = qt_radares_federais)
}

calculo_br_2025 <- function(){
  br_2025_fixo <<- ufs_2025_fixo %>% 
    summarise(
      qt_radares_federais = sum(qt_radares_federais, na.rm = T),
      total_faixas_federais = sum(total_faixas_federais, na.rm = T),
      qt_radares_estaduais = sum(qt_radares_estaduais, na.rm = T),
      total_faixas_estaduais = sum(total_faixas_estaduais),
      total_radares_rodovias = sum(total_radares_rodovias),
      total_faixas_rodovias = sum(total_faixas_rodovias),
      qt_radares_urbanos = sum(qt_radares_urbanos),
      total_faixas_urbanos = sum(total_faixas_urbanos),
      total_faixas_aprv_rprd = sum(total_faixas_aprv_rprd),
      frota = sum(frota),
      reparados = sum(reparados),
      aprovados = sum(aprovados),
      total_radares_aprv_rprd = sum(total_radares_aprv_rprd),
      reprovados = sum(reprovados),
      total_radares_geral = sum(total_radares_geral),
      total_faixas_geral = sum(total_faixas_geral),
      soma_vl_extensa = sum(soma_vl_extensa)
    ) %>% 
    mutate(ano = 2025,
           uf = "BR",
           i1 = total_faixas_aprv_rprd/frota*10000,
           i2 = total_radares_geral/frota*10000,
           i3 = total_faixas_urbanos/frota*10000,
           i4 = total_faixas_rodovias/frota*10000,
           i5 = total_faixas_aprv_rprd/total_faixas_geral,
           i6 = total_faixas_federais/soma_vl_extensa*100) %>% 
    relocate(uf, .before = qt_radares_federais)
}

calculo_br_2026 <- function(){
  br_2026_fixo <<- ufs_2026_fixo %>% 
    summarise(
      qt_radares_federais = sum(qt_radares_federais, na.rm = T),
      total_faixas_federais = sum(total_faixas_federais, na.rm = T),
      qt_radares_estaduais = sum(qt_radares_estaduais, na.rm = T),
      total_faixas_estaduais = sum(total_faixas_estaduais),
      total_radares_rodovias = sum(total_radares_rodovias),
      total_faixas_rodovias = sum(total_faixas_rodovias),
      qt_radares_urbanos = sum(qt_radares_urbanos),
      total_faixas_urbanos = sum(total_faixas_urbanos),
      total_faixas_aprv_rprd = sum(total_faixas_aprv_rprd),
      frota = sum(frota),
      reparados = sum(reparados),
      aprovados = sum(aprovados),
      total_radares_aprv_rprd = sum(total_radares_aprv_rprd),
      reprovados = sum(reprovados),
      total_radares_geral = sum(total_radares_geral),
      total_faixas_geral = sum(total_faixas_geral),
      soma_vl_extensa = sum(soma_vl_extensa)
    ) %>% 
    mutate(ano = 2026,
           uf = "BR",
           i1 = total_faixas_aprv_rprd/frota*10000,
           i2 = total_radares_geral/frota*10000,
           i3 = total_faixas_urbanos/frota*10000,
           i4 = total_faixas_rodovias/frota*10000,
           i5 = total_faixas_aprv_rprd/total_faixas_geral,
           i6 = total_faixas_federais/soma_vl_extensa*100) %>% 
    relocate(uf, .before = qt_radares_federais)
}
join_extensao_rod_federais <- function(ano){
  nome_variavel <- paste0("ufs_", ano, "_fixo")
  base <- get(nome_variavel) %>% 
    left_join(extensao_rod_federais, by = "uf")
  
  assign(nome_variavel, base, envir = .GlobalEnv)
  
}



calculo_i6_uf <- function(df){
  base <- get(df) %>% 
    mutate(
      i6 = total_faixas_federais/soma_vl_extensa*100)
  
  assign(df, base, envir = .GlobalEnv)
}
