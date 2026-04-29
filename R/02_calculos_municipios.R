agrupar_fixos_municipio <- function(ano){
  base <- get(paste0("radares_", ano)) %>%  
    filter(tipo_medidor == "FIXO" & ultimo_resultado != "REPROVADO") %>% 
    
    group_by(uf, municipio) %>% 
    summarise(qt_radares_federais = sum(tipo_via == "Rodovia Federal", na.rm = TRUE),
              total_faixas_federais = sum(if_else(tipo_via == "Rodovia Federal", qt_faixas, 0), na.rm = TRUE),
              qt_radares_estaduais = sum(tipo_via == "Rodovia Estadual", na.rm = TRUE),
              total_faixas_estaduais = sum(if_else(tipo_via == "Rodovia Estadual", qt_faixas, 0), na.rm = TRUE),
              total_radares_rodovias = qt_radares_federais + qt_radares_estaduais,
              total_faixas_rodovias = total_faixas_federais + total_faixas_estaduais,
              qt_radares_urbanos = sum(tipo_via == "Via Urbana", na.rm = T),
              total_faixas_urbanos = sum(if_else(tipo_via == "Via Urbana", qt_faixas, 0), na.rm = TRUE),
              total_faixas_aprv_rprd = sum(qt_faixas, na.rm = T))
  
  nome_variavel <- paste0("municipios_", ano, "_fixo")
  assign(nome_variavel, base, envir = .GlobalEnv)
}

join_municipios_frota <- function(ano){
  municipios <- get(paste0("municipios_", ano, "_fixo"))
  frota <- get(paste0("frota_", ano))
  
  join <- left_join(municipios, frota, by = c("uf", "municipio"))
  
  assign(paste0("municipios_", ano, "_fixo"), join, envir = .GlobalEnv)
}

contar_ultimo_resultado_municipios <- function(ano){
  base <- get(paste0("radares_", ano)) %>% 
    group_by(uf, municipio) %>% 
    summarise(
      reparados = sum(ultimo_resultado == "REPARADO", na.rm = TRUE),
      aprovados = sum(ultimo_resultado == "APROVADO", na.rm = TRUE),
      total_radares_aprv_rprd = reparados + aprovados,
      reprovados = sum(ultimo_resultado == "REPROVADO", na.rm = TRUE),
      total_radares_geral = aprovados + reparados + reprovados,
      total_faixas_geral = sum(qt_faixas, na.rm = T)
    ) %>% 
    mutate(ano = {{ano}})
  nome_variavel <- paste0("reprovados_", ano)
  assign(nome_variavel, base, envir = .GlobalEnv)
}

join_contagem_ultimo_resultado_municipios <- function(ano){
  nome_variavel <- paste0("municipios_", ano, "_fixo")
  base <- get(nome_variavel) %>% 
    left_join(get(paste0("reprovados_", ano)), by = c("uf", "municipio"))
  
  assign(nome_variavel, base, envir = .GlobalEnv)
}

# apenas para saber número de radares de cada tipo (para o relatório)-----------
agrupar_municipio_total <- function(ano){
  base <- get(paste0("radares_", ano)) %>%
    group_by(uf, municipio) %>% 
    summarise(reparados = sum(ultimo_resultado == "REPARADO", na.rm = TRUE),
              aprovados = sum(ultimo_resultado == "APROVADO", na.rm = TRUE),
              reprovados = sum(ultimo_resultado == "REPROVADO", na.rm = TRUE),
              qt_radares_federais = sum(tipo_via == "Rodovia Federal", na.rm = TRUE),
              qt_radares_estaduais = sum(tipo_via == "Rodovia Estadual", na.rm = TRUE),
              total_rodovias = qt_radares_federais + qt_radares_estaduais,
              qt_radares_urbanos = sum(tipo_via == "Via Urbana", na.rm = T),
              qt_fixo = sum(tipo_medidor == "FIXO", na.rm = T),
              qt_movel = sum(tipo_medidor == "MÓVEL", na.rm = T),
              qtde_estatico = sum(tipo_medidor == "ESTÁTICO", na.rm = T),
              qtde_estatico_ou_portatil = sum(tipo_medidor == "ESTÁTICO OU PORTÁTIL", na.rm = T),
              total_faixas = sum(qt_faixas, na.rm = T))
  
  nome_variavel <- paste0("municipios_", ano)
  assign(nome_variavel, base, envir = .GlobalEnv)
}

verificar_valores_ausentes <- function(ano){
  base <- get(paste0("municipios_", ano, "_fixo")) %>% 
    filter(is.na(frota))
  
  assign(paste0("valores_ausentes_", ano), base, envir = .GlobalEnv)
}

calculo_indicadores <- function(df){
  base <- get(df) %>% 
    mutate(
      i1 = total_faixas_aprv_rprd/frota*10000,
      i2 = total_radares_geral/frota*10000,
      i3 = total_faixas_urbanos/frota*10000,
      i4 = total_faixas_rodovias/frota*10000,
      i5 = total_faixas_aprv_rprd/total_faixas_geral
    )
  
  assign(df, base, envir = .GlobalEnv)
}