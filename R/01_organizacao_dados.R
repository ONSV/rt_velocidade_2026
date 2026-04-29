
limpar_nomes <- function(df){
  base <- clean_names(get(df))
  assign(df, base, envir = .GlobalEnv)
}
                      
eliminar_duplicatas <- function(df){
  base_nova <- get(df) %>% 
    group_by(uf, estado, municipio, local_verificacao, tipo_medidor) %>% 
    arrange(
      desc(ultimo_resultado %in% c("APROVADO", "REPARADO")),
      desc(data_ultima_verificacao)
    ) %>% 
    slice(1) %>% 
    ungroup()
  
  assign(df, base_nova, envir = .GlobalEnv)
}

join_radares <- function(ano){
  arquivos <- list.files(path = "data/", pattern = paste0(".*", ano, ".*\\.rda$"), full.names = TRUE)
  
  base_anual <- arquivos %>% 
    map_df(~{
      env <- new.env()
      load(.x, envir = env)
      as.list(env)[[1]]
    })
  
  nome_variavel <- paste0("radares_", ano)
  assign(nome_variavel, base_anual, envir = .GlobalEnv)
}

arrange_radares <- function(df){
  siglas <- c("ac", "al", "ap", "am", "ba", "ce", "df", "es", "go", 
              "ma", "mt", "ms", "mg", "pa", "pb", "pr", "pe", "pi", 
              "rj", "rn", "rs", "ro", "rr", "sc", "sp", "se", "to", "ers")
  
  busca_estados <- paste(siglas, collapse = "|")
  
  regex_federal <- "(?i)(?<![[:alpha:]])BR[[:space:]-]*[0-9]+(?![[:alpha:]])"
  regex_estadual <- paste0("(?i)(?<![[:alpha:]])(", busca_estados, ")[[:space:]-]*[0-9]+(?![[:alpha:]])")
  
  base_nova <- get(df) %>% 
    select(any_of(vetor_variaveis)) %>% 
    mutate(municipio = stri_trans_general(municipio, "Latin-ASCII"),
           municipio = tolower(municipio),
           municipio = str_replace_all(municipio,"'", " "),
           municipio = str_replace_all(municipio,"-", " "),
           uf = case_when(
             municipio == "curitiba" & uf == "GO" ~ "PR",
             municipio == "sao jose do norte" & uf == "RJ" ~ "RS",
             municipio == "rio branco" & uf == "RO" ~ "AC",
             municipio == "alecrim" & uf == "SP" ~ "RS",
             
             TRUE ~ uf
           ),
           municipio = case_when(
             municipio == "santana da parnaiba" & uf == "SP" ~ "santana de parnaiba",
             municipio == "senador la roque" & uf == "MA" ~ "senador la rocque",
             
             TRUE ~ municipio
           ),
           data_validade = as.Date(data_validade, format = "%d/%m/%Y"),
           data_ultima_verificacao = as.Date(data_ultima_verificacao, format = "%d/%m/%Y"),
           local_verificacao = tolower(local_verificacao),
           local_verificacao = str_replace_all(local_verificacao, "-", " "),
           tipo_via = case_when(
             str_detect(local_verificacao, regex_estadual) ~ "Rodovia Estadual",
            
             str_detect(local_verificacao, regex_federal) ~ "Rodovia Federal",
             
             TRUE ~ "Via Urbana"
           ),
           mes_ultima_verificacao = month(data_ultima_verificacao),
           ano_ultima_verificacao = year(data_ultima_verificacao)
    )
  
  assign(df, base_nova, envir = .GlobalEnv)
}

#frota

arrange_frota <- function(df){
  base <- get(df) %>% 
    clean_names() %>% 
    select(1:3) %>% 
    mutate(
     across(-c(1,2), as.double),
      municipio = stri_trans_general(municipio, "Latin-ASCII"),
      municipio = tolower(municipio),
      municipio = str_replace_all(municipio,"'", " "),
      municipio = str_replace_all(municipio,"-", " "),
      municipio = case_when(
        municipio == "muquem do sao francisco" & uf == "BA" ~ "muquem de sao francisco",
        municipio == "brazopolis" & uf == "MG" ~ "brasopolis",
        municipio == "sao tome das letras" & uf == "MG" ~ "sao thome das letras",
        municipio == "santarem" & uf == "PB" ~ "joca claudino",
        municipio == "trajano de morais" & uf == "RJ" ~ "trajano de moraes",
        municipio == "assu" & uf == "RN" ~ "acu",
        municipio == "santana do livramento" & uf == "RS" ~ "sant ana do livramento",
        municipio == "sao miguel d oeste" & uf == "SC" ~ "sao miguel do oeste",
        municipio == "santana da parnaiba" & uf == "SP" ~ "santana de parnaiba",
        
        TRUE ~ municipio),
      frota = total
    ) %>% select(-total)
  
  assign(df, base, envir = .GlobalEnv)
}

arrange_rodovias_federais <- function(){
  extensao_rod_federais <<- read_excel("data-raw/rodovias_federais.xlsx") %>% 
    clean_names() %>% 
    rename(uf = sg_uf)
}

arrange_extensao_capitais <- function(){
  load("data/df_osm.rda") 
  df_osm %>% 
    rename(municipio = nome_mun) %>% 
    mutate(municipio = stri_trans_general(municipio, "Latin-ASCII"), 
           municipio = tolower(municipio),
           dist_vias = dist_vias/1000) %>% 
    select(-nome_uf)
    
}

# mortes

arrange_rtdeaths <- function(){
  
  
  estados <- data.frame(uf = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
                    "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", 
                    "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
             nomes_uf = c("Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", 
                          "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão", "Mato Grosso", 
                          "Mato Grosso do Sul", "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco",
                          "Piauí", "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", 
                          "Roraima", "Santa Catarina", "São Paulo", "Sergipe", "Tocantins"))
  
  roadtrafficdeaths::rtdeaths %>% 
    filter(ano_ocorrencia == 2024) %>% 
    group_by(nome_municipio_ocor, nome_uf_ocor) %>% 
    summarise(n_mortes = n()) %>% 
    rename(municipio = nome_municipio_ocor,
           estado = nome_uf_ocor) %>% 
    mutate(municipio = stri_trans_general(municipio, "Latin-ASCII"),
           municipio = tolower(municipio),
           municipio = str_replace_all(municipio,"'", " "),
           municipio = str_replace_all(municipio,"-", " ")) %>% 
    left_join(estados, by = c("estado" = "nomes_uf")) %>% 
    select(-estado) %>% 
    relocate(uf, .before = n_mortes)
}

# infracoes

join_infracoes <- function(ano){
  arquivos <- list.files(
    path = "data/", 
    pattern = paste0("infracoes_.*", ano, ".*\\.rds$"), 
    full.names = TRUE 
  )
    
    base <- arquivos %>% 
      map(readRDS) %>% 
      bind_rows()
    
    assign(paste0("infracoes_", ano), base, envir = .GlobalEnv )
}

arrange_infracoes_2024 <- function(){
  infracoes_2024 %>% 
    clean_names() %>% 
    mutate(across(-c(uf_jurisdicao_veiculo_desc, mes), as.numeric)) %>%
    mutate(across(where(is.numeric), ~ .x * 1000)) %>% 
    filter(uf_jurisdicao_veiculo_desc %in% c("7455", "7463", "7471")) %>% 
    pivot_longer(
      cols = -c(uf_jurisdicao_veiculo_desc, mes),
      names_to = "estado",
      values_to = "n_infracoes"
    ) %>% 
    group_by(estado) %>% 
    summarise(n_infracoes = sum(n_infracoes))
    
}

arrange_infracoes_2023 <- function(){
  infracoes_2023 %>%
    clean_names() %>% 
    filter(cod_infracao %in% c("7455", "7463", "7471")) %>% 
    rename(estado = uf) %>% 
    mutate(cod_infracao = coalesce(cod_infracao, cod_infracao_2, cod_infracao_3),
           estado = tolower(str_replace_all(estado, " ", "_"))) %>% 
    select(-c(cod_infracao_2, cod_infracao_3)) %>% 
    group_by(estado) %>% 
    summarise(n_infracoes = sum(quantidade))
}

# seleiconar
selecionar_indicadores <- function(ano, variavel){
  df <- paste0(variavel, "_", ano, "_fixo")
  
  if (variavel == "municipios") {
    base <- get(df) %>% 
      select(c(uf, municipio, ano, starts_with("i")))
  } else {
    base <- get(df) %>% 
      select(c(uf, ano, starts_with("i")))
  }
  
  nome_variavel <- paste0("indicadores_", variavel, "_", ano, "_fixo")
  assign(nome_variavel, base, envir = .GlobalEnv)
}

