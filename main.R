library(tidyverse)
library(readxl)
library(janitor)
library(magrittr)
library(stringi)
library(curl)
library(onsvplot)

source("R/00_radares.R")
source("R/00_frota.R")
source("R/00_renainf.R")
source("R/01_organizacao_dados.R")
source("R/02_calculos_municipios.R")
source("R/03_calculos_ufs.R")
source("R/04_calculos_capitais.R")
source("R/05_testes_correlacoes.R")
source("plots.R")

# importa e limpa bases de frota
lapply(2023:2026, importar_frota)
lapply(ls(pattern = "frota_.*"), arrange_frota)


# importa dados de todos os meses e anos combinados-----------------------------
# combinacoes <- expand.grid(ano = 2024:2026, mes = 1:12)
# pmap(combinacoes, importar_dados)

# junta os dados de todos os meses de cada ano
lapply(2024:2026, join_radares)

radares_2023 <- read_excel("data-raw/Dados Radares Puerta - 2023.xlsx") %>% 
  clean_names() %>% 
  rename(uf = sigla_uf,
         qt_faixas = qtd_faixas) %>% 
  mutate(uf = ifelse(municipio == "BRASÍLIA", "DF", uf),
         estado = ifelse(municipio == "BRASÍLIA", "Distrito Federal", uf),
         tipo_medidor = toupper(tipo_medidor),
         ultimo_resultado = toupper(ultimo_resultado))


# limpa os nomes de cada df
lapply(ls(pattern = "radares_.*"), limpar_nomes)

# elimina duplicatas
lapply(ls(pattern = "radares_.*"), eliminar_duplicatas)

# arruma o df final.
vetor_variaveis <- names(radares_2024)     
lapply(ls(pattern = "radares_.*"), arrange_radares)



# calcula métricas por município (tipo fixo)
lapply(2023:2026, agrupar_fixos_municipio)

lapply(2023:2026, contar_ultimo_resultado_municipios)

lapply(2023:2026, join_municipios_frota)

lapply(2023:2026, verificar_valores_ausentes)

lapply(2023:2026, join_contagem_ultimo_resultado_municipios)

lapply(ls(pattern = "municipios.*_fixo"), calculo_indicadores)


# UFS
lapply(2023:2026, agrupar_fixos_uf)
lapply(2023:2026, contar_ultimo_resultado_uf)
lapply(2023:2026, join_ufs_frota)
lapply(2023:2026, join_contagem_ultimo_resultado_uf)
arrange_rodovias_federais()
lapply(2023:2026, join_extensao_rod_federais)
lapply(ls(pattern = "ufs.*_fixo"), calculo_indicadores)
lapply(ls(pattern = "ufs.*_fixo"), calculo_i6_uf)

#
siglas <- data.frame(uf = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
                            "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", 
                            "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
                     estado = stri_trans_general(str_replace_all(tolower(unique(radares_2024$estado)), " ", "_"),"Latin-ASCII" )) 

bilhao_km_percorrido_2023 <- read_excel("data/2023-2024.xlsx") %>% 
  clean_names() %>% 
  select(-c(x2024, taxa_2024)) %>% 
  rename(estado = x1,
         bilhao_km_percorrido = x2023) %>% 
  mutate(estado = str_replace_all(tolower(estado), " ", "_")) %>% 
  left_join(siglas, by = "estado") %>% 
  mutate(uf = ifelse(estado == "brasil", "BR", uf)) %>% 
  select(-estado) %>% 
  relocate(uf, .before = bilhao_km_percorrido)

bilhao_km_percorrido_2024 <- read_excel("data/2023-2024.xlsx") %>% 
  clean_names() %>% 
  select(-c(x2023, taxa_2023)) %>% 
  rename(estado = x1,
         bilhao_km_percorrido = x2024) %>% 
  mutate(estado = str_replace_all(tolower(estado), " ", "_")) %>% 
  left_join(siglas, by = "estado") %>% 
  mutate(uf = ifelse(estado == "brasil", "BR", uf)) %>% 
  select(-estado) %>% 
  relocate(uf, .before = bilhao_km_percorrido)

ufs_2023_fixo %<>% left_join(bilhao_km_percorrido_2023, by = "uf") %>% 
  mutate(i7 = total_faixas_aprv_rprd/bilhao_km_percorrido * 1000000000)

ufs_2024_fixo %<>% left_join(bilhao_km_percorrido_2024, by = "uf") %>% 
  mutate(i7 = total_faixas_aprv_rprd/bilhao_km_percorrido * 1000000000,
        taxa_2024 = str_replace_all(taxa_2024, ",", "."),
        taxa_2024 = as.double(taxa_2024))

# capitais
dist_vias <- arrange_extensao_capitais()
lapply(2023:2026, filtra_capitais)
lapply(ls(pattern = "capitais.*_fixo"), join_capitais_dist_vias)
lapply(ls(pattern = "capitais.*_fixo"), calculo_i6_capitais)

rtdeaths <- arrange_rtdeaths()
municipios_mortes_2024 <- join_municipios_mortes()
correlacao <- calcular_correlacao()
correlacao_ufs <- lapply(unique(municipios_2024_fixo$uf), calcular_correlacao_uf)
correlacao_ufs <- bind_rows(correlacao_ufs)

# infrações
# links_2024 <- c(
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/74764b9f-218d-42c9-8196-77416e376349/download/quantidadedeinfraescomnotificaodepenalidadenpemitidasporufdejurisdiodoveculojaneiro2024.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/b996382f-8805-4160-8055-e5cf5cfb825c/download/quantidadedeinfraescomnotificaodepenalidadenpemitidasporufdejurisdiodoveculofevereiro2024.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/623b510f-87e0-487c-a0e9-9cff359132d7/download/quantidadedeinfraescomnotificaodepenalidadenpemitidasporufdejurisdiodoveculomarco2024.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/f5311a6a-7bfb-48d6-9b22-ead58741c3b4/download/quantidadedeinfraescomnotificaodepenalidadenpemitidasemabrilde2024csv.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/7fa0528d-9435-4c77-b334-8e5e555dbcd4/download/quantidadedeinfraescomnotificaodepenalidadenpemitidasemmaiode2024.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/628097bc-7cac-4143-865a-6e962b6eda51/download/quantidadedeinfraescomnotificaodepenalidadenpemitidasemjunhode2024.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/a16ef84b-441b-4851-9d78-97e61d898f95/download/quantidadedeinfraescomnotificaodepenalidadenpemitidasemjulhode2024.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/6a8bbb4b-66b8-4bfd-964c-85bf55545cc5/download/quantidadedeinfraescomnotificaodepenalidadenpemitidasemagosto2024.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/40953292-ca26-4c80-9b96-cb9c36739a4e/download/copy_of_quantidadedeinfraescomnotificaodepenalidadenpemitidasemsetembro2024.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/28ce4015-81fc-48dd-8efd-8255e57a364e/download/quantidadedeinfraescomnotificaodepenalidadenpemitidasemoutubro2024.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/84e2174b-b240-4091-8c78-81387626d490/download/quantidadedeinfraescomnotificaodepenalidadenpemitidasemnovembro2024.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/7bfd15d8-05d8-418a-9007-ab7a8547f94b/download/quantidadedeinfraescomnotificaodepenalidadenpemitidasemdezembrode2024.csv"
# )
# 
# meses <- c(
#   "janeiro", "fevereiro", "marco", "abril", "maio", "junho", 
#   "julho", "agosto", "setembro", "outubro", "novembro", "dezembro"
# )
# 
# dados_2024 <- data.frame(link = links_2024,
#                     mes = meses,
#                     ano = 2024)
# 
# pmap(dados_2024, importar_infracoes)
# 
# 
# links_2023 <- c(
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/571432d2-e00f-4033-b856-582bc030e9b7/download/2023_01_infracoes_com_np.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/61312dac-957f-4424-8cc4-fa5b0a4d813b/download/2022_12_infracoes_com_np.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/ccbfe4a1-b247-4e84-9134-721a537b651b/download/2023_03_infracoes_com_np.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/b0f3612f-3550-4eaf-98de-d3d83b632d53/download/2023_04_infracoes_com_np.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/2e7e1439-a305-4d17-9cb3-4e58bcad6618/download/2023_05_infracoes_com_np.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/b8d4b961-2120-405b-96cd-721580eaaec7/download/2023_06_infracoes_com_np.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/87462ff4-3f43-4821-a518-d64b2b294c6e/download/2023_07_infracoes_com_np.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/2f933318-8486-41a1-acb1-5456be8aa987/download/2023_08_infracoes_com_np.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/1d2ebadb-5ec6-4307-b8c9-a6d6090d8162/download/quantidade-de-infracoes-com-notificacao-de-penalidade-np-emitidas-em-setembro-de-2023-por-uf-de-.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/d9c8a3f3-e548-4e63-be2d-3cdbcdfe68c6/download/quantidade-de-infracoes-com-notificacao-de-penalidade-np-emitidas-em-outubro-de-2023-por-uf-de-j.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/57a56be9-23ff-4807-a2ee-febb0fc64be4/download/quantidade-de-infracoes-com-notificacao-de-penalidade-np-emitidas-em-novembro-de-2023-por-uf-de-.csv",
#   "https://dados.transportes.gov.br/dataset/1f29c461-f1db-4fb6-b94b-8b75a7cc2da0/resource/8f636d21-0c86-4e69-aabe-bb352bf8af0b/download/quantidadedeinfraescomnotificaodepenalidadenpemitidasemdezembrode2023porufdejuris.csv"
# )
# 
# dados_2023 <- data.frame(link = links_2023,
#                          mes = meses,
#                          ano = 2023)

# pmap(dados_2023, importar_infracoes)


join_infracoes(2023)
join_infracoes(2024)

infracoes_2023 <- arrange_infracoes_2023()
infracoes_2024 <- arrange_infracoes_2024()


siglas <- data.frame(uf = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
                             "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", 
                             "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
                      estado = unique(infracoes_2024$estado))

infracoes_2024 %<>% left_join(siglas, by = "estado")
infracoes_2023 %<>% left_join(siglas, by = "estado")

ufs_2024_fixo %<>% left_join(infracoes_2024 %>% select(-estado), by = "uf") %>% 
  relocate(n_infracoes, .after = soma_vl_extensa) %>% 
  mutate(ii1 = n_infracoes/frota,
         ii2 = n_infracoes/total_faixas_aprv_rprd)

ufs_2023_fixo %<>% left_join(infracoes_2023 %>% select(-estado), by = "uf") %>% 
  relocate(n_infracoes, .after = soma_vl_extensa) %>% 
  mutate(ii1 = n_infracoes/ frota,
         ii2 = n_infracoes/ total_faixas_aprv_rprd)

# plots 
plot_corr_ii1_if1()
cor.test(ufs_2024_fixo$i1, ufs_2024_fixo$ii1, method = "spearman")

plot_corr_ii2_if1()
cor.test(ufs_2024_fixo$i1, ufs_2024_fixo$ii2, method = "spearman")


##
# indicadores_municipios_2023 <- municipios_2023_fixo %>% 
#   select(c(uf, municipio, ano, starts_with("i")))


calculo_br_2023()
calculo_br_2024()
calculo_br_2025()
calculo_br_2026()

combinacoes <- expand.grid(ano = 2023:2026,
                           variavel = c("municipios", "ufs"))

pmap(combinacoes, selecionar_indicadores)


tabela_2023 <- ufs_2023_fixo %>%
  select(-c(ii1, ii2)) %>% bind_rows(br_2023_fixo) %>% 
  select(uf, ano, starts_with("i"))

tabela_2024 <- ufs_2024_fixo %>%
  select(-c(ii1, ii2)) %>% bind_rows(br_2024_fixo) %>% 
  select(uf, ano, starts_with("i"))

tabela_2025 <- ufs_2025_fixo %>% bind_rows(br_2025_fixo) %>% 
  select(uf, ano, starts_with("i"))

tabela_2025 <- ufs_2025_fixo %>% bind_rows(br_2025_fixo) %>% 
  select(uf, ano, starts_with("i"))

tabela_2026 <- ufs_2026_fixo %>% bind_rows(br_2026_fixo) %>% 
  select(uf, ano, starts_with("i"))


tabela_geral <- bind_rows(tabela_2023, tabela_2024, tabela_2025, tabela_2026)

# lista_de_bases <- list(indicadores_ufs_2023_fixo, indicadores_ufs_2024_fixo)
# 
# tabela_geral <- bind_rows(lista_de_bases)


#-----------------------------------------------------------------------------
azul_onsv <- "#00496d"

p <- tabela_geral %>%
  ggplot(aes(x = ano, y = i1, color = uf, group = uf,
             text = paste("UF:", uf, "<br>Valor:", round(i1, 2)))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = rep(azul_onsv, length(unique(tabela_geral$uf)))) +
  theme_onsv() +
  labs(
    title = "I1",
    x = "Ano",
    y = "Valor do I1",
    color = "Selecione a UF:"
  ) +
  scale_x_continuous(breaks = c(2023, 2024, 2025, 2026))

fig <- ggplotly(p, tooltip = "text")

for(i in seq_along(fig$x$data)){
  if(fig$x$data[[i]]$name != "BR"){
    fig$x$data[[i]]$visible <- "legendonly"
  }
}

fig <- fig %>% layout(
  showlegend = TRUE,
  legend = list(
    orientation = "v",  
    x = 1.05,           
    y = 0.5,             
    xanchor = "left",    
    yanchor = "middle",
    traceorder = "normal"
  ),
  margin = list(r = 100) 
)

fig

# correlacao 
# 2023

uf_tabela_correlacao_2023 <- radares_2023 %>%  
  filter(ultimo_resultado != "REPROVADO") %>% 
  
  group_by(uf) %>% 
  summarise(qt_radares_federais = sum(tipo_via == "Rodovia Federal", na.rm = TRUE),
            total_faixas_federais = sum(if_else(tipo_via == "Rodovia Federal", qt_faixas, 0), na.rm = TRUE),
            qt_radares_estaduais = sum(tipo_via == "Rodovia Estadual", na.rm = TRUE),
            total_faixas_estaduais = sum(if_else(tipo_via == "Rodovia Estadual", qt_faixas, 0), na.rm = TRUE),
            total_radares_rodovias = qt_radares_federais + qt_radares_estaduais,
            total_faixas_rodovias = total_faixas_federais + total_faixas_estaduais,
            qt_radares_urbanos = sum(tipo_via == "Via Urbana", na.rm = T),
            total_faixas_urbanos = sum(if_else(tipo_via == "Via Urbana", qt_faixas, 0), na.rm = TRUE),
            total_faixas_aprv_rprd = sum(qt_faixas, na.rm = T)) %>% 
  left_join(frota_2023 %>% group_by(uf) %>% summarise(frota = sum(frota, na.rm = T)), by = "uf") %>% 
  left_join(extensao_rod_federais, by = "uf") %>% 
  left_join(infracoes_2023, by = c("uf")) %>% 
  left_join(reprovados_ufs_2023, by = "uf") %>% 
  left_join(bilhao_km_percorrido_2023, by = "uf") %>% 
  mutate(
    i1 = total_faixas_aprv_rprd/frota*10000,
    i2 = total_radares_geral/frota*10000,
    i3 = total_faixas_urbanos/frota*10000,
    i4 = total_faixas_rodovias/frota*10000,
    i5 = total_faixas_aprv_rprd/total_faixas_geral,
    i6 = total_faixas_federais/soma_vl_extensa*100,
    i7 = total_faixas_aprv_rprd/bilhao_km_percorrido,
    ii1 = n_infracoes/frota,
    ii2 = n_infracoes/total_faixas_aprv_rprd)

uf_tabela_correlacao_2024 <- radares_2024 %>%  
  filter(ultimo_resultado != "REPROVADO") %>% 
  
  group_by(uf) %>% 
  summarise(qt_radares_federais = sum(tipo_via == "Rodovia Federal", na.rm = TRUE),
            total_faixas_federais = sum(if_else(tipo_via == "Rodovia Federal", qt_faixas, 0), na.rm = TRUE),
            qt_radares_estaduais = sum(tipo_via == "Rodovia Estadual", na.rm = TRUE),
            total_faixas_estaduais = sum(if_else(tipo_via == "Rodovia Estadual", qt_faixas, 0), na.rm = TRUE),
            total_radares_rodovias = qt_radares_federais + qt_radares_estaduais,
            total_faixas_rodovias = total_faixas_federais + total_faixas_estaduais,
            qt_radares_urbanos = sum(tipo_via == "Via Urbana", na.rm = T),
            total_faixas_urbanos = sum(if_else(tipo_via == "Via Urbana", qt_faixas, 0), na.rm = TRUE),
            total_faixas_aprv_rprd = sum(qt_faixas, na.rm = T)) %>% 
  left_join(frota_2024 %>% group_by(uf) %>% summarise(frota = sum(frota, na.rm = T)), by = "uf") %>% 
  left_join(extensao_rod_federais, by = "uf") %>% 
  left_join(infracoes_2024, by = c("uf")) %>% 
  left_join(reprovados_ufs_2024, by = "uf") %>% 
  left_join(bilhao_km_percorrido_2024, by = "uf") %>% 
  mutate(
    i1 = total_faixas_aprv_rprd/frota*10000,
    i2 = total_radares_geral/frota*10000,
    i3 = total_faixas_urbanos/frota*10000,
    i4 = total_faixas_rodovias/frota*10000,
    i5 = total_faixas_aprv_rprd/total_faixas_geral,
    i6 = total_faixas_federais/soma_vl_extensa*100,
    i7 = total_faixas_aprv_rprd/bilhao_km_percorrido,
    ii1 = n_infracoes/frota,
    ii2 = n_infracoes/total_faixas_aprv_rprd)
  

# infracoes vs faixas
# 2023
cor.test(uf_tabela_correlacao_2023$i1, uf_tabela_correlacao_2023$ii1, method = "spearman")
cor.test(uf_tabela_correlacao_2023$i1, uf_tabela_correlacao_2023$ii2, method = "spearman") # gráfico

# 2024
cor.test(uf_tabela_correlacao_2024$i1, uf_tabela_correlacao_2024$ii1, method = "spearman")
cor.test(uf_tabela_correlacao_2024$i1, uf_tabela_correlacao_2024$ii2, method = "spearman") # gráfico


# n_mortes vs faixas
cor.test(ufs_2023_fixo$taxa_2023, ufs_2023_fixo$i7, method = "spearman") 
cor.test(ufs_2024_fixo$taxa_2024, ufs_2024_fixo$i7, method = "spearman") 

