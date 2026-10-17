
library(tidyverse)
library(dbplyr)
library(duckplyr)
library(DBI)
library(duckdb)
library(R.utils)
library(ggeffects)
library(ggspatial)
library(sf)
library(duckdb)
library(ggtext)
library(ggshadow)
library(patchwork)
library(fixest)

options(scipen = 999)

#############################################################################
#############################################################################

codigos <- read.csv("reference/codigos.csv")
mun_stations <- readRDS("data/processed/mun_stations_distance.rds")
rain_stations <- readRDS("data/processed/rain_stations.rds")

censo_data <- readRDS("data/processed/censo_data.rds")

mun_area   <- read.csv2("data/raw/census/tabela4714.csv", skip = 3) |>
  janitor::clean_names() |>
  select(1, 4) |>
  rename(area_mun = 2)

#############################################################################
#############################################################################

tse <- dbConnect(
  duckdb(),
  dbdir = "data/processed/TSE"
)

cand <- tbl(tse, "cand") |>
  filter(
    ANO_ELEICAO %in% c(2008:2024),
    DS_CARGO %in% c("PRESIDENTE", "PREFEITO"),
    NM_TIPO_ELEICAO == "ELEICAO ORDINARIA"
  )

cand_votes <- tbl(tse, "cand_votes") |>
  filter(
    ANO_ELEICAO %in% c(2008:2024),
    DS_CARGO %in% c("PRESIDENTE", "DEPUTADO FEDERAL"),
    NM_TIPO_ELEICAO == "ELEICAO ORDINARIA"
  )

pt_votos <- cand_votes |> 
  mutate(
    pt_dummy = (SG_PARTIDO == "PT") |> 
      as.numeric()
  ) |> 
  group_by(ANO_ELEICAO, DS_CARGO, NR_TURNO, CD_MUNICIPIO, pt_dummy) |> 
  summarise(
    QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS)
  ) |> 
  group_by(ANO_ELEICAO, DS_CARGO, NR_TURNO, CD_MUNICIPIO) |> 
  mutate(
    p_votos  = QT_VOTOS_NOMINAIS/sum(QT_VOTOS_NOMINAIS) * 100
  ) |> 
  select(-QT_VOTOS_NOMINAIS) |> 
  pivot_wider(
    names_from = pt_dummy, 
    values_from = p_votos, names_prefix = "pt_votos_"
  )  |> 
  mutate(
    across(contains("pt_votos_"), ~ifelse(is.na(.x), 0, .x))
  ) |> 
  ungroup() |> 
  select(-pt_votos_0) |> 
  pivot_wider(names_from = DS_CARGO, names_prefix = "pt_", values_from = pt_votos_1) 

n_cand <- cand |>
  filter(
    ST_CANDIDATO_INSERIDO_URNA %in% c("#NE", "SIM")
  ) |>
  group_by(ANO_ELEICAO, SG_UE, NR_TURNO) |>
  summarise(n_cand_executivo = n()) |>
  collect() |>
  mutate(
    ANO_ELEICAO = as.numeric(ANO_ELEICAO),
    NR_TURNO = as.numeric(NR_TURNO)
  ) |>
  janitor::clean_names()

turnout <- tbl(tse, "mun_turnout") |>
  filter(
    ANO_ELEICAO %in% c(2008:2024),
    DS_CARGO %in% c("PRESIDENTE", "PREFEITO"),
    NM_TIPO_ELEICAO == "ELEICAO ORDINARIA"
  ) |>
  mutate(
    SG_UE = ifelse(DS_CARGO  == "PREFEITO", as.character(CD_MUNICIPIO), "BR")
  ) |>
  select(
    ANO_ELEICAO, DT_ELEICAO, NR_TURNO, SG_UF, CD_MUNICIPIO, SG_UE, QT_APTOS, 
    QT_COMPARECIMENTO, QT_VOTOS_BRANCOS, QT_VOTOS_NULOS
  ) |>
  mutate(
    p_comp    = QT_COMPARECIMENTO/QT_APTOS * 100
  ) |>
  left_join(
    pt_votos,
  ) |> 
  janitor::clean_names() |>
  collect() |>
  left_join(
    codigos |>
      select(codigo_ibge = id_municipio, id_municipio_tse, regiao, mesorregiao, capital_uf),
    by = c("cd_municipio" = "id_municipio_tse")
  ) |>
  left_join(
    n_cand,
    by = c("ano_eleicao", "sg_ue", "nr_turno")
  ) |> 
  left_join(
    mun_stations |>
      filter(i == 1) |>
      #filter(distance <= 50) |>
      select(ano, codigo_ibge = code_muni, codigo_wmo, distance),
    by = c("ano_eleicao" = "ano", "codigo_ibge")
  ) |>
  mutate(
    turno_2 = (nr_turno == 2) |>
           as.numeric(),
    metro_dummy = stringi::stri_detect(tolower(mesorregiao), regex = "metro") |>
      as.numeric(),
    regiao = factor(
     regiao,
     levels = c("Sudeste", "Sul", "Norte", "Nordeste", "Centro-Oeste")
   ),
   data_formatada = lubridate::dmy(dt_eleicao) |>
     format("%Y-%m-%d") |>
     as.character()
  )

#############################################################################
#############################################################################

inmet <- dbConnect(
  duckdb(),
  dbdir = "data/processed/INMET"
)

rain <- tbl(inmet, "rainfall") |>
  mutate(
    hora_num = as.numeric(substr(hora_utc, 1, 2)),
    precipitacao_total_horario_mm = ifelse(precipitacao_total_horario_mm == -9999, NA, precipitacao_total_horario_mm),
    temperatura_do_ar_bulbo_seco_horaria_c = ifelse(temperatura_do_ar_bulbo_seco_horaria_c == -9999, NA, precipitacao_total_horario_mm)
  ) %>% 
  #filter(hora_num <= 18) |>
  group_by(codigo_wmo, data_yyyy_mm_dd) |>
  summarise(
      precipitacao       = sum(precipitacao_total_horario_mm, na.rm = TRUE),
      temp_max           = max(temperatura_do_ar_bulbo_seco_horaria_c, na.rm = TRUE),
  ) |>
  filter(data_yyyy_mm_dd %in% c(turnout$data_formatada)) |>
  collect()

#############################################################################
#############################################################################

distancia_censo <- data.frame(
  "anos" =  unique(turnout$ano_eleicao)
) |>
  mutate(
    censo_2022 = abs(2022 - anos),
    censo_2010 = abs(2010 - anos),
    distancia_censo = case_when(
      censo_2010 < censo_2022 ~ "2010",
      censo_2010 > censo_2022 ~ "2022",
      censo_2010 == censo_2022 ~ "2010"
    )
  )

eleicoes_censo_2010 <- distancia_censo |>
  filter(distancia_censo == "2010") |>
  pull(anos)

eleicoes_censo_2022 <- distancia_censo |>
  filter(distancia_censo == "2022") |>
  pull(anos)

turnout_rain <- turnout |>
  left_join(
    rain |>
      rename(data_formatada = data_yyyy_mm_dd) |>
      mutate(
        data_formatada = as.character(data_formatada)
      ),
    by = c("data_formatada", "codigo_wmo")
  ) |>
  mutate(
    tipo_elecao = ifelse(ano_eleicao %in% c(seq(2000, 2024, by = 4)), "Municipais", "Nacionais"),
    periodo = glue::glue("{ano_eleicao}/{nr_turno}")
  ) |>
  left_join(
    censo_data |>
      mutate(code_muni = as.numeric(code_muni)),
    by = c("codigo_ibge" = "code_muni")
  ) |>
  left_join(
    mun_area |>
      mutate(cod = as.numeric(cod)),
    by = c("codigo_ibge" = "cod")
  ) |>
  filter(
    cd_municipio %in% c(
      turnout |>
      filter(ano_eleicao %in% c(2008, 2012, 2016, 2020, 2024)) |>
      pull(cd_municipio) |>
      unique()
    )
  ) |>
  mutate(
    p_idosos = case_when(
      ano_eleicao %in% eleicoes_censo_2010 ~ p_idosos_2010,
      ano_eleicao %in% eleicoes_censo_2022 ~ p_idosos_2022,
    ),
    p_alfabetizados = case_when(
      ano_eleicao %in% eleicoes_censo_2010 ~ p_alfabetizados_2010,
      ano_eleicao %in% eleicoes_censo_2022 ~ p_alfabetizados_2022,
    ),
    p_pop_urbano = case_when(
      ano_eleicao %in% eleicoes_censo_2010 ~ p_pop_urbana_2010,
      ano_eleicao %in% eleicoes_censo_2022 ~ p_pop_urbana_2022,
    )
  ) |>
  select(
    -c(p_idosos_2010, p_idosos_2022, p_alfabetizados_2010, p_alfabetizados_2022, p_pop_urbana_2010, p_pop_urbana_2022)
  ) |>
  filter(!is.na(codigo_ibge))

saveRDS(turnout_rain, "data/processed/turnout_rain.rds")
