
library(dplyr)
library(tidyr)
library(duckplyr)
library(DBI)
library(duckdb)
library(R.utils)
source("scripts/functions/download_tse.R")

#############################################################################
#############################################################################

options(timeout = Inf)

download_tse(file_name = "consulta_cand", years = seq(1996, 2026, 2), unzip_path = "data/raw/cand/")
download_tse(file_name = "consulta_cand_complementar", years = seq(2018, 2026, 2), unzip_path = "data/raw/cand_complementar/")

con <- dbConnect(
  duckdb(),
  dbdir = "data/processed/TSE"
)

#---------------------------------------------------------------------------#

# VARIAVEIS
VARIAVEIS_GERAIS <- c(
  "ANO_ELEICAO", "NM_TIPO_ELEICAO", "DT_ELEICAO",
  "SG_UF", "SG_UE", "NR_TURNO", "DS_CARGO",
  "SQ_CANDIDATO", "NR_CANDIDATO", "NR_TITULO_ELEITORAL_CANDIDATO",
  "NM_CANDIDATO", "NM_URNA_CANDIDATO",
  "SG_PARTIDO",
  "DT_NASCIMENTO", "DS_GENERO",
  "DS_COR_RACA", "DS_OCUPACAO",
  "DS_ESTADO_CIVIL", "DS_GRAU_INSTRUCAO",
  "DS_COMPOSICAO_COLIGACAO",
  "DS_SITUACAO_CANDIDATURA",
  "DS_SIT_TOT_TURNO"
)
VARIAVEIS_COMP <- c(
  "ANO_ELEICAO",
  "SQ_CANDIDATO",
  #"NR_IDADE_DATA_POSSE", -- BEM melhor fazer a idade manualmente ~Kaio 08 Mar 2026
  "NM_TIPO_DESTINACAO_VOTOS",
  "ST_CANDIDATO_INSERIDO_URNA",
  #"VR_DESPESA_MAX_CAMPANHA", -- Julgo desnecessario Kaio 08 Mar 2026
  "DS_DETALHE_SITUACAO_CAND",
  "DS_SITUACAO_CANDIDATO_PLEITO",
  "DS_SITUACAO_CANDIDATO_URNA",
  "DS_SITUACAO_JULGAMENTO",
  "DS_SITUACAO_JULGAMENTO_PLEITO",
  "DS_SITUACAO_JULGAMENTO_URNA"
)

TODAS_VARS <- unique(c(VARIAVEIS_GERAIS, VARIAVEIS_COMP))

#---------------------------------------------------------------------------#
# CANDIDATURAS
cand <- list()
for (x in list.files("data/raw/cand/", full.names = T)) {
  print(x)
  cand[[x]] <- data.table::fread(x, encoding = "Latin-1") |>
    select(any_of(TODAS_VARS)) |>
    mutate(across(everything(), as.character))
}

cand_df <- cand |>
  bind_rows()

rm(cand)

#---------------------------------------------------------------------------#
# VARIAVEIS ADICIONAIS DE CANDIDATURAS

cand_comp <- list()
for (x in list.files("data/raw/cand_complementar/", full.names = T)) {
  print(x)

  df <- data.table::fread(x, encoding = "Latin-1")

  cand_comp[[x]] <- df |>
    select(any_of(VARIAVEIS_COMP)) |>
    mutate(across(everything(), as.character))
}


cand_comp_df <- cand_comp |>
  bind_rows() |>
  distinct(
    ANO_ELEICAO, SQ_CANDIDATO,
    .keep_all = T
  )


rm(cand_comp)

#---------------------------------------------------------------------------#

cand_2018 <- cand_df |>
  filter(ANO_ELEICAO < 2018) |>
  select(
    any_of(TODAS_VARS)
  )

cand_unificado <- cand_df |>
  filter(ANO_ELEICAO >= 2018) |>
  select(
    any_of(VARIAVEIS_GERAIS)
  ) |>
  left_join(cand_comp_df) |>
  mutate(
    DS_SITUACAO_CANDIDATO_PLEITO = ifelse(DS_SITUACAO_CANDIDATO_PLEITO %in% c("#NE", "#NE#"), DS_SITUACAO_JULGAMENTO_PLEITO, DS_SITUACAO_CANDIDATO_PLEITO),
    DS_SITUACAO_CANDIDATO_URNA = ifelse(DS_SITUACAO_CANDIDATO_URNA %in% c("#NE", "#NE#"), DS_SITUACAO_JULGAMENTO_URNA, DS_SITUACAO_CANDIDATO_URNA),
  ) |>
  bind_rows(cand_2018) |>
  mutate(
    across(any_of(VARIAVEIS_COMP), ~ifelse(is.na(.x), "#NE", .x)),
    DS_SITUACAO_CANDIDATURA = ifelse(DS_SITUACAO_CANDIDATURA %in% c("#NE", "#NE#"), DS_SITUACAO_JULGAMENTO, DS_SITUACAO_CANDIDATURA),
  ) |>
  select(
    -c(
      DS_SITUACAO_JULGAMENTO,
      DS_SITUACAO_JULGAMENTO_PLEITO,
      DS_SITUACAO_JULGAMENTO_URNA
    )
  ) |>
  mutate(
    # É NECESSARIO ALGUMAS TRANSFORMAÇÕES PQ O BANCO DO TSE DIVERSAS INCONSISTENCIAS
    # NA CATEGORIZAÇÃO DE ALGUNS DADOS
    across(c(SQ_CANDIDATO, NR_CANDIDATO), as.numeric),
    across(c(SQ_CANDIDATO, NR_CANDIDATO), as.character),
    across(
      c(NM_TIPO_ELEICAO, DS_CARGO), 
      ~ .x  |>
        stringi::stri_trans_general("ascii") |>
        toupper()
    ),
    NM_TIPO_ELEICAO = ifelse(NM_TIPO_ELEICAO == "ORDINARIA", "ELEICAO ORDINARIA", NM_TIPO_ELEICAO),
    SG_UE = ifelse(DS_CARGO == "PRESIDENTE", "BR", SG_UE)
  ) |>
  distinct(
    ANO_ELEICAO, NM_TIPO_ELEICAO, DT_ELEICAO,
    SG_UF, SG_UE, NR_TURNO, DS_CARGO,
    SQ_CANDIDATO, NR_CANDIDATO,
    NR_TITULO_ELEITORAL_CANDIDATO,
    .keep_all = T
  )

dbWriteTable(con, "cand", cand_unificado, overwrite = TRUE)
