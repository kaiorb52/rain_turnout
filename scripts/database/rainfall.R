
library(dplyr)
library(tidyr)
library(duckplyr)
library(DBI)
library(duckdb)
library(R.utils)

source("scripts/functions/download_inmet.R")

#############################################################################
#############################################################################

anos_eleicao <- seq(2008, 2024, by = 2)

download_inmet(years = anos_eleicao, unzip_path = "./data/raw/inmet/")

con <- dbConnect(
  duckdb(),
  dbdir = "data/processed/INMET"
)

lista_df <- list()
for (ano_ in anos_eleicao){

  print(ano_)
  
  for (x in list.files(glue::glue("./data/raw/inmet/{ano_}/"), full.names = TRUE)) {

    df <- data.table::fread(x, nrows = 6) |>
      pivot_wider(names_from = 1, values_from = 2) |>
      janitor::clean_names() |>
      select(codigo_wmo) |>
      cross_join(
        data.table::fread(x, skip = 7, sep = ";", encoding = "Latin-1") |>
          janitor::clean_names() |>
          mutate(
            across(3:19, ~
                     stringi::stri_replace(.x, regex = "[,]",replacement = ".") |>
                     as.numeric()
            )
          )
      ) |>
      mutate(ano = ano_)
  
    if ("data" %in% c(colnames(df))){
      df <- df |>
      rename(data_yyyy_mm_dd = data)
    }
    
    if ("data" %in% c(colnames(df))){
      df <- df |>
        rename(data_de_fundacao = data)
    }
    
    lista_df[[as.character(ano_)]][[x]] <- df
    rm(df)
  }
  
  rainfall_ano <- lista_df[[as.character(ano_)]] |>
    bind_rows()
  
  cols_db <- dbListFields(con, "rainfall")
  rainfall_ano_filtrado <- rainfall_ano[, intersect(names(rainfall_ano), cols_db)]
  rainfall_ano_filtrado <- rainfall_ano_filtrado[, cols_db]
  
  dbWriteTable(con, "rainfall", rainfall_ano_filtrado, append = TRUE)
  
  lista_df <- list()
  rm(rainfall_ano)
}

dbDisconnect(con)
