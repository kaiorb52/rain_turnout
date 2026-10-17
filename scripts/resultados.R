
library(tidyverse)
library(R.utils)
library(ggeffects)
library(ggspatial)
library(sf)
library(ggtext)
library(ggshadow)
library(patchwork)

options(scipen = 999)
addTaskCallback(function(...) {set.seed(123);TRUE})

turnout_rain <- readRDS("data/processed/turnout_rain.rds")
mun_stations <- readRDS("data/processed/mun_stations_distance.rds")
rain_stations <- readRDS("data/processed/rain_stations.rds")

#############################################################################
#############################################################################

mun_24 <- geobr::read_municipality()

mun_centroid <- mun_24 |>
  select(code_muni, geom) |>
  mutate(
    ponto = st_point_on_surface(geom),
  ) |>
  st_drop_geometry()

mun_grid_sf <- mun_centroid |>
  st_as_sf() |>
  st_transform(crs = 29193)

#############################################################################
#############################################################################
# MAPAS

mun_geo <- mun_24
states_geo <- geobr::read_state()

go_mun <- mun_geo |>
  filter(abbrev_state %in% c("GO", "DF")) |>
  mutate(
    centro = st_point_on_surface(geom),
    mun_lon = unlist(st_coordinates(centro)[, 1]),
    mun_lat = unlist(st_coordinates(centro)[, 2]),
  ) |>
  select(-centro) |>
  st_drop_geometry()

go_mun_stations <- mun_stations |>
  filter(
    ano == 2024,
    code_muni %in% c(go_mun$code_muni),
    i == 1
  ) 

go_stations <- rain_stations |>
  filter(ano == 2024, uf %in% c("GO", "DF", "MT", "MS")) |>
  select(codigo_wmo, latitude, longitude) |>
  filter(codigo_wmo %in% c(go_mun_stations$codigo_wmo))

go_lines <- go_mun |>
  select(abbrev_state, code_muni, mun_lon, mun_lat) |>
  left_join(go_mun_stations |> select(code_muni, codigo_wmo, distance), by = "code_muni") |>
  left_join(go_stations, by = "codigo_wmo")  |>
  rowwise() |>
  mutate(
    geometry = st_sfc(
      st_linestring(matrix(
        c(mun_lon, mun_lat,
          longitude, latitude),
        ncol = 2,
        byrow = TRUE
      )),
      crs = 4326
    )
  ) |>
  st_as_sf()

map_brasilia <- ggplot() +
  geom_sf(data = mun_geo, fill = NA, size = 0.2, linetype = "dotted") +
  geom_sf(data = states_geo, fill = NA, size = 0.5) +
  geom_sf(data = go_lines, color = "grey15", size = 0.4, alpha = 0.6) +
  geom_point(
    data = go_mun,
    aes(y = mun_lat, x = mun_lon, fill = "Municipios"), 
    shape = 4,
    size = 1.5, color = "firebrick2"
  ) +
  geom_point(
    data = go_stations, 
    aes(y = latitude, x = longitude, fill = "Estações Automáticas"), 
    size = 3, shape = 17
  ) +
  theme_minimal() +
  coord_sf(xlim = c(-54, -45), ylim = c(-20, -12)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme(legend.position = c(0.85, 0.15), legend.background = element_rect(fill = "white")) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering
  )


ggsave(plot = map_brasilia, filename = "plots/map_brasilia_2.png", width = 8, height = 7)

#############################################################################
#############################################################################

mun_geo_rain <- mun_geo |>
  select(code_muni, geom) |> 
  left_join(
    turnout_rain |> 
      select(ano_eleicao, codigo_ibge, precipitacao) |> 
      mutate(
        
        precipitacao_cat = factor(
          case_when(
            precipitacao <= 1 ~ "0-1",
            precipitacao > 1 & precipitacao <= 10 ~ "1-10",
            precipitacao > 10 & precipitacao <= 25 ~ "10-25",
            precipitacao > 25 & precipitacao <= 50 ~ "25-50",
            precipitacao > 50 & precipitacao <= 80 ~ "50-80",
            precipitacao > 80 ~ "Maior que 80"
          ),
          levels = c(
            "0-1",
            "1-10",
            "10-25",
            "25-50",
            "50-80",
            "Maior que 80"
          ),
          ordered = TRUE
        )
      ),
    by = c("code_muni" = "codigo_ibge")
  ) |> 
  filter(!is.na(ano_eleicao))

map <- ggplot() +
  geom_sf(data = mun_geo_rain, aes(fill = precipitacao_cat), color = NA) +
  geom_sf(data = states_geo, fill = NA, size = 0.5) +
  facet_wrap(.~ano_eleicao) +
  scale_fill_manual(
    values = c(
      #"#e5684c",
      "#ea8457",
      #"#fee08b",
      #"#f2e5b6",
      #"#ffffbf",
      #"#f7cf97",
      "#b5c06f",
      "#6e9851",
      "#498445",
      "#20703b",
      #"seagreen4",
      #"seagreen3",
      #"seagreen2"
      #"#164d28",
      "#113d20"
    )
  ) +
  #scale_fill_brewer(palette = "YlGnBu", na.value = "grey30") +
  labs(fill = "Chuva (mm)") +
  theme_void()

ggsave(plot = map, filename = "plots/map.png", width = 8, height = 7)

#############################################################################
#############################################################################

group_comp <- turnout_rain |> 
  group_by(ano_eleicao, tipo_elecao) |> 
  summarise(
    mean_p_comp = mean(p_comp)
  ) 

group_comp_2 <- turnout_rain |> 
  mutate(
    cat_turno = ifelse(nr_turno == 1, "1º Turno", "2º Turno")
  ) |> 
  group_by(ano_eleicao, tipo_elecao, cat_turno) |> 
  summarise(
    mean_p_comp = mean(p_comp)
  ) 

plot_comp <- turnout_rain |>
  mutate(
    cat_turno = ifelse(nr_turno == 1, "1º Turno", "2º Turno")
  ) |> 
  ggplot(aes(x = as.character(ano_eleicao), y = p_comp, fill = as.character(cat_turno))) +
  geom_line(
    aes(x = as.character(ano_eleicao), y = mean_p_comp, group = tipo_elecao, color = tipo_elecao),
    color = "grey25",
    linetype = 'dashed',
    size  = 1.3,
    inherit.aes = FALSE,
    data = group_comp
  ) +
  geom_jitter(
    alpha = 0.03, 
    shape = 21,
    fill  = NA,
    color = "grey25",
    position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.7),
    aes(group = cat_turno)
  ) +
  geom_boxplot(alpha = 0.85, position = "dodge", width = 0.5) +
  shadowtext::geom_shadowtext(
    aes(
      x = as.character(ano_eleicao),
      y = mean_p_comp + 0.5,
      label = paste0(round(mean_p_comp, 1), "%"),
      group = cat_turno,
      fill  = cat_turno
    ),
    color = "white",
    size  = 3.8,
    inherit.aes = FALSE,
    fontface = "bold",
    position = position_dodge(width = 0.5),
    data = group_comp_2
  ) +
  scale_y_continuous(
    breaks = seq(40, 100, by = 10)
  ) +
  scale_x_discrete(breaks = seq(2008, 2024, by = 2) |> as.character()) +
  facet_wrap(.~tipo_elecao, scales = "free_x") +
  theme_linedraw() +
  labs(x = NULL, y = "Comparecimento (%)", fill = NULL) +
  theme(
    legend.position = "bottom"
  ) +
  scale_fill_manual(
    values = c("steelblue4", "#F2AE2E")
  ) + 
  theme(
    axis.title.y = element_text(size = 15),
    axis.text.x  = element_text(size = 15, face = "bold"),
    legend.text  = element_text(size = 13),
    strip.text   = element_text(size = 18) 
  )

ggsave(plot = plot_comp, filename = "plots/plot_comp.png", width = 12, height = 8)

#############################################################################
#############################################################################
# Estatisticas Gerais
## Participação

turnout_rain |>
  filter(tipo_elecao == "Nacionais") |>
  pull(p_comp) |>
  summary()

turnout_rain |>
  filter(tipo_elecao == "Municipais") |>
  pull(p_comp) |>
  summary()

## Chuva

turnout_rain |> 
  nrow()

turnout_rain |>
  mutate(
    chuva_forte = (precipitacao >= 25) |> 
      as.numeric()
  ) |> 
  group_by(chuva_forte) |> 
  summarise(
    n = n()
  )

turnout_rain |>
  pull(precipitacao) |>
  summary()

turnout_rain |>
  filter(tipo_elecao == "Nacionais") |>
  pull(precipitacao) |>
  summary()

turnout_rain |>
  filter(tipo_elecao == "Municipais") |>
  pull(precipitacao) |>
  summary()

#############################################################################
#############################################################################
# MODELOS RE

f <- list(
  "{vd} ~ precipitacao + (1|periodo)",
  "{vd} ~ precipitacao + turno_2 + n_cand_executivo + log(qt_aptos) + area_mun + p_idosos + p_alfabetizados + p_pop_urbano + metro_dummy + capital_uf + regiao + (1|periodo)",
  "{vd} ~ precipitacao + turno_2 + n_cand_executivo + log(qt_aptos) + area_mun + p_idosos + p_alfabetizados + p_pop_urbano + metro_dummy + capital_uf + regiao + (1|periodo/sg_uf)"
)

modelos_nac <- list()
modelos_mun <- list()
for (x in 1:3){
  
  for(vd in c("p_comp")){
    
    modelos_nac[[vd]][[x]] <- lme4::lmer(
      as.formula(glue::glue(f[[x]])),
      data = turnout_rain |>
        filter(tipo_elecao == "Nacionais")
    )
    
    modelos_mun[[vd]][[x]] <- lme4::lmer(
      as.formula(glue::glue(f[[x]])),
      data = turnout_rain |>
        filter(tipo_elecao == "Municipais")
    )
  }
}

##############
##############

make_modelo_df <- function(list_ = list(), index = 2, name = "Municipais"){
  
  list(
    list_[["p_comp"]][[index]]
  ) |>
    map_df(
      ~ {
        dep_var <- all.vars(formula(.x))[1]
        
        #effectsize::standardize_parameters(.x) |>
        broom.mixed::tidy(.x) |> 
          as.data.frame() |>
          mutate(
            variavel_dependente = dep_var
          )
      }
    ) |> 
    filter(term == "precipitacao") |> 
    mutate(
      modelo = name,
      re = ifelse(index == 2, "RE: Eleição", "RE: Eleição/UF")
    )  
  
}

modelos_df <- bind_rows(
  make_modelo_df(
    list_ = modelos_mun
  ),
  make_modelo_df(
    list_ = modelos_mun,
    index = 3
  ),
  make_modelo_df(
    list_ = modelos_nac,
    name  = "Nacionais"
  ),
  make_modelo_df(
    list_ = modelos_nac,
    index = 3, 
    name  = "Nacionais" 
  )
) |> 
  mutate(
    ci_low = estimate - (1.96 * std.error),
    ci_high = estimate + (1.96 * std.error),
    variavel_dependente = case_when(
      variavel_dependente == "p_comp" ~ "Comparecimento (%)",
      variavel_dependente == "p_brancos" ~ "Votos Brancos (%)",
      variavel_dependente == "p_nulos" ~ "Votos Nulos (%)"
    )
  ) 

plot_models <- modelos_df |> 
  ggplot(aes(y = re, x = estimate, fill = modelo, color = modelo)) +
  geom_errorbar(size = 1.25, aes(xmin = ci_low, xmax = ci_high), width = 0.25, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.85) + 
  geom_text(size = 4.2, color = "black", aes(label = paste0("β: ",round(estimate, 4))), , position = position_dodge(width = 1.1)) +
  geom_point(aes(shape = modelo), size = 6, color = "white", position = position_dodge(width = 0.5), stroke = 1) +
  theme_linedraw() +
  scale_x_continuous(breaks = seq(-0.1, 0.1, by = 0.1), limits = c(-0.115, 0.115)) +
  scale_shape_manual(
    values = c(22, 24)
  ) +
  scale_color_manual(
    values = c("black", "grey30")
  ) +
  scale_fill_manual(
    values = c("black", "grey30")
  ) +
  labs(
    x     = "Efeito da chuva(mm)",
    y     = NULL,
    color = NULL,
    fill  = NULL,
    shape = NULL
  ) +
  theme(
    legend.position = c(0.8, 0.2),
    legend.background = element_blank(),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 13),
    strip.text   = element_text(size = 18), 
    legend.text  = element_text(size = 15)
  )


ggsave(plot = plot_models, filename = "plots/plot_models.png", width = 12, height = 7)

##########################################
##########################################
# MODELOS FE - Desing de antes e depois com controle e tratamento

### Gambiarra
fazer_modelos_fe <- function(a = 2010, corte = 30){
  
  turnout_ano <- turnout_rain |>
    filter(
      ano_eleicao == a
    ) |> 
    mutate(
      chuva_dummy = case_when(
        precipitacao > corte ~ 1,
        precipitacao < 1  ~ 0,
        TRUE ~ NA
      ),
      chuva_t1_dummy = ifelse(chuva_dummy == 1 & turno_2 == 0, 1, 0)
    ) |> 
    group_by(cd_municipio) |> 
    mutate(
      tratamento = max(chuva_dummy),
      tratamento_preciso = max(chuva_t1_dummy)
    ) |> 
    filter(!is.na(tratamento)) |> 
    filter(tratamento_preciso == 0) |> 
    filter(!is.na(p_idosos))
  
  pareamento <- MatchIt::matchit(
    tratamento ~ qt_aptos + area_mun + p_idosos + p_alfabetizados + p_pop_urbano,
    data = turnout_ano |> 
      select(-distance) |> 
      filter(turno_2 == 0)
  )
  
  pareamento_id <- pareamento |> 
    MatchIt::match_data() |> 
    pull(cd_municipio)
  
  pareamento_df <- turnout_ano |> 
    filter(cd_municipio %in% c(pareamento_id)) |> 
    arrange(dt_eleicao) |> 
    select(ano_eleicao, dt_eleicao, nr_turno, turno_2, cd_municipio, qt_aptos, p_comp, precipitacao, chuva_dummy, tratamento) |> 
    mutate(
      diff = turno_2 * tratamento
    )
  
  n_tratamento <- pareamento_df |> 
    arrange(dt_eleicao) |> 
    group_by(nr_turno, tratamento) |> 
    summarise(
      n = n()
    ) |> 
    filter(tratamento == 1, nr_turno == 2) |> 
    pull(n) |> 
    unique()
  
  md_tr_1 <- mean(pareamento_df[pareamento_df$nr_turno == 1,]$p_comp, na.rm = TRUE)
  md_tr_2 <- mean(pareamento_df[pareamento_df$nr_turno == 2,]$p_comp, na.rm = TRUE)
  
  md_chuva <- mean(pareamento_df[pareamento_df$diff == 1,]$precipitacao, na.rm = TRUE)
  
  m <- fixest::feols(
    p_comp ~ diff | nr_turno + cd_municipio,
    data = pareamento_df
  ) |>
    broom::tidy() |>
    mutate(
      ano = a,
      n_tratamento = n_tratamento,
      corte = corte,
      md_tr_1 = md_tr_1,
      md_tr_2 = md_tr_2,
      md_chuva = md_chuva
    )
  
  return(m)  
}

modelos_fe <- list()
for (a in seq(2010, 2022, by = 4)){
  
  for (corte in seq(20, 50, by = 5)){
    modelos_fe[[paste0(as.character(a), "_", corte)]] <- fazer_modelos_fe(a = a, corte = corte)
  }
  
}

modelos_fe_df <- modelos_fe |> 
  bind_rows() |> 
  mutate(
    ci_low  = estimate - (1.96 * std.error),
    ci_high = estimate + (1.96 * std.error)
  )

plot_modelos_fe <- modelos_fe_df |> 
  filter(corte == 30) |> 
  ggplot(aes(y = as.character(ano), x = estimate)) +
  geom_point(size = 8, shape = 18, position = position_dodge(width = 0.5)) +
  geom_errorbar(size = 1, aes(xmin = ci_low, xmax = ci_high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.85) + 
  geom_text(
    aes(label = paste0("β: ", round(estimate, 4))),
    size = 4.2,
    vjust = -2,
    position = position_dodge(width = 0.5)
  ) +
  theme_linedraw() +
  coord_cartesian(xlim = c(-1.9, 1.9)) +
  scale_x_continuous(breaks = seq(-2, 2, by = 0.5)) +
  labs(
    x     = "Efeito do Tratamento(chuva>30)",
    y     = NULL,
    color = NULL
  ) +
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 13),
    strip.text   = element_text(size = 18), 
    legend.text  = element_text(size = 15)
  )

ggsave(plot = plot_modelos_fe, filename = "plots/plot_modelos_fe.png", width = 10, height = 7)

##############################################################################
##############################################################################
# APENDICES

modelsummary::modelsummary(
  c(
    modelos_mun[[1]],
    modelos_nac[[1]]
  ), 
  stars = TRUE
)

plot_modelos_fe2 <- modelos_fe_df |> 
  ggplot(aes(x = estimate, y = as.character(ano))) +
  geom_point(shape = 18, size = 4) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high)) +
  facet_wrap(.~corte, scales = "free_x") +
  coord_cartesian(xlim = c(-2, 2)) +
  theme_linedraw() + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.85) + 
  theme(
    legend.position = c(0.75, 0.15)
  ) +
  labs(x = NULL, y = "Ano do Modelo")

ggsave(plot = plot_modelos_fe2, filename = "plots/plot_modelos_fe2.png", width = 9, height = 7)

plot_modelos_fe3 <- modelos_fe_df |> 
  filter(ano == 2014) |> 
  bind_rows(
    bind_rows(
      fazer_modelos_fe(a = 2014, corte = 55),
      fazer_modelos_fe(a = 2014, corte = 60)
    ) |>
      mutate(
        ci_low  = estimate - (1.96 * std.error),
        ci_high = estimate + (1.96 * std.error)
      )
  ) |>
  ggplot(aes(x = estimate, y = as.character(corte))) +
  geom_point(shape = 18, size = 4) +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high)) +
  coord_cartesian(xlim = c(-2.2, 2.2)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", alpha = 0.85) + 
  geom_text(
    aes(label = paste0("β: ", round(estimate, 4))),
    size = 4.2,
    vjust = -2,
    position = position_dodge(width = 0.5)
  ) +
  theme_linedraw() +
  labs(x = "Efeito do Tratamento", y = "Corte")

ggsave(plot = plot_modelos_fe3, filename = "plots/plot_modelos_fe3.png", width = 9, height = 7)

#############
# TABELA

modelos_fe_df |> 
  filter(corte == 30) |> 
  select(ano, md_tr_1, md_tr_2, md_chuva) |> 
  View()

#############
# EXEMPLO PAREAMENTO

library(rstatix)
library(ggpubr)

plot_list <- list()
for (a_ in seq(2010, 2022, by = 4)) {
  print(a_)
  
  turnout_rain_nv <- turnout_rain |>
    filter(
      ano_eleicao == a_
    ) |> 
    mutate(
      chuva_dummy = case_when(
        precipitacao > 30 ~ 1,
        precipitacao < 1  ~ 0,
        TRUE ~ NA
      ),
      chuva_t1_dummy = ifelse(chuva_dummy == 1 & turno_2 == 0, 1, 0)
    ) |> 
    group_by(cd_municipio) |> 
    mutate(
      tratamento = max(chuva_dummy),
      tratamento_preciso = max(chuva_t1_dummy)
    ) |> 
    filter(!is.na(tratamento)) |> 
    filter(tratamento_preciso == 0) |> 
    filter(!is.na(p_idosos)) |> 
    mutate(
      diff = turno_2 * tratamento
    )
  
  pareamento <- MatchIt::matchit(
    tratamento ~ qt_aptos + area_mun + p_idosos + p_alfabetizados + p_pop_urbano,
    data = turnout_rain_nv |>
      select(-distance) |>
      filter(turno_2 == 0)
  )
  
  pareamento2 <- pareamento |> 
    MatchIt::match_data() |> 
    ungroup() |> 
    select(subclass, p_comp, tratamento) 
  
  stat.test <- pareamento2 %>%
    t_test(p_comp ~ tratamento, paired = TRUE) %>%
    add_significance() %>%
    add_xy_position(x = "tratamento")
  
  plot_list[[as.character(a_)]] <- ggpaired(
    pareamento2, x = "tratamento", y = "p_comp", title = a_,
    #order = c("before", "after"),
    color = "tratamento", palette = c("#3a86d4", "#8338ec"),
    line.color = "gray", line.size = 0.4,
    xlab = "Tratamento", ylab = "Comparecimento",
    ylim = c(60, 99)
  ) +
    stat_pvalue_manual(stat.test, tip.length = 0) +
    labs(subtitle = get_test_label(stat.test, detailed = TRUE))
}

p_t_test <- (plot_list[["2010"]] + plot_list[["2014"]]) / (plot_list[["2018"]] + plot_list[["2022"]])

ggsave(plot = p_t_test, filename = "plots/p_t_test.png", width = 10, height = 8)
