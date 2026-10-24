
library(tidyverse)
library(meta)

options(scipen = 999, digits = 5)

literatura <- read.csv2("reference/chuva_literatura.csv") |> 
  mutate(
    Efeito_padronizado = ifelse(Medida == "Polegada", Efeito/25.4, Efeito),
    Efeito_padronizado = ifelse(Medida == "Polegada Mil", (Efeito * 100)/25.4, Efeito_padronizado),
    SE_padronizado = ifelse(Medida == "Polegada", SE/25.4, SE),
    SE_padronizado = ifelse(Medida == "Polegada Mil", SE/25.4, SE_padronizado),
    ci_low  = Efeito_padronizado - (SE_padronizado * 1.96),
    ci_high = Efeito_padronizado + (SE_padronizado * 1.96),
    Autor = Autor |> 
      stringi::stri_replace(regex = "  ", replacement = " ")
    #Autor = ifelse(stringi::stri_detect(Autor, regex = "(.*?)Presente Estudo(.*?)"), yes = paste0("<b>", Autor, "</b>"), Autor),
    #Seleção = ifelse(Seleção == "Brasil", yes = paste0("<b>", Seleção, "</b>"), Seleção)
  ) |> 
  arrange(desc(Efeito_padronizado))

m.gen <- metagen(
  data = literatura,
  TE = Efeito_padronizado,
  seTE = SE_padronizado,
  studlab = Autor,
  sm = "SMD",
  fixed = FALSE,
  random = TRUE,
  method.tau = "REML",
  method.random.ci = "HK"
)

png(
  filename = "plots/plot_rev_lit.png",
  width = 8,
  height = 5,
  units = "in",
  res = 200
)

forest(
  m.gen,
  sortvar = TE,
  prediction = TRUE,
  print.tau2 = FALSE,
  print.I2 = TRUE,
  print.pval.Q = TRUE,
  leftlabs = c("Autores", "g", "EP"),
  rightlabs = c("IC 95%"),
  smlab = "Efeito em Milímetros",
  xlim = c(-0.3, 0.1),
  refline = 0,
  col.square = "grey30",
  col.diamond = "steelblue",
  col.predict = "red",
  fontsize = 10
)

dev.off()

#plot_lit <- 
# literatura |> 
#   mutate(
#     Autor = ifelse(stringi::stri_detect(Autor, regex = "(.*?)Presente Estudo(.*?)"), yes = paste0("<b>", Autor, "</b>"), Autor),
#     Seleção = ifelse(Seleção == "Brasil", yes = paste0("<b>", Seleção, "</b>"), Seleção)
#   ) |> 
#   ggplot(aes(x = Efeito_padronizado, y = reorder(Autor, -Efeito_padronizado))) + 
#   geom_errorbar(aes(xmin = ci_low, xmax = ci_high), width = 0.5, color = "black") +
#   geom_point(size = 5, shape = 18, color = "black") +
#   geom_vline(xintercept = 0, linetype = "dashed") +
#   geom_vline(xintercept = mean(literatura$Efeito_padronizado), linetype = "dotdash", color = "firebrick2") +
#   theme_linedraw() +
#   labs(
#     x = "Efeito em Milímetros",
#     y = NULL,
#     color = NULL
#   ) +
#   coord_cartesian(xlim = c(-0.3, 0.1)) +
#   scale_y_discrete(
#     sec.axis = dup_axis(
#       labels = literatura$Seleção
#     )
#   ) +
#   theme(
#     legend.position = "bottom",
#     axis.title.y = element_text(size = 15),
#     axis.title.x = element_text(size = 15),
#     axis.text.x  = element_text(size = 12),
#     axis.text.y  = element_markdown(size = 13),
#     axis.text.y.right = element_markdown(size = 13),
#     legend.text  = element_text(size = 15)
#   )

#ggsave(plot = plot_lit, filename = "plots/plot_rev_lit.png", width = 12, height = 7)
