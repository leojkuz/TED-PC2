#----------------------------------#
#      Practica Calificada 2       #    
#----------------------------------#

# Para limpiar el workspace, por si hubiera algún dataset 
# o información cargada
rm(list = ls())

# Para limpiar el área de gráficos
graphics.off()

# Limpiar la consola
cat("\014")

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Paquetes y lectura de datos ---------------- 
library(pacman) # Package Manager
p_load(ggplot2, dplyr, lubridate, readr, readxl)

# Leer el archivo CSV
data <- read_excel("data-models.xlsx")

# Regresión lineal para estimar valores faltantes ------------

predict_value <- function(target_var, predictor_vars, data) {
  # Filtrar datos para entrenar (excluyendo filas con NA en las variables relevantes)
  train_data <- data[complete.cases(data[, c(target_var, predictor_vars)]), ]
  
  # Crear fórmula para la regresión
  formula <- as.formula(paste(target_var, "~", paste(predictor_vars, collapse = " + ")))
  
  # Ajustar modelo
  model <- lm(formula, data = train_data)
  
  # Realizar predicción
  prediction <- predict(model, newdata = data[data$Modelo == "Claude 2", predictor_vars, drop = FALSE])
  
  return(prediction)
}

# Primero predecir GPQA usando MMLU y HumanEval
gpqa_pred <- predict_value("GPQA", c("MMLU", "HumanEval"), data)
data$GPQA[data$Modelo == "Claude 2"] <- gpqa_pred

# Luego predecir MATH usando GPQA, MMLU y HumanEval
math_pred <- predict_value("MATH", c("GPQA", "MMLU", "HumanEval"), data)
data$MATH[data$Modelo == "Claude 2"] <- math_pred

# Ajustando el dataframe ------
data$Fecha <- as.Date(data$Fecha, format = "%d/%m/%Y")
data <- data %>% mutate(Promedio = rowMeans(select(., c(GPQA, MMLU, HumanEval, MATH))))




# GRAFICO ------
library(tidyverse)
library(ggtext)
library(showtext)
library(htmltools)
library(grid)
library(scales)

# Agregar fuentes de Google
font_add_google("Roboto", "Roboto")
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

# Crear título con HTML para estilos personalizados
title <- tagList(
  p("Evolución de modelos de Lenguaje IA:",
    span("2023-2024", style="color:#666666;font-size:16px;")
  )
)

# Crear subtítulo con formato
subtitle <- "Seguimiento de las mejoras de rendimiento de los principales modelos de lenguaje de IA a lo largo del tiempo. <br>
<span style='color:#666666;'>Las puntuaciones representan puntos de referencia promediados que incluyen MMLU, GPQA, MATH y HumanEval.</span>"

# Crear nota al pie
caption <- paste0(
  "<span style='font-family:Roboto;'>Source: Evaluaciones de modelos públicas en la web</span><br>",
  "<span style='font-family:Roboto;color:#666666;'>Note: Se ha excluido el modelo Gemini 1.5 Pro-002 por ser un modelo experimental</span>"
)

# Crear el gráfico
ggplot(data, aes(x = Fecha, y = Promedio, color = Familia, group = Familia)) +
  # Agregar área sombreada bajo las líneas
  geom_step(aes(fill = Familia), alpha = 0.1, direction = "hv") +
  
  # Agregar las líneas escalonadas
  geom_step(size = 1.2, direction = "hv") +
  
  # Agregar puntos
  geom_point(size = 1.5, shape = 21, fill = "white", stroke = 1.5) +
  
  # Agregar etiquetas para los modelos importantes
  geom_text(data, mapping = aes(label = Modelo),
            hjust = 0.6, 
            vjust = -1,
            family = "Roboto Condensed",
            fontface = "bold",
            size = 3.5) +
  
  # Establecer colores personalizados
  scale_color_manual(values = c(
    "Claude" = "#FF6B6B",
    "GPT" = "#4ECDC4",
    "Gemini" = "#45B7D1"
  )) +
  
  scale_fill_manual(values = c(
    "Claude" = "#FF6B6B",
    "GPT" = "#4ECDC4",
    "Gemini" = "#45B7D1"
  )) +
  
  # Configurar escalas
  scale_y_continuous(
    limits = c(50, 90),
    breaks = seq(50, 90, 10),
    labels = function(x) paste0(x, ""),
    expand = expansion(mult = c(0.02, 0.1))
  ) +
  
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b '%y",
    expand = expansion(mult = c(0.02, 0.1))
  ) +
  
  # Configurar etiquetas
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = "",
    y = "Benchmark Score Average"
  ) +
  
  # Tema personalizado
  theme_minimal() +
  theme(
    # Texto y títulos
    text = element_text(family = "Roboto", color = "#2c3e50"),
    plot.title = element_textbox_simple(
      face = "bold",
      size = 20,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_textbox_simple(
      family = "Roboto",
      size = 11,
      margin = margin(b = 20)
    ),
    plot.caption = element_textbox_simple(
      color = "#666666",
      size = 8,
      margin = margin(t = 20)
    ),
    
    # Ejes
    axis.title.y = element_text(
      margin = margin(r = 10),
      size = 10
    ),
    axis.text = element_text(size = 9),
    
    # Leyenda
    legend.position = "none",
    
    # Grilla
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#ededed"),
    
    # Márgenes
    plot.margin = margin(20, 40, 20, 20),
    
    # Otros elementos
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Agregar líneas diagonales para indicar eje quebrado
  annotate("segment", 
           x = as.Date("2023-01-15"), 
           xend = as.Date("2023-01-25"),
           y = 48, 
           yend = 52,
           color = "gray70",
           size = 0.5) +
  annotate("segment", 
           x = as.Date("2023-01-20"),
           xend = as.Date("2023-01-30"),
           y = 47,
           yend = 51,
           color = "gray70",
           size = 0.5)










# Crear el gráfico
# 
# ggplot(data, aes(x = Fecha, y = Promedio, color = Familia, group = Familia)) +
#   geom_step(size = 1, direction = "hv") +  
#   geom_point(size = 3) +                   # Puntos más grandes para mayor visibilidad
#   labs(title = "AI Model Release and Capabilities Timeline",
#        x = "Release Date",
#        y = "Benchmark Scores",
#        color = "") +
#   theme_minimal() +
#   scale_color_manual(values = c(
#     "Claude" = "#FF7F7F",    # Rojo coral para Claude
#     "GPT" = "#B8860B",       # Marrón dorado para GPT
#     "Gemini" = "#87CEEB"     # Azul cielo para Gemini
#   )) +
#   theme(
#     plot.title = element_text(size = 16, face = "bold"),
#     axis.title = element_text(size = 12),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     legend.position = "bottom",
#     panel.grid.minor = element_blank(),
#     panel.grid.major = element_line(color = "gray90")
#   ) +
#   scale_y_continuous(limits = c(50, 90), breaks = seq(50, 90, by = 10)) +
#   scale_x_date(
#     date_breaks = "2 months",
#     date_labels = "%b '%y"
#   )
