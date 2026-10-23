# ==============================================================================
# BDA400 - Asignación 2: Technical Analysis using R (etapa preliminar)
# Autora: Alba Avila
# Archivo: A02_analisis.R
# Contenido: carga de datos, estadísticas y funciones para mostrar resultados
#
# Antes de ejecutar:
#   1. Guarda portfolio.txt, A02_funciones.R y este archivo en la MISMA carpeta.
#   2. En RStudio: Session > Set Working Directory > To Source File Location
# ==============================================================================

library(quantmod)
library(TTR)
library(ggplot2)

source("A02_funciones.R")

# ------------------------------------------------------------------------------
# Funciones para mostrar resultados
# ------------------------------------------------------------------------------

# Muestra el data frame de una acción: estructura, primeras y últimas filas
display_stock_data <- function(df, symbol, n = 6) {
  cat("\n==============================================\n")
  cat(" Datos de", symbol, "\n")
  cat("==============================================\n")
  cat("Periodo:", format(min(df$Date)), "a", format(max(df$Date)),
      "| Filas:", nrow(df), "\n\n")
  cat("Primeras", n, "filas:\n")
  print(head(df, n), row.names = FALSE)
  cat("\nÚltimas", n, "filas:\n")
  print(tail(df, n), row.names = FALSE)
}

# Muestra la tabla de estadísticas calculadas
display_statistics <- function(summary_table) {
  cat("\n==============================================\n")
  cat(" Estadísticas del precio de cierre (Close)\n")
  cat("==============================================\n")
  print(format(summary_table, digits = 5, nsmall = 2), row.names = FALSE)
}

# Gráfica: precio de cierre con promedio móvil
plot_close_with_ma <- function(ma_df, symbol, ma_window) {
  ggplot(ma_df, aes(x = Date)) +
    geom_line(aes(y = Close, color = "Cierre"), linewidth = 0.6) +
    geom_line(aes(y = SMA, color = paste("Promedio móvil", ma_window)),
              linewidth = 0.9, na.rm = TRUE) +
    labs(title = paste(symbol, "- Precio de cierre y promedio móvil"),
         x = "Fecha", y = "Precio (USD)", color = "Serie") +
    theme_minimal(base_family = "sans", base_size = 12)
}

# Gráfica: volumen negociado
plot_volume <- function(df, symbol) {
  ggplot(df, aes(x = Date, y = Volume)) +
    geom_col(fill = "steelblue") +
    labs(title = paste(symbol, "- Volumen negociado"),
         x = "Fecha", y = "Volumen") +
    theme_minimal(base_family = "sans", base_size = 12)
}

# Gráfica: histograma del precio de cierre con media y mediana
plot_close_histogram <- function(df, symbol, stats_row) {
  ggplot(df, aes(x = Close)) +
    geom_histogram(bins = 30, fill = "grey70", color = "white") +
    geom_vline(xintercept = stats_row$Media, color = "red", linewidth = 1) +
    geom_vline(xintercept = stats_row$Mediana, color = "blue",
               linewidth = 1, linetype = "dashed") +
    labs(title = paste(symbol, "- Distribución del cierre (rojo: media, azul: mediana)"),
         x = "Precio de cierre (USD)", y = "Frecuencia") +
    theme_minimal(base_family = "sans", base_size = 12)
}

# Gráfica de velas con quantmod (últimos 90 días)
plot_candles <- function(df, symbol) {
  xts_data <- xts::xts(df[, c("Open", "High", "Low", "Close", "Volume")],
                       order.by = df$Date)
  chartSeries(xts_data, name = symbol, theme = chartTheme("white"),
              subset = "last 90 days", TA = "addVo()")
}

# ------------------------------------------------------------------------------
# Ejecución principal
# ------------------------------------------------------------------------------
MA_WINDOW <- 20   # días del promedio móvil

# 1. Importar datos (lee portfolio.txt)
stocks <- load_stock_data("portfolio.txt", from = "2024-01-01")

# 2. Calcular estadísticas
results <- calculate_all_statistics(stocks, ma_window = MA_WINDOW)

# 3. Mostrar datos y estadísticas
for (sym in names(stocks)) {
  display_stock_data(stocks[[sym]], sym)
}
display_statistics(results$summary_table)

# 4. Visualizaciones
for (sym in names(stocks)) {
  stats_row <- results$summary_table[results$summary_table$Simbolo == sym, ]
  print(plot_close_with_ma(results$by_stock[[sym]]$moving_average, sym, MA_WINDOW))
  print(plot_volume(stocks[[sym]], sym))
  print(plot_close_histogram(stocks[[sym]], sym, stats_row))
  plot_candles(stocks[[sym]], sym)
}
