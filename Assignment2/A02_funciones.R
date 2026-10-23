# ==============================================================================
# BDA400 - Asignación 2: Technical Analysis using R (etapa preliminar)
# Autora: Alba Avila
# Archivo: A02_funciones.R
# Contenido: funciones para importar datos de acciones y calcular estadísticas
# ==============================================================================

library(quantmod)
library(TTR)

# ------------------------------------------------------------------------------
# 1. load_stock_data()
#    Lee los símbolos de portfolio.txt (uno por línea) y descarga los datos de
#    cada acción con quantmod (fuente: Yahoo Finance).
#    Devuelve una lista con un data frame por símbolo. Si assign_global = TRUE,
#    también crea un data frame separado en el entorno global (por ejemplo, MSFT).
# ------------------------------------------------------------------------------
load_stock_data <- function(file = "portfolio.txt",
                            from = "2024-01-01",
                            to = Sys.Date(),
                            assign_global = TRUE) {

  if (!file.exists(file)) {
    stop("No se encontró el archivo '", file,
         "'. Revisa el directorio de trabajo con getwd().")
  }

  # Leer símbolos: quitar espacios, líneas vacías y comentarios (#)
  symbols <- trimws(readLines(file, warn = FALSE))
  symbols <- symbols[symbols != "" & !startsWith(symbols, "#")]

  stock_list <- list()

  for (sym in symbols) {
    data_xts <- tryCatch(
      getSymbols(sym, src = "yahoo", from = from, to = to, auto.assign = FALSE),
      error = function(e) {
        warning("No se pudieron descargar los datos de ", sym, ": ", e$message)
        NULL
      }
    )
    if (is.null(data_xts)) next

    # Convertir xts a data frame con columna Date y nombres simples
    df <- data.frame(Date = index(data_xts), coredata(data_xts))
    names(df) <- c("Date", sub("^.*\\.", "", names(df)[-1]))  # MSFT.Open -> Open
    rownames(df) <- NULL

    stock_list[[sym]] <- df
    if (assign_global) assign(sym, df, envir = .GlobalEnv)
  }

  message("Símbolos cargados: ", paste(names(stock_list), collapse = ", "))
  return(stock_list)
}

# ------------------------------------------------------------------------------
# 2. get_mode()
#    Base R no trae una función para la moda, por eso se define aquí.
#    Si hay empate, devuelve el primer valor más frecuente.
# ------------------------------------------------------------------------------
get_mode <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# ------------------------------------------------------------------------------
# 3. calculate_statistics()
#    Recibe el data frame de una acción y calcula, sobre el precio de cierre
#    (Close): media, mediana, moda, desviación estándar y promedio móvil.
#    La moda se calcula sobre el cierre redondeado a 2 decimales.
#    Devuelve una lista con:
#      $summary         -> data frame de una fila con las estadísticas
#      $moving_average  -> data frame con Date, Close y la columna SMA
# ------------------------------------------------------------------------------
calculate_statistics <- function(df, ma_window = 20) {

  close_price <- df$Close

  sma <- TTR::SMA(close_price, n = ma_window)   # promedio móvil simple

  summary_df <- data.frame(
    Observaciones      = length(close_price),
    Media              = mean(close_price, na.rm = TRUE),
    Mediana            = median(close_price, na.rm = TRUE),
    Moda               = get_mode(round(close_price, 2)),
    Desviacion_Estandar = sd(close_price, na.rm = TRUE),
    Promedio_Movil_Ultimo = tail(sma[!is.na(sma)], 1),
    Ventana_Promedio_Movil = ma_window
  )

  moving_average <- data.frame(Date = df$Date, Close = close_price, SMA = sma)

  list(summary = summary_df, moving_average = moving_average)
}

# ------------------------------------------------------------------------------
# 4. calculate_all_statistics()
#    Aplica calculate_statistics() a cada acción de la lista.
#    Devuelve una lista con:
#      $by_stock       -> resultados completos por símbolo
#      $summary_table  -> tabla comparativa con una fila por símbolo
# ------------------------------------------------------------------------------
calculate_all_statistics <- function(stock_list, ma_window = 20) {

  by_stock <- lapply(stock_list, calculate_statistics, ma_window = ma_window)

  summary_table <- do.call(rbind, lapply(names(by_stock), function(sym) {
    cbind(Simbolo = sym, by_stock[[sym]]$summary)
  }))
  rownames(summary_table) <- NULL

  list(by_stock = by_stock, summary_table = summary_table)
}
