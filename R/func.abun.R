#' @author Biol. Michell Ivann Romero Pacheco
#' @title Cálcular abundancias, actividades estratos y diversidad
#' @description
#' Aplica calculos de abundancia relativa de cada especie, actividades, diversidad y estratos
#' @param PC_lista lista con dataframes de distintos proyectos de una hoja (ej: PCTS_PEEM_OAX).
#' @return Archivos xlsx con los reusltados por proyecto
#' @export
#' @importFrom dplyr filter select group_by summarise mutate arrange n_distinct
#' @importFrom janitor adorn_totals
#' @importFrom tidyr pivot_longer
#' @importFrom vegan diversity specnumber
#' @importFrom openxlsx write.xlsx
#' @importFrom magrittr %>%
calc.abun <- function(PC_lista) {

  # Función interna aplicada a cada dataframe (proyecto) dentro de la lista
  lapply(PC_lista, function(df) {

    # ------------------------------------------------------------------
    # 1. Datos de abundancia por especie y cálculo del IAR
    # ------------------------------------------------------------------
    abun <- df %>%
      tidyverse:: select(Especie, Individuos) %>%
      tidyverse:: filter(Especie != "Z sin registro") %>%
      tidyverse:: group_by(Especie) %>%
      tidyverse:: summarise(Abundancias = sum(Individuos, na.rm = TRUE), .groups = "drop") %>%
      tidyverse:: mutate(IAR = Abundancias / sum(Abundancias) * 100) %>%
      tidyverse:: mutate(IAR = round(IAR,2)) %>%
      tidyverse:: arrange(tidyverse::desc(Abundancias)) %>%
      janitor:: adorn_totals(where = "row")

    # ------------------------------------------------------------------
    # 2. Datos de abundancia por actividad (Cortejo, Forrajeo, Percha, Vocalización, Vuelo)
    # ------------------------------------------------------------------
    actividad <- df %>%
      tidyverse::select(Especie, Cortejo, Forrajeo, Percha, Vocalizacion, Vuelo) %>%
      tidyverse:: filter(Especie != "Z sin registro") %>%
      tidyr::pivot_longer(
        cols = -Especie, # todas las columnas
        names_to = "Actividad",
        values_to = "Individuos"
      ) %>%
      tidyverse::filter(Individuos > 0) %>%
      tidyverse::group_by(Actividad) %>%
      tidyverse::summarise(Abundancias = sum(Individuos, na.rm = TRUE),
            Especies = tidyverse:: n_distinct(Especie),
            .groups = "drop"
          ) %>%
      janitor::adorn_totals(where = "row")

    # ------------------------------------------------------------------
    # 3. Datos de abundancia por estratos
    # ------------------------------------------------------------------
    estratos <- df %>%
      tidyverse::select(Especie, Arboreo, Arbustivo, Herbaceo, Suelo, Agua, Estructura, Aereo) %>%
      tidyverse:: filter(Especie != "Z sin registro") %>%
      tidyr::pivot_longer(
        cols = c(Arboreo, Arbustivo, Herbaceo, Suelo, Agua, Estructura, Aereo),
        names_to = "Estrato",
        values_to = "Individuos"
      ) %>%
      tidyverse::filter(Individuos > 0) %>%
      tidyverse::group_by(Estrato) %>%
      tidyverse::summarise(Abundancias = sum(Individuos, na.rm = TRUE), 
                       Especies = tidyverse:: n_distinct(Especie),
                       .groups = "drop") %>%
      janitor::adorn_totals(where = "row")

    # ------------------------------------------------------------------
    # 4. Cálculo de índices de diversidad
    # ------------------------------------------------------------------
    diversidad_data <- df %>%
      tidyverse::select(Metodo, Codigo_metodo, Especie, Individuos) %>%
      tidyverse:: filter(Especie != "Z sin registro") %>%
      tidyverse::filter(Metodo != "INC") %>%
      tidyverse::select(-Metodo) %>%
      tidyverse::group_by(Codigo_metodo, Especie) %>%
      tidyverse::summarise(Individuos = sum(Individuos, na.rm = TRUE), .groups = "drop") %>%
      tidyr::pivot_wider(
        id_cols = Codigo_metodo,
        names_from = Especie,
        values_from = Individuos,
        values_fill = 0
      )

    # 1. Extraer los IDs de muestra ANTES de quitar la columna de la matriz de conteo
    ids_muestra <- diversidad_data$Codigo_metodo

    # 2. Crear la Matriz de Conteos sin la columna ID
    matriz_conteo <- diversidad_data %>%
      tidyverse::select(-Codigo_metodo)

    # 3. Cálculo de los índices
    Shannon <- vegan::diversity(matriz_conteo, index = "shannon")
    GiniSimpson <- vegan::diversity(matriz_conteo, index = "simpson") # D o Gini-Simpson
    Riqueza <- vegan::specnumber(matriz_conteo)
    Pielou <- ifelse(Riqueza > 0, Shannon / log(Riqueza), 0)

    # 4. Creación de la tabla final con los Codigo_metodo como COLUMNAS
    indices <- data.frame(
      Shannon = Shannon,
      GiniSimpson = GiniSimpson,
      D = 1 - GiniSimpson, # Asumiendo que D es el índice de Simpson inverso
      Pielou = Pielou,
      Riqueza = Riqueza
    )
    indices_final_t <- t(indices) # Transponer la tabla
    indices_final <- data.frame(
      Indice = rownames(indices_final_t),
      indices_final_t,
      row.names = NULL
    )

    # 5. Asignar los Codigo_metodo como nombres de las COLUMNAS
    colnames(indices_final)[2:ncol(indices_final)] <- ids_muestra

    # ------------------------------------------------------------------
    # 5. Guardar resultados en un archivo Excel por proyecto
    # ------------------------------------------------------------------
    nombre_archivo <- paste0("Resul_PCTS_",
                             unique(df$Proyecto)[1], ".xlsx")

     openxlsx::write.xlsx(
      list(
        abundancia = abun,
        actividad = actividad,
        estratos = estratos,
        indices_diversidad = indices_final),
      file = nombre_archivo,
      overwrite = TRUE
    )

    return(nombre_archivo)

  }) # cierre de lapply
} # cierre de función calc.abun
