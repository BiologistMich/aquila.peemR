#' @author Biol. Michell Romero - Coordinador del área de Monitoreo de aves TCT 🦉
#' @title Asignación del tipo de dato a cada una de las variables de la hoja PC_TS
#' @description
#' Aplica a cada columna el tipo de dato que le corresponde, integer, string, etc.
#' @param lista_archivos lista con dataframes de distintos proyectos de una hoja (ej: PCTS_PEEM_OAX).
#' @return El mismo dataframe pero con el tipo de dato correcto.
#' @export
#' @importFrom dplyr mutate across
#' @importFrom magrittr %>%
#' @importFrom duckplyr as_duckplyr_df
tipo_dato.PCTS <-function(lista_archivos) {

lapply(lista_archivos, function(df) {

     df %>% 
       duckplyr::as_duckplyr_df() |> 
       dplyr::mutate(
        dplyr::across(
      .cols = c("Cortejo", "Forrajeo", "Percha", "Vocalizacion", "Vuelo", 
                "Individuos", "Arboreo", "Arbustivo", "Herbaceo", "Suelo", 
                "Rocas", "Agua", "Estructura", "Aereo", "Voc", 
                "Sustratos_registros"),
      .fns = as.integer  
    ))
  })
}
  