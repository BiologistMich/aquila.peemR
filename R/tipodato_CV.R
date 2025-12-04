#' @author Biol. Michell Romero - Coordinador del área de Monitoreo de aves TCT 🦉
#' @title Asignación del tipo de dato a cada una de las variables de la hoja CV
#' @description
#' Aplica a cada columna el tipo de dato que le corresponde, integer, string, etc.
#' @param lista_archivos lista con dataframes de distintos proyectos de una hoja (ej: PCTS_PEEM_OAX).
#' @return El mismo dataframe pero con el tipo de dato correcto.
#' @export
#' @importFrom dplyr mutate across
#' @importFrom magrittr %>%
#' @importFrom duckplyr as_duckplyr_df
tipo_dato.CV <-function(lista_archivos) {

lapply(lista_archivos, function(df) {

     df %>% 
       duckplyr::as_duckplyr_df() |> 
       dplyr::mutate(
        dplyr::across(
      .cols = c("Percha_Linea", "Ind_percha_linea", "Deslizamiento", "Cicleo", 
                "Directo", "Otro", "Forrajeo", "Registros", "Altura_m", 
                "Distancia_WTG_LT", "Percha_Torre", "Distancia_WTG_LT", 
                "Percha_Torre"),
      .fns = as.integer  
    ),
    dplyr::across(
      .cols= c("Observaciones","Colision_Electrocu", "DV", "Categoria_RV"),
      .fns = as.character
    )
  )
  })
}