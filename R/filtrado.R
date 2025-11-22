#' @author Biol. Michell Ivann Romero Pacheco
#' @title Filtrado de datos por proyecto y periodos
#' @description
#' Aplica filtrados de datos por proyecto y una fecha en específico
#' @param df Dataframe combinado o único (ej: CV_combinadas, PCTS_combinado).
#' @param proyectos_filtrar Vector de nombres de proyectos a incluir (ej: c("PEEM_PE", "PEEM_LT", "OAX_I", "BH")).
#' @param anual Año de la fecha a filtrar.
#' @param mes Mes de la fecha a filtrar (número).
#' @return Un dataframe, dividido por la columna 'Proyecto'.
#' @export
#' @importFrom dplyr filter select
#' @importFrom lubridate year month
filtrado <- function(df, proyectos_filtrar, anual, mes) {

  datos_filtrados <- df |> 
    dplyr::filter(
      Proyecto %in% proyectos_filtrar,
      lubridate::year(Fecha) == anual,
      lubridate::month(Fecha) == mes
    )
  # Dividir el dataframe filtrado en un data frame
  return(datos_filtrados)
}