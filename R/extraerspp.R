#' @author Biol. Michell Romero - Coordinador del área de Monitoreo de aves TCT 🦉
#' @title Función para extraer información de spp de un listado potencial 
#' @description
#' Extraer las especies únicas de cada hoja o df y las usa para filtrar la información de un inventario general.
#' @param lista_hojas lista con dataframes de donde se obtendrán las spp de consulta.
#' @param inventario Inventario limpio (clean_names()) con información de las spp objetivo y sus variables en columnas (p.ej. NOM059, IUCN, CITES).
#' @return Las especies consultadas con toda la información del inventario (columnas).
#' @export
#' @importFrom dplyr select filter distinct arrange pull 
#' @importFrom janitor clean_names
extraer_spp <- function(lista_hojas, inventario) {   

  lista_de_listas_spp <- lapply(lista_hojas, function(hoja) {
  
      hoja |> 
      janitor::clean_names() |> 
      dplyr::select(especie) |> 
      dplyr::filter(!(especie %in% c("S/R", "NA", "Z sin registro"))) |> 
      dplyr::distinct(especie) |> 
      dplyr::arrange(especie) |> 
      dplyr::pull(especie)
})
  especies_totales <- unlist(lista_de_listas_spp) |> 
        unique()
  
 taxo <- inventario |> 
  dplyr::filter(especie %in% especies_totales)
  
    return(taxo)
}