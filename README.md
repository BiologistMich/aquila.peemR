# 🦅 `aquila.peemR`: Automatización de procesos internos para el monitoreo de aves en proyecto eólico

| ![Logo](https://github.com/BiologistMich/aquila.peemR/blob/main/inst/logo/aquila.peem_portada.png?raw=true) | Este paquete de R contiene un conjunto de funciones diseñadas para estandarizar, procesar y analizar datos generados por un programa de monitoreo biológico en un parque eólico. Su objetivo principal es automatizar procedimientos internos, desde el filtrado de datos crudos hasta el cálculo de índices clave (diversidad, mortalidad, etc.). |
| :---: | :--- |
---

## 📌 Estado del Paquete

| Estatus | Versión | Licencia |
| :---: | :---: | :---: |
| ![lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg) | ![CRAN status](https://www.r-pkg.org/badges/version/aquila.peemR) | ![License](https://img.shields.io/github/license/BiologistMich/aquila.peemR) |

> **Nota:** La librería se encuentra en **fase experimental** y está sujeta a cambios rápidos en las funciones.

---

## 📥 Instalación

El paquete se aloja en GitHub. Para que colegas lo instalen en una computadora diferente, solo se requiere el paquete `devtools` y el nombre del repositorio.

```r
# Instalar devtools (si no lo tienen)
if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
}

# 1. Instalar la librería directamente desde GitHub
# Asegúrate de usar el usuario o la organización correcta (ej: BiologistMich/aquila.peemR)
devtools::install_github("BiologistMich/aquila.peemR")
