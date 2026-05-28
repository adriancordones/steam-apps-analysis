# Limpieza y análisis: Steam Apps Metadata

Análisis estadístico utilizando datos recopilados de aplicaciones publicadas en la tienda de Steam (store.steampowered.com).

### Tabla de contenidos

- [Estructura del repositorio](#estructura-del-repositorio)
- [Integrantes del grupo](#intregantes-del-grupo)
- [Licencia](#licencia)

## Estructura del repositorio

- `.gitignore`: Ficheros y directorios excluidos del control de versiones.
- `LICENSE`: Licencia del proyecto (CC BY-NC-SA 4.0).
- `README.md`: Descripción general del proyecto.
- `source/scraping.R`: Parte del código que realiza la integración de datos.
- `source/cleaning_and_analysis.R`: Parte del código que realiza la limpieza, análisis y visualización.
- `datasets/steam_apps.csv`: Dataset de partida (creado en la Práctica 1).
- `datasets/steam_apps_temp.csv`: Dataset con variables integradas sin limpiar.
- `datasets/steam_apps:cleaned.csv`: Dataset resultante de limpieza y análisis.
- `docs/memoria.pdf`: Memoria del proyecto resultante de ejecutar `docs/memoria.rmd`.

## Integrantes del grupo

Esta práctica fue realizada por **Agustín Barbatelli Balboa** y **Adrián Cordones Martínez**.

## Licencia

Shield: [![CC BY-NC-SA 4.0][cc-by-nc-sa-shield]][cc-by-nc-sa]

Esta obra está bajo licencia [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International][cc-by-nc-sa].

[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]

[cc-by-nc-sa]: https://creativecommons.org/licenses/by-nc-sa/4.0/deed.es
[cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png
[cc-by-nc-sa-shield]: https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg