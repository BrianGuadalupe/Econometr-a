# Econometria
Este trabajo analiza el conjunto de datos Hitters, que contiene estadísticas de jugadores de béisbol  y sus salarios, para identificar las variables más relevantes en la predicción de los ingresos y  comparar diferentes métodos de modelado.

#Correcciones: 
- Preguntas 3 y 4: se debería haber incluido una tabla con los principales resultados (coeficientes, p-valor, R2 ajustado, test F) para comparar entre los dos modelos.

- Pregunta 5: Según veo en R se aplica el método Backward, y se pide el exhaustivo.

- Preguntas 5, 6, 7, 8, 10,11, 12, 13, 14 y 15: los valores del RMSE del trabajo no coinciden con los valores calculados en R.

- Pregunta 10: No se elije lambda por vc.

# Trabajo Final de Econometría para Datos Masivos

Este repositorio contiene el análisis y los resultados del trabajo final desarrollado como parte del **Máster Universitari en Anàlisi de Dades Massives en Economia i Empresa** de la **Universitat de les Illes Balears (UIB)**. El estudio se centra en el análisis y modelado de datos estadísticos de jugadores de béisbol profesional utilizando enfoques econométricos avanzados.

## Objetivos

1. Identificar las variables más relevantes para predecir los salarios de jugadores de béisbol.
2. Comparar diferentes enfoques de modelado, incluyendo:
   - Regresión Lineal Múltiple
   - Modelos Regularizados (LASSO y Ridge)
   - Métodos basados en Componentes Principales (PCA y PLS)
3. Evaluar la precisión predictiva mediante métricas como el Error Cuadrático Medio (RMSE).

## Resumen del Estudio

El conjunto de datos utilizado es **Hitters**, que contiene estadísticas de jugadores de béisbol profesional de la Major League Baseball (MLB). Este incluye información como turnos al bate, hits, home runs, carreras impulsadas y salarios, entre otros.

El análisis consistió en:
- Exploración inicial de datos con herramientas visuales, como mapas de calor de correlaciones.
- Ajuste de modelos econométricos utilizando múltiples enfoques.
- Evaluación de modelos mediante validación cruzada.

### Resultados Principales

| Modelo                        | RMSE (10 validaciones) | RMSE (5 validaciones) |
|-------------------------------|------------------------|------------------------|
| Selección por pasos hacia adelante | **324.36**              | 338.60                 |
| PCA                          | 333.29                | 339.29                 |
| PLS                          | 332.06                | **332.06**             |
| Ridge                        | 335.70                | 335.70                 |
| LASSO                        | 336.36                | 335.00                 |

**Conclusión:** El modelo de selección por pasos hacia adelante con validación cruzada de 10 particiones ofrece la mejor precisión predictiva con un RMSE de 324.36.

## Estructura del Repositorio

- `Trabajo_Final.pdf`: Informe académico completo que incluye descripción, análisis y resultados.
- `Código_R_Trabajo_final.R`: Código fuente en R utilizado para el análisis.
- `README.md`: Este archivo explicativo.

## Herramientas Utilizadas

- Lenguaje: **R**
- Bibliotecas: `caret`, `glmnet`, `leaps`, `corrplot`

## Reproducción del Análisis

1. **Clona el repositorio:**
   ```bash
   git clone https://github.com/BrianGuadalupe/WorkDaily
   ```
2. **Abre los scripts en la carpeta `/src` y ejecútalos en R.**
3. **Revisa los resultados y gráficos en la carpeta `/plots`.**

## Créditos

Este trabajo fue realizado por **Brian Abad Guadalupe** como parte del Máster Universitario en Anàlisi de Dades Massives en Economia i Empresa de la Universitat de les Illes Balears.
