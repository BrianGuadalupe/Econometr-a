library(ISLR2) 
library(ggplot2)
library(reshape2)
library(caret)  # Para validación cruzada y preprocesamiento
library(glmnet) # Para modelos lineales regulares (LASSO y Ridge)
library(leaps)
library(corrplot)    # Para crear matrices de correlación visualmente atractivas
library(dplyr) # Cargar y explorar los datos
data("Hitters")
str(Hitters)
summary(Hitters)

# Detectar valores faltantes
anyNA(Hitters)  # Detectar si hay valores faltantes

# Filtrar datos completos (sin NAs)
datos_completos <- na.omit(Hitters)
anyNA(datos_completos)  # Verificar si hay valores faltantes después de na.omit()

# Fijar semilla para reproducibilidad
set.seed(800)

# Ejercicio 1: Análisis exploratorio y matriz de correlación
variables_numericas <- select_if(datos_completos, is.numeric)
matriz_correlacion <- cor(variables_numericas)

# 4. Visualizar la matriz de correlación con un mapa de calor
corrplot(matriz_correlacion, 
         method = "color",      # Usar colores para representar correlaciones
         type = "upper",        # Mostrar solo la mitad superior
         addCoef.col = "black", # Añadir coeficientes en negro
         tl.col = "black",      # Color del texto de las etiquetas
         tl.srt = 45,           # Rotar etiquetas 45 grados
         diag = FALSE)          # No mostrar la diagonal

# 5. Identificar correlaciones altas (por ejemplo, > 0.8)
correlaciones_altas <- which(abs(matriz_correlacion) > 0.8 & 
                               abs(matriz_correlacion) < 1, 
                             arr.ind = TRUE)
print("Variables con alta correlación:")
print(correlaciones_altas)
# Calcular matriz de correlación (solo variables numéricas)
cor_matrix <- cor(datos_completos[, sapply(datos_completos, is.numeric)], use = "complete.obs")

# Visualizar matriz de correlación con corrplot
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.cex = 0.8)

# Visualizar matriz de correlación como heatmap con ggplot2
cor_data <- melt(cor_matrix)
ggplot(cor_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Mapa de Calor de Correlación entre Variables Numéricas", fill = "Correlación") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data("Hitters")
str(Hitters)
colnames(Hitters)  # Confirma los nombres de las columnas



# Decisiones basadas en el análisis de correlación:
# 1. Variables con alta correlación mutua (CAtBat, CHits, CRBI y CRuns): 
#    Excluir todas excepto CHits, que representa bien la información.
# 2. Variables relevantes para Salary: Hits, Years, HmRun, Walks, RBI, y Runs.
# 3. Variables con baja correlación con Salary (Errors, Assists, PutOuts): Excluir estas variables.

# Ejercicio 2: Ajustar modelo de regresión lineal con variables seleccionadas

# Ajustar un modelo de regresión lineal (OLS) con las variables seleccionadas
modelo_ols <- lm(Salary ~ CHits + HmRun + Years + Walks + RBI + Hits + Runs, data = datos_completos)

# Resumen del modelo
summary(modelo_ols)

# Ejercicio 3: Modelo con todas las variables explicativas

# Ajustar modelo con todas las variables explicativas
modelo_completo <- lm(Salary ~ ., data = datos_completos)
# Excluye `Division` si es necesario:
modelo_completo <- lm(Salary ~ . - Division, data = datos_completos)

# Resumen del modelo completo
summary(modelo_completo)

# Identificar variables no significativas al 1%
variables_no_significativas <- names(coef(modelo_completo))[which(summary(modelo_completo)$coefficients[, 4] > 0.01)]
print("Variables no significativas al 1%:")
print(variables_no_significativas)

# Realizar un contraste de significatividad conjunta de las variables no significativas
modelo_sin_no_significativas <- update(
  modelo_completo,
  formula = as.formula(paste("Salary ~", paste(setdiff(names(coef(modelo_completo)), variables_no_significativas), collapse = "+")))
)

# Contraste F entre el modelo completo y el modelo sin variables no significativas
anova_test <- anova(modelo_sin_no_significativas, modelo_completo)
print("Resultado del contraste F:")
print(anova_test)

# Interpretación del ANOVA y el estadístico F:
# El ANOVA compara el modelo completo (incluyendo todas las variables) con el modelo reducido 
# (excluyendo las variables no significativas al nivel del 1%). 
# El estadístico F calculado es 8.553101 con un valor p de 5.347717e-15, 
# lo que indica que las variables eliminadas tienen un impacto significativo en la predicción de "Salary".
# En términos prácticos, rechazamos la hipótesis nula de que las variables excluidas no contribuyen al modelo.
# Esto sugiere que incluir estas variables en el modelo mejora significativamente su ajuste.



#Ejercicio 4:
# Paso 1: Dividir el conjunto de datos en entrenamiento y prueba
set.seed(800) # Para reproducibilidad
# Usamos la función createDataPartition del paquete caret
library(caret)
trainIndex <- createDataPartition(datos_completos$Salary, p = 0.85, list = FALSE)
datos_entrenamiento <- datos_completos[trainIndex, ]
datos_prueba <- datos_completos[-trainIndex, ]

# Paso 2: Ajustar el modelo OLS completo en el conjunto de entrenamiento
modelo_ols_completo <- lm(Salary ~ . - Division, data = datos_entrenamiento)

# Paso 3: Evaluar el modelo en el conjunto de prueba
predicciones <- predict(modelo_ols_completo, newdata = datos_prueba)

# Paso 4: Calcular el error de prueba (usando RMSE como métrica)
rmse_prueba <- sqrt(mean((datos_prueba$Salary - predicciones)^2))

# Mostrar el RMSE del conjunto de prueba
print(paste("El RMSE en el conjunto de prueba es:", rmse_prueba))


# Convertir factores a variables dummy para usar en glmnet
x <- model.matrix(Salary ~ ., data = datos_completos)[, -1]  # Matriz de predictores
y <- datos_completos$Salary  # Variable respuesta
control <- trainControl(method = "cv", number = 10)  # Validación cruzada de 10 particiones

# Paso 5: Ajustar modelos con glmnet (LASSO para seleccionar variables)
modelo_lasso <- train(
  x = x, y = y,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001, 0.1, by = 0.001)), # LASSO (alpha=1)
  trControl = control
)

# Obtener el mejor modelo basado en el menor ECM
mejor_lambda <- modelo_lasso$bestTune$lambda  # Lambda que minimiza el ECM
mejor_ecm <- min(modelo_lasso$results$RMSE^2)  # Menor ECM de validación cruzada
print(paste("Lambda que minimiza el ECM:", mejor_lambda))
print(paste("ECM mínimo:", mejor_ecm))

# Paso 6: Aplicar la "regla del codo" (una desviación típica del mínimo ECM)
# Identificar el valor de lambda dentro de una desviación estándar del mínimo ECM
mejor_ecm_sd <- mejor_ecm + sd(modelo_lasso$results$RMSE^2)
lambda_codo <- max(modelo_lasso$results$lambda[modelo_lasso$results$RMSE^2 <= mejor_ecm_sd])
print(paste("Lambda seleccionado por la regla del codo:", lambda_codo))

# Paso 7: Ajustar el modelo final con el lambda seleccionado (por ECM mínimo o por el codo)
modelo_final <- glmnet(x, y, alpha = 1, lambda = lambda_codo)  # Modelo final con regla del codo

# Paso 8: Resumen del modelo final
coeficientes <- coef(modelo_final)  # Coeficientes del modelo
print("Coeficientes seleccionados por LASSO:")
print(coeficientes)

# Selección de variables numéricas
datos_numericos <- datos_completos[, sapply(datos_completos, is.numeric)]

# Configuración de la validación cruzada de 10 veces
control_10 <- trainControl(method = "cv", number = 10)

# Función para predecir con los modelos de subset selection
subset_pred <- function(object, newdataid, ...) {
  coefi <- coef(object, id = newdataid)
  xvars <- names(coefi)
  predict.regsubsets <- function(object, newdata, id, ...) {
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
  }
  predict.regsubsets(object, newdata = datos_numericos, id = newdataid)
}

# Mejor Selección de Conjuntos con Validación Cruzada de 10 Veces
best_subset_10 <- regsubsets(Salary ~ ., data = datos_numericos, nvmax = ncol(datos_numericos) - 1)
modelo_best_subset_cv_10 <- train(
  x = datos_numericos[, -which(names(datos_numericos) == "Salary")],
  y = datos_numericos$Salary,
  method = "leapBackward",
  tuneLength = 15,
  trControl = control_10,
  preProc = c("center", "scale")
)
error_prueba_best_subset_10 <- modelo_best_subset_cv_10$results[which.min(modelo_best_subset_cv_10$results$RMSE), "RMSE"]

# Selección por Pasos Hacia Adelante con Validación Cruzada de 10 Veces
forward_step_10 <- regsubsets(Salary ~ ., data = datos_numericos, nvmax = ncol(datos_numericos) - 1, method = "forward")
modelo_forward_cv_10 <- train(
  x = datos_numericos[, -which(names(datos_numericos) == "Salary")],
  y = datos_numericos$Salary,
  method = "leapForward",
  tuneLength = 15,
  trControl = control_10,
  preProc = c("center", "scale")
)
error_prueba_forward_10 <- modelo_forward_cv_10$results[which.min(modelo_forward_cv_10$results$RMSE), "RMSE"]

# Configuración de la validación cruzada de 5 veces
control_5 <- trainControl(method = "cv", number = 5)

# Mejor Selección de Conjuntos con Validación Cruzada de 5 Veces
best_subset_5 <- regsubsets(Salary ~ ., data = datos_numericos, nvmax = ncol(datos_numericos) - 1)
modelo_best_subset_cv_5 <- train(
  x = datos_numericos[, -which(names(datos_numericos) == "Salary")],
  y = datos_numericos$Salary,
  method = "leapBackward",
  tuneLength = 15,
  trControl = control_5,
  preProc = c("center", "scale")
)
error_prueba_best_subset_5 <- modelo_best_subset_cv_5$results[which.min(modelo_best_subset_cv_5$results$RMSE), "RMSE"]

# Selección por Pasos Hacia Adelante con Validación Cruzada de 5 Veces
forward_step_5 <- regsubsets(Salary ~ ., data = datos_numericos, nvmax = ncol(datos_numericos) - 1, method = "forward")
modelo_forward_cv_5 <- train(
  x = datos_numericos[, -which(names(datos_numericos) == "Salary")],
  y = datos_numericos$Salary,
  method = "leapForward",
  tuneLength = 15,
  trControl = control_5,
  preProc = c("center", "scale")
)
error_prueba_forward_5 <- modelo_forward_cv_5$results[which.min(modelo_forward_cv_5$results$RMSE), "RMSE"]

# Resultados de TODOS: 
cat("Error de prueba para Mejor Selección de Conjuntos (10 veces CV):", error_prueba_best_subset_10, "\n")
cat("Error de prueba para Selección por Pasos Hacia Adelante (10 veces CV):", error_prueba_forward_10, "\n")
cat("Error de prueba para Mejor Selección de Conjuntos (5 veces CV):", error_prueba_best_subset_5, "\n")
cat("Error de prueba para Selección por Pasos Hacia Adelante (5 veces CV):", error_prueba_forward_5, "\n")





#Ejercicio 9: seleccionamos el modelo con el menor error de prueba que es el de Selección por Pasos Hacia Adelante con Validación Cruzada de 10 Veces, con un error de prueba (RMSE) de 324.364

# Obtener los coeficientes del mejor modelo seleccionado
coeficientes <- coef(modelo_forward_cv_10$finalModel, modelo_forward_cv_10$bestTune$nvmax)

# Mostrar los coeficientes seleccionados y sus valores
print(coeficientes)

variables_seleccionadas <- c("AtBat", "Hits", "Walks", "CAtBat", "CRuns", "CRBI", "CWalks", "PutOuts", "Assists")
formula_modelo <- as.formula(paste("Salary ~", paste(variables_seleccionadas, collapse = " + ")))
modelo_lm <- lm(formula_modelo, data = datos_numericos)
summary(modelo_lm)





#Ejercico 10
# Preparar datos para glmnet (ya que x e y ya están definidos)
# Aseguramos que x no incluya la columna de intercepto si ya está creada antes
x <- model.matrix(Salary ~ ., data = datos_completos)[, -1]
y <- datos_completos$Salary

# Ajustar el modelo Ridge con λ = 0.1
modelo_ridge <- glmnet(x, y, alpha = 0, lambda = 0.1)

# Configuración para validación cruzada
control <- trainControl(method = "cv", number = 10)

# Ajuste del modelo con validación cruzada para obtener el error de prueba
modelo_ridge_cv <- train(
  x = x,
  y = y,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 0, lambda = 0.1), # Aquí especificamos α=0 para Ridge y λ=0.1
  trControl = control
)

# Error de prueba
error_prueba <- min(modelo_ridge_cv$results$RMSE)
print(paste("Error de prueba (RMSE) para Regresión Ridge con λ = 0.1:", error_prueba))




#Ejercicio 10

# Aseguramos que x e y estén definidos correctamente
x <- model.matrix(Salary ~ ., data = datos_completos)[, -1]  # Matriz de predictores sin intercepto
y <- datos_completos$Salary  # Variable respuesta

# Configuración para validación cruzada
control <- trainControl(method = "cv", number = 10)

# Ajuste del modelo LASSO con validación cruzada para seleccionar λ
modelo_lasso_cv <- train(
  x = x,
  y = y,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001, 1, by = 0.001)), # LASSO (alpha=1)
  trControl = control
)

# Obtener el mejor λ
mejor_lambda <- modelo_lasso_cv$bestTune$lambda

# Ajustar el modelo final con el mejor λ
modelo_lasso_final <- glmnet(x, y, alpha = 1, lambda = mejor_lambda)

# Error de prueba
error_prueba <- min(modelo_lasso_cv$results$RMSE)
print(paste("Error de prueba (RMSE) para Regresión LASSO:", error_prueba))

# Número de coeficientes diferentes de cero
coeficientes_no_cero <- sum(coef(modelo_lasso_final) != 0)
print(paste("Número de coeficientes estimados diferentes de cero:", coeficientes_no_cero))




#Ejercicio 12
#Regresión Ridge con Validación Cruzada de 5 Veces
# Aseguramos que x e y estén definidos correctamente
x <- model.matrix(Salary ~ ., data = datos_completos)[, -1]  # Matriz de predictores sin intercepto
y <- datos_completos$Salary  # Variable respuesta

# Configuración para validación cruzada de 5 veces
control <- trainControl(method = "cv", number = 5)

# Ajuste del modelo Ridge con validación cruzada para seleccionar λ
modelo_ridge_cv_5 <- train(
  x = x,
  y = y,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 1, by = 0.001)), # Ridge (alpha=0)
  trControl = control
)

# Obtener el mejor λ
mejor_lambda_ridge <- modelo_ridge_cv_5$bestTune$lambda

# Ajustar el modelo final con el mejor λ
modelo_ridge_final_5 <- glmnet(x, y, alpha = 0, lambda = mejor_lambda_ridge)

# Error de prueba
error_prueba_ridge_5 <- min(modelo_ridge_cv_5$results$RMSE)
print(paste("Error de prueba (RMSE) para Regresión Ridge con 5 veces CV:", error_prueba_ridge_5))


#Regresión LASSO con Validación Cruzada de 5 Veces
# Configuración para validación cruzada de 5 veces
control <- trainControl(method = "cv", number = 5)

# Ajuste del modelo LASSO con validación cruzada para seleccionar λ
modelo_lasso_cv_5 <- train(
  x = x,
  y = y,
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 1, lambda = seq(0.001, 1, by = 0.001)), # LASSO (alpha=1)
  trControl = control
)

# Obtener el mejor λ
mejor_lambda_lasso <- modelo_lasso_cv_5$bestTune$lambda

# Ajustar el modelo final con el mejor λ
modelo_lasso_final_5 <- glmnet(x, y, alpha = 1, lambda = mejor_lambda_lasso)

# Error de prueba
error_prueba_lasso_5 <- min(modelo_lasso_cv_5$results$RMSE)
print(paste("Error de prueba (RMSE) para Regresión LASSO con 5 veces CV:", error_prueba_lasso_5))

# Número de coeficientes diferentes de cero
coeficientes_no_cero_lasso_5 <- sum(coef(modelo_lasso_final_5) != 0)
print(paste("Número de coeficientes estimados diferentes de cero para LASSO con 5 veces CV:", coeficientes_no_cero_lasso_5))







#Ejercicio 13
# Escalar los datos
# Seleccionar solo las columnas numéricas para escalar
variables_numericas <- sapply(datos_completos, is.numeric)
datos_numericos <- datos_completos[, variables_numericas]

# Excluir la variable 'Salary' de las variables predictoras
datos_predictoras <- datos_numericos[, names(datos_numericos) != "Salary"]

# Escalar los datos numéricos
datos_escalados <- scale(datos_predictoras)

# La variable respuesta sigue siendo 'Salary'
y <- datos_completos$Salary

# Configuración para validación cruzada de 10 veces
control_10 <- trainControl(method = "cv", number = 10)

# Ajustar el modelo PCA
modelo_pca_10 <- train(
  x = datos_escalados,
  y = y,
  method = "pcr",
  tuneLength = 20, # Ajuste para probar varios valores de M
  trControl = control_10,
  preProcess = c("center", "scale")  # Aunque ya escalamos, esto asegura consistencia
)

# Mejor número de componentes (M) y error de prueba
mejor_M_10 <- modelo_pca_10$bestTune$pc
error_prueba_10 <- min(modelo_pca_10$results$RMSE)

cat("Validación Cruzada de 10 Veces:\n")
cat(paste("Mejor número de componentes (M):", mejor_M_10, "\n"))
cat(paste("Error de prueba (RMSE):", error_prueba_10, "\n"))

print(modelo_pca_10$bestTune)




#Ejercicio 14

library(caret)
library(pls)

# Configuración para validación cruzada de 10 veces
control_10 <- trainControl(method = "cv", number = 10)

# Ajustar el modelo PLS con 10 veces CV
modelo_pls_10 <- train(
  x = datos_escalados,
  y = y,
  method = "pls",
  tuneLength = 20, 
  trControl = control_10
)

# Mejor número de componentes (M) y error de prueba para 10 veces CV
mejor_M_10 <- modelo_pls_10$bestTune$ncomp
error_prueba_10 <- min(modelo_pls_10$results$RMSE)

cat("Validación Cruzada de 10 Veces:\n")
cat(paste("Mejor número de componentes (M):", mejor_M_10, "\n"))
cat(paste("Error de prueba (RMSE):", error_prueba_10, "\n"))

# Configuración para validación cruzada de 5 veces
control_5 <- trainControl(method = "cv", number = 5)

# Ajustar el modelo PLS con 5 veces CV
modelo_pls_5 <- train(
  x = datos_escalados,
  y = y,
  method = "pls",
  tuneLength = 20, 
  trControl = control_5
)

# Mejor número de componentes (M) y error de prueba para 5 veces CV
mejor_M_5 <- modelo_pls_5$bestTune$ncomp
error_prueba_5 <- min(modelo_pls_5$results$RMSE)

cat("Validación Cruzada de 5 Veces:\n")
cat(paste("Mejor número de componentes (M):", mejor_M_5, "\n"))
cat(paste("Error de prueba (RMSE):", error_prueba_5, "\n"))

