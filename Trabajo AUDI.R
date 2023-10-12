#------------------------------------------------------
# MUCD: Tema 3
# Ejemplo 1 PCA
#------------------------------------------------------

library(ggplot2)
library(ggcorrplot)
library('corrr')
library("FactoMineR") 
library("factoextra")
library(readr)
library(dplyr)
library(readxl)

rm(list=ls())

setwd("D:/DOCUMENTOS/Clase/Máster/Fundamentos para el análisis de datos y la investigación/Trabajo 2")
AUDI_df = read_xlsx("idae_nombre.xlsx")

head(AUDI_df)

class(AUDI_df$Modelo)
class(AUDI_df$`Potencia CV`)
class(AUDI_df$`Consumo Mínimo`)
class(AUDI_df$'Consumo Máximo')
class(AUDI_df$'Emisiones Mínimo')
class(AUDI_df$'Emisiones Máximo')
class(AUDI_df$Tipo)

AUDI_df$Modelo <- as.factor(AUDI_df$Modelo)

class(AUDI_df$Modelo)

#------------------------------------------------------
# Agrupamos en una tabla los modelos que son similares
#------------------------------------------------------

AUDI_df_agrupado <- AUDI_df %>% 
  group_by(Modelo, `Potencia CV`) %>% 
  summarise(
    `Media Consumo Mínimo` = mean(`Consumo Mínimo`, na.rm = TRUE),
    `Media Consumo Máximo` = mean(`Consumo Máximo`, na.rm = TRUE),
    `Media Emisiones Mínimo` = mean(`Emisiones Mínimo`, na.rm = TRUE),
    `Media Emisiones Máximo` = mean(`Emisiones Máximo`, na.rm = TRUE)
  )

AUDI_df_agrupado_potencia <- AUDI_df %>% 
  group_by(Modelo) %>% 
  summarise(
    'Media Potencia CV' = mean (`Potencia CV`, na.rm = TRUE),
    `Media Consumo Mínimo` = mean(`Consumo Mínimo`, na.rm = TRUE),
    `Media Consumo Máximo` = mean(`Consumo Máximo`, na.rm = TRUE),
    `Media Emisiones Mínimo` = mean(`Emisiones Mínimo`, na.rm = TRUE),
    `Media Emisiones Máximo` = mean(`Emisiones Máximo`, na.rm = TRUE)
  )


#------------------------------------------------------
# Analisis descriptivo de los datos
#------------------------------------------------------

boxplot_emisiones <- boxplot(AUDI_df_agrupado[, c("Media Emisiones Mínimo", "Media Emisiones Máximo")],
        col = "blue",
        xlab = "Variables",
        ylab = "Valor",
        main = "Boxplot de Media de Emisiones (Mínimo y Máximo)")

boxplot_consumo <- boxplot(AUDI_df_agrupado[, c("Media Consumo Mínimo", "Media Consumo Máximo")],
        col = "blue",
        xlab = "Variables",
        ylab = "Valor",
        main = "Boxplot de Media de Consumo (Mínimo y Máximo)")

boxplot_potencia <- boxplot (AUDI_df_agrupado[, c("Potencia CV")],
        col = "blue",
        xlab = "Variables",
        ylab = "Valor",
        main = "Boxplot de Potencia en caballos")


library(TeachingDemos)


stars(AUDI_df_agrupado_potencia[,-1],  # Excluyendo la columna "Equipo" de los datos
      labels = as.character(AUDI_df_agrupado$Modelo),  # Convertir a character
      key.loc = c(15, 1),  # Ubicación de la leyenda (puede necesitar ajuste)
      main = "Gráfico de Estrellas para Equipos")

stars(AUDI_df_agrupado_potencia[, -1],  # Excluyendo la columna "Modelo" de los datos
      labels = as.character(AUDI_df_agrupado_potencia$Modelo),  # Convertir a character
      key.loc = c(15, 1),  # Ubicación de la leyenda (puede necesitar ajuste)
      main = "Gráfico de Estrellas para Equipos",
      col.stars = colours()  # Asignar colores a las estrellas
)

#------------------------------------------------------
# Matriz de Correlaciones
#------------------------------------------------------
cor_matrix <- cor(AUDI_df_agrupado[,-1])
rounded_cor_matrix <- round(cor_matrix, 3)
det_value <- det(cor_matrix)

#------------------------------------------------------
# Autovalores y autovectores de la Matriz de Correlaciones 
# Inteprtaremos como varianzas
#------------------------------------------------------

eigen(cor(AUDI_df_agrupado[,-1]))

#------------------------------------------------------
# Adecuacion del Modelo
# Test de Bartlett y KMO usando la libreria psych
#------------------------------------------------------

library(psych)

AUDI_df_agrupado_numeric <- AUDI_df_agrupado[, sapply(AUDI_df_agrupado, is.numeric)]

correl<-cor(AUDI_df_agrupado_numeric)

bartlett_test <- cortest.bartlett(correl, n = nrow(AUDI_df_agrupado_numeric))

kmo_value <- KMO(correl)

#------------------------------------------------------
# Componentes Principales: Valores y Vectores propios 
# de la Matriz de Correlaciones. Varianza Acumulada
#------------------------------------------------------

acp <- princomp(AUDI_df_agrupado_numeric, cor=TRUE)
summary(acp)

#------------------------------------------------------
# Valors propios y varianza
#------------------------------------------------------

val.propios <- acp$sdev^2
cat("Varianzas:")
val.propios

#------------------------------------------------------
# Grafico de codo
#------------------------------------------------------
fviz_eig(acp, 
         addlabels = TRUE, 
         main = "Gráfico de Codo para el ACP",  
         xlab = "Componentes Principales",     
         ylab = "Varianza") +    
  theme_minimal()  
library(factoextra)
library(ggplot2)

#------------------------------------------------------
# ACP: Cargas Factoriales: Interpretacion
#------------------------------------------------------
acp$loadings

#------------------------------------------------------
# Puntuaciones Factoriales
#------------------------------------------------------

acp$scores

#------------------------------------------------------
# Interpretacion: Biplot
#------------------------------------------------------
biplot(acp, cex=0.50)

# Tamaño personalizado para las observaciones y las etiquetas de las variables
cex_obs <- 0.6  # Tamaño para las observaciones
cex_var <- 0.7  # Tamaño para las etiquetas de las variables

# Colores personalizados
col_obs <- "navy"  # Color para las observaciones
col_var <- "darkmagenta"   # Color para las etiquetas de las variables

# Tipo de punto personalizado
pch_obs <- 17  # Tipo de punto para las observaciones

rownames(acp$scores)

# Crear el biplot
biplot(acp, 
       cex = cex_obs, 
       cex.axis = cex_var, 
       col = c(col_obs, col_var),
       pch = pch_obs,
       main = "Biplot del ACP")

abline(h = 0, v = 0, col = "gray", lty = 2)

# Biplot circular

fviz_pca_var(acp, col.var="cos2", gradient.cols=c("blue", "red"), labelsize=4) + 
  ggtitle("Visualización PCA de Variables") +
  xlab("Dimensión 1 (x% de la varianza)") + 
  ylab("Dimensión 2 (y% de la varianza)") +
  theme_minimal()

# Calidad de cada variable
fviz_cos2(acp, choice = "var", axes = 1:2)

fviz_cos2(acp, choice = "var", axes = 1:2) +
  labs(
    title = "Gráfico de Cosenos al Cuadrado (Cos^2) para el ACP",
    x = "Dimensión 1",
    y = "Dimensión 2"
  )


#------------------------------------------------------
# Graficos Puntuaciones sobre las componentes
# Esto nos sirve para crear indices acp$scores[,1:3]
# Se eligen los dos primeros y se normalizan
#------------------------------------------------------

score<-acp$scores[,1:3]
score

plot(score[,1], score[,2], type='n')
text(score[,1], score[,2], labels=rownames(AUDI_df_agrupado_numeric), lwd=1)

plot(score[,1], score[,3], type='n')
text(score[,1], score[,3], labels=rownames(AUDI_df_agrupado_numeric), lwd=1)

plot(score[,2], score[,3], type='n')
text(score[,2], score[,3], labels=rownames(AUDI_df_agrupado_numeric), lwd=1)

#------------------------------------------------------
# Graficos desde ggplot2
#------------------------------------------------------

# install.packages("ggfortify")
library(ggfortify)

pcagrafico1<-prcomp(AUDI_df_agrupado_numeric, scale. = TRUE)
autoplot(pcagrafico1, data=AUDI_df_agrupado_numeric, colours="rownames(AUDI_df_agrupado_numeric)", loadings=TRUE)

#------------------------------------------------------
# Rotacion varimax: Ayuda interpretacion
# Con el paquete *psych* (necesita el paquete *GPArotation*)
#------------------------------------------------------
equipos_numeric
library(psych)

acp.varimax <- principal(AUDI_df_agrupado_numeric, nfactors=2, rotate="varimax", scores=TRUE)
summary(acp.varimax)
loadings(acp.varimax)

#------------------------------------------------------
# Biplot-varimax
#------------------------------------------------------

biplot(acp.varimax, labels=rownames(AUDI_df_agrupado_numeric), cex=0.5, main="")

#------------------------------------------------------
# Computo de componentes principales con estimadores robustos de M. Correl
#------------------------------------------------------

library(MASS)

# Identificar las columnas con IQR 0
cols_with_iqr_zero <- sapply(AUDI_df_agrupado_numeric, IQR) == 0

# Eliminar esas columnas
AUDI_filtered <- AUDI_df_agrupado_numeric[, !cols_with_iqr_zero]

usair.mve <- cov.rob(AUDI_filtered, method="mcd")

usair.pc1<-princomp(AUDI_filtered,covmat=usair.mve,cor=TRUE)
summary(usair.pc1,loadings=T)


