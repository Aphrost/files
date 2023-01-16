library(tidyverse) # dplyr, ggplot2, y amigos
library(lubridate)
library(R.utils)
library(rpart)
library(rpart.plot)
library(class)

setwd('C:/Users/Aphrost/OneDrive/Educacion/Diplomados/Diplomado en Data Science y Big Data/Mineria de Datos/Evaluaciones')

setwd('C:/Users/luxob/OneDrive/Educacion/Diplomados/Diplomado en Data Science y Big Data/Mineria de Datos/Evaluaciones')


datos <- read.csv('SouthGermanCredit.csv', sep = ";")

datos %>%
  glimpse()


#### Clase 5

datos_small <- as.data.frame(cbind(age = datos$age,
                                   housing_free = (datos$housing == 1),
                                   housing_rent = (datos$housing == 2),
                                   job = datos$job,
                                   foreign = datos$foreign,
                                   credit = datos$credit))

normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

datos_norm <- as.data.frame(lapply(datos_small, normalize))

set.seed(123)
subset <- sample(1:nrow(datos), size = 0.75*nrow(datos), replace = FALSE)

datos_train <- datos_norm[subset, 1:5]
label_train <- datos_norm[subset, 6]
datos_test <- datos_norm[-subset, 1:5]
label_test <- datos_norm[-subset, 6]

modelo_k3 <- knn(datos_train,
                 datos_test,
                 label_train,
                 k = 3)

modelo_k5 <- knn(datos_train,
                 datos_test,
                 label_train,
                 k = 5)

table(label_test, modelo_k3)
table(label_test, modelo_k5)

sum(label_test == modelo_k3)/length(label_test)
sum(label_test == modelo_k5)/length(label_test)

modelo_k4 <- knn(datos_train,
                 datos_test,
                 label_train,
                 k = 4)

table(label_test, modelo_k4)
sum(label_test == modelo_k4)/length(label_test)

library(recipes)
library(caret)
library(e1071)



confusionMatrix(modelo_k3,as.factor(label_test))
confusionMatrix(modelo_k5,as.factor(label_test))

