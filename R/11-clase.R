
# practico 11, calidad de modelos y otras tecnicas de estimacion ----------


# paquetes a utilizar  ----------------------------------------------------

pacman::p_load(tidyverse, magrittr, 
               summarytools, #decriptivos
               sjPlot, #visualización
               performance, #evaluación de modelos
               see) # herramientas para la visualización


# datos a utilizar ----------------------------------------------------------------

movid_proc <- readRDS("output/movid_proc.RDS")

# explorar datos ----------------------------------------------------------

view(dfSummary(movid_proc, headings = FALSE, method = "render"))

# 3. Introducción
# **2. Ajuste** (*post-hoc*): implica abordar qué tan bien ajustan nuestros modelos 
#     con los datos utilizados. La *bondad de ajuste* implica que si trabajamos con una
# 
# - 2.1 *Regresión lineal múltiple*, analizaremos el $R^2$ ajustado
# - 2.2 *Regresión logística*, analizaremos el $Pseudo$ $R^2$ y los
#   *Criterios de Información* $BIC$ y $AIC$ (ambos nos dicen cuánta información
#    se está "perdiendo" con las variables que se ocupan en cada modelo. 
#    Por ello elegiremos el modelo que tenga un BIC y AIC más bajo)
# 
# **3. Comparación**: luego de que hacemos **transformaciones** a los modelos, 
#    una etapa importante para la **selección** de estos es compararlos. 
#    Para ello consideraremos toda la información que tenemos de ellos en su diagnóstico 
#    de **pre y post hoc.**
#   
#   **¿Cómo evaluar la robustez y ajuste de modelos en `R`?**
#   
#   En R existen diversas funciones para crear un diagnóstico de supuestos y de 
#   ajuste a nuestros modelos. Ahora bien, la gran parte de ello se desenvuelve en distintos
#   paquetes, sobre todo  considerando que, como vimos, **no existen criterios únicos** para
#   evaluar la calidad de nuestros modelos (más aún cuando son su variable dependiente tiene 
#   un nivel distinto de medición).
# 
# La buena noticia es que el paquete `performace` reúne todas estas herramientas, 
# y que los tres ejes de la **calidad de los modelos** también las distingue en una 
# función para cada una.



# diagnóstico de la calidad del modelo ------------------------------------
# Imaginemos que queremos analizar los determinantes de la **Fatiga pandémica**,
# y para ello estimaremos un modelo de regresión lineal.


model1 <- lm(as.numeric(fatiga) ~ c2_1 + c2_2 + c2_3 + c2_4 +
               trabaja + sexo + edad + ingreso,
             data = movid_proc)


texreg::screenreg(model1, custom.model.names = "Modelo 1", caption = "")



# robustez y suspuestos ---------------------------------------------------
# Los modelos de regresión tienen una serie de **supuestos** que se deben cumplir 
# para que las estimaciones sean **fidedignas**.En esa sesión consideraremos los supuestos 
# más esenciales: 
# 1. linealidad, 
# 2. normalidad de residuos, 
# 3. homogeneidad de la varianza,  
# 4. independencia de variables, 
# 5. multicolinealidad y 
# 6. casos influyentes (*los primeros tres aplican solo para regresiones lineales*).



#  linealidad -------------------------------------------------------------

# Para la regresión lineal múltiple, un supuesto importante es que existe una **relación lineal entre la 
# variable dependiente e independiente**.
# 
# Podemos saber si se cumple este supuesto a partir de un *gráfico de dispersión de datos*, que relacione 
# la variable dependiente y la independiente, y verificar de manera "intuitiva" si la **tendencia** de esta relación se 
# puede describir por una **línea recta**. El paquete `performace` nos permite hacer esto con su función `check_model` indicando en el argumento `check = "ncv`

check_model(model1, check = c("ncv", "linearity"))


#  Homocedasticidad -------------------------------------------------------
# Este concepto indica que **los residuos** se distribuyen de forma **homogénea** (por eso el sufijo *homo*). Como ya podrás haber notado este supuesto se vincula con el de [linealidad](#linealidad)

check_heteroscedasticity(model1)


# Normalidad de residuos --------------------------------------------------

# Además de la linealidad (media 0), la homocedasticidad (varianza mínima y constante), el método de estimación de la regresión 
# lineal (*OLS* o *MCO* en español) requiere asegurar una **distribución normal** de los residuos pues en caso contrario el modelo 
# no es consistente a través de las variables y observaciones (esto significa que los errores no son aleteatorios).


check_normality(model1)


# independencia  ----------------------------------------------------------

# Evidentemente si los residuos no siguen una distribución normal, es probable que estos no sean independientes entre sí. 
# Esto significa que buscaremos que los errores asociados a nuestro modelo de regresión sean **independientes**.

check_autocorrelation(model1)


# multicolinealidad -------------------------------------------------------
# La multicolinealidad es la relación de **dependencia lineal fuerte** entre más de dos **predictores** de un modelo.

plot(check_collinearity(model1))

check_collinearity(model1)


# Casos influyentes  ------------------------------------------------------

# Un último supuesto que revisaremos, y es el que probablemente parte del que más nos enfrentamos en las ciencias sociales, son los **casos influyentes** 
# (también llamados en inglés, *outliers*). Un ejemplo claro de esto son las variables como ingresos, donde muchas veces tenemos casos extremos con muy 
# bajos salarios y otros muy altos, y que pueden tendenciar nuestras rectas de regresión pese a que no es evidente una relación lineal(o algún tipo de relación) 
# entre la variable independiente y dependiente.
# 
# Para verificar si un "caso es *influyente*" examinaremos si la ausencia o presencia de ese caso genera un cambio importante en la estimación del modelo de regresión. 
# Este enfoque se aborda a partir del cálculo de la **Distancia de Cook** (Cook,1977)

plot(check_outliers(model1))

check_outliers(model1)


# Ajuste del modelo -------------------------------------------------------
# Podemos evaluar qué tan bien ajustan nuestros modelos con los datos utilizados. Sabemos que:
#   
# 2.1 Si trabajamos una regresión lineal: el $R^2$ ajustado
# 
# 2.2 Si trabajamos una regresión logística: Pseudo $R^2$ y los *Criterios de Información* BIC y AIC 
# (ambos nos dicen cuánta información se está "perdiendo" con las variables que se ocupan en cada modelo. 
#   Por ello elegiremos el modelo  que tenga un BIC y AIC más bajo).

performance::model_performance(model1)

table <- readRDS(url("https://github.com/learn-R/11-class/raw/main/input/tab-performance.rds"))
table


# Tranformar predictor al cuadrado o cubo ---------------------------------

movid_proc <- movid_proc %>% mutate(edad2 = (edad)^2)


# logaritmizar cariable dependiente ---------------------------------------

movid_proc <- movid_proc %>% mutate(ingresos = log(ingresos))


# recuperar casos perdidos ------------------------------------------------

movid_proc %>% select(ingreso, tingreso) %$%
  view(dfSummary(., headings = FALSE, method = "render", varnumbers = F, lang = "es"))

# La estrategia posee los siguientes pasos:
# - **Paso 1:** Calcular la media por cada tramo
# - **Paso 2:** En el caso de no tener información, remplazar por la media del tramo

movid_proc <- movid_proc %>%
  mutate(tingreso = case_when(tingreso == "Menos de $200 mil pesos" ~ 200000,
                              tingreso == "Entre $200 y 350 mil pesos" ~ 275000,
                              tingreso == "Entre $351 y 500 mil pesos" ~ 425500,
                              tingreso == "Entre 501 y 800 mil pesos" ~ 650500,
                              tingreso == "Entre 801 mil y 1 millón 200 mil pesos" ~ 1000500,
                              tingreso == "Entre 1 millón 201 mil y 2 millones de pesos" ~ 1600500,
                              tingreso == "Entre 2 millones y 5 millones de pesos" ~ 3500000,
                              tingreso == "Más de 5 millones de pesos" ~ 5000000), #Paso 1
         ingreso = if_else(is.na(ingreso), tingreso, ingreso))

# - **Paso 3:** Comparar el resultado de los tramos


movid_proc %>% select(ingreso, tingreso) %$%
  view(dfSummary(., headings = FALSE, method = "render", varnumbers = F, lang = "es")) #Paso 3


# Vemos que pasamos de tener 25,3% de datos perdidos en ingresos a un 8,72% (es decir, recuperamos un 16,58% de los casos). 
# A ingresos se le pueden hacer tres transformaciones más
# 
# **1. Logaritmizar**: en caso de que queramos seguir trabajando ingresos como una variable continua es una buena opción.
# 
# **2. Calcular el ingreso per cápita**: si dividimos el ingreso por el tamaño del hogar (n° de habitantes en este), obtendremos el ingreso por persona.
# 
# **3. Cálculo de medidas de posición acumulada**: con los ingresos per cápita se puede calcular la media o mediana de medidas de posición acumulada como quitiles


movid_proc <- movid_proc %>%
  mutate(log_ing = log(ingreso), #Log ingresos
         ing_per = ingreso/tamanohogar, #Ingreso percapita
         quintiles = dplyr::ntile(ing_per,
                                  n = 5)) # n de categorias, para quintiles usamos 5



# dicotomizar variable dependiente ----------------------------------------

# Ocuparemos dos criterios para la **dicotomización**:
# 
# 1. **Medias**: se ocupará como criterio discriminante la media de la variable (donde 1 puede ser los valores mayores a la media, 
#                                                                                y 0 los inferiores).
# 
# 2. **Mediana**: la más frecuente en medidas ordinales como las *escalas Likert* es cuando el 50% de los casos se concentra en unas 
# pocas categoría de respuesta (eg, "Muy de acuerdo" y "De acuerdo" serán 1 y el resto 0).

movid_proc <- movid_proc %>%
  mutate(fatigadummy = case_when(fatiga %in% c(5,4) ~ 1,
                                 fatiga %in% c(3,2,1) ~ 0, TRUE ~ NA_real_))



# errores estándares robustos ---------------------------------------------
# En caso de que estemos ante problemas de heterocedasticidad debemos re-estimar nuestro modelo considerando errores estándares robustos


model_robust<- lmtest::coeftest(model1, vcov=sandwich::vcovHC(model1))



# creacion de indices -----------------------------------------------------

# Se debe cacular para aquellos ítems que mostraron posibilidad de ser colineales. Los pasos son
# 
# 1. **Correlacionar** para verificar que estamos ante la presencia de ítems que podrían estar midiendo 
# un constructo común. Como vemos en la siguiente tabla, las correlaciones son altas
# 
# ```{r}
# movid_proc %>% select(starts_with("c2")) %>%
#   mutate_all(~as.numeric(.)) %>%
#   sjPlot::tab_corr(., triangle = "lower")
# ```
# 
# 2. **Construcción de índice**: este puede ser sumativo o promedio. Esto dependerá de la escala de los ítems 
# y del sentido del constructo final que queremos utilizar. En nuestro caso crearemos una índice sumativa de `salud mental`


movid_proc <- movid_proc %>%
  mutate_at(vars(starts_with("c2")),~as.numeric(.)) %>%
  rowwise() %>%
  mutate(salud_mental = sum(c2_1,c2_2,c2_3,c2_4, na.rm = T))



# eliminar casos influyentes ----------------------------------------------
# En caso de que estemos ante la presencia de casos influyentes debemos seguir los siguientes pasos


n<- nobs(model1) #n de observaciones
k<- length(coef(model1)) # n de parametros
dcook<- 4/(n-k-1) #Punto de corte
# Datos donde se filtran los valores sobre el punto de corte

movid_proc_so <- broom::augment_columns(model1,data = movid_proc) %>% filter(.cooksd<dcook)

# Una vez finalizada las transformaciones calcular dos nuevos modelos


model1_fit <- lm(as.numeric(fatiga) ~ salud_mental +
                   trabaja + sexo + edad + ing_per,
                 data = movid_proc)

model2 <- lm(as.numeric(fatiga) ~ salud_mental +
               trabaja + sexo + edad2 + ing_per,
             data = movid_proc)

model3 <- glm(fatigadummy ~ salud_mental +
                trabaja + sexo + edad2 + ing_per, family = "binomial",
              data = movid_proc)


# Luego, podemos hacer un chequeo general de diagnósticos de robustez con `check_model`, 
# pero ahora indicando que queremos evaluar todos los indicadores posibles


check_model(model1_fit, check = c("vif","normality", "linearity", "ncv", "homogeneity"))
check_model(model2, check = c("vif",  "normality", "linearity", "ncv", "homogeneity"))
check_model(model3, check = c("vif",  "homogeneity"))


# comparacion -------------------------------------------------------------

# Modelos de regresión que predice la fatiga a la pandemia

texreg::screenreg(list(model1,model2,model3), custom.model.names = c("Modelo 1", "Modelo 2", "Modelo 3"), caption = "")

# Si bien en con la tabla N°3  podemos tener un panorama, es **imprescindible** recordar que para comparar modelos 
# (en su robustez y ajuste) es necesario que estos tengan *(1) la misma variable de respuesta y (2) el mismo número de observaciones.*


compare_performance(model1_fit, model2)


plot(compare_performance(model1_fit, model2))


# Ahora bien, esto no quita que, considerando que el ajuste entre el `model1_fit` y el `modelo2` no cambia sustantivamente, consideremos en 
# seleccionar el `modelo3` por criterios más sustantivos. Podemos asegurarnos de esta comparación entre el modelo1 y modelo2 con un test que 
# permite facilitar la decisión sobre la significancia de los índices que estamos viendo


test_performance(model1_fit, model2)












