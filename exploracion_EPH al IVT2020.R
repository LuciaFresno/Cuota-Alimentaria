
# ___
# 1 - CODIGO de hogar
# ___
# 11 - Relación de parentesco 
# CH03 N(2) Relación de Parentesco
# 01 = Jefe/a
# 02 = Cónyuge/Pareja
# 03 = Hijo/Hijastro/a
# 04 = Yerno/Nuera
# 05 = Nieto/a
# 06 = Madre/Padre
# 07 = Suegro/a
# 08 = Hermano/a
# 09 = Otros Familiares
# 10 = No Familiares
# 
# ___
# 12 - Varon/Mujer
# CH04 N(1) Sexo
# 1 = varón
# 2 = mujer
# 
# ___
# 14 - Edad cumplida al momento del cuestionario
# 
#  


#__________________________________________________________________
#
# @author: Lucia Fresno luciafresno@gmail.com


library(tidyverse)
library(dplyr)
library(readxl)
options(max.print=25000)



EPH <- read_excel('EPH al IVT2020.xlsx',trim_ws=TRUE, skip=1)

# selecciono las columnas relevantes para moverme con más velocidad
ssEPH <- EPH[,c(1,11,12,14)]

# emprolijo headers para qeu sean más fáciles de invocar
colnames(ssEPH) <- c("CODUSU","RELACION", "SEXO", "EDAD")

# cuántos hogares hay en mi lista?
# 
codigos_de_hogar_unicos <- length(unique(ssEPH$CODUSU)) #  14310 resultados / 998 hogares con 1 sola persona / nro mayor de convivientes 12 /


# Identificar los hogares incompletos, o sea aquellos donde haya unx jefx de hogar pero no haya cónyugue/pareja
# quiero los codusu únicos que se repitan más de una vez y tengan gente menor de 18 años y que no tengan cónyugue,
# 
#identifico los hogares en los que tienen NNyA
hogares_con_NNyA <-  ssEPH %>% filter(EDAD <= 18) #  12,036 entries incluyendo  EDAD=18 y  11,264 entries sacándolos
# 60 NNyA entre 14 y 18 años con ESTADO_CIVIL diferente a 5-soltero
#identifico los hogares donde vive lx cónyugue de lx jefx de hogar
hogares_con_conyugues <- ssEPH %>% filter (RELACION == 2) # 7,720 entries

# identifico los hogares donde viven NNyA pero no hay cónyugue
hogares_seleccionados <- hogares_con_NNyA[!(hogares_con_NNyA$CODUSU %in% hogares_con_conyugues$CODUSU),] # 3,667 entries,

# conseguir la información completa de cada uno de esos hogares seleccionados (agregando lxs adultxs que conviven con esxs NNyA)

ssEPH_mono  <- ssEPH[ssEPH$CODUSU %in% hogares_seleccionados$CODUSU,]  #  7,933 entries


#  Desagregar por sexo de lx jefx de hogar
#  Mujeres
ssEPH_mono_M <-  ssEPH_mono %>% filter (RELACION == 1 & SEXO == 2)# 1,671 entries

#  hombres
ssEPH_mono_H <-  ssEPH_mono %>% filter (RELACION == 1 & SEXO == 1)# 390 entries

# ahora que tengo los registros seleccionados traigo el resto de las columnas y los datos completos de la familia
# para eso necesito usar un KEY en EPH

colnames(EPH)[1] <- c("CODUSU")
EPH_mono_M <- EPH[EPH$CODUSU %in% ssEPH_mono_M$CODUSU,] # 6,416 entries
EPH_mono_H <- EPH[EPH$CODUSU %in% ssEPH_mono_H$CODUSU,] # 1,591 entries

source("imprimir_archivos.R")
