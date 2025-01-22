################################################################################
###### Script to read the outputs of the different stock assessments      ######
###### (SS3 model) and calculate from the outputs, how many juevniles     ######
######  and adults each fleet is fishing                                  ######
###### Species: BET, YFT and SKJ                                          ######
###### Author: Roger Amate (AZTI)                                         ######
###### year: 2023                                                         ######
################################################################################
#install.packages("zoo")
#install.packages("r4ss") # 
#remotes::install_github("r4ss/r4ss")
library(r4ss)
library(zoo)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(hrbrthemes)
library(scales)

### Paste the directory where the outputs of each species are stored
dir.BET=".../8. Lectura de fichers/latest_SA_files/BET/io_h80_Gbase_MHamel17_sD/"         # It's a example
dir.YFT=".../8. Lectura de fichers/latest_SA_files/YFT/io_h80_q1_Gbase_Mbase_tlambda01/"  # It's a example
dir.SKJ=".../8. Lectura de fichers/latest_SA_files/SKJ/io_h80_Ua_q0_L70/"                 # It's a example

#######################################################    BIGEYE   ####################################################
SA_BET=SS_output(dir.BET, covar=FALSE)  # Esto lee todo el output del assessment del "Report.soo".

# To diferentiate juveniles from adults, we will use the 50% maturity. First we obtain the size at 50% maturity from
#SA_BET_parameters, and then we check to which age that size corresponds in the endgrowth:
x=which(SA_BET$parameters$Label=="Mat50%_Fem_GP_1")   
size50Mat=SA_BET$parameters[x,"Value"]       # This is the size at 50% maturity. Now we pass it to age class.
age_length=SA_BET$endgrowth[,c("Age_Beg", "Len_Mid")]  # Here we can see that the fish of sizes =>110 are fish older than 16 quarters (4 years).

# Con esto entiendo que puedes obtener las matrices de biomasa x edad x año y separar juveniles y adultos.

Catch_at_age <- SA_BET$catage # Esto son capturas por area, flota, sexo, tipo... La matriz es muy parecida a la anterior, te interesan el area, la flota, el año y el resto
# las edades de 0 a 40.
write.csv(Catch_at_age, 'BET_catch_at_age.csv')

SA_BET$definitions # Esto permite ver a que corresponde cada flota. En las evaluaciones no importa el país, sino el arte, que es el que describe la interacción
# con cada clase de talla

SS_plots(SA_BET) # La función SS_plots hace automaticamente casi todas las figuras de la libreria con los datos.



#######################################################    Yellowfin   ####################################################

SA_YFT=SS_output(dir.YFT, covar=FALSE)  # Esto lee todo el output del assessment del "Report.soo".

# Biomasss
Biomass_YFT <- SA_YFT$timeseries
write.csv(Biomass_YFT, 'Biomass_YFT.csv')

# Biomass at age
SA_YFT$batage # Biomass At Age: Esto son las matrices que hemos comentado: Tenemos Area (de 1 a 4), 
# Bio_Pattern, Sex, BirthSeas, Settlement, Platoon, Morph (ni caso, siempre 1)
# Yr (de 191 a 420 para el BET, son indices de quarter_year, 13 corresponde a 1950 y 301 a 2022). 
# Seas (siempre 1), Beg/Mid, esto se refiere al valor medio de ese quarter o al inicio (muy parecidos, elige uno),
# Era: Se refiere a la inicialización, No hay que tener en cuenta los dos primeros valores (las dos primeras filas)
# clases de edad de 0 a 28 (YFT), son cuartos, es decir, la edad máxima de biomasa explotada son 7 años. 
# Para diferenciar juveniles/adultos, solemos usar el 50% madurez. Primero obtenemos la talla del 50% de madurez de 
# SA_YFT$parameters, y luego ver a qué edad corresponde esa talla en el endgrowth. Te lo hago aqui:
x=which(SA_YFT$parameters$Label=="Mat50%_Fem_GP_1")
size50Mat=SA_YFT$parameters[x,"Value"]   # Esta es la talla del 50% madurez. Ahora lo pasamos a clase de edad.
age_length=SA_YFT$endgrowth[,c("Age_Beg", "Len_Mid")]  # de aquí puedes ver que los peces de talla 78 cm son peces de clase de edad superior a 9 (cuartos)
# es decir unos 2.5 años.

# Con esto entiendo que puedes obtener las matrices de biomasa x edad x año y separar juveniles y adultos.

Catch_at_age <- SA_YFT$catage # Esto son capturas por area, flota, sexo, tipo... La matriz es muy parecida a la anterior, te interesan el area, la flota, el año y el resto
# las edades de 0 a 28.

write.csv(Catch_at_age, 'YFT_catch_at_age.csv')

Biomass_at_age <- SA_YFT$batage # Esto son capturas por area, flota, sexo, tipo... La matriz es muy parecida a la anterior, te interesan el area, la flota, el año y el resto
# las edades de 0 a 28.

setwd("C:/Use/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/7. Stock_assessment/2. YFT")

write.csv(Biomass_at_age, 'YFT_Biomass_at_age.csv')

SA_YFT$definitions # Esto permite ver a que corresponde cada flota. En las evaluaciones no importa el país, sino el arte, que es el que describe la interacción
# con cada clase de talla


SA_YFT$definitions # Esto permite ver a que corresponde cada flota. En las evaluaciones no importa el país, sino el arte, que es el que describe la interacción
# con cada clase de talla

SS_plots(SA_YFT) # La función SS_plots hace automaticamente casi todas las figuras de la libreria con los datos.


#######################################################    Skipjack   ####################################################

SA_SKJ=SS_output(dir.SKJ, covar=FALSE)  # Esto lee todo el output del assessment del "Report.soo".

# Biomasss
Biomass_SKJ <- SA_SKJ$timeseries
write.csv(Biomass_SKJ, 'Biomass_SKJ.csv')

# Biomass at age
SA_SKJ$batage # Biomass At Age: Esto son las matrices que hemos comentado: Tenemos Area (de 1 a 4), 
# Bio_Pattern, Sex, BirthSeas, Settlement, Platoon, Morph (ni caso, siempre 1)
# Yr, 1950 corresponde a 1950 y 2022 a 2022). 
# Seas (siempre 1), Beg/Mid, esto se refiere al valor medio de ese quarter o al inicio (muy parecidos, elige uno),
# Era: Se refiere a la inicialización, No hay que tener en cuenta los dos primeros valores (las dos primeras filas)
# clases de edad de 0 a 8 (SKJ), son años. 
# Para diferenciar juveniles/adultos, solemos usar el 50% madurez. Primero obtenemos la talla del 50% de madurez de 
# SA_YFT$parameters, y luego ver a qué edad corresponde esa talla en el endgrowth. Te lo hago aqui:
x=which(SA_SKJ$parameters$Label=="Mat50%_Fem_GP_1")
size50Mat=SA_SKJ$parameters[x,"Value"]   # Esta es la talla del 50% madurez. Ahora lo pasamos a clase de edad.
age_length=SA_SKJ$endgrowth[,c("Age_Beg", "Len_Mid")]  # de aquí puedes ver que los peces de talla 38 cm son peces de clase de edad superior a 3 (cuartos)
# es decir <1 año de edad.

# Con esto entiendo que puedes obtener las matrices de biomasa x edad x año y separar juveniles y adultos.

Catch_at_age <- SA_SKJ$catage # Esto son capturas por area, flota, sexo, tipo... La matriz es muy parecida a la anterior, te interesan el area, la flota, el año y el resto
# las edades de 0 a 8.

Biomass_at_age <- SA_SKJ$batage # Esto son capturas por area, flota, sexo, tipo... La matriz es muy parecida a la anterior, te interesan el area, la flota, el año y el resto
# las edades de 0 a 8.

setwd("C:/Use/OneDrive - AZTI/1. Tesis/5. Data/1. IOTC/2. Workind data/7. Stock_assessment/3. SKJ")

write.csv(Biomass_at_age, 'SKJ_Biomass_at_age.csv')
write.csv(Catch_at_age, 'SKJ_catch_at_age.csv')
SA_SKJ$definitions # Esto permite ver a que corresponde cada flota. En las evaluaciones no importa el país, sino el arte, que es el que describe la interacción
# con cada clase de talla

SS_plots(SA_SKJ) # La función SS_plots hace automaticamente casi todas las figuras de la libreria con los datos.


#######################################################    SWORDFISH   ####################################################

SA_SWO=SS_output(dir.SWO, covar=FALSE)  # Esto lee todo el output del assessment del "Report.soo".

# Biomasss
Biomass_SWO <- SA_SWO$timeseries
write.csv(Biomass_SWO, 'Biomass_SWO.csv')

# Biomass at age
Biomass_at_age <- SA_SWO$batage # Biomass At Age: Esto son las matrices que hemos comentado: Tenemos Area (de 1 a 4), 
# Bio_Pattern, Sex, BirthSeas, Settlement, Platoon, Morph (ni caso, siempre 1)
# Yr (de 191 a 420 para el BET, son indices de quarter_year, 193 corresponde a 1975 y 381 a 2022). 
# Seas (siempre 1), Beg/Mid, esto se refiere al valor medio de ese quarter o al inicio (muy parecidos, elige uno),
# Era: Se refiere a la inicialización, no hagas caso.
# clases de edad de 0 a 40 (BET), son cuartos, es decir, la edad máxima de biomasa explotada son 10 años. 
write.csv(Biomass_at_age, 'SWO_biomass_at_age.csv')

Catch_at_age <- SA_SWO$catage # Esto son capturas por area, flota, sexo, tipo... La matriz es muy parecida a la anterior, te interesan el area, la flota, el año y el resto
# las edades de 0 a 40.
write.csv(Catch_at_age, 'SWO_catch_at_age.csv')

SA_SWO$definitions # Esto permite ver a que corresponde cada flota. En las evaluaciones no importa el país, sino el arte, que es el que describe la interacción
# con cada clase de talla

SS_plots(SA_SWO) # La función SS_plots hace automaticamente casi todas las figuras de la libreria con los datos.




#######################################################    ALBACORE   ####################################################

SA_ALB=SS_output(dir.ALB, covar=FALSE)  # Esto lee todo el output del assessment del "Report.soo".

# Biomasss
Biomass_ALB <- SA_ALB$timeseries
write.csv(Biomass_ALB, 'Biomass_ALB.csv')

# Biomass at age
Biomass_at_age <- SA_ALB$batage # Biomass At Age: Esto son las matrices que hemos comentado: Tenemos Area (de 1 a 4), 
# Bio_Pattern, Sex, BirthSeas, Settlement, Platoon, Morph (ni caso, siempre 1)
# Yr (de 191 a 420 para el BET, son indices de quarter_year, 193 corresponde a 1975 y 381 a 2022). 
# Seas (siempre 1), Beg/Mid, esto se refiere al valor medio de ese quarter o al inicio (muy parecidos, elige uno),
# Era: Se refiere a la inicialización, no hagas caso.
# clases de edad de 0 a 40 (BET), son cuartos, es decir, la edad máxima de biomasa explotada son 10 años. 
write.csv(Biomass_at_age, 'ALB_biomass_at_age.csv')

Catch_at_age <- SA_ALB$catage # Esto son capturas por area, flota, sexo, tipo... La matriz es muy parecida a la anterior, te interesan el area, la flota, el año y el resto
# las edades de 0 a 40.
write.csv(Catch_at_age, 'ALB_catch_at_age.csv')

SA_ALB$definitions # Esto permite ver a que corresponde cada flota. En las evaluaciones no importa el país, sino el arte, que es el que describe la interacción
# con cada clase de talla

SS_plots(SA_ALB) # La función SS_plots hace automaticamente casi todas las figuras de la libreria con los datos.




#######################################################    Blue Shark   ####################################################

SA_BSH=SS_output(dir.BSH, covar=FALSE)  # Esto lee todo el output del assessment del "Report.soo".

# Biomasss
Biomass_BSH <- SA_BSH$timeseries
write.csv(Biomass_BSH, 'Biomass_BSH.csv')

# Biomass at age
Biomass_at_age <- SA_BSH$batage # Biomass At Age: Esto son las matrices que hemos comentado: Tenemos Area (de 1 a 4), 
# Bio_Pattern, Sex, BirthSeas, Settlement, Platoon, Morph (ni caso, siempre 1)
# Yr (de 191 a 420 para el BET, son indices de quarter_year, 193 corresponde a 1975 y 381 a 2022). 
# Seas (siempre 1), Beg/Mid, esto se refiere al valor medio de ese quarter o al inicio (muy parecidos, elige uno),
# Era: Se refiere a la inicialización, no hagas caso.
# clases de edad de 0 a 40 (BET), son cuartos, es decir, la edad máxima de biomasa explotada son 10 años. 
write.csv(Biomass_at_age, 'BSH_biomass_at_age.csv')

Catch_at_age <- SA_BSH$catage # Esto son capturas por area, flota, sexo, tipo... La matriz es muy parecida a la anterior, te interesan el area, la flota, el año y el resto
# las edades de 0 a 40.
write.csv(Catch_at_age, 'BSH_catch_at_age.csv')

SA_BSH$definitions # Esto permite ver a que corresponde cada flota. En las evaluaciones no importa el país, sino el arte, que es el que describe la interacción
# con cada clase de talla

SS_plots(SA_BSH) # La función SS_plots hace automaticamente casi todas las figuras de la libreria con los datos.

