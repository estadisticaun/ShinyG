# Librerías ----

library("googledrive")
library(tidyverse) # version 1.2.1
library(readxl)    # version 1.0.0

options(digits = 10)

# Funciones  ----

# FUNCIÓN IMPORTAR DESDE EXCEL

importar <- function(Archivo, Periodo){
  read_excel(Archivo, sheet = Periodo, guess_max = 100000,  col_types = tipovar)}


# FUNCIÓN DE AGREGACIÓN POR DESAGREGACIONES TEMÁTICAS

Agregar <- function(poblacion, var){
  poblacion %>% group_by(.dots = c("YEAR", "SEMESTRE", var), .drop = FALSE) %>% 
    summarise(Total = n()) %>% 
    rename("Clase"=var) %>% 
    mutate(Variable = var) %>%
    select(Variable, YEAR, SEMESTRE, Clase, Total) %>%
    ungroup()
}


# FUNCIÓN DE TOTALES POR DESAGREGACIÓN TEMÁTICA

Totales <- function(poblacion){
  poblacion %>% group_by(YEAR, SEMESTRE, .drop = FALSE) %>%  summarise(Total = n()) %>% ungroup() %>%
    mutate(Variable="TOTAL", YEAR=YEAR, SEMESTRE=SEMESTRE, Clase = "Total", Total=Total) %>%
    select(Variable, YEAR, SEMESTRE, Clase, Total)
}

# Importar Microdatos ----

drive_download("Estadisticas UN/Graduados/P2009 Graduados.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Graduados/P2010 Graduados.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Graduados/P2011 Graduados.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Graduados/P2012 Graduados.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Graduados/P2013 Graduados.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Graduados/P2014 Graduados.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Graduados/P2015 Graduados.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Graduados/P2016 Graduados.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Graduados/P2017 Graduados.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Graduados/P20181 Graduados.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Graduados/P20182 Graduados.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Graduados/P20191 Graduados.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Graduados/P20192 Graduados.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Graduados/P20192 Graduados.xlsx", overwrite = TRUE)
drive_download("Estadisticas UN/Graduados/P20201 Graduados.xlsx", overwrite = TRUE)

# Definir tipo de las variables a importar

tipovar <- c("text", "text", "text", "text", "text", "text", "text", "numeric", 
             "text", "numeric", "text", "text", "text", "numeric", "text", "numeric", 
             "text", "text", "text", "numeric", "text", "numeric", "text", "text", 
             "text", "text", "text", "numeric", "text", "text", "text", "text", "numeric", 
             "text", "numeric", "text", "text", "text", "text", "text", "text", "text", 
             "text", "text", "numeric", "text", "text", "text", "numeric", "text", "text", 
             "text", "numeric", "numeric", "text")

# Función para importar microdatos desde Excel

G2009 <- importar("P2009 Graduados.xlsx", "P2009G") 
G2010 <- importar("P2010 Graduados.xlsx", "P2010G") 
G2011 <- importar("P2011 Graduados.xlsx", "P2011G") 
G2012 <- importar("P2012 Graduados.xlsx", "P2012G") 
G2013 <- importar("P2013 Graduados.xlsx", "P2013G") 
G2014 <- importar("P2014 Graduados.xlsx", "P2014G") 
G2015 <- importar("P2015 Graduados.xlsx", "P2015G") 
G2016 <- importar("P2016 Graduados.xlsx", "P2016G") 
G2017 <- importar("P2017 Graduados.xlsx", "P2017G") 
G20181 <- importar("P20181 Graduados.xlsx", "P20181G")
G20182 <- importar("P20182 Graduados.xlsx", "P20182G") 
G20191 <- importar("P20191 Graduados.xlsx", "P20191G") 
G20192 <- importar("P20192 Graduados.xlsx", "P20192G") 
G20201 <- importar("P20201 Graduados.xlsx", "P20201G") 


Microdatos <- bind_rows(G2009, G2010, G2011, G2012, G2013, G2014, G2015, G2016, G2017, G20181, G20182, G20191, G20192,
                        G20201)


Microdatos$LON_CIU_NAC	<- as.numeric(str_replace(Microdatos$LON_CIU_NAC, ",", "."))
Microdatos$LAT_CIU_NAC <- as.numeric(str_replace(Microdatos$LAT_CIU_NAC, ",", "."))
Microdatos$LON_CIU_PROC <- as.numeric(str_replace(Microdatos$LON_CIU_PROC, ",", "."))
Microdatos$LAT_CIU_PROC <- as.numeric(str_replace(Microdatos$LAT_CIU_PROC, ",", "."))


unlink(c('P2009 Graduados.xlsx', 'P2010 Graduados.xlsx', 'P2011 Graduados.xlsx',
         'P2012 Graduados.xlsx', 'P2013 Graduados.xlsx', 'P2014 Graduados.xlsx', 
         'P2015 Graduados.xlsX', 'P2016 Graduados.xlsx', 'P2017 Graduados.xlsx',
         'P20181 Graduados.xlsx', 'P20182 Graduados.xlsx', 'P20191 Graduados.xlsx',
         'P20192 Graduados.xlsx', 'P20201 Graduados.xlsx'))

# Transformaciones ----

Microdatos <- Microdatos %>% mutate(TIPO_NIVEL = if_else(is.na(TIPO_NIVEL), "Sin información", TIPO_NIVEL),
                                    NIVEL = if_else(is.na(NIVEL), "Sin información", NIVEL),
                                    SEDE_NOMBRE_ADM = if_else(is.na(SEDE_NOMBRE_ADM), "Sin información", SEDE_NOMBRE_ADM),
                                    SEDE_NOMBRE_MAT = if_else(is.na(SEDE_NOMBRE_MAT), "Sin información", SEDE_NOMBRE_MAT),
                                    FACULTAD = if_else(FACULTAD %in% c("Agronomía", "Ciencias grarias"), "Ciencias agrarias", FACULTAD),
                                    FACULTAD = if_else(FACULTAD %in% c("Ingenieria"), "Ingeniería", FACULTAD),
                                    FACULTAD = if_else(FACULTAD %in% c("Ciencias humanas  y económicas"), "Ciencias humanas y económicas", FACULTAD),
                                    FACULTAD = if_else(FACULTAD %in% c("Ciencias agropecuarias") & SEDE_NOMBRE_MAT == "Medellín" , "Ciencias agrarias", FACULTAD),
                                    FACULTAD = if_else(FACULTAD %in% c("Ingenieria y administración"), "Ingeniería y administración", FACULTAD),
                                    FACULTAD = if_else(FACULTAD %in% c("ingeniería y arquitectura"), "Ingeniería y arquitectura", FACULTAD),
                                    NACIONALIDAD = if_else(is.na(NACIONALIDAD), "Sin información", NACIONALIDAD),
                                    SEXO = if_else(is.na(SEXO), "Sin información", SEXO),
                                    CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD),
                                    ESTRATO = if_else(is.na(ESTRATO), "Sin información", ESTRATO),
                                    TIPO_COL = if_else(is.na(TIPO_COL), "Sin información", TIPO_COL),
                                    PBM = if_else(is.na(PBM), "Sin información", PBM),
                                    MAT_PVEZ = if_else(is.na(MAT_PVEZ), "Sin información", MAT_PVEZ),
                                    MOD_ADM = if_else(is.na(MOD_ADM), "Sin información", MOD_ADM),
                                    TIPO_ADM = if_else(is.na(TIPO_ADM), "Sin información", TIPO_ADM),
                                    PAES = if_else(is.na(PAES), "Sin información", PAES),
                                    PEAMA = if_else(is.na(PEAMA), "Sin información", PEAMA),
                                    MOV_PEAMA = if_else(is.na(MOV_PEAMA), "Sin información", MOV_PEAMA),
                                    CONVENIO = if_else(is.na(CONVENIO), "Sin información", CONVENIO),
                                    TIP_CONVENIO = if_else(is.na(TIP_CONVENIO), "Sin información", TIP_CONVENIO),
                                    AREAC_SNIES = if_else(is.na(AREAC_SNIES), "Sin información", AREAC_SNIES),
                                    NIVEL = if_else(NIVEL == "Especialidades  médicas", "Especialidades médicas", NIVEL),
                                    CAT_EDAD = if_else(CAT_EDAD == "26 o  más años", "26 o más años", CAT_EDAD)
                                    )


# CREAR FACTORES A PARTIR DE VARIABLES CUALITATIVAS

Microdatos$YEAR <- factor(Microdatos$YEAR, levels = c(2009:2020))
Microdatos$SEMESTRE <- factor(Microdatos$SEMESTRE, levels = c(1, 2))
Microdatos$TIPO_NIVEL <- factor(Microdatos$TIPO_NIVEL, levels = c('Postgrado', 'Pregrado'))
# Microdatos$NIVEL <- factor(Microdatos$NIVEL, levels = c('Doctorado', 'Especialidades médicas', 'Especialización', 'Maestría', 'Pregrado', 'Tecnología'))
Microdatos$SEDE_NOMBRE_ADM <- factor(Microdatos$SEDE_NOMBRE_ADM, levels = c('Amazonía', 'Bogotá', 'Caribe', 'La Paz', 'Manizales', 'Medellín', 'Orinoquía', 'Palmira', 'Tumaco'))
Microdatos$NACIONALIDAD <- factor(Microdatos$NACIONALIDAD, levels = c('Colombiana', 'Extranjero', 'Sin información'))
Microdatos$SEXO <- factor(Microdatos$SEXO, levels = c('Hombres', 'Mujeres'))
Microdatos$ESTRATO <- factor(Microdatos$ESTRATO, levels = c('Estrato 2 o menos', 'Estrato 3', 'Estrato 4 o más', 'ND/NE'))
Microdatos$TIPO_COL <- factor(Microdatos$TIPO_COL, levels = c('Oficial', 'Otros', 'Privado', 'Sin información'))
Microdatos$PBM <- factor(Microdatos$PBM, levels = c('11 o menos', '12 a 17', '18 a 50', '51 a 100', 'Sin información'))
Microdatos$MAT_PVEZ <- factor(Microdatos$MAT_PVEZ, levels = c('No', 'Sí'))
Microdatos$MOD_ADM <- factor(Microdatos$MOD_ADM, levels = c('Especial', 'Regular'))
# Microdatos$TIPO_ADM <- factor(Microdatos$TIPO_ADM, levels = c('PAES', 'PEAA', 'PEAMA', 'Regular'))
Microdatos$PAES <- factor(Microdatos$PAES, levels = c('Comunidades indígenas', 'Mejores bachilleres', 'Mejores bachilleres de municipios pobres', 'Población afrocolombiana', 'Victimas del conflicto armado interno en Colombia'))
# Microdatos$PEAMA <- factor(Microdatos$PEAMA, levels = c('PEAMA - Amazonía', 'PEAMA - Caribe', 'PEAMA - Medellín - Sinifaná', 'PEAMA - Orinoquía', 'PEAMA - Sede Bogotá - Sumapaz', 'PEAMA - Sede Manizales - Caldas', 'PEAMA - Tumaco'))
Microdatos$MOV_PEAMA <- factor(Microdatos$MOV_PEAMA, levels = c('Etapa de movilidad', 'Etapa Inicial'))
Microdatos$CONVENIO <- factor(Microdatos$CONVENIO, levels = c('No', 'Sí', 'Sin información'))
Microdatos$TIP_CONVENIO <- factor(Microdatos$TIP_CONVENIO, levels = c('Externo', 'Interno', 'Sin información'))
Microdatos$AREAC_SNIES <- factor(Microdatos$AREAC_SNIES, levels = c('Agronomía, veterinaria y afines', 'Bellas artes', 'Ciencias de la educación','Ciencias de la salud', 'Ciencias sociales y humanas', 'Economía, administración, contaduría y afines', 'Ingeniería, arquitectura, urbanismo y afines', 'Matemáticas y ciencias naturales'))


# Poblaciones ----

# Graduados

Gra_Nacional <- Microdatos # Total matriculados a nivel nacional
Gra_Bogota <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Bogotá") # Total matriculados Bogotá
Gra_Medellin <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Medellín") # Total matriculados Medellín
Gra_Manizales <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Manizales") # Total matriculados Manizales
Gra_Palmira <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Palmira") # Total matriculados Palmira

# Graduados en pregrado

Gra_Pre_Nacional <- Microdatos %>% filter(TIPO_NIVEL == "Pregrado") # Total matriculados pregrado a nivel nacional
Gra_Pre_Bogota <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Bogotá", TIPO_NIVEL == "Pregrado") # Total matriculados pregrado Bogotá
Gra_Pre_Medellin <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Medellín", TIPO_NIVEL == "Pregrado") # Total matriculados pregrado Medellín
Gra_Pre_Manizales <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Manizales", TIPO_NIVEL == "Pregrado") # Total matriculados pregrado Manizales
Gra_Pre_Palmira <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Palmira", TIPO_NIVEL == "Pregrado") # Total matriculados pregrado Palmira


# Graduados en postgrado

Gra_Pos_Nacional <- Microdatos %>% filter(TIPO_NIVEL == "Postgrado") # Total matriculados postgrado a nivel nacional
Gra_Pos_Bogota <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Bogotá", TIPO_NIVEL == "Postgrado") # Total matriculados postgrado Bogotá
Gra_Pos_Medellin <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Medellín", TIPO_NIVEL == "Postgrado") # Total matriculados postgrado Medellín
Gra_Pos_Manizales <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Manizales", TIPO_NIVEL == "Postgrado") # Total matriculados postgrado Manizales
Gra_Pos_Palmira <-  Microdatos %>% filter(SEDE_NOMBRE_MAT == "Palmira", TIPO_NIVEL == "Postgrado") # Total matriculados postgrado Palmira


# GRADUADOS 1100 ---- 

# Tabla agregada

DT1 <- Agregar(Gra_Nacional, 'TIPO_NIVEL')
DT2 <- Agregar(Gra_Nacional, 'NIVEL')
DT2 <- DT2 %>% mutate(Clase = if_else(Clase == "Especialidades Médicas", "Especialidades médicas", Clase))
DT3 <- Agregar(Gra_Nacional, 'SEDE_NOMBRE_ADM')
DT4 <- Agregar(Gra_Nacional, 'NACIONALIDAD')
DT4 <- DT4 %>% filter(YEAR != 2009)
DT5 <- Agregar(Gra_Nacional, 'SEXO')
DT6 <- Agregar(Gra_Nacional, 'AREAC_SNIES')
Total <- Totales(Gra_Nacional)


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Gra_Nacional %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Graduados Universidad

Gra1100 <- Agregado %>% 
               unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
               mutate(Id = as.numeric(Id)) %>% filter(Id <= Uperiodo) %>% 
               select(-Id) %>% mutate(Nivel = "Gra1100")


# Gra Bogotá 1101 ---- 

# Tabla agregada

DT1 <- Agregar(Gra_Bogota, 'TIPO_NIVEL')
DT2 <- Agregar(Gra_Bogota, 'NIVEL')
DT2 <- DT2 %>% mutate(Clase = if_else(Clase == "Especialidades Médicas", "Especialidades médicas", Clase))
DT3 <- Agregar(Gra_Bogota, 'FACULTAD')
DT4 <- Agregar(Gra_Bogota, 'NACIONALIDAD')
DT4 <- DT4 %>% filter(YEAR != 2009)
DT5 <- Agregar(Gra_Bogota, 'SEXO')
DT6 <- Agregar(Gra_Bogota, 'AREAC_SNIES')
Total <- Totales(Gra_Bogota)



# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Gra_Bogota %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

Gra1101 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = as.numeric(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "Gra1101")


# Gra Medellín 1102 ---- 

# Tabla agregada

DT1 <- Agregar(Gra_Medellin, 'TIPO_NIVEL')
DT2 <- Agregar(Gra_Medellin, 'NIVEL')
DT2 <- DT2 %>% mutate(Clase = if_else(Clase == "Especialidades Médicas", "Especialidades médicas", Clase))
DT3 <- Agregar(Gra_Medellin, 'FACULTAD')
DT3 <- DT3 %>% mutate(Clase = if_else(Clase == "Ciencias Agropecuarias", "Ciencias agrarias", Clase))
DT4 <- Agregar(Gra_Medellin, 'NACIONALIDAD')
DT4 <- DT4 %>% filter(YEAR != 2009)
DT5 <- Agregar(Gra_Medellin, 'SEXO')
DT6 <- Agregar(Gra_Medellin, 'AREAC_SNIES')
Total <- Totales(Gra_Medellin)


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Gra_Medellin %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

Gra1102 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = as.numeric(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "Gra1102")


# Gra Manizales 1103 ---- 

# Tabla agregada

DT1 <- Agregar(Gra_Manizales, 'TIPO_NIVEL')
DT2 <- Agregar(Gra_Manizales, 'NIVEL')
DT2 <- DT2 %>% mutate(Clase = if_else(Clase == "Especialidades Médicas", "Especialidades médicas", Clase))
DT3 <- Agregar(Gra_Manizales, 'FACULTAD')
DT4 <- Agregar(Gra_Manizales, 'NACIONALIDAD')
DT4 <- DT4 %>% filter(YEAR != 2009)
DT5 <- Agregar(Gra_Manizales, 'SEXO')
DT6 <- Agregar(Gra_Manizales, 'AREAC_SNIES')
Total <- Totales(Gra_Manizales)


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Gra_Manizales %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

Gra1103 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = as.numeric(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "Gra1103")

# Gra palmira 1104 ---- 

# Tabla agregada

DT1 <- Agregar(Gra_Palmira, 'TIPO_NIVEL')
DT2 <- Agregar(Gra_Palmira, 'NIVEL')
DT2 <- DT2 %>% mutate(Clase = if_else(Clase == "Especialidades Médicas", "Especialidades médicas", Clase))
DT3 <- Agregar(Gra_Palmira, 'FACULTAD')
DT4 <- Agregar(Gra_Palmira, 'NACIONALIDAD')
DT4 <- DT4 %>% filter(YEAR != 2009)
DT5 <- Agregar(Gra_Palmira, 'SEXO')
DT6 <- Agregar(Gra_Palmira, 'AREAC_SNIES')
Total <- Totales(Gra_Palmira)


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Gra_Palmira %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

Gra1104 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = as.numeric(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "Gra1104")


# GRADUADOS PRE 1100 ---- 

# Modificar niveles y crear factor para edad 


Gra_Pre_Nacional <- Gra_Pre_Nacional %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD)) %>% 
                                         mutate(CAT_EDAD = if_else(CAT_EDAD=="23 o menos", "23 años o menos", CAT_EDAD))
Gra_Pre_Nacional$CAT_EDAD <- factor(Gra_Pre_Nacional$CAT_EDAD, levels = c('23 años o menos', '24 a 25 años', '26 o más años', 'Sin información'))

# Tabla agregada

DT1 <- Agregar(Gra_Pre_Nacional, 'SEDE_NOMBRE_ADM')
DT2 <- Agregar(Gra_Pre_Nacional, 'NACIONALIDAD')
DT2 <- DT2 %>% filter(YEAR != 2009)
DT3 <- Agregar(Gra_Pre_Nacional, 'SEXO')
DT4 <- Agregar(Gra_Pre_Nacional, 'CAT_EDAD')
DT4 <- DT4 %>% filter(YEAR != 2009)
DT5 <- Agregar(Gra_Pre_Nacional, 'ESTRATO')
DT6 <- Agregar(Gra_Pre_Nacional, 'MOD_ADM')
DT7 <- Agregar(Gra_Pre_Nacional, 'TIPO_ADM')
DT8 <- Agregar(Gra_Pre_Nacional, 'PAES')
DT8 <- DT8 %>% filter(!is.na(Clase))
DT9 <- Agregar(Gra_Pre_Nacional, 'PEAMA')
DT9 <- DT9 %>% filter(Clase != "Sin información")
DT9 <- DT9 %>% filter(!YEAR %in% c(2009, 2010, 2011, 2012))
DT10 <- Agregar(Gra_Pre_Nacional, 'AREAC_SNIES')
Total <- Totales(Gra_Pre_Nacional)  


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Gra_Pre_Nacional %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados 

GraPre1100 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = as.numeric(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "GraPre1100")



# Gra Pre Bogotá 1101 ---- 

# Modificar niveles y crear factor para edad 


Gra_Pre_Bogota <- Gra_Pre_Bogota %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD)) %>% 
  mutate(CAT_EDAD = if_else(CAT_EDAD=="23 o menos", "23 años o menos", CAT_EDAD))
Gra_Pre_Bogota$CAT_EDAD <- factor(Gra_Pre_Bogota$CAT_EDAD, levels = c('23 años o menos', '24 a 25 años', '26 o más años', 'Sin información'))

# Tabla agregada

DT1 <- Agregar(Gra_Pre_Bogota, 'FACULTAD')
DT2 <- Agregar(Gra_Pre_Bogota, 'NACIONALIDAD')
DT2 <- DT2 %>% filter(YEAR != 2009)
DT3 <- Agregar(Gra_Pre_Bogota, 'SEXO')
DT4 <- Agregar(Gra_Pre_Bogota, 'CAT_EDAD')
DT4 <- DT4 %>% filter(YEAR != 2009)
DT5 <- Agregar(Gra_Pre_Bogota, 'ESTRATO')
DT6 <- Agregar(Gra_Pre_Bogota, 'MOD_ADM')
DT7 <- Agregar(Gra_Pre_Bogota, 'TIPO_ADM')
DT8 <- Agregar(Gra_Pre_Bogota, 'PAES')
DT8 <- DT8 %>% filter(!is.na(Clase))
DT9 <- Agregar(Gra_Pre_Bogota, 'PEAMA')
DT9 <- DT9 %>% filter(!YEAR %in% c(2009, 2010, 2011, 2012))
DT9 <- DT9 %>% filter(Clase != 'Sin información')
DT10 <- Agregar(Gra_Pre_Bogota, 'AREAC_SNIES')
Total <- Totales(Gra_Pre_Bogota)  


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Gra_Pre_Bogota %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados 

GraPre1101 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = as.numeric(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "GraPre1101")


# Gra Pre Medellín 1102 ---- 

# Modificar niveles y crear factor para edad 


Gra_Pre_Medellin <- Gra_Pre_Medellin %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD)) %>% 
  mutate(CAT_EDAD = if_else(CAT_EDAD=="23 o menos", "23 años o menos", CAT_EDAD))
Gra_Pre_Medellin$CAT_EDAD <- factor(Gra_Pre_Medellin$CAT_EDAD, levels = c('23 años o menos', '24 a 25 años', '26 o más años', 'Sin información'))

# Tabla agregada

DT1 <- Agregar(Gra_Pre_Medellin, 'FACULTAD')
DT1 <- DT1 %>% mutate(Clase = if_else(Clase == "Ciencias Agropecuarias", "Ciencias agrarias", Clase))
DT2 <- Agregar(Gra_Pre_Medellin, 'NACIONALIDAD')
DT2 <- DT2 %>% filter(YEAR != 2009)
DT3 <- Agregar(Gra_Pre_Medellin, 'SEXO')
DT4 <- Agregar(Gra_Pre_Medellin, 'CAT_EDAD')
DT4 <- DT4 %>% filter(YEAR != 2009)
DT5 <- Agregar(Gra_Pre_Medellin, 'ESTRATO')
DT6 <- Agregar(Gra_Pre_Medellin, 'MOD_ADM')
DT7 <- Agregar(Gra_Pre_Medellin, 'TIPO_ADM')
DT8 <- Agregar(Gra_Pre_Medellin, 'PAES')
DT8 <- DT8 %>% filter(!is.na(Clase))
DT9 <- Agregar(Gra_Pre_Medellin, 'PEAMA')
DT9 <- DT9 %>% filter(!YEAR %in% c(2009, 2010, 2011, 2012))
DT9 <- DT9 %>% filter(Clase != 'Sin información')
DT10 <- Agregar(Gra_Pre_Medellin, 'AREAC_SNIES')
Total <- Totales(Gra_Pre_Medellin)  


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Gra_Pre_Medellin %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados 

GraPre1102 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = as.numeric(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "GraPre1102")


# Gra Pre Manizales 1103 ---- 

# Modificar niveles y crear factor para edad 


Gra_Pre_Manizales <- Gra_Pre_Manizales %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD)) %>% 
  mutate(CAT_EDAD = if_else(CAT_EDAD=="23 o menos", "23 años o menos", CAT_EDAD))
Gra_Pre_Manizales$CAT_EDAD <- factor(Gra_Pre_Manizales$CAT_EDAD, levels = c('23 años o menos', '24 a 25 años', '26 o más años', 'Sin información'))

# Tabla agregada

DT1 <- Agregar(Gra_Pre_Manizales, 'FACULTAD')
DT2 <- Agregar(Gra_Pre_Manizales, 'NACIONALIDAD')
DT2 <- DT2 %>% filter(YEAR != 2009)
DT3 <- Agregar(Gra_Pre_Manizales, 'SEXO')
DT4 <- Agregar(Gra_Pre_Manizales, 'CAT_EDAD')
DT4 <- DT4 %>% filter(YEAR != 2009)
DT5 <- Agregar(Gra_Pre_Manizales, 'ESTRATO')
DT6 <- Agregar(Gra_Pre_Manizales, 'MOD_ADM')
DT7 <- Agregar(Gra_Pre_Manizales, 'TIPO_ADM')
DT8 <- Agregar(Gra_Pre_Manizales, 'PAES')
DT8 <- DT8 %>% filter(!is.na(Clase))
DT9 <- Agregar(Gra_Pre_Manizales, 'PEAMA')
DT9 <- DT9 %>% filter(!YEAR %in% c(2009, 2010, 2011, 2012))
DT9 <- DT9 %>% filter(Clase != 'Sin información')
DT10 <- Agregar(Gra_Pre_Manizales, 'AREAC_SNIES')
Total <- Totales(Gra_Pre_Manizales)  


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Gra_Pre_Manizales %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados 

GraPre1103 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = as.numeric(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "GraPre1103")


# Gra Pre Palmira 1104 ---- 

# Modificar niveles y crear factor para edad 


Gra_Pre_Palmira <- Gra_Pre_Palmira %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD)) %>% 
  mutate(CAT_EDAD = if_else(CAT_EDAD=="23 o menos", "23 años o menos", CAT_EDAD))
Gra_Pre_Palmira$CAT_EDAD <- factor(Gra_Pre_Palmira$CAT_EDAD, levels = c('23 años o menos', '24 a 25 años', '26 o más años', 'Sin información'))

# Tabla agregada

DT1 <- Agregar(Gra_Pre_Palmira, 'FACULTAD')
DT2 <- Agregar(Gra_Pre_Palmira, 'NACIONALIDAD')
DT2 <- DT2 %>% filter(YEAR != 2009)
DT3 <- Agregar(Gra_Pre_Palmira, 'SEXO')
DT4 <- Agregar(Gra_Pre_Palmira, 'CAT_EDAD')
DT4 <- DT4 %>% filter(YEAR != 2009)
DT5 <- Agregar(Gra_Pre_Palmira, 'ESTRATO')
DT6 <- Agregar(Gra_Pre_Palmira, 'MOD_ADM')
DT7 <- Agregar(Gra_Pre_Palmira, 'TIPO_ADM')
DT8 <- Agregar(Gra_Pre_Palmira, 'PAES')
DT8 <- DT8 %>% filter(!is.na(Clase))
DT9 <- Agregar(Gra_Pre_Palmira, 'PEAMA')
DT9 <- DT9 %>% filter(!YEAR %in% c(2009, 2010, 2011, 2012))
DT9 <- DT9 %>% filter(Clase != 'Sin información')
DT10 <- Agregar(Gra_Pre_Palmira, 'AREAC_SNIES')
Total <- Totales(Gra_Pre_Palmira)  


# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Gra_Pre_Palmira %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística Matriculados 

GraPre1104 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = as.numeric(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "GraPre1104")



# GRADUADOS POS 1100 ---- 

Gra_Pos_Nacional <- Gra_Pos_Nacional %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Gra_Pos_Nacional$CAT_EDAD <- factor(Gra_Pos_Nacional$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))


# Tabla agregada



DT1 <- Agregar(Gra_Pos_Nacional, "SEDE_NOMBRE_MAT")
DT1 <- DT1 %>% mutate(Clase = if_else(Clase == "Amazonia", "Amazonía", Clase))
DT2 <- Agregar(Gra_Pos_Nacional, "NIVEL")
DT2 <- DT2 %>% mutate(Clase = if_else(Clase == "Especialidades Médicas", "Especialidades médicas", Clase))
DT3 <- Agregar(Gra_Pos_Nacional, "NACIONALIDAD")
DT3 <- DT3 %>% filter(YEAR != 2009)
DT4 <- Agregar(Gra_Pos_Nacional, "SEXO")
DT5 <- Agregar(Gra_Pos_Nacional, "CAT_EDAD")
DT5 <- DT5 %>% filter(YEAR != 2009) 
DT6 <- Agregar(Gra_Pos_Nacional, "CONVENIO")
DT6 <- DT6 %>% filter(!YEAR %in% c(2009:2018))
DT7 <- Agregar(Gra_Pos_Nacional, "TIP_CONVENIO")
DT7 <- DT7 %>% filter(!YEAR %in% c(2009:2018) & Clase != "Sin información")
DT8 <- Agregar(Gra_Pos_Nacional, "AREAC_SNIES")
Total <- Totales(Gra_Pos_Nacional)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Gra_Pos_Nacional %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

GraPos1100 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = as.numeric(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "GraPos1100")


# Gra Pos Bogotá 1101 ---- 

Gra_Pos_Bogota <- Gra_Pos_Bogota %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Gra_Pos_Bogota$CAT_EDAD <- factor(Gra_Pos_Bogota$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))


# Tabla agregada


DT1 <- Agregar(Gra_Pos_Bogota, "FACULTAD")
DT2 <- Agregar(Gra_Pos_Bogota, "NIVEL")
DT2 <- DT2 %>% mutate(Clase = if_else(Clase == "Especialidades Médicas", "Especialidades médicas", Clase))
DT3 <- Agregar(Gra_Pos_Bogota, "NACIONALIDAD")
DT3 <- DT3 %>% filter(YEAR != 2009)
DT4 <- Agregar(Gra_Pos_Bogota, "SEXO")
DT5 <- Agregar(Gra_Pos_Bogota, "CAT_EDAD")
DT5 <- DT5 %>% filter(YEAR != 2009) 
DT6 <- Agregar(Gra_Pos_Bogota, "CONVENIO")
DT6 <- DT6 %>% filter(!YEAR %in% c(2009:2018))
DT7 <- Agregar(Gra_Pos_Bogota, "TIP_CONVENIO")
DT7 <- DT7 %>% filter(!YEAR %in% c(2009:2018) & Clase != "Sin información")
DT8 <- Agregar(Gra_Pos_Bogota, "AREAC_SNIES")
Total <- Totales(Gra_Pos_Bogota)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Gra_Pos_Bogota %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

GraPos1101 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = as.numeric(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "GraPos1101")


# Gra Pos Medellín 1102 ---- 

Gra_Pos_Medellin <- Gra_Pos_Medellin %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Gra_Pos_Medellin$CAT_EDAD <- factor(Gra_Pos_Medellin$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))


# Tabla agregada


DT1 <- Agregar(Gra_Pos_Medellin, "FACULTAD")
DT1 <- DT1 %>% mutate(Clase = if_else(Clase == "Ciencias Agropecuarias", "Ciencias agrarias", Clase))
DT2 <- Agregar(Gra_Pos_Medellin, "NIVEL")
DT2 <- DT2 %>% mutate(Clase = if_else(Clase == "Especialidades Médicas", "Especialidades médicas", Clase))
DT3 <- Agregar(Gra_Pos_Medellin, "NACIONALIDAD")
DT3 <- DT3 %>% filter(YEAR != 2009)
DT4 <- Agregar(Gra_Pos_Medellin, "SEXO")
DT5 <- Agregar(Gra_Pos_Medellin, "CAT_EDAD")
DT5 <- DT5 %>% filter(YEAR != 2009) 
DT6 <- Agregar(Gra_Pos_Medellin, "CONVENIO")
DT6 <- DT6 %>% filter(!YEAR %in% c(2009:2018))
DT7 <- Agregar(Gra_Pos_Medellin, "TIP_CONVENIO")
DT7 <- DT7 %>% filter(!YEAR %in% c(2009:2018) & Clase != "Sin información")
DT8 <- Agregar(Gra_Pos_Medellin, "AREAC_SNIES")
Total <- Totales(Gra_Pos_Medellin)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Gra_Pos_Medellin %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

GraPos1102 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = as.numeric(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "GraPos1102")



# Gra Pos Manizales 1103 ---- 

Gra_Pos_Manizales <- Gra_Pos_Manizales %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Gra_Pos_Manizales$CAT_EDAD <- factor(Gra_Pos_Manizales$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))


# Tabla agregada


DT1 <- Agregar(Gra_Pos_Manizales, "FACULTAD")
DT2 <- Agregar(Gra_Pos_Manizales, "NIVEL")
DT2 <- DT2 %>% mutate(Clase = if_else(Clase == "Especialidades Médicas", "Especialidades médicas", Clase))
DT3 <- Agregar(Gra_Pos_Manizales, "NACIONALIDAD")
DT3 <- DT3 %>% filter(YEAR != 2009)
DT4 <- Agregar(Gra_Pos_Manizales, "SEXO")
DT5 <- Agregar(Gra_Pos_Manizales, "CAT_EDAD")
DT5 <- DT5 %>% filter(YEAR != 2009) 
DT6 <- Agregar(Gra_Pos_Manizales, "CONVENIO")
DT6 <- DT6 %>% filter(!YEAR %in% c(2009:2018))
DT7 <- Agregar(Gra_Pos_Manizales, "TIP_CONVENIO")
DT7 <- DT7 %>% filter(!YEAR %in% c(2009:2018) & Clase != "Sin información")
DT8 <- Agregar(Gra_Pos_Manizales, "AREAC_SNIES")
Total <- Totales(Gra_Pos_Manizales)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Gra_Pos_Manizales %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

GraPos1103 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = as.numeric(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "GraPos1103")


# Gra Pos Palmira 1104 ---- 

Gra_Pos_Palmira <- Gra_Pos_Palmira %>% mutate(CAT_EDAD = if_else(is.na(CAT_EDAD), "Sin información", CAT_EDAD))
Gra_Pos_Palmira$CAT_EDAD <- factor(Gra_Pos_Palmira$CAT_EDAD, levels = c('25 años o menos', '26 a 30 años', '31 a 35 años', '36 años o más', 'Sin información'))


# Tabla agregada


DT1 <- Agregar(Gra_Pos_Palmira, "FACULTAD")
DT2 <- Agregar(Gra_Pos_Palmira, "NIVEL")
DT2 <- DT2 %>% mutate(Clase = if_else(Clase == "Especialidades Médicas", "Especialidades médicas", Clase))
DT3 <- Agregar(Gra_Pos_Palmira, "NACIONALIDAD")
DT3 <- DT3 %>% filter(YEAR != 2009)
DT4 <- Agregar(Gra_Pos_Palmira, "SEXO")
DT5 <- Agregar(Gra_Pos_Palmira, "CAT_EDAD")
DT5 <- DT5 %>% filter(YEAR != 2009) 
DT6 <- Agregar(Gra_Pos_Palmira, "CONVENIO")
DT6 <- DT6 %>% filter(!YEAR %in% c(2009:2018))
DT7 <- Agregar(Gra_Pos_Palmira, "TIP_CONVENIO")
DT7 <- DT7 %>% filter(!YEAR %in% c(2009:2018) & Clase != "Sin información")
DT8 <- Agregar(Gra_Pos_Palmira, "AREAC_SNIES")
Total <- Totales(Gra_Pos_Palmira)  

# Consolidado tabla agregada

Agregado <- bind_rows(DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, Total)


# Eliminar periodos sobrantes hacia adelante

# Último periodo

Uperiodo <- Gra_Pos_Palmira %>% unite("Id", YEAR, SEMESTRE, sep = "") %>%
  mutate(Id = as.numeric(Id)) %>% summarise(Id = max(Id))
Uperiodo <- Uperiodo[[1]]
Uperiodo

# Consolidado Estadística 

GraPos1104 <- Agregado %>% 
  unite("Id", YEAR, SEMESTRE, sep = "", remove = FALSE) %>%
  mutate(Id = as.numeric(Id)) %>% filter(Id <= Uperiodo) %>% 
  select(-Id) %>% mutate(Nivel = "GraPos1104")


# Consolidado Estadísticas ----


ConsolidadoG <- bind_rows(Gra1100, Gra1101, Gra1102, Gra1103, Gra1104,
                          GraPre1100, GraPre1101, GraPre1102, GraPre1103, GraPre1104, 
                          GraPos1100, GraPos1101, GraPos1102, GraPos1103, GraPos1104)

# Etiquetas último periodo ----

# Año
ano <- ConsolidadoG %>% unite("YEAR", YEAR, sep = "")%>%
  mutate(YEAR = as.numeric(YEAR))
ano <- max(ano %>% select(YEAR))

#Semestre
semestre <- ConsolidadoG %>% unite("SEMESTRE", SEMESTRE, sep = "")%>%
  mutate(SEMESTRE = as.numeric(SEMESTRE))

semestre <- semestre[[nrow(semestre), "SEMESTRE"]]

#Periodo Actual
periodo_actual_titulo <- paste0(" ", ano, "-", semestre)


ano
semestre
periodo_actual_titulo


# Remover archivos ----

rm(Agregado, DT1, DT2, DT3, DT4, DT5, DT6, DT7, DT8, DT9, DT10, 
   G2009, G2010, G2011, G2012, G2013, G2014, G2015, G2016, 
   G2017, G20181, G20182, G20191, G20192, G20201, Gra1100, Gra1101,
   Gra1102, Gra1103, Gra1104, GraPos1100, GraPos1101, GraPos1102,
   GraPos1103, GraPos1104, GraPre1100, GraPre1101, GraPre1102,
   GraPre1103, GraPre1104, importar, tipovar, Total)
   
   

   
    



