
# INICIO FASE DE VISUALIZACIÓN

# Librerías Requeridas ----

library(tidyverse)   # version 1.2.1
library(readxl)      # version 1.0.0
library(DT)          # version 0.4
library(highcharter) # version 0.5.0.9999
library(treemap) # version 2.4-2

# Importar Scripts ----

source("R/Importar Graduados.R", encoding = 'UTF-8')

source("R/Funciones.R", encoding = 'UTF-8')

# Funciones ----

# Exportar archivos HTML


Salvar <- function(objeto, ruta, nombre){
  saveWidget(objeto,
             file = file.path(str_sub(getwd(), 1, str_length(getwd())-12),
                              ruta,
                              nombre),
             selfcontained = F, libdir = "libraryjs")
  
}


# GRADUADOS ---- 

# Gra1100 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "Gra1100") %>% select(-(Nivel))


# Evolución histórica total de graduados ---

col <-   c("#0071bc") # Azul vivo, Total

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes graduados", eje = "Número de graduados (k: miles)");EVOLUCION_SERIE


# Modalidad de formación ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado

TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes graduados por modalidad de formación', mensaje = "Número de estudiantes graduados por modalidad de formación", titulo = "Graduados por modalidad de formación");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes graduados por modalidad de formación", eje = "Número de graduados  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col, titulo = "Distribución de graduados por modalidad de formación", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#fbb03b", # amarillo, Especialidades médicas
            "#29abe2", # azul claro, Especialización
            "#c1272d",  # rojo, Maestría
            "#8cc63f")  # verde, Pregrado
            

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes graduados por nivel de formación', mensaje = "Número de estudiantes graduados por nivel de formación", titulo = "Graduados por nivel de formación");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes graduados por nivel de formación", eje = "Número de graduados");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados por modalidad de formación", eje = "Número de graduados"); NIVEL_ACTUAL


# Sedes  ---


col <-   c( "#29abe2", # azul claro, Amazonía
            "#8cc63f", # verde, Bogotá
            "#c1272d", # rojo, Caribe 
            "#9e9ac8",  # Morado claro, De la Paz
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellín
            "#fbb03b", # amarillo, Orinoquía 
            "#93278f", # Morado, Palmira
            "#6d6666"  # gris, Tumaco 
) 

SEDE_TABLA <- tabla(datos = Consolidado, categoria = "SEDE_NOMBRE_ADM", variable = 'Total estudiantes graduados por sede de graduación', mensaje = "Total de estudiantes por sede de graduación", titulo = "Sede estudiantes graduados");SEDE_TABLA
SEDE_SERIE <- series(datos = Consolidado, categoria = "SEDE_NOMBRE_ADM", colores = col, titulo = "Evolución del número de graduados por sede", eje = "Número de graduados (k: miles)");SEDE_SERIE
SEDE_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "SEDE_NOMBRE_ADM", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados por sedes de la Universidad", eje = "Número de graduados"); SEDE_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes graduados según nacionalidad', mensaje = "Número de estudiantes graduados por nacionalidad", titulo = "Graduados según nacionalidad");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de graduados según nacionalidad", eje = "Número de graduados (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de graduados según nacionalidad", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total graduados por sexo', mensaje = "Número de graduados por sexo", titulo = "Graduados por sexo");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados por sexo", eje = "Número de graduados (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de graduados por sexo", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total graduados por áreas del conocimiento SNIES', mensaje = "Total de graduados por áreas del conocimiento SNIES", titulo = "Sede graduados por áreas del conocimiento SNIES");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de graduados por áreas del conocimiento SNIES", eje = "Número de graduados (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados por áreas del conocimiento SNIES", eje = "Número de graduados"); AREAC_SNIES_ACTUAL

# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Graduados/Nal/Graduados", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Graduados/Nal/Graduados", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Graduados/Nal/Graduados", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Graduados/Nal/Graduados", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Graduados/Nal/Graduados", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Graduados/Nal/Graduados", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Graduados/Nal/Graduados", "A_nivel.html")
Salvar(SEDE_TABLA, "G_Graduados/Nal/Graduados", "T_sede.html")
Salvar(SEDE_SERIE, "G_Graduados/Nal/Graduados", "S_sede.html")
Salvar(SEDE_ACTUAL, "G_Graduados/Nal/Graduados", "A_sede.html")
Salvar(NACIONALIDAD_TABLA, "G_Graduados/Nal/Graduados", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Graduados/Nal/Graduados", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Graduados/Nal/Graduados", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Graduados/Nal/Graduados", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Graduados/Nal/Graduados", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Graduados/Nal/Graduados", "A_sexo.html")
Salvar(AREAC_SNIES_TABLA, "G_Graduados/Nal/Graduados", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Graduados/Nal/Graduados", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Graduados/Nal/Graduados", "A_snies.html")


# Gra1101 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "Gra1101") %>% select(-(Nivel))


# Evolución histórica total de graduados ---

col <-   c("#8cc63f") # verde, Total

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes graduados - sede Bogotá", eje = "Número de graduados (k: miles)");EVOLUCION_SERIE


# Modalidad de formación ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado

TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes graduados por modalidad de formación - sede Bogotá', mensaje = "Número de estudiantes graduados por modalidad de formación - sede Bogotá", titulo = "Graduados por modalidad de formación - sede Bogotá");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes graduados por modalidad de formación - sede Bogotá", eje = "Número de graduados  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col, titulo = "Distribución de graduados por modalidad de formación - sede Bogotá", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#fbb03b", # amarillo, Especialidades médicas
            "#29abe2", # azul claro, Especialización
            "#c1272d",  # rojo, Maestría
            "#8cc63f")  # verde, Pregrado


NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes graduados por nivel de formación - sede Bogotá', mensaje = "Número de estudiantes graduados por nivel de formación - sede Bogotá", titulo = "Graduados por nivel de formación - sede Bogotá");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes graduados por nivel de formación - sede Bogotá", eje = "Número de graduados");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados por modalidad de formación - sede Bogotá", eje = "Número de graduados"); NIVEL_ACTUAL


# Facultad  ---


col <-   c( "#9e9ac8",  # Morado claro, Enfermería
            "#0071bc", # azul vivo, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias económicas
            "#f15a24", # naranja, Ciencias humanas
            "#fbb03b", # amarillo, Derecho, ciencias políticas y sociales
            "#93278f", # Morado, Artes 
            "#8cc63f", # verde, Ingeniería
            "#bdbdbd",  # Gris claro, Medicina 
            "#c1272d", # rojo, Medicina veterianaria y zootecnia
            "#99d8c9")  # Agua Marina, Odontología 
            


FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes graduados por facultad - sede Bogotá', mensaje = "Total de estudiantes graduados por facultad - sede Bogotá", titulo = "Facultad estudiantes graduados - sede Bogotá");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de graduados por facultad - sede Bogotá", eje = "Número de graduados (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados por facultad - sede Bogotá", eje = "Número de graduados"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes graduados según nacionalidad - sede Bogotá', mensaje = "Número de estudiantes graduados por nacionalidad - sede Bogotá", titulo = "Graduados según nacionalidad - sede Bogotá");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de graduados según nacionalidad - sede Bogotá", eje = "Número de graduados (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de graduados según nacionalidad - sede Bogotá", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total graduados por sexo - sede Bogotá', mensaje = "Número de graduados por sexo - sede Bogotá", titulo = "Graduados por sexo - sede Bogotá");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados por sexo - sede Bogotá", eje = "Número de graduados (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de graduados por sexo - sede Bogotá", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales   


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total graduados por áreas del conocimiento SNIES - sede Bogotá', mensaje = "Total de graduados por áreas del conocimiento SNIES - sede Bogotá", titulo = "Sede graduados por áreas del conocimiento SNIES - sede Bogotá");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de graduados por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de graduados (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de graduados"); AREAC_SNIES_ACTUAL

# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Graduados/Bog/Graduados", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Graduados/Bog/Graduados", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Graduados/Bog/Graduados", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Graduados/Bog/Graduados", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Graduados/Bog/Graduados", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Graduados/Bog/Graduados", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Graduados/Bog/Graduados", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Graduados/Bog/Graduados", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Graduados/Bog/Graduados", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Graduados/Bog/Graduados", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Graduados/Bog/Graduados", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Graduados/Bog/Graduados", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Graduados/Bog/Graduados", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Graduados/Bog/Graduados", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Graduados/Bog/Graduados", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Graduados/Bog/Graduados", "A_sexo.html")
Salvar(AREAC_SNIES_TABLA, "G_Graduados/Bog/Graduados", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Graduados/Bog/Graduados", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Graduados/Bog/Graduados", "A_snies.html")


# Gra1102 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "Gra1102") %>% select(-(Nivel))


# Evolución histórica total de graduados ---

col <-   c("#f15a24") # Naranja, Total

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes graduados - sede Medellín", eje = "Número de graduados (k: miles)");EVOLUCION_SERIE


# Modalidad de formación ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado

TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes graduados por modalidad de formación - sede Medellín', mensaje = "Número de estudiantes graduados por modalidad de formación - sede Medellín", titulo = "Graduados por modalidad de formación - sede Medellín");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes graduados por modalidad de formación - sede Medellín", eje = "Número de graduados  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col, titulo = "Distribución de graduados por modalidad de formación - sede Medellín", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d",  # rojo, Maestría
            "#8cc63f")  # verde, Pregrado


NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes graduados por nivel de formación - sede Medellín', mensaje = "Número de estudiantes graduados por nivel de formación - sede Medellín", titulo = "Graduados por nivel de formación - sede Medellín");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes graduados por nivel de formación - sede Medellín", eje = "Número de graduados");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados por modalidad de formación - sede Medellín", eje = "Número de graduados"); NIVEL_ACTUAL


# Facultad  ---


col <-   c( "#9e9ac8",  # Morado claro, Arquitectura
            "#f15a24", # naranja, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias humanas y económicas 
            "#8cc63f" # verde, Minas
)


FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes graduados por facultad - sede Medellín', mensaje = "Total de estudiantes graduados por facultad - sede Medellín", titulo = "Facultad estudiantes graduados - sede Medellín");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de graduados por facultad - sede Medellín", eje = "Número de graduados (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados por facultad - sede Medellín", eje = "Número de graduados"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes graduados según nacionalidad - sede Medellín', mensaje = "Número de estudiantes graduados por nacionalidad - sede Medellín", titulo = "Graduados según nacionalidad - sede Medellín");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de graduados según nacionalidad - sede Medellín", eje = "Número de graduados (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de graduados según nacionalidad - sede Medellín", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total graduados por sexo - sede Medellín', mensaje = "Número de graduados por sexo - sede Medellín", titulo = "Graduados por sexo - sede Medellín");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados por sexo - sede Medellín", eje = "Número de graduados (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de graduados por sexo - sede Medellín", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total graduados por áreas del conocimiento SNIES - sede Medellín', mensaje = "Total de graduados por áreas del conocimiento SNIES - sede Medellín", titulo = "Sede graduados por áreas del conocimiento SNIES - sede Medellín");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de graduados por áreas del conocimiento SNIES - sede Medellín", eje = "Número de graduados (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados por áreas del conocimiento SNIES - sede Medellín", eje = "Número de graduados"); AREAC_SNIES_ACTUAL

# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Graduados/Med/Graduados", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Graduados/Med/Graduados", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Graduados/Med/Graduados", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Graduados/Med/Graduados", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Graduados/Med/Graduados", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Graduados/Med/Graduados", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Graduados/Med/Graduados", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Graduados/Med/Graduados", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Graduados/Med/Graduados", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Graduados/Med/Graduados", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Graduados/Med/Graduados", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Graduados/Med/Graduados", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Graduados/Med/Graduados", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Graduados/Med/Graduados", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Graduados/Med/Graduados", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Graduados/Med/Graduados", "A_sexo.html")
Salvar(AREAC_SNIES_TABLA, "G_Graduados/Med/Graduados", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Graduados/Med/Graduados", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Graduados/Med/Graduados", "A_snies.html")


# Gra1103 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "Gra1103") %>% select(-(Nivel))


# Evolución histórica total de graduados ---


col <-   c("#0071bc") # Azul vivo, Total 


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes graduados - sede Manizales", eje = "Número de graduados (k: miles)");EVOLUCION_SERIE


# Modalidad de formación ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado

TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes graduados por modalidad de formación - sede Manizales', mensaje = "Número de estudiantes graduados por modalidad de formación - sede Manizales", titulo = "Graduados por modalidad de formación - sede Manizales");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes graduados por modalidad de formación - sede Manizales", eje = "Número de graduados  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col, titulo = "Distribución de graduados por modalidad de formación - sede Manizales", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d",  # rojo, Maestría
            "#8cc63f")  # verde, Pregrado


NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes graduados por nivel de formación - sede Manizales', mensaje = "Número de estudiantes graduados por nivel de formación - sede Manizales", titulo = "Graduados por nivel de formación - sede Manizales");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes graduados por nivel de formación - sede Manizales", eje = "Número de graduados");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados por modalidad de formación - sede Manizales", eje = "Número de graduados"); NIVEL_ACTUAL


# Facultad  ---


col <-   c( "#f15a24", # naranja, Administración
            "#0071bc", # azul vivo, Ciencias exactas y naturales
            "#8cc63f" # verde, Ingeniaría y arquitectura
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes graduados por facultad - sede Manizales', mensaje = "Total de estudiantes graduados por facultad - sede Manizales", titulo = "Facultad estudiantes graduados - sede Manizales");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de graduados por facultad - sede Manizales", eje = "Número de graduados (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados por facultad - sede Manizales", eje = "Número de graduados"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes graduados según nacionalidad - sede Manizales', mensaje = "Número de estudiantes graduados por nacionalidad - sede Manizales", titulo = "Graduados según nacionalidad - sede Manizales");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de graduados según nacionalidad - sede Manizales", eje = "Número de graduados (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de graduados según nacionalidad - sede Manizales", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total graduados por sexo - sede Manizales', mensaje = "Número de graduados por sexo - sede Manizales", titulo = "Graduados por sexo - sede Manizales");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados por sexo - sede Manizales", eje = "Número de graduados (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de graduados por sexo - sede Manizales", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total graduados por áreas del conocimiento SNIES - sede Manizales', mensaje = "Total de graduados por áreas del conocimiento SNIES - sede Manizales", titulo = "Sede graduados por áreas del conocimiento SNIES - sede Manizales");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de graduados por áreas del conocimiento SNIES - sede Manizales", eje = "Número de graduados (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados por áreas del conocimiento SNIES - sede Manizales", eje = "Número de graduados"); AREAC_SNIES_ACTUAL

# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Graduados/Man/Graduados", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Graduados/Man/Graduados", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Graduados/Man/Graduados", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Graduados/Man/Graduados", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Graduados/Man/Graduados", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Graduados/Man/Graduados", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Graduados/Man/Graduados", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Graduados/Man/Graduados", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Graduados/Man/Graduados", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Graduados/Man/Graduados", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Graduados/Man/Graduados", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Graduados/Man/Graduados", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Graduados/Man/Graduados", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Graduados/Man/Graduados", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Graduados/Man/Graduados", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Graduados/Man/Graduados", "A_sexo.html")
Salvar(AREAC_SNIES_TABLA, "G_Graduados/Man/Graduados", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Graduados/Man/Graduados", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Graduados/Man/Graduados", "A_snies.html")


# Gra1104 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "Gra1104") %>% select(-(Nivel))


# Evolución histórica total de graduados ---


col <-   c("#93278f") # Morado, Total


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes graduados - sede Palmira", eje = "Número de graduados (k: miles)");EVOLUCION_SERIE


# Modalidad de formación ---


col <-   c( "#f15a24", # naranja, Postgrado
            "#8cc63f") # verde, Pregrado

TIPO_NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_NIVEL", variable = 'Total estudiantes graduados por modalidad de formación - sede Palmira', mensaje = "Número de estudiantes graduados por modalidad de formación - sede Palmira", titulo = "Graduados por modalidad de formación - sede Palmira");TIPO_NIVEL_TABLA
TIPO_NIVEL_SERIE <- series(datos = Consolidado, categoria = "TIPO_NIVEL", colores = col, titulo = "Evolución del número de estudiantes graduados por modalidad de formación - sede Palmira", eje = "Número de graduados  (k: miles)");TIPO_NIVEL_SERIE
TIPO_NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_NIVEL", colores = col, titulo = "Distribución de graduados por modalidad de formación - sede Palmira", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_NIVEL_ACTUAL


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2", # azul claro, Especialización
            "#c1272d",  # rojo, Maestría
            "#8cc63f")  # verde, Pregrado


NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes graduados por nivel de formación - sede Palmira', mensaje = "Número de estudiantes graduados por nivel de formación - sede Palmira", titulo = "Graduados por nivel de formación - sede Palmira");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes graduados por nivel de formación - sede Palmira", eje = "Número de graduados");NIVEL_SERIE
NIVEL_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "NIVEL", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados por modalidad de formación - sede Palmira", eje = "Número de graduados"); NIVEL_ACTUAL


# Facultad  ---


col <-   c( "#0071bc", # azul vivo, Ciencias agropecuarias 
            "#8cc63f" # verde, Ingeniería y Administración
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes graduados por facultad - sede Palmira', mensaje = "Total de estudiantes graduados por facultad - sede Palmira", titulo = "Facultad estudiantes graduados - sede Palmira");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de graduados por facultad - sede Palmira", eje = "Número de graduados (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados por facultad - sede Palmira", eje = "Número de graduados"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes graduados según nacionalidad - sede Palmira', mensaje = "Número de estudiantes graduados por nacionalidad - sede Palmira", titulo = "Graduados según nacionalidad - sede Palmira");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de graduados según nacionalidad - sede Palmira", eje = "Número de graduados (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de graduados según nacionalidad - sede Palmira", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total graduados por sexo - sede Palmira', mensaje = "Número de graduados por sexo - sede Palmira", titulo = "Graduados por sexo - sede Palmira");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados por sexo - sede Palmira", eje = "Número de graduados (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de graduados por sexo - sede Palmira", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total graduados por áreas del conocimiento SNIES - sede Palmira', mensaje = "Total de graduados por áreas del conocimiento SNIES - sede Palmira", titulo = "Sede graduados por áreas del conocimiento SNIES - sede Palmira");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de graduados por áreas del conocimiento SNIES - sede Palmira", eje = "Número de graduados (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados por áreas del conocimiento SNIES - sede Palmira", eje = "Número de graduados"); AREAC_SNIES_ACTUAL

# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Graduados/Pal/Graduados", "Serie.html")
Salvar(TIPO_NIVEL_TABLA, "G_Graduados/Pal/Graduados", "T_modalidad.html")
Salvar(TIPO_NIVEL_SERIE, "G_Graduados/Pal/Graduados", "S_modalidad.html")
Salvar(TIPO_NIVEL_ACTUAL, "G_Graduados/Pal/Graduados", "A_modalidad.html")
Salvar(NIVEL_TABLA, "G_Graduados/Pal/Graduados", "T_nivel.html")
Salvar(NIVEL_SERIE, "G_Graduados/Pal/Graduados", "S_nivel.html")
Salvar(NIVEL_ACTUAL, "G_Graduados/Pal/Graduados", "A_nivel.html")
Salvar(FACULTAD_TABLA, "G_Graduados/Pal/Graduados", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Graduados/Pal/Graduados", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Graduados/Pal/Graduados", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Graduados/Pal/Graduados", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Graduados/Pal/Graduados", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Graduados/Pal/Graduados", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Graduados/Pal/Graduados", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Graduados/Pal/Graduados", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Graduados/Pal/Graduados", "A_sexo.html")
Salvar(AREAC_SNIES_TABLA, "G_Graduados/Pal/Graduados", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Graduados/Pal/Graduados", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Graduados/Pal/Graduados", "A_snies.html")


# GRADUADOS PREGRADO ---- 

# GraPre1100 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "GraPre1100") %>% select(-(Nivel))


# Evolución histórica ---

col <-   c("#8cc63f") # verde, Total

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes graduados en pregrado ", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Sedes  ---


col <-   c( "#29abe2", # azul claro, Amazonía
            "#8cc63f", # verde, Bogotá
            "#c1272d", # rojo, Caribe 
            "#9e9ac8",  # Morado claro, De la Paz
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellín
            "#fbb03b", # amarillo, Orinoquía 
            "#93278f", # Morado, Palmira
            "#6d6666"  # gris, Tumaco 
) 


SEDE_TABLA <- tabla(datos = Consolidado, categoria = "SEDE_NOMBRE_ADM", variable = 'Total estudiantes en pregrado por sede de graduación', mensaje = "Total de estudiantes en pregrado por sede de graduación", titulo = "Sede estudiantes graduados pregrado");SEDE_TABLA
SEDE_SERIE <- series(datos = Consolidado, categoria = "SEDE_NOMBRE_ADM", colores = col, titulo = "Evolución del número de graduados en pregrado por sedes", eje = "Número de graduados (k: miles)");SEDE_SERIE
SEDE_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "SEDE_NOMBRE_ADM", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por sedes de la Universidad",eje = "Número de graduados"); SEDE_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información



NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes graduados en pregrado según nacionalidad', mensaje = "Número de estudiantes graduados en pregrado por nacionalidad", titulo = "Matriculados según nacionalidad");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de graduados en pregrado según nacionalidad", eje = "Número de graduados (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de graduados en pregrado según nacionalidad", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total graduados en pregrado por sexo', mensaje = "Número de graduados en pregrado por sexo", titulo = "Graduados en pregrado por sexo");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados en pregrado por sexo", eje = "Número de graduados (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de graduados en pregrado por sexo", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total graduados en pregrado por grupos de edad', mensaje = "Número de graduados en pregrado por grupos de edad", titulo = "Graduados en pregrado por grupos de edad");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de graduados en pregrado por grupos de edad", eje = "Número de graduados (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por grupos de edad", eje = "Número de graduados (k: miles)"); CAT_EDAD_ACTUAL


# Estrato ---

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)


ESTRATO_TABLA <- tabla(datos = Consolidado, categoria = "ESTRATO", variable = 'Total de graduados en pregrado por estrato socioeconómico', mensaje = "Número de graduados en pregrado por estrato", titulo = "Graduados en pregrado por estrato");ESTRATO_TABLA
ESTRATO_SERIE <- series(datos = Consolidado, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de graduados en pregrado por estrato socioeconómico", eje = "Número de graduados (k: miles)");ESTRATO_SERIE
ESTRATO_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por estrato socioeconómico", eje = "Número de graduados (k: miles)"); ESTRATO_ACTUAL


# Modalidad de admisión ---

col <-   c( "#f15a24",  # naranja, Especial
            "#8cc63f" # verde, Regular
)


MOD_ADM_TABLA <- tabla(datos = Consolidado, categoria = "MOD_ADM", variable = 'Total graduados en pregrado según modalidad de admisión', mensaje = "Número de graduados en pregrado según modalidad de admisión", titulo = "Graduados en pregrado según modalidad de admisión");MOD_ADM_TABLA
MOD_ADM_SERIE <- series(datos = Consolidado, categoria = "MOD_ADM", colores = col, titulo = "Evolución del número de graduados en pregrado por modalidad de admisión", eje = "Número de graduados (k: miles)");MOD_ADM_SERIE
MOD_ADM_ACTUAL <- torta(datos = Consolidado, variable = "MOD_ADM", colores = col, titulo = "Distribución de graduados en pregrado según modalidad de admisión", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MOD_ADM_ACTUAL


# tipo de admisión ---

col <-   c( "#f15a24", # naranja, PAES
            "#6d6666", # gris, PEAA
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f" # verde, Regular
)


TIPO_ADM_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_ADM", variable = 'Total graduados en pregrado por programa de admisión', mensaje = "Número de graduados en pregrado por programa de admisión", titulo = "Graduados en pregrado por programa de admisión");TIPO_ADM_TABLA
TIPO_ADM_SERIE <- series(datos = Consolidado, categoria = "TIPO_ADM", colores = col, titulo = "Evolución del número de graduados en pregrado por programa de admisión", eje = "Número de graduados (k: miles)");TIPO_ADM_SERIE
TIPO_ADM_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_ADM", colores = col, titulo = "Distribución de graduados en pregrado por programa de admisión", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_ADM_ACTUAL


# PAES ---


col <-   c( "#0071bc", # azul vivo, comunidades indigenas
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666",  # gris, mejores bachilleres municipios pobres 
            "#f15a24", # naranja, población afro
            "#8cc63f"  # verde, victimas del conflicto
)


PAES_TABLA <- tabla(datos = Consolidado, categoria = "PAES", variable = 'Total graduados en pregrado programa PAES', mensaje = "Número de graduados en pregrado del programa PAES", titulo = "Graduados en pregrado del programa PAES");PAES_TABLA
PAES_SERIE <- series(datos = Consolidado, categoria = "PAES", colores = col, titulo = "Evolución del número de graduados en pregrado del programa PAES", eje = "Número de graduados (k: miles)");PAES_SERIE
PAES_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "PAES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado del programa PAES", eje = "Número de graduados (k: miles)"); PAES_ACTUAL


# PEAMA ---

col <-   c( "#29abe2", # azul claro, Amazonía
            "#c1272d", # Rojo, Caribe
            "#fbb03b", # amarillo, Orinoquia
            "#f15a24", # naranja, Medellín - Sinifaná
            "#8cc63f", # verde, Bogotá - Sumapaz
            "#0071bc", # azul vivo, Manizales
            "#6d6666" # gris, Tumaco
)


PEAMA_TABLA <- tabla(datos = Consolidado, categoria = "PEAMA", variable = 'Total graduados en pregrado programa PEAMA', mensaje = "Número de graduados en pregrado programa PEAMA", titulo = "Graduados en pregrado programa PEAMA");PEAMA_TABLA
PEAMA_SERIE <- series(datos = Consolidado, categoria = "PEAMA", colores = col, titulo = "Evolución del número de graduados en pregrado del programa PEAMA", eje = "Número de graduados");PEAMA_SERIE
PEAMA_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "PEAMA", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado del programa PEAMA", eje = "Número de graduados"); PEAMA_ACTUAL




# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total graduados en pregrado por áreas del conocimiento SNIES', mensaje = "Total de graduados en pregrado por áreas del conocimiento SNIES", titulo = "Sede graduados en pregrado por áreas del conocimiento SNIES");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de graduados en pregrado por áreas del conocimiento SNIES", eje = "Número de graduados (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por áreas del conocimiento SNIES", eje = "Número de graduados"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Graduados/Nal/Pregrado", "Serie.html")
Salvar(SEDE_TABLA, "G_Graduados/Nal/Pregrado", "T_sede.html")
Salvar(SEDE_SERIE, "G_Graduados/Nal/Pregrado", "S_sede.html")
Salvar(SEDE_ACTUAL, "G_Graduados/Nal/Pregrado", "A_sede.html")
Salvar(NACIONALIDAD_TABLA, "G_Graduados/Nal/Pregrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Graduados/Nal/Pregrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Graduados/Nal/Pregrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Graduados/Nal/Pregrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Graduados/Nal/Pregrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Graduados/Nal/Pregrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Graduados/Nal/Pregrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Graduados/Nal/Pregrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Graduados/Nal/Pregrado", "A_edad.html")
Salvar(ESTRATO_TABLA, "G_Graduados/Nal/Pregrado", "T_estrato.html")
Salvar(ESTRATO_SERIE, "G_Graduados/Nal/Pregrado", "S_estrato.html")
Salvar(ESTRATO_ACTUAL, "G_Graduados/Nal/Pregrado", "A_estrato.html")
Salvar(MOD_ADM_TABLA, "G_Graduados/Nal/Pregrado", "T_modalidad.html")
Salvar(MOD_ADM_SERIE, "G_Graduados/Nal/Pregrado", "S_modalidad.html")
Salvar(MOD_ADM_ACTUAL, "G_Graduados/Nal/Pregrado", "A_modalidad.html")
Salvar(TIPO_ADM_TABLA, "G_Graduados/Nal/Pregrado", "T_tipo.html")
Salvar(TIPO_ADM_SERIE, "G_Graduados/Nal/Pregrado", "S_tipo.html")
Salvar(TIPO_ADM_ACTUAL, "G_Graduados/Nal/Pregrado", "A_tipo.html")
Salvar(PAES_TABLA, "G_Graduados/Nal/Pregrado", "T_paes.html")
Salvar(PAES_SERIE, "G_Graduados/Nal/Pregrado", "S_paes.html")
Salvar(PAES_ACTUAL, "G_Graduados/Nal/Pregrado", "A_paes.html")
Salvar(PEAMA_TABLA, "G_Graduados/Nal/Pregrado", "T_peama.html")
Salvar(PEAMA_SERIE, "G_Graduados/Nal/Pregrado", "S_peama.html")
Salvar(PEAMA_ACTUAL, "G_Graduados/Nal/Pregrado", "A_peama.html")
Salvar(AREAC_SNIES_TABLA, "G_Graduados/Nal/Pregrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Graduados/Nal/Pregrado", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Graduados/Nal/Pregrado", "A_snies.html")




# GraPre1101 ----


Consolidado <- ConsolidadoG %>% filter(Nivel == "GraPre1101") %>% select(-(Nivel))

# Evolución histórica ---


col <-   c("#8cc63f") # verde, Total


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes graduados en pregrado - sede Bogotá", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Facultad  ---


col <-   c( "#9e9ac8",  # Morado claro, Enfermería
            "#0071bc", # azul vivo, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias económicas
            "#f15a24", # naranja, Ciencias humanas
            "#fbb03b", # amarillo, Derecho, ciencias políticas y sociales
            "#93278f", # Morado, Artes 
            "#8cc63f", # verde, Ingeniería
            "#bdbdbd",  # Gris claro, Medicina 
            "#c1272d", # rojo, Medicina veterianaria y zootecnia
            "#99d8c9"  # Agua Marina, Odontología 
            
)


FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes graduados en pregrado por facultad - sede Bogotá', mensaje = "Total de estudiantes graduados en pregrado por facultad - sede Bogotá", titulo = "Facultad de estudiantes graduados en pregrado - sede Bogotá");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de graduados en pregrado por facultad - sede Bogotá", eje = "Número de graduados (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por facultad - sede Bogotá",eje = "Número de graduados"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información



NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total graduados en pregrado según nacionalidad - sede Bogotá', mensaje = "Número de graduados en pregrado por nacionalidad - sede Bogotá", titulo = "Graduados en pregrado según nacionalidad - sede Bogotá");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de graduados en pregrado según nacionalidad - sede Bogotá", eje = "Número de graduados (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de graduados en pregrado según nacionalidad - sede Bogotá", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total graduados en pregrado por sexo - sede Bogotá', mensaje = "Número de graduados en pregrado por sexo - sede Bogotá", titulo = "Graduados en pregrado por sexo - sede Bogotá");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados en pregrado por sexo - sede Bogotá", eje = "Número de graduados (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de graduados en pregrado por sexo - sede Bogotá", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total graduados en pregrado por grupos de edad - sede Bogotá', mensaje = "Número de graduados en pregrado por grupos de edad - sede Bogotá", titulo = "Graduados en pregrado por grupos de edad - sede Bogotá");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de graduados en pregrado por grupos de edad - sede Bogotá", eje = "Número de graduados (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por grupos de edad - sede Bogotá", eje = "Número de graduados (k: miles)"); CAT_EDAD_ACTUAL


# Estrato ---

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)


ESTRATO_TABLA <- tabla(datos = Consolidado, categoria = "ESTRATO", variable = 'Total de graduados en pregrado por estrato socioeconómico - sede Bogotá', mensaje = "Número de graduados en pregrado por estrato socioeconómico - sede Bogotá", titulo = "Graduados en pregrado por estrato socioeconómico - sede Bogotá");ESTRATO_TABLA
ESTRATO_SERIE <- series(datos = Consolidado, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de graduados en pregrado por estrato socioeconómico - sede Bogotá", eje = "Número de graduados (k: miles)");ESTRATO_SERIE
ESTRATO_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por estrato socioeconómico - sede Bogotá", eje = "Número de graduados (k: miles)"); ESTRATO_ACTUAL


# Modalidad de admisión ---

col <-   c( "#f15a24",  # naranja, Especial
            "#8cc63f" # verde, Regular
)


MOD_ADM_TABLA <- tabla(datos = Consolidado, categoria = "MOD_ADM", variable = 'Total graduados en pregrado según modalidad de admisión - sede Bogotá', mensaje = "Número de graduados en pregrado según modalidad de admisión - sede Bogotá", titulo = "Graduados en pregrado según modalidad de admisión - sede Bogotá");MOD_ADM_TABLA
MOD_ADM_SERIE <- series(datos = Consolidado, categoria = "MOD_ADM", colores = col, titulo = "Evolución del número de graduados en pregrado por modalidad de admisión - sede Bogotá", eje = "Número de graduados (k: miles)");MOD_ADM_SERIE
MOD_ADM_ACTUAL <- torta(datos = Consolidado, variable = "MOD_ADM", colores = col, titulo = "Distribución de graduados en pregrado según modalidad de admisión - sede Bogotá", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MOD_ADM_ACTUAL


# tipo de admisión ---

col <-   c( "#f15a24", # naranja, PAES
            "#6d6666", # gris, PEAA
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f" # verde, Regular
)


TIPO_ADM_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_ADM", variable = 'Total graduados en pregrado por programa de admisión - sede Bogotá', mensaje = "Número de graduados en pregrado por programa de admisión - sede Bogotá", titulo = "Graduados en pregrado por programa de admisión - sede Bogotá");TIPO_ADM_TABLA
TIPO_ADM_SERIE <- series(datos = Consolidado, categoria = "TIPO_ADM", colores = col, titulo = "Evolución del número de graduados en pregrado por programa de admisión - sede Bogotá", eje = "Número de graduados (k: miles)");TIPO_ADM_SERIE
TIPO_ADM_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_ADM", colores = col, titulo = "Distribución de graduados en pregrado por programa de admisión - sede Bogotá", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_ADM_ACTUAL


# PAES ---


col <-   c( "#0071bc", # azul vivo, comunidades indigenas
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666",  # gris, mejores bachilleres municipios pobres 
            "#f15a24", # naranja, población afro
            "#8cc63f"  # verde, victimas del conflicto
)


PAES_TABLA <- tabla(datos = Consolidado, categoria = "PAES", variable = 'Total de graduados en pregrado del programa PAES - sede Bogotá', mensaje = "Número de graduados en pregrado del programa PAES - sede Bogotá", titulo = "Graduados en pregrado del programa PAES - sede Bogotá");PAES_TABLA
PAES_SERIE <- series(datos = Consolidado, categoria = "PAES", colores = col, titulo = "Evolución del número de graduados en pregrado del programa PAES - sede Bogotá", eje = "Número de graduados (k: miles)");PAES_SERIE
PAES_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "PAES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado del programa PAES - sede Bogotá", eje = "Número de graduados (k: miles)"); PAES_ACTUAL


# PEAMA ---

col <-   c( "#29abe2", # azul claro, Amazonía
            "#c1272d", # Rojo, Caribe
            "#fbb03b", # amarillo, Orinoquia
            "#f15a24", # naranja, Medellín - Sinifaná
            "#8cc63f", # verde, Bogotá - Sumapaz
            "#0071bc", # azul vivo, Manizales
            "#6d6666" # gris, Tumaco
)


PEAMA_TABLA <- tabla(datos = Consolidado, categoria = "PEAMA", variable = 'Total de graduados en pregrado del programa PEAMA - sede Bogotá', mensaje = "Número de graduados en pregrado del programa PEAMA - sede Bogotá", titulo = "Graduados en pregrado del programa PEAMA - sede Bogotá");PEAMA_TABLA
PEAMA_SERIE <- series(datos = Consolidado, categoria = "PEAMA", colores = col, titulo = "Evolución del número de graduados en pregrado del programa PEAMA - sede Bogotá", eje = "Número de graduados");PEAMA_SERIE
PEAMA_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "PEAMA", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado del programa PEAMA - sede Bogotá", eje = "Número de graduados"); PEAMA_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total graduados en pregrado por áreas del conocimiento SNIES - sede Bogotá', mensaje = "Total de graduados en pregrado por áreas del conocimiento SNIES - sede Bogotá", titulo = "Total de graduados en pregrado por áreas del conocimiento SNIES - sede Bogotá");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de graduados en pregrado por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de graduados (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de graduados"); AREAC_SNIES_ACTUAL


# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Graduados/Bog/Pregrado", "Serie.html")
Salvar(FACULTAD_TABLA, "G_Graduados/Bog/Pregrado", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Graduados/Bog/Pregrado", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Graduados/Bog/Pregrado", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Graduados/Bog/Pregrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Graduados/Bog/Pregrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Graduados/Bog/Pregrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Graduados/Bog/Pregrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Graduados/Bog/Pregrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Graduados/Bog/Pregrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Graduados/Bog/Pregrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Graduados/Bog/Pregrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Graduados/Bog/Pregrado", "A_edad.html")
Salvar(ESTRATO_TABLA, "G_Graduados/Bog/Pregrado", "T_estrato.html")
Salvar(ESTRATO_SERIE, "G_Graduados/Bog/Pregrado", "S_estrato.html")
Salvar(ESTRATO_ACTUAL, "G_Graduados/Bog/Pregrado", "A_estrato.html")
Salvar(MOD_ADM_TABLA, "G_Graduados/Bog/Pregrado", "T_modalidad.html")
Salvar(MOD_ADM_SERIE, "G_Graduados/Bog/Pregrado", "S_modalidad.html")
Salvar(MOD_ADM_ACTUAL, "G_Graduados/Bog/Pregrado", "A_modalidad.html")
Salvar(TIPO_ADM_TABLA, "G_Graduados/Bog/Pregrado", "T_tipo.html")
Salvar(TIPO_ADM_SERIE, "G_Graduados/Bog/Pregrado", "S_tipo.html")
Salvar(TIPO_ADM_ACTUAL, "G_Graduados/Bog/Pregrado", "A_tipo.html")
Salvar(PAES_TABLA, "G_Graduados/Bog/Pregrado", "T_paes.html")
Salvar(PAES_SERIE, "G_Graduados/Bog/Pregrado", "S_paes.html")
Salvar(PAES_ACTUAL, "G_Graduados/Bog/Pregrado", "A_paes.html")
Salvar(PEAMA_TABLA, "G_Graduados/Bog/Pregrado", "T_peama.html")
Salvar(PEAMA_SERIE, "G_Graduados/Bog/Pregrado", "S_peama.html")
Salvar(PEAMA_ACTUAL, "G_Graduados/Bog/Pregrado", "A_peama.html")
Salvar(AREAC_SNIES_TABLA, "G_Graduados/Bog/Pregrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Graduados/Bog/Pregrado", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Graduados/Bog/Pregrado", "A_snies.html")


# GraPre1102 ----


Consolidado <- ConsolidadoG %>% filter(Nivel == "GraPre1102") %>% select(-(Nivel))

# Evolución histórica ---


col <-   c("#f15a24") # Naranja, Total


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes graduados en pregrado - sede Medellín", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Facultad  ---


col <-   c( "#9e9ac8",  # Morado claro, Arquitectura
            "#f15a24", # naranja, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias humanas y económicas 
            "#8cc63f" # verde, Minas
)


FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes graduados en pregrado por facultad - sede Medellín', mensaje = "Total de estudiantes graduados en pregrado por facultad - sede Medellín", titulo = "Facultad de estudiantes graduados en pregrado - sede Medellín");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de graduados en pregrado por facultad - sede Medellín", eje = "Número de graduados (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por facultad - sede Medellín",eje = "Número de graduados"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información



NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total graduados en pregrado según nacionalidad - sede Medellín', mensaje = "Número de graduados en pregrado por nacionalidad - sede Medellín", titulo = "Graduados en pregrado según nacionalidad - sede Medellín");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de graduados en pregrado según nacionalidad - sede Medellín", eje = "Número de graduados (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de graduados en pregrado según nacionalidad - sede Medellín", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total graduados en pregrado por sexo - sede Medellín', mensaje = "Número de graduados en pregrado por sexo - sede Medellín", titulo = "Graduados en pregrado por sexo - sede Medellín");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados en pregrado por sexo - sede Medellín", eje = "Número de graduados (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de graduados en pregrado por sexo - sede Medellín", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total graduados en pregrado por grupos de edad - sede Medellín', mensaje = "Número de graduados en pregrado por grupos de edad - sede Medellín", titulo = "Graduados en pregrado por grupos de edad - sede Medellín");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de graduados en pregrado por grupos de edad - sede Medellín", eje = "Número de graduados (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por grupos de edad - sede Medellín", eje = "Número de graduados (k: miles)"); CAT_EDAD_ACTUAL


# Estrato ---

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)


ESTRATO_TABLA <- tabla(datos = Consolidado, categoria = "ESTRATO", variable = 'Total de graduados en pregrado por estrato socioeconómico - sede Medellín', mensaje = "Número de graduados en pregrado por estrato socioeconómico - sede Medellín", titulo = "Graduados en pregrado por estrato socioeconómico - sede Medellín");ESTRATO_TABLA
ESTRATO_SERIE <- series(datos = Consolidado, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de graduados en pregrado por estrato socioeconómico - sede Medellín", eje = "Número de graduados (k: miles)");ESTRATO_SERIE
ESTRATO_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por estrato socioeconómico - sede Medellín", eje = "Número de graduados (k: miles)"); ESTRATO_ACTUAL


# Modalidad de admisión ---

col <-   c( "#f15a24",  # naranja, Especial
            "#8cc63f" # verde, Regular
)


MOD_ADM_TABLA <- tabla(datos = Consolidado, categoria = "MOD_ADM", variable = 'Total graduados en pregrado según modalidad de admisión - sede Medellín', mensaje = "Número de graduados en pregrado según modalidad de admisión - sede Medellín", titulo = "Graduados en pregrado según modalidad de admisión - sede Medellín");MOD_ADM_TABLA
MOD_ADM_SERIE <- series(datos = Consolidado, categoria = "MOD_ADM", colores = col, titulo = "Evolución del número de graduados en pregrado por modalidad de admisión - sede Medellín", eje = "Número de graduados (k: miles)");MOD_ADM_SERIE
MOD_ADM_ACTUAL <- torta(datos = Consolidado, variable = "MOD_ADM", colores = col, titulo = "Distribución de graduados en pregrado según modalidad de admisión - sede Medellín", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MOD_ADM_ACTUAL


# tipo de admisión ---

col <-   c( "#f15a24", # naranja, PAES
            "#6d6666", # gris, PEAA
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f" # verde, Regular
)


TIPO_ADM_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_ADM", variable = 'Total graduados en pregrado por programa de admisión - sede Medellín', mensaje = "Número de graduados en pregrado por programa de admisión - sede Medellín", titulo = "Graduados en pregrado por programa de admisión - sede Medellín");TIPO_ADM_TABLA
TIPO_ADM_SERIE <- series(datos = Consolidado, categoria = "TIPO_ADM", colores = col, titulo = "Evolución del número de graduados en pregrado por programa de admisión - sede Medellín", eje = "Número de graduados (k: miles)");TIPO_ADM_SERIE
TIPO_ADM_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_ADM", colores = col, titulo = "Distribución de graduados en pregrado por programa de admisión - sede Medellín", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_ADM_ACTUAL


# PAES ---


col <-   c( "#0071bc", # azul vivo, comunidades indigenas
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666",  # gris, mejores bachilleres municipios pobres 
            "#f15a24", # naranja, población afro
            "#8cc63f"  # verde, victimas del conflicto
)


PAES_TABLA <- tabla(datos = Consolidado, categoria = "PAES", variable = 'Total de graduados en pregrado del programa PAES - sede Medellín', mensaje = "Número de graduados en pregrado del programa PAES - sede Medellín", titulo = "Graduados en pregrado del programa PAES - sede Medellín");PAES_TABLA
PAES_SERIE <- series(datos = Consolidado, categoria = "PAES", colores = col, titulo = "Evolución del número de graduados en pregrado del programa PAES - sede Medellín", eje = "Número de graduados (k: miles)");PAES_SERIE
PAES_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "PAES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado del programa PAES - sede Medellín", eje = "Número de graduados (k: miles)"); PAES_ACTUAL


# PEAMA ---

col <-   c( "#29abe2", # azul claro, Amazonía
            "#c1272d", # Rojo, Caribe
            "#fbb03b", # amarillo, Orinoquia
            "#f15a24", # naranja, Medellín - Sinifaná
            "#8cc63f", # verde, Medellín - Sumapaz
            "#0071bc", # azul vivo, Manizales
            "#6d6666" # gris, Tumaco
)


PEAMA_TABLA <- tabla(datos = Consolidado, categoria = "PEAMA", variable = 'Total de graduados en pregrado del programa PEAMA - sede Medellín', mensaje = "Número de graduados en pregrado del programa PEAMA - sede Medellín", titulo = "Graduados en pregrado del programa PEAMA - sede Medellín");PEAMA_TABLA
PEAMA_SERIE <- series(datos = Consolidado, categoria = "PEAMA", colores = col, titulo = "Evolución del número de graduados en pregrado del programa PEAMA - sede Medellín", eje = "Número de graduados");PEAMA_SERIE
PEAMA_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "PEAMA", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado del programa PEAMA - sede Medellín", eje = "Número de graduados"); PEAMA_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total graduados en pregrado por áreas del conocimiento SNIES - sede Medellín', mensaje = "Total de graduados en pregrado por áreas del conocimiento SNIES - sede Medellín", titulo = "Total de graduados en pregrado por áreas del conocimiento SNIES - sede Medellín");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de graduados en pregrado por áreas del conocimiento SNIES - sede Medellín", eje = "Número de graduados (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por áreas del conocimiento SNIES - sede Medellín", eje = "Número de graduados"); AREAC_SNIES_ACTUAL


# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Graduados/Med/Pregrado", "Serie.html")
Salvar(FACULTAD_TABLA, "G_Graduados/Med/Pregrado", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Graduados/Med/Pregrado", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Graduados/Med/Pregrado", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Graduados/Med/Pregrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Graduados/Med/Pregrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Graduados/Med/Pregrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Graduados/Med/Pregrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Graduados/Med/Pregrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Graduados/Med/Pregrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Graduados/Med/Pregrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Graduados/Med/Pregrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Graduados/Med/Pregrado", "A_edad.html")
Salvar(ESTRATO_TABLA, "G_Graduados/Med/Pregrado", "T_estrato.html")
Salvar(ESTRATO_SERIE, "G_Graduados/Med/Pregrado", "S_estrato.html")
Salvar(ESTRATO_ACTUAL, "G_Graduados/Med/Pregrado", "A_estrato.html")
Salvar(MOD_ADM_TABLA, "G_Graduados/Med/Pregrado", "T_modalidad.html")
Salvar(MOD_ADM_SERIE, "G_Graduados/Med/Pregrado", "S_modalidad.html")
Salvar(MOD_ADM_ACTUAL, "G_Graduados/Med/Pregrado", "A_modalidad.html")
Salvar(TIPO_ADM_TABLA, "G_Graduados/Med/Pregrado", "T_tipo.html")
Salvar(TIPO_ADM_SERIE, "G_Graduados/Med/Pregrado", "S_tipo.html")
Salvar(TIPO_ADM_ACTUAL, "G_Graduados/Med/Pregrado", "A_tipo.html")
Salvar(PAES_TABLA, "G_Graduados/Med/Pregrado", "T_paes.html")
Salvar(PAES_SERIE, "G_Graduados/Med/Pregrado", "S_paes.html")
Salvar(PAES_ACTUAL, "G_Graduados/Med/Pregrado", "A_paes.html")
Salvar(PEAMA_TABLA, "G_Graduados/Med/Pregrado", "T_peama.html")
Salvar(PEAMA_SERIE, "G_Graduados/Med/Pregrado", "S_peama.html")
Salvar(PEAMA_ACTUAL, "G_Graduados/Med/Pregrado", "A_peama.html")
Salvar(AREAC_SNIES_TABLA, "G_Graduados/Med/Pregrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Graduados/Med/Pregrado", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Graduados/Med/Pregrado", "A_snies.html")


# GraPre1103 ----


Consolidado <- ConsolidadoG %>% filter(Nivel == "GraPre1103") %>% select(-(Nivel))


# Evolución histórica ---


col <-   c("#0071bc") # Azul vivo, Total


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes graduados en pregrado - sede Manizales", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Facultad  ---


col <-   c( "#f15a24", # naranja, Administración
            "#0071bc", # azul vivo, Ciencias exactas y naturales
            "#8cc63f" # verde, Ingeniaría y arquitectura
)



FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes graduados en pregrado por facultad - sede Manizales', mensaje = "Total de estudiantes graduados en pregrado por facultad - sede Manizales", titulo = "Facultad de estudiantes graduados en pregrado - sede Manizales");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de graduados en pregrado por facultad - sede Manizales", eje = "Número de graduados (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por facultad - sede Manizales",eje = "Número de graduados"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información



NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total graduados en pregrado según nacionalidad - sede Manizales', mensaje = "Número de graduados en pregrado por nacionalidad - sede Manizales", titulo = "Graduados en pregrado según nacionalidad - sede Manizales");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de graduados en pregrado según nacionalidad - sede Manizales", eje = "Número de graduados (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de graduados en pregrado según nacionalidad - sede Manizales", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total graduados en pregrado por sexo - sede Manizales', mensaje = "Número de graduados en pregrado por sexo - sede Manizales", titulo = "Graduados en pregrado por sexo - sede Manizales");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados en pregrado por sexo - sede Manizales", eje = "Número de graduados (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de graduados en pregrado por sexo - sede Manizales", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total graduados en pregrado por grupos de edad - sede Manizales', mensaje = "Número de graduados en pregrado por grupos de edad - sede Manizales", titulo = "Graduados en pregrado por grupos de edad - sede Manizales");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de graduados en pregrado por grupos de edad - sede Manizales", eje = "Número de graduados (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por grupos de edad - sede Manizales", eje = "Número de graduados (k: miles)"); CAT_EDAD_ACTUAL


# Estrato ---

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)


ESTRATO_TABLA <- tabla(datos = Consolidado, categoria = "ESTRATO", variable = 'Total de graduados en pregrado por estrato socioeconómico - sede Manizales', mensaje = "Número de graduados en pregrado por estrato socioeconómico - sede Manizales", titulo = "Graduados en pregrado por estrato socioeconómico - sede Manizales");ESTRATO_TABLA
ESTRATO_SERIE <- series(datos = Consolidado, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de graduados en pregrado por estrato socioeconómico - sede Manizales", eje = "Número de graduados (k: miles)");ESTRATO_SERIE
ESTRATO_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por estrato socioeconómico - sede Manizales", eje = "Número de graduados (k: miles)"); ESTRATO_ACTUAL


# Modalidad de admisión ---

col <-   c( "#f15a24",  # naranja, Especial
            "#8cc63f" # verde, Regular
)


MOD_ADM_TABLA <- tabla(datos = Consolidado, categoria = "MOD_ADM", variable = 'Total graduados en pregrado según modalidad de admisión - sede Manizales', mensaje = "Número de graduados en pregrado según modalidad de admisión - sede Manizales", titulo = "Graduados en pregrado según modalidad de admisión - sede Manizales");MOD_ADM_TABLA
MOD_ADM_SERIE <- series(datos = Consolidado, categoria = "MOD_ADM", colores = col, titulo = "Evolución del número de graduados en pregrado por modalidad de admisión - sede Manizales", eje = "Número de graduados (k: miles)");MOD_ADM_SERIE
MOD_ADM_ACTUAL <- torta(datos = Consolidado, variable = "MOD_ADM", colores = col, titulo = "Distribución de graduados en pregrado según modalidad de admisión - sede Manizales", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MOD_ADM_ACTUAL


# tipo de admisión ---

col <-   c( "#f15a24", # naranja, PAES
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f", # verde, Regular
            "#6d6666" # gris, PEAA
)


TIPO_ADM_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_ADM", variable = 'Total graduados en pregrado por programa de admisión - sede Manizales', mensaje = "Número de graduados en pregrado por programa de admisión - sede Manizales", titulo = "Graduados en pregrado por programa de admisión - sede Manizales");TIPO_ADM_TABLA
TIPO_ADM_SERIE <- series(datos = Consolidado, categoria = "TIPO_ADM", colores = col, titulo = "Evolución del número de graduados en pregrado por programa de admisión - sede Manizales", eje = "Número de graduados (k: miles)");TIPO_ADM_SERIE
TIPO_ADM_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_ADM", colores = col, titulo = "Distribución de graduados en pregrado por programa de admisión - sede Manizales", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_ADM_ACTUAL


# PAES ---


col <-   c( "#0071bc", # azul vivo, comunidades indigenas
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666",  # gris, mejores bachilleres municipios pobres 
            "#f15a24", # naranja, población afro
            "#8cc63f"  # verde, victimas del conflicto
)


PAES_TABLA <- tabla(datos = Consolidado, categoria = "PAES", variable = 'Total de graduados en pregrado del programa PAES - sede Manizales', mensaje = "Número de graduados en pregrado del programa PAES - sede Manizales", titulo = "Graduados en pregrado del programa PAES - sede Manizales");PAES_TABLA
PAES_SERIE <- series(datos = Consolidado, categoria = "PAES", colores = col, titulo = "Evolución del número de graduados en pregrado del programa PAES - sede Manizales", eje = "Número de graduados (k: miles)");PAES_SERIE
PAES_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "PAES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado del programa PAES - sede Manizales", eje = "Número de graduados (k: miles)"); PAES_ACTUAL


# PEAMA ---

col <-   c( "#29abe2", # azul claro, Amazonía
            "#c1272d", # Rojo, Caribe
            "#fbb03b", # amarillo, Orinoquia
            "#f15a24", # naranja, Manizales - Sinifaná
            "#8cc63f", # verde, Manizales - Sumapaz
            "#0071bc", # azul vivo, Manizales
            "#6d6666" # gris, Tumaco
)


PEAMA_TABLA <- tabla(datos = Consolidado, categoria = "PEAMA", variable = 'Total de graduados en pregrado del programa PEAMA - sede Manizales', mensaje = "Número de graduados en pregrado del programa PEAMA - sede Manizales", titulo = "Graduados en pregrado del programa PEAMA - sede Manizales");PEAMA_TABLA
PEAMA_SERIE <- series(datos = Consolidado, categoria = "PEAMA", colores = col, titulo = "Evolución del número de graduados en pregrado del programa PEAMA - sede Manizales", eje = "Número de graduados");PEAMA_SERIE
PEAMA_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "PEAMA", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado del programa PEAMA - sede Manizales", eje = "Número de graduados"); PEAMA_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total graduados en pregrado por áreas del conocimiento SNIES - sede Manizales', mensaje = "Total de graduados en pregrado por áreas del conocimiento SNIES - sede Manizales", titulo = "Total de graduados en pregrado por áreas del conocimiento SNIES - sede Manizales");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de graduados en pregrado por áreas del conocimiento SNIES - sede Manizales", eje = "Número de graduados (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por áreas del conocimiento SNIES - sede Manizales", eje = "Número de graduados"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Graduados/Man/Pregrado", "Serie.html")
Salvar(FACULTAD_TABLA, "G_Graduados/Man/Pregrado", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Graduados/Man/Pregrado", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Graduados/Man/Pregrado", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Graduados/Man/Pregrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Graduados/Man/Pregrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Graduados/Man/Pregrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Graduados/Man/Pregrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Graduados/Man/Pregrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Graduados/Man/Pregrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Graduados/Man/Pregrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Graduados/Man/Pregrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Graduados/Man/Pregrado", "A_edad.html")
Salvar(ESTRATO_TABLA, "G_Graduados/Man/Pregrado", "T_estrato.html")
Salvar(ESTRATO_SERIE, "G_Graduados/Man/Pregrado", "S_estrato.html")
Salvar(ESTRATO_ACTUAL, "G_Graduados/Man/Pregrado", "A_estrato.html")
Salvar(MOD_ADM_TABLA, "G_Graduados/Man/Pregrado", "T_modalidad.html")
Salvar(MOD_ADM_SERIE, "G_Graduados/Man/Pregrado", "S_modalidad.html")
Salvar(MOD_ADM_ACTUAL, "G_Graduados/Man/Pregrado", "A_modalidad.html")
Salvar(TIPO_ADM_TABLA, "G_Graduados/Man/Pregrado", "T_tipo.html")
Salvar(TIPO_ADM_SERIE, "G_Graduados/Man/Pregrado", "S_tipo.html")
Salvar(TIPO_ADM_ACTUAL, "G_Graduados/Man/Pregrado", "A_tipo.html")
Salvar(PAES_TABLA, "G_Graduados/Man/Pregrado", "T_paes.html")
Salvar(PAES_SERIE, "G_Graduados/Man/Pregrado", "S_paes.html")
Salvar(PAES_ACTUAL, "G_Graduados/Man/Pregrado", "A_paes.html")
Salvar(PEAMA_TABLA, "G_Graduados/Man/Pregrado", "T_peama.html")
Salvar(PEAMA_SERIE, "G_Graduados/Man/Pregrado", "S_peama.html")
Salvar(PEAMA_ACTUAL, "G_Graduados/Man/Pregrado", "A_peama.html")
Salvar(AREAC_SNIES_TABLA, "G_Graduados/Man/Pregrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Graduados/Man/Pregrado", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Graduados/Man/Pregrado", "A_snies.html")


# GraPre1104 ----


Consolidado <- ConsolidadoG %>% filter(Nivel == "GraPre1104") %>% select(-(Nivel))


# Evolución histórica ---


col <-   c("#93278f") # Morado, Total


EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes graduados en pregrado - sede Palmira", eje = "Número de estudiantes (k: miles)");EVOLUCION_SERIE


# Facultad  ---


col <-   c( "#0071bc", # azul vivo, Ciencias agropecuarias 
            "#8cc63f" # verde, Ingeniería y Administración
)


FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes graduados en pregrado por facultad - sede Palmira', mensaje = "Total de estudiantes graduados en pregrado por facultad - sede Palmira", titulo = "Facultad de estudiantes graduados en pregrado - sede Palmira");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de graduados en pregrado por facultad - sede Palmira", eje = "Número de graduados (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por facultad - sede Palmira",eje = "Número de graduados"); FACULTAD_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información



NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total graduados en pregrado según nacionalidad - sede Palmira', mensaje = "Número de graduados en pregrado por nacionalidad - sede Palmira", titulo = "Graduados en pregrado según nacionalidad - sede Palmira");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de graduados en pregrado según nacionalidad - sede Palmira", eje = "Número de graduados (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de graduados en pregrado según nacionalidad - sede Palmira", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres

################ 1. Tabla

SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total graduados en pregrado por sexo - sede Palmira', mensaje = "Número de graduados en pregrado por sexo - sede Palmira", titulo = "Graduados en pregrado por sexo - sede Palmira");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados en pregrado por sexo - sede Palmira", eje = "Número de graduados (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de graduados en pregrado por sexo - sede Palmira", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 17 o menos
            "#f15a24", # naranja,  18 a 20 
            "#0071bc", # azul vivo, 21 a 25
            "#6d6666", # gris, 26 o más
            "#fbb03b" ) # amarillo, sin información


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total graduados en pregrado por grupos de edad - sede Palmira', mensaje = "Número de graduados en pregrado por grupos de edad - sede Palmira", titulo = "Graduados en pregrado por grupos de edad - sede Palmira");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de graduados en pregrado por grupos de edad - sede Palmira", eje = "Número de graduados (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por grupos de edad - sede Palmira", eje = "Número de graduados (k: miles)"); CAT_EDAD_ACTUAL


# Estrato ---

col <-   c( "#8cc63f", # verde, Estrato 2 o menos
            "#f15a24", # naranja, Estrato 3
            "#0071bc", # azul vivo, Estrato 4 o más
            "#6d6666" # gris, ND/NE
)


ESTRATO_TABLA <- tabla(datos = Consolidado, categoria = "ESTRATO", variable = 'Total de graduados en pregrado por estrato socioeconómico - sede Palmira', mensaje = "Número de graduados en pregrado por estrato socioeconómico - sede Palmira", titulo = "Graduados en pregrado por estrato socioeconómico - sede Palmira");ESTRATO_TABLA
ESTRATO_SERIE <- series(datos = Consolidado, categoria = "ESTRATO", colores = col, titulo = "Evolución del número de graduados en pregrado por estrato socioeconómico - sede Palmira", eje = "Número de graduados (k: miles)");ESTRATO_SERIE
ESTRATO_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "ESTRATO", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por estrato socioeconómico - sede Palmira", eje = "Número de graduados (k: miles)"); ESTRATO_ACTUAL


# Modalidad de admisión ---

col <-   c( "#f15a24",  # naranja, Especial
            "#8cc63f" # verde, Regular
)


MOD_ADM_TABLA <- tabla(datos = Consolidado, categoria = "MOD_ADM", variable = 'Total graduados en pregrado según modalidad de admisión - sede Palmira', mensaje = "Número de graduados en pregrado según modalidad de admisión - sede Palmira", titulo = "Graduados en pregrado según modalidad de admisión - sede Palmira");MOD_ADM_TABLA
MOD_ADM_SERIE <- series(datos = Consolidado, categoria = "MOD_ADM", colores = col, titulo = "Evolución del número de graduados en pregrado por modalidad de admisión - sede Palmira", eje = "Número de graduados (k: miles)");MOD_ADM_SERIE
MOD_ADM_ACTUAL <- torta(datos = Consolidado, variable = "MOD_ADM", colores = col, titulo = "Distribución de graduados en pregrado según modalidad de admisión - sede Palmira", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);MOD_ADM_ACTUAL


# tipo de admisión ---

col <-   c( "#f15a24", # naranja, PAES
            "#0071bc", # azul vivo, PEAMA
            "#8cc63f", # verde, Regular
            "#6d6666" # gris, PEAA
)


TIPO_ADM_TABLA <- tabla(datos = Consolidado, categoria = "TIPO_ADM", variable = 'Total graduados en pregrado por programa de admisión - sede Palmira', mensaje = "Número de graduados en pregrado por programa de admisión - sede Palmira", titulo = "Graduados en pregrado por programa de admisión - sede Palmira");TIPO_ADM_TABLA
TIPO_ADM_SERIE <- series(datos = Consolidado, categoria = "TIPO_ADM", colores = col, titulo = "Evolución del número de graduados en pregrado por programa de admisión - sede Palmira", eje = "Número de graduados (k: miles)");TIPO_ADM_SERIE
TIPO_ADM_ACTUAL <- torta(datos = Consolidado, variable = "TIPO_ADM", colores = col, titulo = "Distribución de graduados en pregrado por programa de admisión - sede Palmira", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);TIPO_ADM_ACTUAL


# PAES ---


col <-   c( "#0071bc", # azul vivo, comunidades indigenas
            "#fbb03b", # amarillo, mejores bachilleres
            "#6d6666",  # gris, mejores bachilleres municipios pobres 
            "#f15a24", # naranja, población afro
            "#8cc63f"  # verde, victimas del conflicto
)


PAES_TABLA <- tabla(datos = Consolidado, categoria = "PAES", variable = 'Total de graduados en pregrado del programa PAES - sede Palmira', mensaje = "Número de graduados en pregrado del programa PAES - sede Palmira", titulo = "Graduados en pregrado del programa PAES - sede Palmira");PAES_TABLA
PAES_SERIE <- series(datos = Consolidado, categoria = "PAES", colores = col, titulo = "Evolución del número de graduados en pregrado del programa PAES - sede Palmira", eje = "Número de graduados (k: miles)");PAES_SERIE
PAES_ACTUAL <- barra_vertical(datos = Consolidado, categoria = "PAES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado del programa PAES - sede Palmira", eje = "Número de graduados (k: miles)"); PAES_ACTUAL


# PEAMA ---

col <-   c( "#29abe2", # azul claro, Amazonía
            "#c1272d", # Rojo, Caribe
            "#fbb03b", # amarillo, Orinoquia
            "#f15a24", # naranja, Palmira - Sinifaná
            "#8cc63f", # verde, Palmira - Sumapaz
            "#0071bc", # azul vivo, Palmira
            "#6d6666" # gris, Tumaco
)


PEAMA_TABLA <- tabla(datos = Consolidado, categoria = "PEAMA", variable = 'Total de graduados en pregrado del programa PEAMA - sede Palmira', mensaje = "Número de graduados en pregrado del programa PEAMA - sede Palmira", titulo = "Graduados en pregrado del programa PEAMA - sede Palmira");PEAMA_TABLA
PEAMA_SERIE <- series(datos = Consolidado, categoria = "PEAMA", colores = col, titulo = "Evolución del número de graduados en pregrado del programa PEAMA - sede Palmira", eje = "Número de graduados");PEAMA_SERIE
PEAMA_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "PEAMA", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado del programa PEAMA - sede Palmira", eje = "Número de graduados"); PEAMA_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total graduados en pregrado por áreas del conocimiento SNIES - sede Palmira', mensaje = "Total de graduados en pregrado por áreas del conocimiento SNIES - sede Palmira", titulo = "Total de graduados en pregrado por áreas del conocimiento SNIES - sede Palmira");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de graduados en pregrado por áreas del conocimiento SNIES - sede Palmira", eje = "Número de graduados (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en pregrado por áreas del conocimiento SNIES - sede Palmira", eje = "Número de graduados"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Graduados/Pal/Pregrado", "Serie.html")
Salvar(FACULTAD_TABLA, "G_Graduados/Pal/Pregrado", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Graduados/Pal/Pregrado", "S_facultad.html")
Salvar(FACULTAD_ACTUAL, "G_Graduados/Pal/Pregrado", "A_facultad.html")
Salvar(NACIONALIDAD_TABLA, "G_Graduados/Pal/Pregrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Graduados/Pal/Pregrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Graduados/Pal/Pregrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Graduados/Pal/Pregrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Graduados/Pal/Pregrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Graduados/Pal/Pregrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Graduados/Pal/Pregrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Graduados/Pal/Pregrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Graduados/Pal/Pregrado", "A_edad.html")
Salvar(ESTRATO_TABLA, "G_Graduados/Pal/Pregrado", "T_estrato.html")
Salvar(ESTRATO_SERIE, "G_Graduados/Pal/Pregrado", "S_estrato.html")
Salvar(ESTRATO_ACTUAL, "G_Graduados/Pal/Pregrado", "A_estrato.html")
Salvar(MOD_ADM_TABLA, "G_Graduados/Pal/Pregrado", "T_modalidad.html")
Salvar(MOD_ADM_SERIE, "G_Graduados/Pal/Pregrado", "S_modalidad.html")
Salvar(MOD_ADM_ACTUAL, "G_Graduados/Pal/Pregrado", "A_modalidad.html")
Salvar(TIPO_ADM_TABLA, "G_Graduados/Pal/Pregrado", "T_tipo.html")
Salvar(TIPO_ADM_SERIE, "G_Graduados/Pal/Pregrado", "S_tipo.html")
Salvar(TIPO_ADM_ACTUAL, "G_Graduados/Pal/Pregrado", "A_tipo.html")
Salvar(PAES_TABLA, "G_Graduados/Pal/Pregrado", "T_paes.html")
Salvar(PAES_SERIE, "G_Graduados/Pal/Pregrado", "S_paes.html")
Salvar(PAES_ACTUAL, "G_Graduados/Pal/Pregrado", "A_paes.html")
Salvar(PEAMA_TABLA, "G_Graduados/Pal/Pregrado", "T_peama.html")
Salvar(PEAMA_SERIE, "G_Graduados/Pal/Pregrado", "S_peama.html")
Salvar(PEAMA_ACTUAL, "G_Graduados/Pal/Pregrado", "A_peama.html")
Salvar(AREAC_SNIES_TABLA, "G_Graduados/Pal/Pregrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Graduados/Pal/Pregrado", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Graduados/Pal/Pregrado", "A_snies.html")


# GRADUADO POSTGRADO ---- 

# GraPos1100 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "GraPos1100") %>% select(-(Nivel))


# Evolución histórica  ---

col <-    c("#f15a24") # naranja, Total

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes graduados en postgrado", eje = "Número de graduados (k: miles)");EVOLUCION_SERIE


# Sedes  ---


col <-   c( "#29abe2", # azul claro, Amazonía
            "#8cc63f", # verde, Bogotá
            "#c1272d", # rojo, Caribe 
            "#0071bc", # azul vivo, Manizales
            "#f15a24", # naranja, Medellín
            "#93278f", # Morado, Palmira
            "#fbb03b", # amarillo, Orinoquía 
            "#9e9ac8",  # Morado claro, De la Paz
            "#6d6666"  # gris, Tumaco 
) 


SEDE_TABLA <- tabla(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", variable = 'Total estudiantes graduados en postgrado por sede de graduación', mensaje = "Total de estudiantes en postgrado por sede de graduación", titulo = "Sede estudiantes graduados postgrado");SEDE_TABLA
SEDE_SERIE <- series(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", colores = col, titulo = "Evolución del número de graduados en postgrado por sedes", eje = "Número de graduados (k: miles)");SEDE_SERIE
SEDE_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "SEDE_NOMBRE_MAT", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en postgrado por sedes de la Universidad", eje = "Número de graduados"); SEDE_ACTUAL


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#fbb03b", # amarillo, Especialidades médicas
            "#29abe2", # azul claro, Especialización
            "#c1272d")  # rojo, Maestría

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes graduados en postgrado por nivel de formación', mensaje = "Número de estudiantes graduados en postgrado por nivel de formación", titulo = "Graduados en postgrado por nivel de formación");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes graduados en postgrado por nivel de formación", eje = "Número de graduados");NIVEL_SERIE
NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "NIVEL", colores = col, titulo = "Distribución de graduados en postgrado por nivel de formación", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NIVEL_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes graduados en postgrado según nacionalidad', mensaje = "Número de estudiantes graduados en postgrado por nacionalidad", titulo = "Graduados según nacionalidad");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de graduados en postgrado según nacionalidad", eje = "Número de graduados (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de graduados en postgrado según nacionalidad", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres


SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total graduados en postgrado por sexo', mensaje = "Número de graduados en postgrado por sexo", titulo = "Graduados en postgrado por sexo");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados en postgrado por sexo", eje = "Número de graduados (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de graduados en postgrado por sexo", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 25 años o menos
            "#0071bc", # azul vivo, 26 a 30 años
            "#f15a24", # naranja, 31 a 35 años
            "#fbb03b", # amarillo, 36 años o más
            "#6d6666")  # gris, sin información 


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total graduados en postgrado por grupos de edad', mensaje = "Número de graduados en postgrado por grupos de edad", titulo = "Graduados en postgrado por grupos de edad");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de graduados en postgrado por grupos de edad", eje = "Número de graduados (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en postgrado por grupos de edad", eje = "Número de graduados (k: miles)"); CAT_EDAD_ACTUAL


# Convenios  ---


col <-   c( "#f15a24", # naranja, No
            "#8cc63f", # verde, Sí
            "#0071bc") # azul vivo, sin información


CONVENIO_TABLA <- tabla(datos = Consolidado, categoria = "CONVENIO", variable = 'Total estudiantes graduados en postgrado por convenios', mensaje = "Total de estudiantes graduados en postgrado en convenios", titulo = "Sede estudiantes graduados en postgrado");CONVENIO_TABLA
CONVENIO_SERIE <- series(datos = Consolidado, categoria = "CONVENIO", colores = col, titulo = "Evolución del número de estudiantes graduados en postgrado por convenios", eje = "Número de estudiantes (k: miles)");CONVENIO_SERIE
CONVENIO_ACTUAL <- drilldown_si_no_torta(datos = Consolidado, categoria = "CONVENIO", categoria_drilldown = "TIP_CONVENIO", colores = col, titulo = "Distribución de graduados en postgrado en convenios, periodo", titulo_drilldown = "Graduados", etiqueta = "Total de graduados", eje = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);CONVENIO_ACTUAL



# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total graduados en postgrado por áreas del conocimiento SNIES', mensaje = "Total de graduados en post por áreas del conocimiento SNIES", titulo = "Sede graduados en postgrado por áreas del conocimiento SNIES");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de graduados en postgrado por áreas del conocimiento SNIES", eje = "Número de graduados (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en postgrado por áreas del conocimiento SNIES", eje = "Número de graduados"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Graduados/Nal/Postgrado", "Serie.html")
Salvar(SEDE_TABLA, "G_Graduados/Nal/Postgrado", "T_sede.html")
Salvar(SEDE_SERIE, "G_Graduados/Nal/Postgrado", "S_sede.html" )
Salvar(SEDE_ACTUAL, "G_Graduados/Nal/Postgrado", "A_sede.html" )
Salvar(NIVEL_TABLA, "G_Graduados/Nal/Postgrado", "T_nivel.html" )
Salvar(NIVEL_SERIE, "G_Graduados/Nal/Postgrado", "S_nivel.html" )
Salvar(NIVEL_ACTUAL, "G_Graduados/Nal/Postgrado", "A_nivel.html" )
Salvar(NACIONALIDAD_TABLA, "G_Graduados/Nal/Postgrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Graduados/Nal/Postgrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Graduados/Nal/Postgrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Graduados/Nal/Postgrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Graduados/Nal/Postgrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Graduados/Nal/Postgrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Graduados/Nal/Postgrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Graduados/Nal/Postgrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Graduados/Nal/Postgrado", "A_edad.html" )
Salvar(CONVENIO_TABLA, "G_Graduados/Nal/Postgrado", "T_convenio.html")
Salvar(CONVENIO_SERIE, "G_Graduados/Nal/Postgrado", "S_convenio.html") 
Salvar(CONVENIO_ACTUAL, "G_Graduados/Nal/Postgrado", "A_convenio.html")
Salvar(AREAC_SNIES_TABLA, "G_Graduados/Nal/Postgrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Graduados/Nal/Postgrado", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Graduados/Nal/Postgrado", "A_snies.html")


# GraPos1101 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "GraPos1101") %>% select(-(Nivel))


# Evolución histórica  ---

col <-   c("#8cc63f") # verde, Total

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes graduados en postgrado - sede Bogotá", eje = "Número de graduados (k: miles)");EVOLUCION_SERIE


# Facultad  ---


col <-   c( "#9e9ac8",  # Morado claro, Enfermería
            "#0071bc", # azul vivo, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias económicas
            "#f15a24", # naranja, Ciencias humanas
            "#fbb03b", # amarillo, Derecho, ciencias políticas y sociales
            "#93278f", # Morado, Artes 
            "#8cc63f", # verde, Ingeniería
            "#bdbdbd",  # Gris claro, Medicina 
            "#c1272d", # rojo, Medicina veterianaria y zootecnia
            "#99d8c9"  # Agua Marina, Odontología 
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes graduados en postgrado por facultad de graduación - sede Bogotá', mensaje = "Total de estudiantes en postgrado por facultad de graduación - sede Bogotá", titulo = "Facultad de estudiantes graduados en postgrado - sede Bogotá");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de graduados en postgrado por facultad - sede Bogotá", eje = "Número de graduados (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en postgrado por facultad - sede Bogotá", eje = "Número de graduados"); FACULTAD_ACTUAL


  # Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#fbb03b", # amarillo, Especialidades médicas
            "#29abe2", # azul claro, Especialización
            "#c1272d")  # rojo, Maestría

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes graduados en postgrado por nivel de formación - sede Bogotá', mensaje = "Número de estudiantes graduados en postgrado por nivel de formación - sede Bogotá", titulo = "Graduados en postgrado por nivel de formación - sede Bogotá");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes graduados en postgrado por nivel de formación - sede Bogotá", eje = "Número de graduados");NIVEL_SERIE
NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "NIVEL", colores = col, titulo = "Distribución de graduados en postgrado por nivel de formación - sede Bogotá", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NIVEL_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes graduados en postgrado según nacionalidad - sede Bogotá', mensaje = "Número de estudiantes graduados en postgrado por nacionalidad - sede Bogotá", titulo = "Graduados en postgrado según nacionalidad - sede Bogotá");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de graduados en postgrado según nacionalidad - sede Bogotá", eje = "Número de graduados (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de graduados en postgrado según nacionalidad - sede Bogotá", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres


SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total graduados en postgrado por sexo - sede Bogotá', mensaje = "Número de graduados en postgrado por sexo - sede Bogotá", titulo = "Graduados en postgrado por sexo - sede Bogotá");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados en postgrado por sexo - sede Bogotá", eje = "Número de graduados (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de graduados en postgrado por sexo - sede Bogotá", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 25 años o menos
            "#0071bc", # azul vivo, 26 a 30 años
            "#f15a24", # naranja, 31 a 35 años
            "#fbb03b", # amarillo, 36 años o más
            "#6d6666")  # gris, sin información 


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total graduados en postgrado por grupos de edad - sede Bogotá', mensaje = "Número de graduados en postgrado por grupos de edad - sede Bogotá", titulo = "Graduados en postgrado por grupos de edad - sede Bogotá");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de graduados en postgrado por grupos de edad - sede Bogotá", eje = "Número de graduados (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en postgrado por grupos de edad - sede Bogotá", eje = "Número de graduados (k: miles)"); CAT_EDAD_ACTUAL


# Convenios  ---


col <-   c( "#f15a24", # naranja, No
            "#8cc63f", # verde, Sí
            "#0071bc") # azul vivo, sin información


CONVENIO_TABLA <- tabla(datos = Consolidado, categoria = "CONVENIO", variable = 'Total estudiantes graduados en postgrado por convenios - sede Bogotá', mensaje = "Total de estudiantes graduados en postgrado por convenios - sede Bogotá", titulo = "Estudiantes graduados en postgrado por convenios - sede Bogotá");CONVENIO_TABLA
CONVENIO_SERIE <- series(datos = Consolidado, categoria = "CONVENIO", colores = col, titulo = "Evolución del número de estudiantes graduados en postgrado por convenios - sede Bogotá", eje = "Número de graduados (k: miles)");CONVENIO_SERIE
CONVENIO_ACTUAL <- drilldown_si_no_torta(datos = Consolidado, categoria = "CONVENIO", categoria_drilldown = "TIP_CONVENIO", colores = col, titulo = "Distribución de graduados en postgrado en convenios - sede Bogotá, periodo", titulo_drilldown = "Graduados", etiqueta = "Total de graduados", eje = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);CONVENIO_ACTUAL

# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total graduados en postgrado por áreas del conocimiento SNIES - sede Bogotá', mensaje = "Total de graduados en postgrado por áreas del conocimiento SNIES - sede Bogotá", titulo = "Sede graduados en postgrado por áreas del conocimiento SNIES - sede Bogotá");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de graduados en postgrado por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de graduados (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en postgrado por áreas del conocimiento SNIES - sede Bogotá", eje = "Número de graduados"); AREAC_SNIES_ACTUAL


# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Graduados/Bog/Postgrado", "Serie.html")
Salvar(FACULTAD_TABLA, "G_Graduados/Bog/Postgrado", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Graduados/Bog/Postgrado", "S_facultad.html" )
Salvar(FACULTAD_ACTUAL, "G_Graduados/Bog/Postgrado", "A_facultad.html" )
Salvar(NIVEL_TABLA, "G_Graduados/Bog/Postgrado", "T_nivel.html" )
Salvar(NIVEL_SERIE, "G_Graduados/Bog/Postgrado", "S_nivel.html" )
Salvar(NIVEL_ACTUAL, "G_Graduados/Bog/Postgrado", "A_nivel.html" )
Salvar(NACIONALIDAD_TABLA, "G_Graduados/Bog/Postgrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Graduados/Bog/Postgrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Graduados/Bog/Postgrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Graduados/Bog/Postgrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Graduados/Bog/Postgrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Graduados/Bog/Postgrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Graduados/Bog/Postgrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Graduados/Bog/Postgrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Graduados/Bog/Postgrado", "A_edad.html" )
Salvar(CONVENIO_TABLA, "G_Graduados/Bog/Postgrado", "T_convenio.html")
Salvar(CONVENIO_SERIE, "G_Graduados/Bog/Postgrado", "S_convenio.html") 
Salvar(CONVENIO_ACTUAL, "G_Graduados/Bog/Postgrado", "A_convenio.html")
Salvar(AREAC_SNIES_TABLA, "G_Graduados/Bog/Postgrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Graduados/Bog/Postgrado", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Graduados/Bog/Postgrado", "A_snies.html")


# GraPos1102 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "GraPos1102") %>% select(-(Nivel))


# Evolución histórica  ---

col <-   c("#f15a24") # Naranja, Total

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes graduados en postgrado - sede Medellín", eje = "Número de graduados (k: miles)");EVOLUCION_SERIE


# Facultad  ---


col <-   c( "#9e9ac8",  # Morado claro, Arquitectura
            "#f15a24", # naranja, Ciencias
            "#6d6666", # gris, Ciencias agrarias
            "#29abe2", # azul claro, Ciencias humanas y económicas 
            "#8cc63f" # verde, Minas
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes graduados en postgrado por facultad de graduación - sede Medellín', mensaje = "Total de estudiantes en postgrado por facultad de graduación - sede Medellín", titulo = "Facultad de estudiantes graduados en postgrado - sede Medellín");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de graduados en postgrado por facultad - sede Medellín", eje = "Número de graduados (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en postgrado por facultad - sede Medellín", eje = "Número de graduados"); FACULTAD_ACTUAL


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2",  # azul claro, Especialización
            "#c1272d", # rojo, Maestría 
            "#fbb03b" # amarillo, Especialidades médicas
            )  

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes graduados en postgrado por nivel de formación - sede Medellín', mensaje = "Número de estudiantes graduados en postgrado por nivel de formación - sede Medellín", titulo = "Graduados en postgrado por nivel de formación - sede Medellín");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes graduados en postgrado por nivel de formación - sede Medellín", eje = "Número de graduados");NIVEL_SERIE
NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "NIVEL", colores = col, titulo = "Distribución de graduados en postgrado por nivel de formación - sede Medellín", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NIVEL_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes graduados en postgrado según nacionalidad - sede Medellín', mensaje = "Número de estudiantes graduados en postgrado por nacionalidad - sede Medellín", titulo = "Graduados en postgrado según nacionalidad - sede Medellín");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de graduados en postgrado según nacionalidad - sede Medellín", eje = "Número de graduados (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de graduados en postgrado según nacionalidad - sede Medellín", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres


SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total graduados en postgrado por sexo - sede Medellín', mensaje = "Número de graduados en postgrado por sexo - sede Medellín", titulo = "Graduados en postgrado por sexo - sede Medellín");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados en postgrado por sexo - sede Medellín", eje = "Número de graduados (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de graduados en postgrado por sexo - sede Medellín", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 25 años o menos
            "#0071bc", # azul vivo, 26 a 30 años
            "#f15a24", # naranja, 31 a 35 años
            "#fbb03b", # amarillo, 36 años o más
            "#6d6666")  # gris, sin información 


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total graduados en postgrado por grupos de edad - sede Medellín', mensaje = "Número de graduados en postgrado por grupos de edad - sede Medellín", titulo = "Graduados en postgrado por grupos de edad - sede Medellín");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de graduados en postgrado por grupos de edad - sede Medellín", eje = "Número de graduados (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en postgrado por grupos de edad - sede Medellín", eje = "Número de graduados (k: miles)"); CAT_EDAD_ACTUAL


# Convenios  ---


col <-   c( "#f15a24", # naranja, No
            "#8cc63f", # verde, Sí
            "#0071bc") # azul vivo, sin información


CONVENIO_TABLA <- tabla(datos = Consolidado, categoria = "CONVENIO", variable = 'Total estudiantes graduados en postgrado por convenios - sede Medellín', mensaje = "Total de estudiantes graduados en postgrado por convenios - sede Medellín", titulo = "Estudiantes graduados en postgrado por convenios - sede Medellín");CONVENIO_TABLA
CONVENIO_SERIE <- series(datos = Consolidado, categoria = "CONVENIO", colores = col, titulo = "Evolución del número de estudiantes graduados en postgrado por convenios - sede Medellín", eje = "Número de graduados (k: miles)");CONVENIO_SERIE
CONVENIO_ACTUAL <- drilldown_si_no_torta(datos = Consolidado, categoria = "CONVENIO", categoria_drilldown = "TIP_CONVENIO", colores = col, titulo = "Distribución de graduados en postgrado en convenios - sede Medellín, periodo", titulo_drilldown = "Graduados", etiqueta = "Total de graduados", eje = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);CONVENIO_ACTUAL

# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total graduados en postgrado por áreas del conocimiento SNIES - sede Medellín', mensaje = "Total de graduados en postgrado por áreas del conocimiento SNIES - sede Medellín", titulo = "Sede graduados en postgrado por áreas del conocimiento SNIES - sede Medellín");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de graduados en postgrado por áreas del conocimiento SNIES - sede Medellín", eje = "Número de graduados (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en postgrado por áreas del conocimiento SNIES - sede Medellín", eje = "Número de graduados"); AREAC_SNIES_ACTUAL


# Exportar ----


Salvar(EVOLUCION_SERIE, "G_Graduados/Med/Postgrado", "Serie.html")
Salvar(FACULTAD_TABLA, "G_Graduados/Med/Postgrado", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Graduados/Med/Postgrado", "S_facultad.html" )
Salvar(FACULTAD_ACTUAL, "G_Graduados/Med/Postgrado", "A_facultad.html" )
Salvar(NIVEL_TABLA, "G_Graduados/Med/Postgrado", "T_nivel.html" )
Salvar(NIVEL_SERIE, "G_Graduados/Med/Postgrado", "S_nivel.html" )
Salvar(NIVEL_ACTUAL, "G_Graduados/Med/Postgrado", "A_nivel.html" )
Salvar(NACIONALIDAD_TABLA, "G_Graduados/Med/Postgrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Graduados/Med/Postgrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Graduados/Med/Postgrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Graduados/Med/Postgrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Graduados/Med/Postgrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Graduados/Med/Postgrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Graduados/Med/Postgrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Graduados/Med/Postgrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Graduados/Med/Postgrado", "A_edad.html" )
Salvar(CONVENIO_TABLA, "G_Graduados/Med/Postgrado", "T_convenio.html")
Salvar(CONVENIO_SERIE, "G_Graduados/Med/Postgrado", "S_convenio.html") 
Salvar(CONVENIO_ACTUAL, "G_Graduados/Med/Postgrado", "A_convenio.html")
Salvar(AREAC_SNIES_TABLA, "G_Graduados/Med/Postgrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Graduados/Med/Postgrado", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Graduados/Med/Postgrado", "A_snies.html")


# GraPos1103 ----

# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "GraPos1103") %>% select(-(Nivel))


# Evolución histórica  ---

col <-   c("#0071bc") # Azul vivo, Total

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes graduados en postgrado - sede Manizales", eje = "Número de graduados (k: miles)");EVOLUCION_SERIE


# Facultad  ---


col <-   c( "#f15a24", # naranja, Administración
            "#0071bc", # azul vivo, Ciencias exactas y naturales
            "#8cc63f" # verde, Ingeniaría y arquitectura
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes graduados en postgrado por facultad de graduación - sede Manizales', mensaje = "Total de estudiantes en postgrado por facultad de graduación - sede Manizales", titulo = "Facultad de estudiantes graduados en postgrado - sede Manizales");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de graduados en postgrado por facultad - sede Manizales", eje = "Número de graduados (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en postgrado por facultad - sede Manizales", eje = "Número de graduados"); FACULTAD_ACTUAL


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2",  # azul claro, Especialización
            "#c1272d", # rojo, Maestría 
            "#fbb03b" # amarillo, Especialidades médicas
)  

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes graduados en postgrado por nivel de formación - sede Manizales', mensaje = "Número de estudiantes graduados en postgrado por nivel de formación - sede Manizales", titulo = "Graduados en postgrado por nivel de formación - sede Manizales");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes graduados en postgrado por nivel de formación - sede Manizales", eje = "Número de graduados");NIVEL_SERIE
NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "NIVEL", colores = col, titulo = "Distribución de graduados en postgrado por nivel de formación - sede Manizales", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NIVEL_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes graduados en postgrado según nacionalidad - sede Manizales', mensaje = "Número de estudiantes graduados en postgrado por nacionalidad - sede Manizales", titulo = "Graduados en postgrado según nacionalidad - sede Manizales");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de graduados en postgrado según nacionalidad - sede Manizales", eje = "Número de graduados (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de graduados en postgrado según nacionalidad - sede Manizales", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres


SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total graduados en postgrado por sexo - sede Manizales', mensaje = "Número de graduados en postgrado por sexo - sede Manizales", titulo = "Graduados en postgrado por sexo - sede Manizales");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados en postgrado por sexo - sede Manizales", eje = "Número de graduados (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de graduados en postgrado por sexo - sede Manizales", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 25 años o menos
            "#0071bc", # azul vivo, 26 a 30 años
            "#f15a24", # naranja, 31 a 35 años
            "#fbb03b", # amarillo, 36 años o más
            "#6d6666")  # gris, sin información 


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total graduados en postgrado por grupos de edad - sede Manizales', mensaje = "Número de graduados en postgrado por grupos de edad - sede Manizales", titulo = "Graduados en postgrado por grupos de edad - sede Manizales");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de graduados en postgrado por grupos de edad - sede Manizales", eje = "Número de graduados (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en postgrado por grupos de edad - sede Manizales", eje = "Número de graduados (k: miles)"); CAT_EDAD_ACTUAL


# Convenios  ---


col <-   c( "#f15a24", # naranja, No
            "#8cc63f", # verde, Sí
            "#0071bc") # azul vivo, sin información


CONVENIO_TABLA <- tabla(datos = Consolidado, categoria = "CONVENIO", variable = 'Total estudiantes graduados en postgrado por convenios - sede Manizales', mensaje = "Total de estudiantes graduados en postgrado por convenios - sede Manizales", titulo = "Estudiantes graduados en postgrado por convenios - sede Manizales");CONVENIO_TABLA
CONVENIO_SERIE <- series(datos = Consolidado, categoria = "CONVENIO", colores = col, titulo = "Evolución del número de estudiantes graduados en postgrado por convenios - sede Manizales", eje = "Número de graduados (k: miles)");CONVENIO_SERIE
CONVENIO_ACTUAL <- drilldown_si_no_torta(datos = Consolidado, categoria = "CONVENIO", categoria_drilldown = "TIP_CONVENIO", colores = col, titulo = "Distribución de graduados en postgrado en convenios - sede Manizales, periodo", titulo_drilldown = "Graduados", etiqueta = "Total de graduados", eje = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);CONVENIO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total graduados en postgrado por áreas del conocimiento SNIES - sede Manizales', mensaje = "Total de graduados en postgrado por áreas del conocimiento SNIES - sede Manizales", titulo = "Sede graduados en postgrado por áreas del conocimiento SNIES - sede Manizales");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de graduados en postgrado por áreas del conocimiento SNIES - sede Manizales", eje = "Número de graduados (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en postgrado por áreas del conocimiento SNIES - sede Manizales", eje = "Número de graduados"); AREAC_SNIES_ACTUAL


# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Graduados/Man/Postgrado", "Serie.html")
Salvar(FACULTAD_TABLA, "G_Graduados/Man/Postgrado", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Graduados/Man/Postgrado", "S_facultad.html" )
Salvar(FACULTAD_ACTUAL, "G_Graduados/Man/Postgrado", "A_facultad.html" )
Salvar(NIVEL_TABLA, "G_Graduados/Man/Postgrado", "T_nivel.html" )
Salvar(NIVEL_SERIE, "G_Graduados/Man/Postgrado", "S_nivel.html" )
Salvar(NIVEL_ACTUAL, "G_Graduados/Man/Postgrado", "A_nivel.html" )
Salvar(NACIONALIDAD_TABLA, "G_Graduados/Man/Postgrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Graduados/Man/Postgrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Graduados/Man/Postgrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Graduados/Man/Postgrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Graduados/Man/Postgrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Graduados/Man/Postgrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Graduados/Man/Postgrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Graduados/Man/Postgrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Graduados/Man/Postgrado", "A_edad.html" )
Salvar(CONVENIO_TABLA, "G_Graduados/Man/Postgrado", "T_convenio.html")
Salvar(CONVENIO_SERIE, "G_Graduados/Man/Postgrado", "S_convenio.html") 
Salvar(CONVENIO_ACTUAL, "G_Graduados/Man/Postgrado", "A_convenio.html")
Salvar(AREAC_SNIES_TABLA, "G_Graduados/Man/Postgrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Graduados/Man/Postgrado", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Graduados/Man/Postgrado", "A_snies.html")



# GraPos1104 ----


# Base de datos agregada nacional

Consolidado <- ConsolidadoG %>% filter(Nivel == "GraPos1104") %>% select(-(Nivel))


# Evolución histórica  ---

col <-   c("#93278f") # Morado, Total

EVOLUCION_SERIE <- series(datos = Consolidado, categoria = "TOTAL", colores = col, titulo = "Evolución histórica del total de estudiantes graduados en postgrado - sede Palmira", eje = "Número de graduados (k: miles)");EVOLUCION_SERIE


# Facultad  ---


col <-   c( "#0071bc", # azul vivo, Ciencias agropecuarias 
            "#8cc63f" # verde, Ingeniería y Administración
)

FACULTAD_TABLA <- tabla(datos = Consolidado, categoria = "FACULTAD", variable = 'Total estudiantes graduados en postgrado por facultad de graduación - sede Palmira', mensaje = "Total de estudiantes en postgrado por facultad de graduación - sede Palmira", titulo = "Facultad de estudiantes graduados en postgrado - sede Palmira");FACULTAD_TABLA
FACULTAD_SERIE <- series(datos = Consolidado, categoria = "FACULTAD", colores = col, titulo = "Evolución del número de graduados en postgrado por facultad - sede Palmira", eje = "Número de graduados (k: miles)");FACULTAD_SERIE
FACULTAD_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "FACULTAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en postgrado por facultad - sede Palmira", eje = "Número de graduados"); FACULTAD_ACTUAL


# Nivel de Formación ---

col <-   c( "#6d6666",  # gris, Doctorado
            "#29abe2",  # azul claro, Especialización
            "#c1272d", # rojo, Maestría 
            "#fbb03b" # amarillo, Especialidades médicas
)  

NIVEL_TABLA <- tabla(datos = Consolidado, categoria = "NIVEL", variable = 'Total estudiantes graduados en postgrado por nivel de formación - sede Palmira', mensaje = "Número de estudiantes graduados en postgrado por nivel de formación - sede Palmira", titulo = "Graduados en postgrado por nivel de formación - sede Palmira");NIVEL_TABLA
NIVEL_SERIE <- series(datos = Consolidado, categoria = "NIVEL", colores = col, titulo = "Evolución del número de estudiantes graduados en postgrado por nivel de formación - sede Palmira", eje = "Número de graduados");NIVEL_SERIE
NIVEL_ACTUAL <- torta(datos = Consolidado, variable = "NIVEL", colores = col, titulo = "Distribución de graduados en postgrado por nivel de formación - sede Palmira", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NIVEL_ACTUAL


# Nacionalidad ---

col <-   c( "#8cc63f", # verde, Colombiana
            "#f15a24", # naranja, Extranjero
            "#0071bc") # azul vivo, sin información


NACIONALIDAD_TABLA <- tabla(datos = Consolidado, categoria = "NACIONALIDAD", variable = 'Total estudiantes graduados en postgrado según nacionalidad - sede Palmira', mensaje = "Número de estudiantes graduados en postgrado por nacionalidad - sede Palmira", titulo = "Graduados en postgrado según nacionalidad - sede Palmira");NACIONALIDAD_TABLA
NACIONALIDAD_SERIE <- series(datos = Consolidado, categoria = "NACIONALIDAD", colores = col, titulo = "Evolución del número de graduados en postgrado según nacionalidad - sede Palmira", eje = "Número de graduados (k: miles)");NACIONALIDAD_SERIE
NACIONALIDAD_ACTUAL <- torta(datos = Consolidado, variable = "NACIONALIDAD", colores = col, titulo = "Distribución de graduados en postgrado según nacionalidad - sede Palmira", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);NACIONALIDAD_ACTUAL


# Sexo ---

col <-   c( "#f15a24", # naranja, hombres
            "#8cc63f") # verde, mujeres


SEXO_TABLA <- tabla(datos = Consolidado, categoria = "SEXO", variable = 'Total graduados en postgrado por sexo - sede Palmira', mensaje = "Número de graduados en postgrado por sexo - sede Palmira", titulo = "Graduados en postgrado por sexo - sede Palmira");SEXO_TABLA
SEXO_SERIE <- series(datos = Consolidado, categoria = "SEXO", colores = col, titulo = "Evolución del número de graduados en postgrado por sexo - sede Palmira", eje = "Número de graduados (k: miles)");SEXO_SERIE
SEXO_ACTUAL <- torta(datos = Consolidado, variable = "SEXO", colores = col, titulo = "Distribución de graduados en postgrado por sexo - sede Palmira", etiqueta = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);SEXO_ACTUAL


# Edad ---

col <-   c( "#8cc63f", # verde, 25 años o menos
            "#0071bc", # azul vivo, 26 a 30 años
            "#f15a24", # naranja, 31 a 35 años
            "#fbb03b", # amarillo, 36 años o más
            "#6d6666")  # gris, sin información 


CAT_EDAD_TABLA <- tabla(datos = Consolidado, categoria = "CAT_EDAD", variable = 'Total graduados en postgrado por grupos de edad - sede Palmira', mensaje = "Número de graduados en postgrado por grupos de edad - sede Palmira", titulo = "Graduados en postgrado por grupos de edad - sede Palmira");CAT_EDAD_TABLA
CAT_EDAD_SERIE <- series(datos = Consolidado, categoria = "CAT_EDAD", colores = col, titulo = "Evolución del número de graduados en postgrado por grupos de edad - sede Palmira", eje = "Número de graduados (k: miles)");CAT_EDAD_SERIE
CAT_EDAD_ACTUAL <- barra_vertical_ord(datos = Consolidado, categoria = "CAT_EDAD", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en postgrado por grupos de edad - sede Palmira", eje = "Número de graduados (k: miles)"); CAT_EDAD_ACTUAL


# Convenios  ---


col <-   c( "#f15a24", # naranja, No
            "#8cc63f", # verde, Sí
            "#0071bc") # azul vivo, sin información


CONVENIO_TABLA <- tabla(datos = Consolidado, categoria = "CONVENIO", variable = 'Total estudiantes graduados en postgrado por convenios - sede Palmira', mensaje = "Total de estudiantes graduados en postgrado por convenios - sede Palmira", titulo = "Estudiantes graduados en postgrado por convenios - sede Palmira");CONVENIO_TABLA
CONVENIO_SERIE <- series(datos = Consolidado, categoria = "CONVENIO", colores = col, titulo = "Evolución del número de estudiantes graduados en postgrado por convenios - sede Palmira", eje = "Número de graduados (k: miles)");CONVENIO_SERIE
CONVENIO_ACTUAL <- drilldown_si_no_torta(datos = Consolidado, categoria = "CONVENIO", categoria_drilldown = "TIP_CONVENIO", colores = col, titulo = "Distribución de graduados en postgrado en convenios - sede Palmira, periodo", titulo_drilldown = "Graduados", etiqueta = "Total de graduados", eje = "Número de graduados", ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo);CONVENIO_ACTUAL


# Área SNIES ---

col <-   c( "#93278f", # morado, Agronomía, veterinaria y afines
            "#29abe2", # azul claro, Bellas artes
            "#c1272d",  # rojo, Ciencias de la educación
            "#0071bc", # azul vivo, Ciencias de la salud
            "#f15a24", # naranja, Ciencias sociales y humanas
            "#fbb03b", # amarillo, Economía, administración, contaduria y afines
            "#8cc63f", # verde, Ingenieria, arquitectura, urbanismo y afines
            "#6d6666") # gris, Matemáticas y ciencias naturales 


AREAC_SNIES_TABLA <- tabla(datos = Consolidado, categoria = "AREAC_SNIES", variable = 'Total graduados en postgrado por áreas del conocimiento SNIES - sede Palmira', mensaje = "Total de graduados en postgrado por áreas del conocimiento SNIES - sede Palmira", titulo = "Sede graduados en postgrado por áreas del conocimiento SNIES - sede Palmira");AREAC_SNIES_TABLA
AREAC_SNIES_SERIE <- series(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, titulo = "Evolución del número de graduados en postgrado por áreas del conocimiento SNIES - sede Palmira", eje = "Número de graduados (k: miles)");AREAC_SNIES_SERIE
AREAC_SNIES_ACTUAL <- barra_horizontal(datos = Consolidado, categoria = "AREAC_SNIES", colores = col, ano = ano, periodo = semestre, periodo_titulo = periodo_actual_titulo, titulo = "Distribución de graduados en postgrado por áreas del conocimiento SNIES - sede Palmira", eje = "Número de graduados"); AREAC_SNIES_ACTUAL


# Exportar ----

Salvar(EVOLUCION_SERIE, "G_Graduados/Pal/Postgrado", "Serie.html")
Salvar(FACULTAD_TABLA, "G_Graduados/Pal/Postgrado", "T_facultad.html")
Salvar(FACULTAD_SERIE, "G_Graduados/Pal/Postgrado", "S_facultad.html" )
Salvar(FACULTAD_ACTUAL, "G_Graduados/Pal/Postgrado", "A_facultad.html" )
Salvar(NIVEL_TABLA, "G_Graduados/Pal/Postgrado", "T_nivel.html" )
Salvar(NIVEL_SERIE, "G_Graduados/Pal/Postgrado", "S_nivel.html" )
Salvar(NIVEL_ACTUAL, "G_Graduados/Pal/Postgrado", "A_nivel.html" )
Salvar(NACIONALIDAD_TABLA, "G_Graduados/Pal/Postgrado", "T_nacionalidad.html")
Salvar(NACIONALIDAD_SERIE, "G_Graduados/Pal/Postgrado", "S_nacionalidad.html")
Salvar(NACIONALIDAD_ACTUAL, "G_Graduados/Pal/Postgrado", "A_nacionalidad.html")
Salvar(SEXO_TABLA, "G_Graduados/Pal/Postgrado", "T_sexo.html")
Salvar(SEXO_SERIE, "G_Graduados/Pal/Postgrado", "S_sexo.html")
Salvar(SEXO_ACTUAL, "G_Graduados/Pal/Postgrado", "A_sexo.html")
Salvar(CAT_EDAD_TABLA, "G_Graduados/Pal/Postgrado", "T_edad.html")
Salvar(CAT_EDAD_SERIE, "G_Graduados/Pal/Postgrado", "S_edad.html")
Salvar(CAT_EDAD_ACTUAL, "G_Graduados/Pal/Postgrado", "A_edad.html" )
Salvar(CONVENIO_TABLA, "G_Graduados/Pal/Postgrado", "T_convenio.html")
Salvar(CONVENIO_SERIE, "G_Graduados/Pal/Postgrado", "S_convenio.html") 
Salvar(CONVENIO_ACTUAL, "G_Graduados/Pal/Postgrado", "A_convenio.html")
Salvar(AREAC_SNIES_TABLA, "G_Graduados/Pal/Postgrado", "T_snies.html")
Salvar(AREAC_SNIES_SERIE, "G_Graduados/Pal/Postgrado", "S_snies.html")
Salvar(AREAC_SNIES_ACTUAL, "G_Graduados/Pal/Postgrado", "A_snies.html")









