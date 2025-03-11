###############################################################################

# Title            : Explotación gráfica Datos Consolidados 2023 - DGT
# Project          : Informe social-leasing BC3+T&E
# Date of creation : 03/02/2025
# Last update      : 11/03/2025

# Author 1         : Eva Alonso Epelde (eva.alonso[at]bc3research.org)
# Author 2         : Mercè Amich Vidal (merce.amich[at]ehu.eus)
# Institution      : Basque Centre for Climate Change (BC3)

# Last run time    : 24.39 min

###############################################################################

# ************************************************************
# 1. Preliminares y definición de parametros ----
# ************************************************************

# Clean environment
rm(list = ls(all = TRUE))    

# Define start time
start_time <- Sys.time()

# Define language
Sys.setenv(LANG = "en")     

# Install and load any required R-package
packages_loaded <- installed.packages()
packages_needed <- c("dplyr"      ,
                     "stringr"    ,
                     "sqldf"      ,
                     "reshape2"   ,
                     "readxl"     , 
                     "openxlsx"   ,
                     "here"       ,
                     "data.table" ,
                     "spatstat"   ,
                     "ggplot2"    , 
                     "showtext"   ,
                     "extrafont"  ,
                     "fastDummies",
                     "cluster"    ,
                     "fpc"        , 
                     "purrr"      ,
                     "dendextend" ,
                     "mapSpain"   ,
                     "leaflet"    ,
                     "geojsonio"  ,
                     "htmltools"  ,
                     "plotly"     ,
                     "tidyverse"  ,
                     "sf"         ,
                     "stringi"    , 
                     "RColorBrewer",
                     "scales"     ,
                     "htmlwidgets"
                     
                     )


for ( p in packages_needed) {
  if (!p %in% row.names(packages_loaded)) install.packages(p)
  eval(bquote(library(.(p))))
}

loadfonts(device = "win")

# Define paths
path <- here()

# Define working directory
setwd(path)

# ************************************************************
# 2. Load data & create new variables ----
# ************************************************************

# Load transport data
data <- read.xlsx("datos_transporte.xlsx")

data <- data %>% 
  mutate(
    # Índices de motorización
    censo_pc                = Censo.Conductores   / Población.Total,
    parque_pc               = Parque.Total        / Población.Total,
    parque_ciclomotores_pc  = Parque.Ciclomotores / Población.Total,
    parque_motocicletas_pc  = Parque.Motocicletas / Población.Total,
    parque_turismos_pc      = Parque.Turismos     / Población.Total,
    parque_furgonetas_pc    = Parque.Furgonetas   / Población.Total,
    parque_camiones_pc      = Parque.Camiones     / Población.Total,
    
    # Antigüedad media de vehículos
    antiguedad_tot          = `Antigüedad.Media.del.Parque.(<25.años)`,
    antiguedad_ciclomotores = Antigüedad.Media.de.Ciclomotores,
    antiguedad_motocicletas = Antigüedad.Media.de.Motocicletas,
    antiguedad_turismos     = Antigüedad.Media.de.Turismos,
    antiguedad_furgonetas   = Antigüedad.Media.de.Furgonetas,
    antiguedad_camiones     = Antigüedad.Media.de.Camiones,
    
    # Composición del parque vehicular
    pct_ciclomotores        = (Parque.Ciclomotores / Parque.Total) * 100,
    pct_motocicletas        = (Parque.Motocicletas / Parque.Total) * 100,
    pct_turismos            = (Parque.Turismos     / Parque.Total) * 100,
    pct_furgonetas          = (Parque.Furgonetas   / Parque.Total) * 100,
    pct_camiones            = (Parque.Camiones     / Parque.Total) * 100,
    
    # Vehículos sin ITV (<25 años)
    pct_vehiculos_sin_ITV   = ((`Motocicletas.sin.ITV.(<25.años)` + `Turismos.sin.ITV.(<25.años)` 
                                + `Resto.de.Vehículos.sin.ITV.(<25.años)`) / Parque.Total) * 100,
    pct_ITV_turismos        = (`Turismos.sin.ITV.(<25.años)`     / Parque.Turismos) * 100,
    pct_ITV_motocicletas    = (`Motocicletas.sin.ITV.(<25.años)` / Parque.Motocicletas) * 100,
    
    # Vehículos antiguos (≥15 años)
    pct_turismos_15_mas     = ((`Parque.Turismos.(<25.años)`   - `Parque.Turismos.(<15.años)`) 
                               / Parque.Turismos) * 100,
    pct_furgonetas_15_mas   = ((`Parque.Furgonetas.(<25.años)` - `Parque.Furgonetas.(<15.años)`) 
                               / Parque.Furgonetas) * 100,
    pct_camiones_15_mas     = ((`Parque.Camiones.(<25.años)`   - `Parque.Camiones.(<15.años)`) 
                               / Parque.Camiones) * 100,
    
    # Distribución de distintivos ambientales
    distintivo_B            = (Distintivo.B                    / Parque.Total) * 100,
    distintivo_C            = (Distintivo.C                    / Parque.Total) * 100,
    distintivo_ECO          = (Distintivo.ECO                  / Parque.Total) * 100,
    distintivo_0            = (Distintivo.0                    / Parque.Total) * 100,
    sin_distintivo          = (Sin.Distintivo                  / Parque.Total) * 100,
    distintivo_ECO_0        = ((Distintivo.ECO + Distintivo.0) / Parque.Total) * 100,
    
    # Variable dummy para zona rural
    rural                   = ifelse(is.na(`ES.RURAL?`), 0, 1)
  )


# ************************************************************
# 3. Preparar spatial data (shapefiles) & merge with "data" ----
# ************************************************************

# [1] Prepare spatial files $ merge them with "data"  ----

# [a] Download shapefiles via "mapSpain" ----

ccaa       <- esp_get_ccaa(epsg = "4326")
provincias <- esp_get_prov(epsg = "4326")
municipios <- esp_get_munic(epsg = "4326")

# [b] Match names in "data" and "shapefiles" ----
# Needed to merge our "data" with the spatial polygons in the .sf

## [i] Comunidades Autónomas----

### Identify differences in both objects
diff1 <- setdiff(unique(data$Comunidad.Autónoma), unique(ccaa$ine.ccaa.name))
diff2 <- setdiff(unique(ccaa$ine.ccaa.name), unique(data$Comunidad.Autónoma))  

diff1 # Elements in data$ not present in $ccaa
diff2 # Elements in ccaa$ not present in data$

### Recode data$
data$Comunidad.Autónoma <- recode(data$Comunidad.Autónoma,
                  # OLD NAMES:                    # NEW NAMES:
                  "Castilla-La Mancha"           = "Castilla - La Mancha",
                  "Asturias (Principado de)"     = "Asturias, Principado de",
                  "Balears (Illes)"              = "Balears, Illes",
                  "Madrid (Comunidad de)"        = "Madrid, Comunidad de",
                  "Murcia (Región de)"           = "Murcia, Región de",
                  "Navarra (Comunidad Foral de)" = "Navarra, Comunidad Foral de",
                  "Rioja (La)"                   = "Rioja, La"
)

### Check consistency
diff1 <- setdiff(unique(data$Comunidad.Autónoma), unique(ccaa$ine.ccaa.name))
diff2 <- setdiff(unique(ccaa$ine.ccaa.name), unique(data$Comunidad.Autónoma))  

diff1 # Elements in data$ not present in $ccaa
diff2 # Elements in ccaa$ not present in data$

## [ii] Províncias----

### Identify differences in both objects
diff1 <- setdiff(unique(data$Provincia), unique(provincias$ine.provname))
diff2 <- setdiff(unique(provincias$ine.prov.name), unique(data$Provincia))  

diff1 # Elements in data$ not present in $provincias
diff2 # Elements in provincias$ not present in data$

### Recode data$
data$Provincia <- recode(data$Provincia,
                         # OLD NAMES:        # NEW NAMES:
                         "Balears (Illes)" = "Balears, Illes",
                         "Palmas (Las)"    = "Palmas, Las",
                         "Coruña (A)"      = "Coruña, A",
                         "Rioja (La)"      = "Rioja, La"
)

### Check consistency
diff1 <- setdiff(unique(data$Provincia), unique(provincias$ine.prov.name))
diff2 <- setdiff(unique(provincias$ine.prov.name), unique(data$Provincia))  

diff1 # Elements in data$ not present in $ccaa
diff2 # Elements in ccaa$ not present in data$

## [iii] Municipios ####

# España tiene 8131 municipios según el INE y "data" tiene 8133 observaciones

# Identificamos qué observaciones difieren comparando el LAU_CODE
diff1 <- setdiff(unique(data$`Código.INE`), unique(municipios$LAU_CODE))
diff2 <- setdiff(unique(municipios$LAU_CODE), unique(data$`Código.INE`))

diff1 # Códigos en data$`Código.INE` que no están en municipios$LAU_CODE
diff2 # Códigos en municipios$LAU_CODE que no están en data$`Código.INE`

# diff1 devuelve los códigos "01000" y "36012" como distintos. 

# Relativo a diff1, los inspeccionamos:
data$Municipio[data$`Código.INE` == "01000"] # "Alava ( municipio sin especificar)"
data$Municipio[data$`Código.INE` == "36012"] # "Cerdedo-Cotobade"

# En el primer caso, eliminamos la observación
# Serán vehículos sin municipio especificado en el sistema central DGT (vía @Oscar Pulido T&E 14.02.25)
data <- subset(data, Código.INE != "01000")

# El segundo caso refiere a un municipio de Pontevedra de nueva creación (2016)
# Es la fusión de Cerdedo [[36012]] y Cotobade [[36902]] (antiguos)
# El "shapefile" ("municipios") tiene la geometría para el municipio fusionado
# Habrá que fusionar la información de los viejos en las métricas de "data"

### Identify differences in both objects
diff1 <- setdiff(unique(data$Municipio), unique(municipios$name))
diff2 <- setdiff(unique(municipios$name), unique(data$Municipio))  

diff1 # Elements in data$ not present in $ccaa (159 diferencias)
diff2 # Elements in ccaa$ not present in data$ (159 diferencias)

### Recode municipios$

# Si inspeccionamos "diff1" y "diff2" vemos que muchas diferencias de las 160 son debidas a que haya o no un espacio entre la barra horizontal separadora (/) en los municipios con denominación bilingüe. Añadimos el espacio para harmonizar los nombres en "data$" con "municipios$"

data$Municipio <- gsub("/", " / ", data$Municipio)

# Check how many differences are left
diff1 <- setdiff(unique(data$Municipio), unique(municipios$name))
diff2 <- setdiff(unique(municipios$name), unique(data$Municipio))  

diff1 # Elements in data$ not present in $municipios (38 diferencias)
diff2 # Elements in municipios$ not present in data$ (38 diferencias)

# Dos tipos de cambios a recodificar:
# a) Nombres franquistas en "municipios" (vía GISCO Eurostat)
# b) Discrepancias tipográficas (p.ej, "A Coruña" y "Coruña A")

# Tiene que hacerse manualmente, y los cambios tienen que hacerse en municipios$:

municipios$name <- recode(municipios$name,  
              # OLD NAMES:                  # NEW NAMES:
              "Albánchez"                        = "Albanchez",
              "Almazora"                         = "Almassora",
              "Algimia de Alfara"                = "Algímia d'Alfara",
              "Benlloch"                         = "Benlloc",
              "Brunyola"                         = "Brunyola i Sant Martí Sapresa",
              "Cañiza, A"                        = "A Cañiza",
              "Calonge"                          = "Calonge i Sant Antoni",
              "Castellón de la Plana / Castelló de la Plana" = "Castelló de la Plana",
              "Coruña, A"                        = "A Coruña",
              "Ejeme"                            = "Éjeme",
              "Estrada, A"                       = "A Estrada",
              "Genovés"                          = "Genovés, el",
              "Guarda, A"                        = "A Guarda",
              "Guadiana del Caudillo"            = "Guadiana",
              "Gudiña, A"                        = "A Gudiña",
              "Herbés"                           = "Herbers",
              "Íllar"                            = "Illar", 
              "Iglesuela, La"                    = "Iglesuela del Tiétar, La",
              "Jerez del Marquesado"             = "Jérez del Marquesado",
              "Jarque"                           = "Jarque de Moncayo",
              "Laracha, A"                       = "A Laracha",
              "Lezáun"                           = "Lezaun",
              "Lumbreras"                        = "Lumbreras de Cameros",
              "Medina-Sidonia"                   = "Medina Sidonia",
              "Mezquita, A"                      = "A Mezquita",
              "Mendigorría"                      = "Mendigorria",
              "Náquera"                          = "Nàquera / Náquera",
              "Palma de Mallorca"                = "Palma",
              "Pobra do Caramiñal, A"            = "A Pobra do Caramiñal",
              "Pontenova, A"                     = "A Pontenova",
              "Pradales"                         = "Carabias", # En 2016 cambia el nombre
              "Rótova"                           = "Ròtova",
              "Ribera Baja / Erribera Beitia"    = "Erriberabeitia",
              "Teixeira, A"                      = "A Teixeira",
              "Valencia"                         = "València",
              "Veiga, A"                         = "A Veiga",
              "Villanueva de Castellón"          = "Castelló", # Castelló de la Ribera
              "Villafranca del Cid / Vilafranca" = "Vilafranca / Villafranca del Cid"
)

# Nota 1: En 2016, "Pradales" cambia su nombre a "Carabias"
# Nota 2, comprobado:
# "Villanueva de Castellón" es "Castelló" (de la Ribera) [Código INE / LAU: 46257]
# "Castellón de la Plana" es Castelló de la Plana [Código INE / LAU: 12040]

# Check how many differences are left (previous: 38)
diff1 <- setdiff(unique(data$Municipio), unique(municipios$name))
diff2 <- setdiff(unique(municipios$name), unique(data$Municipio))  

diff1 # Elements in data$ not present in $municipios (12 diferencias)
diff2 # Elements in municipios$ not present in data$ (11 diferencias)

# Fusionamos datos de Cerdedo y Cotobade:
data_fusionada <- data %>%
  filter(`Código.INE` %in% c(36012, 36902)) %>%
  summarise(
    `Código.INE`                 = 36902,
    `Municipio`                   = "Cerdedo-Cotobade",
    `Provincia`                   = unique(`Provincia`),
    `Comunidad.Autónoma`          = unique(`Comunidad.Autónoma`),
    `ES.RURAL?`                   = unique(`ES.RURAL?`),
    `X6`                          = unique(`X6`),
    `Población.Total`             = sum(`Población.Total`),
    `Población.Hombres`           = sum(`Población.Hombres`),
    `Población.Mujeres`           = sum(`Población.Mujeres`),
    `Conductores.Hombres`         = sum(`Conductores.Hombres`),
    `Conductoras.Mujeres`         = sum(`Conductoras.Mujeres`),
    `Censo.Conductores`           = sum(`Censo.Conductores`),
    `Parque.Ciclomotores`         = sum(`Parque.Ciclomotores`),
    `Parque.Motocicletas`         = sum(`Parque.Motocicletas`),
    `Parque.Turismos`             = sum(`Parque.Turismos`),
    `Parque.Furgonetas`           = sum(`Parque.Furgonetas`),
    `Parque.Camiones`             = sum(`Parque.Camiones`),
    `Parque.Total`                = sum(`Parque.Total`),
    `Motocicletas.sin.ITV.(<25.años)` = sum(`Motocicletas.sin.ITV.(<25.años)`),
    `Turismos.sin.ITV.(<25.años)`     = sum(`Turismos.sin.ITV.(<25.años)`),
    `Resto.de.Vehículos.sin.ITV.(<25.años)` = sum(`Resto.de.Vehículos.sin.ITV.(<25.años)`),
    `Parque.Ciclomotores.(<25.años)` = sum(`Parque.Ciclomotores.(<25.años)`),
    `Parque.Ciclomotores.(<15.años)` = sum(`Parque.Ciclomotores.(<15.años)`),
    `Parque.Ciclomotores.(<8.años)`  = sum(`Parque.Ciclomotores.(<8.años)`),
    `Parque.Ciclomotores.(<4.años)`  = sum(`Parque.Ciclomotores.(<4.años)`),
    `Parque.Motocicletas.(<25.años)` = sum(`Parque.Motocicletas.(<25.años)`),
    `Parque.Motocicletas.(<15.años)` = sum(`Parque.Motocicletas.(<15.años)`),
    `Parque.Motocicletas.(<8.años)`  = sum(`Parque.Motocicletas.(<8.años)`),
    `Parque.Motocicletas.(<4.años)`  = sum(`Parque.Motocicletas.(<4.años)`),
    `Parque.Turismos.(<25.años)`    = sum(`Parque.Turismos.(<25.años)`),
    `Parque.Turismos.(<15.años)`    = sum(`Parque.Turismos.(<15.años)`),
    `Parque.Turismos.(<8.años)`     = sum(`Parque.Turismos.(<8.años)`),
    `Parque.Turismos.(<4.años)`     = sum(`Parque.Turismos.(<4.años)`),
    `Parque.Furgonetas.(<25.años)`  = sum(`Parque.Furgonetas.(<25.años)`),
    `Parque.Furgonetas.(<15.años)`  = sum(`Parque.Furgonetas.(<15.años)`),
    `Parque.Furgonetas.(<8.años)`   = sum(`Parque.Furgonetas.(<8.años)`),
    `Parque.Furgonetas.(<4.años)`   = sum(`Parque.Furgonetas.(<4.años)`),
    `Parque.Camiones.(<25.años)`    = sum(`Parque.Camiones.(<25.años)`),
    `Parque.Camiones.(<15.años)`    = sum(`Parque.Camiones.(<15.años)`),
    `Parque.Camiones.(<8.años)`     = sum(`Parque.Camiones.(<8.años)`),
    `Parque.Camiones.(<4.años)`     = sum(`Parque.Camiones.(<4.años)`),
    `Antigüedad.Media.del.Parque.(<25.años)` = sum(`Antigüedad.Media.del.Parque.(<25.años)`),
    `Antigüedad.Media.de.Ciclomotores` = sum(`Antigüedad.Media.de.Ciclomotores`),
    `Antigüedad.Media.de.Motocicletas` = sum(`Antigüedad.Media.de.Motocicletas`),
    `Antigüedad.Media.de.Turismos` = sum(`Antigüedad.Media.de.Turismos`),
    `Antigüedad.Media.de.Furgonetas` = sum(`Antigüedad.Media.de.Furgonetas`),
    `Antigüedad.Media.de.Camiones` = sum(`Antigüedad.Media.de.Camiones`),
    `Distintivo.B`                = sum(`Distintivo.B`),
    `Distintivo.C`                = sum(`Distintivo.C`),
    `Distintivo.ECO`              = sum(`Distintivo.ECO`),
    `Distintivo.0`                = sum(`Distintivo.0`),
    `Sin.Distintivo`              = sum(`Sin.Distintivo`),
    `Total.Campañas`              = sum(`Total.Campañas`),
    `censo_pc`                    = sum(`censo_pc`),
    `parque_pc`                   = sum(`parque_pc`),
    `parque_ciclomotores_pc`      = sum(`parque_ciclomotores_pc`),
    `parque_motocicletas_pc`      = sum(`parque_motocicletas_pc`),
    `parque_turismos_pc`          = sum(`parque_turismos_pc`),
    `parque_furgonetas_pc`        = sum(`parque_furgonetas_pc`),
    `parque_camiones_pc`          = sum(`parque_camiones_pc`),
    `antiguedad_tot`              = sum(`antiguedad_tot`),
    `antiguedad_ciclomotores`    = sum(`antiguedad_ciclomotores`),
    `antiguedad_motocicletas`    = sum(`antiguedad_motocicletas`),
    `antiguedad_turismos`        = sum(`antiguedad_turismos`),
    `antiguedad_furgonetas`      = sum(`antiguedad_furgonetas`),
    `antiguedad_camiones`        = sum(`antiguedad_camiones`),
    `pct_ciclomotores`           = sum(`pct_ciclomotores`),
    `pct_motocicletas`           = sum(`pct_motocicletas`),
    `pct_turismos`               = sum(`pct_turismos`),
    `pct_furgonetas`             = sum(`pct_furgonetas`),
    `pct_camiones`               = sum(`pct_camiones`),
    `pct_vehiculos_sin_ITV`      = sum(`pct_vehiculos_sin_ITV`),
    `pct_ITV_turismos`           = sum(`pct_ITV_turismos`),
    `pct_ITV_motocicletas`       = sum(`pct_ITV_motocicletas`),
    `pct_turismos_15_mas`        = sum(`pct_turismos_15_mas`),
    `pct_furgonetas_15_mas`      = sum(`pct_furgonetas_15_mas`),
    `pct_camiones_15_mas`        = sum(`pct_camiones_15_mas`),
    `distintivo_B`               = sum(`distintivo_B`),
    `distintivo_C`               = sum(`distintivo_C`),
    `distintivo_ECO`             = sum(`distintivo_ECO`),
    `distintivo_0`               = sum(`distintivo_0`),
    `sin_distintivo`             = sum(`sin_distintivo`),
    `distintivo_ECO_0`           = sum(`distintivo_ECO_0`)
  )

# Ensure "Código INE" and "LAU_CODE" are numeric
data$`Código.INE`   <- as.numeric(data$`Código.INE`)
municipios$LAU_CODE <- as.numeric(municipios$LAU_CODE)

# Remove both rows with Código INE 36012 and 36902
data <- data %>%
  filter(!`Código.INE` %in% c(36012, 36902))

# Add the new row (data_fusionada)
data <- data %>%
  bind_rows(data_fusionada)
remove("data_fusionada")

# [c] Crear métricas autonómicas y provinciales y hacer merge con shapefiles ----

## Autonómicas
ccaa_data <- data %>%
  group_by(Comunidad.Autónoma) %>%
  summarise(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    rural = mean(rural, na.rm = TRUE) * 100  # Porcentaje de municipios rurales
  )

ccaa_data <- merge(ccaa_data, ccaa, 
                   by.x = "Comunidad.Autónoma", 
                   by.y = "ine.ccaa.name", 
                   all = TRUE)

## Provinciales
provincias_data <- data %>%
  group_by(Provincia) %>%
  summarise(
    across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
    rural = mean(rural, na.rm = TRUE) * 100  # Porcentaje de municipios rurales
  )

provincias_data <- merge(provincias_data, provincias, 
                   by.x = "Provincia", 
                   by.y = "ine.prov.name", 
                   all = TRUE)


#### Municipales
municipios_data <- merge(data, municipios,
                         by.x = "Código.INE",
                         by.y = "LAU_CODE",
                         all  = TRUE)

# ************************************************************
# 4. Preparación comuna mapas estáticos y dinámicos ----
# ************************************************************

# Definir directorio principal
main_path <- paste0(path, "/mapas_estaticos")
dir.create(main_path, recursive = TRUE, showWarnings = FALSE)
setwd(main_path)

# Función para crear subdirectorios
create_subdir <- function(subfolder) {
  sub_path <- file.path(main_path, subfolder)
  dir.create(sub_path, recursive = TRUE, showWarnings = FALSE)
  setwd(sub_path)
}

# Función para crear los breaks de los plots
get_breaks <- function(data, n_bins = 6) {
  
  # Calcular los percentiles
  breaks <- quantile(data, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE)
  
  # Si los breaks no son únicos, forzar exactamente 6 intervalos con 'pretty'
  if (length(unique(breaks)) != length(breaks)) {
    breaks <- pretty(data, n_bins)
  }
  
  # Redondear solo las etiquetas de la leyenda, no los breaks
  labels <- paste0("[", round(head(breaks, -1), 2), "-", round(tail(breaks, -1), 2), "]")
  
  # Devolver los breaks y las etiquetas
  return(list(breaks = breaks, labels = labels))
}


# Definir métricas to plot
metricas <- c(
  # Índices de motorización
  "censo_pc", "parque_pc", 
  "parque_ciclomotores_pc",
  "parque_motocicletas_pc",
  "parque_turismos_pc",
  "parque_furgonetas_pc",
  "parque_camiones_pc",
  
  # Antigüedad media de vehículos
  "antiguedad_tot", 
  "antiguedad_ciclomotores", 
  "antiguedad_motocicletas",
  "antiguedad_turismos", 
  "antiguedad_furgonetas",
  "antiguedad_camiones",
  
  # Composición del parque vehicular
  "pct_ciclomotores", 
  "pct_motocicletas",
  "pct_turismos",
  "pct_furgonetas",
  "pct_camiones",
  
  # Vehículos sin ITV
  "pct_vehiculos_sin_ITV", 
  "pct_ITV_turismos",
  "pct_ITV_motocicletas",
  
  # Vehículos antiguos (≥15 años)
  "pct_turismos_15_mas",
  "pct_furgonetas_15_mas",
  "pct_camiones_15_mas",
  
  # Distribución de distintivos ambientales
  "distintivo_B", 
  "distintivo_C",
  "distintivo_ECO",
  "distintivo_0",
  "sin_distintivo",
  "distintivo_ECO_0"
)

# Crear subdirectorios por familias de mapas
familia_mapas <- list(
  # Índices de motorización
  "mapas_motorizacion" = c("censo_pc", 
                           "parque_pc",
                           "parque_ciclomotores_pc",
                           "parque_motocicletas_pc",
                           "parque_turismos_pc",
                           "parque_furgonetas_pc",
                           "parque_camiones_pc"),
  
  # Antigüedad media de vehículos
  "mapas_antiguedad" = c("antiguedad_tot",
                         "antiguedad_ciclomotores",
                         "antiguedad_motocicletas",
                         "antiguedad_turismos", 
                         "antiguedad_furgonetas",
                         "antiguedad_camiones"),
  
  # Composición del parque vehicular
  "mapas_composicion" = c("pct_ciclomotores", 
                          "pct_motocicletas",
                          "pct_turismos",
                          "pct_furgonetas",
                          "pct_camiones"),
  
  # Vehículos sin ITV
  "mapas_ITV" = c("pct_vehiculos_sin_ITV", 
                  "pct_ITV_turismos",
                  "pct_ITV_motocicletas"),
  
  # Vehículos antiguos (≥15 años)
  "mapas_antiguos" = c("pct_turismos_15_mas", 
                       "pct_furgonetas_15_mas",
                       "pct_camiones_15_mas"),
  
  # Distribución de distintivos ambientales
  "mapas_etiquetas" = c("distintivo_B",
                        "distintivo_C",
                        "distintivo_ECO",
                        "distintivo_0",
                        "sin_distintivo",
                        "distintivo_ECO_0")
)

# Función para determinar la unidad y títulos basados en el tipo de variable
get_unit_and_title <- function(var) {
  
  # Check for 'antiguedad' to assign "años"
  if (grepl("antiguedad", var)) {
    return(list(unit = " años", title = titulos[[var]]))
    
    # Check for 'pct' to assign "%"
  } else if (grepl("pct", var)) {
    return(list(unit = " %", title = titulos[[var]]))
    
    # Check for 'pc' to assign "per cápita"
  } else if (grepl("pc", var)) {
    return(list(unit = " per cápita", title = titulos[[var]]))
  }
  
  # El resto
  return(list(unit = " %", title = titulos[[var]]))
}

# Plot titles
titulos <- c(
  "censo_pc"                     = "Índice de motorización (conductores censados por cápita)",
  "parque_pc"                    = "Número de vehículos per cápita",
  "parque_ciclomotores_pc"       = "Ciclomotores per cápita",
  "parque_motocicletas_pc"       = "Motocicletas per cápita",
  "parque_turismos_pc"           = "Turismos per cápita",
  "parque_furgonetas_pc"         = "Furgonetas per cápita",
  "parque_camiones_pc"           = "Camiones per cápita",
  "antiguedad_tot"               = "Antigüedad media del parque de vehículos",
  "antiguedad_ciclomotores"      = "Antigüedad media del parque de ciclomotores",
  "antiguedad_motocicletas"      = "Antigüedad media del parque de motocicletas",
  "antiguedad_turismos"          = "Antigüedad media del parque de turismos",
  "antiguedad_furgonetas"        = "Antigüedad media del parque de furgonetas",
  "antiguedad_camiones"          = "Antigüedad media del parque de camiones",
  "distintivo_B"                 = "Prevalencia de vehículos con Distintivo B",
  "distintivo_C"                 = "Prevalencia de vehículos con Distintivo C",
  "distintivo_ECO"               = "Prevalencia de vehículos con Distintivo ECO",
  "sin_distintivo"               = "Prevalencia de vehículos sin distintivo ambiental",
  "distintivo_0"                 = "Prevalencia de vehículos con Distintivo 0",
  "pct_ciclomotores"             = "Porcentaje de ciclomotores en el parque vehicular",
  "pct_motocicletas"             = "Porcentaje de motocicletas en el parque vehicular",
  "pct_turismos"                 = "Porcentaje de turismos en el parque vehicular",
  "pct_furgonetas"               = "Porcentaje de furgonetas en el parque vehicular",
  "pct_camiones"                 = "Porcentaje de camiones en el parque vehicular",
  "pct_vehiculos_sin_ITV"        = "Porcentaje de vehículos (<25 años) sin ITV",
  "pct_ITV_turismos"             = "Porcentaje de turismos sin ITV (<25 años)",
  "pct_ITV_motocicletas"         = "Porcentaje de motocicletas sin ITV (<25 años)",
  "pct_turismos_15_mas"          = "Porcentaje de turismos con antigüedad (entre 15 y 25 años)",
  "pct_furgonetas_15_mas"        = "Porcentaje de furgonetas con antigüedad (entre 15 y 25 años)",
  "pct_camiones_15_mas"          = "Porcentaje de camiones con antigüedad (entre 15 y 25 años)",
  "distintivo_ECO_0"             = "Prevalencia de vehículos con Distintivo ECO o 0"
)

# ************************************************************
# 5. Crear mapas estáticos municipales para todas las métricas ----
# ************************************************************

## A) ÁREAS NO RURALES A NA'S

# Definir directorio principal
path_maps <- paste0(main_path, "/mapas_municipales/rural")
dir.create(path_maps, recursive = TRUE, showWarnings = FALSE)
setwd(path_maps)

# Función para crear subdirectorios por familia si no existen
create_subdir <- function(familia) {
  subdir_path <- file.path(path_maps, familia)
  if (!dir.exists(subdir_path)) {
    dir.create(subdir_path)
  }
}

# Loop para generar los mapas
for (familia in names(familia_mapas)) {
  
  # Crear subdirectorio para la familia de mapas dentro de 'mapas_municipales'
  create_subdir(familia)
  
  for (metrica in familia_mapas[[familia]]) {
    
    # Obtener la unidad y el título de la métrica
    metrica_info <- get_unit_and_title(metrica)
    unidad <- metrica_info$unit
    titulo <- metrica_info$title
    
    # Obtener los breaks y las etiquetas con la nueva función
    breaks_info <- get_breaks(municipios_data[[metrica]])
    
    # Asignar los breaks y las etiquetas a sus respectivas variables
    breaks <- breaks_info$breaks
    labels <- breaks_info$labels
    
    # Generar etiquetas dinámicamente con la unidad
    labels <- paste0(labels, unidad)
    
    # Asignar colores basados en la paleta YlOrRd de RColorBrewer
    colors <- colorRampPalette(c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"))(length(labels))
    
    # Crear nueva columna categorizada
    municipios_data[[paste0(metrica, "_cat")]] <- cut(municipios_data[[metrica]], 
                                                      breaks = breaks, 
                                                      labels = labels, 
                                                      include.lowest = TRUE)
    
    # Filtrar municipios rurales y asignar "Municipios no rurales" en vez de NA
    municipios_data$color_variable <- municipios_data[[paste0(metrica, "_cat")]]
    municipios_data$color_variable[is.na(municipios_data$rural) | municipios_data$rural == 0] <- "Municipios no rurales"
    
    # Convertir a factor para asegurar que "Municipios no rurales" aparezca en la leyenda
    municipios_data$color_variable <- factor(municipios_data$color_variable, levels = c(labels, "Municipios no rurales"))
    
    # Crear mapa
    pl <- ggplot() +
      geom_sf(data = municipios_data, 
              aes(geometry = geometry, 
                  fill = color_variable), 
              color = "grey50", size = 0.1) +  
      scale_fill_manual(
        values = c(setNames(colors, labels), "Municipios no rurales" = "white"), 
        name = ""
      ) +  
      ggtitle(paste(titulo, "(municipios rurales)")) +  
      theme_classic() +
      theme(
        legend.position = c(0.95, 0.05),  
        legend.justification = c(1, 0),  
        legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        axis.title.x = element_blank()
      ) +
      annotate("text", 
               x = Inf, y = -Inf, 
               label = "Nota: Tramos de la leyenda calculados como quintiles de la distribución de la variable", 
               hjust = 1.1, vjust = -1.5, size = 3, 
               color = "black", fontface = "italic")
    
    # Guardar mapa dentro de la carpeta de la familia correspondiente
    ggsave(filename = paste0(path_maps, "/", familia, "/map_", metrica, ".png"),
           plot   = pl, 
           width  = 20, 
           height = 20, 
           units  = 'cm',
           scale  = 2, 
           dpi    = 800)
  }
}



## A) ÁREAS RURALES Y NO RURALES
# Definir directorio principal
path_maps <- paste0(main_path, "/mapas_municipales/todos")
dir.create(path_maps, recursive = TRUE, showWarnings = FALSE)
setwd(path_maps)

# Función para crear subdirectorios por familia si no existen
create_subdir <- function(familia) {
  subdir_path <- file.path(path_maps, familia)
  if (!dir.exists(subdir_path)) {
    dir.create(subdir_path)
  }
}

# Loop para generar los mapas
for (familia in names(familia_mapas)) {
  
  # Crear subdirectorio para la familia de mapas dentro de 'mapas_municipales'
  create_subdir(familia)
  
  for (metrica in familia_mapas[[familia]]) {
    
    # Obtener la unidad y el título de la métrica
    metrica_info <- get_unit_and_title(metrica)
    unidad <- metrica_info$unit
    titulo <- metrica_info$title
    
    # Obtener los breaks y las etiquetas con la nueva función
    breaks_info <- get_breaks(municipios_data[[metrica]])
    
    # Asignar los breaks y las etiquetas a sus respectivas variables
    breaks <- breaks_info$breaks
    labels <- breaks_info$labels
    
    # Generar etiquetas dinámicamente con la unidad
    labels <- paste0(labels, unidad)
    
    # Asignar colores basados en la paleta YlOrRd de RColorBrewer
    colors <- colorRampPalette(c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"))(length(labels))
    
    # Crear nueva columna categorizada
    municipios_data[[paste0(metrica, "_cat")]] <- cut(municipios_data[[metrica]], 
                                                      breaks = breaks, 
                                                      labels = labels, 
                                                      include.lowest = TRUE)
    
    # Crear mapa
    pl <- ggplot() +
      geom_sf(data = municipios_data, 
              aes(geometry = geometry, 
                  fill = .data[[paste0(metrica, "_cat")]])) +  
      scale_fill_manual(values = setNames(colors, labels), 
                        name = "")  +  
      ggtitle(titulo) + 
      theme_classic() +
      theme(
        legend.position = c(0.95, 0.05),  
        legend.justification = c(1, 0),  
        legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        axis.title.x = element_blank()
      ) +
      annotate("text", 
               x = Inf, y = -Inf, 
               label = "Nota: Tramos de la leyenda calculados como quintiles de la distribución de la variable", 
               hjust = 1.1, vjust = -1.5, size = 3, 
               color = "black", fontface = "italic")
    
    # Guardar mapa dentro de la carpeta de la familia correspondiente
    ggsave(filename = paste0(path_maps, "/", familia, "/map_", metrica, ".png"),
           plot   = pl, 
           width  = 20, 
           height = 20, 
           units  = 'cm',
           scale  = 2, 
           dpi    = 800)
  }
}

# ************************************************************
# 6. Crear mapas estáticos provinciales para todas las métricas ----
# ************************************************************

# Definir directorio principal
path_maps <- paste0(main_path, "/mapas_provinciales")
dir.create(path_maps, recursive = TRUE, showWarnings = FALSE)
setwd(path_maps)
  
  # Loop para generar los mapas provinciales
  for (familia in names(familia_mapas)) {
    
    # Crear subdirectorio para la familia de mapas dentro de 'mapas_provinciales'
    create_subdir(familia)
    
    for (metrica in familia_mapas[[familia]]) {
      
      # Obtener la unidad y el título de la métrica
      metrica_info <- get_unit_and_title(metrica)
      unidad <- metrica_info$unit
      titulo <- metrica_info$title
      
      # Obtener los breaks y las etiquetas con la nueva función
      breaks_info <- get_breaks(municipios_data[[metrica]])
      
      # Asignar los breaks y las etiquetas a sus respectivas variables
      breaks <- breaks_info$breaks
      labels <- breaks_info$labels
      
      # Generar etiquetas dinámicamente con la unidad
      labels <- paste0(labels, unidad)
      
      # Asignar colores
      colors <- colorRampPalette(c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"))(length(labels))
      
      # Crear nueva columna categorizada
      provincias_data[[paste0(metrica, "_cat")]] <- cut(provincias_data[[metrica]], 
                                                        breaks = breaks, 
                                                        labels = labels, 
                                                        include.lowest = TRUE)
      
      # Crear mapa
      pl <- ggplot() +
        geom_sf(data = provincias_data, 
                aes(geometry = geometry, 
                    fill = .data[[paste0(metrica, "_cat")]])) +  
        scale_fill_manual(values = setNames(colors, labels), 
                          name = "")  +  
        ggtitle(titulo) + 
        theme_classic() +
        theme(
          legend.position = c(0.95, 0.05),  
          legend.justification = c(1, 0),  
          legend.key.size = unit(0.5, "cm"),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          axis.title.x = element_blank()
        ) +
        annotate("text", 
                 x = Inf, y = -Inf, 
                 label = "Nota: Tramos de la leyenda calculados como quintiles de la distribución de la variable", 
                 hjust = 1.1, vjust = -1.5, size = 3, 
                 color = "black", fontface = "italic")
      
      # Guardar mapa dentro de la carpeta de la familia correspondiente
      ggsave(filename = paste0(path_maps, "/", familia, "/map_", metrica, ".png"),
             plot   = pl, 
             width  = 20, 
             height = 20, 
             units  = 'cm',
             scale  = 2, 
             dpi    = 800)
    }
  }

# ************************************************************
# 7. Crear mapas estáticos autonómicos para todas las métricas ----
# ************************************************************

# Set working domain al principio de nuevo 
setwd(path)

# Check si "mapas_dinamicos" existe, sino crear
if (!dir.exists("mapas_dinamicos")) {
  dir.create("mapas_dinamicos")
}

# Loop para cada variable
for (var in metricas) {
  
  # Loop en niveles administrativos
  for (nivel in c("ccaa", "prov", "muni")) {
    
    # Sacar unidades y título del plot
    unit_and_title <- get_unit_and_title(var)
    unidad         <- unit_and_title$unit
    var_titulo     <- unit_and_title$title
    
    # Seleccionar data y merge con shapefiles según nivel administrativo
    if (nivel == "ccaa") {
      # Cálculo para Comunidad Autónoma
      ccaa_data <- data %>%
        mutate(rural_poblacion = if_else(rural == 1, Población.Total, 0)) %>%
        group_by(Comunidad.Autónoma) %>%
        summarise(
          across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
          rural = (sum(rural_poblacion, na.rm = TRUE) / sum(Población.Total, na.rm = TRUE)) * 100
        )
      
      merge_nivel <- ccaa %>%
        left_join(ccaa_data, by = c("ine.ccaa.name" = "Comunidad.Autónoma"))
      
    } else if (nivel == "prov") {
      # Cálculo para Provincia
      provincias_data <- data %>%
        mutate(rural_poblacion = if_else(rural == 1, Población.Total, 0)) %>%
        group_by(Provincia) %>%
        summarise(
          across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
          rural = (sum(rural_poblacion, na.rm = TRUE) / sum(Población.Total, na.rm = TRUE)) * 100
        )
      
      merge_nivel <- provincias %>%
        left_join(provincias_data, by = c("ine.prov.name" = "Provincia"))
      
    } else {
      # Para municipios
      merge_nivel <- municipios %>%
        left_join(municipios_data, by = c("name" = "Municipio"))
    }
    
    # Sacar los breaks y las etiquetas
    breaks_info <- get_breaks(merge_nivel[[var]], n_bins = 6)
    
    # Asignar los breaks y las etiquetas a sus respectivas variables
    breaks_nivel <- breaks_info$breaks
    labels       <- breaks_info$labels
    
    # Crear paleta de color con los nuevos breaks
    pal <- colorBin(palette = "YlOrRd", 
                    domain  = merge_nivel[[var]], 
                    bins    = breaks_nivel)
    
    # Crear labels manejando NAs
    labels_popup <- if (nivel == "ccaa" | nivel == "prov") {
      # Información del popup para "ccaa" o "prov"
      paste("<strong>", 
            merge_nivel[[if (nivel == "ccaa") "ine.ccaa.name" 
                         else if (nivel == "prov") "ine.prov.name" 
                         else "name"]], 
            "</strong><br/>", 
            var_titulo, ": ",
            ifelse(is.na(merge_nivel[[var]]), "No disponible", 
                   paste0(round(merge_nivel[[var]], 2), unidad)),
            "<br/>Población viviendo en municipios rurales: ",
            ifelse(is.na(merge_nivel$rural), "No disponible", 
                   paste0(round(merge_nivel$rural, 2), "%"))
      ) %>%
        lapply(HTML)
    } else {
      # Información del popup para "muni"
      paste("<strong>", 
            merge_nivel$name, 
            "</strong><br/>", 
            var_titulo, ": ",
            ifelse(is.na(merge_nivel[[var]]), "No disponible", 
                   paste0(round(merge_nivel[[var]], 2), unidad)),
            "<br/>Municipio rural: ",
            ifelse(merge_nivel$rural == 1, "Sí", "No")
      ) %>%
        lapply(HTML)
    }
    
    # Crear el mapa dinámico
    map_nivel <- leaflet(merge_nivel) %>%
      
      # Mapa base
      addProviderTiles("CartoDB.Positron") %>%
      
      # Polígonos cloropéticos
      addPolygons(
        fillColor   = ~pal(merge_nivel[[var]]),
        fillOpacity = 0.7,
        weight      = 0.4,
        opacity     = 1,
        color       = "white",
        dashArray   = "3",
        
        # Popup
        popup       = ~paste("<strong>", 
                             merge_nivel[[if (nivel == "ccaa") "ine.ccaa.name" 
                                          else if (nivel == "prov") "ine.prov.name" 
                                          else "name"]], 
                             "</strong><br>",
                             var_titulo, ": ", 
                             ifelse(is.na(merge_nivel[[var]]), "No disponible", 
                                    round(merge_nivel[[var]], 2)), unidad,
                             if (nivel == "ccaa" | nivel == "prov") {
                               paste("<br>Población viviendo en municipios rurales: ", round(merge_nivel$rural, 2), "%")
                             } else {
                               paste("<br>Municipio rural: ", ifelse(merge_nivel$rural == 1, "Sí", "No"))
                             }),
        
        # Labels
        label       = labels_popup,
        labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                 padding       = "3px 8px"),
                                    textsize  = "15px", 
                                    direction = "auto")
      ) %>%
      
      # Leyenda con las etiquetas generadas por get_breaks()
      addLegend(
        pal      = pal, 
        values   = ~merge_nivel[[var]], 
        opacity  = 0.7, 
        title    = var_titulo, 
        position = "bottomright",
        labels   = labels,  
        labFormat = labelFormat(
          digits = 2,             
          prefix = "",            
          suffix = unidad,            
          big.mark = ","
        )
      ) %>%
      
      # Líneas Islas Canarias (1)
      addPolylines(
        lat = c(36.5, 36.5), 
        lng = c(-12.5, -8),  
        color = "grey", 
        weight = 1,
        dashArray = "2",
        opacity = 1
      ) %>%
      # Líneas Islas Canarias (2)
      addPolylines(
        lat = c(36.5, 35),  
        lng = c(-8, -7),
        color = "grey", 
        weight = 1,
        dashArray = "2",
        opacity = 1
      ) %>%
      
      # Título plot
      addControl(html = paste0("<div style='font-size: 20px; font-weight: bold; color: #333; background-color: white; padding: 5px; border-radius: 7px;'>",
                               var_titulo, "</div>"), 
                 position = "topright") %>%
      
      # Anotación justo debajo de la leyenda
      addControl(html = "<div style='text-align: right; position: relative; bottom: 0;'>
                     <div style='font-size: 12px; color: #555; background-color: white; 
                                 padding: 3px 6px; border-radius: 5px; margin-top: 5px;
                                 display: block; width: max-content;'>
                       <em>Nota: Tramos de la leyenda calculados como quintiles de la distribución de la variable</em>
                     </div>
                   </div>", 
                 position = "bottomright")
    
    
    # Save the map
    saveWidget(map_nivel, 
               file = paste0("mapas_dinamicos/map_", nivel, "_", var, ".html"), 
               selfcontained = TRUE)
  }
}


# ************************************************************
# 8. Crear mapas dinámicos para todos los niveles y métricas ----
# ************************************************************

# Set working domain al principio de nuevo 
setwd(path)

# Check si "mapas_dinamicos" existe, sino crear
if (!dir.exists("mapas_dinamicos")) {
  dir.create("mapas_dinamicos")
}

# Loop para cada variable
for (var in metricas) {
  
  # Loop en niveles administrativos
  for (nivel in c("ccaa", "prov", "muni")) {
    
    # Sacar unidades y título del plot
    unit_and_title <- get_unit_and_title(var)
    unidad         <- unit_and_title$unit
    var_titulo     <- unit_and_title$title
    
    # Seleccionar data y merge con shapefiles según nivel administrativo
    if (nivel == "ccaa") {
      # Cálculo para Comunidad Autónoma
      ccaa_data <- data %>%
        mutate(rural_poblacion = if_else(rural == 1, Población.Total, 0)) %>%
        group_by(Comunidad.Autónoma) %>%
        summarise(
          across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
          rural = (sum(rural_poblacion, na.rm = TRUE) / sum(Población.Total, na.rm = TRUE)) * 100
        )
      
      merge_nivel <- ccaa %>%
        left_join(ccaa_data, by = c("ine.ccaa.name" = "Comunidad.Autónoma"))
      
    } else if (nivel == "prov") {
      # Cálculo para Provincia
      provincias_data <- data %>%
        mutate(rural_poblacion = if_else(rural == 1, Población.Total, 0)) %>%
        group_by(Provincia) %>%
        summarise(
          across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
          rural = (sum(rural_poblacion, na.rm = TRUE) / sum(Población.Total, na.rm = TRUE)) * 100
        )
      
      merge_nivel <- provincias %>%
        left_join(provincias_data, by = c("ine.prov.name" = "Provincia"))
      
    } else {
      # Para municipios
      merge_nivel <- municipios %>%
        left_join(municipios_data, by = c("name" = "Municipio"))
    }
    
    # Sacar los breaks y las etiquetas
    breaks_info <- get_breaks(merge_nivel[[var]], n_bins = 6)
    
    # Asignar los breaks y las etiquetas a sus respectivas variables
    breaks_nivel <- breaks_info$breaks
    labels       <- breaks_info$labels
    
    # Crear paleta de color con los nuevos breaks
    pal <- colorBin(palette = "YlOrRd", 
                    domain  = merge_nivel[[var]], 
                    bins    = breaks_nivel)
    
    # Crear labels manejando NAs
    labels_popup <- if (nivel == "ccaa" | nivel == "prov") {
      # Información del popup para "ccaa" o "prov"
      paste("<strong>", 
            merge_nivel[[if (nivel == "ccaa") "ine.ccaa.name" 
                         else if (nivel == "prov") "ine.prov.name" 
                         else "name"]], 
            "</strong><br/>", 
            var_titulo, ": ",
            ifelse(is.na(merge_nivel[[var]]), "No disponible", 
                   paste0(round(merge_nivel[[var]], 2), unidad)),
            "<br/>Población viviendo en municipios rurales: ",
            ifelse(is.na(merge_nivel$rural), "No disponible", 
                   paste0(round(merge_nivel$rural, 2), "%"))
      ) %>%
        lapply(HTML)
    } else {
      # Información del popup para "muni"
      paste("<strong>", 
            merge_nivel$name, 
            "</strong><br/>", 
            var_titulo, ": ",
            ifelse(is.na(merge_nivel[[var]]), "No disponible", 
                   paste0(round(merge_nivel[[var]], 2), unidad)),
            "<br/>Municipio rural: ",
            ifelse(merge_nivel$rural == 1, "Sí", "No")
      ) %>%
        lapply(HTML)
    }
    
    # Crear el mapa dinámico
    map_nivel <- leaflet(merge_nivel) %>%
      
      # Mapa base
      addProviderTiles("CartoDB.Positron") %>%
      
      # Polígonos cloropéticos
      addPolygons(
        fillColor   = ~pal(merge_nivel[[var]]),
        fillOpacity = 0.7,
        weight      = 0.4,
        opacity     = 1,
        color       = "white",
        dashArray   = "3",
        
        # Popup
        popup       = ~paste("<strong>", 
                             merge_nivel[[if (nivel == "ccaa") "ine.ccaa.name" 
                                          else if (nivel == "prov") "ine.prov.name" 
                                          else "name"]], 
                             "</strong><br>",
                             var_titulo, ": ", 
                             ifelse(is.na(merge_nivel[[var]]), "No disponible", 
                                    round(merge_nivel[[var]], 2)), unidad,
                             if (nivel == "ccaa" | nivel == "prov") {
                               paste("<br>Población viviendo en municipios rurales: ", round(merge_nivel$rural, 2), "%")
                             } else {
                               paste("<br>Municipio rural: ", ifelse(merge_nivel$rural == 1, "Sí", "No"))
                             }),
        
        # Labels
        label       = labels_popup,
        labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                 padding       = "3px 8px"),
                                    textsize  = "15px", 
                                    direction = "auto")
      ) %>%
      
      # Líneas Islas Canarias (1)
      addPolylines(
        lat = c(36.5, 36.5), 
        lng = c(-12.5, -8),  
        color = "grey", 
        weight = 1,
        dashArray = "2",
        opacity = 1
      ) %>%
      # Líneas Islas Canarias (2)
      addPolylines(
        lat = c(36.5, 35),  
        lng = c(-8, -7),
        color = "grey", 
        weight = 1,
        dashArray = "2",
        opacity = 1
      ) %>%
      
      # Título plot
      addControl(html = paste0("<div style='font-size: 20px; font-weight: bold; color: #333; background-color: white; padding: 5px; border-radius: 7px;'>",
                               var_titulo, "</div>"), 
                 position = "topright") %>%
      
      # Anotación
      addControl(html = "<div style='font-size: 12px; color: #555; background-color: white; padding: 5px; 
                     border-radius: 5px; margin-top: 5px;'>  <!-- Reducido de 40px a 5px -->
           <em>Nota: Tramos de la leyenda calculados como quintiles de la distribución de la variable</em>
           </div>", 
                 position = "bottomright") %>%
      
      # Leyenda con las etiquetas generadas por get_breaks()
      addLegend(
        pal      = pal, 
        values   = ~merge_nivel[[var]], 
        opacity  = 0.7, 
        title    = var_titulo, 
        position = "bottomright",
        labels   = labels,  
        labFormat = labelFormat(
          digits = 2,             
          prefix = "",            
          suffix = unidad,            
          big.mark = ","
        )
      ) 
    
    # Save the map
    saveWidget(map_nivel, 
               file = paste0("mapas_dinamicos/map_", nivel, "_", var, ".html"), 
               selfcontained = TRUE)
  }
}


#### End time
end_time       <- Sys.time()
execution_time <- end_time - start_time
cat("Tiempo de ejecución:", execution_time, "\n")

