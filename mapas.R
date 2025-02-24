###############################################################################

# Title            : Mapas municipios España
# Project          : T&E
# Date of creation : 03/02/2025
# Last update      : 19/02/2025

# Author 1         : Eva Alonso Epelde (eva.alonso@bc3research.org)
# Author 2         : Mercè Amich Vidal (merce.amich@ehu.eus)
# Institution      : Basque Centre for Climate Change (BC3)

###############################################################################

# ************************************************************
# 1. Preliminares y definición de parametros
# ************************************************************

# Clean environment
rm(list = ls(all = TRUE))    

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
                     "scales"
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


###############################################################################

# ************************************************************
# 2. Load data & create new variables
# ************************************************************

# Load transport data
data <- read.xlsx("datos_transporte.xlsx")

# Create new variables
data <- data %>% 
  mutate(censo_pc                = Censo.Conductores/Población.Total       ,
         parque_pc               = Parque.Total/Población.Total            ,
         parque_ciclomotores_pc  = Parque.Ciclomotores/Población.Total     ,
         parque_motocicletas_pc  = Parque.Motocicletas/Población.Total     ,
         parque_turismos_pc      = Parque.Turismos/Población.Total         ,
         parque_furgonetas_pc    = Parque.Furgonetas/Población.Total       ,
         parque_camiones_pc      = Parque.Camiones/Población.Total         ,
         antiguedad_tot          = `Antigüedad.Media.del.Parque.(<25.años)`,
         antiguedad_ciclomotores = Antigüedad.Media.de.Ciclomotores        ,
         antiguedad_motocicletas = Antigüedad.Media.de.Motocicletas        ,
         antiguedad_turismos     = Antigüedad.Media.de.Turismos            ,
         antiguedad_furgonetas   = Antigüedad.Media.de.Furgonetas          ,
         antiguedad_camiones     = Antigüedad.Media.de.Camiones            ,
         distintivo_B            = (Distintivo.B/Parque.Total)*100         ,
         distintivo_C            = (Distintivo.C/Parque.Total)*100         ,
         distintivo_ECO          = (Distintivo.ECO/Parque.Total)*100       ,
         distintivo_0            = (Distintivo.0/Parque.Total)*100         ,
         sin_distintivo          = (Sin.Distintivo/Parque.Total)*100       )


###############################################################################

# ************************************************************
# 3. Create static maps
# ************************************************************

### Preliminar step: create municipality data
muni <- esp_get_munic()

# Create a variable to merge 
data <- data %>%
  mutate(LAU_CODE = Código.INE)

# Merge dataframes
data_muni <- merge(data, muni, all = TRUE)

# ************************************************************

### Map 1: Censo de conductores per cápita

# Create map: municipalities
esp_get_munic() %>% ggplot() + geom_sf() + theme_minimal()

# Define breaks and colors
breaks <- c(0, 0.3, 0.5, 0.6, 0.7, max(data$censo_pc, na.rm = TRUE)) 

labels <- c("<0.3"   , 
            "0.3-0.5", 
            "0.5-0.6", 
            "0.6-0.7",
            ">0.7"   )

colors <- c("<0.3"    = "#ccf4f4", 
            "0.3-0.5" = "#6fe2e2",
            "0.5-0.6" = "#2bd2d2",
            "0.6-0.7" = "#22a8a8", 
            ">0.7"    = "#104f4f")

# Apply
data_muni$censo_pca <- cut(data_muni$censo_pc, 
                           breaks = breaks, 
                           labels = labels, 
                           include.lowest = TRUE)


# Create map
pl <- ggplot() +
  geom_sf(data = data_muni, 
          aes(geometry = geometry , 
              fill     = censo_pca)) +
  scale_fill_manual(values = colors, 
                    name   = "Censo de conductores per cápita") + 
  theme_classic() +
  theme(legend.position = "bottom")

# Define path
path_o <- paste0(path,"/mapas_censo")
setwd(path_o)

# Save map
ggsave(filename = "map_censo_pc.png",
       plot   = pl, 
       width  = 20, 
       height = 20, 
       units  = 'cm',
       scale  = 2, 
       dpi    = 800)



### Map 2: Parque total per cápita

# Adjusted breaks
breaks <- c(0, 0.75, 1, 1.25, 1.5, 3) 

labels <- c("<0.75"   , 
            "0.75-1"  , 
            "1-1.25"  , 
            "1.25-1.5",
            ">1.5"    )

# Maintain the color scheme
colors <- c("<0.75"   = "#ccf4f4", 
            "0.75-1"  = "#6fe2e2",
            "1-1.25"  = "#2bd2d2",
            "1.25-1.5"= "#22a8a8", 
            ">1.5"    = "#104f4f")

# Apply cut
data_muni$parque_pca <- cut(data_muni$parque_pc   , 
                            breaks = breaks      , 
                            labels = labels      , 
                            include.lowest = TRUE)

# Create map 
pl <- ggplot() +
  geom_sf(data = data_muni, 
          aes(geometry = geometry , 
              fill     = parque_pca)) +
  scale_fill_manual(values = colors, 
                    name   = "Parque total de vehículos per cápita") +
  theme_classic() +
  theme(legend.position = "bottom")

# Define path
path_o <- paste0(path,"/mapas_parque")
setwd(path_o)

# Save map
ggsave(filename = "map_parque_pc.png",
       plot   = pl, 
       width  = 20, 
       height = 20, 
       units  = 'cm',
       scale  = 2, 
       dpi    = 800)



### Map 3: Parque per cápita por tipología

# Define "tipologias"
parque_tipologias <- c("parque_ciclomotores_pc",
                       "parque_motocicletas_pc",
                       "parque_turismos_pc",
                       "parque_furgonetas_pc",
                       "parque_camiones_pc")

# Lista de breaks personalizados por categoría
breaks_list <- list(
  "parque_ciclomotores_pc"    = c(0, 0.03, 0.05, 0.1, 0.5, 2),
  "parque_motocicletas_pc"    = c(0, 0.05, 0.1,  0.5, 1,   6),
  "parque_turismos_pc"        = c(0, 0.1,  0.3,  0.6, 1,   81.5778),
  "parque_furgonetas_pc"      = c(0, 0.05, 0.1,  0.2, 0.5, 16),
  "parque_camiones_pc"        = c(0, 0.05, 0.1,  0.2, 0.5, 10)
)

# Colores para la leyenda
colors <- c("#ccf4f4", 
            "#6fe2e2", 
            "#2bd2d2", 
            "#22a8a8", 
            "#104f4f")

# Loop para crear los mapas para las distintas categorías
for (t in parque_tipologias) {
  
  # Extraer los cortes para la variable
  breaks <- breaks_list[[t]]
  
  # Generar etiquetas dinámicamente a partir de los breaks
  labels <- paste0("[", head(breaks, -1), "-", tail(breaks, -1), "]")  
  
  # Crear nueva columna
  data_muni[[paste0(t, "a")]] <- cut(data_muni[[t]], 
                                     breaks = breaks, 
                                     labels = labels, 
                                     include.lowest = TRUE)
  
  # Nombre de la categoría
  tip <- strsplit(t, "_")[[1]][2]  
  
  # Crear mapa
  pl <- ggplot() +
    geom_sf(data = data_muni, 
            aes(geometry = geometry , 
                fill = .data[[paste0(t, "a")]])) +  
    scale_fill_manual(values = setNames(colors, labels), 
                      name = paste0("Parque de ", tip, " per cápita")) +  
    theme_classic() +
    theme(legend.position = "bottom")
  
  # Guardar mapa
  ggsave(filename = paste0("map_parque_", tip, "_pc.png"),
         plot   = pl, 
         width  = 20, 
         height = 20, 
         units  = 'cm',
         scale  = 2, 
         dpi    = 800)
}


### Map 4: Antiguedad media del parque

# Define breaks and corresponding colors
breaks <- c(0, 12, 14, 15, 16, 25)

labels <- c("<12"   , 
            "12-14" , 
            "14-15" , 
            "15-16" ,
            ">16"   )

colors <- c("<12"   = "#ccf4f4", 
            "12-14" = "#6fe2e2",
            "14-15" = "#2bd2d2",
            "15-16" = "#22a8a8", 
            ">16"   = "#104f4f")

data_muni$antiguedad_total <- cut(data_muni$antiguedad_tot , 
                                  breaks = breaks          , 
                                  labels = labels          , 
                                  include.lowest = TRUE    )

# Create map 
pl <- ggplot() +
  geom_sf(data         = data_muni, 
          aes(geometry = geometry , 
              fill     = antiguedad_total)) +    
  scale_fill_manual(values = colors, 
                    name   = "Antigüedad media del parque") +
  theme_classic() +
  theme(legend.position = "bottom")

# Define path
path_o <- paste0(path,"/mapas_antiguedad")
setwd(path_o)

# Save map
ggsave(filename = "map_antiguedad.png",
       plot   = pl, 
       width  = 20, 
       height = 20, 
       units  = 'cm',
       scale  = 2, 
       dpi    = 800)



### Map 5: Antiguedad media del parque por tipología

# Define "tipologias"
antiguedad_tipologias <- c("antiguedad_ciclomotores",
                           "antiguedad_motocicletas",
                           "antiguedad_turismos",
                           "antiguedad_furgonetas",
                           "antiguedad_camiones")

# Lista de breaks personalizados por categoría
breaks_list <- list(
  "antiguedad_ciclomotores"    = c(0, 5, 10, 15, 20, 25),
  "antiguedad_motocicletas"    = c(0, 5, 10, 15, 20, 25),
  "antiguedad_turismos"       = c(0, 5, 10, 15, 20, 25),
  "antiguedad_furgonetas"     = c(0, 5, 10, 15, 20, 25),
  "antiguedad_camiones"       = c(0, 5, 10, 15, 20, 25)
)

# Colores para la leyenda
colors <- c("#ccf4f4", 
            "#6fe2e2", 
            "#2bd2d2", 
            "#22a8a8", 
            "#104f4f")

# Loop para crear los mapas para las distintas categorías
for (t in antiguedad_tipologias) {
  
  # Extraer los cortes para la variable
  breaks <- breaks_list[[t]]
  
  # Generar etiquetas dinámicamente a partir de los breaks
  labels <- paste0("[", head(breaks, -1), "-", tail(breaks, -1), "]")  
  
  # Crear nueva columna
  data_muni[[paste0(t, "a")]] <- cut(data_muni[[t]], 
                                     breaks = breaks, 
                                     labels = labels, 
                                     include.lowest = TRUE)
  
  # Nombre de la categoría
  tip <- strsplit(t, "_")[[1]][2]  
  
  # Crear mapa
  pl <- ggplot() +
    geom_sf(data = data_muni, 
            aes(geometry = geometry , 
                fill = .data[[paste0(t, "a")]])) +  
    scale_fill_manual(values = setNames(colors, labels), 
                      name = paste0("Antigüedad media de ", tip)) +  
    theme_classic() +
    theme(legend.position = "bottom")
  
  # Guardar mapa
  ggsave(filename = paste0("map_antiguedad_", tip, ".png"),
         plot   = pl, 
         width  = 20, 
         height = 20, 
         units  = 'cm',
         scale  = 2, 
         dpi    = 800)
}



### Map 6: % de vehículos por etiquetas

# Define "tipos de distintivos"
tipo_distintivo <- c("distintivo_B",
                     "distintivo_C",
                     "distintivo_ECO",
                     "distintivo_0",
                     "sin_distintivo")

# Lista de breaks personalizados
breaks_list <- list(
  "distintivo_B"   = c(0, 10, 20, 30, 40, 50, 75),
  "distintivo_C"   = c(0, 10, 20, 30, 40, 50, 86),
  "distintivo_ECO" = c(0, 1, 2, 3, 4, 33),        
  "distintivo_0"   = c(0, 0.5, 1, 2, 5, 15),      
  "sin_distintivo" = c(0, 10, 20, 30, 50, 100)    
)

# Colores que se usarán en la leyenda
colors_list <- list(
  "distintivo_B"   = c("#ccf4f4", "#6fe2e2", "#2bd2d2", "#22a8a8", "#104f4f", "#004949"),
  "distintivo_C"   = c("#ccf4f4", "#6fe2e2", "#2bd2d2", "#22a8a8", "#104f4f", "#004949"),
  "distintivo_ECO" = c("#ccf4f4", "#6fe2e2", "#2bd2d2", "#22a8a8", "#104f4f"),
  "distintivo_0"   = c("#ccf4f4", "#6fe2e2", "#2bd2d2", "#22a8a8", "#104f4f"),
  "sin_distintivo" = c("#ccf4f4", "#6fe2e2", "#2bd2d2", "#22a8a8", "#104f4f")
)

# Loop para generar los mapas
for (t in tipo_distintivo) {
  
  # Extraer los cortes para la variable
  breaks <- breaks_list[[t]]
  
  # Generar etiquetas dinámicamente a partir de los breaks
  labels <- paste0("[", head(breaks, -1), "-", tail(breaks, -1), "]")  
  
  # Crear nueva columna para la variable categorizada
  data_muni[[paste0(t, "a")]] <- cut(data_muni[[t]], 
                                     breaks = breaks, 
                                     labels = labels, 
                                     include.lowest = TRUE)
  
  # Nombre de la categoría (tipo de distintivo)
  tip <- strsplit(t, "_")[[1]][2]  
  
  # Extraer los colores correspondientes para este tipo de distintivo
  colors <- colors_list[[t]]
  
  # Crear mapa con ggplot
  pl <- ggplot() +
    geom_sf(data         = data_muni, 
            aes(geometry = geometry , 
                fill     = .data[[paste0(t, "a")]])) +  
    scale_fill_manual(values = setNames(colors, labels), 
                      name   = paste0("% de vehículos con ", tip)) +  
    theme_classic() +
    theme(legend.position = "bottom")
  
  # Definir la ruta
  path_o <- paste0(path, "/mapas_etiquetas")
  setwd(path_o)
  
  # Guardar mapa
  ggsave(filename = paste0("map_", t, ".png"),
         plot   = pl, 
         width  = 20, 
         height = 20, 
         units  = 'cm',
         scale  = 2, 
         dpi    = 800)
}


###############################################################################

rm(list = setdiff(ls(), c("data", "path"))) # Clean environment
gc()                                        # Free unused local memory

###############################################################################

# ************************************************************
# 3. Create dynamic maps
# ************************************************************

# [1] Prepare spatial files $ merge them with "data"  ----

# [a] Download shapefiles via "mapSpain" ----

ccaa       <- esp_get_ccaa(epsg = "4326")
provincias <- esp_get_prov(epsg = "4326")
municipios <- esp_get_munic()

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
diff1 <- setdiff(unique(data$LAU_CODE), unique(municipios$LAU_CODE))
diff2 <- setdiff(unique(municipios$LAU_CODE), unique(data$LAU_CODE))

diff1 # Códigos en data$LAU_CODE que no están en municipios$LAU_CODE
diff2 # Códigos en municipios$LAU_CODE que no están en data$LAU_CODE

# diff1 devuelve los códigos "01000" y "36012" como distintos. 
# diff2 devuelve el código "48916" como distinto

# Relativo a diff1, los inspeccionamos:
data$Municipio[data$LAU_CODE == "01000"] # "Alava ( municipio sin especificar)"
data$Municipio[data$LAU_CODE == "36012"] # "Cerdedo-Cotobade"

# En el primer caso, eliminamos la observación
# Serán vehículos sin municipio especificado en el sistema central DGT (vía @Oscar Pulido T&E 14.02.25)

data <- subset(data, LAU_CODE != "01000")

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
# a) Nombres franquistas en "municipios" (GISCO Eurostat)
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
  filter(LAU_CODE %in% c(36012, 36902)) %>%
  group_by(Municipio) %>%
  summarise(
    LAU_CODE                     = as.numeric(36902),
    Municipio                    = "Cerdedo-Cotobade",
    Provincia                    = first(Provincia),
    Comunidad.Autónoma           = first(Comunidad.Autónoma),
    `ES.RURAL?`                  = first(`ES.RURAL?`),                         
    Población.Total              = sum(Población.Total, na.rm = TRUE),
    Población.Hombres            = sum(Población.Hombres, na.rm = TRUE),
    Población.Mujeres            = sum(Población.Mujeres, na.rm = TRUE),
    Censo.Conductores            = sum(Censo.Conductores, na.rm = TRUE),
    parque_pc                    = sum(parque_pc, na.rm = TRUE),
    parque_ciclomotores_pc       = sum(parque_ciclomotores_pc, na.rm = TRUE),
    parque_motocicletas_pc       = sum(parque_motocicletas_pc, na.rm = TRUE),
    parque_turismos_pc           = sum(parque_turismos_pc, na.rm = TRUE),
    parque_furgonetas_pc         = sum(parque_furgonetas_pc, na.rm = TRUE),
    parque_camiones_pc           = sum(parque_camiones_pc, na.rm = TRUE),
    antiguedad_tot               = mean(antiguedad_tot, na.rm = TRUE),
    antiguedad_ciclomotores      = mean(antiguedad_ciclomotores, na.rm = TRUE),
    antiguedad_motocicletas      = mean(antiguedad_motocicletas, na.rm = TRUE),
    antiguedad_turismos          = mean(antiguedad_turismos, na.rm = TRUE),
    antiguedad_furgonetas        = mean(antiguedad_furgonetas, na.rm = TRUE),
    antiguedad_camiones          = mean(antiguedad_camiones, na.rm = TRUE),
    distintivo_B                 = sum(distintivo_B, na.rm = TRUE),
    distintivo_C                 = sum(distintivo_C, na.rm = TRUE),
    distintivo_ECO               = sum(distintivo_ECO, na.rm = TRUE),
    distintivo_0                 = sum(distintivo_0, na.rm = TRUE),
    sin_distintivo               = sum(sin_distintivo, na.rm = TRUE),
    Total.Campañas               = sum(Total.Campañas, na.rm = TRUE)
  )

# Ensure "Código INE" and "LAU_CODE" are numeric
data$LAU_CODE       <- as.numeric(data$LAU_CODE)
municipios$LAU_CODE <- as.numeric(municipios$LAU_CODE)

# Remove both rows with Código INE 36012 and 36902
data <- data %>%
  filter(!LAU_CODE %in% c(36012, 36902))

# Add the new row (data_fusionada)
data <- data %>%
  bind_rows(data_fusionada)
remove("data_fusionada")

# [c] Crear métricas autonómicas y provinciales ----

## Autonómicas
ccaa_data <- data %>%
  group_by(Comunidad.Autónoma) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

## Provinciales

provincias_data <- data %>%
  group_by(Provincia) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

## Rename "data" (ensure consistency with 3 admin. levels)
municipios_data <- data
remove("data")

###############################################################################

# Set working domain at the start again 
setwd(path)

# Lista de variables
variables <- c("censo_pc", "parque_pc", "parque_ciclomotores_pc", "parque_motocicletas_pc", 
               "parque_turismos_pc", "parque_furgonetas_pc", "parque_camiones_pc", 
               "antiguedad_tot", "antiguedad_ciclomotores", "antiguedad_motocicletas",
               "antiguedad_turismos", "antiguedad_furgonetas", "antiguedad_camiones",
               "distintivo_B", "distintivo_C", "distintivo_ECO", "sin_distintivo", "distintivo_0"
)

# Plot titles
titulos <- c(
  "censo_pc"                     = "Vehículos censados por cápita",
  "parque_pc"                    = "Parque de vehículos por cápita",
  "parque_ciclomotores_pc"       = "Parque de ciclomotores por cápita",
  "parque_motocicletas_pc"       = "Parque de motocicletas por cápita",
  "parque_turismos_pc"           = "Parque de turismos por cápita",
  "parque_furgonetas_pc"         = "Parque de furgonetas por cápita",
  "parque_camiones_pc"           = "Parque de camiones por cápita",
  "antiguedad_tot"               = "Antigüedad media del parque total",
  "antiguedad_ciclomotores"      = "Antigüedad media del parque de ciclomotores",
  "antiguedad_motocicletas"      = "Antigüedad media del parque de motocicletas",
  "antiguedad_turismos"          = "Antigüedad media del parque de turismos",
  "antiguedad_furgonetas"        = "Antigüedad media del parque de furgonetas",
  "antiguedad_camiones"          = "Antigüedad media del parque de camiones",
  "distintivo_B"                 = "Prevalencia de vehículos con Distintivo B",
  "distintivo_C"                 = "Prevalencia de vehículos con Distintivo C",
  "distintivo_ECO"               = "Prevalencia de vehículos con Distintivo ECO",
  "sin_distintivo"               = "Prevalencia de vehículos sin distintivo",
  "distintivo_0"                 = "Prevalencia de vehículos con Distintivo 0"
)

# Función para obtener los breaks con comprobación (to avoid unique breaks)
get_breaks <- function(data, n_bins = 6) {
  
  # Calcular los percentiles
  breaks <- quantile(data, probs = seq(0, 1, length.out = n_bins), na.rm = TRUE)
  
  # Si los breaks no son únicos, usar 'pretty' para generar los intervalos
  if (length(unique(breaks)) != length(breaks)) {
    breaks <- pretty(data, n_bins)
  }
  
  return(breaks)
}

# Check si "mapas_dinamicos" existe, if not create it
if (!dir.exists("mapas_dinamicos")) {
  dir.create("mapas_dinamicos")
}

# Loop para cada variable
for (var in variables) {
  
  # Loop por niveles administrativos
  for (nivel in c("ccaa", "prov", "muni")) {
    
    # Determinar si la variable es porcentaje o años
    unidad <- ifelse(grepl("antiguedad", var), " años", 
                     ifelse(grepl("censo", var), "", " %"))
    
    # Formatear título
    var_titulo <- gsub("_", " ", var)
    var_titulo <- paste0(toupper(substr(var_titulo, 1, 1)), substr(var_titulo, 2, nchar(var_titulo)))
    
    # Seleccionar y agregar datos según el nivel
    if (nivel == "ccaa") {
      data_nivel <- municipios_data %>%
        group_by(Comunidad.Autónoma) %>%
        summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
      
      merge_nivel <- ccaa %>% left_join(data_nivel, 
                                        by = c("ine.ccaa.name" = "Comunidad.Autónoma"))
      
    } else if (nivel == "prov") {
      data_nivel <- municipios_data %>%
        group_by(Provincia) %>%
        summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
      
      merge_nivel <- provincias %>% left_join(data_nivel, 
                                              by = c("ine.prov.name" = "Provincia"))
    } else {
      merge_nivel <- municipios %>% left_join(municipios_data, 
                                              by = c("name" = "Municipio"))
    }
    
    # Obtener los breaks
    breaks_nivel <- get_breaks(merge_nivel[[var]], n_bins = 6)
    
    # Crear paleta de colores
    pal <- colorBin(palette = "YlOrRd", 
                    domain  = merge_nivel[[var]], 
                    bins    = breaks_nivel)
    
    # Crear etiquetas con manejo de NA
    labels <- sprintf("<strong>%s</strong><br/>%s: %s%s", 
                      merge_nivel[[if (nivel == "ccaa") "ine.ccaa.name" else if (nivel == "prov") "ine.prov.name" else "name"]],
                      titulos[[var]], 
                      ifelse(is.na(merge_nivel[[var]]), "No disponible", round(merge_nivel[[var]], 2)), unidad) %>%
      lapply(HTML)
    
    # Crear el mapa
    map_nivel <- leaflet(merge_nivel) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor   = ~pal(merge_nivel[[var]]),
        fillOpacity = 0.7,
        weight      = 1,
        opacity     = 1,
        color       = "white",
        dashArray   = "3",
        
        popup       = ~paste("<strong>", 
                             merge_nivel[[if (nivel == "ccaa") "ine.ccaa.name" else if (nivel == "prov") "ine.prov.name" else "name"]], 
                             "</strong><br>",
                             titulos[[var]], ": ", 
                             ifelse(is.na(merge_nivel[[var]]), "No disponible", round(merge_nivel[[var]], 2)), unidad),
        
        highlight   = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.7, bringToFront = TRUE),
        
        label       = labels,
        labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                 padding       = "3px 8px"),
                                    textsize  = "15px", 
                                    direction = "auto")
      ) %>%
      addLegend(pal      = pal, 
                values   = ~merge_nivel[[var]], 
                opacity  = 0.7, 
                title    = titulos[[var]], 
                position = "bottomright") %>%
      
      # Añadir líneas de separación entre la península y las Islas Canarias
      ## Línea horizontal 
      addPolylines(
        lat = c(36.5, 36.5), 
        lng = c(-12.5, -8),  
        color = "grey", 
        weight = 1,
        dashArray = "2",
        opacity = 1
      ) %>%
      ## Línea diagonal
      addPolylines(
        lat = c(36.5, 35),  
        lng = c(-8, -7),
        color = "grey", 
        weight = 1,
        dashArray = "2",
        opacity = 1
      ) %>%
      
      addControl(html = paste0("<div style='font-size: 20px; font-weight: bold; color: #333; background-color: white; padding: 5px; border-radius: 7px;'>",
                               titulos[[var]], "</div>"), position = "topright")
    
    # Guardar el mapa
    saveWidget(map_nivel, file = paste0("mapas_dinamicos/map_", nivel, "_", var, ".html"), selfcontained = TRUE)
  }
}

