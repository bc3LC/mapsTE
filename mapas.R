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
packages_needed <- c ( "dplyr"      ,
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
                       "mapSpain")

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

### Map 1: Censo de conductores per cápita

# Create municipality data
muni <- esp_get_munic()

# Create a variable to merge
data <- data %>%
  mutate(LAU_CODE = Código.INE)

# Merge dataframes
data_muni <- merge(data, muni, all = TRUE)

# Create map: municipalities
esp_get_munic() %>% ggplot() + geom_sf() + theme_minimal()

# Define breaks and corresponding colors
breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 100)

labels <- c("<0.2"   , 
            "0.2-0.4", 
            "0.4-0.6", 
            "0.6-0.8",
            ">0.8"   )

colors <- c("<0.2"    = "#ccf4f4", 
            "0.2-0.4" = "#6fe2e2",
            "0.4-0.6" = "#2bd2d2",
            "0.6-0.8" = "#22a8a8", 
            ">0.8"    = "#104f4f")

data_muni$censo_pca <- cut(data_muni$censo_pc   , 
                           breaks = breaks      , 
                           labels = labels      , 
                           include.lowest = TRUE)

# Create map
pl <- ggplot() +
  geom_sf(data = data_muni, aes(geometry= geometry , fill = censo_pca)) +    # color map by regions
  scale_fill_manual(values = colors, name = "Censo de conductores per cápita") +  # set colors and breaks
  theme_classic() +
  theme(legend.position = "bottom")

# Define path
path_o <- paste0(path,"/mapas_censo")
setwd(path_o)

# Save map
ggsave(filename = "map_censo_pc.png",
       plot = pl, width = 20, height = 20, units = 'cm',
       scale = 2, dpi = 800)



### Map 2: Parque total per cápita

# Define breaks and corresponding colors
breaks <- c(0, 0.75, 1.5, 2.25, 3, 100)

labels <- c("<0.75"   , 
            "0.75-1.5", 
            "1.5-2.25", 
            "2.25-3"  ,
            ">3"      )

colors <- c("<0.75"    = "#ccf4f4", 
            "0.75-1.5" = "#6fe2e2",
            "1.5-2.25" = "#2bd2d2",
            "2.25-3"   = "#22a8a8", 
            ">3"       = "#104f4f")

data_muni$parque_pca <- cut(data_muni$parque_pc   , 
                           breaks = breaks      , 
                           labels = labels      , 
                           include.lowest = TRUE)

# Create map 
pl <- ggplot() +
  geom_sf(data = data_muni, aes(geometry= geometry , fill = parque_pca)) +    # color map by regions
  scale_fill_manual(values = colors, name = "Parque total de vehículos per cápita") +  # set colors and breaks
  theme_classic() +
  theme(legend.position = "bottom")

# Define path
path_o <- paste0(path,"/mapas_parque")
setwd(path_o)

# Save map
ggsave(filename = "map_parque_pc.png",
       plot = pl, width = 20, height = 20, units = 'cm',
       scale = 2, dpi = 800)



### Map 3: Parque per cápita por tipología

# Define "tipoligias"
parque_tipologias <- c("parque_ciclomotores_pc",
                       "parque_motocicletas_pc",
                       "parque_turismos_pc"    ,
                       "parque_furgonetas_pc"  ,
                       "parque_camiones_pc")

# Define breaks and corresponding colors
breaks <- c(0, 0.5, 1, 1.5, 2, 100)

labels <- c("<0.5"  , 
            "0.5-1" , 
            "1-1.5" , 
            "1.5-2" ,
            ">2"    )

colors <- c("<0.5"  = "#ccf4f4", 
            "0.5-1" = "#6fe2e2",
            "1-1.5" = "#2bd2d2",
            "1.5-2" = "#22a8a8", 
            ">2"    = "#104f4f")


# Loop to create maps for different categories
for (t in parque_tipologias) {
  
  # Create legend breaks
  data_muni[[paste0(t, "a")]] <- cut(data_muni[[t]], 
                                    breaks = breaks      , 
                                    labels = labels      , 
                                    include.lowest = TRUE)
 
   # define tipology
  tip <- strsplit(t, "_")[[1]][2]  
  
  # Create map and save
  pl <- ggplot() +
    geom_sf(data = data_muni, aes(geometry= geometry , fill = .data[[paste0(t, "a")]])) +    # color map by regions
    scale_fill_manual(values = colors, name = paste0("Parque de ", tip, " per cápita")) +  # set colors and breaks
    theme_classic() +
    theme(legend.position = "bottom")
  
  ggsave(filename = paste0("map_parque_",tip,"_pc.png"),
         plot = pl, width = 20, height = 20, units = 'cm',
         scale = 2, dpi = 800)
  
}



### Map 4: Antiguedad media del parque

# Define breaks and corresponding colors
breaks <- c(0, 5, 10, 15, 20, 100)

labels <- c("<5"       , 
            "5-10"     , 
            "10-15"    , 
            "15-20"    ,
            ">20"      )

colors <- c("<5"    = "#ccf4f4", 
            "5-10"  = "#6fe2e2",
            "10-15" = "#2bd2d2",
            "15-20" = "#22a8a8", 
            ">20"   = "#104f4f")


data_muni$antiguedad_total <- cut(data_muni$antiguedad_tot , 
                                  breaks = breaks          , 
                                  labels = labels          , 
                                  include.lowest = TRUE    )

# Create map 
pl <- ggplot() +
  geom_sf(data = data_muni, aes(geometry= geometry , fill = antiguedad_total)) +    # color map by regions
  scale_fill_manual(values = colors, name = "Antigüedad media del parque") +  # set colors and breaks
  theme_classic() +
  theme(legend.position = "bottom")

# Define path
path_o <- paste0(path,"/mapas_antiguedad")
setwd(path_o)

# Save map
ggsave(filename = "map_antiguedad.png",
       plot = pl, width = 20, height = 20, units = 'cm',
       scale = 2, dpi = 800)



### Map 5: Antiguedad media del parque por tipología

# Define "tipoligias"
antiguedad_tipologias <- c("antiguedad_ciclomotores",
                           "antiguedad_motocicletas",
                           "antiguedad_turismos"    ,
                           "antiguedad_furgonetas"  ,
                           "antiguedad_camiones"    )

# Define breaks and corresponding colors
breaks <- c(0, 5, 10, 15, 20, 100)

labels <- c("<5"       , 
            "5-10"     , 
            "10-15"    , 
            "15-20"    ,
            ">20"      )

colors <- c("<5"    = "#ccf4f4", 
            "5-10"  = "#6fe2e2",
            "10-15" = "#2bd2d2",
            "15-20" = "#22a8a8", 
            ">20"   = "#104f4f")

# Loop to create maps for different categories
for (t in antiguedad_tipologias) {
  
  # Create legend breaks
  data_muni[[paste0(t, "a")]] <- cut(data_muni[[t]], 
                                     breaks = breaks      , 
                                     labels = labels      , 
                                     include.lowest = TRUE)
  
  # define tipology
  tip <- strsplit(t, "_")[[1]][2]  
  
  # Create map and save
  pl <- ggplot() +
    geom_sf(data = data_muni, aes(geometry= geometry , fill = .data[[paste0(t, "a")]])) +    # color map by regions
    scale_fill_manual(values = colors, name = paste0("Antigüedad media de ", tip)) +  # set colors and breaks
    theme_classic() +
    theme(legend.position = "bottom")
  
  ggsave(filename = paste0("map_antiguedad_",tip,".png"),
         plot = pl, width = 20, height = 20, units = 'cm',
         scale = 2, dpi = 800)
  
}



### Map 6: % de vehículos por etiquetas

# Define "tipos de distintivos"
tipo_distintivo <- c("distintivo_B"   ,
                     "distintivo_C"   ,
                     "distintivo_ECO" ,
                     "distintivo_0"   ,
                     "sin_distintivo" )

# Define breaks and corresponding colors
breaks <- c(0, 20, 40, 60, 80, 100)

labels <- c("<20"      , 
            "20-40"    , 
            "40-60"    , 
            "60-80"    ,
            ">80"      )

colors <- c("<20"   = "#ccf4f4", 
            "20-40" = "#6fe2e2",
            "40-60" = "#2bd2d2",
            "60-80" = "#22a8a8", 
            ">80"   = "#104f4f")

# Loop to create maps for different categories
for (t in tipo_distintivo) {
  
  # Create legend breaks
  data_muni[[paste0(t, "a")]] <- cut(data_muni[[t]], 
                                     breaks = breaks      , 
                                     labels = labels      , 
                                     include.lowest = TRUE)
  
  # Create map
  pl <- ggplot() +
    geom_sf(data = data_muni, aes(geometry= geometry , fill = .data[[paste0(t, "a")]])) +    # color map by regions
    scale_fill_manual(values = colors, name = paste0("% de vehículos con ", t)) +  # set colors and breaks
    theme_classic() +
    theme(legend.position = "bottom")
  
  # Define path
  path_o <- paste0(path,"/mapas_etiquetas")
  setwd(path_o)
  
  # Save map
  ggsave(filename = paste0("map_",t,".png"),
         plot = pl, width = 20, height = 20, units = 'cm',
         scale = 2, dpi = 800)
  
}


###############################################################################
rm(list = setdiff(ls(), c("data", "path"))) # Clean environment
gc()                                        # Free unused local memory
###############################################################################

# ************************************************************
# 3. Create dynamic maps
# ************************************************************

# [1] PREPARE SPATIAL FILES & MERGE THEM WITH "data" ----

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


