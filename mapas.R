###############################################################################

# Title            : Mapas municipios España
# Project          : T&E
# Date of creation : 03/02/2025

# Author           : Eva Alonso Epelde (eva.alonso@bc3research.org)
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
# 3. Create maps
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
