
## grafico de la región amazonica 

# carpeta de trabajo
setwd("C:/Users/Juan Lastra/Desktop/Programación/R/Proyectos/Proyectos de investigación/Internet colombia")


## paquetes necesarios

library(tidyverse)
library(geojsonio)
library(ggthemes)
library(sf)
library(ggspatial)

## cargamos el archivo gjson con sf para facilitar los filtros

colombia <- read_sf("co_2018_MGN_MPIO_POLITICO.geojson")
  

## cambiar nombre de los municipios
colnames(colombia)[2] <- "COD_MUNICIPIO"

## combinar codigo de municipios y departamentos

colombia$COD_MUNICIPIO <- paste0(colombia$DPTO_CCDGO, 
                                 colombia$COD_MUNICIPIO) |> as.numeric()


## cargamos los datos de internet en colombia

internet <- read.csv("Internet_Fijo_Penetraci_n_Municipio_20240225.csv")

internet$COD_DEPARTAMENTO <- as.character(internet$COD_DEPARTAMENTO)

internet <- internet[internet$AÑO == 2023 & internet$TRIMESTRE == 3, ]


colombia <- merge(internet[, c(5:7, 9)], colombia, all.y = T)

## cambiar comas por puntos
colombia$INDICE <- gsub(",", ".", colombia$INDICE) |> as.numeric()

## transformar data.frame en sfdata.frame

colombia <- st_as_sf(colombia)

## grafico

grafico <- ggplot(colombia, aes(fill = colombia$INDICE)) + geom_sf()  + coord_sf() +
  theme_minimal() + scale_fill_gradient(low = "grey85", high = "red2") +
  labs(fill = "Indice internet", title = "Colombia") +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(family = "Ubuntu Regular", color = "#22211d"),
    panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    legend.position = c(0.15, 0.8),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         height = unit(2.5, "cm"), width = unit(2.0, "cm"),
                         style = north_arrow_fancy_orienteering)



### Antioquia 

antioquia <- colombia[colombia$DPTO_CNMBR == "ANTIOQUIA", ]

grafico2 <- ggplot(antioquia, aes(fill = INDICE)) + geom_sf()  + coord_sf() +
  theme_minimal() + scale_fill_gradient(low = "grey85", high = "red2") +
  labs(fill = "Indice internet", title = "Antioquia") +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(family = "Ubuntu Regular", color = "#22211d"),
    panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0)
  ) +  annotation_scale(location = "tr", width_hint = 0.3)


### valle del cauca

vcauca <- colombia[colombia$DPTO_CNMBR == "VALLE DEL CAUCA", ]

grafico3 <- ggplot(vcauca, aes(fill = INDICE)) + geom_sf()  + coord_sf() +
  theme_minimal() + scale_fill_gradient(low = "grey85", high = "red2") +
  labs(fill = "Indice internet", title = "Valle del cauca") +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(family = "Ubuntu Regular", color = "#22211d"),
    panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0)
  ) +  annotation_scale(location = "tl", width_hint = 0.3)




### cundinamarca

cundinamarca <- colombia[colombia$DPTO_CNMBR == "CUNDINAMARCA", ]

grafico4 <- ggplot(cundinamarca, aes(fill = INDICE)) + geom_sf()  + coord_sf() +
  theme_minimal() + scale_fill_gradient(low = "grey85", high = "red2") +
  labs(fill = "Indice internet", title = "Cundinamarca") +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(family = "Ubuntu Regular", color = "#22211d"),
    panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0)
  ) +  annotation_scale(location = "tr", width_hint = 0.3)


### atlantico


atlantico <- colombia[colombia$DPTO_CNMBR == "ATLÁNTICO", ]

grafico5 <- ggplot(atlantico, aes(fill = INDICE)) + geom_sf()  + coord_sf() +
  theme_minimal() + scale_fill_gradient(low = "grey85", high = "red2") +
  labs(fill = "Indice internet", title = "Atlántico") +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(family = "Ubuntu Regular", color = "#22211d"),
    panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.margin = margin(0, 0, 0, 0)
  )  +  annotation_scale(location = "tl", width_hint = 0.3)


grafico | ((grafico2 | grafico3) / (grafico4 | grafico5))
