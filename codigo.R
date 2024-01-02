
## paquetes necesarios 

library(tidyverse)
library(janitor)
library(showtext)
library(MetBrewer)
library(scico)
library(ggtext)
library(patchwork)
library(gghighlight)
library(ggflags)
library(countrycode)
library(scales)

## cargamos los datos sobre inflación 

datos <- read.csv("Inflation.csv")

## paises a omitir 

paises <- c("El Salvador", "Rep. Dominicana", "Costa Rica", "Guatemala ", 
            "Nicaragua", "Honduras", "Panamá", "Argentina ", 
            "Venezuela")

## seleccionar paises restantes 

paises <- unique(datos$País)[!unique(datos$País) %in% paises]


datos <- datos[datos$País %in% paises, ]

## corregir variable inflación 

datos$Inflación.Anual <- gsub("%", "", datos$Inflación.Anual) |> as.numeric() / 100

## corregir espacio en los nombres

datos$País <- gsub(" ", "", datos$País)

## cambiar Perú por Peru, Brasil por Brazil, México por Mexico

datos$País[datos$País == "Perú"] <- "Peru"
datos$País[datos$País == "Brasil"] <- "Brazil"
datos$País[datos$País == "México"] <- "Mexico"

## extraer codigo de los paises con countrycode

datos$codigo <- tolower(countrycode(datos$País, 
                                    origin = "country.name", 
                                    destination = "iso2c"))


### descargar fuente gudea
font_add_google(family="Gudea", "Gudea", db_cache = TRUE)
fpath <- systemfonts::font_info(family = "Font Awesome 6 Brands")[["path"]]
font_add(family = "fa-brands", regular = fpath)
showtext_auto(enable = TRUE)

### grafico artículo 
grafico1 <- ggplot(datos, aes(datos$Año, datos$Inflación.Anual, color = País), group = 1) +
  geom_line(size = 0.8, color = "black") +
  geom_point(data = datos[datos$Año == "2022", ],
             aes(datos[datos$Año == "2022", 2], 
                 datos[datos$Año == "2022", 5])) +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(colour = alpha("grey85", 1))) +
  
  geom_flag(data = datos[datos$Año == "2022", ],
            aes(datos[datos$Año == "2022", 2], 
                datos[datos$Año == "2022", 5],
                country = datos[datos$Año == "2022", 6]), size = 10) +
  theme_minimal(base_family = "Gudea", base_size = 10) +
  scale_color_met_d(name="Redon") +
facet_wrap(~País) +
  
  scale_y_continuous(labels = label_percent()) +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(color="black", size=12),
    strip.text.x = element_text(face="bold", size = 15),
    plot.title = element_markdown(hjust=.5,size=34, color="black",lineheight=.8, face="bold", margin=margin(20,0,30,0)),
    plot.subtitle = element_markdown(hjust=.5,size=18, color="black",lineheight = 1, margin=margin(10,0,30,0)),
    plot.caption = element_markdown(hjust=.5, margin=margin(60,0,0,0), size=8, color="black", lineheight = 1.2),
    plot.caption.position = "plot",
    plot.background = element_rect(color="#F4F5F1", fill="#F4F5F1"),
    plot.margin = margin(10,10,10,10),
    legend.position = "none",
    legend.title = element_text(face="bold")
  )



# Titulo 
text <- tibble(
  x = 0, y = 0,
  label = "**Panorama Económico de América Latina: Un Análisis de las Tasas de Inflación**"
)


titulo <- ggplot(text, aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = "#F4F5F1", fill="#F4F5F1", width = unit(24, "lines"),
    family="Gudea", size = 14, lineheight = 1
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(color="#F4F5F1", fill="#F4F5F1"))


## texto 

text2 <- tibble(
  x = 0, y = 0,
  label = "*Bienvenidos a esta presentación que arrojará luz sobre el panorama económico de América Latina a través de un minucioso análisis de las tasas de inflación en nueve países clave. Exploraremos la variabilidad, tendencias y desafíos económicos que estos países han enfrentado en sus esfuerzos por mantener la estabilidad financiera. Acompáñenos en este viaje informativo mientras examinamos detalladamente las estadísticas descriptivas que revelan el comportamiento de la inflación en la región.*"
)

sub <- ggplot(text2, aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = "#F4F5F1", fill="#F4F5F1", width = unit(26, "lines"),
    family="Gudea", size = 4.6, lineheight = 1.5
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(color="#F4F5F1", fill="#F4F5F1"))

(titulo / sub | grafico1) + plot_annotation(caption = "-")



### graficos individuales


