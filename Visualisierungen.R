library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
#install.packages("plotly")
#install.packages("gapminder")
library(gapminder)

#### Datensätze einlesen ----
meat <- read_rds("meatnew.rds")
mortality <- read_rds("mortality.rds")
indicator_weight <- read_rds("prevalence_overweight.rds")
supply <- read_rds("supply.rds")
weight <- read_rds("weight.rds") 
obes_region <- read_rds("obes_region.rds")


#### Visualisierungen ----


### Thema 1: Trend global mortality -> Sterblichkeitsrate im Verlauf der Zeit für ausgewählte Länder

# Wie alle Ursachen für Tod in einem Plot darstellen?

# in App: Alternative 1  
# selectInput("Varselect", "Select the Causes of Death",
#             choices = mortality$`Cardiovascular diseases (%)`,
#             mortality$`Cancers (%)`,
#             mortality$`Diabetes (%)`,
#             mortality$`Dementia (%)`,
#             mortality$`Road accidents (%)`,
#             mortality$`Kidney disease (%)`,
#             mortality$`Suicide (%)`,
#             mortality$`Homicide (%)`,
#             mortality$`Nutritional deficiencies (%)`,
#             mortality$`Protein-energy malnutrition (%)`,
#             mortality$`Terrorism (%)`,
#             multiple = TRUE, selected = "`Diabetes (%)`"))
#
# in App: Alternative 2
# mortalityCauses <- mortality %>%
#        filter(year, country)

# selectInput: ("Varselect", "Select the Causes of Death",
#                choices = names(mortalityCauses))
#
# Output: 
# output$mortalityplot <- renderPlot ({
#        mortality %>%
#        filter(causes %in" input_Varselect) %>%
#        ggplot(aes(x = year, y = "mortality", colour = = ausgewählte Länder (Deutschland, USA, Zentralafrika)) +
#        geom_line())



# Test Test Test !!!
##Erstens:Verteilung World
#mortality_world <- mortality %>%
 #filter(country == "World")

#ggplot(data = mortality_world) +
 # aes(x = year, y = TotalVar, color = country) +
  #geom_point() +
  #theme_minimal()

# Zweitens: Verteiling nach Asia, Europa, USA, Afrika, Latein Amerika und Deutschland
#mortality_countries <- mortality %>%
 # filter(country == c("Central Asia", "Central Europe", "United States", "Germany", "Central African Republic", "Central Latin America"))


#ggplot(data = mortality_countries) +
 # aes(x = year, y = `Diabetes (%)`, color = country) +
  #geom_line() +
#  theme_minimal()



### Thema 3: Globale Ernährung (Bubble Chart oder Weltkarte, Choropleth Map) ----

## (1) Weltkarte: Daily caloric supply in den verschiedenen Ländern ---- auf das aktuellste Jahr beziehen

# Video zu leaflet vom Calero !


## (2) BubbleChart: GDP per Capita und Daily per capita fat supply ----

supply_year <- supply %>%
  filter(Year == "2013")

p <- ggplot(data = supply_year) + 
  aes(x = `GDP per capita (2011 international-$)`, y = `Daily per capita fat supply (grams per day) (g/person/day)`) +
  geom_point() +
  theme_minimal() 

ggplotly(p)


## (2.1) BubbleChart: GDP per Capita und Daily per capita fat supply für die Länder USA, Deutschland und Central African Republik ----

supply_countires <- supply %>%
  filter(Entity == c("Central African Republic", "Germany", "United States"))


p2 <- ggplot(data = supply_countires) +
  aes(x = `GDP per capita (2011 international-$)`, y = `Daily per capita fat supply (grams per day) (g/person/day)`, color = Entity, size = `Total population (Gapminder)`) +
  geom_point() +
  theme_minimal() +
  scale_x_log10()

ggplotly(p2)



## Thema 4: Vergleich/Relationship: Übergewicht und Fettleibigkeit ----

# Wie beide Variablen indicator_over & indicator_obes in einem Plot anzeigen lassen? 
# in App: Select Funktion