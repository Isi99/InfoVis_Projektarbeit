#Bibliotheken laden
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
#install.packages("plotly")
#install.packages("gapminder")
library(plotly)
library(gapminder)
#install.packages("png")
library(png)
#install.packages("shinyWidgets")
library(shinyWidgets)
#install.packages("shinydashboardPlus")
library(shinydashboardPlus)


### Datensätze einlesen ----
meat <- read_rds("meatnew.rds")
mortality <- read_rds("mortality.rds")
#indicator_weight <- read_rds("prevalence_overweight.rds")
supply <- read_rds("supply.rds")
weight <- read_rds("weight.rds")
obes_region <- read_rds("obes_region.rds")
weight_comb <- read_rds("weight_comb.rds")
mortality_select <- readxl::read_xlsx("global_mortality_selection.xlsx")
indicator <- read.csv("indicator.csv", sep = ";")

# Übergewichtige Frauen im Verlauf der Zeit 
temp421 <- weight %>%
 filter(Entity == c("Central African Republic", "Germany", "United States")) 


p123test <- weight %>%
  plot_ly(
    x = ~ Year,
    y = ~ `f_Overweight or Obese (%)`,
    color = ~ Entity,
    frame = ~ Year, 
    text = ~ Entity,
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type ="log"
    )
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "YEAR", font = list(color = "red"))
  )


ggplotly(p123test)


# Enwticklung der Kalorienversorgung im Laufe der Zeit

temp31 <- supply %>%
  filter(Entity == c("Australia", "Brazil", "Central African Republic", "China", "Germany", "United Kingdom", "United States", "Sweden","Indonesia"))

p12345test <- temp31 %>%
  plot_ly(
    x = ~ Year,
    y = ~ `Daily caloric supply (kcal/person/day)`,
    color = ~ Entity,
    frame = ~ Year, 
    text = ~ Entity,
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type ="log"
    )
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "YEAR", font = list(color = "red"))
  )


ggplotly(p12345test)

#output$caloricA2plot <- renderPlotly({
  
  
  p31 <- temp31 %>% ggplot() + 
    aes(x = Year, y = `Daily caloric supply (kcal/person/day)`, color = Entity) + # x Achse bis 2013 anzeigen lassen?
    geom_line()+
    theme_minimal() +
    scale_color_manual(values=c("#DBA901", "#00FFBF","#5F4C0B", "#21610B",
                                "#31B404", "#FF0080", "#5882FA", "#8A0829","#0404B4"))+
    labs(
      title = "Entwicklung der täglichen Kalorienversorgung im Laufe der Zeit",
      x = "Jahr",
      y = "Tägliche Kalorienversorgung (in kcal pro Person/Tag)",
      color = "Ausgewählte Länder") 
  ggplotly(p31)
})