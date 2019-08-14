# View(waste_df)

library(tidyverse)
#install.packages("janitor")
library(janitor)


## Data cleaning ----
coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")
mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")
continent <- readr::read_csv("https://datahub.io/JohnSnowLabs/country-and-continent-codes-list/r/country-and-continent-codes-list-csv.csv")

# Clean up names and mutate variables
coast_vs_waste <- clean_names(coast_vs_waste) %>%
  mutate(coastal_pop_pct = coastal_population/total_population_gapminder) 

mismanaged_vs_gdp <- mismanaged_vs_gdp %>%
  set_names(c("entity", "code", "year", "mismg_kg_pp_pd", "gdp_per_capita", "total_population_gapminder"))

waste_vs_gdp <- waste_vs_gdp %>%
  set_names(c("entity", "code", "year", "waste_kg_pp_pd", "gdp_per_capita", "total_population_gapminder"))

continent <- clean_names(continent)

continent$continent_code <- ifelse(is.na(continent$continent_code), "NA", continent$continent_code)

# Merging data in a single dataframe
waste_df <- merge(coast_vs_waste, mismanaged_vs_gdp, by = c("code", "year"))
waste_df <- merge(waste_df, waste_vs_gdp, by = c("code", "year"))
waste_df <- merge(waste_df, continent, by.x = "code", by.y = "three_letter_country_code")
waste_df <- select(waste_df, -c(country_name, two_letter_country_code, country_number))

# Bibliotheken laden ----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(scales)


# Plastic Waste app.R

ui <- dashboardPage(
  # Dashboard Header ----
  dashboardHeader(title = "Plastic Waste"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plastic Waste of the World", tabName = "plasticworld", icon = icon("globe")),
      menuItem("Waste per GDP", tabName = "gdp", icon = icon("coins"))
    )
  ),
  # Dashboard Body ----
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "plasticworld",
        # Menü Seite 1 Plastic Waste of the World ----
        fluidRow(
          column(width = 9,
                 
                 box(
                   plotOutput("mainplot", height = 350)
                   #DTOutput("test")
                 ),
                 
                 box(
                   plotOutput("thirdplot", height = 350)),
                 
                 box(
                   checkboxGroupInput("continent_sel",
                                      label = "Please Select the Continent",
                                      choices = supply_countires_all$Entity %>% unique(),
                                      selected = supply_countires_all$Entity %>% unique())
                   ),
                 box(
                   textInput("text", label = "Kalorienkonsum")
                    )
          )
        )
      ),
      
      tabItem(
        tabName = "gdp",
        fluidRow(
          # Menü Seite 2 Waster per GDP
          column(width = 9,
                 
                 box(
                   plotOutput("secondplot", height = 350)),
                 
                 box(
                   title = "Plastic Waste Facts and Figures", background = "maroon",
                   "Im Meer befinden sich mehr als fünf Bilionen Plastikteile / 
                   73% des weltweiten Müll an Stränden besteht aus Plastik / 
                   Die Plastikproduktion ist weltweit von 2,1 Mo. Tonnen im Jahr 1950 auf 406. Mio Tonnen im Jahr 2015 angestiegen/
                   Weltweit werden fast eine Million Getränkeflaschen aus Kunststoff verkauft / 
                   Über 700 Meerestierarten haben nach Berichten Plastik gefressen oder sichd darin verfangen/
                   Über 40 Prozent aller Kunsstoffe wird nur einmal verwendet und dann weggeworfen.
                   (Quelle: https://www.nationalgeographic.de/10-erschreckende-fakten-uber-plastik)")
                 )
                 )
                 )
      
                 )
          )
          ) 

server <- function(input, output) {
  
  data_df <- reactive({
    supply_countires_all %>% na.omit() %>% 
      filter(Entity %in% input$continent_sel) 

  })
  
  output$menu <- renderMenu({
    siderbarMenu(
      menuItem("Plastic Waste of the World", tabName = "plasticworld", icon = icon("globe")),
      menuItem("Waste per GDP", tabName = "gdp", icon = icon("coins"))
    )
    
  })
  
  output$test <- renderDT(options = list(lengthChange = FALSE), data_df()  )
  
  output$mainplot <- renderPlot({
    data_df() %>%
      ggplot() +
      aes(x = Entity, y = `Daily caloric supply (kcal/person/day)`, fill = Entity ) +
      geom_boxplot() +
      labs(
        title = "Mismanaged Plastic Waste highest in Oceania",
        subtitle = "Boxplot of Mismanaged Plastic Waste per Capita by Continent",
        x = "Continent",
        y = "Amount of Mismanaged Plastic waste per Capita in kg (day)",
        caption = "Tidy Tuesday data 05/2019 / Points are outliers") +
      guides(fill = FALSE)
  })
  
  output$thirdplot <- renderPlot ({
    waste_df %>% 
      ggplot() +
      aes(x = coastal_population, y = mismg_kg_pp_pd, colour = continent_name) + 
      geom_point() +
      labs(
        title = "Costal Population and Continent' Mismanaged Placstic Waste",
        subtitle = "Scatter Plot of Mismanaged Plastic Waste per Capita by Coastal Population",
        x = "Coastal Population",
        y = "Amount of Mismanaged Plastic Waste per Capita in kg (day)",
        colour = "Continents",
        caption = "Tidy Tuesday data 05/2019"
      )
  })
  
  output$secondplot <- renderPlot({
    waste_df %>%
      ggplot() +
      aes(x = waste_kg_pp_pd, y = gdp_per_capita.x, colour = continent_name) +
      geom_point(alpha = 0.75) +
      scale_y_log10() +
      coord_flip() + 
      labs(
        title = "Does the GDP affects the Plastic Waste of the Continents?",
        subtitle = "Scatterplot of Plastic Waste per Capita according to GDP",
        x = "Plastic Waste per Capita in kg (day)",
        y = "GDP per Capita",
        colour = "Continent",
        caption = "Tidy Tuesday data 05/2019"
      )
  })
  
  
}


shinyApp(ui, server)





