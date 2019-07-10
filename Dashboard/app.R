library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
#install.packages("plotly")
#install.packages("gapminder")
library(plotly)
library(gapminder)

### Datensätze einlesen ----
meat <- read_rds("meatnew.rds")
mortality <- read_rds("mortality.rds")
indicator_weight <- read_rds("prevalence_overweight.rds")
supply <- read_rds("supply.rds")
weight <- read_rds("weight.rds")
obes_region <- read_rds("obes_region.rds")
#weight_comb <- read_rds("weight_comb.rds")
mortality_select <- readxl::read_xlsx("global_mortality_selection.xlsx")


#UI ----
# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "Gesundheit 4.0"),
  dashboardSidebar(width = 250,
                   #Menü einrichten----
                   sidebarMenu(
                     menuItem("Worum geht's?",tabName = "ta1",icon=icon("search")),
                     menuItem("Was passiert mit uns?",tabName = "ta2",icon=icon("chart-line")),
                     menuItem("Globale Ernährung",tabName = "ta3",icon=icon("fas fa-utensils")),
                     menuItem("Übergewicht und Fettleibigkeit",tabName = "ta4",icon=icon("weight")),
                     menuItem("Fleischkonsum",tabName = "ta5",icon=icon("drumstick-bite")),
                     menuItem("In Form",tabName = "ta6",icon=icon("walking")),
                     menuItem("About this Project",tabName = "ta7",icon=icon("comment-alt"))
                   )  
  ),
  
  dashboardBody(
    tabItems(
      #First tab content---- 
      tabItem(tabName = "ta1",
              fluidRow(
                mainPanel( 
                  HTML('<p><img src="Lebensmittelkorb.png"/></p>') ),
                box(width = 12,
                    title = "Neue Ära der Gesundheit",
                    status = "warning",
                    "Das erste Mal in der Weltgeschichte sterben mehr Menschen auf unserem Planeten an den Folgen von 
                    Fettleibigkeit als an Hunger.[Quelle: Yuval Noah Harari] Woran liegt das? Das BMLE hat sich im Projekt Gesundheit 4.0 
                    mit dieser Frage beschäftigt. Entdecken Sie wie sich die Entwicklung des Körpergewichts in den letzten Jahrzehnten 
                    verändert hat und welche Faktoren dabei eine Rolle spielen. Viel Spaß beim Stöbern und Entdecken. Ihr BMLE")
              )
      ),
      
      #Second tab content ----
      tabItem(tabName = "ta2",
              fluidRow(
                sidebarLayout(
                  sidebarPanel(
                    selectInput("diseasselect","Select a Disease", choices = mortality_select$Disease_type, multiple = TRUE, selected = "Diabetes")
                  ),
                  box(width="12",
                      title="disease",
                      plotOutput("diseaseplot")
                      )
                ),
                
                infoBoxOutput("totalbox1", width = 6), 
                infoBoxOutput("totalbox2", width = 6),
                infoBoxOutput("totalbox3", width = 6),
                infoBoxOutput("totalbox4", width = 6),
                    
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput("diseasecount","Select a Disease", choices = mortality_select$Disease_type, multiple = TRUE, selected = "Diabetes")
                  ),
                  box(width="12",
                      title="diseasecount",
                      plotOutput("diseasecountplot")
                      )
                )
              )
      ),    
  
      #Third tab content ----
      tabItem(tabName = "ta3",
              fluidRow(
                  box(width="12",
                      title="GDP1",
                      plotlyOutput("gdp1plot")
                      ),
                  box(width="12",
                      title="GDP2",
                      plotlyOutput("gdp2plot")    
                      )
            )
      ),        

      
      #Fourth tab content ----

      #Fifth tab content----
    tabItem(tabName = "ta5",
            fluidRow(
              sidebarLayout(
                sidebarPanel(
                  selectInput("countselect","Select a Country", choices = meat$Entity, multiple = TRUE, selected = "Germany")
                ),
              box(width="12",
                  title="Fleischkonsum",
                  plotOutput("fleischplot")
                  ),
              box(width="12",
                  title="Übergewicht",
                  plotOutput("overweightplot")
              )
              )
            )
    ),    
    
      #Sixth tab contenct ----
    
    
      #Seventh tab content----
    tabItem(tabName = "ta6",
            fluidRow(
               box(width = 12,
                  title = "Motivation",
                  status = "warning",
                  ""),
               
               box(width = 12,
                   title = "Methodik",
                   status = "warning",
                   ""),
               
               box(width = 12,
                   title = "Quellen, Urheberrechte & Lizenzen",
                   status = "warning",
                   "")
            )
    )   
   
      )
    )

)



#Server----
server <- function(input, output) { 
  
  #Output Menu ----
  output$menu <- renderMenu ({
    sidebarMenu(
      menuItem("Worum geht's?",tabName = "ta1",icon=icon("search")),
      menuItem("Was passiert mit uns?",tabName = "ta2",icon=icon("chart-line")),
      menuItem("Globale Ernährung",tabName = "ta3",icon=icon("fas fa-utensils")),
      menuItem("Übergewicht und Fettleibigkeit",tabName = "ta4",icon=icon("weight")),
      menuItem("Fleischkonsum",tabName = "ta5",icon=icon("drumstick-bite")),
      menuItem("In Form",tabName = "ta6",icon=icon("walking")),
      menuItem("About this Project",tabName = "ta7",icon=icon("comment-alt"))
    )  
  })
  
  #Output Text Homepage----
  #Bild einfügen
#  output$image <- renderImage({
#    })
    # outfile <- tempfile(fileext = "Lebensmittelkorb.png")
  
   
  
  output$text <- renderText({input$text})
  

  #Output Plot Tab 2----  
  #Plot Disease
  output$diseaseplot <- renderPlot({
    
    temp21 <- mortality_select %>% 
      filter(Disease_type %in% input$diseasselect) %>% filter(country == "World")
    
    temp21 %>%      ggplot(
      aes(x = year, y= `Disease (%)`, colour= Disease_type, group= Disease_type)) + 
      geom_line() +
      labs(x="Jahr",
           y="Y",
           title= "X",
           fill = "X")
  })
  
  #Output Progess Box---- 
  #Überschrift Kontrast Weltbevölkerung und Tote durch (Stand 2016)...

  output$totalbox1 <- renderInfoBox({
    infoBox(
      "Weltbevölkerung", paste0(25 + input$count,"7,47 Mrd."), icon = icon("users"),
      color = "purple"
    )
  })  
  
  output$totalbox2 <- renderInfoBox({
    infoBox(
      "Herzleiden", paste0(25 + input$count, " 2,4 Mrd."), icon = icon("heartbeat"),
      color = "red"
    )
  })
    
    output$totalbox3 <- renderInfoBox({
      infoBox(
        "Diabetes", paste0(25 + input$count,"0,44 Mrd."), icon = icon("crutch"),
        color = "purple"
      )
    })
  
    output$totalbox4 <- renderInfoBox({
      infoBox(
        "Terrorismus", paste0(25 + input$count, " 0,004 Mrd."), icon = icon("bomb"),
        color = "black"
      )
    })
    
  #Plot Disease Countryselection
  output$diseasecountplot <- renderPlot({
    
    temp22 <- mortality_select %>% 
      filter(Disease_type %in% input$diseasecount) %>%  filter(country == c("Germany"))
    
    temp22 %>%      ggplot(
      aes(x = year, y= `Disease (%)`, colour= Disease_type, group= Disease_type)) + 
      geom_line() +
      labs(x="Jahr",
           y="Y",
           title= "X",
           fill = "X")
  })
  
  #Output Plot Tab 3----
  
  ## (1) Weltkarte: Daily caloric supply in den verschiedenen Ländern ---- auf das aktuellste Jahr beziehen
  
  # Video zu leaflet vom Calero !
  
  
  ## (2) BubbleChart: GDP per Capita und Daily per capita fat supply ----

  output$gdp1plot <- renderPlotly({
  
  temp31 <- supply %>%
    filter(Year == "2013")
  
  temp31 %>%  plot_ly() +
    aes(x = `GDP per capita (2011 international-$)`, y = `Daily per capita fat supply (grams per day) (g/person/day)`) +
    geom_point() +
    theme_minimal() 

  })
  
  ## (2.1) BubbleChart: GDP per Capita und Daily per capita fat supply für die Länder USA, Deutschland und Central African Republik ----
  output$gdp2plot <- renderPlotly({
  
    temp32 <- supply %>%
    filter(Entity == c("Central African Republic", "Germany", "United States"))
  
  
  temp32 %>% plot_ly() +
    aes(x = `GDP per capita (2011 international-$)`, y = `Daily per capita fat supply (grams per day) (g/person/day)`, color = Entity, size = `Total population (Gapminder)`) +
    geom_point() +
    theme_minimal() +
    scale_x_log10() 
  
})
  
  #Output Plot Tab 4----
  
  #Output Plot Tab 5----  
  output$fleischplot <- renderPlot({
    
    temp5 <- meat %>% 
      filter(Entity %in% input$countselect) %>% filter(`Food Balance Sheets: Meat - Food supply quantity (kg/capita/yr) (FAO (2017)) (kg)` != "2007")
    
    temp5 %>%      ggplot(
      aes(x = Year, y = `Food Balance Sheets: Meat - Food supply quantity (kg/capita/yr) (FAO (2017)) (kg)`)) + 
      geom_line() +
        labs(x="Years",
           y="Density",
           title= "Food Supply quantity (kg/capita/year) over Years",
           fill = "Food Supply quantity (kg/capita/year)")
  })

  output$overweightplot <- renderPlot({
    
    temp5 <- meat %>% 
      filter(Entity %in% input$countselect) %>% filter(`Food Balance Sheets: Meat - Food supply quantity (kg/capita/yr) (FAO (2017)) (kg)` != "2007")
    
    temp5 %>%      ggplot(
      aes(x = Year, y = `Food Balance Sheets: Meat - Food supply quantity (kg/capita/yr) (FAO (2017)) (kg)`)) + 
      geom_line() +
      labs(x="Years",
           y="Density",
           title= "Food Supply quantity (kg/capita/year) over Years",
           fill = "Food Supply quantity (kg/capita/year)")
  })
  
  #Output Plot 6 ----
  
  output$text <- renderText({input$text})
   
}
shinyApp(ui, server)