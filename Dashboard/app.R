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
#indicator_weight <- read_rds("prevalence_overweight.rds")
supply <- read_rds("supply.rds")
weight <- read_rds("weight.rds")
obes_region <- read_rds("obes_region.rds")
weight_comb <- read_rds("weight_comb.rds")
mortality_select <- readxl::read_xlsx("global_mortality_selection.xlsx")
indicator <- read.csv("indicator.csv", sep = ";")


### Anmerkungen
# im suppy_contries_all Datensatz werden nicht alle Jahre angezeigt
supply_countries_all <- supply %>%
  filter(Entity == c("Australia", "Brazil", "Central African Republic", "China", "Germany", "United Kingdom", "United States", "Sweden",
                     "Indonesia"))

#UI ----
# Define UI for application that draws a histogram

ui <- dashboardPage(
  skin = "green",
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
                  box(width = "12",
                      title = "Einführung...",
                      status = "warning",
                      "..."
                      ), 
                  box(width = "12",
                      title = "Kalorienkonsum",
                      plotOutput("caloricplot")
                      ),
                  box(width = "12",
                      title = "KalorienkonsumsA2",
                      plotlyOutput("caloricA2plot")
                      ),
                  box(
                    checkboxGroupInput("continent_sel",
                                       label = "Please Select the Continent or Country",
                                       choices = supply_countries_all$Entity %>% unique(),
                                       selected = c("Germany", "Indonesia", "United States"))
                      ),
                  box(width = "12",
                      title = "Fettkonsum & GDP",
                      status = "warning",
                      "..."
                      )
                      ),
              splitLayout(
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
    tabItem(tabName = "ta4",
              fluidRow(
                  box(widht = "12",
                       title = "Einleitung", 
                       status = "warning",
                       "..."
                       ),
                  infoBoxOutput("totalbox6", width = 12)
                  ,
                  box(width = "12",
                      title = "Definition Übergewicht und Fettleibigkeit (Adipositas)",
                      status = "warning",
                      "Übergewicht und Fettleibigkeit (Adipositas) sind nicht dasselbe.
                       Übergewicht bedeutet, dass der Betroffene über seinem Normalgewicht liegt. Es handelt sich dabei um den Übergang vom Normalgewicht zur Adipositas. 
                       Der Begriff Adipositas bedeutet, dass jemand sehr starkes Übergewicht und dadurch einen krankhaft erhöhten Körperfettanteil hat. 
                       Daher wird Adipositas auch Fettleibigkeit oder Fettsucht genannt.
                       Zu Definition dient die Kennzahl des Body-Mass-Index (BMI).
                       Ein BMI ab 25 kg/m2 gilt per Definition als Übergewicht, ein BMI von 30 kg/m2 und höher als Adipositas.
                      (Quelle: https://www.vigo.de/rubriken/krankheit-und-therapie/stoffwechsel/lesen/uebergewicht-und-adipositas-volkskrankheiten.html)"
                      ),
                  box(width = "12",
                    title = "Übergewicht & Fettleibigkeit",
                    plotOutput("überfettplot")
                    )
                    ),
                  sliderInput("select_year", label ="Select a Year",
                              min = min(weight$Year),
                              max = max(weight$Year),
                              value = 2014),
             splitLayout(
                 box(width = "12",
                   title = "Übergewicht Frau",
                   plotOutput("womenplot")
                     ),
                box(width = "12",
                  title = "Übergwicht Mann",
                  plotOutput("manplot")
                )
              )
            ),

      #Fifth tab content----
    tabItem(tabName = "ta5",
            fluidRow(
              sidebarLayout(
                sidebarPanel(
                  selectInput("countselect","Select a Country", choices = meat$Entity, multiple = TRUE, selected = c("Germany", "United States", "Central African Republic"))
                ),
                  box(width="12",
                      title="Fleischkonsum",
                      plotOutput("fleischplot")
                      )
              ),
              
              infoBoxOutput("totalbox5", width = 12),
              
              sidebarLayout(
                sidebarPanel(
                  selectInput("countselect","Select a Country", choices = weight_comb$Entity, multiple = TRUE, selected = c("Germany", "United States", "Central African Republic"))
                ),
                   box(width="12",
                      title="Übergewicht",
                      plotOutput("overweightplot")
              )
              )
            )
    ),    
    
      #Sixth tab contenct ----
    tabItem(tabName = "ta6",
            fluidRow(
              box(widht = "12",
               title = "Projekt", 
               status = "warning",
                "..."
                 ),
              actionLink("linkinfo", label = "Weitere Informationen zum Thema
                         'Gesunde Ernährung' finden Sie hier.", icon=icon ("seedling")
                         ),
              
              actionLink("linkproject", label = "Ein weiteres spannendes Projekt zur Entwicklung der Ernährung finden Sie hier.", icon=icon ("external-link-alt"))
                    )
            ),
      # Infos zum Proejkt In form
      # Broschüre als PDF downloaden
      # Link zu Freya und Nicoles App
    
    
      #Seventh tab content----
    tabItem(tabName = "ta7",
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
        "Diabetes", paste0(25 + input$count,"0,44 Mrd."), icon = icon("syringe"),
        color = "yellow"
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
  
  output$caloricplot <- renderPlot({
    
    temp23 <- supply_countries_all %>%
      filter(Entity %in% input$continent_sel)
    
    temp23 %>%  ggplot() + 
      aes(x = Entity, y = `Daily caloric supply (kcal/person/day)`, fill = Entity) + # Filter(year)?
      geom_boxplot() + #oder geom_bar?
      labs(x = "Country/Continent",
           y = "Y",
           title = "Daily caloric supply highest in US") +
      theme_minimal() +
      guides(fill = FALSE)
  
  })
  
  ## (1.1) Alternative: Daily caloric supply im Verlauf der Jahre
  
  output$caloricA2plot <- renderPlotly({
    
    temp24 <- supply_countries_all %>%
      filter(Entity %in% input$continent_sel)
    
    p4 <- temp24 %>% ggplot() + 
      aes(x = Year, y = `Daily caloric supply (kcal/person/day)`, color = Entity) + # x Achse bis 2013 anzeigen lassen?
      geom_line()+
      theme_minimal() 
    ggplotly(p4)
  })
  
  
  ## (2) BubbleChart: GDP per Capita und Daily per capita fat supply ----

  output$gdp1plot <- renderPlotly({
  
  temp31 <- supply %>%
    filter(Year == "2013")
  
  p1 <- temp31 %>% ggplot() +
    aes(x = `GDP per capita (2011 international-$)`, y = `Daily per capita fat supply (grams per day) (g/person/day)`) +
    geom_point() +
    theme_minimal() 
  ggplotly(p1)
  })
  
  ## (2.1) BubbleChart: GDP per Capita und Daily per capita fat supply für die Länder USA, Deutschland und Central African Republik ----
  output$gdp2plot <- renderPlotly({
  
    temp32 <- supply %>%
    filter(Entity == c("Central African Republic", "Germany", "United States"))
  
  
  p <- temp32 %>% ggplot() +
    aes(x = `GDP per capita (2011 international-$)`, y = `Daily per capita fat supply (grams per day) (g/person/day)`, color = Entity, size = `Total population (Gapminder)`) +
    geom_point() +
    theme_minimal() +
    scale_x_log10() 
  ggplotly(p)
})
  
  #Output Plot Tab 4----
  #Output Progess Box---- 
  
  # (1) Überschrift Differenzierung Übergewicht und Fettleibigkeit
  
  output$totalbox6 <- renderInfoBox({
    infoBox(
      "Was ist der Unterschied zwischen Übergwicht und Fettleibigkeit?", icon = icon("question"),
      color = "red"
           )
  })
  
  # (2) Entwicklung und Kontrast Übergewicht und Fettleibigkeit 
  
  # Übergwicht Frauen
  
  output$womenplot <- renderPlot ({
    
    temp25 <- weight %>%
      filter(Entity == c("Central African Republic", "Germany", "United States")) 
     
    
    temp25 %>%  ggplot(
      aes(y = `f_Overweight or Obese (%)`,x = Year, colour = Entity)) +
        geom_line()
   
  })
  
  # Übergwicht Männer
  output$manplot <- renderPlot ({
    
    temp26 <- weight %>%
      filter(Entity == c("Central African Republic", "Germany", "United States")) 
    
    temp26 %>% ggplot(
      aes(y = `m_Overweight or Obese (%)`, x = Year, color = Entity)) + 
      geom_line()
    
  })
  
  # Infoboxen einfügen: DEU 2014: 48,6 % Frauen übergewichtig, 64% Männer !!!
  
  #Output Plot Tab 5----  
  output$fleischplot <- renderPlot({
    
    temp5 <- meat %>% 
    filter(Entity == c("Central African Republic", "Germany", "United States")) %>% 
     filter(Entity %in% input$countselect) %>% filter(`Food Balance Sheets: Meat - Food supply quantity (kg/capita/yr) (FAO (2017)) (kg)` != "2007")
    
    temp5 %>%      ggplot(
      aes(x = Year, y = `Food Balance Sheets: Meat - Food supply quantity (kg/capita/yr) (FAO (2017)) (kg)`, group = Entity, colour = Entity)) + 
      geom_line() +
        labs(x="Years",
           y="Density",
           title= "Food Supply quantity (kg/capita/year) over Years",
           fill = "Food Supply quantity (kg/capita/year)")
  })

  output$totalbox5 <- renderInfoBox({
    infoBox(
      "Was fällt hier auf?", icon = icon("lightbulb"),
      color = "green"
    )
  })  
  
  output$overweightplot <- renderPlot({
    
    temp5 <- weight_comb %>% 
      filter(Entity == c("Central African Republic", "Germany", "United States")) %>% 
      filter(weight_type == "Overweight") %>% 
      filter(Entity %in% input$countselect) 
    
    temp5 %>%      ggplot(
      aes(x = Year, y = weight_total, group = Entity, colour = Entity)) + 
      geom_line() +
      labs(x="Years",
           y="Overweight",
           title= "Overweight over Years",
           fill = "Land")
  })

  #Output Plot 6 ----
  
  output$text <- renderText({input$text})
   
}
shinyApp(ui, server)