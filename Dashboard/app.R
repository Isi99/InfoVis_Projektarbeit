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

# Diese sind eventuell falsche Daten - Herausfiltern?
# Zeile 7659: USA overweigt 1993
# Zeile 7641: USA overweight 1975
# Zeile 7669: USA overweight 2003
# Zeile 7673: USA overweight 2007
# Zeile 7676: USA overweight 2010
# Zeile 2714: Germany overweight 2008
# Zeile 2718: Germany overweight 2012
# Zeile 2709: Germany overweight 2003
    
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
                mainPanel(tags$img (src = "bacach.png")),
               
                gradientBox(
                  width = 12,
                  title = "Neue Ära der Gesundheit",
                  icon = "fa fa-th",
                  gradientColor = "teal", 
                  boxToolSize = "sm", 
                  # footer = 
                  footer_padding = FALSE,
                  "Das erste Mal in der Weltgeschichte sterben mehr Menschen auf unserem Planeten an den Folgen von 
                    Fettleibigkeit als an Hunger.[Quelle: Yuval Noah Harari] Woran liegt das? Das BMLE hat sich im Projekt Gesundheit 4.0 
                  mit dieser Frage beschäftigt. Entdecken Sie wie sich die Entwicklung des Körpergewichts in den letzten Jahrzehnten 
                  verändert hat und welche Faktoren dabei eine Rolle spielen. Viel Spaß beim Stöbern und Entdecken. Ihr BMLE"
                )
              )
      ),
      
      

#Second tab content ----
      tabItem(tabName = "ta2",
              setShadow("card"),
              fluidRow(
 
                column(
                  width = 12,
                  align = "center",
                  flipBox(
                    id = 1,
                    main_img = "question2.png",
                    header_img = "island.png",
                    front_title = "Was passiert mit uns?",
                    hr(),
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center",
                        "Was passiert eigentlich mit uns? Erstmals in der Geschichte
                        sterben mehr Menschen an den Folgen von Übergewicht, als dass sie verhungern.
                        Die beiden Grafiken sollen dies verdeutlichen. In der ersten Grafik
                        zeigt, wie die Sterblichkeit im Verlauf der Jahre zugenommen hat.
                        Die zweite Grafik stellt die Sterblichkeitsrate der einzelnen Länder dar. 
                        Viel Spaß beim Explorieren!"
                        ),
                      plotOutput("distPlot")
                    )
                  )
              ),
            
                    gradientBox(
                      width = 12,
                      title = "Sterblichkeitsrate im Überblick",
                      icon = "fa fa-th",
                      gradientColor = "teal", 
                      boxToolSize = "sm", 
                      footer = selectInput(
                        "diseasselect","Wähle eine Krankheit", 
                        choices = mortality_select$Disease_type, 
                        multiple = TRUE, selected = "Diabetes"
                      ),
                      "Schau dir an, wie sich die Sterblichkeitsrate auf der Welt im Hinblick auf 
                      die beiden Krankheiten im Verlauf der Jahre geändert hat."
                    ),

                  box(width="12",
                      plotlyOutput("diseaseplot")
                      ),

                    gradientBox(
                      width = 12,
                      title = "Auf einen Blick",
                      icon = "fa fa-th",
                      gradientColor = "teal", 
                      boxToolSize = "sm",
                      "Auf unserer Erde leben 7,47 Milliarden Menschen. 
                      Knapp ein Drittel davon sterben jedes Jahr an einem Herzleiden."
                    ),
                    
                infoBoxOutput("totalbox1", width = 6), 
                infoBoxOutput("totalbox2", width = 6),
                infoBoxOutput("totalbox3", width = 6),
                infoBoxOutput("totalbox4", width = 6),
                    
                    gradientBox(
                      width = 12,
                      title = "Länder im Kontrast",
                      icon = "fa fa-th",
                      gradientColor = "teal", 
                      boxToolSize = "sm", 
                      footer = selectInput(
                        "countselect22","Select a Country",
                        choices = mortality_select$country,
                        multiple = TRUE, selected = c("Germany","United States")
                      ),
                      "Hier kannst du dir die Entwicklung der Sterblichkeitsraten 
                      anschauen und dabei mehrere Länder miteinander vergleichen."
                    ),

                  box(width="12",
                      title="Entwicklung der Sterblichkeitsrate nach Ländern",
                      plotOutput("diseasecountplot")
                      )

            )  
      ),    
  

#Third tab content ----
      tabItem(tabName = "ta3",
              fluidRow(
                box(width = "12",
                    title = "Einführung",
                    status = "warning",
                    "..."),
                
              sidebarLayout(
                sidebarPanel(
                    
                    infoBoxOutput("totalbox9", width = 6)
                             ),
                  
                mainPanel(
                    box(width = "12",
                      title = "KalorienkonsumA2",
                      plotlyOutput("caloricA2plot")
                        )
                            )
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
                
      #Einleitungsbox           
                  box(widht = "12",
                       title = "Einleitung", 
                       status = "warning",
                       "..."
                       ),
                  
      #Infobox Differenzierung Übergewicht und Fettleibigkeit
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
      
      #SelectionInput Country für Plot1
                  selectInput("countselect41","Select a Country", choices = indicator$Entity, multiple = TRUE, selected = c("Germany","United States","Central Africa Republic")),
      
      #Plot1 Entwicklung Übergewicht und Fettleibigkeit           
                 box(width = "12",
                    title = "Übergewicht & Fettleibigkeit",
                    plotOutput("überfettplot")
                    )
                    ),

                  box(width = "12",
                      title = "Einführung + Titel",
                      status = "warning",
                      "..."
                      ),
                  box(width = "12",
                     title = "Anzahl der übergewichtigen oder fettleibigen Personen in Deutschland im Jahr 2014 in Prozent:",
                     status = "warning"
                     ),
                  infoBoxOutput("totalbox7", width = 6)
                  ,
                  infoBoxOutput("totalbox8", width = 6)
                  ,

      
      #SliderInput Year für Plot2
                  sliderInput("select_year", label ="Select a Year",
                              min = min(weight$Year),
                              max = max(weight$Year),
                              value = 2014),
      
      #Plot2 Vergleich Männer und Frauen Übergewicht/Fettleibigkeit

             splitLayout(
                 box(width = "12",
                   title = "Übergewicht Frau",
                   plotlyOutput("womenplot")
                     ),
                 box(width = "12",
                  title = "Übergwicht Mann",
                  plotlyOutput("manplot")
                )
              )
            ),


#Fifth tab content----
    tabItem(tabName = "ta5",
            setShadow("card"),
            fluidRow(
              
              column(
                width = 12,
                align = "center",
                flipBox(
                  id = 1,
                  main_img = "question.png",
                  header_img = "forest.png",
                  front_title = "Wie sich der Fleischkonsum verändert.",
                  back_title = "Fleischkonsum",
                  hr(),
                  back_content = tagList(
                    column(
                      width = 12,
                      align = "center",
                      "Der Fleischkonsum hat sich im Laufe der Jahre stark verändert. 
                      Besonders auffällig sind die Unterschiede zwischen wohlhabenderen 
                      Ländern und Entwicklungs- und Schwellenländern."
                    ),
                    plotOutput("Plot2")
                    )
                  )
                  ),
              

                  gradientBox(
                    width = 12,
                    title = "Fleischkonsum im Kontrast",
                    icon = "fa fa-th",
                    gradientColor = "teal", 
                    boxToolSize = "sm", 
                    footer = selectInput(
                      "countselect51","Wähle ein Land", 
                      choices = meat$Entity, 
                      multiple = TRUE, selected = c("Germany", "United States", "Central African Republic")
                    ),
                    "Wie viel Fleisch konsumieren wir eigentlich? 
                    Vergleiche den Fleischkonsum unterschiedlicher Länder miteinander."
                  ),
                  
                box(width="12",
                    title = "Entwicklung der Menge an Fleischkonsum (Kilogramm pro Kopf im Jahr)",
                    plotlyOutput("fleischplot")
                ),

                  gradientBox(
                    width = 12,
                    title = "Was fällt hier auf?",
                    icon = "fas fa-lightbulb",
                    gradientColor = "teal", 
                    boxToolSize = "sm", 
                    footer = selectInput(
                      "countselect52","Wähle ein Land", 
                      choices = weight_comb$Entity, 
                      multiple = TRUE, selected = c("Germany", "United States", "Central African Republic")
                    ),
                    "Wir werden immer dicker! Woran kann das liegen? 
                     Hier kannst du den Anteil der übergewichtigen Bevölkerung 
                    der jeweiligen Länder miteinander vergleichen."
                  ),
                  
                box(width="12",
                    title = "Übergewicht im Verlauf der Jahre",
                    plotlyOutput("overweightplot")
                )

            )
    ),    
    
      
#Sixth tab contenct ----
    tabItem(tabName = "ta6",
            fluidRow(
              
              mainPanel(tags$img (src = "woman2.png")),
              
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
              
              gradientBox(
                width = 12,
                title = "Motivation",
                icon = "fa fa-th",
                gradientColor = "teal", 
                boxToolSize = "sm", 
                # footer = 
                "Ziel der App ist, dass junge Erwachsene und Familien über die Folgen von ungesunder Ernährung aufgeklärt werden.
                Dies soll der Prävention dienen und verdeutlichen, wie sich der Anteil an Übergewichtigen und Fettleibigen in
                unserer Gesellschaft im Verlaufe der Jahre verändert hat."
              ),
              
              gradientBox(
                width = 12,
                title = "Methodik",
                icon = "fa fa-th",
                gradientColor = "teal", 
                boxToolSize = "sm", 
                # footer = 
                "Für dieses Projekt wurden mehrere Datensätze, die von der Plattform 'Our World in Data' stammen, einbezogen.
                Zu Beginn wurden 17 Datensätze miteinander verglichen. Diese befassten sich mit Themen wie Sterblichkeitsrate,
                Ernährung, Fettleibigkeit und Übergewicht, Fettkonsum, Fleischkonsum. Die Datensätze wurden bereinigt und 
                teilweise zusammengefügt, sodass das Projekt auf Basis von sechs unterschiedlichen Datensätzen beruht."
              ),
              
              gradientBox(
                width = 12,
                title = "Quellen, Urheberrechte & Lizenzen",
                icon = "fa fa-th",
                gradientColor = "teal", 
                boxToolSize = "sm", 
                # footer = 
                "All unsere Daten sind von der Webseite 'Our World in Data'. 
                Diese Daten sind frei zugänglich und verwendbar, sofern man die Nutzung angibt. 
                Alle Arbeiten sind unter der 'Creative Commons BY'-Lizenz."
              )
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
  
  #Output Text Homepage 
#  output$text <- renderText({input$text})
  
#Output Plot Tab 2----  
  #Plot 21 Disease
  output$diseaseplot <- renderPlotly({
    
    temp21 <- mortality_select %>% 
      filter(Disease_type %in% input$diseasselect) %>% filter(country == "World")
    
    p21 <- temp21 %>%      ggplot() +
      aes(x = year, y= `Disease (%)`, colour= Disease_type, group= Disease_type) + 
      geom_line() +
      labs(x="Jahre",
           y="Sterblichkeitsrate auf der Welt (%)",
           title= "Wie hat sich die Sterblichkeitsrate im Verlauf der Jahre verändert?",
           colour = "Krankheiten")+
       theme_minimal() 
      ggplotly(p21)
  })
  
  #Output Progess Box
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
      filter(country %in% input$countselect22) 
    
    temp22 %>%      ggplot(
      aes(x = country, y= `Disease (%)`, fill= Disease_type, group= Disease_type)) + 
      geom_col() +
      scale_fill_discrete() +
      labs(x="Länder",
           y="Anteile der Sterblichkeit nach Ländern (%)",
           fill = "Krankheiten")+
      coord_flip()
    
  })
  
  
#Output Plot Tab 3----
  
  ## (1) Daily caloric supply im Verlauf der Jahre
  
  
  output$caloricA2plot <- renderPlotly({
    
    temp31 <- supply %>%
      filter(Entity == c("Australia", "Brazil", "Central African Republic", "China", "Germany", "United Kingdom", "United States", "Sweden","Indonesia"))

    p31 <- temp31 %>% ggplot() + 
      aes(x = Year, y = `Daily caloric supply (kcal/person/day)`, color = Entity) + # x Achse bis 2013 anzeigen lassen?
      geom_line()+
      theme_minimal() 
    ggplotly(p31)
  })
  
  
  ## (2) BubbleChart: GDP per Capita und Daily per capita fat supply 

  output$gdp1plot <- renderPlotly({
  
  temp31 <- supply %>%
    filter(Year == "2013")
  
  p1 <- temp31 %>% ggplot() +
    aes(x = `GDP per capita (2011 international-$)`, y = `Daily per capita fat supply (grams per day) (g/person/day)`) +
    geom_point() +
    theme_minimal() 
  ggplotly(p1)
  })
  
  ## (2.1) BubbleChart: GDP per Capita und Daily per capita fat supply für die Länder USA, Deutschland und Central African Republik 
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
  #Output Progess Box
  
  # (1) Überschrift Differenzierung Übergewicht und Fettleibigkeit 
  
  output$totalbox6 <- renderInfoBox({
    infoBox(
      "Was ist der Unterschied zwischen Übergewicht und Fettleibigkeit (Adipositas)?", icon = icon("question"),
      color = "red"
           )
  })

  # (2) Output Vergleich Übergewicht/Fettleibigkeit
  output$überfettplot <- renderPlot({
    
    temp41 <- indicator %>%
      filter(Entity %in% input$countselect41) 
    
  temp41 %>% ggplot( 
        aes(x = Entity, y = indicator_total, fill = indicator_type, group = indicator_type)) + 
      geom_col() +
     scale_fill_discrete() +
      labs(x="Länder",
           y="Indikator: Body-Mass-Index (BMI)",
           title= "Vergleich von Übergewicht und Fettleibigkeit nach Ländern",
           fill = "Jahr")+
      coord_flip()
    
  })
  
  # (3) Entwicklung und Kontrast Übergewicht und Fettleibigkeit 
  
  # Übergewicht Frauen
  
  output$womenplot <- renderPlotly ({
    
    temp421 <- weight %>%
      filter(Entity == c("Central African Republic", "Germany", "United States")) 
     
    
   p421 <- temp421 %>%  ggplot() + 
      aes(y = `f_Overweight or Obese (%)`,x = Year, colour = Entity) +
        geom_line()
   ggplotly(p421)
   
  })
  
  # Übergewicht Männer
  output$manplot <- renderPlotly ({
    
    temp422 <- weight %>%
      filter(Entity == c("Central African Republic", "Germany", "United States")) 
    
   p422 <- temp422 %>% ggplot() +
      aes(y = `m_Overweight or Obese (%)`, x = Year, color = Entity) + 
      geom_line()
    ggplotly(p422)
  })
  
  # (2.1= Infoboxen einfügen: DEU 2014: 48,6 % Frauen übergewichtig, 64% Männer !!!
  
  # Infobox Frauen
  output$totalbox7 <- renderInfoBox({
    infoBox(
      "Frauen", paste0(25 + input$count,"48,6 %."), icon = icon("female"),
      color = "maroon"
    )
  })
  
  # Infobox Männer
  output$totalbox8 <- renderInfoBox({
    infoBox(
      "Männer", paste0(25 + input$count, "64 %"), icon = icon("male"),
      color = "aqua"
    )
  })
  

#Output Plot Tab 5----  
  output$fleischplot <- renderPlotly({
    
    temp51 <- meat %>% 
    filter(Entity == c("Central African Republic", "Germany", "United States")) %>% 
     filter(Entity %in% input$countselect51) %>% filter(`Food Balance Sheets: Meat - Food supply quantity (kg/capita/yr) (FAO (2017)) (kg)` != "2007")
    
    p51 <- temp51 %>%      ggplot(
      aes(x = Year, y = `Food Balance Sheets: Meat - Food supply quantity (kg/capita/yr) (FAO (2017)) (kg)`, group = Entity, colour = Entity)) + 
      geom_line() +
        labs(x="Jahre",
           y="Menge an Fleischkonsum pro Kopf und Jahr (kg)",
           colour = "Ausgewählte Länder")
    ggplotly(p51)
    
  })

#  output$totalbox5 <- renderInfoBox({
#    infoBox(
#      "Was fällt hier auf?", icon = icon("lightbulb"),
#      color = "green"
#    )
#  })  
  
  output$overweightplot <- renderPlotly({
    
    temp52 <- weight_comb %>% 
      filter(Entity == c("Central African Republic", "Germany", "United States")) %>% 
      filter(weight_type == "Overweight") %>% 
      filter(Entity %in% input$countselect52)
    
    p52 <- temp52 %>%      ggplot(
      aes(x = Year, y = weight_total, group = Entity, colour = Entity)) + 
      geom_line() +
      labs(x="Jahre",
           y="Anteil der Bevölkerung mit Übergewicht (%)",
           colour = "Ausgewählte Länder")
    ggplotly(p52)
    
  })



#Output Plot 6 ----
  
  output$text <- renderText({input$text})
   

}

shinyApp(ui, server)