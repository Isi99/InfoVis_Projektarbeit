#Bibliotheken laden
library(shiny)
#library(shinydashboard)
library(tidyverse)
library(ggplot2)
#install.packages("plotly")
#install.packages("shinydashboard")
#install.packages("gapminder")
library(plotly)
library(gapminder)
#install.packages("png")
library(png)
#install.packages("shinyWidgets")
library(shinyWidgets)
#install.packages("shinydashboardPlus")
#library(shinydashboardPlus)

### Datensätze einlesen ----
  meat <- read_rds("meatnew.rds")
  mortality <- read_rds("mortality.rds")
  #indicator_weight <- read_rds("prevalence_overweight.rds")
  supply <- read_rds("supply.rds")
  weight <- read_rds("weight.rds")
  obes_region <- read_rds("obes_region.rds")
  weight_comb <- read_rds("weight_comb_new.rds")
  mortality_select <- readxl::read_xlsx("global_mortality_selection.xlsx")
  indicator <- read.csv("indicator.csv", sep = ";")
  #tab4_weight <- read_rds("tab4_weight.rds")
  tab4_weight <- read.csv("20190908_weight.csv", sep = ";")

#UI ----
# Define UI for application that draws a histogram

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Gesundheit 4.0"),
  dashboardSidebar(width = 250,
                   collapsed = TRUE,

#Menü einrichten----
                   sidebarMenu(
                     menuItem("Worum geht's?",tabName = "ta1",icon=icon("search")),
                     menuItem("Was passiert mit uns?",tabName = "ta2",icon=icon("chart-line")),
                     menuItem("Globale Ernährung",tabName = "ta3",icon=icon("fas fa-utensils")),
                     menuItem("Übergewicht und Fettleibigkeit",tabName = "ta4",icon=icon("weight")),
                     menuItem("Fleischkonsum",tabName = "ta5",icon=icon("bone")),
                     menuItem("In Form",tabName = "ta6",icon=icon("walking")),
                     menuItem("About this Project",tabName = "ta7",icon=icon("comment-alt"))
                   )  
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "header.css")
    ),
    tabItems(

      
#First tab content---- 
      tabItem(tabName = "ta1", 
              fluidRow(
                mainPanel(tags$img (src = "bacach2.png")),
               
                gradientBox(
                  width = 12,
                 # icon = "fa fa-th",
                  title = h2(p(strong ("Neue Ära der Gesundheit"))),
                  gradientColor = "green", 
                  boxToolSize = "sm", 
                  # footer = 
                  footer_padding = FALSE,
                  h4("Das erste Mal in der Weltgeschichte sterben mehr Menschen auf unserem Planeten an den Folgen von 
                    Fettleibigkeit als an Hunger. [Quelle: Yuval Noah Harari] Woran liegt das? Das BMLE hat sich im Projekt Gesundheit 4.0 
                  mit dieser Frage beschäftigt. Entdecke, wie sich die Entwicklung des Körpergewichts in den letzten Jahrzehnten 
                  verändert hat und welche Faktoren dabei eine Rolle spielen. Viel Spaß beim Stöbern und Entdecken. Dein BMLE")
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
                    main_img = "circle.png",
                    header_img = "header1.png",
                    front_title = "Was passiert mit uns?",
                    hr(),
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center",
                        h4("Was passiert eigentlich mit uns? Erstmals in der Geschichte
                        sterben mehr Menschen an den Folgen von Übergewicht, als dass sie verhungern.
                        Die beiden Grafiken sollen dies verdeutlichen. Die erste Grafik
                        zeigt, wie die Sterblichkeit im Verlauf der Jahre zugenommen hat.
                        Die zweite Grafik stellt die Sterblichkeitsrate der einzelnen Länder dar. 
                        Viel Spaß beim Explorieren!")
                        ),
                      plotOutput("distPlot")
                    )
                  )
              ),
            
                    gradientBox(
                      width = 12,
                      title = p(strong ("Sterblichkeitsrate im Überblick")),
                      icon = "fa fa-history",
                      gradientColor = "green", 
                      boxToolSize = "sm", 
                      footer = selectInput(
                        "diseasselect","Wähle eine Krankheit", 
                        choices = mortality_select$Disease_type, 
                        multiple = TRUE, selected = "Diabetes"
                      ),
                      h4("Schau dir an, wie sich die Sterblichkeitsrate auf der Welt im Hinblick auf 
                      die beiden Krankheiten, Diabetes (Engl.: Diabetes) und Herz-Kreislauf-Erkrankungen 
                         (Engl.: Cardiovascular-diseases), im Verlauf der Jahre geändert hat.")
                    ),

                  box(width="12",
                      plotlyOutput("diseaseplot")
                      ),

                    gradientBox(
                      width = 12,
                      title = p(strong("Auf einen Blick")),
                      icon = "fa fa-th",
                      gradientColor = "green",
                      footer_padding = FALSE,
                      boxToolSize = "sm",
                      h4("Auf unserer Erde leben 7,47 Milliarden Menschen. 
                      Knapp ein Drittel davon sterben jedes Jahr an einem Herzleiden.")
                    ),
                    
                infoBoxOutput("totalbox1", width = 6), 
                infoBoxOutput("totalbox2", width = 6),
                infoBoxOutput("totalbox3", width = 6),
                infoBoxOutput("totalbox4", width = 6),
                    
                    gradientBox(
                      width = 12,
                      title = p(strong("Länder im Kontrast")),
                      icon = "fa fa-globe",
                      gradientColor = "green", 
                      boxToolSize = "sm", 
                      footer = selectInput(
                        "countselect22","Select a Country",
                        choices = mortality_select$country,
                        multiple = TRUE, selected = c("Germany","United States")
                      ),
                      h4("Hier kannst du dir die Entwicklung der Sterblichkeitsraten 
                      anschauen und dabei mehrere Länder miteinander vergleichen.")
                    ),

                  box(width="12",
                      title="Entwicklung der Sterblichkeitsrate nach Ländern [2014]",
                      plotOutput("diseasecountplot")
                      )

            )  
      ),    
  

#Third tab content ----
      tabItem(tabName = "ta3",
              setShadow("card"),
              fluidRow(
          
          #Einleitungsbox      
                column(
                  width = 12,
                  align = "center",
                  flipBox(
                    id = 2,
                    main_img = "circle.png",
                    header_img = "header2.png",
                    front_title = "Ernährung im Wandel der Zeit",
                    hr(),
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center",
                        h4("Die Welt hat sich in den letzten Jahren stark verändert.
                         Die daraus resultierenden Folgen schlagen sich auch in unserem Essensverhalten wieder.
                         Während früher die Ernährung sehr abhängig von der Natur war und Mahlzeiten größtenteils zuhause verzehrt worden sind,
                         gibt es heutzutage eine große Anzahl an Auswahlmöglichkeiten von Lebensmitteln und der Alltag ist von Hektik geprägt, sodass man
                         auch mal schnell unterwegs eine Mahlzeit isst. Zudem haben die Menschen heute mehr Bewegungsmangel als früher.
                         Die folgende Grafik zeigt, wie sich die tägliche Kalorienversorgung im Laufe der Zeit sowie in unterschiedlichen Ländern entwickelt hat.")
                      ),
                      plotOutput("globalPlot")
                    )
                  )
                ),
            # Einleitung Kalorienverbrauch     
                gradientBox(
                  width = 12,
                  title = p(strong ("Kalorienversorgung im Überblick")),
                  icon = "fa fa-history",
                  gradientColor = "green", 
                  footer_padding = FALSE,
                  boxToolSize = "sm",
                  h4("Wie viel Kalorien nehmen wir eigentlich täglich zu uns? Und welches Land verbraucht am meisten Kalorien?
                   Schau dir an, wie sich die Kalorienversorgung in den Jahren von 1800 - 2013 verändert hat. ")
                ),
                  
                box(width = "12",
                      title = "",
                      plotlyOutput("caloricA2plot")
                        ),
                
                infoBoxOutput("totalbox9", width = 12),
                   
             # Einleitung Wohlstand und Kalorienverbrauch     
                gradientBox(
                  width = 12,
                  title = p(strong ("Wohlstand und Fettversorgung  - ein Zusammenhang?")),
                  icon = "fa fa-coins",
                  gradientColor = "green", 
                  footer_padding = FALSE,
                  boxToolSize = "sm",
                  h4("Welchen Einfluss hat der Wohlstand auf die tägliche Versorgung mit Nahrungsmitteln pro Kopf?
                  Während sich die Grafik oben auf die Kalorienversorgung bezieht, steht hier die tägliche Pro-Kopf-Fettversorgung
                  in Beziehung zu dem 'Gross Domestic Product (offizielle Abkürzung: GDP)'  in international-$ pro Kopf im Jahr 2011. Das Gross Domestic Product (GDP) 
                  entspricht dem deutschen Bruttoinlandsprodukt (BIP).")
                  ),
                 
                
                  box(width="12",
                      title = "",
                      plotlyOutput("gdp1plot")
                      ),
                
                gradientBox(
                  width = 12,
                  title = p(strong ("Zentralafrika, Deutschland und USA im Vergleich ")),
                  icon = "fa fa-globe",
                  gradientColor = "green", 
                  footer_padding = FALSE,
                  boxToolSize = "sm",
                  h4("Bestehen Unterschiede zwischen den Ländern? Schau dir den Zusammenhang 
                  in Ländern mit höherem Einkommen und ärmeren Regionen an.")
                     ),
              
                  box(width="12",
                      title="",
                      plotlyOutput("gdp2plot")    
                      )
             
                        )             
          ),        


#Fourth tab content ----
    tabItem(tabName = "ta4",
              fluidRow(
                
      #Einleitungsbox   
                column(
                  width = 12,
                  align = "center",
                  flipBox(
                    id = 3,
                    main_img = "circle.png",
                    header_img = "header3.png",
                    front_title = "Weltweite Verbreitung von Übergewicht und Adipositas",
                    hr(),
                    back_content = tagList(
                      column(
                        width = 12,
                        align = "center",
                        h4("Für die Beschreibung des Gesundheitszustandes einer Bevölkerung sind das
                         Körpergewicht und die Körpergröße wichtige Merkmale. In den letzten Jahren ist die Prävalenz (Häufigkeit einer Krankheit)
                         von Übergewicht und Adipositas (wird oft auch als 'Fettleibigkeit' übersetzt) stark angestiegen. Auf dieser Seite kannst 
                         du dir das Verhältnis von Übergewicht und Adipositas in ausgewählten Ländern anschauen.
                         Außerdem erfährst du mehr über die Häufigkeiten der beiden Krankheiten nach den Geschlechtern.")
                      ),
                      plotOutput("überundadiPlot")
          
                    )                       
                  )
                ),
                
      #Infobox Differenzierung Übergewicht und Fettleibigkeit
#                  infoBoxOutput("totalbox6", width = 12)
#                  ,
                 
      
      #SelectionInput Country für Plot1
                gradientBox(
                  width = 12,
                  title = p(strong ("Was ist der Unterschied zwischen Übergewicht und Adipositas?")),
                  icon = "fa fa-question",
                  gradientColor = "green", 
                  boxToolSize = "sm", 
                  footer = selectInput(
                   "countselect41", "Wähle ein Land aus:",
                    choices =indicator$Entity,
                    multiple = TRUE,
                    selected = c("Germany", "United States", "Central Africa Republic")
                 ),
                     
        h4("'Übergewicht und Adipositas (Fettleibigkeit) sind nicht dasselbe.
        Übergewicht bedeutet, dass der Betroffene über seinem Normalgewicht liegt. Es handelt sich dabei um den Übergang vom Normalgewicht zur Adipositas. 
        Der Begriff Adipositas bedeutet, dass jemand sehr starkes Übergewicht und dadurch einen krankhaft erhöhten Körperfettanteil hat. 
        Daher wird Adipositas auch Fettleibigkeit oder Fettsucht genannt [...].
        Zur Definition dient die Kennzahl des Body-Mass-Index (BMI).Ein BMI ab 25 kg/m2 gilt per Definition als Übergewicht, ein BMI von 30 kg/m2 und höher als Adipositas.'"),
        h5("[Quelle: https://www.vigo.de/rubriken/krankheit-und-therapie/stoffwechsel/lesen/uebergewicht-und-adipositas-volkskrankheiten.html]."),
        h4("Schau dir die Verteilung von Übergewicht und Adipositas in verschiedenen Ländern der Welt zu unterschiedlichen Zeitpunkten an.")
                            ),
      
      
      #Plot1 Entwicklung Übergewicht und Fettleibigkeit 1990 vs. 2016       
          
                   box(width = 12,
                    title = "",
                    plotOutput("überfettplot1975")
                    ),
                  
                   infoBoxOutput("totalboxüberfett", width = 12),
                   
                   box(width = 12,
                    title = "",
                    plotOutput("überfettplot2016")
                    ),
                   
  # Entwicklung nach Geschlechtern                 
                  gradientBox(
                    width = 12,
                      title = p(strong ("Prävalenz von Übergewicht und Adipositas nach Geschlechtern")),
                      icon = "fa fa-venus-mars",
                      gradientColor = "green",
                      footer_padding = FALSE,
                      boxToolSize = "sm", 
                      h4("Unterscheiden sich die Geschlechter hinsichtlich der zunehmenden Prävalenz von Übergewicht und Adipositas?
                       Im Jahr 2014 war fast die Hälfte der erwachsenen Frauen von Übergwicht oder Adipositas betroffen.
                       Bei den Männern waren es sogar über die Hälfte.")
                      ),
                    
    
      
                  infoBoxOutput("totalbox7", width = 6),
                  
      
                  infoBoxOutput("totalbox8", width = 6),

# Vergleich Männer und Frauen im Verlauf der Jahre
                  gradientBox(
                     width = 12,
                      title = p(strong ("Übergewicht und Adipositas im Verlauf der Jahre")),
                      icon = "fa fa-history",
                      gradientColor = "green", 
                      boxToolSize = "sm", 
                      footer_padding = FALSE,
                       h4("Schaue dir an, wie sich die Geschlechter im Hinblick auf die beiden Krankheiten im Verlauf der Jahre verändert haben.
                           Die sich verändernde Größe der Kreise für das jeweilige Land stellt zusätzlich die tägliche Kalorienversorgung in kcal pro Person/Tag dar.")
                      )
                  
              ),
      
      #Plot2 Vergleich Männer und Frauen Übergewicht/Fettleibigkeit

             splitLayout(
                 box(width = "12",
                   title = "Prävalenz von Übergewicht und Adipositas bei Frauen",
                   plotlyOutput("womenplot")
                     ),
                 
                 box(width = "12",
                  title = "Prävalenz von Übergewicht und Adipositas bei Männern",
                  plotlyOutput("manplot")
                )
              )
      
      #SliderInput Year für Plot2
#      fluidRow(
#        gradientBox(
#          width = 12,
#          title = "Übergewicht und Adipositas im Verlauf der Jahre",
#          icon = "fa fa-history",
#          gradientColor = "green", 
#          boxToolSize = "sm", 
#          footer = sliderInput(
#            "select_year", label ="Wähle einen Zeitraum:",
#            min = min(weight$Year),
#            max = max(weight$Year),
#            sep = "",
#            value = 2014),
#            " Schaue dir an, wie sich die Geschlechter im Hinblick auf die beiden Krankheiten im Verlauf der Jahre verändert haben."
#          )
        
      
              ),
           
    
            


#Fifth tab content----
    tabItem(tabName = "ta5",
            setShadow("card"),
            fluidRow(
              
              column(
                width = 12,
                align = "center",
                flipBox(
                  id = 4,
                  main_img = "circle.png",
                  header_img = "header4.png",
                  front_title = "Wie sich der Fleischkonsum verändert.",
                  back_title = "Fleischkonsum",
                  hr(),
                  back_content = tagList(
                    column(
                      width = 12,
                      align = "center",
                      h4("Der Fleischkonsum hat sich im Laufe der Jahre stark verändert. 
                      Besonders auffällig sind die Unterschiede zwischen wohlhabenderen 
                      Ländern und Entwicklungs- und Schwellenländern.")
                    ),
                    plotOutput("Plot2")
                    )
                  )
                  ),
              

                  gradientBox(
                    width = 12,
                    title = p(strong ("Fleischkonsum im Kontrast")),
                    icon = "fa fa-history",
                    gradientColor = "green", 
                    boxToolSize = "sm", 
                    footer_padding = FALSE,
                    h4("Wie viel Fleisch konsumieren wir eigentlich? 
                    Vergleiche den Fleischkonsum unterschiedlicher Länder miteinander.")
                  ),
                  
                box(width="12",
                    title = "",
                    plotlyOutput("fleischplot")
                ),

                  gradientBox(
                    width = 12,
                    title = p(strong ("Was fällt hier auf?")),
                    icon = "fas fa-lightbulb",
                    gradientColor = "green", 
                    boxToolSize = "sm", 
                    footer_padding = FALSE,
                      h4("Wir werden immer dicker! Woran kann das liegen? 
                     Hier kannst du den Anteil der übergewichtigen Bevölkerung 
                    der jeweiligen Länder miteinander vergleichen.")
                  ),
                  
                box(width="12",
                    title = "",
                    plotlyOutput("overweightplot")
                )

            )
    ),    
    
      
#Sixth tab contenct ----
    tabItem(tabName = "ta6",
            fluidRow(
            
           
            mainPanel(tags$img (src = "woman3.png")),
             
               
              gradientBox(
                width =  12,
                title = h2(p(strong ("Du willst mehr auf eine gesunde Ernährung achten? Dann IN FORM-ier dich hier!"))),
                #icon = "fa fa-th",
                gradientColor = "green", 
                footer_padding = FALSE,
                boxToolSize = "sm", 
                h4("Um mehr für die gesunde Ernährung und die eigene Gesundheit zu tun, wurde 'IN FORM' - Deutschlands Initiative für gesunde Ernährung ins Leben gerufen. Die Initative verfolgt
                 das Ziel, das Ernährungs- und Bewegungsverhalten in Deutschland bis zum Jahr 2020 nachhaltig zu verbessern. Es ist mehr als eine reine Übergewichtsprävention -
                 es geht um die Prävention von Fehlernährung, Bewegungsmangel und damit zusammenhängenden Krankheiten.
                 Sowohl Erwachsene als auch insbesondere
                 Kinder sollen lernen, gesünder zu leben und somit von einer erhöhten Lebensqualität
                 und einer gesteigerten Leistungsfähigkeit in allen Lebensbereichen profitieren zu können.
                 Der nationale Aktiosnplan 'IN FROM' unterstütze bereits über 200 Projekte. In Zukunft stehen vor allem die 
                 Etablierung geförderter Maßnahmen & Projekte, die Verarbeitung von neuen Erkentnissen sowie die Förderung zum Austausch & zur Vernetzung zwischen Akteuren
                 der unterschiedlichen Projekte im Fokus.")
                ),
             
            fluidRow(
              column(
                width = 12,
                align = "center",
                appButton(
                  url = "https://www.bmel.de/DE/Ernaehrung/GesundeErnaehrung/GesundeErnaehrung_node.html",
                  label = "In Form",
                  icon = "fa fa-seedling",
                  enable_badge = TRUE,
                  badgeColor = "purple",
                  badgeLabel = "Neu"
                ),
                appButton(
                  url = "https://frepai.shinyapps.io/milleniumdevelopmentgoals/",
                  label = "Weiterfühhrende Projekte",
                  icon = "fa fa-external-link-alt",
                  enable_badge = FALSE,
                  badgeColor = NULL,
                  badgeLabel = NULL
                ),
                appButton(
                  url = "https://www.dge.de/fileadmin/public/doc/fm/10-Regeln-der-DGE.pdf",
                  label = "Download",
                  icon = "fa fa-download",
                  enable_badge = FALSE,
                  badgeColor = NULL,
                  badgeLabel = NULL
                )
              )
              
              
 #         downloadButton("downloadData", "10 Regeln der DGE"),
                
#           actionLink("linkinfo", label = "Weitere Informationen rund ums Thema
 #                       'Gesunde Ernährung' sowie wie Rezeote und detallierte Projektbeschreibungen findest du hier.", icon=icon ("seedling")
#                     ),
              
#           actionLink("linkproject", label = "Ein weiteres spannendes Projekt zur Entwicklung der Ernährung findest du hier.", icon=icon ("external-link-alt"))
            
            )  
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
                title = p(strong ("Motivation")),
                icon = "fa fa-th",
                gradientColor = "green",
                footer_padding = FALSE,
                boxToolSize = "sm", 
                # footer = 
                h4("Ziel der App ist, dass junge Erwachsene und Familien über die Folgen von ungesunder Ernährung aufgeklärt werden.
                Dies soll der Prävention dienen und verdeutlichen, wie sich der Anteil an Übergewichtigen und Fettleibigen in
                unserer Gesellschaft im Verlaufe der Jahre verändert hat.")
              ),
              
              gradientBox(
                width = 12,
                title = p(strong ("Methodik")),
                icon = "fa fa-th",
                gradientColor = "green", 
                footer_padding = FALSE,
                boxToolSize = "sm", 
                # footer = 
                h4("Für dieses Projekt wurden mehrere Datensätze, die von der Plattform 'Our World in Data' stammen, einbezogen.
                Zu Beginn wurden 17 Datensätze miteinander verglichen. Diese befassten sich mit Themen wie Sterblichkeitsrate,
                Ernährung, Fettleibigkeit und Übergewicht, Fettkonsum, Fleischkonsum. Die Datensätze wurden bereinigt und 
                teilweise zusammengefügt, sodass das Projekt auf Basis von sechs unterschiedlichen Datensätzen beruht.")
              ),
              
              gradientBox(
                width = 12,
                title = p(strong ("Quellen, Urheberrechte & Lizenzen")),
                icon = "fa fa-th",
                gradientColor = "green", 
                footer_padding = FALSE,
                boxToolSize = "sm", 
                # footer = 
                h4("All unsere Daten sind von der Webseite 'Our World in Data'. 
                Diese Daten sind frei zugänglich und verwendbar, sofern die Nutzung angeben wird. 
                Alle Arbeiten sind unter der 'Creative Commons BY'-Lizenz' lizensiert.")
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
  #Plot 21 
  output$diseaseplot <- renderPlotly({
    
    temp21 <- mortality_select %>% 
      filter(Disease_type %in% input$diseasselect) %>% filter(country == "World")
    
    p21 <- temp21 %>%      ggplot() +
      aes(x = year, y= `Disease (%)`, colour= Disease_type, group= Disease_type) + 
      geom_line() +
      labs(x="Jahre",
           y="Sterblichkeitsrate auf der Welt (%)",
           title= "Wie hat sich die Sterblichkeitsrate im Verlauf der Jahre verändert?",
           colour = "Krankheiten",
           caption = "Diabetes = dt. Diabetes | Cardivascular diseases = dt. Herz-Kreislauf-Erkrankung") +
      scale_color_manual(values=c("#FE9A2E","#DF3A01"))+
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
      filter(country %in% input$countselect22) %>% filter(year == "2014")
    
    temp22 %>%      ggplot(
      aes(x = country, y= `Disease (%)`, fill= Disease_type, group= Disease_type)) + 
      geom_col() +
      scale_fill_discrete() +
      theme_minimal()+
      theme(text = element_text(size = 15)) +
      scale_fill_manual(values=c("#FF0000", "#ffa500"))+
      labs(x="Länder",
           y="Anteile der Sterblichkeit nach Ländern (%)",
           fill = "Krankheiten") +
     # scale_y_continuous(labels = scales::percent)+
      ylim(0,50)+
      coord_flip()
    
  })
  
  
#Output Plot Tab 3----
  
  ## (1) Abbildung: Entwicklung der Kalorienversorgung im Laufe der Zeit
  output$caloricA2plot <- renderPlotly({
    
    temp31 <- supply %>%
      filter(Entity == c("Australia", "Brazil", "Central African Republic", "China", "Germany", "United Kingdom", "United States", "Sweden","Indonesia"))

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
  
  
  ## (2) BubbleChart: Beeinflusst das GDP die Fettversorgung? (GDP per Capita und Daily per capita fat supply) 

  output$gdp1plot <- renderPlotly({
  
  temp31 <- supply %>%
    filter(Year == "2013")
  
  p1 <- temp31 %>% ggplot() +
    aes(x = `GDP per capita (2011 international-$)`, y = `Daily per capita fat supply (grams per day) (g/person/day)`) +
    geom_point() +
    theme_minimal() +
    labs(
      title = "Beeinflusst das GDP die Fettversorgung? [2013]",
      x = "GDP pro Kopf",
      y = "Tägliche Fettversorgung (in g pro Person/Tag)"
#     caption = "Die Daten stellen das Jahr 2013 dar."
        )
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
    scale_x_log10() +
    scale_color_manual(values=c("#5F4C0B", "#31B404", "#0404B4"))+
    labs(
      title = "Kontrast der täglichen Fettversorgung nach Wohlstand eines Landes",
      x = "GDP pro Kopf",
      y = "Tägliche Fettversorgung (in g pro Person/Tag)",
      color = "Länder nach Größe") + 
    guides(size = FALSE)
    
  ggplotly(p)
})
  
  output$totalbox9 <- renderInfoBox({
    infoBox(
      "Tipp!",
      paste0(25 + input$text, "Wähle einzelne Länder aus, dann kannst du sie besser miteinander vergleichen!"), 
      icon = icon("lightbulb"),
      color = "green"
    )
  })

#Output Plot Tab 4----
  #Output Progess Box
  
  # (1) Überschrift Differenzierung Übergewicht und Fettleibigkeit 
  
#  output$totalbox6 <- renderInfoBox({
#    infoBox(
#      "Frage:",
#      paste0(25 + input$text, "Was ist der Unterschied zwischen Übergewicht und Adipositas?"), icon = icon("question"),
#      color = "green"
#           )
#    
#  })

  # (2) Output Vergleich Übergewicht/Fettleibigkeit 1975
  output$überfettplot1975 <- renderPlot({
    
    temp41 <- indicator %>%
      filter(Entity %in% input$countselect41) %>%
      filter(Year == 1975)
    
  temp41 %>% ggplot( 
     aes(x = Entity, y = indicator_total, fill = indicator_type, group = indicator_type)) + 
     geom_col() +
     scale_fill_discrete() +
     theme_minimal() +
     theme(text = element_text(size = 15)) +
     scale_fill_manual(values = c("#00868B","#7FFFD4"))+
     labs(x="Länder",
           y="Indikator: Verbreitung von Übergewicht und Fettleibigkeit, gemessen am Body-Mass-Index (BMI) in %",
           title= "Vergleich von Übergewicht und Fettleibigkeit nach Ländern [1975]",
           fill = "Krankheitstyp",
           caption = "Der Kranheitstyp bedeutet Folgendes:
           indicator_obes = Adipositas (Fettleibigkeit) | indicator_over = Übergewicht") +
           coord_flip()
    
  })
  
  # Output Vergleich Übergewicht/Fettleibigkeit 2016
  
  output$überfettplot2016 <- renderPlot({
    
    temp411 <- indicator %>%
      filter(Entity %in% input$countselect41) %>%
      filter(Year == 2016)
    
    temp411 %>% ggplot( 
      aes(x = Entity, y = indicator_total, fill = indicator_type, group = indicator_type)) + 
      geom_col() +
      scale_fill_discrete() +
      theme_minimal() +
      theme(text = element_text(size = 15)) +
      scale_fill_manual(values = c("#00868B","#7FFFD4"))+
      labs(x="Länder",
           y="Indikator: Verbreitung von Übergewicht und Fettleibigkeit, gemessen am Body-Mass-Index (BMI) in %",
           title= "Vergleich von Übergewicht und Fettleibigkeit nach Ländern [2016]",
           fill = "Krankheitstyp",
           caption = "Der Kranheitstyp bedeutet Folgendes:
           indicator_obes = Adipostias (Fettleibigkeit) | indicator_over = Übergewicht") +
      coord_flip()
    
  })
  
  # Infobox Vergleich
  
  output$totalboxüberfett <- renderInfoBox({
    infoBox(
      "Frage:", paste0(25 + input$text,"Wie sieht die Verteilung 41 Jahre später aus?"), icon = icon("question"),
       color = "green"
    )
  })
  
  # (3) Entwicklung und Kontrast Übergewicht und Fettleibigkeit 
  
  # Übergewicht Frauen
  
 output$womenplot <- renderPlotly ({
    
  temp421 <- tab4_weight

# bisherige Visualsierung     
#   p421 <- temp421 %>%  ggplot() + 
#     aes(y = `f_Overweight or Obese (%)`,x = Year, colour = Entity) +
#     geom_line() +
#     theme_minimal() +
#     scale_color_manual(values=c("#5F4C0B",
#                                 "#31B404",
#                                 "#0404B4"))+
#      labs(
#       x = "Jahr",
#       y = "Übergewichtige/fettleibige Frauen (in %)",
#       color = "Ausgewählte Länder")
#   ggplotly(p421)
   
#   })
  
  # Test: Slider für Übergwicht Frauen Plotly Alternataive?
  

 p421 <- temp421 %>%
   plot_ly(
     x = ~ Year,
     y = ~ `f_Overweight.or.Obese....`,
     size = ~ `Daily.caloric.supply..per.person...kcal.person.day.`,
     color = ~ Entity,
     frame = ~ Year,
     text = ~ Entity,
     hoverinfo = "text",
     type = 'scatter',
     mode = 'markers'
   ) %>%
   layout(
     xaxis = list(
       type ="log",
       title = "Jahr"
     ),
     yaxis = list(
       title = "Übergewichtige/fettleibige Frauen (in %)"
     )
       )
#     text = list(
#     color = (values = c("#5F4C0B","#31B404", "#0404B4"))
#     )
#     )
#    marker = list (
#    colors = pal
#    )
     #color = list(
      # (values=c("#5F4C0B","#31B404", "#0404B4"))
     #)
#   )
ggplotly(p421)  

 })
 
# TEST TEST TEST 
# temp_421 <- reactive  ({
# filter(Year == input$select_year)
    
#    weight %>% na.omit() %>%
#      filter(weight$Year == input$select_year)
#      filter(weight$Entity == c("Central African Republic", "Germany", "United States")) 
#    
#  })
  
#  output$womenplot <- renderPlot ({
#    data_421() %>%
#     ggplot() + 
#      aes(y = `f_Overweight or Obese (%)`,x = Year, colour = Entity) +
#      geom_line() +
#      theme_minimal() +
#      scale_color_manual(values=c("#5F4C0B",
#      "#31B404",
#      "#0404B4"))+
#      labs(
#       x = "Jahr",
#       y = "Übergewichtige/fettleibige Frauen (in %)",
#       color = "Ausgewählte Länder")
    
# })
      
    

  
  # Übergewicht Männer
 output$manplot <- renderPlotly ({
   
   temp422 <- tab4_weight
   
 p422 <- temp422 %>%
   plot_ly(
     x = ~ Year,
     y = ~ `m_Overweight.or.Obese....`,
     size = ~ `Daily.caloric.supply..per.person...kcal.person.day.`,
     color = ~ Entity,
     frame = ~ Year,
     text = ~ Entity,
     hoverinfo = "text",
     type = 'scatter',
     mode = 'markers'
   ) %>%
   layout(
     xaxis = list(
       type ="log",
       title = "Jahr"
     ),
     yaxis = list(
       title = "Übergewichtige/fettleibige Männer (in %)"
     )
   )
     
# bisherige Visualisierung     
#  output$manplot <- renderPlotly ({
#    
#    temp422 <- weight %>%
#      filter(Entity == c("Central African Republic", "Germany", "United States")) 
#    
#   p422 <- temp422 %>% ggplot() +
#      aes(y = `m_Overweight or Obese (%)`, x = Year, color = Entity) + 
#      geom_line() +
#      theme_minimal() +
#      scale_color_manual(values=c("#5F4C0B",
#                                  "#31B404",
#                                  "#0404B4"))+
#      labs(
#       x = "Jahr",
#       y =  "Übergewichtige/fettleibige Männer (in %)",
#       color = "Ausgewählte Länder")
#    ggplotly(p422)
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
      filter(`Food Balance Sheets: Meat - Food supply quantity (kg/capita/yr) (FAO (2017)) (kg)` != "2007")
    
    p51 <- temp51 %>%      ggplot(
      aes(x = Year, y = `Food Balance Sheets: Meat - Food supply quantity (kg/capita/yr) (FAO (2017)) (kg)`, group = Entity, colour = Entity)) + 
      geom_line() +
      scale_color_manual(values=c("#5F4C0B",
                                  "#31B404",
                                  "#0404B4"))+
        labs(
          title = "Entwicklung der Menge an Fleischkonsum (Kilogramm pro Kopf im Jahr)",
           x="Jahre",
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
      filter(weight_type == "Overweight")  
 
    p52 <- temp52 %>%      ggplot(
      aes(x = Year, y = weight_total, group = Entity, colour = Entity)) + 
      geom_line() +
      scale_color_manual(values=c("#5F4C0B",
                                  "#31B404",
                                  "#0404B4"))+
      labs(
        title = "Übergewicht im Verlauf der Jahre",
          x="Jahre",
           y="Anteil der Bevölkerung mit Übergewicht (%)",
           colour = "Ausgewählte Länder")
    ggplotly(p52)
    
  })



#Output Plot 6 ----
  
  output$text <- renderText({input$text})
  
  output$downloadData <- downloadHandler (
    filename = "DGE-Ernährungsregeln.pdf",
    content = function(file) {
      file.copy("https://www.dge.de/fileadmin/public/doc/fm/10-Regeln-der-DGE.pdf", file)
    }
    
  ) 

}

shinyApp(ui, server)