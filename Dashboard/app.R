library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)

### Datensätze einlesen ----
meat <- read_rds("meatnew.rds")
mortality <- read_rds("mortality.rds")
indicator_weight <- read_rds("prevalence_overweight.rds")
supply <- read_rds("supply.rds")
weight <- read_rds("weight.rds")
obes_region <- read_rds("obes_region.rds")
weight_comb <- read_rds("weight_comb.rds")

#UI ----
# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "Worum geht's?"),
  dashboardSidebar(width = 250,
                   #Menü einrichten----
                   sidebarMenu(
                     menuItem("Home Page",tabName = "homepage",icon=icon("fas fa-globe")),
                     menuItem("Was passiert mit uns?",tabName = "ta1",icon=icon("question")),
                     menuItem("Unterernährung",tabName = "ta2",icon=icon("feather")),
                     menuItem("Globale Ernährung",tabName = "ta3",icon=icon("fas fa-utensils")),
                     menuItem("Übergewicht und Fettleibigkeit",tabName = "ta4",icon=icon("weight")),
                     menuItem("Home Page",tabName = "homepage",icon=icon("drumstick-bite")),
                     menuItem("About this Project",tabName = "aboutus",icon=icon("comment-alt"))
                   )  
  ),
  
  dashboardBody(
    tabItems(
      #First tab content---- 
      tabItem(tabName = "homepage",
              fluidRow(
                infoBoxOutput("totalbox", width=6), 
                infoBoxOutput("percentbox", width = 6),
                
                box(width = 12,
                    title = "Challenges for the Future",
                    status = "warning",
                    "Plastic pollution is a major and growing problem, negatively affecting oceans and wildlife health. This app provides an overview of mismanaged plastic waste of 148 countries in 2010. Furthermore, it discusses whether there is a correlation between the amount of plastic that is produced and the GDP of each country.")
              )
      ),
      
      #Second tab content ----
    
      
      #Third tab content----
      tabItem(tabName = "ta2",
              fluidRow(
                sidebarLayout(
                  sidebarPanel(
                    selectInput("countselect","Select a Country", choices = weight_comb$Year, multiple = TRUE, selected = "2013")
                  ),
                  box(width="12",
                      title="Übergewicht im Vergleich zu Unterernährung",
                      plotOutput("underplot")
                  )
                )
              )            
      )
      
      #Fourth tab content----
      
      #About this Project tab content----
      
   
    )
    
  )
)


#Server----
server <- function(input, output) { 
  
  #Output Menu ----
  output$menu <- renderMenu ({
    sidebarMenu(
      menuItem("Home Page",tabName = "homepage",icon=icon("fas fa-globe")),
      menuItem("Was passiert mit uns?",tabName = "ta1",icon=icon("question")),
      menuItem("Unterernährung",tabName = "ta2",icon=icon("feather")),
      menuItem("Globale Ernährung",tabName = "ta3",icon=icon("fas fa-utensils")),
      menuItem("Übergewicht und Fettleibigkeit",tabName = "ta4",icon=icon("weight")),
      menuItem("Home Page",tabName = "homepage",icon=icon("drumstick-bite")),
      menuItem("About this Project",tabName = "aboutus",icon=icon("comment-alt"))
    )  
  })
  
  #Output Text Homepage----
  output$text <- renderText({input$text})
  
  #Output Progess Box----      
  output$percentbox <- renderInfoBox({
    infoBox(
      "Mismanaged Waste", paste0(25 + input$count,"39,4%"), icon = icon("percent"),
      color = "purple"
    )
  })
  
  output$totalbox <- renderInfoBox({
    infoBox(
      "Total Waste per day", paste0(25 + input$count, "749.684t"), icon = icon("trash"),
      color = "red"
    )
  })
  
  #Output Plot Tab2 ----
  output$underplot <- renderPlot({
    
    temp <- weight_comb %>% 
      filter(Year %in% input$countselect)
    
    temp %>%      ggplot(
      aes(x = Year, y = weight_total, fill= weight_type, group = weight_type)) + 
      geom_col(position = "dodge2") +
      labs(x="Countries",
           y="Percentage of Females who are Underweight",
           title= "Comparison of Females who are Overweight or Obese and Underweight",
           fill = "Percentage of Females who are Overweight or Obese")+
      coord_flip()
  })
  
  #Output Plot Tab3----  

}
shinyApp(ui, server)