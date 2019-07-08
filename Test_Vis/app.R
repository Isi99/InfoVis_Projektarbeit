#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# 

#install.packages("gghighlight")

#Bibliotheken laden
library(shiny)
library(tidyverse)
library(gghighlight)
library(rsconnect)

#Daten laden (aktueller Datensatz)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
#install.packages("plotly")
#install.packages("gapminder")
library(gapminder)




student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv") 

country_names <- student_ratio$country %>% unique() %>% sort()
indicator_names <- student_ratio$indicator %>% unique() %>% sort()



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Teacher Ratio"),
  
  # Sidebar with a slider input for number of bins 
  tabsetPanel(
    tabPanel(
      "Teacher Ratio by Continent", 
      sidebarLayout(
        sidebarPanel(
          selectInput("totalVar", "Select the causes of death", choices = names(mortality))),
                     
          sliderInput("select_year", label="Select a year", 
                      min = min(student_ratio$year),
                      max = max(student_ratio$year), 
                      value = 2015),
          checkboxGroupInput("country_sel", label="Select the countries", choices = country_names, selected = c(country_names[1], "Germany"))
        ),
        mainPanel(wellPanel(
          plotOutput("mainplot")
        )
        )
      )
    ),
    tabPanel(
      "Teacher Ratio by Indicator", 
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("indicator_sel", label = "Select the indicators", choices = indicator_names, selected = c(indicator_names[1], "Primary Education"))
        ),
        mainPanel(wellPanel(
          plotOutput("mainplot2")
        )
        )
      )
      
    )
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$mainplot <- renderPlot({
    student_ratio %>%
      filter(country %in% input$country_sel) %>% 
      filter(year == input$select_year) %>% 
      ggplot() + aes_string(x = input$xachse, y = "student_ratio") + 
      geom_point() + 
      coord_flip()
    #gghighlight(student_ratio > 30) +
    # facet_wrap(indicator)
    
  })
  
  output$mainplot2 <- renderPlot({
    student_ratio %>%
      filter(indicator %in% input$indicator_sel) %>% 
      ggplot() + aes(x = indicator, y = student_ratio, fill = indicator)  + 
      geom_bar(stat = "identity") +
      theme_grey() + 
      facet_wrap(vars(year)) +
      coord_flip() +
      labs(title = "Klassengrößen in der Primarstufe am höchsten  ",
           subtitle = "Balkendiagramm der Schüler-/Lehrer-Verhälntnisse nach Bildungstand verteilt über die Jahre 2012-2018",
           x = "Bildungsstand",
           y = "Schüler/Lehrer-Verhältnis",
           fill = "Bildungsstand",
           caption = "UNESCO Institute of Statistics"
      ) +
      guides(fill = FALSE)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)



