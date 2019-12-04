library(shiny)
library(gganimate)
library(ggmap)
library(ggridges)
library(ggthemes)
library(knitr)
library(leaflet)
library(lubridate)
library(plotly)
library(scales)
library(tidyverse)
library(tibble)
library(skimr)
library(naniar)
library(gridExtra)
library(sf)
library(htmltools)

data <- read_csv("https://raw.githubusercontent.com/ereyesherrera/stats112_finalproject/master/Honors_stats_updated_nov18_2.csv")

data <- data %>%
    select(-X22) %>%
    mutate(post_2015 = fct_collapse(post_2015, Yes = c("Yes", "yes")))

data <- data %>%
    mutate(party = case_when(perc_demo_senate < 50 & perc_demo_house < 50 ~ "Red",
                             perc_demo_senate > 50 & perc_demo_house > 50 ~ "Blue",
                             perc_demo_senate == 50 & perc_demo_house == 50 ~ "Purple",
                             perc_demo_senate <= 50 & perc_demo_house >= 50 ~ "Purple",
                             perc_demo_senate >= 50 & perc_demo_house <= 50 ~ "Purple")) 


shinyApp(
    
    ui <- fluidPage(
        h3(tags$u("Comparing Variables Across States")),
        p(tags$i("Check the boxes for states you would like to compare. \
       Uncheck the boxes to remove that state from the  table.")),
        sidebarLayout(sidebarPanel(
            checkboxGroupInput("choices", label = "States:", choices = c(data$State),
                               selected = c("Minnesota", "California"))),
            
            mainPanel(tableOutput(outputId = "table")
            ))),
    
    server <- function(input, output) {
        output$table = renderTable({
            data %>%
                mutate_all(as.character) %>%
                select(-bill_name) %>%
                filter(State %in% c(input$choices)) %>%
                pivot_longer(-State, names_to = "Variables", values_to = "Values") %>%
                pivot_wider(names_from = State, values_from = Values)
        })
    }
)