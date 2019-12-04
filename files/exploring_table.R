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
library(DT)

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

ui <- fluidPage(dataTableOutput("dataTable"))

server <- function(input, output) {
    
    output$dataTable <- DT::renderDataTable(
        data %>%
            mutate(State = factor(State), 
                   lgl_abortion_clinics = as.integer(lgl_abortion_clinics),
                   health_clinics = as.integer(health_clinics),
                   prop_equal_pay = as.integer(prop_equal_pay),
                   equal_pay_rank = as.integer(equal_pay_rank),
                   law_strength = as.integer(law_strength),
                   year_passage = as.integer(year_passage),
                   year_amend = as.integer(year_amend),
                   Senate = factor(Senate),
                   House = factor(House),
                   Governor = factor(Governor),
                   party = factor(party)), # data
        filter = "top" # location of column filters
    )
    
}

shinyApp(ui, server)