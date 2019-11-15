#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(tibble)

# Define UI for application that draws a histogram


ca_fires <- read_csv("https://raw.githubusercontent.com/BuzzFeedNews/2018-07-wildfire-trends/master/data/calfire_frap.csv")
ca_damage <- read_csv("https://raw.githubusercontent.com/BuzzFeedNews/2018-07-wildfire-trends/master/data/calfire_damage.csv")
cause_names <- tibble(
    cause = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
    cause_name = c("Lightning", "Equipment Use", "Smoking", "Campfire", "Debris", 
                   "Railroad", "Arson", "Playing with Fire", "Miscellaneous", "Vehicle", 
                   "Power Line", "Firefighter Training", "Non-Firefighter Training", "Unknown/Unidentified", 
                   "Structure", "Aircraft", "Volcanic", "Escaped Prescribed Burn", "Illegal Alien Campfire"))
ca_fires <- ca_fires %>%
    left_join(cause_names, by = "cause")

 fires <- ca_fires %>%
    group_by(year_) %>%
    summarize(total_acres = sum(gis_acres, na.rm = TRUE)) %>%
    left_join(ca_damage, by = c("year_" = "year")) %>%
    arrange(desc(year_))

ui <- fluidPage(
    h1("Wildfires Ravaging California"),
    textInput("year1", "Pick a start year!", value = "", placeholder = "1950"),
    textInput("year2", "Pick an end year!", value = "", placeholder = "1950"),
    submitButton(text = "Create my plot!"),
    plotOutput(outputId = "timeplot")
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$timeplot<- renderPlot({
   ggplot(fires) +
            geom_line(aes(x= year_, y=total_acres)) +
        scale_x_continuous(limits = c(as.integer(input$year1), as.integer(input$year2))) +
            theme_minimal() +
            labs(x= "Year", y="Total Acres Burned")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
