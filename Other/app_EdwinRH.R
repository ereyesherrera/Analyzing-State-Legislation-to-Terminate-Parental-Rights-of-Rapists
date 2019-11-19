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
library(shiny)
library(naniar)

ca_fires <- read_csv("https://raw.githubusercontent.com/BuzzFeedNews/2018-07-wildfire-trends/master/data/calfire_frap.csv")
ca_damage <- read_csv("https://raw.githubusercontent.com/BuzzFeedNews/2018-07-wildfire-trends/master/data/calfire_damage.csv")

ca_fires <- ca_fires %>% 
  mutate(total_days = cont_date - alarm_date)

cause_names <- tibble(
  cause = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
  cause_name = c("Lightning", "Equipment Use", "Smoking", "Campfire", "Debris", "Railroad", "Arson", "Playing with Fire", "Miscellaneous", "Vehicle", "Power Line", "Firefighter Training", "Non-Firefighter Training", "Unknown/Unidentified", "Structure", "Aircraft", "Volcanic", "Escaped Prescribed Burn", "Illegal Alien Campfire")
)

# This uses info above to create new column in original data set to have name for each cause
ca_fires <- ca_fires %>%
  left_join(cause_names, by = "cause")

# Summarizing total acres a fire spanned and number of structures destroyed in year
fires <- ca_fires %>%
  group_by(year_) %>%
  summarize(total_acres = sum(gis_acres, na.rm = TRUE)) %>%
  left_join(ca_damage, by = c("year_" = "year")) %>%
  ungroup()

colors <- colors()

# Function to check to make sure color is in list of available colors
not_color <- function(input) {
  input <- tolower(input)
  if (!input %in% colors) {
    "Choose another color"
  } else if (input == "") {
    "Choose another color"
  } else {
    NULL
  }
}

ui <- fluidPage(
  h1("Visualizing Damage of CA Fires"),
  h3(tags$a(href = "https://github.com/BuzzFeedNews/2018-07-wildfire-trends", 
            "Click here to view data source!")),
  p("Note: Data on structures destroyed only available between 1989-2017"),
  sidebarLayout(sidebarPanel(sliderInput(inputId = "year", label = "Select Year Range:",
              min = 1950, max = 2017, value = c(2007, 2012), step = 1, ticks = FALSE, sep = ""), 
              textInput("color", "Choose Color of Plot:", value = "", placeholder = "Red"),
              submitButton(text = "Plot")),
    mainPanel(plotOutput(outputId = "acres_burned"),
  plotOutput(outputId = "structures")
)
)
)

server <- function(input, output) {
  output$acres_burned<- renderPlot({
    validate(
    not_color(input$color) #display error messages
  )

   fires %>%
      ggplot(aes(x = year_, y = total_acres)) +
      geom_point(color = tolower(input$color)) +
      geom_line(color = tolower(input$color)) + 
      labs(x = "Year", y = "Acres Burned", title = "Total Acres Burned From Fires in a Year",
           subtitle = "1950 through 2017") +
      scale_x_continuous(limits = input$year) +
      theme_minimal() +
     theme(plot.title = element_text(face = "bold", size = 15),
           plot.subtitle = element_text(face = "bold", size = 12))
  }
  )
  
  output$structures <- renderPlot({
    validate(
      not_color(input$color) # display error messages
    )
    fires %>%
      drop_na(structures) %>%
      ggplot(aes(x = year_, y = structures)) +
      geom_col(fill = tolower(input$color)) + 
      labs(x = "Year", y = "Number of Structures", title = "Total Stuctures Burned from Fires",
           subtitle = "1989 through 2017") +
      scale_x_continuous(limits = input$year + c(-1,1)) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold", size = 15),
            plot.subtitle = element_text(face = "bold", size = 12))
  }
  )
}

shinyApp(ui = ui, server = server)