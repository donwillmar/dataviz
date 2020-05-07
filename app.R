# Packages ----
library(shiny)  # Required to run any Shiny app
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(ggvis)# The package where the data comes from

df <- read.table("Data/evoNBA.xlsx")
# ui.R ----
ui <- fluidPage(
  
  titlePanel("Mass Evolution of Players in the NBA 1950-2019"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "X-axis variable", choices = c("Year","cm", "kg", "bmi"),selected = "Year"),
      selectInput("yvar", "Y-axis variable", choices = c("cm", "kg", "bmi"), selected = "cm")
    ),
    ggvisOutput("plot")
  )
)

# server.R ----
server <- function(input, output, session) {
  vis <- reactive({
    
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    
    df %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(fill=~Zone, size= ~bmi, opacity:=0.8,
                   stroke := "black", strokeWidth := 0.75) %>%
      group_by(Zone)%>%
      add_legend("size", properties = legend_props(legend = list(y = 70)))%>%
      set_options(duration = 0)
  })
  
  vis %>% bind_shiny("plot")
}

# Run the app ----
shinyApp(ui = ui, server = server)

