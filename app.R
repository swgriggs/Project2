library(shiny)
library(RKaggle)

df <- RKaggle::get_dataset("valakhorasani/mobile-device-usage-and-user-behavior-dataset") |>
    as_tibble()

ui <- fluidPage(
    
    titlePanel(""),
    
    sidebarLayout(
        sidebarPanel(),
        mainPanel()
    )
)

server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)
