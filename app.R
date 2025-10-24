library(shiny)
library(shinyWidgets)
library(RKaggle)

df <- RKaggle::get_dataset("valakhorasani/mobile-device-usage-and-user-behavior-dataset") |>
    dplyr::as_tibble()

cat_vars <- c("Device Model", "Operating System", "Gender", "User Behavior Class")
num_vars <- c("App Usage Time" = "App Usage Time (min/day)",
              "Screen On Time" = "Screen On Time (hours/day)",
              "Battery Drain" = "Battery Drain (mAh/day)",
              "Numer of Apps Installed" = "Number of Apps Installed",
              "Data Usage" = "Data Usage (MB/day)",
              "Age" = "Age")

ui <- fluidPage(
    navset_card_tab(
        sidebar = sidebar("Sidebar",
            checkboxGroupInput(
                inputId = "device",
                label = "Device Model",
                choices = unique(df$`Device Model`)
            ),
            checkboxGroupInput(
                inputId = "os",
                label = "Operating System",
                choices = unique(df$`Operating System`)
            ),
            checkboxGroupInput(
                inputId = "gender",
                label = "Gender",
                choices = unique(df$`Gender`)
            ),
            checkboxGroupInput(
                inputId = "user_class",
                label = "User Behavior Class",
                choices = unique(df$`User Behavior Class`)
            ),
            pickerInput(
                inputId = "num_vars",
                label = "Numeric Variables",
                choices = num_vars,
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
            ),
            uiOutput(outputId = "num_sliders"),
            actionButton("subset", "Subset Data")
        ),
        nav_panel("About", "Page 1 content"),
        nav_panel("Data Download", "Page 2 content"),
        nav_panel("Data Exploration", "Page 3 content")
    ),
    mainPanel(
        verbatimTextOutput("Put output Here")
    )
)

server <- function(input, output, session) {
    output$num_sliders <- renderUI({
        req(input$num_vars)
        sliders <- lapply(input$num_vars, function(var) {
            rng <- range(df[[var]], na.rm = TRUE)
            sliderInput(
                inputId = paste0("slider_", var),
                label = var,
                min = rng[1],
                max = rng[2],
                value = rng,
                step = (rng[2] - rng[1]) / 100
            )
        })
        do.call(tagList, sliders)
    })
    # Subsetted data
    subset_data <- reactiveVal(df)
    
    # Observe action button
    #observeEvent(input$subset, {
        #req(input$device | input$os | input$os | input$user_class | device$num_vars)
        # Need to subset data based on variable selection
        #new_data <- df |>
        #subset_data(new_data)
    #})
    
    # Put output here
    #output$outputElement <- render*({
    #    subset_data()
    #})
}

shinyApp(ui = ui, server = server)
