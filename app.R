library(shiny)
library(bslib)
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
        nav_panel("About",
                  h3("Application"),
                  tags$p("This web application lets you explore the Mobile Device Usage dataset by
                         Vala Khorasani. The main panel is used to select the analysis or plot type
                         and adjust its visual appearance: create contingency tables for one or two
                         categorical variables; produce numerical summaries or a correlation matrix
                         for numeric variables; and generate scatterplots, boxplots, and density
                         scatterplots. The left side panel is for choosing which variables to
                         summarize: select numeric variables and subset them by range, or select
                         categorical variables and subset them by any combination of their values.
                         The About tab gives general information about the application. The Data
                         Download tab shows the current raw data (including any subsets) as a table
                         and lets you download it as a CSV file. The Data Exploration tab is
                         dedicated to exploring the data with numeric summaries, plots, and tables."
                         ),
                  h3("Data Set"),
                  tags$p("The Mobile Device Usage dataset by Vala Khorasani is hosted at",
                        a("https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset",
                        href = "https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset",
                        target = "_blank")
                  ),
                  tags$p("It contains 700 simulated observations generated from five predefined
                         user-behavior classes, which determine the values of the numeric variables
                         that describe mobile device usage characteristics. The dataset is available
                         on the Kaggle platform, commonly used for data-science competitions; its
                         intended purpose is to help users develop and test predictive models.
                         Because the data are highly synthetic, it's not a reliable source for
                         scientific or professional applications. There are 10 variables in addition
                         to the user ID (the identifier for each user): four categorical and six
                         numeric. The following variable descriptions were taken from the dataset’s
                         Kaggle page:"
                        ),
                  tags$ul(
                      tags$li("User ID: Unique identifier for each user."),
                      tags$li("Device Model: Model of the user's smartphone."),
                      tags$li("Operating System: The OS of the device (iOS or Android)."),
                      tags$li("App Usage Time: Daily time spent on mobile applications, measured in minutes."),
                      tags$li("Screen On Time: Average hours per day the screen is active."),
                      tags$li("Battery Drain: Daily battery consumption in mAh."),
                      tags$li("Number of Apps Installed: Total apps available on the device."),
                      tags$li("Data Usage: Daily mobile data consumption in megabytes."),
                      tags$li("Age: Age of the user."),
                      tags$li("Gender: Gender of the user (Male or Female)."),
                      tags$li("User Behavior Class: Classification of user behavior based on usage patterns (1 to 5)."),
                    ),
                  img(src = "dataset-cover.jpeg", height = "300px", width = "600px")
                ),
        nav_panel("Data Download", "Page 2 content"),
        nav_panel("Data Exploration", "Page 3 content")
    ),
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
