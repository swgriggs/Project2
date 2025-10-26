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
            pickerInput(
                inputId = "device",
                label = "Device Model",
                choices = unique(df$`Device Model`),
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
            ),
            pickerInput(
                inputId = "os",
                label = "Operating System",
                choices = unique(df$`Operating System`),
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
            ),
            pickerInput(
                inputId = "gender",
                label = "Gender",
                choices = unique(df$`Gender`),
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
            ),
            pickerInput(
                inputId = "user_class",
                label = "User Behavior Class",
                choices = unique(df$`User Behavior Class`),
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
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
        nav_panel("Data Download",
                  div(style = "height:700px",
                      DT::dataTableOutput("data_table")),
                  downloadButton("download_csv", "Download CSV"),
                  ),
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
    observeEvent(input$subset, {
        df_temp <- df
        
        if (!is.null(input$device) && length(input$device) > 0) {
            df_temp <- df_temp |> dplyr::filter(`Device Model` %in% input$device)
        }
        if (!is.null(input$os) && length(input$os) > 0) {
            df_temp <- df_temp |> dplyr::filter(`Operating System` %in% input$os)
        }
        if (!is.null(input$gender) && length(input$gender) > 0) {
            df_temp <- df_temp |> dplyr::filter(`Gender` %in% input$gender)
        }
        if (!is.null(input$user_class) && length(input$user_class) > 0) {
            df_temp <- df_temp |> dplyr::filter(`User Behavior Class` %in% input$user_class)
        }
        
        if (!is.null(input$num_vars) && length(input$num_vars) > 0) {
            for (var in input$num_vars) {
                rng <- input[[paste0("slider_", var)]]
                if (!is.null(rng) && length(rng) == 2) {
                    df_temp <- df_temp |> dplyr::filter(.data[[var]] >= rng[1], .data[[var]] <= rng[2])
                }
            }
        }
        subset_data(df_temp)
    })
    
    output$data_table <- DT::renderDataTable({
        DT::datatable(
            subset_data(),
            options = list(
                autoWidth = TRUE
            ),
            rownames = FALSE
        )
    })
    
    output$download_csv <- downloadHandler(
        filename = function() {
            paste0("data-", Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv(subset_data(), file, row.names = FALSE)
        }
    )

}

shinyApp(ui = ui, server = server)
