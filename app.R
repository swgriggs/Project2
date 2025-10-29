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
        nav_panel("Data Exploration",
                  nav_panel("Data Exploration", navset_card_tab(
                    nav_panel(id = "contingency", title = "Contingency",
                              h3("Contingency Tables"),
                              fluidRow(column(width = 8,
                                 selectizeInput("cont_vars", "Select up to 2 variables",
                                                choices = cat_vars, multiple = TRUE,
                                                options = list(maxItems = 2))),
                                  column(width = 4,
                                         actionButton("reset_cont", "Reset"),
                                         actionButton("cont_button", "Generate Table")
                                  )
                              ),
                              DT::dataTableOutput("cont_table")
                    ),
                    nav_panel(id = "num_summary", title = "Summary",
                              h3("Numeric Summary"),
                              fluidRow(column(width = 8,
                                      selectizeInput("summ_cat", "Select a categorical variable",
                                                     choices = cat_vars, selected = character(0)),
                                      selectizeInput("summ_num", "Select a numeric variable",
                                                     choices = num_vars, selected = character(0))
                                  ),
                                  column(width = 4,
                                      actionButton("reset_summ", "Reset"),
                                      actionButton("summ_button", "Generate Summary")
                                  )
                              ),
                              DT::dataTableOutput("summ_table")
                    ),
                    nav_panel(id = "corr", title = "Correlation",
                              h3("Correlation Matrix"),
                              tableOutput("corr_table")
                    ),
                    nav_panel(id = "scatter", title = "Scatterplot",
                              h3("Scatterplot"),
                              fluidRow(column(8,
                                 selectizeInput("scatter_num", "Select 2 numeric variables",
                                                choices = num_vars, multiple = TRUE,
                                                options = list(maxItems = 2)),
                                 selectizeInput("scatter_cat", "Select a categorical variable",
                                                choices = cat_vars, selected = character(0))
                                 ),
                                 column(4,actionButton("reset_scatter", "Reset"),
                                        actionButton("scatter_button", "Generate Plot"))
                              ),
                              fluidRow(
                                  column(12, plotOutput("scatter_plot", height = "600px"))
                              )
                    ),
                    nav_panel(id = "boxplot", title = "Boxplot",
                              h3("Boxplot"),
                              fluidRow(column(8,
                                  selectizeInput("boxplot_num", "Select a numeric variable",
                                                 choices = num_vars, selected = character(0)),
                                  selectizeInput("boxplot_cat", "Select a categorical variable (and
                                                 a facetting varible if needed)", choices = cat_vars,
                                                 selected = character(0), multiple = TRUE,
                                                 options = list(maxItems = 2))
                              ),
                              column(4,actionButton("reset_boxplot", "Reset"),
                                     actionButton("boxplot_button", "Generate Boxplot"))
                              ),
                              fluidRow(
                                  column(12, plotOutput("boxplot_output", height = "600px"))
                              )
                    ),
                    nav_panel(id = "dens_scatter", title = "Density",
                              h3("Density Scatterplot"),
                              fluidRow(column(7,
                                  selectizeInput("density_num", "Select 2 numeric variables",
                                                 choices = num_vars, multiple = TRUE,
                                                 options = list(maxItems = 2)),
                                  selectizeInput("density_cat", "Select a categorical variable (and
                                                 a facetting varible if needed)",
                                                 choices = cat_vars, multiple = TRUE,
                                                 options = list(maxItems = 2)
                              ),
                              column(5,actionButton("reset_density", "Reset"),
                                     actionButton("density_button", "Generate Plot"))
                              ),
                              fluidRow(
                                  column(12, plotOutput("density_plot", height = "600px"))
                              )
                        )
                    )
                )
            )
        )
    ),
)

server <- function(input, output, session) {
    
    # Dynamic sliders for subsetting ranges of numeric variables
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
    
    # Subset action button
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
    
    # Data table for viewing subsetted data
    output$data_table <- DT::renderDataTable({
        DT::datatable(
            subset_data(),
            options = list(
                autoWidth = TRUE
            ),
            rownames = FALSE
        )
    })
    
    # Download button and handler for subsetted data
    output$download_csv <- downloadHandler(
        filename = function() {
            paste0("data-", Sys.Date(), ".csv")
        },
        content = function(file) {
            write.csv(subset_data(), file, row.names = FALSE)
        }
    )
    
    # Contingency table
    observeEvent(input$reset_cont, {
        updateSelectizeInput(session, "cont_vars", selected = character(0))
        cont_data(data.frame())
    })
    
    cont_data <- reactiveVal(data.frame())
    
    observeEvent(input$cont_button, {
        if (length(input$cont_vars) == 1) {
            out <- subset_data() |> dplyr::select(input$cont_vars[1]) |> table() |> as.data.frame()
        } else if (length(input$cont_vars) == 2) {
            out <- subset_data() |> dplyr::select(input$cont_vars[1], input$cont_vars[2]) |> table() |> as.data.frame.matrix()
        } else {
            out <- data.frame()
        }
        cont_data(out)
    })
    
    output$cont_table <- DT::renderDataTable({
        DT::datatable(cont_data())
    })
    
    # Numeric summary handling and output
    observeEvent(input$reset_summ, {
        updateSelectizeInput(session, "summ_cat", selected = character(0))
        updateSelectizeInput(session, "summ_num", selected = character(0))
        summ_data(data.frame())
    })

    summ_data <- reactiveVal(data.frame())

    observeEvent(input$summ_button, {
        if (length(input$summ_cat) > 0 && length(input$summ_num) > 0) {
            grp <- input$summ_cat[1]
            num <- input$summ_num[1]
            out <- subset_data() |>
                dplyr::group_by(.data[[grp]]) |>
                dplyr::summarise(
                    n = dplyr::n(),
                    mean = mean(.data[[num]]),
                    sd   = sd(.data[[num]]),
                    IQR  = IQR(.data[[num]]),
                    range = max(.data[[num]]) - min(.data[[num]]),
                    min  = min(.data[[num]]),
                    Q1   = quantile(.data[[num]], 0.25),
                    median = median(.data[[num]]),
                    Q3   = quantile(.data[[num]], 0.75),
                    max  = max(.data[[num]]),
                    .groups = "drop"
                )
        }
        else {
            out <- data.frame()
        }
        summ_data(out)
    })

    output$summ_table <- DT::renderDataTable({
        DT::datatable(summ_data())
    })
    
    # Correlation Matrix
    output$corr_table <- renderTable({
        subset_data() |> dplyr::select(all_of(num_vars)) |> cor()
    })
    
    # Scatter plot
    scatter_data <- reactiveVal(NULL)
    
    observeEvent(input$reset_scatter, {
        updateSelectizeInput(session, "scatter_num", selected = character(0))
        updateSelectizeInput(session, "scatter_cat", selected = character(0))
        scatter_data(NULL)
    })
    
    observeEvent(input$scatter_button, {
        if (length(input$scatter_num) == 2 && length(input$scatter_cat) == 1) {
            xvar <- input$scatter_num[1]
            yvar <- input$scatter_num[2]
            catvar <- input$scatter_cat[1]
            title <- paste0(xvar, " vs ", yvar, " by ", catvar)
            
            out <- subset_data() |> ggplot2::ggplot(ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]],
                    color = .data[[catvar]])) + ggplot2::geom_point() + ggplot2::labs(title = title)
        }
        else {
            out <- NULL
        }
        scatter_data(out)
    })
    
    output$scatter_plot <- renderPlot({
        scatter_data()
    })
    
    # Boxplot
    boxplot_data <- reactiveVal(NULL)
    
    observeEvent(input$reset_boxplot, {
        updateSelectizeInput(session, "boxplot_num", selected = character(0))
        updateSelectizeInput(session, "boxplot_cat", selected = character(0))
        boxplot_data(NULL)
    })
    
    observeEvent(input$boxplot_button, {
        if (length(input$boxplot_num) == 1 && length(input$boxplot_cat) >= 1) {
            xvar <- input$boxplot_cat[1]
            yvar <- input$boxplot_num[1]
            
            title <- paste0("Distribution of ", xvar, " by ", yvar)
            
            plt <- subset_data() |> ggplot2::ggplot(ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]])) + 
                ggplot2::geom_boxplot() + ggplot2::labs(title = title)
            
            if (length(input$boxplot_cat) == 2) {
                facetvar <- input$boxplot_cat[2]
                plt <- plt + ggplot2::facet_wrap(~ .data[[facetvar]])
            }
            
            out <- plt
        } else {
            out <- NULL
        }
        boxplot_data(out)
    })
    
    output$boxplot_output <- renderPlot({
        boxplot_data()
    })
    
    # Scatter Density Plot
    density_data <- reactiveVal(NULL)
    
    observeEvent(input$reset_density, {
        updateSelectizeInput(session, "density_num", selected = character(0))
        updateSelectizeInput(session, "density_cat", selected = character(0))
        density_data(NULL)
    })
    
    observeEvent(input$density_button, {
        if (length(input$density_num) == 2 && length(input$density_cat) >= 1) {
            xvar <- input$density_num[1]
            yvar <- input$density_num[2]
            catvar <- input$density_cat[1]
            title <- paste0("Probablity Density of ", xvar, " vs ", yvar, " by ", catvar)
            
            plt <- subset_data() |> ggplot2::ggplot(
                ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], fill = .data[[catvar]])) +
                ggdensity::geom_hdr() + ggplot2::geom_point(shape = 21) + ggplot2::labs()
            
            if (length(input$density_cat) == 2) {
                facetvar <- input$density_cat[2]
                plt <- plt + ggplot2::facet_wrap(~ .data[[facetvar]])
            }
            
            out <- plt
        }
        else {
            out <- NULL
        }
        density_data(out)
    })
    
    output$density_plot <- renderPlot({
        density_data()
    })
}

shinyApp(ui = ui, server = server)
