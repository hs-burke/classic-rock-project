#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(shinyWidgets)
library(fs)
library(shinythemes)
library(moderndive)
library(gt)
library(tidyverse)

data <- read.csv("athlete_events.csv")

row_data <- data %>%
    filter(Sport == "Rowing") %>%
    mutate(Medal = as.character(Medal))

row_data[is.na(row_data)] <- 0


countries_with_medals <- row_data %>%
    mutate(medal_id = case_when(
        Medal == "0" ~ 0,
        Medal == "Bronze" ~ 1,
        Medal == "Silver" ~ 2,
        Medal == "Gold" ~ 3
    )) %>%
    group_by(NOC) %>%
    summarize(medal_check = sum(medal_id)) %>%
    filter(medal_check > 0) %>%
    mutate(NOC = as.character(NOC))



# Define UI for application that draws a histogram
ui <- navbarPage(
    "Olympic Rowing since 1900",
    
    tabPanel(
        "Country",
        
        titlePanel("Number of Medals by Country"),
        
        sidebarLayout(sidebarPanel(
            pickerInput(
                "selectCountry",
                "Select Country",
                choices = countries_with_medals$NOC,
                options = list("actions-box" = TRUE),
                multiple = FALSE
            )
        ),
        
        # Main panel
        # Created two tabs, one for the plot and one for the model.
        
        mainPanel(
            tabsetPanel(
                id = "tabsMain",
                tabPanel("Plot",
                         plotlyOutput("plot1"))
            )
        ))
        
        
    ),
    
    tabPanel(
        "Model",
        
        titlePanel("Height and Result"),
        
        sidebarLayout(sidebarPanel(
            radioButtons("selectGender", h3("Select Gender"),
                         choices = list("Male" = "M",
                                        "Female" = "F"), selected = "M")
        ),
        
        # Main panel
        # Created two tabs, one for the plot and one for the model.
        
        mainPanel(tabsetPanel(
            id = "tabsMain",
            tabPanel("Model",
                     gt_output("model"))
        )))
        
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plot1 <- renderPlotly({
        plot1 <- row_data %>%
            filter(NOC %in% input$selectCountry) %>%
            group_by(Year, Event, NOC) %>%
            distinct(Event, .keep_all = TRUE) %>%
            add_count(Medal) %>%
            select(Year, NOC, Event, Medal, n) %>%
            pivot_wider(
                names_from = "Medal",
                values_from = "n",
                values_fill = list(n = 0)
            ) %>%
            group_by(Year) %>%
            mutate(ycount = Gold + Bronze + Silver) %>%
            group_by(Year) %>%
            summarize(
                Gold = sum(Gold),
                Bronze = sum(Bronze),
                Silver = sum(Silver),
                Total = sum(ycount)
            ) %>%
            pivot_longer(
                cols = c(Gold, Bronze, Silver, Total),
                names_to = "Medal",
                values_to = "nmedals"
            ) %>%
            ggplot(aes(
                x = Year,
                y = nmedals,
                color = Medal
            )) + ggtitle("The Number of Olympic Medals Won by Year") +
            labs(y = "Number of Medals") +
            geom_line(size = 1) +
            geom_point() +
            scale_color_manual(values = c("#cd7f32", "#d4af37", "#aaa9ad", "#000000")) +
            expand_limits(y = c(0, 15)) +
            theme_minimal()
        
        ggplotly(plot1)
    })
    
    
    output$model <- render_gt({
        model_data <- row_data %>%
            filter(Sex == input$selectGender) %>%
            filter(Height > 0) %>%
            mutate(
                medal_id = case_when(
                    Medal == "0" ~ 0,
                    Medal == "Bronze" ~ 1,
                    Medal == "Silver" ~ 2,
                    Medal == "Gold" ~ 3
                )
            ) %>%
            filter(medal_id > 0)
        
        
        model <-
            get_regression_table(lm(medal_id ~ Height, data = model_data))
        
        model %>%
            select(term, estimate, lower_ci, upper_ci) %>%
            mutate(term = c("Intercept", "Height")) %>%
            gt() %>%
            cols_label(
                term = "",
                estimate = "Coefficient",
                lower_ci = "5th percentile",
                upper_ci = "95th percentile"
            ) %>%
            fmt_number(columns = 2:4,
                       decimals = 3) %>%
            tab_header(title = "Height of Athlete and Podium Placement",
                       subtitle = "As height increases, likelihood of better podium placement increases")
        
    })
}

# Run the application
shinyApp(ui = ui, server = server)
