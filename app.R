#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(htmltools)
library(vembedr)
library(shiny)
library(shinyWidgets)
library(fs)
library(shinythemes)
library(moderndive)
library(gt)
library(tidyverse)
library(countrycode)
library(plotly)

source("data_prep.R")


#creating list for slider input
years <-
    big_data %>% distinct(Year) %>% arrange(Year) %>% select(Year)


#creating a numerical index for the type of medal each athlete received, and filtering out countries that have never won any medals
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

#The NOC value countries_with_medals is what I use for the pickerInput because i only contains countries that have medalled.
#Row_data still contains all countries, but the shinyapp will only allow you to select from the ones that have medalled.



# Define UI for application that draws the plots and the gt table
ui <- navbarPage("Exploring Olympic Rowing Success", theme = shinytheme("darkly"),
                 
                 # Page title
                 tabPanel(
                     "Plots",
                     
                     # Tab title
                     titlePanel("Medal Data"),
                     br(),
                     # Input for plot
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
                     # Created one tab for the plot.
                     
                     mainPanel(
                         tabsetPanel(
                             id = "tabsMain",
                             tabPanel("Number",
                                      plotlyOutput("plot1")),
                             tabPanel("GDP Per Capita Data",
                                      plotlyOutput("plot2"))
                         ),
                         br(),
                         p(paste("The first plot explores simple result data from individual countries for rowing at the Olympics since 1900. There are some points on the data that come with interesting historical commentary. For example, East Germany did phenomenally well in the 1980 Olympics, winning 14 medals. This was largely due to the boycott, although that wouldn't explain their similar performance in the 1976 Olympics, also finishing with 14 medals. Around the early-mid 70's, the East Germans had discovered that pushing as hard as possible for every session was not the best training methodology. They began 'steady state' training, aerobic work that requires less effort but occurs for a much longer duration. This was a massive breakthrough, and explains their dominance in that decade.")),
                         br(),
                         p(paste("The second plot looks at the relationship between a country's GDP Per Capita and the number of medals they won that year. Rowing is an expensive sport; the best eight-man shell will cost upwards of $100,000. As such, one would be expected to believe that the wealthier nations would perform better as a result of improved funding for equipment. GDP Per Capita might also indicate certain socio-economic conditions that result in a greater number of rowing clubs. More clubs mean a higher chance of attracting better athletes, ultimately resulting in better Olympic performances. Unfortunately, there is no data on the number of rowing clubs per country. Somewhat surprisingly, the relationship between medal count and GDP Per Capita varied wildly between countries. Some had strong positive correlations, others had strong negative correlations, and many had little to no correlation at all. I explored this relationship more in my model. I expected more consistency across countries, but there are obviously many other factors at play that are not accounted for in GDP"))
                     ))
                 ),

# Tab for the model
tabPanel("Model",
         
         # Title
         titlePanel("GDP Per Capita and Result"),
         
         # Side panel that contains radio buttons to select either male or female
         sidebarLayout(sidebarPanel(
             pickerInput(
                 "selectCountry1",
                 "Select Country",
                 choices = countries_with_medals$NOC,
                 options = list("actions-box" = TRUE),
                 multiple = FALSE
             )
         ),
         
         # Main panel that contains the model
         
         mainPanel(tabsetPanel(
             id = "tabsMain",
             tabPanel("Model",
                      gt_output("model_gdp")),
             br(),
             p(paste("This model is a continuation of the visualization on the previous page. The table is describing a linear relationship between GDP Per Capita and Number of Medals. Most countries show the number of medals expected when GDP is zero."))
         )))),

tabPanel("Table",
         
         # Title
         titlePanel("Table"),
         
         # Side panel that contains slider to select the year
         sidebarLayout(sidebarPanel(
             sliderTextInput(
             inputId = "selectYear",
             label = "Choose a Year:",
             choices = years$Year,
             grid = TRUE
         ),
         br(),
         p(paste("This table just shows the rankings by country for medals per capita. It's an interesting statistic, because you can see that there's not much of a relationship between population size and number of medals. New Zealand is consistently among the top, even though it's a very small country. They consistently perform comparatively well. One might think that population would be related to number of medals, because the more people to draw from, the higher the likelihood you get better athletes."))),
             # Main panel
             # Created one tab for the plot.
             
             mainPanel(
                 gt_output("medals_percap_table") # Printing table
             )
         )),

tabPanel("About",
         
         mainPanel(
             br(),
             p(paste("My goal in this project was to take a deeper dive into factors that influence rowing success at the Olympics. The stereotype of the sport is that it's elitist and only for the wealthy, so I made a point to look at the relationship between wealth metrics and rowing results. Furthermore, there is a surprising lack of interesting rowing data on the web, so even exploring within the dataset will uncover some interesting stories.")),
             br(),
             p(paste("I am mainly using a dataset I found on"), a(href = "https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results", "Kaggle"), paste("that includes every athlete that has competed in the olympics since 1900, some of their demographics, and their result. I also pulled population data from the World Bank, and gdp data from the World Bank. I had to do a significant amount of cleaning and transforming in order to get all of the information into one workable dataset.")),
             br(),
             p(paste("I, Harrison Burke, am a Sophomore at Harvard studying Philosophy with a secondary in Music. You can access the code for this project at my Github"), a(href = "https://github.com/hs-burke/classic-rock-project", "here.")),
             br(),
             p(embed_url("https://youtu.be/laWqKZYmsNI"))
         ))
)

# Define server logic
server <- function(input, output) {
    output$plot1 <- renderPlotly({
        plot1 <- row_data %>%
            group_by(Year, Event, NOC) %>%
            distinct(Event, .keep_all = TRUE) %>%       # Changing medals to per boat rather than per person
            add_count(Medal) %>%                        # Counting the medals
            select(Year, NOC, Event, Medal, n) %>%
            pivot_wider(
                # Expanding Medal column into columns for each medal type
                names_from = "Medal",
                values_from = "n",
                values_fill = list(n = 0)
            ) %>%
            filter(NOC %in% input$selectCountry) %>%    # Filters for the selected country
            mutate(ycount = Gold + Bronze + Silver) %>% # Calculating Medal totals
            group_by(Year) %>%
            summarize(
                # Summarizing Medal data by Year
                Gold = sum(Gold),
                Bronze = sum(Bronze),
                Silver = sum(Silver),
                Total = sum(ycount)
            ) %>%
            pivot_longer(
                # Bring Medal columns back into one column for plot
                cols = c(Gold, Bronze, Silver, Total),
                names_to = "Medal",
                values_to = "nmedals"
            ) %>%
            ggplot(aes(
                # Plotting data
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
    
    output$plot2 <- renderPlotly({
        
        plot2 <- model_data_gdp %>%
            filter(NOC == input$selectCountry) %>%
            ggplot(aes(x = gdp_percap, y = nmedals)) + geom_point() + geom_smooth(method = "lm", se = F) + ggtitle("The Relationship Between GDP and Number of Medals") +
            labs(y = "Number of Medals Won") +
            theme_minimal()
        
        ggplotly(plot2)
        
    })
    
    output$model_gdp <- render_gt({
        
        gdp_model <- model_data_gdp %>%
            filter(NOC == input$selectCountry1) #filtering for selected country
        
        model_gdp <-
            get_regression_table(lm(nmedals ~ gdp_percap, data = gdp_model)) # Creating model
        
        # Creating table
        model_gdp %>%
            select(term, estimate, lower_ci, upper_ci) %>%
            mutate(term = c("Intercept", "GDP")) %>%
            gt() %>%
            cols_label(
                term = "",
                estimate = "Coefficient",
                lower_ci = "5th percentile",
                upper_ci = "95th percentile"
            ) %>%
            fmt_number(columns = 2:4,
                       decimals = 3) %>%
            tab_header(title = "Country GDP Per Capita and Number of Medals Won",
                       subtitle = "The relationship between GDP per capita and medal count varies by country")
        
        
    })
    
    output$medals_percap_table <- render_gt({
        big_data %>%
            mutate(NOC = countrycode(NOC, origin = "ioc", destination = "country.name")) %>% # Reformatting 
            mutate(gdp_percap = as.double(gdp_percap)) %>%
            group_by(Year, Event, NOC, total_pop) %>%
            distinct(Event, .keep_all = TRUE) %>%
            add_count(Medal) %>%
            pivot_wider(
                names_from = "Medal",
                values_from = "n",
                values_fill = list(n = 0)
            ) %>%
            filter(Year == input$selectYear) %>% # Filtering for year
            group_by(NOC, total_pop) %>%
            summarize(total = sum(Gold, Bronze, Silver)) %>%
            ungroup() %>%
            mutate(medals_per_mil = (total / total_pop) * 1000000) %>%
            select(NOC, medals_per_mil) %>%
            arrange(desc(medals_per_mil)) %>% # Arrange so highest medals per million is at top
            gt() %>% # Make gt table
            cols_label(NOC = "Country",
                       medals_per_mil = "# of Medals per Million People")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
