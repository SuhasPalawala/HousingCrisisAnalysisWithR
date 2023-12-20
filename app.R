library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(gifski)

#Make functions for graphs
source("graphs.R")

ui <- navbarPage("The Housing Crisis",
  tabPanel("Introduction",
           h1("Introduction", align = "center"),
           p(HTML("Owning a home is a universal aspiration for most Americans, however the worrying trend of increasing house prices concerns many younger Americans who may never be able to reach such a goal. California and Washington are among the most expensive states when it comes to the cost of buying a house. While it is certainly possible to find an affordable house in both states, the vast majority of dwellings in the proximity of major cities, and therefore most jobs, come at a significant premium. Take Seattle and Los Angeles, the biggest cities of each respective state. As of September 2023, the median house price per square foot in Seattle was $584 while Los Angeles was at an eye watering $723. While this comparison may appear to show that Seattle is an affordable city to live in, the national median is a measly $222. These enormous disparities may be a factor in the high rates of homelessness in many major cities where home prices have skyrocketed.<br/><br/>
In AP Macroeconomics, we learned about inflation and just how expensive certain products are nowadays compared to what they cost decades ago. This inspired us to take a look at how the price of the most essential thing a person needs has increased over time, a home. While national house prices have data dating back to 1963, it was surprisingly difficult to find data on individual states going back more than 10 years. However, after doing further research and discovering how much houses have increased in price, we started to realize that this increase in price would be unjustifiable without accounting for increases in median household income. In order to examine this, the basis of our final project will be analyzing how median house prices have increased compared to how median household income has increased in each state.<br/><br/>
As students who go to college in the big city of Seattle, naturally, it is difficult not to notice the problem of homelessness in this city. Prices have gotten so expensive to a point where buying a house is not a realistic purchase for many people, which is a terrible situation to find oneself in. Studies have shown that affordable housing is increasingly becoming a problem in many parts of the United States, a problem that needs to be solved immediately. After realizing this, we wanted to add another component to our final project. We would like to analyze the rates of homelessness by state to determine if there is a correlation between the change in house prices/average income level and homelessness. In addition, we would like to integrate a feature in our website that allows the user to compare this correlation between states so they can see how this phenomenon varies across the country. We hope that this analysis will shed light on what adjustments need to be made in order to reduce problems such as the lack of affordable housing.
Additionally, this analysis could identify places where it would be wise to invest in a house, helping make that important long-term decision.")),
           tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                       src="Info_201_Data_Nutrition_Label.pdf")),
  
  tabPanel("Across the Nation",
           h1("Country-Wide Housing/Homelessness Analysis", align = "center"),
           p(HTML("When we look at the graph below, we can see that housing prices have clearly been increasing across the country at an astronomical rate. Over the past decade, this graph depicts a:"), align = "center"),
           p(HTML(paste(strong("106%"), em("increase in price for all residential properties"))), align = "center"),
           p(HTML(paste(strong("114%"), em("increase in price for condo/co-op properties"))), align = "center"),
           p(HTML(paste(strong("192%"), em("increase in price for multi-family (2-4 unit) properties"))), align = "center"),
           p(HTML(paste(strong("107%"), em("increase in price for single family residential properties"))), align = "center"),
           
           p(HTML("When accounting for inflation, these statistics become:"), align = "center"),
           p(HTML(paste(strong("66%"), em("increase in price for all residential properties"))), align = "center"),
           p(HTML(paste(strong("66%"), em("increase in price for condo-co-op properties"))), align = "center"),
           p(HTML(paste(strong("136%"), em("increase in price for multi-family (2-4 unit) properties"))), align = "center"),
           p(HTML(paste(strong("61%"), em("increase in single family residential properties"))), align = "center"),
           p(strong("Housing Prices for Different Types of Properties across the US")),
           p("Select different types of properties in the dropdown in order to examine how prices have changed over time for that type of home."),
           
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "country_HPrice_type", label = "Choose a type of property:", choices = list("All Residential", "Condo/Co-op", "Multi-Family (2-4 Unit)", "Single Family Residential")),
               checkboxInput(inputId = "country_HPrice_infla", label = "Inflation adjusted", value = FALSE),
               width = 2
             ),
             mainPanel(plotOutput(outputId = "country_HPrice"), align = "center", width = 10)
           ),
           
           p(HTML("<br/>As we look at how income levels have changed over the past decade, we immediately see one of the main problems in this housing crisis."), align = "center"),
           p(HTML("Income levels have not been increasing nearly as quickly as the prices of the aforementioned propoerties."), align = "center"),
           p(HTML(paste(em("Over the past decade, the average income level has only increased by"), strong("17%"), ".")), align = "center"),
           p(HTML("Every single property has increased significantly more than this, meaning that people's ability to afford housing has been consistently decreasing over the past decade."), align = "center"),
           p(strong("Average State Income Across the Country")),
           plotOutput(outputId = "country_income"),
           
           
           p(HTML("<br/>When we look at the graph below, we see a trend of decreasing homelessness across the United States in most age ranges. We see a:"), align = "center"),
           p(HTML(paste(strong("6%"), em("decrease in overall homelessness"))), align = "center"), 
           p(HTML(paste(strong("37%"), em("decrease in homelessness for people who are under 18 years old"))), align = "center"),
           p(HTML(paste(strong("41%"), em("decrease in homelessness for people who are between 18 and 24 years old"))), align = "center"),
           p(HTML(paste(strong("17%"), em("increase in homelessness for people who are over 24 years old"))), align = "center"),
           p(HTML("As we can see, efforts to decrease homelessness across the United States seem to have had a mostly positive impact on homelessness as we are clearly seeing a decrease in homelessness in the United States. However, this represents the country as a whole and it is entirely possible that different states have undergone different changes."), align = "center"),
           p(strong("Average Homelessness Counts Across the Country")),
           p("Select different age ranges in the dropdown in order to examine homelessness counts for that age group over the past decade."),
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "country_HLStat", label = "Choose a homelessness statistic:", choices = list("Overall homelessness", "Under 18", "18-24", "Over 24")), 
               width = 2
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput(outputId = "country_HLPlot")
                 ),
                 tabPanel("Table",
                          tableOutput(outputId = "country_HLTable")
                          )
               ), width = 10
             )
           ),
           p(HTML("<br/>")),
           p(strong("On the next page, we will analyze how housing prices and homelessness levels have changed in specific states. We will be able to compare statistics between states and observe any differences based on location."), align = "center"),
  ),
  
  tabPanel("Individual States",
           h1("State-Wide Housing/Homelessness Analysis", align = "center"),
           p(HTML("Here we can analyze housing and homelessness trends for each individual state in the United States."), align = "center"),
           p(HTML("For example, when we look at a state like California, the housing trend seems to match fairly well with the housing trend across the United States. We see an unfortunate consistent increase in pricing over the past decade."), align = "center"),
           p(HTML("However, when it comes to homelessness, overall homelessness actually increased quite a bit over the past decade, which is not consistent with the trend across the country. Evidently, different states have undergone significantly different changes that can be analyzed with the graphs and tables below."), align = "center"),
           
           selectInput(inputId = "state", label = "Choose a state to analyze:", choices = state.name),
           
           p(strong("Housing Prices for Different Types of Properties for Selected State")),
           p("Select different types of properties in the dropdown in order to examine how prices have changed over time for that type of home in the selected state."),
           
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "state_HPrice_type", label = "Choose a type of property:", choices = list("All Residential", "Condo/Co-op", "Multi-Family (2-4 Unit)", "Single Family Residential")),
               checkboxInput(inputId = "state_HPrice_infla", label = "Inflation adjusted", value = FALSE),
               width = 2
             ),
             mainPanel(
               plotOutput(outputId = "state_HPrice"),
               width = 10
             )
           ),
           
           p(HTML("<br/>")),
           p(strong("Average Income Over Time in Selected State")),
           plotOutput(outputId = "state_income"),
           
           p(HTML("<br/>")),
           p(strong("Homelessness Counts Over Time for Selected State")),
           p("Select different age ranges in the dropdown in order to examine homelessness counts for that age group over the past decade."),
           
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "state_HLStat", label = "Choose a homelessness statistic:", choices = list("Overall homelessness", "Under 18", "18-24", "Over 24")), 
               width = 2
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Plot",
                          plotOutput(outputId = "state_HLPlot")
                 ),
                 tabPanel("Table",
                          tableOutput(outputId = "state_HLTable")
                 )
               ), width = 10
              )
           ),
           p(HTML("<br/>"))
  ),
  tabPanel("Trends across regions",
           h1("Regional Housing/Homelessness Analysis", align = "center"),
           p(HTML("Here we can analyze trends across different regions of the United States."), align = "center"),
           p(HTML("We have already seen how some states, such as California, don't always follow the overall trend of the country. However, we can identify a more systematic pattern by looking at regional differences. Some parts of the country, such as the west coast, are known for their high cost of living, which includes the price of homes. However, they are also known for high minimum wages and high-paying jobs."), align = "center"),
           p(HTML("We can find out more about these two trends by looking at how home prices compare to income across different regions of the United States"), align = "center"),
           p(HTML("<br/>")),
           p(strong("Housing Prices for Different Types of Properties vs Median Income over Time by Region")),
           p(HTML("Select different types of properties in the dropdown in order to examine that type of home.")),
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "tran1_HPrice_type", label = "Choose a type of property:", choices = list("All Residential", "Condo/Co-op", "Multi-Family (2-4 Unit)", "Single Family Residential")),
               checkboxInput(inputId = "tran1_HPrice_infla", label = "Inflation adjusted", value = FALSE),
               width = 2
             ),
             mainPanel(imageOutput(outputId = "t1_plot", height = 600))
           ),
           p(HTML("<br/>")),
           p(HTML("As we can see in the graph above, regions tend to correlete with each other quite well. We can use this correlation to further compare regional changes in homelessness. Unfortunately, with the current trend over the past ten years, it appears homelessness is on the rise in the West and South as home prices grow explosively."), align = "center"),
           p(HTML("<br/>")),
           p(strong("Number of Homeless Individuals vs Home Prices over Time by Region")),
           p("Select different types of properties in the dropdown in order to examine that type of home and choose a homelessness statistic to compare against."),
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "tran2_HPrice_type", label = "Choose a type of property:", choices = list("All Residential", "Condo/Co-op", "Multi-Family (2-4 Unit)", "Single Family Residential")),
               checkboxInput(inputId = "tran2_HPrice_infla", label = "Inflation adjusted", value = FALSE),
               selectInput(inputId = "tran2_HLStat", label = "Choose a homelessness statistic:", choices = list("Overall homelessness", "Under 18", "18-24", "Over 24")),
               width = 2
             ),
             mainPanel(imageOutput(outputId = "t2_plot", height = 600))
           ),
           p(HTML("<br/>"))
  ),
  tabPanel("Summary",
           h1("In Conclusion...", align = "center"),
           p("Because the general trend of housing prices shows that housing prices are on the rise in virtually every state and homelessness levels have, on average, decreased across the country, we cannot establish a correlation between housing prices and homelessness. However, we can still make some observations based on the general trends that we see. Homelessness appears to be on the rise for people that are middle-age and up. This might suggest that things like education and targetting high-paying jobs early is of a higher priority among this generation's youth and people of the older generation may need more assistance in being able to afford housing. Additionally, income levels have not risen as much as housing prices, which means that if the general trend continues, we can expect more people in the future to not be able to comfortably afford housing.", align = "center"),
           p(HTML("<br/>")),
           p("This analysis can also be used to compare housing prices and trends between states. We hope this will be beneficial to future homeowners who would like to make sure they are making the best economic decision they can, whether they are trying to purchase a home or invest.", align = "center"),
           p(HTML("<br/>")),
           tags$img(style="height:800px; width:100%; scrolling=yes", 
                       src="house.png"),
           h2(strong("Sources"), align = "center"),
           p("Home Prices: https://www.redfin.com/news/data-center/"),
           p("Median Household Income of States: https://fred.stlouisfed.org/release/tables?eid=259515&rid=249"),
           p("Median Household Income of United States: https://fred.stlouisfed.org/series/MEHOINUSA672N"),
           p("Homelessness Statistics: https://www.huduser.gov/portal/datasets/ahar/2022-ahar-part-1-pit-estimates-of-homelessness-in-the-us.html"),
           p("Consumer Price Index: https://data.bls.gov/timeseries/CUUR0000SA0?years_option=all_years")
           ),
  
  tags$style(HTML("
            
            h1 {
              background-color: #b1cae7;
              color: Black;
              font-family: Copperplate;
            }
            
            p {
            font-family: Georgia;
            color: Black;
            font-size: 19px;
            }      
            
            .navbar-default {
            background-color: #3F3F6E;
            color: Black;
            font-family: Copperplate;
            }

            .navbar-default:hover {
            background-color: #aaaaaa;
            color: Black;
            }
            
            .navbar-default .navbar-brand {
                color: #cc3f3f;
            }
                  ")),
  
  setBackgroundColor(
    color = "#E6E5E5",
    gradient = c("linear", "radial"),
    direction = c("bottom", "top", "right", "left"),
    shinydashboard = FALSE
  )
  
)
  

server <- function(input, output){
  output$country_HPrice <- renderPlot({
    c_HPrice(input$country_HPrice_type, input$country_HPrice_infla)
  })
  output$country_income <- renderPlot({
    c_Income
  })
  output$country_HLPlot <- renderPlot({
    c_HLPlot(input$country_HLStat)
  })
  output$country_HLTable <- renderTable({
    c_HLTable(input$country_HLStat)
  })
  output$state_HPrice <- renderPlot({
    s_HPrice(input$state, input$state_HPrice_type, input$state_HPrice_infla)
  })
  output$state_income <- renderPlot({
    s_Income(input$state)
  })
  output$state_HLPlot <- renderPlot({
    s_HLPlot(input$state, input$state_HLStat)
  })
  output$state_HLTable <- renderTable({
    s_HLTable(input$state, input$state_HLStat)
  })
  
  output$t1_plot <- renderImage({
      outfile <- tempfile(fileext='.gif')
      
      p = tran_HPrice(input$tran1_HPrice_infla, input$tran1_HPrice_type)
      
      anim_save("outfile.gif", animate(p, height = 600, width = 1200, res = 120))
      
      list(src = "outfile.gif", contentType = 'image/gif')
      }, deleteFile = TRUE)
  
  output$t2_plot <- renderImage({
    outfile <- tempfile(fileext='.gif')
    
    p = tran_HL(input$tran2_HPrice_infla, input$tran2_HPrice_type, input$tran2_HLStat)

    anim_save("outfile2.gif", animate(p, height = 600, width = 1200, res = 120))
    
    list(src = "outfile2.gif", contentType = 'image/gif')
  }, deleteFile = TRUE)
}


shinyApp(ui = ui, server = server)