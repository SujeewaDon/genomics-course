#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(maps)
library(ggplot2)
library(lubridate)
library(wesanderson)





report_09_26_2020 <-   read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>% 
    rename(Province_State = "Province/State") %>%
    rename(Country_Region = "Country/Region") %>% 
    filter(Country_Region!="US")

us_09_26_2020 <-   read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")) %>% 
    rename(Long="Long_")

report_09_26_2020 <- report_09_26_2020 %>% 
    select(intersect(colnames(report_09_26_2020), colnames(us_09_26_2020)))

us_09_26_2020 <- us_09_26_2020 %>% 
    select(intersect(colnames(report_09_26_2020), colnames(us_09_26_2020)))

analyzed_df <-bind_rows(list(report_09_26_2020, us_09_26_2020))

analyzed_df<- analyzed_df %>% 
    pivot_longer(cols = -c("Province_State", "Country_Region", "Lat", "Long"), names_to="date", values_to="value") %>%
    mutate(Date = mdy(date))





min_date <- min(analyzed_df$Date, na.rm = TRUE)
max_date <- max(analyzed_df$Date, na.rm = TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(verticalLayout(

    # Application title
    titlePanel("Number of Confirmed COVID-19 Cases"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("curr_date",
                        "Select Date",
                        min = min_date,
                        max = max_date,
                        value = min_date,
                        step = 1)
        ),

        # Show a plot of the generated distribution
         mainPanel(
           plotOutput("distPlot")
        )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        world<- map_data("world")
        curr_df <- analyzed_df %>% filter(Date==input$curr_date, value!=0)
        
        p<-ggplot(data=curr_df, mapping = aes(x=Long, y=Lat))+
            coord_fixed(ratio= 1.3)+
            borders(database="world", colour = "grey", fill="NA")+
            geom_point(aes(size= value/1000, color=value), show.legend = FALSE)+
            scale_colour_gradientn(colours = wes_palette(name="Royal1", n=50, type = "continuous"),
                                 n.breaks=10, trans="log10")+
            #geom_polygon(fill=NA, colour="grey")+
            #geom_polygon(data=us,fill=NA, colour="grey")+
            #ggtitle("Number of Confirmed Cases")+
            theme_classic()+
            theme(axis.line = element_blank(),
                  axis.text = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank())
        print(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
