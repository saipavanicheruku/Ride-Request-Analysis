#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scales)
library(tidyverse)
library(ggplot2)
library(ggmap)
ride_data = read.csv('ride_requests.csv')

#Q1
ride_data$timestamp =
  as.Date(ride_data$timestamp)
ride_count_2021 = ride_data %>% 
  filter(timestamp >= "2021-05-01" & timestamp <= "2021-12-31")
ride_count <- ride_count_2021 %>% count(timestamp)
register_google(key = "*******")

#Q2
total_loc <- rbind(data.frame(loc=ride_data$drop_off_stop_name), data.frame(loc=ride_data$pick_up_stop_name))
loc_count <- total_loc %>% count(loc)
top_five = head(arrange(loc_count, desc(n)), 5)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Ride Request Analysis"),
    tabsetPanel(
      tabPanel("Question 1", textOutput("text1"), verbatimTextOutput("expr1"),plotOutput("plot1"),textOutput("text2"),plotOutput("plot2"),textOutput("text3")),
      tabPanel("Question 2", textOutput("text4"), verbatimTextOutput("expr2"),plotOutput("plot3")),
      tabPanel("Question 3", textOutput("text5"),plotOutput("plot4")))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #Question1
  output$text1 = renderText("--> Total ride requests that were made from May through December 2021")
  output$expr1 = renderPrint({

    sum(ride_count$n)
  })
  output$text2 = renderText("--> July to October time frame seems to have had consistent high number of ride requests as per the graph.
The pattern observed seems to be uniform in a way where the highest number of rides per day reached 250+ almost ~17 times in the time frame.
We can further check the pattern by splitting and plotting data from August month")
  output$plot1 = renderPlot({
    ggplot(ride_count, aes(x=timestamp, y=n)) +
      geom_line() + labs(title="Plot Of No Of Ride Requests Over Time",
                         x ="Timestamp", y = "No of ride requests (per day)")
  })
  output$text3 = renderText("--> Based on the pattern of August, we see the riders utilizing the vans mostly over weekdays and we see a downward trend over the weekends.
Additionally, July to October seems to be the peak season for riders as its spring and summer. This seems to gradually decline with the onset of winter. 
However, we would have to perform our analysis on a much larger data set to make this inference with confidence.
")
  output$plot2 = renderPlot({
    Aug = ride_data %>% 
      filter(timestamp >= "2021-08-01" & timestamp <= "2021-08-31")
    ride_count_aug <-
      Aug %>% count(timestamp)
    ggplot(ride_count_aug, aes(x=timestamp, y=n)) +
      geom_line()+ labs(title="Plot Of No Of Ride Requests Over Time (August)",
                        x ="Timestamp", y = "No of ride requests (per day)")
})
  #Question2
  output$text4 = renderText("The top 5 most popular stops in total")
  output$expr2 = renderPrint(top_five)
  output$plot3 = renderPlot({
    ride_data_nona <- ride_data %>%
      na.omit() 
    ride_filt <- ride_data_nona %>% 
      filter(drop_off_stop_name %in% top_five$loc) %>% 
      distinct(drop_off_stop_name, .keep_all = TRUE)
    nyc_map <- get_map(location = c(lon = -73.79561, lat = 40.68865), maptype = "terrain", zoom = 13)
    
    ggmap(nyc_map)
    ggmap(nyc_map) +
      geom_point(aes(drop_off_stop_long, drop_off_stop_lat), size=3.5, data = ride_filt) +
      labs(title="Geospatial plot of the top 5 stops",
             x ="Longitude", y = "Latitude")
  })
  #Question3
  output$text5 = renderText("Density plot of average number of days it takes each individual
rider to request their second ride after they requested their first")
  output$plot4 = renderPlot({
    ride_data <- ride_data[order(ride_data$timestamp),]
    
    new <- ride_data %>%
      group_by(rider_id) %>%
      mutate(diff = c(difftime(tail(timestamp, -1), head(timestamp, -1)),0))%>%
      mutate(diff = as.numeric(diff/86400))
    
    avg = new %>% 
      group_by(rider_id) %>%
      summarise(Avg_days = mean(diff))
    
    hist(avg$Avg_days,breaks = 25,labels=TRUE,main="Average no of days it takes for a rider to request next ride",xlab="Average no of days")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
