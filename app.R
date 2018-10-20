#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plyr)         # Data manipulation
library(dplyr)        # Data manipulation
library(ggplot2)      # Data visualization
library(plotly)       # Dynamic data visualization
library(caTools)      # Setting seeds
library(rpart)        # Decision Trees
library(randomForest) # Random Forest
library(maps)         # World Map
library(countrycode)  # Gets country code 
library(leaflet)      # Interactive maps
library(corrplot)     # Nice correlation matrix
library(RColorBrewer)
library(mapproj)
source("DataCleaning.R")

# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "WSDS Hackathon - Laurels for Learning"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics", tabName = "Demographics", icon = icon("dashboard")),
      menuItem("Strategy", tabName = NULL, icon = icon("bars"),
               menuSubItem("Volunteers and Donators", tabName = "TrendsVolunteers", icon = icon("stack-overflow")),
               menuSubItem("Music Specific Volunteers", tabName = "MusicVolunteers", icon = icon("sign-language")),
               menuSubItem("Partnership Opportunity", tabName = "peerAnalysis", icon = icon("users"))
      ),
      menuItem("Summary", tabName = "Summary", icon = icon("slideshare"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems( 
      tabItem(tabName = "Demographics",
              flipBox(
                id = 1,
                main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                front_title = "Potential Volunteers",
                back_title = "Potential Students",
                "We're looking to tap in the vast pool of graduate students to act as volunteers. 
                Here, we have a heat map showing the density of colleges per state. 
                In order to get college graduates to work as volunteers with Laurels for Learning, 
                we should look at reaching out to them during the first few weeks of their academic year. 
                Laurels for Learning can use social media to reach out to prospective volunteers as well 
                as interact with the student organizations in these colleges. We have used the following 
                publicly available dataset for this visualization (titled 'College Scorecard'):",
                shiny::tags$a(href="https://catalog.data.gov/dataset?tags=college", "Click here for the Volunteers dataset"),
                
                
                plotOutput("plot1"),
                back_content = tagList(
                  "Here we have shown the state-wide child poverty level estimates. We can look at states 
                  that have higher child poverty rates and specifically encourage students from those areas
                  to participate in Laurels for Learning program. We have used the following publicly 
                  available dataset for this visualization - ",
                  shiny::tags$a(href="https://www.census.gov/data/tables/2018/demo/income-poverty/state-level-child-poverty.html", "Click here for the Students dataset"),
                  
                  column(
                    width = 12,
                    align = "center",
                    plotOutput("plot2")
                  )
                )
                , height = 500)
      ),
      tabItem(tabName = "TrendsVolunteers",
              box(plotOutput("plot4")),
              box(imageOutput("TrendsVolunteers")),
              h3("Volunteering in America: "),
              h4("This dataset provides the most comprehensive look at volunteering and civic life in the 50 states and 51 cities across the country."),
              h4("The number of people volunteering across the US was the highest in 2004."), 
              h4("It fell sharply after that in 2007 but has been on the rise ever since."),
                  shiny::tags$a(href="https://www.nationalservice.gov/vcla", "View the full report here"),"and",
                  shiny::tags$a(href="https://data.nationalservice.gov/Volunteering-and-Civic-Engagement/Volunteering-and-Civic-Life-in-America/spx3-tt2b/data
                                ", "Snapshot of Volunteering and Civic Life in America")
                  ),
      
      tabItem(tabName = "MusicVolunteers",
              box(plotOutput("plot3")),
              box(
                  h3("Music Schools:"),
                  h4("With one of Laurels for Learning's areas of instruction being music, we have looked at all the music schools in the country 
                      and the number of people graduating as music majors from these schools yearly."), 
                  h4("We can reach out to these schools via student organizations to spread more awareness about Laurels for Learning."),
                  h4("From the map, we can see that California and the northeast have a higher number of music grads."), 
                  h4("Laurels for learning can approach these students and see if they're available for
                  volunterring online to teach students in the other parts of the country.
                  We have used the following publicly available dataset for this visualization:"),
                  shiny::tags$a(href="https://api.datausa.io/api/?sort=desc&sumlevel=puma&cip=5009&show=geo&year=all&required=avg_wage%2Cavg_wage_moe%2Cnum_ppl%2Cnum_ppl_moe&where=num_records%3A%3E4&order=avg_wage
                                ", "Music Majors Data"))),
      
      tabItem(tabName = "peerAnalysis",
              imageOutput("peerAnalysis"), align = "center",
              h3("Partnership Opportunity? - VH1 Save the music"),
              h4("The VH1 Save The Music Foundation is a non-profit organization dedicated to restoring instrumental music education in America's public schools, 
                 and raising awareness about the importance of music as part of each child's complete education."),
              h4("The VH1 Save The Music Foundation is dedicated to restoring instrumental music programs to ensure that all children have access to a complete education."),
              h4(" If a need for instrumental music education exists, they look to the superintendent and school board for a commitment to restore music in every school within a district. 
                 With a common goal of fully rebuilding, community partnerships are created and instrumental music education is brought back to life."),
              h4("They are concentrated in the North East part of the country.")
              ),
      
      
      tabItem(tabName = "Summary",
              
              h4("The number of people voluntering their time and money across the US has been on the rise since 2008."), 
              h4("Laurels for learning could talk to the incoming graduate students because they are the highest pool of people most likely to volunteer."),
              h4("From our visualizations,
                  we see that there is a high concentration of graduate students in California, the northeast, the midwest, Florida and Texas."), 
              h4("Looking at the child poverty rate across the US, we see some overlap between the states having a high density of graduate students."), 
              h4("Laurels for learning may advertise their mission to these schools via various student organizations to tap in the talent early on in their academic year."),
              h2(shiny::a(href="https://goo.gl/forms/mt3kbWEQh892ZM6r2", "Sign up to volunteer here")),
              imageOutput("endingImg"), align = "center"
      ))))



server <- function(input, output) {
  # get dataset
  
  output$plot1 <- renderPlot({
    ggplot(map.df, aes(x=long,y=lat,group=group))+
      geom_polygon(aes(fill=count))+
      geom_path()+ 
      scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
      coord_map() 
  })
  
  output$plot2 <- renderPlot({
    ggplot(poverty.df, aes(x=long,y=lat,group=group))+
      geom_polygon(aes(fill=count))+
      geom_path()+ 
      scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
      coord_map() 
  })
  
  output$plot3 <- renderPlot({
    ggplot(MusicMap.df, aes(x=long,y=lat,group=group))+
      geom_polygon(aes(fill=count))+
      geom_path()+ 
      scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
      ggtitle("Number of Music Majors")+
      coord_map()
  })
  
  
  # leaflet(data = MusicMap.df) %>%  
  #      addProviderTiles(providers$CartoDB.Positron) %>%
  #      addHeatmap(lng=~long, lat=~lat)})
  
  output$plot4 <- renderPlot({plot(number_of_vol, type='l', xlab='Year', ylab = "# of volunteers in millions", lwd = 3, 
                                   main = "People volunteering in the US over the years")
                  })
  
  output$TrendsVolunteers <- renderImage({
    filename <- normalizePath(file.path('donation.jpeg'))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         width = 500,
         height = 400,
         alt = paste("Donator Heatmap"))
    
  }, deleteFile = FALSE)
  
  output$peerAnalysis <- renderImage({
    filename <- normalizePath(file.path('peerAnalysis.PNG'))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         width = 900,
         height = 410)
    
  }, deleteFile = FALSE)
  
  output$endingImg <- renderImage({
    filename <- normalizePath(file.path('volunteer.jpg'))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         width = 600,
         height = 300,
         alt = paste("Volunteer"))
    
  }, deleteFile = FALSE)
  
     
}

# Run the application 
shinyApp(ui = ui, server = server)

