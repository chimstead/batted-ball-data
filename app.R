#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(RCurl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(stringr)
library(data.table)
library(shiny)
library(stats)
library(plotly)
library(ggforce)
library(neuralnet)
library(nnet)

#load data
load('nnet1.RData')

load('maxmin.RData')

d2<-read_csv('d2.csv')

d4<-read_csv('d4.csv')

d5<-read_csv('d5.csv')

p3<-read_csv('p3.csv')

circles<-read_csv('circles.csv')

# Define UI
ui <- fluidPage(
    titlePanel("Batted Ball Data"),
    tabsetPanel(
        ##tah 1
        tabPanel("Neural Net Outputs", 
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("LAin", "Launch Angle:",
                                     min = -84, max = 88, value = 20
                         ),
                         sliderInput("ESin", "Exit Speed:",
                                     min = 15, max = 118, value = 100
                         ),
                         br(),
                         p("This graph is derived from a small neural network I trained. While it didn't have
                            enough data to fully grasp player positioning or make qualified guesses on balls hit less than 50 or 60mph,
                            it seems to understand the basic trends of where balls are caught and where they fall, especially with regard 
                            to singles, doubles, and home runs outside of the infield. This approximates what HitTrax and catch probability
                            metrics offer, but with a lot less data, and therefore accuracy and complexity.")
                     ),
                     mainPanel(
                         plotOutput("NNplot")),
                     position = "left",
                     fluid = TRUE)),
        ##tab 2
        tabPanel("Graphic Analysis", 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("stat", "Statistic:",
                                     c("Batting Average" = "avg",
                                       "wOBA hit weights" = "woba",
                                       "Slugging Percentage" = "slg"),
                                     selected = 'woba'),
                         br(),
                         p("These graphs display the statistical value a batter is likely to gain by hitting a ball with
                           certain characteristics. They display one of the many dichotomies of hitting, that batters are incentivized
                           to hit the ball in the air, but the least productive areas of the field to hit the ball are where outfielders play.
                           These graphs could likely be improved by filtering for defensive positioning were that data available; 
                           fielders in shifts change the locations of batted balls that are hits as well as the launch angles 
                           that produce hits (e.g. beating a shift by grounding the other way). The wOBA statistic is calculated from 2019 
                           weights here: https://www.fangraphs.com/guts.aspx?type=cn.")
                         
                     ),
                     mainPanel(
                     
                 plotOutput("GAplot"),
                 plotOutput("GAplot2")),
                 position = "left",
                 fluid = TRUE)),
        ##tab 3
        tabPanel("Hit Distance Analysis", 
                 plotlyOutput("HDplot"))

    )
)

# Define server
server <- function(input, output) {
    
    #plot of outcomes when the ball is hit to a location
    output$GAplot <- renderPlot({
        d4%>%
            ggplot()+
            geom_bin2d(aes(x = xdist, y = ydist, group = case_when(
                input$stat == 'avg' ~ bin_avg,
                input$stat == 'woba' ~ bin_wOBA,
                input$stat == 'slg' ~ bin_slg,
            ), fill = case_when(
                input$stat == 'avg' ~ bin_avg,
                input$stat == 'woba' ~ bin_wOBA,
                input$stat == 'slg' ~ bin_slg,
            )))+
            geom_circle(aes(x0 = x0, y0 = y0, r = r), data = circles)+
            geom_hline(yintercept = 0)+
            geom_vline(xintercept = 0)+
            coord_fixed(ratio = 1, xlim = c(0, 500), ylim = c(0, 500))+
            ggtitle("Batting Statistics by Hit Location")+
            xlab("Right Field Line")+
            ylab("Left Field Line")+
            labs(fill = "Statistic Value")+
            theme_minimal()
    })
    
    #plot of outcomes given LA and ES
    output$GAplot2 <- renderPlot({
        d4%>%
            ggplot()+
            geom_bin2d(aes(x = EXIT_SPEED, y = LAUNCH_ANGLE, group = case_when(
                input$stat == 'avg' ~ bin_avg,
                input$stat == 'woba' ~ bin_wOBA,
                input$stat == 'slg' ~ bin_slg,
            ), fill = case_when(
                input$stat == 'avg' ~ bin_avg,
                input$stat == 'woba' ~ bin_wOBA,
                input$stat == 'slg' ~ bin_slg,
            )))+
            coord_fixed(ratio = 0.6)+
            ggtitle("Batting Statistics by Exit Speed and Launch Angle")+
            xlab("Exit Speed")+
            ylab("Launch Angle")+
            labs(fill = "Statistic Value")+
            theme_minimal()
    })
    
    #stacked area plot of outcomes over hit distance
    output$HDplot <- renderPlotly({
        g1<-d5%>%
            ggplot(aes(x=HIT_DISTANCE, fill=factor(numbases))) +
            geom_area(stat ="bin", binwidth = 15)+
            ggtitle("Hit Outcome by Distance")+
            xlab("Hit Distance")+
            ylab("Count")+
            labs(fill = "Bases")
            theme_minimal()
        
        ggplotly(g1)
    })
    
    #Plot of predicted outcomes given LA and ES
    output$NNplot <- renderPlot({
        
        LAin <- input$LAin
        
        ESin <- input$ESin
        
        #get similar hits' distance travelled
        d6<-d2%>%
            filter(LAUNCH_ANGLE - LAin < 3,
                   LAUNCH_ANGLE - LAin > -3,
                   EXIT_SPEED - ESin < 3,
                   EXIT_SPEED - ESin > -3)
        
        ran <- c(min(d6$HIT_DISTANCE), max(d6$HIT_DISTANCE))
        
        #add HT, LA, ES, rearrange cols
        p4<-p3%>%
            filter(HIT_DISTANCE<=ran[2],
                   HIT_DISTANCE>=ran[1])%>%
            mutate(HANG_TIME = mean(d6$HANG_TIME),
                   LAUNCH_ANGLE = LAin,
                   EXIT_SPEED = ESin)
        
        p4<-p4[,c(6, 7, 4, 3, 5, 1, 2)]
        
        #scaledata, run it through previously created model
        p_scaled <- as.data.frame(scale(p4[,1:5], center = mins[1:5], scale = maxs[1:5] - mins[1:5]))
        
        p_results <- compute(nn1, p_scaled)
        
        pr<- p_results$net.result
        
        pr2 <- max.col(pr)
        
        p4$result <- factor(pr2)
        
        #graph it
        p4%>%
            mutate(result = case_when(
                result == 1 ~ 'Out',
                result == 2 ~ 'Single',
                result == 3 ~ 'Double',
                result == 4 ~ 'Triple',
                result == 5 ~ 'HomeRun'
            ))%>%
            ggplot()+
            geom_point(aes(x = x, y = y, fill = result, color = result))+
            geom_circle(aes(x0 = x0, y0 = y0, r = r), data = circles)+
            geom_hline(yintercept = 0)+
            geom_vline(xintercept = 0)+
            coord_fixed(ratio = 1, xlim = c(0, 500), ylim = c(0, 500))+
            ggtitle("Neural Network Hit Outcome Predictions")+
            xlab("Right Field Line")+
            ylab("Left Field Line")+
            theme_minimal()
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
