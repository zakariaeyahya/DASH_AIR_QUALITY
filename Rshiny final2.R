library(shinydashboard)
library(DT)
library(leaflet)
library(shinycssloaders)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(dplyr)
library(ggplot2)
library(shinythemes)
#  --------------------------------------------------------------------------------------------------------
#                                              READING THE FILES
#  --------------------------------------------------------------------------------------------------------
title <- tags$img(src='logo.png',height='50', width='46'," E N V I R O N S")
Datafinal= read.csv("D:/dowload/finalDS.csv")
Datafinal$Date = as.Date(Datafinal$Date,"%d-%m-%Y")
Datafinal <- mutate(Datafinal, Year = format(Date,"%Y"))
Year<-unique(Datafinal$Year)
City<-unique(Datafinal$City)


# ---------------------------------------------------------------------------------------------------------
#                                                USER INTERFACE
# ---------------------------------------------------------------------------------------------------------

ui <- dashboardPage(
  skin = 'red',
  dashboardHeader(title = title),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon('home')),
      menuItem(("Map Distribution"),tabName = "dashboard",icon=icon('map')),
      menuItem(("Line graph"), tabName = "Line_graph",icon = icon('chart-line')),
      menuItem(("Pollutant trends"), tabName = "year_data", icon = icon('th')),
      menuItem("Raw data",tabName = "table",icon = icon('table')),
      menuItem("Visit Us", icon = icon("send", lib = "glyphicon"), href = "https://github.com/zakariaeyahya/tesla-vs-GameStop.git")
    )
  ),
  
  dashboardBody(
    #custom css
    tags$head(
      tags$style(
        HTML(" #compare_state_option,#compare_year_option ,.compare-county-wrapper { display:flex; margin-bottom:-10px;}
    				#compare_state_option > div, #compare_year_option > div, .compare-county-wrapper > div > div {padding-right: 15px;}
    				.shiny-output-error:before,.shiny-output-error{visibility: hidden !important;}
    				.compare-class > .tabbable > .nav-tabs {margin-bottom: 20px !important;}
    				.box.box-solid.box-primary {border: 1px solid #dd4b39 !important;}
    				.box.box-solid.box-primary>.box-header { background: #dd4b39 !important; background-color: #dd4b39 !important; }
    				.sidebar-menu>li {font-size:17px;}")
      )
    ),
    #--------------------------------------------RAW DATA TAB---------------------------------------------------------
    tabItems(
      tabItem(tabName = "table",
              tags$h3('Download Data'),
              downloadButton("downloadData"),
              br(),
              br(),
              # box(title = "Dataset",solidHeader = TRUE,status = "primary",height="100%", width =12,
              tableOutput("tableData")),
      # ---------------------------------------------HOME TAB-------------------------------------------------------------
      
      
      # Home Tab
      tabItem(
        tabName = "home",
        br(),
        fluidRow(
          column(width = 12, tags$h2("Introduction au tableau de bord de la qualité de l’air en Inde"))
        ),
        fluidRow(
          column(
            width = 12,
            tags$p(
              "Bienvenue dans notre projet de tableau de bord interactif sur la qualité de l'air en Inde. Ce projet, réalisé avec R Shiny, vise à fournir une visualisation claire et compréhensible des données sur la qualité de l’air dans différentes villes de l’Inde. Notre objectif est de sensibiliser à l’importance de la qualité de l’air et de son impact sur notre santé et notre environnement."
            )
          )
        ),
        fluidRow(
          column(width = 12, tags$h2("Défis de la Pollution de l'Air en Inde"))
        ),
        fluidRow(
          column(
            width = 12,
            tags$p(
              "L'Inde fait face à des défis importants liés à la pollution de l'air. Cette situation a des conséquences graves sur la santé publique, l'environnement et la qualité de vie des habitants. Notre tableau de bord explore les données de qualité de l'air pour différentes villes indiennes, mettant en lumière les tendances et les variations afin de mieux comprendre et atténuer ce problème majeur."
            )
          )
        ),
        fluidRow(
          column(width = 12, tags$h2("Top News"))
        ),
        fluidRow(
          box(
            width = 12, height = 460,
            HTML('<html>
            <head>
              <style>
                table {
                  font-family: arial, sans-serif;
                  border-collapse: collapse;
                  width: 100%;
                }

                td, th {
                  border: 1px solid #dddddd;
                  text-align: left;
                  padding: 8px;
                }

                tr:nth-child(even) {
                  background-color: #dddddd;
                }

                p {
                  font-size: 19px;
                }

                h3, h2 {
                  font-weight: bold;
                }
              </style>
            </head>
            <body>
              <table style="width:125%; border:0px;">
                <tr>
                  <td>
                    <h3>More to Delhi’s pollution than crop burning</h3>
                    crop residue burning is a demonstration of why environmental concerns such as air quality
                    cannot be addressed
                    in isolation and there are no easy or quick solutions.
                    <a href="https://economictimes.indiatimes.com/news/politics-and-nation/more-to-delhis-pollution-than-crop-burning/printarticle/78891537.cms">Read More</a></td>
                </tr>
                <tr>
                  <td>
                    <h3>Air quality worsens in the national capital post-Diwali </h3>
                    The air quality dipped to "severe" at several places across the national
                    capital on Saturday night itself owing to a combination of stubble burning and firecrackers burst during 
                    the Diwali
                    <a href="https://economictimes.indiatimes.com/news/politics-and-nation/air-quality-worsens-in-the-national-capital-post-diwali/articleshow/79228838.cms?utm_source=contentofinterest&utm_medium=text&utm_campaign=cppst">Read more<a/></td>
                </tr>
                <tr>
                  <td>
                    <h3>Delhis air quality very poor, improvement</h3>
                   The city air quality index was 374 at 9 am on Thursday. 
                   The 24-hour average AQI was 413 on Wednesday, 379 on
                 Tuesday and 295 on Monday, according to the Central Pollution Control Board data.
                 <a href="https://economictimes.indiatimes.com/news/politics-and-nation/delhis-air-quality-very-poor-improvement-predicted/articleshow/79422430.cms?utm_source=contentofinterest&utm_medium=text&utm_campaign=cppst">Read more</a></td>
                   </tr>
                   </table>
                   </body>
                   </html>')
    )
  )
),



      
      
      
      
      
      # -------------------------------------------------MAP AND BAR GRAPH TAB------------------------------------------------------------------ 
      tabItem(tabName = "dashboard",
              fluidRow(
                column(3,
                       dateInput("select_date",
                                 h3("Select Date"),
                                 format = "yyyy-mm-dd",
                                 value="2017-01-01",
                                 min="2017-01-01",
                                 max="2019-12-31"))),
              fluidRow(
                box(title = "Map",solidHeader = TRUE, status = "primary",height=650,
                    width = 5,leafletOutput(height = 590,"map")),
                box(title = "Pollutants Distribution",solidHeader = TRUE, status = "primary",width = 7,
                    tabBox(width=12,
                           tabPanel(title="AQI",plotOutput(height = 500, "AQI")),
                           tabPanel(title="PM2_5",plotOutput(height = 500,"PM2_5")),
                           tabPanel(title="PM10",plotOutput(height = 500,"PM10")),
                           tabPanel(title="NO",plotOutput(height = 500,"NO")),
                           tabPanel(title="NO2",plotOutput(height = 500, "NO2")),
                           tabPanel(title="NH3",plotOutput(height = 500, "NH3")),
                           tabPanel(title="CO",plotOutput(height = 500, "CO")),
                           tabPanel(title="SO2",plotOutput(height = 500, "SO2")),
                           tabPanel(title="O3",plotOutput(height = 500, "O3"))
                    ))),
      ),
      # ---------------------------------------------------------------LINEGRAPH TAB---------------------------------------------------------
      tabItem(tabName = "Line_graph",
              fluidRow(
                column(3,
                       box(title = "Inputs",solidHeader = TRUE, status = "primary", width =12,height=600,
                           selectInput("Cities1",h3("Choose a City"),City,selected = 'Delhi'),
                           
                           dateInput("start_date",h3("From"),
                                     format = "yyyy-mm-dd",
                                     value="2017-01-01",
                                     min="2017-01-01",
                                     max="2019-12-31"),
                           
                           
                           dateInput("end_date",h3("To"),
                                     format="yyyy-mm-dd",
                                     value="2017-01-07",
                                     min="2017-01-01",
                                     max="2019-12-31")
                           
                           
                           
                           
                       )    
                ),
                
                
                column(9,
                       box(title = "Line Graph",solidHeader=TRUE, status = "primary",width = 12,height=600,
                           box(width=12,plotOutput(height = 500,"plots"))),
                       
                ))),     
      
      
      # -----------------------------------------------------------CORRELATION MATRIX TAB--------------------------------------------------    
      tabItem(tabName = "year_data",
              fluidRow(column(4,selectInput("Cities", ("Choose a City:"),City,selected = 'Delhi')),
                       column(8,selectInput("years",("Choose a Year:"),Year,selected="2017"))),
              fluidRow(
                column(6,
                       box(title = "Correlation matrix", solidHeader = TRUE, status = "primary", width = 12,
                           tabsetPanel(
                             tabPanel("correlation coefficients", withSpinner(plotOutput("corrcoeff",height = 475))),
                             tabPanel("Heat map", withSpinner(plotOutput("heatmap",height = 475)))
                             
                           )
                       )	
                       
                ),
                column(6,
                       box(title = "Precautions table", solidHeader = TRUE, status = "primary", width = 12,
                           tabsetPanel(
                             tabPanel("PM2.5", withSpinner(dataTableOutput("pm2_5",height = 475))),
                             tabPanel("PM10", withSpinner(dataTableOutput("pm10",height = 475))),
                             tabPanel("NO2", withSpinner(dataTableOutput("no2",height = 475))),
                             tabPanel("CO", withSpinner(dataTableOutput("co",height = 475))),
                             tabPanel("SO2", withSpinner(dataTableOutput("so2",height = 475))),
                             tabPanel("O3", withSpinner(dataTableOutput("o3",height =475))),
                             tabPanel("NO", withSpinner(dataTableOutput("no",height = 475))),
                             tabPanel("NH3", withSpinner(dataTableOutput("nh3",height = 475)))
                           )
                           
                       ))
              ))
    ),
    
    
  )
)

# -------------------------------------------------------------------------------------------------------------------------------------
#                                                   SERVER
# -------------------------------------------------------------------------------------------------------------------------------------

server <- function(input, output) {
  
  # ----------------------------------------------------------TAB4-----------------------------------------------------------------------  
  
  # --------------------------------------------------------CORRELATION MATRIX ----------------------------------------------------------    
  
  output$corrcoeff <- renderPlot({
    mydata2 <- Datafinal %>%filter(Year==input$years, City==input$Cities)
    mydata<-mydata2[,c(3:11)]
    mydata.rcorr = rcorr(as.matrix(mydata))
    mydata.coeff = mydata.rcorr$r
    corrplot(mydata.coeff,method="number")
  })
  
  # ------------------------------------------------------SCATTERPLOT CORRELATION-------------------------------------------------------    
  
  output$corrscatt <- renderPlot({
    mydata2 <- Datafinal %>%filter(Year==input$years, City==input$Cities)
    mydata<-mydata2[,c(3:11)]
    chart.Correlation(mydata, histogram=TRUE, pch=19)
    
  })
  
  # -----------------------------------------------------------HEAT MAP-----------------------------------------------------------------    
  
  output$heatmap <- renderPlot({
    mydata2 <- Datafinal %>%filter(Year==input$years, City==input$Cities)
    mydata<-mydata2[,c(3:11)]
    mydata.rcorr = rcorr(as.matrix(mydata))
    mydata.coeff = mydata.rcorr$r
    palette = colorRampPalette(c("green", "white", "red")) (20)
    heatmap(x = mydata.coeff, col = palette, symm = TRUE)
  })
  
  
  # ----------------------------------------------------TABLES FOR POLLUTANT PRECAUTIONS-------------------------------------------------- 
  
  # reading csv file containing precautions from pollutants
  Poltab= read.csv("D:/dowload/pollutants table.csv")
  
  # Table showing PM2.5 cautions
  pm2_5data<-Poltab[,c(1:3)]
  output$pm2_5 <- DT::renderDataTable(
    DT::datatable({ 
      pm2_5data
    }, 
    options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    
    ))
  
  # Table showing PM10 cautions
  pm10data<- Poltab[,c(4:6)]
  output$pm10 <- DT::renderDataTable(
    DT::datatable({ 
      pm10data
    }, 
    options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    ))
  
  # Table showing NO2 cautions
  no2data<-Poltab[,c(7:9)]
  output$no2 <- DT::renderDataTable(
    DT::datatable({ 
      no2data
    }, 
    options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    
    ))
  
  # Table showing CO cautions
  codata<-Poltab[,c(10:12)]
  output$co <- DT::renderDataTable(
    DT::datatable({ 
      codata
    }, 
    options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    
    ))
  
  # Table showing SO2 cautions
  so2data<-Poltab[,c(13:15)]
  output$so2 <- DT::renderDataTable(
    DT::datatable({ 
      so2data
    }, 
    options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    
    ))
  
  # Table showing O3 cautions
  o3data<-Poltab[,c(16:18)]
  output$o3 <- DT::renderDataTable(
    DT::datatable({ 
      o3data
    }, 
    options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    
    ))
  
  # Table showing NO cautions
  nodata<-Poltab[,c(19:21)]
  output$no <- DT::renderDataTable(
    DT::datatable({ 
      nodata
    }, 
    options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    
    ))
  
  # Table showing NH3 cautions
  nh3data<-Poltab[,c(22:24)]
  output$nh3 <- DT::renderDataTable(
    DT::datatable({ 
      nh3data
    }, 
    options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    
    ))
  
  
  # ------------------------------------------------------TAB1--------------------------------------------------    
  output$AQI<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=AQI, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  # ----------------------------------------------------MAP FOR AQI--------------------------------------------------    
  
  output$map<-renderLeaflet({
    
    # filtering the data according to the date selected
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    
    # mutating the data with the popup info for easy access.
    Day<-Day %>% 
      mutate(popup_Info=paste("City: ",City,"</br>","AQI: ",AQI,"</br>","Condition: ",AQI_Bucket))
    
    # gradient based on the AQI level
    colour<-c("green","red")
    
    # creating a pallet out of it
    pal<-colorFactor(colour,Datafinal$AQI)
    
    # sending the data to the leaflet map to be rendered
    # the markers are provided the pallet colour
    leaflet() %>% 
      addTiles() %>% 
      addCircleMarkers(data=Day, lat=~Latitude, lng =~Longitude, 
                       radius = 20, popup = ~popup_Info, color = ~pal(AQI))
    
    
  })
  
  # ----------------------------------------------------BAR GRAPHS FOR AQI--------------------------------------------------    
  
  output$PM2_5<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=PM2.5, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  output$PM10<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=PM10, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  output$NO<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=NO, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  output$NO2<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=NO2, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  output$NH3<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=NH3, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  output$CO<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=CO, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  output$SO2<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=SO2, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  output$O3<-renderPlot({
    Day <- filter(Datafinal,Datafinal$Date == input$select_date)
    df_base <- ggplot(data=Day, aes(x=City, y=O3, fill=AQI_Bucket))
    df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  })
  
  # ----------------------------------------TAB3---------------------------------------------------------------------     
  # ----------------------------------------LINE GRAPHS------------------------------------------------------------- 
  
  output$plots <- renderPlot({
    Datafinal$Date <- as.Date(Datafinal$Date)
    week_new <- Datafinal[,c(1:10)]
    week_new <- filter(week_new,between(Date, as.Date(input$start_date), as.Date(input$end_date)))
    week_city <- filter(week_new,City==input$Cities1)
    
    plot(week_city$CO,type="b",lwd=2,
         xaxt="n",ylim=c(0,500),col="blue",
         xlab="Date",ylab="values",
         main = input$Cities1)
    
    axis(1,at=1:length(week_city$Date),labels=week_city$Date)
    lines(week_city$NO2,col="red",type="b",lwd=2)
    lines(week_city$NH3,col="orange",type="b",lwd=2)
    lines(week_city$NO,col="purple",type="b",lwd=2)
    lines(week_city$O3,col="grey",type="b",lwd=2)
    lines(week_city$PM2.5,col="green",type = "b",lwd=2)
    lines(week_city$PM10,col="brown",type = "b",lwd=2)
    lines(week_city$SO2,col="violet",type = "b",lwd=2)
    
    
    
    legend("topright",legend=c("CO","NO2","NH3","NO","O3","PM2.5","PM10","SO2
                             "),
           lty=5,lwd=4,pch=10,col=c("blue","red","orange","purple","grey","green","brown","violet"),
           ncol=2,bty="n",cex=0.8,
           text.col=c("blue","red","orange","purple","grey","green","brown","violet")
    )
  })
  #------------------------------------------------------------TAB5------------------------------------------------------------
  #------------------------------------------------------------RAW DATA------------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename=function(){
      paste("DayData","csv", sep = '.')
    },
    content=function(file){
      write.csv(Datafinal,file)
    }
  )
  
  output$tableData <- renderTable(
    head(Datafinal,200),width = "100%"
  )
  
}
# ------------------------------------------------------------RUNNING THE PROJECT--------------------------------------------------     
shinyApp(ui = ui, server = server)