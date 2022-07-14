library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)


ui <- dashboardPage(
    dashboardHeader(title = "Spotify Top Hits"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "page1", icon = icon("home")),
            menuItem("Data Overview", tabName = "page2", icon = icon("file")),
            menuItem("Data Analysis", tabName = "page3", icon = icon("envelope-open")),
            menuItem("Artists", tabName = "page4", icon = icon("user-circle-o")),
            menuItem("Trend", tabName = "page5", icon = icon("align-left")),
            menuItem("Word Cloud", tabName = "page6", icon = icon("cloud")),
            menuItem("Summary", tabName = "page7", icon = icon("commenting"))
        )
    ),
    

    dashboardBody(
      tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                font-weight: bold;
                                }
                         
                                '))),
      
      #########################################################################page1
        tabItems(
          tabItem("page1",
                  img(
                    src ='banner.png',
                    #src = "spotify-logo.png",
                    height = 210,
                    width = 1160
                  ),
                  br(),
                  br(),
                  fluidRow(
                    box(
                      title = "Abstract",
                      solidHeader = TRUE,
                      status = "primary",
                      width = 12,
                      collapsible = TRUE,
                      column(
                        12,
                        tags$div(
                          "With the innovation of information technology, our life is getting faster and faster, 
                          and people's listening tastes have become very fast, so that we are almost forgetting 
                          the very popular songs before.Our group decided to find the 2010 to 2019 on Spotify for 
                          the top100 song list, and did some data processing to check the trend of popular songs 
                          and changes in people's listening tastes at these years."
                        ),
                        style = "font-size:14px"
                      )
                    ),
                    box(
                      title = "About the Dataset", solidHeader = TRUE,
                      status = "success", width = 12, collapsible = TRUE,
                      column(12, 
                             tags$div(
                               tags$span("Spotify has an outstanding API to connect you to its ubiquitous database of songs and their features. 
                               We can get visual insights from songs you love or integrate a playback into your web application. There is also 
                               a powerful song search engine available as well as a recommendation system which helps you listen to more of what you love. 
                               In this project, we using data from the Spotify API + Rshiny app illustrate different plots and tables to tell people  how the top tracks, 
                                         artists, and genres have changed during  2009-2019. Hope you can have a deeper insight into them 
                                         through our web application."),
                               br(),
                                )
                      ),
                      
                    ),
                    box(
                      title = "About us", solidHeader = TRUE,
                      status = "warning", width = 12, collapsible = TRUE,
                      column(12,tags$div(
                               fluidRow( 
                                 column(2,img(src ="zhichao.jpg",height = 100, width = 100)),
                                 column(6,style = "font-size:16px",
                                        tags$strong("Zhichao Li"),
                                    br(),
                                    tags$li("Currently an MS in Information System student in Johns Hopkins Carey Business School."),
                                    tags$li("In this project, he is responsible for ui design.")))),br(),),
                      
                      column(12,tags$div(
                        fluidRow( 
                          column(2,img(src ="huangdian.pic.jpg",height = 100, width = 100)),
                          column(6,style = "font-size:16px",
                                 tags$strong("Dian Huang"),
                                 br(),
                                 tags$li("Currently an MS in Information System student in Johns Hopkins Carey Business School."),
                                 tags$li("In this project, he is responsible for ui design.")))),br(),),
                      column(12,tags$div(
                        fluidRow( 
                          column(2,img(src ="zhichao.jpg",height = 100, width = 100)),
                          column(6,style = "font-size:16px",
                                 tags$strong("Zhichao Li"),
                                 br(),
                                 tags$li("Currently an MS in Information System student in Johns Hopkins Carey Business School."),
                                 tags$li("In this project, he is responsible for ui design.")))),br(),),
                      column(12,tags$div(
                        fluidRow( 
                          column(2,img(src ="zhichao.jpg",height = 100, width = 100)),
                          column(6,style = "font-size:16px",
                                 tags$strong("Zhichao Li"),
                                 br(),
                                 tags$li("Currently an MS in Information System student in Johns Hopkins Carey Business School."),
                                 tags$li("In this project, he is responsible for ui design.")))),br(),),
                      column(12,tags$div(
                        fluidRow( 
                          column(2,img(src ="zhichao.jpg",height = 100, width = 100)),
                          column(6,style = "font-size:16px",
                                 tags$strong("Zhichao Li"),
                                 br(),
                                 tags$li("Currently an MS in Information System student in Johns Hopkins Carey Business School."),
                                 tags$li("In this project, he is responsible for ui design.")))))
                    ),
                  )),                     
          
          #########################################################################page2                    
                     
            tabItem(
              tabName = "page2",
              
              fluidRow(
                box(
                  title = "About the Dataset", solidHeader = TRUE,
                  status = "primary", width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span("Spotify top songs data set contains information about 
                               top 100 songs on Spotify between 2010 and 2019. Descriptors of 
                               each songs are as follow."),
                           br(),br(),
                           tags$span(
                             "This shiny dashboard application designed to explore and compare those hits among", tags$strong("17"), "factors, including:"),
                           br(),br(),
                           fluidRow(column(5, tags$li("title - song's title"), tags$li("artist - Song's artist"), tags$li("genre - Genre of song"), tags$li("year released -Year the song was released"), 
                                           tags$li("added - Day song was added to Spotify's Top Hits playlist"), tags$li("bpm (Beats Per Minute )- The tempo of the song"), 
                                           tags$li("nrgy (Energy) - How energetic the song is"), tags$li("dnc (Danceability) - How easy it is to dance to the song")),
                                    column(5, tags$li("db (Decibel) - How loud the song is"), tags$li("live - How likely the song is a live recording"), 
                                           tags$li("val - How positive the mood of the song is"), tags$li("dur - Duration of the song"),
                                           tags$li("acous - How acoustic the song is"), tags$li("spch - The more the song is focused on spoken word"), 
                                           tags$li("pop - Popularity of the song (not a ranking)"), tags$li("top year - Year the song was a top hit"),
                                           tags$li("artist type - Tells if artist is solo, duo, trio, or a band"))
                                    
                           ),
                           br(),
                           tags$li(tags$strong("Source: "),tags$a(href = "https://developer.spotify.com/documentation/web-api/reference/#/operations/get-several-audio-features", "Spotify API")),
                           
                         )
                  ),
                ),
              ),
              h2(tags$strong("Top 100 Table:")),
              h3(tags$li("By top year:")),
              fluidRow(
                column(2,
                       selectInput(
                         inputId = "pg21",
                         label = h5("Select top year:"), 
                         choices = c("2010","2011", "2012","2013","2014","2015","2016","2017","2018","2019")
                       )
                ),
                column(10,
                       dataTableOutput("myTable1")
                )
              ),
              br(),
              h3(tags$li("By top genre:")),
              fluidRow(
                column(2,
                       selectInput(
                         inputId = "pg22",
                         label = h5("Select genre:"), 
                         choices = sort(unique(main$`top genre`)),
                         selected = "dance pop"
                       )
                ),
                column(10,
                       dataTableOutput("myTable2")
                )
              ),
              br(),
              h3(tags$li("By artist type:")),
              fluidRow(
                column(2,
                       selectInput(
                         inputId = "pg23",
                         label = h5("Select artist type:"), 
                         choices = sort(unique(main$`artist type`)),
                         selected = "Solo"
                       )
                ),
                column(10,
                       dataTableOutput("myTable3")
                )
              ),
              br(),
              br(),
            ),    
              #########################################################################page3              

            tabItem(tabName = "page3",
                    h3(tags$strong("Top 5 Artist in each year")),
                    h4(tags$li("Ariana Grande, Post Malone, Billie Eilish are the top singer in recent year.")),
                    br(),
                    plotlyOutput("plot1", height = "550px"),
                    br(),
                    br(),
                    h3(tags$strong("Top genre in each year")),
                    h4(tags$li("Dance pop is the most genres in every year, but the proportion is decreasing in recent year.")),
                    h4(tags$li("Latin, metro rap, rap are popular in recent year.")),
                    br(),
                    plotlyOutput("plot2", height = "550px"),
                    br(),
                    plotlyOutput("plot3",  height = "500px")
                    )
          #########################################################################page3 
          #########################################################################page3 
          #########################################################################page3 
          #########################################################################page3 
          #########################################################################page3 
        )
    )
)


server <- function(input, output, session) {
  main = read_csv("Spotify 2010 - 2019 Top 100.csv")
  
  #(1) By top year  
  output$myTable1 = renderDataTable({
    tops_topyear <- main %>%
      filter(`top year` == input$pg21)%>%
      select("title", "artist", "top genre","year released", "top year", "pop", "artist type")
    
    return(datatable(tops_topyear, 
                     options = list(pageLength = 10, lengthChange = FALSE),
                     rownames= FALSE))
  })
  
  #(2) By top genre  
  output$myTable2 = renderDataTable({
    tops_topgenre <- main %>%
      filter(`top genre` == input$pg22)%>%
      select("title", "artist", "top genre","year released", "top year", "pop", "artist type")
    
    return(datatable(tops_topgenre, 
                     options = list(pageLength = 10, lengthChange = FALSE),
                     rownames= FALSE))
  })
  
  #(3) By artist type  
  output$myTable3 = renderDataTable({
    tops_artisttype <- main %>%
      filter(`artist type` == input$pg23)%>%
      select("title", "artist", "top genre","year released", "top year", "pop", "artist type")
    
    return(datatable(tops_artisttype, 
                     options = list(pageLength = 10, lengthChange = FALSE),
                     rownames= FALSE))
  })  
  
    output$plot1 = renderPlotly({
      ###top artist
      top_artist = main%>%
        group_by(`top year`)%>%
        count(artist)%>%
        mutate(prop=n/sum(n))%>%
        arrange(desc(prop))%>%
        slice(1:5)
      
      p = top_artist %>% ggplot(aes(as_factor(`top year`), prop, fill=artist))+
        geom_bar(stat='identity', color = 'white', show.legend = F)+
        geom_text(aes(label=paste(artist)), size=2.5, color='black',
                  position = position_stack(vjust = .5))+
        theme_gray()+
        labs(title='Hot artists in each year', y='Percent', x='Year')
      
      ggplotly(p)
})
    
    output$plot2 = renderPlotly({
      ###top genre
      top_genre<-main%>%
        group_by(`top year`)%>%
        count(`top genre`)%>%
        mutate(prop=n/sum(n))
      p1 = top_genre[order(top_genre$n, decreasing=TRUE)[1:50], ]%>%
        ggplot(aes(as_factor(`top year`), prop, fill=`top genre` ))+
        geom_bar(stat='identity',  color = 'white', show.legend = F)+
        geom_text(aes(label=paste(`top genre` )), size=2.5, color='black',
                  position = position_stack(vjust = .5))+
        theme_gray()+
        labs(title='Hot genre in each year', y='Percent', x='Year')
      ggplotly(p1)
        
    })
    
    output$plot3 = renderPlotly({
      p2 = main%>%
        group_by(`top year`)%>%
        count(`top genre`)%>%
        filter(`top genre` == 'dance pop')%>%
        ggplot(aes(as_factor(`top year`), n))+
        geom_point(color='blue')+
        geom_line(group=1, color='blue')+
        theme_gray()+
        labs(title='Number of Dance pop in each year', y='Number of Dance pop', x='Year')
      ggplotly(p2)
    })
    

}

shinyApp(ui = ui, server = server)
