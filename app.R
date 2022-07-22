library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(ggplot2)
library(rvest)
library(stringr)
library(sp)
library(wordcloud2)
library(scales)
library(ggthemes)
library(dplyr)
library(leaflet)
library(DT)
library(plotly)
library(shiny)
library(shinydashboard)
library(sf)
library(maps)
library(dplyr)
library(shiny)
library(ggmap)
library(scales)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(shinyjs)


getPic <- function(title){
    movie <- read_html(paste0("https://www.imdb.com/find?q=", URLencode(title)))
    print(paste0("https://www.imdb.com/find?q=", URLencode(title)))
    results = movie %>%
        html_nodes(".findList .primary_photo a img") %>% 
        html_attr('src')
    
    print(results)
    
    result_link=""
    if(length(results)>0){
        result_link=results[1]
        lastCh = str_locate_all(pattern ="@",result_link)[[1]][1]-1
        v = unlist(strsplit(result_link, "\\."))
        finalLink = paste0(c(v[1:(length(v)-2)], "_V1.jpg"),collapse = ".")
        return(finalLink)
    }
    return(NULL)
}

#read data and transformation
data = read_csv("project_data.csv")
data = data %>% arrange(title)
data$runtime <- as.numeric(data$runtime)
data$release_year <- as.numeric(data$release_year)
data0 <- data %>%
    filter(!is.na(runtime)) %>%
    filter(!is.na(imdb_score)) %>%
    filter(!is.na(imdb_votes)) 

data1 <- data0 %>%
    filter(!is.na(tmdb_popularity)) %>%
    filter(!is.na(tmdb_score)) 

    

#selection options for filter
chargenres = data%>% select(title,genres)
chargenres$genres = gsub("[[:punct:]]","", data$genres)
releaseyear = 1953:2022
country_names=sort(c(unique(data1$Country[data1$Country!="#N/A"])))
age=unique(data$age_certification)
genres_names=c("drama","action", "comedy","scifi","war","thriller","horror",
               "music","documentation","crime","animation","romance","family",
               "fantasy","sport","history","western","european","reality")

#process the genre column
wcdata <- separate(data, 'genres', paste("genre", 1:6, sep="_"), sep=",", extra="drop")
wcdata1 <- wcdata %>%
    select(title, release_year, genre_1, genre_2, genre_3, genre_4, genre_5, genre_6, production_countries)
wcdata1$genre_1 <-str_replace_all(wcdata1$genre_1, "[^[:alnum:]]", "")
wcdata1$genre_2 <-str_replace_all(wcdata1$genre_2, "[^[:alnum:]]", "")
wcdata1$genre_3 <-str_replace_all(wcdata1$genre_3, "[^[:alnum:]]", "")
wcdata1$genre_4 <-str_replace_all(wcdata1$genre_4, "[^[:alnum:]]", "")
wcdata1$genre_5 <-str_replace_all(wcdata1$genre_5, "[^[:alnum:]]", "")
wcdata1$genre_6 <-str_replace_all(wcdata1$genre_6, "[^[:alnum:]]", "")
wcdata2 <- pivot_longer(wcdata1,
                        c("genre_1","genre_2","genre_3","genre_4","genre_5","genre_6"),
                        names_to = "number", values_to = "genre")
wcdata2$genre <- as.factor(wcdata2$genre)
wcdata3 <- wcdata2[,-4] %>%
    filter(!is.na(genre)) %>%
    filter(!(genre == ""))



ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Netflix Movie Engine"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "page1", icon = icon("couch")),
      menuItem("Data Table", tabName = "page8", icon = icon("cog")),
      menuItem("Search", tabName = "page2", icon = icon("film")),
      menuItem("Map", tabName = "page3", icon = icon("tape")),
      menuItem("General Analysis",tabName = "page4",icon = icon("video"),
               selected = T,
               menuItem(
                 "Genre Trend",
                 tabName = "page5",
                 icon = icon("file-video")),
               menuItem(
                 "Other Trends",
                 tabName = "page6",
                 icon = icon("compact-disc"))),
      menuItem("Filters", tabName = "page7", icon = icon("fas fa-ticket-alt"))
    )
  ),
  dashboardBody(
      
    tags$head( 
      tags$style(HTML(".main-sidebar { font-size: 16px; }"))
    ),
    tabItems(
      tabItem(
          tabName = "page1",
              fluidRow(
                box(
                  title = "Project description", solidHeader = TRUE,
                  width = 12, collapsible = TRUE,
                  column(12, 
                         fluidRow(column(12,"This app,",tags$strong("Netflix Movie Engine,"),"aims to provide customers with mass trending information or personalized recommendations related to movies. "), style = "font-size:16px"),
                         br(),
                         fluidRow(column(12,"What Netflix Movie Engine can do are:" ,br(),
                                         tags$li("Find relevant movies and list movie synopsis based on keywords provided by the user."),
                                         tags$li("Show the difference in the number and subject matter of movies by country and year;"),
                                         tags$li("Further analyze movies, for example, by year and country, to understand the genres of movies that audiences like."), 
                                         tags$li("Visualize other trends, such as the change in the total number of movies released each year from 1945 to 2022, the runtime of movies, IMDB ratings or votes."),
                                         tags$li("Netflix Movie Engine also supports personalized search, allowing customers to manually select the categories of movie shelf time, movie's runtime and IMDB rating, and then get more personalized recommendations based on movie genre, country of filming and director.")), style = "font-size:16px"),
                         br(),
                         br(),
                         fluidRow(column(12,"We want Netflix Movie Engine to serve not only the audience who want to select movies to watch, but also those who work in the film-related industry. We expect our clients to be able to predict what today's users are likely to enjoy based on the types of movies that audiences have enjoyed over the years, the length of more accepted movies. This will help them discover or improve more movies that match the audience's appetite to increase user consumption."), style = "font-size:16px")
                         
                  )#column12
                ),
              ),#fluidrow
              
          tags$style(HTML(".box.box-solid.box-info>.box-header {color:#fff;background:#2d3c42}
                .box.box-solid.box-info{
                border-bottom-color:#222d32;
                border-left-color:#222d32;
                border-right-color:#222d32;
                border-top-color:#222d32;}"
          )),
              fluidRow(
                box(
                  title = "About the Dataset", solidHeader = TRUE,
                  status = 'info', width = 12, collapsible = TRUE,
                  column(12, 
                         tags$div(
                           tags$span("This dataset lists all shows available on Netflix streaming, and analyzes the data to find interesting facts. This data was acquired in May 2022 containing data available in the United States."),
                           br(),
                           tags$li(tags$strong("Source: "),tags$a(href = "https://www.kaggle.com/datasets/victorsoeiro/netflix-tv-shows-and-movies?select=titles.csv", "Netflix TV Shows and Movies")),
                           tags$li("This dataset contains +5k unique titles on Netflix with 15 columns containing their information. And over +77k credits of actors and directors on Netflix titles with 5 columns containing their information."),
                           style = "font-size:16px")
                         
                  ),#cloumn12
                  
                  
                )
                
              ),#fluidrow
              
              
              fluidRow(
                box(
                  title = "About Us", solidHeader = TRUE, 
                  status = 'info', width = 12, collapsible = TRUE,
                  column(12,fluidRow(column(12,tags$strong("Team members:"),
                                            br(),
                                            tags$li("Qian Luo - qluo4@jh.edu"),br(),
                                            tags$li("Yuhan He - yhe73@jh.edu"),br(),
                                            tags$li("Yunan Yang - yyang212@jh.edu"),br(),
                                            tags$li("Yuzhen Tan - ytan30@jh.edu"),br(),
                                            tags$li("Yue Su - ysu38@jh.edu"),br(),
                                            "BU.520.650.53.SU22 Data Visualization",br(),
                                            "Johns Hopkins University Carey Business School BARM Candidates "),style = "font-size:16px"))))
      ),
      
            
      
            tabItem(tabName = "page8",
                    column(
                      width = 12,
                      tabBox(
                        width = 12,
                        height = 500,
                        tabPanel(
                          title = "Data",
                          height = 500,
                          dataTableOutput("UserBehaviorTable"),
                          )))),
      
      
            tabItem(tabName = "page2",
                    h2(id = "search-heading","Search for Movies"),
                    tags$style("#search-heading{ 
                         color: #CA2444;
                         font-size: 40px;
                         font-family: Verdana;
                         }"),
                    br(),br(),
                    fluidRow(
                      column(4, box(width = 12, 
                                    solidHeader = TRUE,
                                    tags$div(
                        img(src="Netflix-logos.jpg",alt="Netflixlogos",width="100%"),
                        style="text-align:center"),
                        br(),
                        selectizeInput("title", "Type in keywords or select",
                                       choices = NULL,
                                       selected = 1
                        )
                      )),
                      column(8, box(width = 12, 
                                    solidHeader = TRUE,
                                    mainPanel(
                                      uiOutput("name"),
                                      textOutput('tinfo'),
                                      br(),
                                      style = "width:100%",
                                      fluidRow(
                                        column(7,
                                               fluidRow(style = "text-align:center;width:100%",
                                                        column(6,box(strong("Year"), textOutput('tyear'),width = 12,status = 'danger')),
                                                        column(6,box(strong("Runtime"),  textOutput('ttime'), width=12,status = 'danger'))
                                               ),
                                               
                                               fluidRow(style = "text-align:center;width:100%",
                                                        column(6,box(strong("Genre"),   textOutput('tgenre'), width=12,status = 'danger')),
                                                        column(6,box(strong("Votes"),  textOutput('tvote'), width=12,status = 'danger'))
                                               ),
                                               
                                               fluidRow(style = "text-align:center;width:100%",
                                                        column(6,box(strong("IMDB Score"),   textOutput('timdb'), width=12,status = 'danger')),
                                                        column(6,box(strong("TMDB Score"),  textOutput('ttmdb'), width=12,status = 'danger'))
                                               )),
                                        column(5,style="text-align:center;margin-top:0px",uiOutput('picture'))
                                      )
                                    )
                      )
                      )
                    )
            ),
        
            tabItem( 
                tabName = "page3", 
                h2(id = "maptitle", HTML('&nbsp;'),"Movie Distribution Map"),
                tags$style("#maptitle{ 
                         color: #2C5364;
                         font-size: 40px;
                         font-family: Verdana;
                         }"),
                fluidRow(
                    h3(id = "year1",HTML('&nbsp;'),HTML('&nbsp;'),"Number of Movies by Country"),
                    tags$style("#year1{ 
                         color: #2C5364;
                         font-size: 20px;
                         font-family: Verdana;
                         }"),
                    column( 
                        width = 4, 
                        tabBox( 
                            width = 12, 
                            id = "tabset1", 
                            height = "250px", 
                            tabPanel( 
                                title = "Total Movie Made by Country", 
                                sliderInput( 
                                    "yearSlider",
                                    "Start Year:", 
                                    min = 2014, 
                                    max = 2022, 
                                    value = 2014,
                                    step = 1,
                                    animate = animationOptions(interval = 2600,loop = TRUE)
                                ) 
                            ) 
                        ) 
                    ), 
                    column(width = 8, 
                           box( 
                               width = 12, 
                               solidHeader = TRUE,
                               plotlyOutput(outputId = "plot1", 
                                            height = 500) 
                           )), 
                    br(), 
                    br(), 
                    h3(id = "year2",HTML('&nbsp;'),HTML('&nbsp;'),"Average Ratings by Country"),
                    tags$style("#year2{ 
                         color: #2C5364;
                         font-size: 20px;
                         font-family: Verdana;
                         }"),
                    column( 
                        width = 4, 
                        tabBox( 
                            width = 12, 
                            id = "tabset1", 
                            height = "250px", 
                            tabPanel( 
                                title = "Avg Ratings by Country", 
                                sliderInput( 
                                    "year",
                                    "Start Year:", 
                                    min = 2014, 
                                    max = 2022, 
                                    value = 2014,
                                    step = 1,
                                    animate = animationOptions(interval = 2600,loop = TRUE)
                                ) 
                            ) 
                        ) 
                    ), 
                    column(width = 8, 
                           box( 
                               width = 12,
                               solidHeader = TRUE,
                               plotlyOutput(outputId = "plot2", 
                                            height = 500) 
                           )), 
                    br(), 
                    br() 
                ) 
            ),
      
            tabItem(
                tabName = "page5",
                h2(id = "trend-heading", "Genre trend"),
                tags$style("#trend-heading{ 
                         color: #CA2444;
                         font-size: 40px;
                         font-family: Verdana;
                         }"),
                br(),
                br(),
                tabsetPanel(
                    tabPanel(
                        "Genre Trends Over Years",
                        fluidRow(
                           box(width = 12,
                               solidHeader = TRUE,
                            sliderInput(
                                "Plot31Input",
                                "Select a year to show word cloud of genres:",
                                min = 1979,
                                max = 2022,
                                value = 1979,
                                step = 1,
                                width = "100%",
                                animate = animationOptions(interval = 1000, loop = FALSE)
                            )
                            ),
                            br(),
                            box(width = 12,
                                solidHeader = TRUE,
                                   wordcloud2Output("plot31", width = "100%")
                            )
                        )
                    ),
                    tabPanel(
                        "Genre Trends Over Countries",
                        fluidRow(
                            box(width = 12,
                                solidHeader = TRUE,
                            sliderInput(
                                "Plot32Input",
                                "Select a year to show word cloud of genres:",
                                min = 2018,
                                max = 2022,
                                value = 2018,
                                step = 1,
                                width = "100%",
                                animate = animationOptions(interval = 1000, loop = FALSE)
                            ),
                            selectInput(
                                "select22",
                                label = "Choose a country",
                                width = "100%",
                                choices = list("US", "CA", "GB", "FR", "IN", "JP"),
                                selected = 1
                            )
                        ),
                        br(),
                        box(width = 12, 
                            solidHeader = TRUE,
                               wordcloud2Output("plot32")
                        ),
                        fluidRow(br(),br(),br(),br(),br(),br(),
                                 mainPanel(br())
                        )
                    )
                )
            )
            ),
            tabItem(
                tabName = "page6",
                h2(id = "trend-heading2",  "Other trends over years"),
                tags$style("#trend-heading2{ 
                         color: #CA2444;
                         font-size: 40px;
                         font-family: Verdana;
                         }"),
                br(),
                fluidRow(
                  box(width = 12,
                      solidHeader = TRUE,
                    sliderInput(
                        "be_yr",
                        "Select begin year:",
                        min = 1945,
                        max = 2022,
                        value = 1945,
                        step = 1,
                        width = "100%"
                    ),
                    sliderInput(
                        "en_yr",
                        "Select end year:",
                        min = 1955,
                        max = 2022,
                        value = 2022,
                        step = 1,
                        width = "100%"
                    ),
                    selectInput(
                        "select11",
                        label = "Choose a variable ",
                        width = "90%",
                        choices = list(
                            "number of movies" = "num_movie",
                            "average runtime" = "avg_runtime",
                            "imdb score" = "avg_imdb_score",
                            "imdb vote" = "avg_imdb_votes",
                            "average popularity" = "avg_tmdb_popularity"
                        ),
                        selected = 1
                    )
                ),
                br(),
                box(width=12, 
                    solidHeader = TRUE,
                    style = "background-color: white;",
                       plotlyOutput("plot33")
                ),
                fluidRow(br(),br(),br(),br(),br(),br(),
                         mainPanel(br())
                )
            )
            ),  

        tabItem(tabName = "page7",
                h1(id = "big-heading",  HTML('&nbsp;'),"Discover the Movies You Like"),
                tags$style("#big-heading{ 
                         color: #CA2444;
                         font-size: 40px;
                         font-family: Brush Script MT;
                         }"),
 
                fluidRow(
                    column(12,
                              box(width = 3,
                                  solidHeader = TRUE,
                        selectInput("Mstart", "Start Release Year",
                                    choices = releaseyear,
                                    selected = 1953),
                        selectInput("Mend", "End Release Year",
                                    choices = releaseyear,
                                    selected = 2022),
                        sliderInput("Mruntime",
                                    label = "Runtime in min",
                                    min = 0, max = 240, value = 240),
                        sliderInput("Mrating",
                                    label = "IMDb Rating",
                                    min = 0, max = 10, value = 0),
                        selectInput(inputId = "Mgenre",
                                    label = "Genres",
                                    choices = genres_names,
                                    selected = "drama"),
                        selectInput(inputId = "Mcountry",
                                    label = "Production Country",
                                    choices = country_names,
                                    selected = "United States"),
                        selectInput(inputId = "Mage",
                                    label = "Age Rating",
                                    choices = age,
                                    selected = "PG-13"),
                        actionButton(inputId = "actButton",
                                     label="Search",
                                     icon=icon('play-circle'))
                    ),
                box(width = 9,
                    solidHeader = TRUE,
                    height = "700px",
                    mainPanel(
                        style = "width:100%",
                        tabsetPanel(
                            tabPanel(
                                "Movie List",
                                br(),
                                fluidRow(style = "text-align:center;width:100%",
                                         column(3,box(uiOutput('pic1'),br(),strong(textOutput('title1')),width = 15,status = 'danger')),
                                         column(3,box(uiOutput('pic2'),br(),strong(textOutput('title2')),width = 15,status = 'danger')),
                                         column(3,box(uiOutput('pic3'),br(),strong(textOutput('title3')),width = 15,status = 'danger')),
                                         column(3,box(uiOutput('pic4'),br(),strong(textOutput('title4')),width = 15,status = 'danger'))),
                                fluidRow(style = "text-align:center;width:100%",
                                         column(3,box(uiOutput('pic5'),br(),strong(textOutput('title5')),width = 15,status = 'danger')),
                                         column(3,box(uiOutput('pic6'),br(),strong(textOutput('title6')),width = 15,status = 'danger')),
                                         column(3,box(uiOutput('pic7'),br(),strong(textOutput('title7')),width = 15,status = 'danger')),
                                         column(3,box(uiOutput('pic8'),br(),strong(textOutput('title8')),width = 15,status = 'danger'))),
                                br(),br(),br(),
                                fluidRow(style = "text-align:center;width:100%", strong(textOutput('yearwarning'))),
                                tags$head(tags$style("#yearwarning{color: #FB4266;
                                 font-size: 20px;
                                 font-family: monospace;
                                 }")),
                                fluidRow(style = "text-align:center;width:100%", strong(textOutput('NAwarning'))),
                                tags$head(tags$style("#NAwarning{color: #FB4266;
                                 font-size: 20px;
                                 font-family: monospace;
                                 }"))
                                )
                            
                        ))
                    )
                )
        )
    )  
    )
)
)
    



server <- function(input, output, session) {
  df = subset(data, select = -c(description) )
  output$UserBehaviorTable = renderDataTable(df)
  
  ###############input and out put for search page
  updateSelectizeInput(session = session, inputId = 'title', choices = unique(data$title), server = TRUE)
  
  output$name <- renderUI(
    h2(strong(input$title)),
  )
  
  selected <- reactive({
    input$title
  })
  output$tinfo = renderText({
    data$description[which(data$title==selected())]
  })
  output$tyear = renderText({
    data$release_year[which(data$title==selected())]
  })
  output$ttime = renderText({
    data$runtime[which(data$title==selected())]
  })
  output$tgenre = renderText({
    chargenres$genres[which(data$title==selected())]
  })
  output$tvote = renderText({
    data$imdb_votes[which(data$title==selected())]
  })
  output$timdb = renderText({
    data$imdb_score[which(data$title==selected())]
  })
  output$ttmdb = renderText({
    data$tmdb_score[which(data$title==selected())]
  })
  
  
  ############page 2 search picture 
  output$picture <- renderUI({
    req(input$title)
    pic_url <- getPic(input$title)
    print(pic_url)
    fluidRow(
      column(12,tags$div(img(src=pic_url, width="80%"))
      ),
      p(em("Picture Source: https://www.imdb.com/"))
    )
  }) 

  ################# PAGE 3 Map ###########################
  
  Country <- map_data("world") %>% select(1, 2, 3, 5)
  colnames(Country) <- c("long", "lat", "group", "region")
  Country_name <- aggregate(. ~ region, data = Country, mean)
  
  
  providers<- read_csv("111.csv")
  providers$Year <- as.character(providers$Year)
  
  world = map_data("world")
  world= world%>%group_by(region)%>%slice_head(n=1)
  
  d = left_join(providers,Country_name,by="region")
  c=d[d$Year ==2022,]
  
  
  
  p <- ggplot(data = Country, aes(x = long, y = lat, group = group)) +
      geom_polygon(fill = "gray90",
                   color = "gray",
                   size = 0.1)
  
  output$plot1 <- renderPlotly({ 
      data = d %>%
          filter(as.numeric(Year) == input$yearSlider)
      p +
          geom_point(
              position = "jitter",
              data = data,
              aes(
                  x = long,
                  y = lat,
                  size = Number,
                  color =Number
              )
          ) +
          scale_size(name = "", range = c(-0.8, 6.5)) +
          guides(size = guide_legend("Number")) +
          labs(title = "Numbers of Movie Each Country Made") +
          theme(
              plot.title = element_text(colour = "#2C5364"),
              plot.background = element_rect(fill = "transparent"),
              panel.background = element_rect(fill = "transparent")
          ) 
          
  }) 
  
  
  output$plot2 <- renderPlotly({ 
      data = d %>%
          filter(as.numeric(Year) == input$year)
      p +
          geom_point(
              position = "jitter",
              data = data,
              aes(
                  x = long,
                  y = lat,
                  size = rating,
                  color =rating
              )
          ) +
          scale_size(name = "", range = c(-0.8, 6.5)) +
          guides(size = guide_legend("Rating")) +
          labs(title = "Numbers of Movie Each Country Made") +
          theme(
              plot.title = element_text(colour = "#2C5364"),
              plot.background = element_rect(fill = "transparent"),
              panel.background = element_rect(fill = "transparent")
          ) 
  }) 
  

  ################# PAGE 5 & PAGE 6 trend ###########################
  output$plot33 <- renderPlotly({
      yr_be <- input$be_yr
      yr_en <- input$en_yr
      data2 <- data1 %>%
          filter(release_year >= yr_be) %>%
          filter(release_year <= yr_en) %>%
          group_by(release_year) %>%
          summarise(
              num_movie = n(),
              avg_runtime = mean(runtime),
              avg_imdb_score = mean(imdb_score),
              avg_imdb_votes = mean(imdb_votes),
              avg_tmdb_popularity = mean(tmdb_popularity)
          ) 
      
      #### plot33
      if (yr_be > yr_en) {
          ggplot(NULL) +
              annotate("text",
                       label = "No Graph, since the end year is less than the begin year",
                       x = 1, y = 1, size = 6
              ) +
              theme_bw() +
              theme(
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  legend.position = "none"
              )
      } else {
          index <- input$select11
          xlable <- seq(yr_be, yr_en, by = 5)
          f = 
              ggplot(data = data2) +
              aes_string(x = "release_year", y = index) +
              geom_line(color="black") + 
              geom_point(color="red",size=1) +
              labs(
                  x="Time"
              ) +
              theme_grey()
          ggplotly(f)
      }
  })
  
  #### plot31
  output$plot31 <-renderWordcloud2({
      wcdata4 <-wcdata3 %>%
          filter(release_year == input$Plot31Input) %>%
          group_by(genre) %>%
          summarise(n =n()) %>%
          arrange(n) %>%
          mutate(genre = factor(genre, levels = genre)
          )
      if (nrow(wcdata4)==0) {
          wordcloud2(NULL) +
              annotate("text",
                       label = "No Trend",
                       x = 1, y = 1, size = 6
              ) +
              theme_bw() +
              theme(
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  legend.position = "none"
              )
      } else {
          wordcloud2(
              wcdata4,size=1.5, color='random-light', backgroundColor="black"
          )
      }
  }
  )
  
  #### plot32
  output$plot32 <-renderWordcloud2({
      wcdata5<-wcdata3 %>%
          filter(release_year == input$Plot32Input) %>%
          filter(production_countries == input$select22) %>%
          group_by(genre) %>%
          summarise(n =n()) %>%
          arrange(n) %>%
          mutate(genre = factor(genre, levels = genre))
      
      if (nrow(wcdata5)==0) {
          ggplot(NULL) +
              annotate("text",
                       label = "No Trend",
                       x = 1, y = 1, size = 6
              ) +
              theme_bw() +
              theme(
                  plot.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  legend.position = "none"
              )
      } else {
          wordcloud2(
              wcdata5,size=1.4, color='random-light', backgroundColor="black"
          )
      }
  })
  
  ###################### END of PAGE 6 ################################
  
  
    ####input and out put for filter page 7############  
    data0.input = reactive({
        input$actButton
        movieout=
            data0%>%
            filter(release_year>=isolate(input$Mstart) & release_year<=isolate(input$Mend))%>%
            filter(imdb_score>=isolate(input$Mrating))%>%
            filter(runtime<=isolate(input$Mruntime))%>%
            filter(title %in% unlist(wcdata3%>%
                                         filter(genre==isolate(input$Mgenre))%>%
                                         select(title)))%>%
            filter(Country==isolate(input$Mcountry))%>%
            filter(age_certification==isolate(input$Mage))%>%
            arrange(desc(imdb_score))
    })
    
    output$pic1 <- renderUI({
        req(data0.input()$title[1])
        pic_url <- getPic(data0.input()$title[1])
        print(pic_url)
        fluidRow(
            column(12,tags$div(img(src=pic_url, width="80%"))
            )
        )
    })
    
    output$title1 = renderText({
        data0.input()$title[1]
    })
    
    output$pic2 <- renderUI({
        req(data0.input()$title[2])
        pic_url <- getPic(data0.input()$title[2])
        print(pic_url)
        fluidRow(
            column(12,tags$div(img(src=pic_url, width="80%"))
            )
        )
    })
    
    output$title2 = renderText({
        data0.input()$title[2]
    })
    
    output$pic3 <- renderUI({
        req(data0.input()$title[3])
        pic_url <- getPic(data0.input()$title[3])
        print(pic_url)
        fluidRow(
            column(12,tags$div(img(src=pic_url, width="80%"))
            )
        )
    })
    
    output$title3 = renderText({
        data0.input()$title[3]
    })
    
    output$pic4 <- renderUI({
        req(data0.input()$title[4])
        pic_url <- getPic(data0.input()$title[4])
        print(pic_url)
        fluidRow(
            column(12,tags$div(img(src=pic_url, width="80%"))
            )
        )
    })
    
    output$title4 = renderText({
        data0.input()$title[4]
    })
    
    output$pic5 <- renderUI({
        req(data0.input()$title[5])
        pic_url <- getPic(data0.input()$title[5])
        print(pic_url)
        fluidRow(
            column(12,tags$div(img(src=pic_url, width="80%"))
            )
        )
    })
    
    output$title5 = renderText({
        data0.input()$title[5]
    })
    
    output$pic6 <- renderUI({
        req(data0.input()$title[6])
        pic_url <- getPic(data0.input()$title[6])
        print(pic_url)
        fluidRow(
            column(12,tags$div(img(src=pic_url, width="80%"))
            )
        )
    })
    
    output$title6 = renderText({
        data0.input()$title[6]
    })
    
    output$pic7 <- renderUI({
        req(data0.input()$title[7])
        pic_url <- getPic(data0.input()$title[7])
        print(pic_url)
        fluidRow(
            column(12,tags$div(img(src=pic_url, width="80%"))
            )
        )
    })
    
    output$title7 = renderText({
        data0.input()$title[7]
    })
    
    output$pic8 <- renderUI({
        req(data0.input()$title[8])
        pic_url <- getPic(data0.input()$title[8])
        print(pic_url)
        fluidRow(
            column(12,tags$div(img(src=pic_url, width="80%"))
            )
        )
    })
    
    output$title8 = renderText({
        data0.input()$title[8]
    })
    
    output$yearwarning = renderText({
        if (input$Mstart > input$Mend) {
        "The selected end year is earlier than the begin year"}
    })
    
    output$NAwarning = renderText({
        if (is.na(data0.input()$title[1])== TRUE) {
            "No movies available. Please select again."}
    })
    
    
}


shinyApp(ui = ui, server = server)

