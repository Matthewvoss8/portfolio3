library("shiny")
library("leaflet")
library("tidyverse")
library("shinyWidgets")
library("lubridate")
library("superheat")
tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")
tickets <- tickets %>% 
  separate(issue_datetime,c("date","time"),sep = " ") %>% 
  filter(date<="2017-01-31") %>% 
  mutate(dow=wday(date,label = T)) %>% 
  mutate(date=date(date))
color<-c("springgreen4","yellow1","red2")
wrangler<-function(dateone,datetwo,day,type,priceone,pricetwo){
  tickets %>% 
    filter(date>=dateone) %>% 
    filter(date<=datetwo) %>% 
    filter(dow%in%day) %>% 
    filter(violation_desc%in%type) %>% 
    filter(fine>=priceone) %>% 
    filter(fine<=pricetwo)
}

mapper<-function(df){
  leaflet(data = df) %>% 
    addTiles() %>% 
    setView(lng=-75.1652, lat = 39.9526,zoom = 10) %>% 
    addProviderTiles(providers$Esri.WorldTopoMap) %>% 
    addMarkers(clusterOptions = markerClusterOptions()) 
}

heatmapShiny<-function(df){ #the heat map seems to be glitching and you may need to just replace this plot with some traditional gg
  if(nrow(df)!=0){#It can't accept a null vector or it'll give a length 0 error. 
    df %>% 
      group_by(date,violation_desc) %>% 
      summarise(count = n()) %>% 
      pivot_wider(names_from = violation_desc, values_from = count,values_fill = 0) %>% 
      arrange(desc(date)) %>% 
      separate(date, c("year","month","day")) %>% 
      mutate(day = paste("March, ", day)) %>% 
      select(-c("year","month")) %>% 
      column_to_rownames(var = "day") %>% 
      superheat(heat.pal = color)
  }
}

ui<-fluidPage(
  titlePanel("Philadelphia Parking Tickets in January, 2017"),
  helpText("Examine Parking Tickets by Date, Violation type, and Fine. To examine the map, click on the clusters and see the location of the parking tickets. Warning: the Heatmap will not work if there are less than three rows and two columns."),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date","Select the Date Range of Parking Tickets",format = "yyyy-dd-mm",min = min(tickets$date),
                     max = max(tickets$date),start = min(tickets$date), end = max(tickets$date)),
      pickerInput("day","Select Days of the Week",choices = levels(tickets$dow), multiple = T,
                  options = list(`actions-box` = TRUE), selected = levels(tickets$dow)),
      pickerInput("violation","Select the type of parking violation",choices = unique(tickets$violation_desc), 
                  multiple = T, options = list(`actions-box` = TRUE),selected = unique(tickets$violation_desc)),
      sliderInput("price","Choose the Price Range of the Tickets", min = min(tickets$fine), max = max(tickets$fine),
                  value = c(min(tickets$fine),max(tickets$fine)))
    ),
    mainPanel(
      leafletOutput("plot"),
      plotOutput("heat")
    )
  )
)
server<-function(input,output){
  ticket_subset<-reactive({
    wrangler(input$date[1],input$date[2],input$day,input$violation,input$price[1],input$price[2])
  })
  output$plot<-renderLeaflet({
    mapper(ticket_subset())
  })
  output$heat<-renderPlot({
    heatmapShiny(ticket_subset())
  })
}
shinyApp(ui,server)