library(shiny)
library(ggplot2)
library(lubridate)
library(wordcloud)
library(readr)
library(dplyr)
library(tm)

data.tb <- read_csv("all.csv")

# build dashboard by Rshiny
ui <- fluidPage(
  # set dashboard title
  titlePanel("Twitter users sentiment analysis about the 2020 presidential election"),
  
  #sidebar layout
  sidebarLayout(
    
    
    sidebarPanel(
      
      #set a time controller
      sliderInput(inputId = "time",
                  label = "set the time range of all three plots",
                  min = min(ymd(data.tb$date)),
                  max = max(ymd(data.tb$date)),
                  value = c(min(ymd(data.tb$date)), max(ymd(data.tb$date)))
      ),
      
      
      #implementing radio buttons
      # the first button can help locate certain recently event
      radioButtons("x", "locate the recently events for the line plot",
                   list("No event mark"= "0", 
                        "Sep 22: Trump takes speech in the UN\n\n\n"="2020-09-22",
                        "Sep 29: First presidential debate of 2020"="2020-09-29", 
                        "Oct 03: Trump confirms his positive result for COVID19 test"="2020-10-03", 
                        "Oct 07: The vice presidential debate"= "2020-10-07")),
      
      #the second button decide the woldcloud parameter
      radioButtons("y", "Select filter condition for wordcloud",
                   list("all"='0', "Trump"='biden', "Biden"='trump'))
      
    ),
    
    # mainpanal layout
    mainPanel(
      # the first output: the horizontal stacked bar chart
      # the second output: time-series line chart
      # the third output: wouldcloud
      plotOutput(outputId = "barchart"),
      plotOutput(outputId = "linechart"),
      plotOutput(outputId = "wordcloud")
      
    )
  )
)

server <- shinyServer(function(input, output) {
  output$barchart <- renderPlot({
    data.tb[(data.tb$date >= min(input$time) &data.tb$date <= max(input$time)),] %>% 
      ggplot(aes(x = name, y =..count.., fill = result)) +
      geom_bar(position="fill", stat="count", width = 0.5, alpha = 0.8) +
      ggtitle("Who do Twitter users prefer, Biden or Trump?") +
      scale_fill_manual(values=c("#525254", "#ABABAD", "#940115")) +
      guides(fill = guide_legend(reverse = TRUE)) +
      ylab("") + 
      coord_flip() +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "top",
            legend.title = element_blank())
  })
  
  output$linechart <- renderPlot({
    data.tb[(data.tb$date >= min(input$time) &data.tb$date <= max(input$time)),] %>%
      group_by(date, name, result) %>%
      summarise(N = n()) %>%
      ungroup() %>%
      group_by(date, name) %>%
      mutate(percent = N / sum(N)) %>%
      ungroup() %>%
      ggplot(aes(x = date, y = percent,group = name, color = name)) +
      geom_line(size = 1) +
      scale_y_continuous(limits = c(0,NA),labels = scales::percent) +
      geom_vline(xintercept = ymd(input$x), color = "grey", size = 1.1) +
      facet_grid(result~.) +
      theme_bw()+
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
  output$wordcloud <- renderPlot({
    
    x <-  data.tb[(data.tb$date >= min(input$time) &data.tb$date <= max(input$time)),] %>%
      filter(name != input$y) %>%
      select(textfre)
    x <- VCorpus(VectorSource(x))
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, "When users talk about candidates, what they're talking about?")
    wordcloud(x, max.words = 30, scale=c(4,.5), 
              random.order = FALSE,rot.per=.5)
  })
})

shinyApp(ui, server)