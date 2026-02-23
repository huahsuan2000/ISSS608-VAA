pacman::p_load(shiny, tidyverse)

exam <- read_csv("data/Exam_data.csv")

print(exam) 

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Pupils Examination Results Dashboard"), #panel的標題，加完一個東西要逗號
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable",
                  label = "Subject:",
                  choices = c("English" = "ENGLISH",
                              "Maths" = "MATHS",
                              "Science" = "SCIENCE"),
                  selected = "ENGLISH"),   #default
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 5,
                  max = 20,
                  value = 10) #default
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({ #$後需要跟ui那邊mainplot的名字一樣
    ggplot(data = exam,
           aes_string(x = input$variable)) +  #$後的要跟ui的inputId一樣
      geom_histogram(bins = input$bins,   #先寫ggplot再寫geom_，ggplot是tidyverse的核心成員
                     color = "black",
                     fill = "light blue")
  })  
}

# Run the application 
shinyApp(ui = ui, server = server)
