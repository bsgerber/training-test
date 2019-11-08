# Shiny app to explore Arctic Data Center data

library(shiny)
library(ggplot2)

# Load data from Arctic Data Center
data_url <- "https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A35ad7624-b159-4e29-a700-0c0770419941"
bg_chem <- read.csv(data_url, stringsAsFactors = FALSE)
str(bg_chem)

mychoices <- c("Salinity" = "CTD_Salinity",
               "Temperature" = "CTD_Temperature")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("North Pole Environmental Observatory Bottle Chemistry Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("depth",
                        "Depth Range:",
                        min = 1,
                        max = 500,
                        value = c(1, 100)
                        ),
            selectInput("myvar",
                        "Variable:",
                        choices = mychoices)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("myPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$myPlot <- renderPlot({
        # generate plot
        ggplot(bg_chem, mapping = aes_string(x = "CTD_Depth", y = input$myvar)) +
            geom_point(color = "green", size = 4) +
            xlim(c(input$depth[1], input$depth[2])) +
            ylab(names(mychoices)[mychoices == input$myvar]) +
            xlab("Depth") +
            theme_light()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
