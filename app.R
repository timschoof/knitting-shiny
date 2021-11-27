# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

## load packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(owidR) # devtools::install_github("piersyork/owidR")
library(janitor)
library(shiny)
library(tinytex)
library(magick)

# load helper functions
source("utils.R")

# specify pattern dimensions
nstitches = 200
nrows = 40
# specify number of knit rows at the start and end of the pattern (i.e. no bobbles)
nknit_start = 2
nknit_end = 2
# specify stitch location of first bobble
first_bobble = 10
# jitter amount (this variable is used if jitter == "yes")
jitter_amount = 3

# load data from https://ourworldindata.org/maternal-mortality
data <- owid("number-of-maternal-deaths-by-region")

# compute number of maternal deaths per minute in the world
d <- data %>%
    clean_names() %>%
    filter(code == "OWID_WRL",
           year == max(year)) %>%
    mutate(event_per_min = number_of_maternal_deaths/525960) # 525960 minutes per year

# set bobble frequency to number of maternal deaths per minute in the world
event_per_min = d$event_per_min

## Define UI for application
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "sandstone"),
    titlePanel("Knitting maternal mortality rates: an infinity scarf pattern"),
    sidebarLayout(
        sidebarPanel(
            h4("Personalize the knitting experience"),
            # specify stitch rate (typical is 20-30 stitches per minute)
            numericInput('stitch_rate', 'How many stitches do you knit per minute?', value = 20, min = 0, max = 100),
            # indicate whether you would like to jitter the location of the bobbles
            radioButtons("jitter", "Do you want to jitter the location of the bobbles?", c("yes","no")),
            h6(strong("Materials")),
            tags$li("Medium or worsted weight yarn"),
            tags$li("US size 9 (5.5 mm) circular knitting needles"),
            tags$li("Stitch marker"),
            h6(strong("Gauge")),
            p("16 sts / 21 rows = 4 in (10 cm). Use any size needles to obtain gauge."),
            h6(strong("Scarf dimension")),
            p("48 in (122 cm) by 8 in (20 cm)"),
            h6(strong("Estimated knitting time")),
            textOutput("knittingTime"),
            p("\n"),
            downloadButton("downloadPattern", "Download the pattern")
        ),
        mainPanel(
            tabsetPanel(id = "panels",
                tabPanel("About",
                         h3("About the pattern"),
                         p("This knitting pattern is designed to visceralize how often mothers still die in childbirth around the world. The pattern is based on data from", tags$a(href = "https://ourworldindata.org/maternal-mortality", "Our World in Data", target = "blank"),". In 2017, 295,000 people died in childbirth around the world. That comes down to one human life lost approximately every two minutes. While maternal mortality rates have certainly declined over the last century, too many people still lose their lives. As Our World in Data points out, 'if we can make maternal deaths as rare as they are in the healthiest countries we can save almost 300,000 mothers each year'. Maternal deaths are represented in this pattern by", tags$a(href="https://www.thesprucecrafts.com/three-dimensional-texture-by-making-bobbles-2116336", "bobbles", target = "blank"), "that are spaced roughly 2 minutes of knitting apart. The bobbles will take longer to knit than the regular stitches, slowing you down, forcing you to reflect on the fact that every data point, every bobble, represents a real human being that lost their life in childbirth."),
                         p("The idea behind using a knitting pattern to represent data is that it really allows you to slow down and experience the data in real time. It is as much about the process, the experience, as it is about the end product. The end product, by the way, is an infinity scarf. The idea behind the infinity scarf is that it stresses the fact that the events continue to unfold, even when you are not knitting."),
                         p("The original pattern assumes an average knitting pace of 20 stitches per minute. However, you can personalize this pattern based on your knitting speed. You can also specify whether you want the bobbles regularly spaced, or whether you want to add a bit of jitter to the data (have a look at the", actionLink("link_to_viz", "visual representation"), "for the different effects)."),
                         h3("Author"),
                         p("This pattern was created by Tim Schoof for the Data Science By Design Anthology.")),
                tabPanel("Knitting instructions", 
                         h3("Stitches"),
                         tags$li(strong("k:"), "knit"),
                         tags$li(strong("p:"), "purl"),
                         tags$li(strong("mb (make bobble):"), "k1, p1, k1, p1, k1 into next stitch, turn, p5, turn, k5, pass the 4 stitches one at a time over the knit stitch and off the needle to finish bobble"),
                         h3("Pattern"),
                         p("Cast on 200 stitches. Be careful not to twist any stitches when joining knitting in the round. Place a marker at the beginning of the round."),
                         tableOutput("pat")),
                tabPanel("Visual representation", 
                         h3("The scarf"),
                         p("Play around with the settings on the left to see what your scarf would look like if you decided to jitter the location of the bobbles (or not), or if you tried to knit really fast or really slow. The dots indicate the approximate position of the bobbles on the scarf."),
                         plotOutput("plot"))
            )
        )
    )
)

## Define server logic
server <- function(input, output, session) {
    # some basic computations
    tot_bobble_rows <- reactive(nrows - nknit_start - nknit_end)
    
    # estimate knitting time
    output$knittingTime <- renderText(
        paste("about", 
              round((((nstitches*nrows) +
                          (15*(round((nstitches*nrows)/bobble_freq())))) /
                         input$stitch_rate)/60),
              "hours", sep = " "))
    
    # compute bobble frequency
    bobble_freq <- reactive(round(input$stitch_rate / event_per_min))
    
    # calculate bobble locations
    df <- reactive(set_bobble_locations(first_bobble,
                                        nstitches,
                                        tot_bobble_rows(),
                                        bobble_freq(),
                                        nknit_start,
                                        input$jitter,
                                        jitter_amount, 
                                        input$method))
    
    # plot knitting pattern
    output$plot <- renderPlot(plot_pattern(df(), nrows, nstitches, input$method), alt = "Rectangular image showing the relative position of the bobbles on the infinity scarf")

    # provide knitting instructions in table
    dft <- reactive(inter_bobble_timing(df(), nstitches))
    output$pat <- renderTable(write_pattern(dft(), 
                                            nrows, 
                                            nstitches, 
                                            nknit_start, 
                                            nknit_end),
                              colnames = FALSE,
                              striped = TRUE)
    
    # clickable link to "visual representation" tab
    observeEvent(input$link_to_viz, {
        newvalue <- "Visual representation"
        updateTabsetPanel(session, "panels", newvalue)
    })
    
    # download pattern
    output$downloadPattern <- downloadHandler(
        filename = "knittingPattern.pdf",
        content = function(file) {
            # copy Rmd to temporary directory
            tempReport <- file.path(tempdir(), "knittingPattern.Rmd")
            file.copy("knittingPattern.Rmd", tempReport, overwrite = TRUE)
            # indicate to user that it's rendering the pattern
            id <- showNotification(
                "Generating document...", 
                duration = NULL, 
                closeButton = FALSE
            )
            on.exit(removeNotification(id), add = TRUE)
            # specify parameters to pass to Rmd file
            params = list(stitch_rate = input$stitch_rate,
                          jitter = input$jitter)
            # render pdf
            rmarkdown::render(tempReport,
                              output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv()))
        }
    )
}

## Run the application 
shinyApp(ui, server)
