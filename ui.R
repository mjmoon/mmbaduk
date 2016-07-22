#############################################################
# mmbaduk - Baduk (Go) player by michael moon               #
#                                                           #
# Shiny UI                                                  #
#                                                           #
#############################################################
shinyUI(fluidPage(
  titlePanel("mmbaduk v1.5"),
  fluidRow(
    column(4,
           selectInput("mode", "Select a game mode.", 
                        list("Player vs. player" = 1
                             , "Player vs. computer" = 2
                             , "Computer vs. player" = 3),
                       width = "100%"
                       )
    ),
    column(4,
           selectInput("size", "Select the board size.",
                        list("19x19" = 19, 
                             "9x9" = 9, 
                             "5x5" = 5),
                       width = "100%")),
    column(4,
           selectInput("komi", "Select a komi.",
                        list(7.5, 6.5, 5.5, 0.5),
                       width = "100%"))
    ),
  actionButton("start", label = "Start", width = "100%"),
  br(),br(),
  sidebarLayout(
    position = "right",
    sidebarPanel(
       align = "center",
       h5(textOutput("status")),
       shinyjs::useShinyjs(),
       actionButton("play", label = "Play", width = "100%"),
       br(),br(),
       actionButton("skip", label = "Pass", width = "100%"),
       br(),br(),
       actionButton("quit", label = "Resign", width = "100%"),
       uiOutput("printinstui")
       ),
     mainPanel(
       align = "center",
       shinyjs::useShinyjs(),
       htmlOutput("inst", align = "left", width = "100%"),
       plotOutput("game", click = "clickpos", height = "auto"),
       fluidRow(column(6, strong("Black   "), br(),
                       textOutput("on.b", inline = TRUE), " on board   |   ",
                       textOutput("terr.b", inline = TRUE), " in territory   |   ",
                       textOutput("cap.b", inline = TRUE), "captured", 
                       align = "center"),
                column(6,strong("White   "), br(),
                       textOutput("on.w", inline = TRUE), " on board   |   ",
                       textOutput("terr.w", inline = TRUE), " in territory   |   ",
                       textOutput("cap.w", inline = TRUE), "captured", align = "center")
                ),
       br()
       )
     )
    )
  )
