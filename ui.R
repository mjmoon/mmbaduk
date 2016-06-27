#############################################################
# mmbaduk - Baduk (Go) player by michael moon               #
#                                                           #
# Shiny UI                                                  #
#                                                           #
#############################################################
shinyUI(fluidPage(
  titlePanel("mmbaduk v0.1"),
  fluidRow(
    column(3,
           selectInput("mode", "Select a game mode.", 
                        list("Player vs. player" = 1
                             , "Player vs. computer" = 2
                             , "Computer vs. player" = 3
                             )
                       )
    ),
    column(3,
           selectInput("size", "Select the board size.",
                        list("19x19" = 19, 
                             "9x9" = 9, 
                             "5x5" = 5))),
    column(3,
           selectInput("komi", "Select a komi.",
                        list(7.5, 6.5, 5.5, 0.5))),
    
    column(3,
      shinyjs::useShinyjs(),
      actionButton("start", label = "Start")
    )
  ),
  
  fluidPage(
    sidebarLayout(
     sidebarPanel(
       width = 2,
       h5(textOutput("status")),
       shinyjs::useShinyjs(),
       textInput("pos", NULL, width = "100%"),
       actionButton("play", label = "Play stone", width = "100%"),
       br(),br(),
       actionButton("skip", label = "Skip turn", width = "100%"),
       br(),br(),
       actionButton("quit", label = "Quit game", width = "100%")
     ),
     mainPanel(
       plotOutput("game"),
       fluidRow(
         column(3),
         column(3, h5("On board")),
         column(3, h5("Territory")), 
         column(3, h5("Captured"))
         ),
       fluidRow(
         column(3,div("Black: ", br(),"White: ")),
         column(3,
                div(textOutput("on.b", inline = TRUE), br(),
                    textOutput("on.w", inline = TRUE))
         ),
         column(3,
                div(textOutput("terr.b", inline = TRUE), br(),
                    textOutput("terr.w", inline = TRUE))
         ), 
         column(3,
                div(textOutput("cap.b", inline = TRUE), br(),
                    textOutput("cap.w", inline = TRUE))
         )
       )
     )
    )
  )
))