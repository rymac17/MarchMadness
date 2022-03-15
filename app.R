
# load packages ----
library(shiny)
library(dplyr)
library(gridExtra)
library(glmnet)
library(xlsx)

# load data ----
source('src/ncaaHelpers.R')
# read model
cv_outcome <<- readRDS('data/models/cv_outcome_2022.rds')
# read tables
masterTBL <<- readRDS('data/masterTBL.rds')
statsTBL <<- readRDS('data/statsTBL.rds')
teams <<- readRDS('data/teams2022.rds')
# hyperparameters
yr <<- 2022


# user interface ----
ui <- fluidPage(
  h2(HTML('<b>March Madness</b>')),
  h4(HTML('<i>Modeling each possible game</i>')),
  
  tabsetPanel(id='tab_display',
              tabPanel('Predict Game', value='pred_game',
                       sidebarLayout(
                         sidebarPanel(
                           fluidRow(
                             column(width=12, 
                                    h4(HTML('<b>Select two teams to simulate a game</b><br/>
                                            <i>(2022 season)</i>')),
                                    # team 1
                                    selectInput(inputId='team1',
                                                label='Team 1', 
                                                selected=NULL,
                                                choices=statsTBL$Team %>% sort()),
                                    # team 2
                                    selectInput(inputId='team2',
                                                label='Team 2',
                                                selected=NULL, 
                                                choices=statsTBL$Team %>% sort()),
                                    numericInput(inputId='ss1',
                                                 label='Choose number of simulations', 
                                                 value=1),
                                    actionButton(inputId='go',
                                                 label='Submit'),
                                    br(),
                                    br(),
                                    actionButton(inputId='clear',
                                                 label='Clear Table'),
                                    br(),
                                    br(),
                                    h4(HTML('<b><em>About</em></b>')),
                                    h5(HTML('<b>Submit:</b><br/>
                                            Run simulation between two teams. Every submission is saved in the table 
                                            until the table is cleared.')),
                                    h5(HTML('<b>Choose number of simulations:</b><br/>
                                            This option produces a winner based on random sample weighted by the
                                            normalized win probability that each team wins. Selecting 1 yields 
                                            the most variability, while higher numbers of simulations are more likely
                                            to result in the team with the higher normalized win probability winning. 
                                            Selecting 0 will select the team with the higher normalized win probability.')),
                                    h5(HTML('<b>Win Probability:</b><br/>
                                            The modeled likelihood that a given team can win the simulated game. 
                                            This number does not usually add up to 1 for each simulation. 
                                            Instead, normalized win probability is used to estimate the 
                                            percent chance each team has of winning the simulation.'))
                             )
                           )
                         ),
                         mainPanel(
                           uiOutput(outputId='winner')
                         )
                       )
              ),
              tabPanel('Predict Bracket', value='pred_brkt',
                       sidebarLayout(
                         sidebarPanel(
                           fluidRow(
                             column(width=12,
                                    h4(HTML('<b>Project a bracket</b><br/>
                                            <i>(2021 season)</i>')),
                                    numericInput(inputId='ss2',
                                                 label='Choose number of simulations',
                                                 value=1),
                                    actionButton(inputId='dance',
                                                 label="Let's Dance!"),
                                    br(),
                                    br(),
                                    h4(HTML('<b><em>About</em></b>')),
                                    h5(HTML('<b>Choose number of simulations:</b><br/>
                                            This option produces a winner based on random sample weighted by the
                                            normalized win probability that each team wins. Selecting 1 yields 
                                            the most variability, while higher numbers of simulations are more likely
                                            to result in the team with the higher normalized win probability winning. 
                                            Selecting 0 will select the team with the higher normalized win probability.')),
                                    h5(HTML('<b>Team (%):</b><br/>
                                            For each team predicted to advance, there is a percent value representing the 
                                            cumulative modeled probability of the team winning each of the proceeding games. 
                                            The lower the value, the less likely the outcome. However, some outcomes, 
                                            like winning a championship, are unlikey for all teams. 
                                            Event the top seeds rarely have >50% chance.'))

                             )
                           ), width=2
                         ),
                         mainPanel(
                           uiOutput(outputId='bigdance')
                         )
                       )
              )
  )
)


# server ---
server <- function(input, output) {
  
  # build matchup on click
  gameTable <- reactiveValues(DFT=NULL)
  observeEvent(input$go, {
    if (input$team1=='' | input$team2==''){
      return(NULL)
    } else {
      gameTable$DFT <- rbind(gameTable$DFT,
                             SIMgame(tbl1=input$team1, tbl2=input$team2, SS=input$ss1, alacarte=T) %>% 
                               rename(`Team 1`=team_1, `Team 2`=team_2,
                                      `Win Probability (Team 1)`=winP_1, `Win Probability (Team 2)`=winP_2,
                                      `Normalized Win Probability`=winNorm,
                                      `Projected Winner`=Winner)
      )
    }
  })
  observeEvent(input$clear, {
    gameTable$DFT <- NULL
  })
  output$winner <- renderTable(gameTable$DFT)
  
  
  # bracket page before click
  output$bigdance <- renderUI({
    textOutput('none')
  })
  output$none <- renderText({
    return(paste("Let's Dance! to generate bracket"))
  })
  
  # generate bracket on click
  observeEvent(input$dance, {
    sampleSize <<- input$ss2
    r <- mkBracket(use_historic=F)
    predDFT <- data.frame()
    for (i in 1:6){
      r <- runRND(r, i)
      predDFT <- rbind(predDFT, r %>%
                         dplyr::select(round, region_number, region_name=Region, winner=Team, Seed, Prob=CumWinPct))
    }
    new <- assignBRKT(t=teams, pdft=predDFT)
    
    output$bigdance <- renderUI({
      imageOutput('BRKT')
    })
    output$BRKT <- renderImage({
      outfile <- tempfile(fileext='.png')

      png(outfile, width=18, height=26, units='in', res=72)
      grid.table(new)
      dev.off()
  
      list(src = outfile,
           contentType = 'image/png',
           alt = "This is alternate text")
    }, deleteFile=T)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
