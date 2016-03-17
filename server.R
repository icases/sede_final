
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(shinyjs)
source('functs.R')

shinyServer( function(input, output) {
  n=1000;
  games=my_games;
  teams=my_teams;
  modifiers=NULL;
  observeEvent(input$mod,{
    if (input$mod) {
      shinyjs::enable("sliderContainer")
    } else {
      shinyjs::disable("sliderContainer")
    }
  })
  get_df<-reactive({
    K=input$K;
    modifiers<-get_modifiers()
    thousands<-get_sede_results(n,teams,games,K,modifiers)
    df<- tabulate_sedes(thousands)
    return(df);
  })
  
  get_modifiers<-reactive({
    modifiers=NULL;
    if(input$mod) {modifiers=c("Chiclana"=input$ChiclanaMod,
                               "Cartagena"=input$CartagenaMod,
                               "Sevilla"=input$SevillaMod,
                               "Fuengirola"=input$FuengirolaMod,
                               "Aguilas"=input$AguilasMod,
                               "Don Patin"=input$DonPatinMod
    )
    };
    return(modifiers)
  });
  
  output$slopePlot <- renderPlot({
    modifiers<-get_modifiers();
    K=input$K;
    slopePlot(modifiers=modifiers,K=K)
  })
  
  output$picksPlot<-renderPlot({
    df<-get_df()
    densityPlot(df)
  })
  
  output$barsPlot <-renderPlot({
  df<-get_df()
  barsPlot(df)
  })
  
})
