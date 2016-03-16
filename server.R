
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
  get_thousands<-reactive({
    K=input$K;
    modifiers<-get_modifiers()
    thousands<-replicate(n,resultado_tras_sede(teams=teams,games=games,K=K,modifiers=modifiers))
    return(thousands);
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
    #thousands<-get_thousands();
    K=input$K;
    slopePlot(modifiers=modifiers,K=K)
   
  })
  
  output$picksPlot<-renderPlot({
    thousands<-get_thousands()
    ranks<-apply(thousands,2,function(c){rank(-c,ties.method = 'max')})
    df<-data.frame(team=character(0),rank=numeric(0),freq=numeric(0))
    for(t in names(teams)){
      t_count<-count(ranks[t,])/n*100
      t_count<-cbind(rep(t,nrow(t_count)),t_count);
      colnames(t_count)<-c('team','rank','freq');
      df<-rbind(df,t_count)
    }
    ggplot(df, aes(x=rank,y=freq,fill=team))+
            geom_density(stat='identity')+facet_grid(team~.)+
            teams_scale+
            xlab('Posicion')+ylab("%")+
            theme_bw()+
      theme(axis.title.y=element_text(margin=margin(0,10,0,0),size=18))+
      theme(axis.title.x=element_text(margin=margin(20,0,0,0),size=18))
     
  })
  
  output$barsPlot <-renderPlot({
  thousands<-get_thousands()
  lranks<-lapply(1:6,function(n){count(apply(thousands,2,function(c){names(sort(c,decreasing = T))[n]}))})
  names(lranks)<-as.character(1:6);
  count_ranks<-ldply(lranks,.id='rank')
  #print(count_ranks)
  ggplot(count_ranks, aes(x=rank,y=freq/n*100,fill=x))+
          geom_bar(stat='identity',position='stack',color='black')+
          teams_scale+
          xlab('Posicion')+ylab("%")+
          theme_bw()+
          theme(axis.title.y=element_text(margin=margin(0,10,0,0),size=18))+
          theme(axis.title.x=element_text(margin=margin(20,0,0,0),size=18))
  
  })
})
