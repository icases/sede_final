library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)

my_teams<-c("Chiclana"=18,"Cartagena"=17,"Sevilla"=13,"Fuengirola"=9,"Aguilas"=9,"Don Patin"=6);
my_games<-list(c("Sevilla","Aguilas"),c("Fuengirola","Cartagena"),c("Chiclana","Don Patin"),c("Cartagena","Sevilla"),c("Aguilas","Chiclana"),c("Don Patin","Fuengirola"))
my_modifier<-c("Chiclana"=-2,"Cartagena"=-2,"Sevilla"=2,"Fuengirola"=2,"Aguilas"=-2,"Don Patin"=2);

teams_scale<-scale_fill_manual(values = c('Chiclana'='#59e419',
                                          'Cartagena'='#f4ff00',
                                          'Sevilla'='#fff3fd',
                                          'Aguilas'='#26269B'
                                          ,'Fuengirola'='#367FF9'
                                          ,'Don Patin'='#262626'))

p_val_dif<-function(dif,K=.3){
  return((1/(1+exp(-dif*K))))
}

p_val_game<-function(g,teams=my_teams,K=.3){
  #print(K)
  l=g[1];
  v=g[2];
  
  pl=teams[l];
  pv=teams[v];
  
  p_val_dif(pl-pv,K=K)
}

winner_game<-function(g,teams=my_teams,K=.3){
  return(ifelse(runif(1)<p_val_game(g,teams=teams,K=K),g[1],g[2]))
}

modify<-function(teams=my_teams,modifiers=my_modifier){
  return(teams*2^modifiers)
}

resultado_tras_sede<-function(teams=my_teams,games=my_games,modifiers=NULL,K=.3){
  res=teams; 
  if(!is.null(modifiers)) teams=modify(teams,modifiers)
  winners<-sapply(games,winner_game,teams=teams,K=K)
  for(winner in winners){
    res[winner]<-res[winner]+3;
  }
  #res=sort(res,decreasing = T)
  return(res)
}

slopePlot<-function(teams=my_teams,games=my_games,modifiers=NULL,K=.3){
  lteams<-teams;
  if(!is.null(modifiers)) lteams=modify(teams,modifiers)
  df_games<-as.data.frame(do.call(rbind, games));
  dif=apply(df_games,1,function(x)lteams[x[1]]-lteams[x[2]])
  maxDiff=max(abs(dif))
  dif_range=c(-maxDiff:maxDiff);
  pval=sapply(dif_range,p_val_dif,K=K)
  df=data.frame(dif=dif_range,pval=pval);
  colnames(df_games)<-c('local','visitante')
  df_games<-cbind(df_games,dif);
  qplot(dif,pval,data=df,geom='line')+ylim(0,1)+theme_minimal()+
    geom_point(data=df_games,aes(x=dif,y=p_val_dif(dif,K)),color='red')+
    xlab("Diferencia de puntos (local - visitante)")+
    ylab("Probabilidad de victoria del equipo local")+
    geom_text_repel(data=df_games,aes(label=paste(local,visitante,sep='-'),x=dif,y=p_val_dif(dif,K)))+
    theme(axis.title.y=element_text(margin=margin(0,10,0,0),size=18))+
    theme(axis.title.x=element_text(margin=margin(20,0,0,0),size=18))
  
}

get_sede_results<-function(n,teams=my_teams,games=my_games,K=.3,modifiers=NULL){
  replicate(n,resultado_tras_sede(teams=teams,games=games,K=K,modifiers=modifiers))
}

tabulate_sedes<-function(thousands,teams=my_teams){
  n<-ncol(thousands)
  r<-apply(-thousands,2,rank,ties.method='first')
  df<-as.data.frame(t(r)) %>% gather(team,rank) %>% group_by(team,rank) %>% tally() 
  colnames(df)[3]<-'freq';
  df$freq<-df$freq/n*100;
  df$team<-factor(df$team,levels=names(sort(teams,decreasing = T)),ordered = T)
  df
}

densityPlot<-function(df){
  ggplot(df, aes(x=rank,y=freq,fill=team))+
    geom_density(stat='identity')+facet_grid(team~.)+
    teams_scale+
    xlab('Posicion')+ylab("%")+
    theme_bw()+
    theme(axis.title.y=element_text(margin=margin(0,10,0,0),size=18))+
    theme(axis.title.x=element_text(margin=margin(20,0,0,0),size=18))
  
}



barsPlot<-function(df){
  ggplot(df, aes(x=rank,y=freq,fill=team))+
    geom_bar(stat='identity',position='stack',color='black')+
    teams_scale+
    xlab('Posicion')+ylab("%")+
    theme_bw()+
    theme(axis.title.y=element_text(margin=margin(0,10,0,0),size=18))+
    theme(axis.title.x=element_text(margin=margin(20,0,0,0),size=18))
}
