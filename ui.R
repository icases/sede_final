
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(shinyjs)
shinyUI(fluidPage(
  useShinyjs(),
  #tags$head(tags$script(src="sede.js")),
  tags$head(tags$link(href="http://fonts.googleapis.com/css?family=Oswald",type="text/css",type='text/css',rel='stylesheet')),
  tags$head(tags$link(href="sede.css",type="text/css",type='text/css',rel='stylesheet')),
  title='Quien Ganará la Liga Sur',
  fluidRow(
    column(12,style='border-bottom:10px solid #ff9f0c;margin-bottom:2em',
           h1(img(src='ligasur.png',class='img-responive',style='height:150px;width:auto'),"¿Quien Ganará la Liga Sur?",style='font-size:36pt'),
           p(class='lead',"Utilizando la diferencia de puntos en la clasificación
             podemos intentar predecir el resultado de los partidos de la próxima sede, y por tanto la clasificacion final")
           ,
           p(tags$b("Que es esto?"),"Es una applicación que acompaña a ",a("esta entrada en el blog LgSmallData",href='http://lgsmalldata.madcases.es/last'))
    )    
  ),
  fluidRow(
    column(7,
           tabsetPanel(
             tabPanel("Partidos",
                      p("Basandonos en la diferencia de puntos en la clasificacion
                        y un conjunto de modificadores para reflejar las condiciones
                        particulares de esta sede, podemos calcular la probabilidad 
                        de cada equipo de ganar su partidos."),
                      h2("Probabilidad de victoria del Equipo local"),

                      plotOutput("slopePlot")
                      ),
             tabPanel("Equipos",
                      p("Con esas probabilidades, podemos simular la sede 
                        y calcular la clasificación final*. Si hacemos eso miles de veces
                        podemos calcular una probabildad para la posición que ocupará
                        cada equipo en la clasificación final. Asi por ejemlo, con una K de 0.2, Cartagena
                        tiene una probabilidad de alrededor del 15% de quedar primero, un 80% de 
                        quedar segundo y otro 5% de quedar tercero."),
                      h2("Clasificación final"),
                      plotOutput("picksPlot")
                      ,
                      p(style='font-size:small',"* Este análisis no considera la posibilidad de que los partidos acaben
                        en empate, en cuyo caso la distribución de puntos sería distinta, y en caso de empate a puntos 
                        en la clasifición final asigna los puestos en el orden actual, ya que en este modelo 
                        no se predice el gol average de cada equipo.")
             ),
             tabPanel("Posiciones",
                      p("Otra manera de representar esto es, para cada posición, calcular la probabilidad 
                        de cada equipo de ocuparla. Así, de nuevo con una K de 0.2, hay alrededor de un 85% 
                        de posibilidades de que gane Sancti Petri y un 15% de que gane Cartagena."),
                      h2("Clasificación final"),
                      plotOutput("barsPlot") 
             )
             )
             )
  ,
  
  # Sidebar with a slider input for number of bins
  
  column(5,
         wellPanel(
    p("K regula como de dependiente es el resultado de un partido de la diferencia de puntos entre los dos equipos. K=0 quiere decir que el resultado es al azar, mientras que  K=1 ( o mayores ) indica que prácticamente siempre ganará el  el que tenga más puntos"),
    
    sliderInput("K",
                "K:",
                min = 0,
                max = 1,
                value = .2)
    
  ),
  wellPanel(
    p("Podemos ajustar la fuerza o debilidad de cada equipo, mediante un factor de corrección. 
      Por ejemplo, podemos suponer que Águilas lo  hará mejor de lo normal por jugar en casa,
      o que Don Patín jugará muy motivado debido a su racha de dos victorias"),
    div(class='center-block',checkboxInput("mod",
                  "Usar modificadores")),
    fluidRow(id='sliderContainer',
      
      column(6,
    sliderInput("ChiclanaMod",
                "Chiclana:",
                min = -2,
                max = 2,
                value = 0,
                step=.1),
    sliderInput("CartagenaMod",
                "Cartagena:",
                min = -2,
                max = 2,
                value = 0,
                step=.1),
    sliderInput("SevillaMod",
                "Sevilla:",
                min = -2,
                max = 2,
                value = 0,
                step=.1)
    ),
    column(6,id='slidersContainer',
    sliderInput("FuengirolaMod",
                "Fuengirola:",
                min = -2,
                max = 2,
                value = 0,
                step=.1),
    sliderInput("AguilasMod",
                "Aguilas:",
                min = -2,
                max = 2,
                value = 0,
                step=.1),
    sliderInput("DonPatinMod",
                "Don Patin:",
                min = -2,
                max = 2,
                value = 0,
                step=.1)
    )
    )
  ))
)))
