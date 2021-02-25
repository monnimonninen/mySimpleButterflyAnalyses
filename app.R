# varianssianalyysikuvaaja tunnuslukuineen

library(shiny)  # shiny
library(readr)   # read_csv
library(ggplot2)
library(tidyverse)





var7_factors <- read.csv(file.path("monarkki_7var_f.csv"))

siiven_pituus <- as.factor(var7_factors$siiven_pituus)
sukupuoli <- as.factor(var7_factors$sukupuoli)
siiven_syvyys <- as.factor(var7_factors$siiven_syvyys)
siiven_pinta_ala <- as.factor(var7_factors$siiven_pinta_ala)
siiven_pyoreys <- as.factor(var7_factors$siiven_pyoreys)
lento_km <- var7_factors$lento_km

monarkki_7var_f <- data.frame(
  siiven_pituus = siiven_pituus,
  siiven_syvyys = siiven_syvyys,
  siiven_pyoreys = siiven_pyoreys,
  siiven_pinta_ala = siiven_pinta_ala,
  lento_km = lento_km,
  sukupuoli = sukupuoli)



# shiny user interface
ui <- fluidPage(
                
           sidebarLayout(
             sidebarPanel(
               
                         titlePanel("Valitse kuvaajaan ominaisuus"),
                         
                         radioButtons( #valitaan yksi perhosen ominaisuus
                              inputId="xcol",
                              label = "",
                              choices = c("siiven_pituus","siiven_syvyys","siiven_pyoreys","siiven_pinta_ala"),
                              selected = "siiven_pituus",
                              # inline = FALSE,
                              # width = NULL,
                              # choiceNames = NULL,
                              # choiceValues = NULL,
                              ),
                         
                         hr(),
                         
                         textInput( #muistiinpanot
                           inputId = "muistiinpanot", 
                           label = em("Voit kirjoittaa lyhyen muistiinpanon"), 
                           value = "",
                           width = NULL, 
                           placeholder = NULL
                           ),
                         
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         
                         
                         titlePanel("Tunnusluvut (MANOVA)"),
                         
                         fluidRow(
                         column(5,
                         
                               selectInput( #1. perhosen ominaisuus
                                 inputId="sarakeA",
                                 label = "1.",
                                 choices = c("siiven_pituus","siiven_syvyys","siiven_pyoreys","siiven_pinta_ala"),
                                 selected = NULL,
                                 multiple = FALSE)  #selection of multiple items is not allowed
        
                         ),
                         
                         column(5, ofset = 6,
                         
                               selectInput( #2. perhosen ominaisuus
                                 inputId="sarakeB",
                                 label = "2.",
                                 choices = c("siiven_pituus","siiven_syvyys","siiven_pyoreys","siiven_pinta_ala"),
                                 selected = NULL,
                                 multiple = FALSE)
                          
                         ),
                         
                         column(5, ofset = 4,
                         
                               selectInput( #3. perhosen ominaisuus
                                 inputId="sarakeC",
                                 label = "3.",
                                 choices = c("siiven_pituus","siiven_syvyys","siiven_pyoreys","siiven_pinta_ala"),
                                 selected = NULL,
                                 multiple = FALSE)
                           
                         ),
                         
                         column(5, ofset = 2,
                         
                               selectInput( #4. perhosen ominaisuus
                                 inputId="sarakeD",
                                 label = "4.",
                                 choices = c("siiven_pituus","siiven_syvyys","siiven_pyoreys","siiven_pinta_ala"),
                                 selected = NULL,
                                 multiple = FALSE)
                           
                         )),
                      )
             
                 , #pilkku inputin ja outputin valiin
             
                   #outputit tarvitsevat iu-lohkossa vain nimen
                   mainPanel( 
                         
                     headerPanel('Monarkkiperhosen lentomatkan estimointi'),
                     
                         fluidRow(
                           column(8,
                            plotOutput("kuvaaja"))
                         ),
                     
                            br(),
                            br(),
                            br(),
                             
                         fluidRow(
                           column(7,
                                verbatimTextOutput("tunnusluvut"))
                          ),
                     
                           column(3,
                                imageOutput("perhonen.png")),
                                
                     
                         fluidRow( 
                           column(4,
                                textOutput("muistiinpanot"))
                         ),
                     
                          
              )#for sidebarLayout
             )#for sidebarPanel
)


             

#ohjeet palvelimen koneelle
server <- function(input, output) {
                                  
                    #kaarisulkeet renderin sisalla antaa liittaa sisalle isommankin koodilohkon
                    output$kuvaaja <- renderPlot({
                      
                      #"apu" tible-taulukko hajontapisteiden muodostusta varten
                      mean.lentomatka <- monarkki_7var_f %>%
                        group_by(input$xcol, sukupuoli) %>%
                        summarise(
                          lento_km = mean(lento_km)
                        )
                      
                      #1. taulukon versio
                      two.way.p2 <- ggplot(monarkki_7var_f, aes(x = input$xcol, y = lento_km, group=sukupuoli)) +
                        geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0),
                                   colour="grey30")
                      
                      #2. taulukon versio
                      two.way.p3 <- two.way.p2 +
                        #errorbar
                        stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.25) +
                        #jokin point range
                        stat_summary(fun.data = 'mean_se', geom = 'pointrange')  +
                        #pisteet
                        geom_point(data=mean.lentomatka, aes(x=input$xcol, y=lento_km, 
                                                             colour=sukupuoli))
                      
                      #3. taulukon versio
                      two.way.p4 <- two.way.p3 +
                        facet_wrap(~ sukupuoli)
                      two.way.p4
                      
                      
                      #teeman vaihto
                      two.way.p5 <- two.way.p4 +
                        theme_bw()
                      two.way.p5
                      
                      })                                   
              
                   
                                     
                      output$tunnusluvut <- renderPrint({
        
                        #tämä oli vaikeahko. Ratkaisu: https://stackoverflow.com/questions/54058769/how-to-fix-error-variable-lengths-differ-found-for-inputs-in-r-shiny   
                        text <- paste0("lento_km ~", input$sarakeA)
                        # korjaan seuraavat
                        # text2 <- paste0("text +", input$sarakeB)
                        # text3 <- paste0("text2 +", input$sarakeC)
                        # text4 <- paste0("text3 +", input$sarakeD)
                        
                        multi.way <- aov(as.formula(text), # paste(input$sarakeB, collapse = "+")input$sarakeB))
                                     data = monarkki_7var_f)
                      
                        summary(multi.way)
                        })
                      
                      # lisään vielä kuvan
                      # output$perhonen <- renderImage({
                      # 
                      # })
                                  

}

shinyApp(ui = ui, server = server)