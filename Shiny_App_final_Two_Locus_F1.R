library(shiny)
library(Rdice)


PhenoGenodata <- read.csv("")#insert your excel file here
Phenovector<- Vectorize(PhenoGenodata$Trait)#create phenotype vector
Genovector<- Vectorize(PhenoGenodata$MOI)#create Genotype vector

selectorUImale <- function(id) {#create a reactive selection button with Dropdown menu for males
  #ns <- NS(id)
  selectizeInput(inputId = 'select1',
                 label = 'Select male phenotype:',
                 choices = Phenovector)#Using the Phenogenovector we created above
}
selectorUIfemale <- function(id) {#create a reactive selection button with dropdown menu for females
  #ns <- NS(id) #for female input
  selectizeInput(inputId = 'select2',
                 label = 'Select female phenotype:',
                 choices = Phenovector)#using the phenogenovector we created above
}
ui <- shinyUI(
  fluidPage(
    #for title
    titlePanel("ShinyFly"),
    #UI selection section
    selectorUImale('id1'),
    
    
    selectorUIfemale('id1'),
    #close off fluid page
    #how to make the chart display after we select the male and female phenotypes
    #how to get the choices to be a table with undiscoverable modes of inheritance
    #action button
    actionButton("do", "Mate"),
    
    #to create a table
    fluidRow(
      column(12,
             tableOutput('table')))
    
  ))



server <- shinyServer(function(input, output, session) {
  
  output$value <- renderPrint({ input$select })
  
  observeEvent(input$do, {        #argument for shiny app
    
    offspring<-sample(990:1010,1) #simulate 1000 offspring  
    
    #here is where you connect the phenotypes selected above to the concealed genotypes
    #Recessive and Recessive1
    if(Genovector[which(Phenovector==input$select1)]=="Recessive" & Genovector[which(Phenovector==input$select2)]=="Recessive" )
    {
      TR<-dice.roll(8, 1, offspring, weights = c(0.5, 0.5,0,0,0,0,0,0))
      tableR <- data.frame(Gender = c("Female", "Male"),
                          WTWT = c(TR$frequencies$N[1], TR$frequencies$N[2]),
                          WTDisease = c( 0, 0),
                          DiseaseWT = c( 0, 0),
                          DiseaseDisease = c( 0, 0))
      output$table <- renderTable(tableR) #Display table
    }
    
    #male X-linked and recessive 2
    
    else if(Genovector[which(Phenovector==input$select1)]=="Xlinked" & Genovector[which(Phenovector==input$select2)]=="Recessive" )
    {
      TR<-dice.roll(8, 1, offspring, weights = c(0.5, 0.5,0,0,0,0,0,0))
      tableR <- data.frame(Gender = c("Female", "Male"),
                           WTWT = c(TR$frequencies$N[1], TR$frequencies$N[2]),
                           WTDisease = c( 0, 0),
                           DiseaseWT = c( 0, 0),
                           DiseaseDisease = c( 0, 0))
      output$table <- renderTable(tableR) #Display table
    }
    
    #Male recessive and Female x-linked3
    
    else if(Genovector[which(Phenovector==input$select1)]=="Recessive" & Genovector[which(Phenovector==input$select2)]=="Xlinked")
    {
      TX<-dice.roll(4, 1, offspring, weights = c(0.5,0,0.5,0))
      tableX <- data.frame(Gender = c("Female", "Male"), #table set to display male and female offspring for x-linked recessive
                           WT = c(TX$frequencies$N[1], 0),
                           DiseaseWT = c(0, TX$frequencies$N[2]),
                           WTDisease = c( 0, 0),
                           DiseaseDisease = c( 0, 0))
      
      output$table <- renderTable(tableX) #display table
    }
    
    #recessive and homozygous lethal4
    
    else if(Genovector[which(Phenovector==input$select1)]=="Recessive" & Genovector[which(Phenovector==input$select2)]=="Homozygous dominant lethal")
      #dominant
    {
      TD<-dice.roll(4, 1, offspring, weights = c(0.25,0.25,0.25,0.25))#probabilities by quarters
      tableRD <- data.frame(Gender = c("Female", "Male"),
                           WTWT = c(TD$frequencies$N[3], TD$frequencies$N[4]),
                           DiseaseWT = c( 0, 0),
                           WTDisease = c(TD$frequencies$N[1], TD$frequencies$N[2]),
                           DiseaseDisease = c( 0, 0)
      )
      output$table <- renderTable(tableRD)
    }
    
    #homozyous lethal and homozygous lethal5
    
    else if(Genovector[which(Phenovector==input$select1)]=="Homozygous dominant lethal" & Genovector[which(Phenovector==input$select2)]=="Homozygous dominant lethal")
      #dominant
    {
      TD<-dice.roll(8, 1, offspring, weights = c(0.125,0.125,0.125,0.125,.125,.125,.125,.125))#probabilities by eighths
      tableDD <- data.frame(Gender = c("Female", "Male"),
                           WTWT = c(TD$frequencies$N[1], TD$frequencies$N[2]),
                           WTDisease = c(TD$frequencies$N[3], TD$frequencies$N[4]),
                           DiseaseWT = c( TD$frequencies$N[5], TD$frequencies$N[6]),
                           DiseaseDisease = c( TD$frequencies$N[7], TD$frequencies$N[8])
      )
      output$table <- renderTable(tableDD) #display table
    }
    #homozyous lethal and Recessive6
    
    else if(Genovector[which(Phenovector==input$select1)]=="Homozygous dominant lethal" & Genovector[which(Phenovector==input$select2)]== "Recessive")
      #dominant
    {
      TD<-dice.roll(8, 1, offspring, weights = c(0.25,0.25,0.25,.25,0,0,0,0))#probabilities by eighths
      tableDR <- data.frame(Gender = c("Female", "Male"),
                            WTWT = c(TD$frequencies$N[1], TD$frequencies$N[2]),
                            DiseaseWT = c(TD$frequencies$N[3], TD$frequencies$N[4]),
                            WTDisease = c( 0, 0),
                            DiseaseDisease = c( 0, 0)
      )
      output$table <- renderTable(tableDR) #display table
    }
    #homozyous lethal and Xlinked 7
    
    else if(Genovector[which(Phenovector==input$select1)]=="Homozygous dominant lethal" & Genovector[which(Phenovector==input$select2)]== "Xlinked")
      #dominant
    {
      TD<-dice.roll(8, 1, offspring, weights = c(0.25,0.25,0.25,.25,0,0,0,0))
      tableDX <- data.frame(Gender = c("Female", "Male"),
                            WTWT = c(TD$frequencies$N[1], TD$frequencies$N[2]),
                            DiseaseWT = c(TD$frequencies$N[3], TD$frequencies$N[4]),
                            WTDisease = c( 0, 0),
                            DiseaseDisease = c( 0, 0)
      )
      output$table <- renderTable(tableDX) #display table
    }
    #xlinked and Homozygous lethal 8
    else if(Genovector[which(Phenovector==input$select1)]=="Xlinked" & Genovector[which(Phenovector==input$select2)]=="Homozygous dominant lethal" )
    {
      TR<-dice.roll(8, 1, offspring, weights = c(0.5, 0.5,0,0,0,0,0,0))
      tableXD <- data.frame(Gender = c("Female", "Male"),
                           WTWT = c(TD$frequencies$N[1], TD$frequencies$N[2]),
                           DiseaseWT = c( 0, 0),
                           WTDisease = c(TD$frequencies$N[3], TD$frequencies$N[4]),
                           DiseaseDisease = c( 0, 0))
      output$table <- renderTable(tableXD) #Display table
    }

    #xlinked and x-linked 9
    else if(Genovector[which(Phenovector==input$select1)]=="Xlinked" & Genovector[which(Phenovector==input$select2)]=="Xlinked" )
    {
      TR<-dice.roll(8, 1, offspring, weights = c(0.25, 0,0.25,0,0.25,0,0.25,0))
      tableXX <- data.frame(Gender = c("Female", "Male"),
                           WTWT = c(0, TD$frequencies$N[1]),
                           DiseaseWT = c(TD$frequencies$N[3], 0),
                           WTDisease = c(0, TD$frequencies$N[5]),
                           DiseaseDisease = c(TD$frequencies$N[7], 0))
      output$table <- renderTable(tableXX) #Display table
    }
  })
})

shinyApp(ui = ui, server = server)