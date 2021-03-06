# Git demo for Munib, Devika and Jenis afternoon session
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(BBRecapture)
library(Hmisc)
library(rdrop2)
#drop_auth(rdstoken = "droptoken.rds")
#drop_auth()  ## login to dropbox
#########
# Listing custom functions
## Double observer survey analysis
dos.bay<-function(dos, no.boot, max.pop){
  model1 <- BBRecap(dos[which(dos[,1] != 0), 2:3], 
                    mod=c("Mt"), prior.N = c("Uniform"), 
                    output = c("complete.ML"), neval = max.pop)
  N.estimate<-model1$N.hat.mean * mean(dos[which(dos[,1] != 0),1]) 
  est.arr<-rep(NA, no.boot)
  det1 <- model1$pH.post.mean[1]
  det2 <- model1$pH.post.mean[2]
  
  for(i in 1:no.boot){
    est.arr[i] <- mean(sample(dos[which(dos[,1] != 0),1], length(dos[which(dos[,1] != 0),1]), replace = T)) *
      sample(model1$N.range, size = 1, prob = model1$posterior.N)
  }
  LCI<- quantile(est.arr, 0.025)
  UCI<- quantile(est.arr, 0.975)
  #hist.plot<-hist(est.arr, xlab = "Population esimate", main = "Posterior distribution of estimated population")
  out.put<-c(N.estimate, model1$N.hat.mean, mean(dos[which(dos[,1] != 0),1]), LCI, UCI, det1, det2, est.arr)
  return(out.put)
}

#dat1<-dat[1:30,]
#x<-dos.bay(cbind(dat1$Group.size, dat1$Obs1, dat1$Obs2), 100, 2000)
#hist(x[6:1000])
#setwd("/Users/Kullu/Dropbox (Snow Leopard Trust)/Kullu_desktop/Git/HAP_ungulate_population_ecology")
#dat<-read.csv("Master DOS Data.csv")
dat<-drop_read_csv('/kullu_desktop/git/HAP_ungulate_population_ecology/Master DOS Data Updated.csv')
#head(dat)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Double observer data analysis"),
  
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Create a subset by species
      selectInput("country", label = h3("Select country program"), 
                  choices = unique(dat$Country)),
      uiOutput("species.select"),   
      uiOutput("location.select"),
      uiOutput("year.select")
      
      
    ),
    
    # Show a plot of the generated distribution
    fluidRow(
      column(4, align="right",
             # Making a histogram of the group size
             plotOutput(outputId = "hist.groupsize", width  = "300px",height = "200px"),  
             #making a histogram of the posterior distribution of the estimated population
             plotOutput(outputId = "hist.posterior", width  = "300px",height = "200px"),
             plotOutput(outputId = "plot.trend", width  = "300px",height = "200px"),
             #Writing the results of the model
             verbatimTextOutput(outputId = "result.text", placeholder = F)
      ))   
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #Subsetting the data using the species, location and year   
  dat.country <- reactive({
    subset(dat, dat$Country == input$country)
  })
  dat.country.species <- reactive({
    subset(dat.country(), dat.country()$Species == input$species)
  })
  dat.country.species.location <- reactive({
    subset(dat.country.species(), dat.country.species()$Block == input$location)
  })
  dat.country.species.location.year <- reactive({
    subset(dat.country.species.location(), dat.country.species.location()$Year == input$year)
  })
  
  #Dynamic subsetting requires creating new UI
  output$species.select <- renderUI({
    selectInput("species", label = h3("Select species"), 
                choices = unique(dat.country()$Species), selected = "Urial") 
  })
  
  output$location.select <- renderUI({
    selectInput("location", label = h3("Select location"), 
                choices = unique(dat.country.species()$Block), selected = "Kargil") 
  })
  output$year.select <- renderUI({
    selectInput("year", label = h3("Select year"), 
                choices = unique(dat.country.species.location()$Year)[order(unique(dat.country.species.location()$Year))]) 
  })
  
  #running the double observer survey function and keeping the output ready
  result.dos<- reactive({
    dos.bay(cbind(dat.country.species.location.year()$Group.size, 
                  dat.country.species.location.year()$Obs1, 
                  dat.country.species.location.year()$Obs2), 1000, sum(dat.country.species.location.year()$Group.size)*6)
  })
  
  # plotting the histogram of the group size data
  output$hist.groupsize <- renderPlot({
    x    <- dat.country.species.location.year()$Group.size 
    hist(x, breaks = 10, col = 'darkgray', border = 'white', 
         main = "Histogram of Group sizes", xlab = "Group size")
  })
  
  #Plotting the histogram of the posterior distribution
  output$hist.posterior <- renderPlot({
    hist(result.dos()[6:1000], col = 'darkgray', border = 'white', 
         main = "Posterior distribution of \n estimated population", xlab = "Estimated population")
  })
  
  # plotting a graph of population change at a site over the years
  output$plot.trend <- renderPlot({
    mat.year<-rep(NA, length(unique(dat.country.species.location()$year)))
    mat.estimate<-rep(NA, length(unique(dat.country.species.location()$year)))
    mat.CI.l <-rep(NA, length(unique(dat.country.species.location()$year)))
    mat.CI.u <-rep(NA, length(unique(dat.country.species.location()$year)))
    count <- 1
    X1 <- NA
    
    for(i in unique(dat.country.species.location()$Year)){
      X1 <- dos.bay(cbind(dat.country.species.location()$Group.size[dat.country.species.location()$Year == i], 
                          dat.country.species.location()$Obs1[dat.country.species.location()$Year == i], 
                          dat.country.species.location()$Obs2[dat.country.species.location()$Year == i]),
                    1000, sum(dat.country.species.location()$Group.size[dat.country.species.location()$Year == i])*6)[1:5]
      mat.estimate[count]<-X1[1]
      mat.CI.l[count]<-as.numeric(X1[4])
      mat.CI.u[count]<-as.numeric(X1[5])
      mat.year[count]<-i
      count<-count+1
    }
    mat.result <- cbind(mat.year, mat.estimate, mat.CI.l, mat.CI.u)
    print(mat.result[,3])
    plot(unique(dat$Year)[order(unique(dat$Year))], 
         rep(NA, length(unique(dat$Year))), ylim = c(0, max(mat.result[,4])+100),
         xlab = "Years", ylab = "Estimated population")
    points(mat.result[,1], mat.result[,2])
    errbar(mat.result[,1], mat.result[,2], 
           mat.result[,3], mat.result[,4], add = T)
  })
  #Writing out the results of the double observer analysis    
  output$result.text <- renderText({
    paste("Estimated population of the ungulate species was", 
          paste(round(result.dos()[1]),
                paste("and CI were \b", 
                      paste(round(result.dos()[4]),
                            paste("and",
                                  paste(round(result.dos()[5]),
                                        paste("\n \n Estimated number of groups were",
                                              paste(result.dos()[2],
                                                    paste("\n Detection probabilities were:",
                                                          paste(round(result.dos()[6],2),
                                                                paste(round(result.dos()[7],2))))))))))))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

#Change_JP

