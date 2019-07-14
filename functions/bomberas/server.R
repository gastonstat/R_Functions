library(shiny)

# import dataset
dat <- read.csv('~/Desktop/bomberas/wfbc_pfas_complete.csv',
                stringsAsFactors = FALSE)

# firefighter status
status <- dat$FF.Status
status[status == 0] <- 'Non-FF'
status[status == 1] <- 'FF'

# selected columns
pfs <- c("PFOA", "PFNA", "PFDA", "PFUnDA", "PFDoA", "PFHpA", 
         "PFOSA", "PFOS", "PFBA", "PFBuS", "PFHxA", "PFHxS")
dat <- dat[ ,pfs]


# Define a server for the Shiny app
shinyServer(function(input, output) {

  # Mean values
  
  output$means <- renderPrint({ 
    tapply(dat[,input$var], status, mean)
  })
  
  # Fill in the spot we created for a plot
  output$datPlot <- renderPlot({
    
    # Render a barplot
    means <- tapply(dat[,input$var], status, mean)
    barplot(means, las = 1, col = c('#547c65', '#dc702a'),
            main = input$var)
  })
})