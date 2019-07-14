

library(shiny)

# read just a few lines (no need to read entire file)
dat <- read.csv('~/Desktop/bomberas/wfbc_pfas_complete.csv', 
                nrows = 3)

# selected columns
pfs <- c("PFOA", "PFNA", "PFDA", "PFUnDA", "PFDoA", 
  "PFHpA", "PFOSA", "PFOS", "PFBA", "PFBuS", "PFHxA", "PFHxS")
dat <- dat[ ,pfs]

# Define the overall UI
shinyUI(
  
  # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Chemicals"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("var", "Variable", 
                    choices = colnames(dat), selected = 'PFOA'),
        hr(),
        helpText('Mean values:'),
        verbatimTextOutput("means")
      ),
      # Create a spot for the barplot
      mainPanel(
        plotOutput("datPlot")  
      )
      
    )
  )
)