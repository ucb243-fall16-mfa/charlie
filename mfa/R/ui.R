#Shinny app(MFA)
#

library(shiny)

shinyUI(fluidPage(

  titlePanel("MFA"),
  sidebarLayout(
    sidebarPanel(

      #a widget to select what to plot
      selectInput("var",
                  label = "Choose a scatterplot display",
                  choices = list("Eigenvalues", "Common factor scores", "Partial factors scores",
                                 "Loadings"),
                  selected = "Eigenvalues"),

      #Choose an expert assessor
      selectInput("guy",
                  label = "Choose an expert",
                  choices = 1:10,
                  selected = 1)
    ),


    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
