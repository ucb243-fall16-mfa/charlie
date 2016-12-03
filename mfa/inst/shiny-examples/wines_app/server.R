#Shinny app(MFA)
#
library(shiny)
if(!require(mfa)){
  devtools::install_github("cmcneil/project-243",subdir = "mfa",force = TRUE)
}
library(mfa)

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
    data(wines)
    rownames(wines) = wines[,1]
    wines = wines[,2:54]
    sets = list(1:6,7:12,13:18,19:23,24:29,30:34,35:38,39:44,45:49,50:53)
    obj = mfa(data = wines, sets, center = TRUE, scale = TRUE)
#Eigenvalues bar-chart
    if (input$var == "Eigenvalues"){
      barplot(obj@eigenvalues, main ="Eigenvalues (largest to smallest)", space = 0)
      axis(1,at = 0.5:11.5,labels=1:12)
    }

#a scatterplot of the common factor scores
    if (input$var == "Common factor scores")
    {compromise_plot(obj)}

#a scatterplot of the partial factors scores
    if (input$var == "Partial factors scores")
    {pfs_plot(obj,as.numeric(input$guy))}

#a scatterplot of theloadings
    if (input$var == "Loadings")
      {loadings_plot(obj,as.numeric(input$guy))}
  })

})
