library(shiny)
library(dplyr) #for data manipulation
library(tidyr) #for reshaping data
library(ggplot2)
library(scales) #for labels as percentage
library(RCurl)#for getting url content
library(jpeg) #for reading JPEG file
library(grid) #for rendering a raster grob



shinyServer(function(input, output) {
  
  output$text1 <- renderText({ 
    input$goButton
    #年計:
    #A-B*(1+r)^n
    #=C*d%*(1+r1)^n+C*e%*(1+r2)^n+C*f%*(1+r3)^n == 投資部分總利合
    if(input$radio =="1"){
      having = as.integer(input$target)-as.integer(input$savings)*(1+(as.integer(input$savingratio)*0.01))^(as.integer(input$period))
      invest_ratio = c()
      for( i in 1:(as.integer(input$period)-1)){
        invest_ratio[i] = (as.integer(input$tool1)*0.01)*(1+(as.integer(input$ratio1)*0.01))^i+(as.integer(input$tool2)*0.01)*(1+(as.integer(input$ratio2)*0.01))^i+(as.integer(input$tool3)*0.01)*(1+(as.integer(input$ratio3)*0.01))^i
      }
      final = having/sum(invest_ratio)
      paste0("以年計息，你平均需要投資",final,"元/年") 
    }else{
      having = as.integer(input$target)-as.integer(input$savings)*(1+(as.integer(input$savingratio)*0.01))^(as.integer(input$period))
      invest_ratio = c()
      for( i in 1:(as.integer(input$period)*12-1)){
        invest_ratio[i] = (as.integer(input$tool1)*0.01)*(1+(as.integer(input$ratio1)*0.01/12))^i+(as.integer(input$tool2)*0.01)*(1+(as.integer(input$ratio2)*0.01/12))^i+(as.integer(input$tool3)*0.01)*(1+(as.integer(input$ratio3)*0.01/12))^i
      }
      final = having/sum(invest_ratio)
      paste0("以月計息，你平均需要投資",final,"元/月") 
    }
    
    
  })
  
  output$plot1 <- renderPlot({
    input$action
    if(input$radio =="1"){
      having = as.integer(input$target)-as.integer(input$savings)*(1+(as.integer(input$savingratio)*0.01))^(as.integer(input$period))
      invest_ratio = c()
      for( i in 1:(as.integer(input$period)-1)){
        invest_ratio[i] = (as.integer(input$tool1)*0.01)*(1+(as.integer(input$ratio1)*0.01))^i+(as.integer(input$tool2)*0.01)*(1+(as.integer(input$ratio2)*0.01))^i+(as.integer(input$tool3)*0.01)*(1+(as.integer(input$ratio3)*0.01))^i
      }
      final = having/sum(invest_ratio)
      peryear = final*invest_ratio
      #plot(peryear)
      peryear_new = c()
      for(i in 1:(as.integer(input$period)-1)){
        if(i==1){
          peryear_new[1] = peryear[1]
        }else{
          peryear_new[i] = sum(peryear[1:i])
        }
      }
      #draw
      plot(peryear_new,pch=20,lwd=5,las=1,ann = FALSE)
      lines(peryear_new,col="red",lwd=2)
    }else{
      having = as.integer(input$target)-as.integer(input$savings)*(1+(as.integer(input$savingratio)*0.01))^(as.integer(input$period))
      invest_ratio = c()
      for( i in 1:(as.integer(input$period)*12-1)){
        invest_ratio[i] = (as.integer(input$tool1)*0.01)*(1+(as.integer(input$ratio1)*0.01/12))^i+(as.integer(input$tool2)*0.01)*(1+(as.integer(input$ratio2)*0.01/12))^i+(as.integer(input$tool3)*0.01)*(1+(as.integer(input$ratio3)*0.01/12))^i
      }
      final = having/sum(invest_ratio)
      peryear = final*invest_ratio
      #
      peryear_new = c()
      for(i in 1:(as.integer(input$period)*12-1)){
        if(i==1){
          peryear_new[1] = peryear[1]
        }else{
          peryear_new[i] = sum(peryear[1:i])
        }
      }
      #draw
      plot(peryear_new,pch=20,lwd=5,las=1,ann = FALSE)
      lines(peryear_new,col="red",lwd=2)
      print(final)
      print(peryear_new)
    }
  })
  output$plot2 <- renderPlot({
    
  })
  
})