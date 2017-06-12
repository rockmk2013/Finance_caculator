library(shiny)
library(dplyr) #for data manipulation
library(tidyr) #for reshaping data
library(ggplot2)
library(scales) #for labels as percentage
library(RCurl)#for getting url content
library(jpeg) #for reading JPEG file
library(grid) #for rendering a raster grob



shinyServer(function(input, output) {
  final <- reactive({
    
    if(input$goButton){
      if(input$radio =="1"){
        having = as.integer(input$target)-as.integer(input$savings)*(1+(as.integer(input$savingratio)*0.01))^(as.integer(input$period))
        invest_ratio = c()
        if(having>0){
          for( i in 1:(as.integer(input$period)-1)){
            invest_ratio[i] = (as.integer(input$tool1)*0.01)*(1+(as.integer(input$ratio1)*0.01))^i+(as.integer(input$tool2)*0.01)*(1+(as.integer(input$ratio2)*0.01))^i+(as.integer(input$tool3)*0.01)*(1+(as.integer(input$ratio3)*0.01))^i
          }
          final = having/sum(invest_ratio)
          final 
        }else{
          final = -1
          final 
        }
      }else{
        having = as.integer(input$target)-as.integer(input$savings)*(1+(as.integer(input$savingratio)*0.01))^(as.integer(input$period))
        invest_ratio = c()
        if(having>0){
          for( i in 1:(as.integer(input$period)*12-1)){
            invest_ratio[i] = (as.integer(input$tool1)*0.01)*(1+(as.integer(input$ratio1)*0.01/12))^i+(as.integer(input$tool2)*0.01)*(1+(as.integer(input$ratio2)*0.01/12))^i+(as.integer(input$tool3)*0.01)*(1+(as.integer(input$ratio3)*0.01/12))^i
          }
          final = having/sum(invest_ratio)
          final 
        }else{
          final = -1
          final 
        }
        
      }
    }
  })
  output$text1 <- renderText({ 
    
    
    if(is.null(final())){
      paste0("Welcome to the Saving & Investment Helper!") 
    }else if (final()==-1){
      paste0("Saving is enough!") 
    } else if(input$radio =="1"){
      # having = as.integer(input$target)-as.integer(input$savings)*(1+(as.integer(input$savingratio)*0.01))^(as.integer(input$period))
      # invest_ratio = c()
      # for( i in 1:(as.integer(input$period)-1)){
      #   invest_ratio[i] = (as.integer(input$tool1)*0.01)*(1+(as.integer(input$ratio1)*0.01))^i+(as.integer(input$tool2)*0.01)*(1+(as.integer(input$ratio2)*0.01))^i+(as.integer(input$tool3)*0.01)*(1+(as.integer(input$ratio3)*0.01))^i
      # }
      # final = having/sum(invest_ratio)
      paste0("By year, you need to invest : ",as.integer(final()),"  NTD/year") 
    }else{
      # having = as.integer(input$target)-as.integer(input$savings)*(1+(as.integer(input$savingratio)*0.01))^(as.integer(input$period))
      # invest_ratio = c()
      # for( i in 1:(as.integer(input$period)*12-1)){
      #   invest_ratio[i] = (as.integer(input$tool1)*0.01)*(1+(as.integer(input$ratio1)*0.01/12))^i+(as.integer(input$tool2)*0.01)*(1+(as.integer(input$ratio2)*0.01/12))^i+(as.integer(input$tool3)*0.01)*(1+(as.integer(input$ratio3)*0.01/12))^i
      # }
      # final = having/sum(invest_ratio)
      paste0("By month, you need to invest : ",as.integer(final()),"  NTD/month") 
    }
    
    
  })
  peryear_new <- reactive({
    
    if(input$goButton){
      if(input$radio =="1"){
        having = as.integer(input$target)-as.integer(input$savings)*(1+(as.integer(input$savingratio)*0.01))^(as.integer(input$period))
        invest_ratio = c()
        if(having >0){
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
          peryear_new
        }else{
          peryear_new = -1
          peryear_new
        }
        
      }else{
        having = as.integer(input$target)-as.integer(input$savings)*(1+(as.integer(input$savingratio)*0.01))^(as.integer(input$period))
        invest_ratio = c()
        if(having >0){
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
          peryear_new
        }else{
          peryear_new=-1
          peryear_new
        }
        
      }
    }
  })
  output$plot1 <- renderPlot({
    if(is.null(peryear_new())){
      
    }else if (peryear_new()==-1){
      paste0("Saving is enough!") 
    }else if(input$radio =="1"){
      # having = as.integer(input$target)-as.integer(input$savings)*(1+(as.integer(input$savingratio)*0.01))^(as.integer(input$period))
      # invest_ratio = c()
      # for( i in 1:(as.integer(input$period)-1)){
      #   invest_ratio[i] = (as.integer(input$tool1)*0.01)*(1+(as.integer(input$ratio1)*0.01))^i+(as.integer(input$tool2)*0.01)*(1+(as.integer(input$ratio2)*0.01))^i+(as.integer(input$tool3)*0.01)*(1+(as.integer(input$ratio3)*0.01))^i
      # }
      # final = having/sum(invest_ratio)
      # peryear = final*invest_ratio
      # plot(peryear)
      # peryear_new = c()
      # for(i in 1:(as.integer(input$period)-1)){
      #   if(i==1){
      #     peryear_new[1] = peryear[1]
      #   }else{
      #     peryear_new[i] = sum(peryear[1:i])
      #   }
      # }
      #draw
      plot(peryear_new(),pch=20,lwd=5,las=1,ann = FALSE)
      lines(peryear_new(),col="red",lwd=2)
    }else{
      # having = as.integer(input$target)-as.integer(input$savings)*(1+(as.integer(input$savingratio)*0.01))^(as.integer(input$period))
      # invest_ratio = c()
      # for( i in 1:(as.integer(input$period)*12-1)){
      #   invest_ratio[i] = (as.integer(input$tool1)*0.01)*(1+(as.integer(input$ratio1)*0.01/12))^i+(as.integer(input$tool2)*0.01)*(1+(as.integer(input$ratio2)*0.01/12))^i+(as.integer(input$tool3)*0.01)*(1+(as.integer(input$ratio3)*0.01/12))^i
      # }
      # final = having/sum(invest_ratio)
      # peryear = final*invest_ratio
      # #
      # peryear_new = c()
      # for(i in 1:(as.integer(input$period)*12-1)){
      #   if(i==1){
      #     peryear_new[1] = peryear[1]
      #   }else{
      #     peryear_new[i] = sum(peryear[1:i])
      #   }
      # }
      #draw
      plot(peryear_new(),pch=20,lwd=5,las=1,ann = FALSE)
      lines(peryear_new(),col="red",lwd=2)
      #print(final)
      #print(peryear_new)
    }
  })
  output$plot2 <- renderPlot({
    
  })
  
})