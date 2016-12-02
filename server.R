#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, clientData, session) {

  glob_crt1<-isolate(input$cri1)
  glob_crt2<-isolate(input$cri2)
  glob_crt3<-isolate(input$cri3)
  glob_crt4<-isolate(input$cri4)
  glob_crt5<-isolate(input$cri5)
  glob_crt6<-isolate(input$cri6)
  glob_crt7<-isolate(input$cri7)
  
  initial_weights<-list(crt1=glob_crt1,crt2=glob_crt2,crt3=glob_crt3,crt4=glob_crt4,crt5=glob_crt5,crt6=glob_crt6,crt7=glob_crt7)
  values <- reactiveValues(weights=list(initial_weights))
  
  observeEvent(eventExpr = input$reset,
               {
                 updateSliderInput(session, "cri1", value = glob_crt1)
                 updateSliderInput(session, "cri2", value = glob_crt2)
                 updateSliderInput(session, "cri3", value = glob_crt3)
                 updateSliderInput(session, "cri4", value = glob_crt4)
                 updateSliderInput(session, "cri5", value = glob_crt5)
                 updateSliderInput(session, "cri6", value = glob_crt6)
                 updateSliderInput(session, "cri7", value = glob_crt7)
                 values$weights<-list(initial_weights,initial_weights, initial_weights)
               })
  observeEvent(eventExpr = c(input$cri1, input$cri2, input$cri3, input$cri4, input$cri5,
                             input$cri6, input$cri7) , 
               handlerExpr = {
                 values$weights[[length(values$weights)+1 ]] <- list(crt1=input$cri1,crt2=input$cri2,
                                                        crt3=input$cri3,crt4=input$cri4,
                                                        crt5=input$cri5,crt6=input$cri6,
                                                        crt7=input$cri7)
                 })
  observe({
                 ll<-length(values$weights)
                 #ll<-ll-(ll+1)%%2
                 cat(ll)
                 cat("\n")
                 if(ll%%2==1){
                   if(values$weights[[ll]]$crt1!=values$weights[[ll-1]]$crt1){
                     #cat("cri 1 changes\n")
                     current <- input$cri1
                     others_sum <- 100-glob_crt1
                     updateSliderInput(session, "cri2", value = round((1-current*0.01)*(glob_crt2/others_sum)*100))
                     updateSliderInput(session, "cri3", value = round((1-current*0.01)*(glob_crt3/others_sum)*100))
                     updateSliderInput(session, "cri4", value = round((1-current*0.01)*(glob_crt4/others_sum)*100))
                     updateSliderInput(session, "cri5", value = round((1-current*0.01)*(glob_crt5/others_sum)*100))
                     updateSliderInput(session, "cri6", value = round((1-current*0.01)*(glob_crt6/others_sum)*100))
                     updateSliderInput(session, "cri7", value = round((1-current*0.01)*(glob_crt7/others_sum)*100))
                   }
                   if(values$weights[[ll]]$crt2!=values$weights[[ll-1]]$crt2){
                     #cat("cri 2 changes\n") 
                     current <- input$cri2
                     others_sum <- 100-glob_crt2
                     updateSliderInput(session, "cri1", value = round((1-current*0.01)*(glob_crt1/others_sum)*100))
                     updateSliderInput(session, "cri3", value = round((1-current*0.01)*(glob_crt3/others_sum)*100))
                     updateSliderInput(session, "cri4", value = round((1-current*0.01)*(glob_crt4/others_sum)*100))
                     updateSliderInput(session, "cri5", value = round((1-current*0.01)*(glob_crt5/others_sum)*100))
                     updateSliderInput(session, "cri6", value = round((1-current*0.01)*(glob_crt6/others_sum)*100))
                     updateSliderInput(session, "cri7", value = round((1-current*0.01)*(glob_crt7/others_sum)*100))
                   }
                   if(values$weights[[ll]]$crt3!=values$weights[[ll-1]]$crt3){
                     #cat("cri 3 changes\n") 
                     current <- input$cri3
                     others_sum <- 100-glob_crt3
                     updateSliderInput(session, "cri1", value = round((1-current*0.01)*(glob_crt1/others_sum)*100))
                     updateSliderInput(session, "cri2", value = round((1-current*0.01)*(glob_crt2/others_sum)*100))
                     updateSliderInput(session, "cri4", value = round((1-current*0.01)*(glob_crt4/others_sum)*100))
                     updateSliderInput(session, "cri5", value = round((1-current*0.01)*(glob_crt5/others_sum)*100))
                     updateSliderInput(session, "cri6", value = round((1-current*0.01)*(glob_crt6/others_sum)*100))
                     updateSliderInput(session, "cri7", value = round((1-current*0.01)*(glob_crt7/others_sum)*100))
                   }
                   if(values$weights[[ll]]$crt4!=values$weights[[ll-1]]$crt4){
                     #cat("cri 4 changes\n") 
                     current <- input$cri4
                     others_sum <- 100-glob_crt4
                     updateSliderInput(session, "cri1", value = round((1-current*0.01)*(glob_crt1/others_sum)*100))
                     updateSliderInput(session, "cri2", value = round((1-current*0.01)*(glob_crt2/others_sum)*100))
                     updateSliderInput(session, "cri3", value = round((1-current*0.01)*(glob_crt3/others_sum)*100))
                     updateSliderInput(session, "cri5", value = round((1-current*0.01)*(glob_crt5/others_sum)*100))
                     updateSliderInput(session, "cri6", value = round((1-current*0.01)*(glob_crt6/others_sum)*100))
                     updateSliderInput(session, "cri7", value = round((1-current*0.01)*(glob_crt7/others_sum)*100))
                   }
                   if(values$weights[[ll]]$crt5!=values$weights[[ll-1]]$crt5){
                     #cat("cri 5 changes\n") 
                     current <- input$cri5
                     others_sum <- 100-glob_crt5
                     updateSliderInput(session, "cri1", value = round((1-current*0.01)*(glob_crt1/others_sum)*100))
                     updateSliderInput(session, "cri2", value = round((1-current*0.01)*(glob_crt2/others_sum)*100))
                     updateSliderInput(session, "cri3", value = round((1-current*0.01)*(glob_crt3/others_sum)*100))
                     updateSliderInput(session, "cri4", value = round((1-current*0.01)*(glob_crt4/others_sum)*100))
                     updateSliderInput(session, "cri6", value = round((1-current*0.01)*(glob_crt6/others_sum)*100))
                     updateSliderInput(session, "cri7", value = round((1-current*0.01)*(glob_crt7/others_sum)*100))
                   }
                   if(values$weights[[ll]]$crt6!=values$weights[[ll-1]]$crt6){
                     #cat("cri 6 changes\n") 
                     current <- input$cri6
                     others_sum <- 100-glob_crt6
                     updateSliderInput(session, "cri1", value = round((1-current*0.01)*(glob_crt1/others_sum)*100))
                     updateSliderInput(session, "cri2", value = round((1-current*0.01)*(glob_crt2/others_sum)*100))
                     updateSliderInput(session, "cri3", value = round((1-current*0.01)*(glob_crt3/others_sum)*100))
                     updateSliderInput(session, "cri4", value = round((1-current*0.01)*(glob_crt4/others_sum)*100))
                     updateSliderInput(session, "cri5", value = round((1-current*0.01)*(glob_crt5/others_sum)*100))
                     updateSliderInput(session, "cri7", value = round((1-current*0.01)*(glob_crt7/others_sum)*100))
                   }
                   if(values$weights[[ll]]$crt7!=values$weights[[ll-1]]$crt7){
                     #cat("cri 6 changes\n") 
                     current <- input$cri7
                     others_sum <- 100-glob_crt7
                     updateSliderInput(session, "cri1", value = round((1-current*0.01)*(glob_crt1/others_sum)*100))
                     updateSliderInput(session, "cri2", value = round((1-current*0.01)*(glob_crt2/others_sum)*100))
                     updateSliderInput(session, "cri3", value = round((1-current*0.01)*(glob_crt3/others_sum)*100))
                     updateSliderInput(session, "cri4", value = round((1-current*0.01)*(glob_crt4/others_sum)*100))
                     updateSliderInput(session, "cri5", value = round((1-current*0.01)*(glob_crt5/others_sum)*100))
                     updateSliderInput(session, "cri6", value = round((1-current*0.01)*(glob_crt6/others_sum)*100))
                   }
                 }
                 cat(sum(input$cri1,input$cri2,input$cri3,input$cri4,input$cri5,input$cri6,input$cri7))
                 cat("\n")
                 })

  
})
