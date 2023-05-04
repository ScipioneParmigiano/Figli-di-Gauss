# pcs.parmigiano@gmail.com


# req_lib <- c("shiny", "forecast", "ggplot2", "xts", "tseries")
# 
# libraries <- function(req_lib){
#   for(l in req_lib){
#     if(!require(l, character.only = TRUE)){
#       install.packages(l, dependencies = TRUE)
#       library(l, character.only = TRUE)
#     }
#   }
# }
# libraries(req_lib)

library("shiny")
library("forecast")
library("ggplot2")
library("xts")
library("tseries")



##################### ui #######################################################

ui <- fluidPage(
  h1("Modelli autoregressivi a media mobile", style="color:#337ab7;"),
  
  sidebarPanel(
    width = 2,
    h4("ARIMA"),
    h5("Stazionarietà:"),
    radioButtons("diff",
                label = "differenziazione:",
                choices = c("falso", "vero"), selected = "falso"),
    radioButtons("pred",
                 label = "predizione:",
                 choices = c("falso", "vero"), selected = "falso")
    ),
  
  mainPanel(
    htmlOutput("picture1"),
    htmlOutput("picture2"),
    
    width = 8,
    tabsetPanel(
      type = "tabs",
      tabPanel("ARIMA", plotOutput("plot_arima"),
               textOutput("kpss"),
               plotOutput("pred_arima")), 
      tabPanel("SARIMA",
               plotOutput("plot_sarima"),
               textOutput("test_kpss"),
               plotOutput("pred_sarima")
      )
    )
  ),     
  
  
  sidebarPanel(
    width = 2,
    h4("SARIMA:"),
    h5("Stazionarietà:"),
    radioButtons("diff2",
                 label = "differenziazione:",
                 choices = c("falso", "vero"), selected = "falso"),
    radioButtons("log",
                 label = "trasformazione logaritmica:",
                 choices = c("falso", "vero"), selected = "falso"),
    radioButtons("pred2",
                 label = "predizione:",
                 choices = c("falso", "vero"), selected = "falso")
  )
)


##################### server ###################################################

server <- function(input, output) {
  
  src1 = "https://yt3.ggpht.com/J_veYK8e_aR9sNdtK4h6tqMnnMHzfQ6MEjS4a0tzV6pIjmVq19lUy5qX5qot6IWgbpGlobK7Yg=s900-c-k-c0x00ffffff-no-rj"
  output$picture1<-renderText({c('<p><a href="https://www.youtube.com/@liberioltreSTEM/"> <img src=',src1,'width="40" height="40" align="right"/>')})
  
  src2 = "https://www.liberioltreleillusioni.it/fileadmin/Websites/LiberiOltre/Assets/Img/logo-liberi-oltre-red-300.png"
  output$picture2<-renderText({c('<p><a href="https://www.liberioltreleillusioni.it/"> <img src=',src2,'width="40" height="40" align="right"/>')})
  
  
  load(url("https://userpage.fu-berlin.de/soga/300/30100_data_sets/Earth_Surface_Temperature.RData"))

  t.global  <- apply.yearly(t.global, mean)
  temp.global  <- t.global["1850/2000", 'Monthly.Anomaly.Global']
  temp.global.test  <- t.global["2000/2016", 'Monthly.Anomaly.Global']
  temp.global.diff1 <- diff(temp.global)
  fit <- Arima(temp.global, order = c(3,1,0), include.drift = T)
  

  # ARIMA -------------------------------------------------------------------
  
  
  output$plot_arima <- renderPlot({
    
    if(input$diff == "falso"){
    plot.zoo(cbind(temp.global,
                   temp.global.test),
             plot.type = "single", 
             col = c("black", "red"), 
             main = 'Anomalie della temperatura superficiale della Terra per anno', 
             ylab = '', xlab = '')
    legend('topleft', 
           legend = c('training set 1850-2000',
                      'test set 2001-2016'), 
           col = c("black", "red"),
           lty = 1, cex = 0.65)
    } else {
      plot(temp.global.diff1)
    }
  })
  
  output$kpss <- renderText({
    if(input$diff == "falso") paste0("Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test: non stazionario")
    else paste0("Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test: stazionario")
  })
  
  
  
  output$pred_arima <- renderPlot({
    
    if(input$pred == "falso"){
      
    } else {
      temp.forecast <- forecast(fit, h = 16)
      plot(temp.forecast)    
      lines(ts(coredata(temp.global.test),
               start = start(temp.forecast$mean)[1],
               frequency = 1), col = 'red', lwd=3)
      }
  })
  
  output$kpss <- renderText({
    if(input$diff == "falso") paste0("Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test: non stazionario")
    else paste0("Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test: stazionario")
  })
  
  
  
  # SARIMA ------------------------------------------------------------------
  
  
  dates <- seq(as.Date("1949-01-01"), length = 12*12, by = "months")
  airPass_ts <- xts(AirPassengers, order.by=dates)
  airP_train  <- airPass_ts["1949/1957", 1]
  airP_test  <- airPass_ts["1957/1960", 1]
  AirPassengers_diff1 <- diff(AirPassengers)
  pdqParam = c(0, 1, 1)
  fit2 <- Arima(airP_train, c(0,1,1), seasonal=list(order = pdqParam, period = 12))
  
  
  output$plot_sarima <- renderPlot({
    if(input$diff2 == "falso"){
      plot.zoo(cbind(airP_train,
                     airP_test),
               plot.type = "single", 
               col = c("black", "red"),
               main = 'Numero passeggeri mensili nel tempo', 
               ylab = '', xlab = '')
      legend('topleft', 
             legend = c('training set 1949-1957',
                        'test set 1958-1960'), 
             col = c("black", "red"),
             lty = 1, cex = 0.65)  
    } else if(input$diff2 == "vero" & input$log=="falso"){
      plot(AirPassengers_diff1)
    } else if(input$diff2 == "vero" & input$log=="vero"){
      plot(diff(log(AirPassengers)))
    }
  })
    
    
  output$test_kpss <- renderText({
    if(input$diff2 == "vero" & input$log=="vero") paste0("Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test: stazionario")
    else paste0("Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test: non stazionario")
  })
  
  
  output$pred_sarima <- renderPlot({
    if(input$pred2 == "falso"){
      
    } else {
      air.forecast <- forecast(fit2, h = 36)
      plot(air.forecast)    
      lines(ts(coredata(airP_test),
               start = start(air.forecast$mean)[1],
               frequency = 1), col = 'red', lwd=3)
    }
  })
  
}

shinyApp(ui, server)

