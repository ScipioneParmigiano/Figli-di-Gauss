# pcs.parmigiano@gmail.com


# req_lib <- c("shiny", "webr")
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

library(webr)
library(shiny)


##################### ui #######################################################

ui <- fluidPage(
  h1("Test d'ipotesi", style="color:#337ab7;"),
  
  sidebarPanel(
    width = 2,
    
    sliderInput("n",
                label = "Numerosità del campione:",
                min = 3, max = 1000, value = 15),
    
    sliderInput("var", 
                label = "Vera varianza della popolazione (sconosciuta):",
                min = 1, max = 15, value = 10),
    
    sliderInput("mean", 
                label = "Vera media della popolazione (sconosciuta):",
                min = 160, max = 180, value = 176),
    
    sliderInput("p", 
                label = "Vera proporzione (sconosciuta):",
                min = 0, max = 1, value = .28)
  ),
  
  mainPanel(
    htmlOutput("picture1"),
    htmlOutput("picture2"),
    
    width = 8,
    tabsetPanel(
      type = "tabs",
      tabPanel("Shapiro-Wilk", 
               plotOutput("qqplot"),
               verbatimTextOutput("shapiro_will"),
               plotOutput("be_real"),
               verbatimTextOutput("be_real_2")), 
      tabPanel("T-test",
               plotOutput("t_plot"),
               verbatimTextOutput("tt"),
               ),
      tabPanel("Binomiale", 
               plotOutput("hist_b"),
               tableOutput("binom_tab"),
               verbatimTextOutput("binomiale"),
               verbatimTextOutput("binomiale_2"))
      )
  ),     
  
  
  sidebarPanel(
    width = 2,
    h4("Shapiro test"),
    
    radioButtons("unif",
                 label = "paragone qq:",
                 choices = c("F", "T"), selected = "F"),
    
    h4("T-test"),
    
    sliderInput("tt", 
                label = "H_0:",
                min = 160, max = 180, value = 175),
    radioButtons("tipo", 
                 label = "Direzione:",
                 choices = c(">", "<", "diverso"), selected = "diverso"),
    radioButtons("conf", 
                 label = "Livello di confidenza del test:",
                 choices = c("95%", "99%", "80%"), selected = "95%"),
    
    h4("Test binomiale"),
    
    sliderInput("bt", 
                label = "H_0:",
                min = 0, max = 1, value = .5),
    
    radioButtons("tipo_2", 
                 label = "Direzione:",
                 choices = c(">", "<", "diverso"), selected = "diverso"),
    
    radioButtons("conf_2", 
                 label = "Livello di confidenza del test:",
                 choices = c("95%", "99%", "80%"), selected = "95%")
  )
)


##################### server ###################################################

server <- function(input, output) {
  
  
  # ---------------- Intro e immagini ------------------------------------------
  
  src1 = "https://yt3.ggpht.com/J_veYK8e_aR9sNdtK4h6tqMnnMHzfQ6MEjS4a0tzV6pIjmVq19lUy5qX5qot6IWgbpGlobK7Yg=s900-c-k-c0x00ffffff-no-rj"
  output$picture1<-renderText({c('<p><a href="https://www.youtube.com/@liberioltreSTEM/"> <img src=',src1,'width="40" height="40" align="right"/>')})
  
  src2 = "https://www.liberioltreleillusioni.it/fileadmin/Websites/LiberiOltre/Assets/Img/logo-liberi-oltre-red-300.png"
  output$picture2<-renderText({c('<p><a href="https://www.liberioltreleillusioni.it/"> <img src=',src2,'width="40" height="40" align="right"/>')})
  
  
  # ---------------- inizializzazioni ------------------------------------------
  
  var_ <- reactive({as.double(input$var)})
  mean_ <- reactive({as.double(input$mean)})
  n <- reactive({as.double(input$n)})
  h_t <- reactive({as.double(input$tt)})
  type <- reactive({
    if(input$tipo == "diverso") return("two.sided")
    else if(input$tipo == ">") return("greater")
    else if(input$tipo == "<") return("less")
  })
  conf <- reactive({
    if(input$conf == "95%"){
      return(.95)
    } else if(input$conf == "99%"){
      return(.99)
    } else if(input$conf == "80%"){
      return(.8)}
  })
  
  alt <- reactive({
    altezze_ <- rnorm(n(), mean_(), sqrt(var_()))
    # return((altezze_ - mean(altezze_))/sd(altezze_))
    })
  
  norm <- reactive({
    set.seed(1)
    x_ <- rnorm(1000, 100, 10)
    # return((x_ - mean(x_))/sd(x_))
  })

  unif <- reactive({
    set.seed(1)
    x_ <- runif(1000, 0, 1)
    # return((x_ - mean(x_))/sd(x_))
  })
  
  prop <- reactive({as.double(input$p)})
  bt <- reactive({as.double(input$bt)})
  type_2 <- reactive({
    if(input$tipo_2 == "diverso") return("two.sided")
    else if(input$tipo_2 == ">") return("greater")
    else if(input$tipo_2 == "<") return("less")
  }) 
  
  # binom.test non vuole la direzione del test ma la direzione dell'alternativa
  alt_2 <- reactive({
    if(input$tipo_2 == "diverso") return("two.sided")
    else if(input$tipo_2 == ">") return("less")
    else if(input$tipo_2 == "<") return("greater")
  }) 
  
  conf_2 <- reactive({
    if(input$conf_2 == "95%"){
      return(.05)
    } else if(input$conf_2 == "99%"){
      return(.01)
    } else if(input$conf_2 == "80%"){
      return(.2)}
  })
  
  
  # ---------------- tab1 ------------------------------------------------------
  
  # shapiro will ------------------------------------------------------------
  
  
  output$qqplot <- renderPlot({

    if(input$unif == "T"){

      par(mfrow=c(1,2))

      qqnorm(unif(), main = "qq uniforme", frame=F)
      qqline(unif(), col="red", lwd=2)


      qqnorm(norm(), main = "qq normale", frame=F)
      qqline(norm(), col="red", lwd=2)

    } else if(input$unif == "F"){

      par(mfrow=c(1,2))
      
      h <- hist(alt())
      xfit <- seq(min(alt()),max(alt()),length = 40)
      yfit <- dnorm(xfit, mean = mean(alt()), sd = sd(alt())) * diff(h$mids[1:2])*length(alt())
      
      lines(xfit, yfit, col="red", lwd=2)

      qqnorm(alt(), main = "qq altezze", frame=F)
      qqline(alt(), col="red", lwd=2)

    }
  })
  
  output$be_real <- renderPlot({
    
    if(input$unif == "T"){
      
    df <- ToothGrowth
    qqnorm(df$len, pch = 1, frame = F)
    qqline(df$len, col = "red", lwd = 2)
    }
  })
  
  output$be_real_2 <- renderText({
    
    if(input$unif == "T"){
      
      df <- ToothGrowth$len
      paste0("p-value: ", round(shapiro.test(df)$p.value,2))      
    }
  })
  
  output$shapiro_will <- renderText({
    if(input$unif == "F"){

      paste0("h_0: il campione è parte di una popolazione distribuita normalmente. 
             \nh_a: la popolazione da cui discende il campione NON si distibuisce normalmente.
             \np-value: ", round(shapiro.test(alt())$p.value, 3))

    } else if(input$unif == "T"){

      paste0("p-value vettore normale: ", round(shapiro.test(norm())$p.value, 3), 
             "\np-value vettore uniforme: ", signif(shapiro.test(unif())$p.value, 1))

    }
  })
  
  
  # ---------------- tab2 ------------------------------------------------------
  
  # t test ------------------------------------------------------------------

  output$t_plot <- renderPlot({
    
    plot(t.test(alt(), mu = h_t(),
         alternative = type(),
         conf.level = conf()), 
         main = "t-test", sub = "")
      
  })


  # tab3 --------------------------------------------------------------------

  # binomial test ----------------------------------------------------------
  
  output$binomiale <- renderText({
    set.seed(1)
    
    x <- runif(n())
    
    for(i in 1:n()){
      if(x[i] > prop()){
        x[i] <- 0
      } else {
        x[i] <- 1
      }
    }
    
    a <- sum(x)
    
    paste0("h_0: ", bt(),
           "\ndirezione: ", type_2(),
           "\nconf: ", conf_2(),
           "\nnumero successi: ", a,
           "\nnumero insuccessi: ", n()-a)
  })
  
  output$binomiale_2 <- renderText({
    set.seed(1)
    x <- runif(n())
    
    for(i in 1:n()){
      if(x[i] > prop()){
        x[i] <- 0
      } else {
        x[i] <- 1
      }
    }
    
    a <- sum(x)
    p <- round(binom.test(a, n(), bt(), alt_2(), conf_2())$p.value,3)
    
    if(p > conf_2()){res <- "non rifiutiamo h_0"
      } else {res <- "rifiutiamo h_0"}
    
    paste0("p-value: ", p,
           "\nrisultato: ", res)
  })
  
  output$hist_b <- renderPlot({
    set.seed(1)
    x <- runif(n())
    
    for(i in 1:n()){
      if(x[i] > prop()){
        x[i] <- 0
      } else {
        x[i] <- 1
      }
    }
    
    hist(x)
  })
  
  
}  

shinyApp(ui, server)