# pcs.parmigiano@gmail.com


req_lib <- c("tidyverse", "simmer", "mathjaxr")

libraries <- function(req_lib){
  for(l in req_lib){
    if(!require(l, character.only = TRUE)){
      install.packages(l, dependencies = TRUE)
      library(l, character.only = TRUE)
    }
  }
}
libraries(req_lib)


ui <- fluidPage(
  h1("Markov chain", style="color:#337ab7;"),
  h3("Catene di Markov a stati dicreti e tempi continui"),
  
  sidebarPanel(
    width = 2,
    h4("Setting iniziale:"),
    
    sliderInput("perc",
                label = "Percentuale auto:",
                min = 0, max = 1, value = 1),
    
    sliderInput("mi_a",
                label = withMathJax("$$\\mu_a:$$"),
                min = 0, max = 1, value = .25),
    
    sliderInput("mi_m",
                label = withMathJax("$$\\mu_m:$$"),
                min = 0, max = 1, value = .25),
    
    sliderInput("lambda",
                label = withMathJax("$$\\lambda:$$"),
                min = 0, max = 1, value = 0)
  ),
  
  mainPanel(
    htmlOutput("picture1"),
    htmlOutput("picture2"),
    
    width = 8,
    tabsetPanel(
      type = "tabs",
      tabPanel("Dinamica",
               fluidRow(
                 plotOutput("plot_utilizzo")
               ),
               fluidRow(
                 uiOutput("txt")
               ),
               column(width=6)
      )
    )
  ),
  
  sidebarPanel(
    width = 2,
    
    radioButtons("x", 
                 "", choices = c("Evoluzione n veicoli", "Evoluzione utilizzo distributori")),
    
    sliderInput("nsim",
                "ampiezza intervallo considerato", min=10, max=10000, val=50),
    
    sliderInput("np",
                label = "Numero distributori:",
                min = 0, max = 10, value = 0),
    
    radioButtons("y", 
                 "", choices = c("Coda illimitata", "Coda limitata")),
    
    sliderInput("qs",
                label = "Coda massima:",
                min = 0, max = 100, value = 0)
  )
)




server <- function(input, output){
  
  y <- reactive({input$y})
  n <- reactive({as.double(input$nsim)})
  x <- reactive({input$x})
  q_size <- reactive({as.double(input$qs)})
  n_pump <- reactive({as.double(input$np)})
  mu_a <- reactive({as.double(input$mi_a)})
  mu_m <- reactive({as.double(input$mi_m)})
  lambda <- reactive({as.double(input$lambda)})
  p <- reactive({as.double(input$perc)})
  
  src1 <- "https://yt3.ggpht.com/J_veYK8e_aR9sNdtK4h6tqMnnMHzfQ6MEjS4a0tzV6pIjmVq19lUy5qX5qot6IWgbpGlobK7Yg=s900-c-k-c0x00ffffff-no-rj"
  output$picture1 <- renderText({c('<p><a href="https://www.youtube.com/@liberioltreSTEM/"> <img src=',src1,'width="40" height="40" align="right"/>')})
  
  src2 <- "https://www.liberioltreleillusioni.it/fileadmin/Websites/LiberiOltre/Assets/Img/logo-liberi-oltre-red-300.png"
  output$picture2 <- renderText({c('<p><a href="https://www.liberioltreleillusioni.it/"> <img src=',src2,'width="40" height="40" align="right"/>')})
  
  output$plot_utilizzo <- renderPlot({
    
    ifelse(y() == "Coda illimitata", qsize <- 1e9, qsize <- q_size())
    
    A <- matrix(c(1,   mu_a(),            0,
                  1, -lambda(), (1-p())*lambda(),
                  1,   mu_m(),       -mu_m()), byrow=T, ncol=3)
    B <- c(1, 0, 0)
    P <- solve(t(A), B)
    N_average_theor <- sum(P * c(1, 0, 1))
    
    set.seed(1234)
    
    option.3 <- function(t) {
      vehicle <- trajectory() %>%
        seize("pump", amount=1) %>%
        timeout(function() {
          if (runif(1) < p()) rexp(1, mu_a())  # car
          else rexp(1, mu_m())               # mcycle
        }) %>%
        release("pump", amount=1)
      
      simmer() %>%
        add_resource("pump", capacity=n_pump(), queue_size=qsize) %>%
        add_generator("vehicle", vehicle, function() rexp(1, lambda())) %>%
        run(until=t)
    }
    
    gas.station <- option.3(n())
    
    res <- tibble(get_mon_resources(gas.station))
    
    if(x() == "Evoluzione n veicoli"){
      res %>%
        transform(avg = c(0, cumsum(head(system, -1) * diff(time)))/time) %>%
        ggplot(aes(time, avg)) +
        geom_line(color = "red") +
        expand_limits(y = 0) +
        labs(title="Evoluzione n veicoli",
             x="tempo", y="numero di veicoli nel sistema", ) +
        theme_minimal()}
    
    else if(x() == "Evoluzione utilizzo distributori"){
      res %>%
        ggplot(aes(time, server)) +
        geom_line(color = "blue") +
        expand_limits(y = c(0, n_pump())) +
        labs(title="Evoluzione utilizzo distributori", x="tempo", y="distributori impiegati", ) +
        theme_minimal()}
    
  })
  
  output$txt <- renderUI({
    
    set.seed(1234)
    
    option.3 <- function(t) {
      vehicle <- trajectory() %>%
        seize("pump", amount=1) %>%
        timeout(function() {
          if (runif(1) < p()) rexp(1, mu_a())  
          else rexp(1, mu_m())               
        }) %>%
        release("pump", amount=1)
      
      simmer() %>%
        add_resource("pump", capacity=n_pump(), queue_size=q_size()) %>%
        add_generator("vehicle", vehicle, function() rexp(1, lambda())) %>%
        run(until=t)
    }
    
    gas.station <- option.3(n())
    
    res <- tibble(get_mon_resources(gas.station))
    
    HTML(paste0("probabilità che si tratti di un'auto: ", p()*100, "%", br(),
                "quanti minuti per un rifornimento di un'auto mediamente: ", round(mu_a()^-1, 2), br(),
                "quanti minuti per un rifornimento di una moto mediamente: ", round(mu_m()^-1, 2), br(),
                "quanta coda può accumularsi al massimo: ", q_size(), br(),
                "numero distributori: ", n_pump(), br(),
                "quanto intercorre tra un evento e il seguente: ", lambda(), br()))
  })
}


shinyApp(ui, server)
