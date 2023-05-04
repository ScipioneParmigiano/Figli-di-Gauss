# pcs.parmigiano@gmail.com


req_lib <- c("shiny", "markovchain", "ggplot2")

libraries <- function(req_lib){
  for(l in req_lib){
    if(!require(l, character.only = TRUE)){
      install.packages(l, dependencies = TRUE)
      library(l, character.only = TRUE)
    }
  }
}
libraries(req_lib)


##################### ui #######################################################

ui <- fluidPage(
  h1("Markov chain", style="color:#337ab7;"),
  h3("Catene di Markov a stati e tempi discreti"),
  
  sidebarPanel(
    width = 2,
    h4("Setting iniziale delle probabilità:"),
    
    sliderInput("na",
                label = "Nike da Adidas:",
                min = 0, max = 1, value = .19),
    
    sliderInput("no",
                label = "Nike da altro:",
                min = 0, max = 1, value = .35),
    
    
    sliderInput("an",
                label = "Adidas da Nike:",
                min = 0, max = 1, value = .22),
    
    sliderInput("ao", 
                label = "Adidas da altro:",
                min = 0, max = 1, value = .37),
    
    sliderInput("on", 
                label = "Altro da Nike:",
                min = 0, max = 1, value = .26),
    
    
    sliderInput("oa", 
                label = "Altro da Adidas:",
                min = 0, max = 1, value = .41)
  ),
  
  mainPanel(
    htmlOutput("picture1"),
    htmlOutput("picture2"),
    
    width = 8,
    tabsetPanel(
      type = "tabs",
      tabPanel("Grafo", plotOutput("plot_dd"),
               tableOutput("matr"),
               verbatimTextOutput("intro")), 
      tabPanel("Dinamica",
               fluidRow( 
                 textOutput("txt1"),
                 textOutput("txt2"),
                 textOutput("txt3")),
               fluidRow(
                 column(width=6,
                        plotOutput("g_limite"),
                        tableOutput("tabella2")
                  ),
                 column(width=6,
                      plotOutput("stat_distr"),
                      tableOutput("tabella")
                 )
              )        
            )
          )
        ),     
 
  
  sidebarPanel(
    width = 2,
    h4("Setting dello stato iniziale:"),
    sliderInput("un", 
                label = "Clienti Nike al presente:",
                min = 0, max = 100, value = 15),
    
    sliderInput("ua", 
                label = "Clienti Adidas al presente:",
                min = 0, max = 100, value = 13),
    
    sliderInput("uo", 
                label = "Clienti di altri marchi al presente:",
                min = 0, max = 100, value = 22),
    
    h4("Comandi intertemporali:"),
    
    selectInput("distr", "Seleziona:", c("Nessuna" = "n", 
                                         "Distribuzione limite" = "dl", 
                                         "Distribuzione stazionaria" = "ds", 
                                         "Entrambe" = "b"), 
                selected = "n"
                ),
    
    sliderInput("tm",
                label = "Tempo (1 anno)",
                min = 0, max = 100, value = 0)
    )
)


##################### server ###################################################

server <- function(input, output) {
  
  
  # ---------------- Intro e immagini ------------------------------------------
  
  output$intro <- renderText(
    "Un negozio che vende scarpe sportive è interessato a conoscere l'evoluzione del mercato negli anni
seguenti. Un'analisi di mercato ha rivelato le preferenze dei consumatori al presente, sappiamo anche
la quantità di utenti che cambiano marchio ogni anno. Supponendo questi dati stabili nel tempo, simuliamo
il mercato e le sue evoluzioni tramite una catena di Markov.")
  
  src1 = "https://yt3.ggpht.com/J_veYK8e_aR9sNdtK4h6tqMnnMHzfQ6MEjS4a0tzV6pIjmVq19lUy5qX5qot6IWgbpGlobK7Yg=s900-c-k-c0x00ffffff-no-rj"
  output$picture1<-renderText({c('<p><a href="https://www.youtube.com/@liberioltreSTEM/"> <img src=',src1,'width="40" height="40" align="right"/>')})
  
  src2 = "https://www.liberioltreleillusioni.it/fileadmin/Websites/LiberiOltre/Assets/Img/logo-liberi-oltre-red-300.png"
  output$picture2<-renderText({c('<p><a href="https://www.liberioltreleillusioni.it/"> <img src=',src2,'width="40" height="40" align="right"/>')})
  
  
  # ---------------- inizializzazioni ------------------------------------------
  
  na <- reactive({as.double(input$na)})
  no <- reactive({as.double(input$no)})
  an <- reactive({as.double(input$an)})
  ao <- reactive({as.double(input$ao)})
  on <- reactive({as.double(input$on)})
  oa <- reactive({as.double(input$oa)})
  un <- reactive({as.double(input$un)})
  ua <- reactive({as.double(input$ua)})
  uo <- reactive({as.double(input$uo)})
  u_tot <- reactive({sum(un(), ua(), uo())})
  t <- reactive({as.double(input$tm)})
  dstr <- reactive({
    if (input$distr == "b")         "b" 
    else if (input$distr == "dl")   "dl"
    else if (input$distr == "ds")   "ds"
    else if (input$distr == "n")    "n"
      })
  
  # ---------------- tab1 ------------------------------------------------------
  
  # grafico markov chain -------------------------------------------------------
  
  output$plot_dd <- renderPlot({
    
    p_matr <- matrix(c(1-ao()-an(), ao(), an(),
                       oa(), 1-oa()-on(), on(),
                       na(), no(), 1-na()-no()),
                     nrow = 3, byrow = T)
    
    m_matr <- new(states = c("Adidas", "Altro", "Nike"), 
                  "markovchain", transitionMatrix = p_matr)
    
    
    plot(m_matr)
    
  })
  
  output$matr <- renderTable({
    p_matr <- matrix(c(1-ao()-an(), ao(), an(),
                       oa(), 1-oa()-on(), on(),
                       na(), no(), 1-na()-no()),
                     nrow = 3, byrow = T)
    
    p_matr <- cbind(c("Adidas", "Altro", "Nike"), p_matr)
    colnames(p_matr) <- c("a / da", "Adidas", "Altro", "Nike")
    p_matr
  })
  

  # ---------------- tab2 ------------------------------------------------------
  
  # distribuzioni marginali ----------------------------------------------------
  
  output$txt1 <- renderText({
    
    marg_distr <- un()/u_tot() 
    paste0("Al presente i clienti Nike sono il: ", 
           round(marg_distr, 4)*100, "% del totale.")
    
  })
  
  output$txt2 <- renderText({
    
    marg_distr <- ua()/u_tot()
    paste0("Al presente i clienti Adidas sono il: ", 
           round(marg_distr, 4)*100, "% del totale.")
    
  })
  
  output$txt3 <- renderText({
    
    marg_distr <- uo()/u_tot()
    paste0("Al presente i clienti di altri marchi sono il: ", 
           round(marg_distr, 4)*100, "% del totale.")
    
  })
  
  
  # grafico distribuzione stazionaria ------------------------------------------
  
  output$stat_distr <- renderPlot({
    
    if(dstr() == "ds" | dstr() == "b"){
      
      p_matr <- matrix(c(1-oa()-na(), oa(), na(),
                         ao(), 1-ao()-no(), no(),
                         an(), on(), 1-an()-on()),
                       nrow = 3, byrow = T)
      
      m_matr <- new(states = c("Adidas", "Altro", "Nike"), 
                    "markovchain", transitionMatrix = p_matr)
      
      one_steady <- dim(steadyStates(m_matr)) == c(1,3)
      
        if(all(one_steady) == T){
        ggplot() + 
          geom_point(aes(x = c("Adidas", "Altro", "Nike"), 
                         y = c(steadyStates(m_matr)[1],
                               steadyStates(m_matr)[2],
                               steadyStates(m_matr)[3]),
                         color = c("Adidas", "Altro", "Nike"),
                         shape = c("Adidas", "Altro", "Nike")), size=8) +
          labs(x = "Marchio", y = "Probabilità") +
          theme(legend.title = element_blank())}
    }
  })
  
  # tabella distribuzione stazionaria ------------------------------------------
  
  output$tabella <- renderTable({
    if(dstr() == "ds" | dstr() == "b"){
      p_matr <- matrix(c(1-oa()-na(), oa(), na(),
                         ao(), 1-ao()-no(), no(),
                         an(), on(), 1-an()-on()),
                       nrow = 3, byrow = T)
      
      m_matr <- new(states = c("Adidas", "Altro", "Nike"), 
                    "markovchain", transitionMatrix = p_matr)
      
      steadyStates(m_matr)*100
      }
   })
  
  # grafico distribuzione limite ---------------------------------------------
  
    output$g_limite <- renderPlot({
      
      if(dstr() == "dl" | dstr() == "b"){
        p_matr <- matrix(c(1-oa()-na(), oa(), na(),
                           ao(), 1-ao()-no(), no(),
                           an(), on(), 1-an()-on()),
                         nrow = 3, byrow = T)
        
        m_matr <- new(states = c("Adidas", "Altro", "Nike"), 
                      "markovchain", transitionMatrix = p_matr)
        
        distr_tot <- c(ua(), uo(), un()) 
        
        initial_state <- (distr_tot * m_matr^0)/sum(distr_tot)
        
        one_steady <- dim(steadyStates(m_matr)) == c(1,3)
      
        if(all(one_steady) == T){
          
          if(t() != 0){
            
            ggplot()+
              geom_bar(aes(y = (after_stat(count))/sum(after_stat(count)),
                           x = markovchainSequence(1e4, m_matr^t()),
                           fill="orange")) + 
              theme(legend.position = "none") +
              labs(x = "Marchio", y = "Probabilità")
            
          } else {
           
            ggplot()+
              geom_bar(aes(y = (after_stat(count))/sum(after_stat(count)), 
                           rep(c("Adidas", "Altro", "Nike"), initial_state*1e3),
                           fill="orange"))+
              labs(x = "Marchio", y = "Probabilità") +
              theme(legend.position = "none")
          }
      }}
    })
  
  # tabella distribuzione limite -----------------------------------------------
  
  output$tabella2 <- renderTable({
    
    if(dstr() == "dl" | dstr() == "b"){
      
      p_matr <- matrix(c(1-oa()-na(), oa(), na(),
                         ao(), 1-ao()-no(), no(),
                         an(), on(), 1-an()-on()),
                       nrow = 3, byrow = T)
      
      m_matr <- new(states = c("Adidas", "Altro", "Nike"), 
                    "markovchain", transitionMatrix = p_matr)
      
      distr_tot <- c(un(), ua(), uo()) 
      
      t_state <- (distr_tot * m_matr ^ t())/sum(distr_tot)*100
      colnames(t_state) <- c("Adidas", "Altro", "Nike")
      
      t_state
    }
  })
}

shinyApp(ui, server)
