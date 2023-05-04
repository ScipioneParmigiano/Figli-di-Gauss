# pcs.parmigiano@gmail.com

# simulazioni monte carlo

library(shiny)
library(ggplot2)
library(ggplot2)
library(tibble)


##################### ui #######################################################

ui <- fluidPage(
  h1("Simulazioni Monte Carlo", style="color:#337ab7;"),
  
  sidebarPanel(
    width = 2,
    h4("Roulette"),
    
    radioButtons("r",
                 label = "immagine roulette",
                 choices = c("mostra", "nascondi"), selected = "mostra"),
    
    radioButtons("fair",
                 label = "Gioco",
                 choices = c("fair", "unfair"), selected = "fair"),
    
    sliderInput("n_games",
                label = "Numero puntate:",
                min = 2, max = 2000, value = 2),
    
    sliderInput("n_players",
                label = "Numero giocatori:",
                min = 1, max = 500, value = 1),
    
    radioButtons("type",
                 label = "Tipo puntate:",
                 choices = c("pari/dispari", "numero singolo"), selected = "pari/dispari"),
    
    radioButtons("normalized",
                 label = "Normalizzato",
                 choices = c("F", "T"), selected = "F"),
    
    radioButtons("norm",
                 label = "Normale",
                 choices = c("F", "T"), selected = "F")
  ),
  
  mainPanel(
    htmlOutput("picture1"),
    htmlOutput("picture2"),
    
    width = 8,
    tabsetPanel(
      type = "tabs",
      tabPanel("1",
               htmlOutput("roulette"),
               plotOutput("clt"),
               tableOutput("tab"),
               verbatimTextOutput("exp_val")),
      
      tabPanel("2",
               verbatimTextOutput("lln")
      )
    )
  )
)


##################### server ###################################################

server <- function(input, output) {
  
  src1 = "https://yt3.ggpht.com/J_veYK8e_aR9sNdtK4h6tqMnnMHzfQ6MEjS4a0tzV6pIjmVq19lUy5qX5qot6IWgbpGlobK7Yg=s900-c-k-c0x00ffffff-no-rj"
  output$picture1 <- renderText({c('<p><a href="https://www.youtube.com/@liberioltreSTEM/"> <img src=',src1,'width="40" height="40" align="right"/>')})
  
  src2 = "https://www.liberioltreleillusioni.it/fileadmin/Websites/LiberiOltre/Assets/Img/logo-liberi-oltre-red-300.png"
  output$picture2 <- renderText({c('<p><a href="https://www.liberioltreleillusioni.it/"> <img src=',src2,'width="40" height="40" align="right"/>')})
  
  src3 = "https://img.freepik.com/free-vector/roulette-background-design_1284-946.jpg?w=2000"
  output$roulette <- renderText({if(input$r == "mostra") c('<p><a> <img src=',src3,'width="1070" height="1070"/>')})
  # funzioni e inizializzazioni ---------------------------------------------
  
  n_games <- (reactive({as.double(input$n_games)}))
  n_players <- (reactive({as.double(input$n_players)}))
  
  # red and black fair ------------------------------------------------------
  
  
  fairRoulette_rb <- function(n_spin){
    
    player_returns <- 100
    for(i in 1:n_spin){
      
      player_bet <- 1
      ball <- sample(0:1, 1)
      
      if(player_bet == ball){
        player_returns[i] <- +1
      } else {
        player_returns[i] <- -1
      }
    }
    return(player_returns)
  }
  
  fairRoulette_rb_2 <- function(n_play){
    n_spin <- 10
    
    player_returns <- 100
    for(i in 1:n_play){
      
      player_returns[i] <- mean(fairRoulette_rb(n_spin))
    } 
    return(player_returns)
  }
  
  # num fair ----------------------------------------------------------------
  
  
  fairRoulette_num <- function(n_spin){
    
    player_returns <- 100
    for(i in 1:n_spin){
      
      player_bet <- 1
      ball <- sample(1:36, 1)
      
      if(player_bet == ball){
        player_returns[i] <- + 35
      } else {
        player_returns[i] <- - 1
      }
    }
    return(player_returns)
  }
  
  fairRoulette_num_2 <- function(n_play){
    
    n_spin <- 10
    
    player_returns <- 100
    for(i in 1:n_play){
      
      player_returns[i] <- mean(fairRoulette_num(n_spin))
    }
    return(player_returns)
  }
  
  # red and black and green unfair ----------------------------------------------------
  
  
  unfairRoulette_rbg <- function(n_spin){
    
    player_returns <- 100
    for(i in 1:n_spin){
      
      player_bet <- 1
      ball <- sample(c(rep(c(0, 1), 18), -1), 1)
      
      if(player_bet == ball){
        player_returns[i] <- + 1
      } else {
        player_returns[i] <- - 1
      }
    }
    return(player_returns)
  }
  
  unfairRoulette_rbg_2 <- function(n_play){
    n_spin <- 10  
    
    player_returns <- 100
    for(i in 1:n_play){
      
      player_returns[i] <- mean(unfairRoulette_rbg(n_spin))
    }
    return(player_returns)
  }
  
  
  # num unfair --------------------------------------------------------------
  
  
  unfairRoulette_num <- function(n_spin){
    
    player_returns <- 100
    for(i in 1:n_spin){
      
      player_bet <- 1
      ball <- sample(0:36, 1)
      
      if(player_bet == ball){
        player_returns[i] <- + 35
      } else {
        player_returns[i] <- - 1
      }
    }
    return(player_returns)
  }
  
  unfairRoulette_num_2 <- function(n_play){
    n_spin <- 10
    
    player_returns <- 100
    for(i in 1:n_play){
      
      player_returns[i] <- mean(unfairRoulette_num(n_spin))
    }
    return(player_returns)
  }
  # dfs ---------------------------------------------------------------------
  
  df2 <- reactive({
    
    set.seed(2)
    
    rs_2 <- tibble(f_c = fairRoulette_rb_2(n_players()), 
                   f_n = fairRoulette_num_2(n_players()),
                   u_c = unfairRoulette_rbg_2(n_players()),
                   u_n = unfairRoulette_num_2(n_players()))
    for(i in 1:length(rs_2$f_c)){
      rs_2$nf_c <- (rs_2$f_c - mean(rs_2$f_c))/sd(rs_2$f_c)
      rs_2$nf_n <- (rs_2$f_n - mean(rs_2$f_n))/sd(rs_2$f_n)
      rs_2$nu_c <- (rs_2$u_c - mean(rs_2$u_c))/sd(rs_2$u_c)
      rs_2$nu_n <- (rs_2$u_n - mean(rs_2$u_n))/sd(rs_2$u_n)
    }
    return(rs_2)
    
  })
  
  df <- reactive({
    
    set.seed(2)
    
    rs <- tibble(f_c = fairRoulette_rb(n_games()), 
                 f_n = fairRoulette_num(n_games()),
                 u_c = unfairRoulette_rbg(n_games()),
                 u_n = unfairRoulette_num(n_games()))
    for(i in 1:length(rs$f_c)){
      rs$nf_c <- (rs$f_c - mean(rs$f_c))/sd(rs$f_c)
      rs$nf_n <- (rs$f_n - mean(rs$f_n))/sd(rs$f_n)
      rs$nu_c <- (rs$u_c - mean(rs$u_c))/sd(rs$u_c)
      rs$nu_n <- (rs$u_n - mean(rs$u_n))/sd(rs$u_n)
    }
    return(rs)
  }) 
  
  
  
  
  # output ------------------------------------------------------------------
  
  
  # roulette ----------------------------------------------------------------
  
  
  output$clt <- renderPlot({
    
    ifelse(n_players() == 1, df <- df(), df <- df2())
    
    if(input$fair == "fair" & input$type == "pari/dispari"){
      
      if(input$normalized == "F"){
        ggplot(df) +
          geom_histogram(aes(x=f_c), fill = "#4287f5") +
          theme_classic() +
          geom_vline(xintercept = mean(df$f_c), linewidth=1.5)
        
      } else if(input$normalized == "T" & input$norm == "F"){
        ggplot(df) +
          geom_histogram(aes(x=nf_c, y=..density..), fill = "#4287f5") +
          theme_classic() +
          geom_vline(xintercept = mean(df$nf_c), linewidth=1.5)
        
      } else if(input$normalized == "T" & input$norm == "T"){
        
        ggplot(df) +
          geom_histogram(aes(x=nf_c, y=..density..), fill = "#4287f5") +
          theme_classic() +
          geom_vline(xintercept = mean(df$nf_c), linewidth=1.5)+
          geom_function(fun = dnorm, col = "red", linewidth=1.5)
        
      }
      
    } else if(input$fair == "fair" & input$type == "numero singolo"){
      
      if(input$normalized == "F"){
        ggplot(df) +
          geom_histogram(aes(x=f_n), fill = "#4287f5") +
          theme_classic() +
          geom_vline(xintercept = mean(df$f_n))
        
      } else if(input$normalized == "T" & input$norm == "F"){
        ggplot(df) +
          geom_histogram(aes(x=nf_n, y=..density..), fill = "#4287f5") +
          theme_classic() +
          geom_vline(xintercept = mean(df$nf_n), linewidth=1.5)
        
      } else if(input$normalized == "T" & input$norm == "T"){
        
        ggplot(df) +
          geom_histogram(aes(x=nf_n, y=..density..), fill = "#4287f5") +
          theme_classic() +
          geom_vline(xintercept = mean(df$nf_n), linewidth=1.5)+
          geom_function(fun = dnorm, col = "red", linewidth=1.5)
        
      }
      
    } else if(input$fair == "unfair" & input$type == "pari/dispari"){
      
      if(input$normalized == "F"){
        ggplot(df) +
          geom_histogram(aes(x=u_c), fill = "#4287f5") +
          theme_classic() +
          geom_vline(xintercept = mean(df$u_c))
        
      } else if(input$normalized == "T" & input$norm == "F"){
        ggplot(df) +
          geom_histogram(aes(x=nu_c, y=..density..), fill = "#4287f5") +
          theme_classic() +
          geom_vline(xintercept = mean(df$nu_c), linewidth=1.5)
        
      } else if(input$normalized == "T" & input$norm == "T"){
        
        ggplot(df) +
          geom_histogram(aes(x=nu_c, y=..density..), fill = "#4287f5") +
          theme_classic() +
          geom_vline(xintercept = mean(df$nu_c), linewidth=1.5)+
          geom_function(fun = dnorm, col = "red", linewidth=1.5)
        
      }
      
      
    } else if(input$fair == "unfair" & input$type == "numero singolo"){
      
      if(input$normalized == "F"){
        ggplot(df) +
          geom_histogram(aes(x=u_n), fill = "#4287f5") +
          theme_classic() +
          geom_vline(xintercept = mean(df$u_n))
        
      } else if(input$normalized == "T" & input$norm == "F"){
        ggplot(df) +
          geom_histogram(aes(x=nu_n, y=..density..), fill = "#4287f5") +
          theme_classic() +
          geom_vline(xintercept = mean(df$n_un), linewidth=1.5)
        
      } else if(input$normalized == "T" & input$norm == "T"){
        
        ggplot(df) +
          geom_histogram(aes(x=nu_n, y=..density..), fill = "#4287f5") +
          theme_classic() +
          geom_vline(xintercept = mean(df$n_un), linewidth=1.5) +
          geom_function(fun = dnorm, col = "red", linewidth=1.5)
        
      }
      
    }
    
  })
  
  output$exp_val <- renderText({
    if(n_players() != 1){
      
      if(input$fair == "fair" & input$type == "pari/dispari"){
        paste0("media campionaria: ", round(mean(df2()$f_c), 3),
               "\nvalore atteso: ", round(100 + 1*18/36 - 18/36, 3))
        
      } else if(input$fair == "fair" & input$type == "numero singolo"){
        paste0("media campionaria: ", round(mean(df2()$f_n), 3),
               "\nvalore atteso: ", round(100 - 35/36 + 35*1/36, 3))
        
      } else if(input$fair == "unfair" & input$type == "numero singolo"){
        paste0("media campionaria: ", round(mean(df2()$u_n), 3),
               "\nvalore atteso: ", round(100 - 19/37 + 18/37, 3))
        
      } else if(input$fair == "unfair" & input$type == "pari/dispari"){
        paste0("media campionaria: ", round(mean(df2()$u_c), 3),
               "\nvalore atteso: ", round(100 - 36/37 + 35*1/37, 3))
      }
      
    } else if(n_players() == 1){
      
      if(input$fair == "fair" & input$type == "pari/dispari"){
        paste0("media campionaria dei guadagni: ", round(mean(df()$f_c), 3),
               "\nvalore atteso: ", round(100 + 1*18/36 - 18/36, 3))
        
      } else if(input$fair == "fair" & input$type == "numero singolo"){
        paste0("media campionaria dei guadagni: ", round(mean(df()$f_n), 3),
               "\nvalore atteso: ", round(100 - 35/36 + 35*1/36, 3))
        
      } else if(input$fair == "unfair" & input$type == "numero singolo"){
        paste0("media campionaria dei guadagni: ", round(mean(df()$u_n), 3),
               "\nvalore atteso: ", round(100 - 19/37 + 18/37, 3))
        
      } else if(input$fair == "unfair" & input$type == "pari/dispari"){
        paste0("media campionaria dei guadagni: ", round(mean(df()$u_c), 3),
               "\nvalore atteso: ", round(100 - 36/37 + 35*1/37, 3))
      }
    } 
  })
  
  
  
  output$tab <- renderTable({
    ifelse(n_players() == 1, df <- df(), df <- df2())
    tail(df[,1:4], 5)
  })
  
  
  # lln ---------------------------------------------------------------------
  
  output$lln <- renderText({
    paste0("cosa NON è a legge dei grandi numeri: \n\n",
           "P(R,R,R,R,R | fair) = ", round(1/2^5*100, 3), "%. P(R) dopo RRRRR?\n")
  })
  
}

shinyApp(ui, server)



# correggere valore di realizzazione finale e media campinaria, sospetto
# l'errore sia nella funzione.