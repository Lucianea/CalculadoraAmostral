#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
ui = tagList(
  #shinythemes::themeSelector(),
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    "Calculadora Amostral",
    
    tabPanel("Amostragem Aleatória Simples",
             sidebarPanel(
               #fileInput("file", "File input:"),
               numericInput(inputId = "N",
                            label = "Tamanho da população:",
                            value = 5000),
               sliderInput("nc", "Nível de confiança %:", min = 90, max = 99.5, value = 95,
                           step = 0.5),
               
               h4("**Proporção amostral**"),
               sliderInput("me1", "Margem de erro %:", min = 0, max = 100, value = 5,
                           step = 1),
               sliderInput("p", "Percentual Estimado da população %:", min = 1, max = 100, value = 50,
                           step = 1),
              
               h4("**Variável Numérica**"),
               numericInput(inputId = "me2",
                            label = "Margem de erro (unidade de medida da variável):",
                            value = 2),
               numericInput(inputId = "S",
                            label = "Estimativa do desvio-padrão da variável:",
                            value = 12, min=0),
             
               actionButton("action2", "Calcular", class = "btn-primary")
    ),
             
             
             #Fim da barra lateral da aba amostragem aleatória simples
             #Inicio dos paineis da aba amostragem aleatória simples
             mainPanel(
               tabsetPanel(
                 tabPanel("Variável Categórica",
                          h2("Amostra Aleatória Simples"),
                          tableOutput("table"),
                          h4("Resultado"),
                          verbatimTextOutput("txtout"),
                          h4("Sorteando a amostra para você"),
                          h5("Sem reposição"),
                          verbatimTextOutput("txtout1"),
                          h6("Como referenciar esta página"),
                          h6("Alcoforado, L.F, Ross, S.D., Moraes, J.R.,outros colaboradores, Calculadora Amostral R-shiny. Disponível em <http://www.estatisticacomr.uff.br>. Acesso em [data]")
                 ),
                 #tabPanel("Sorteio","oi"),
                 tabPanel("Variável Numérica",
                          h2("Amostra Aleatória Simples"),
                          tableOutput("table2"),
                          h4("Resultado"),
                          verbatimTextOutput("txtout2"),
                          h4("Sorteando a amostra para você"),
                          h5("Sem reposição"),
                          verbatimTextOutput("txtout3"),
                          h6("Como referenciar esta página"),
                          h6("Alcoforado, L.F, Ross, S.D., Moraes, J.R.,outros colaboradores, Calculadora Amostral R-shiny. Disponível em <http://www.estatisticacomr.uff.br>. Acesso em [data]"
                          )
                 )
               )
             )
  ),
             
    #fim dos paineis da aba amostragem aleatória simples
    #Inicio dos paineis da aba amostragem estratificada
    tabPanel("Amostragem Estratificada", "Estamos construindo para você! Aguarde!"),
    #fim dos paineis da aba amostragem estratificada
    #Inicio dos paineis da aba amostragem sistemática
    tabPanel("Amostragem Sistemática", "Estamos construindo para você! Aguarde!"),
    tabPanel("Glossário", "Estamos construindo para você! Aguarde!")
  )
)
  #fim dos paineis da aba amostragem sistemática

# Define server logic required to draw a histogram
server = function(input, output) {
  
  output$txtout <- renderText({
    z=qnorm(1-(1-input$nc/100)/2)
    n=ceiling(input$N*(input$p/100*(1-input$p/100))*z^2/((z^2)*(input$p/100*(1-input$p/100))+((input$me1/100)^2)*(input$N-1)))
    paste(c("Selecione uma amostra de tamanho:"), n, sep = " ")
  })
  output$txtout1 <- renderText({
    z=qnorm(1-(1-input$nc/100)/2)
    n=ceiling(input$N*(input$p/100*(1-input$p/100))*z^2/((z^2)*(input$p/100*(1-input$p/100))+((input$me1/100)^2)*(input$N-1)))
    x=sample(1:input$N,size=n)
    paste(sort(x))
  })
  output$txtout2 <- renderText({
    z=qnorm(1-(1-input$nc/100)/2)
    n0=(z^2*input$S^2)/input$me2^2
    n=ceiling(input$N*n0/(n0+input$N))
    paste(c("Selecione uma amostra de tamanho:"), n, sep = " ")
  })
  output$txtout3 <- renderText({
    z=qnorm(1-(1-input$nc/100)/2)
    n0=(z^2*input$S^2)/input$me2^2
    n=ceiling(input$N*n0/(n0+input$N))
    x=sample(1:input$N,size=n)
    paste(sort(x))
  })
  output$table <- renderTable({
    tabela1=data.frame(Entrada=c("Tamanho da população",
                                 "Nível de Confiança %", "Margem de Erro %", "Proporção estimada %"),
                       Valor=c(input$N,input$nc,input$me1, input$p))
    
  })
  output$table2 <- renderTable({
    tabela2=data.frame(Entrada=c("Tamanho da população",
                                 "Nível de Confiança %", "Margem de Erro", "Desvio-padrão estimado"),
                       Valor=c(input$N,input$nc,input$me2, input$S))
    
  })
  
}



shinyApp(ui=ui,server = server)
