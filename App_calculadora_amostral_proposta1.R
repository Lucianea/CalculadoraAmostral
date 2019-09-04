

library(shiny)
library(shinyjs)


# Define UI for application that draws a histogram
ui = tagList(
  shinyjs::useShinyjs(),
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    "Calculadora Amostral",
    tabPanel("Amostragem Aleatória Simples",
             sidebarPanel(
               #fileInput("file", "File input:"),
               numericInput(inputId = "N",
                            label = "Tamanho da população:",
                            value = 2000),
               sliderInput("nc", "Nível de confiança %:", min = 90, max = 99, value = 95,
                           step = 0.5),
               
               
               br(),

               
               radioButtons("checkbox", "Selecione o tipo",
                            c("Proporção amostral" = "propamostra",
                              "Média amostral" = "medamostra")),
               
               numericInput(inputId = "me2",
                            label = "Margem de erro (unidade de medida da variável):",
                            value = 2),
               numericInput(inputId = "S",
                            label = "Estimativa do desvio-padrão da variável:",
                            value = 12, min=0),
               a(id = "toggleAdvanced2", "Mostre-me as fórmulas"),
               hidden(
                 div(id = "advanced2",
                     helpText ("N é o tamanho da população"),
                     helpText ("d é o erro amostral ou a margem de erro"), 
                     helpText ("z é o percentil da distribuição normal correspondente ao nível de confiança fixado"),
                     h4("Média amostral"),
                     helpText ("S é o desvio-padrão populacional da variável de pesquisa. Quando não se tem a informação prévia sobre S a partir de outros estudos, estima-se pelo desvio padrão amostral de uma amostra piloto"),
                     withMathJax(),
                     helpText('O tamanho da amostra é obtido por'),
                     helpText('$$n=\\frac{n_0*N}{(N+n_0)}$$'),
                     helpText('$$n_0=\\frac{z^2}{d^2}*S^2$$')
                     
                 )
               ),
               
               sliderInput("p", "Percentual Estimado da população %:", min = 1, max = 100, value = 50,
                           step = 1),
               sliderInput("me1", "Margem de erro %:", min = 0, max = 100, value = 3,
                           step = 1),
               a(id = "toggleAdvanced", "Mostre-me as fórmulas"),
               hidden(
                 div(id = "advanced1",
                     helpText ("N é o tamanho da população"),
                     helpText ("d é o erro amostral ou a margem de erro"), 
                     helpText ("z é o percentil da distribuição normal correspondente ao nível de confiança fixado"),
                     h4("Proporção amostral"),
                     helpText ("P é a proporção de elementos na população com a característica de interesse. Quando não se tem a informação prévia sobre P utiliza-se P=0.50"),
                     withMathJax(),
                     helpText('O tamanho da amostra é obtido por'),
                     helpText('$$n=\\frac{n_0*N}{(N+n_0)}$$'),
                     helpText('$$n_0=\\frac{z^2}{d^2}*\\frac{N}{(N-1)}*P(1-P)$$')
                     
                 )
               )
             ),
             
             #Fim da barra lateral da aba amostragem aleatória simples
             #Inicio dos paineis da aba amostragem aleatória simples
             mainPanel(
               tabsetPanel(
                 tabPanel("Calculadora Amostral",
                          h2("Amostra Aleatória Simples"),
                          tableOutput("table"),
                          h4("Resultado"),
                          verbatimTextOutput("txtout"),
                          h4("Sorteando a amostra para você"),
                          h5("Sem reposição"),
                          verbatimTextOutput("txtout1"),
                          h6("Como referenciar esta página"),
                          h6("Alcoforado, L.F, Ross, S.D., Moraes, J.R.,outros colaboradores, Calculadora Amostral R-shiny. Disponível em <http://www.estatisticacomr.uff.br>. Acesso em [data]")
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
  #fim dos paineis da aba amostragem sistemática
)
# Define server logic required to draw a histogram
server = function(input, output) {
  
  onclick("toggleAdvanced", toggle(id = "advanced1", anim = TRUE))
  
  onclick("toggleAdvanced2", toggle(id = "advanced2", anim = TRUE))
  
  observe({
    toggle(id = "S", condition = input$checkbox=="medamostra")
    toggle(id = "me2", condition = input$checkbox=="medamostra")
    toggle(id = "p", condition = input$checkbox=="propamostra")
    toggle(id = "me1", condition = input$checkbox=="propamostra")
    toggle(id= "toggleAdvanced", condition = input$checkbox=="propamostra")
    toggle(id = "toggleAdvanced2", condition = input$checkbox=="medamostra")
    
    
  })
  
  
  
  
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
