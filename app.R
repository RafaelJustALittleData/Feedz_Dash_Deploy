#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
# Define UI for application that draws a histogram
library(shiny)
library(shinydashboard)
library(plotly)
#library(shinydashboardPlus)
library(visNetwork)



#titulo<-dashboardHeader(title = "Analise Resultado Feedz",titleWidth = 450)
titulo=dashboardHeader()

titulo$children[[2]]$children <-  tags$a(href='http://just.bi',
                                         tags$img(src='logo.png',height='40',width='200'))


menu<- dashboardSidebar(width = 250,
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"
                        ),
                        sidebarMenu(id="Tab",
                                    menuItem("Tabela", tabName = "Tabela", icon = icon("dashboard")),
                                    menuItem("Visao Individuo", tabName = "VisaoIndividuo", icon = icon("dashboard")),
                                    menuItem("Visao Time",tabName="VisaoTime",icon=icon("dashboard")),
                                    menuItem("Visao Temporal",tabName="VisaoTemporal",icon=icon("dashboard")),
                                    
                                    menuItem('Wordclouds',tabName = 'Wordclouds',icon=icon('dashboard')),
                                    menuItem('Coocorrencia de termos',tabName = 'Grafo',icon=icon('dashboard')),
                                    menuItem("Visao de Gestor", tabName = "Segmentacao", icon = icon("dashboard")),
                                    menuItem("Sobre", tabName = "Sobre", icon = icon("th")),
                                    
                                    uiOutput('Gerador_Seletor_Pessoas'),
                                    selectInput('Seletor_Agrupador','Selecione se usaremos agrupador de valores ou não',choices=c('Sim','Nao'))
                        )
                        
                        
                        
)



## Body content
corpo<- dashboardBody(
  fileInput('Entrada',label='Incluir dados de avaliação aqui'),
  
  dataTableOutput('Correlacao'),
  
  
  tabItems(
    # First tab content
    tabItem(tabName = "Tabela",
            fluidRow(
              dataTableOutput("Tabela")
              
              
            )
    ),
    
    tabItem(tabName = "VisaoIndividuo",
            fluidRow(
              plotOutput('Visao_Comparativa')
              
              
            )
    ),
    
    tabItem(tabName = "VisaoTime",
            fluidRow(
              #plotOutput('Visao_Global')
              box(plotOutput('Visao_AutoAvaliacao')),
              box(plotOutput('Visao_Gestor')),
              box(plotOutput('Visao_Outros')),
              box(plotOutput('Visao_Media'))
              
            )
    ),
    
    tabItem(tabName = "VisaoTemporal",
            fluidRow(
              plotlyOutput('Temporal')
              
              
            )
    ),
    
    tabItem(tabName = "Wordclouds",
            fluidRow(
              plotOutput('WordCloud')
              
              
            )
    ),
    
    tabItem(tabName = "Grafo",
            fluidRow(
              numericInput('OcorrenciaMinima',label = 'Minima quantidade de vezes que uma palavra ocorre',value = 2,min=1,max=1000),
              plotOutput('Grafo'))
              #visNetworkOutput('Grafo_Dinamico'))
            
            
    ),


    tabItem(tabName = "Segmentacao",
            fluidRow(
                            plotlyOutput('Segmentacao_Time')
              
              
            )
    ),

    
  
  tabItem(tabName = "Sobre",
          fluidRow(
            uiOutput('GeradorSobre')
            
            
          )
  )
  ),

      

  
  
  
  
  
  
  
  
)






ui <- dashboardPage(titulo,menu,corpo,skin="black",title = "Analise Resultado Feedz")
# Define server logic required to draw a histogram
require(shiny)
source('Funcoes_Base.R')
require(plotly)
require(ggthemes)


require(tidyr)
require(dplyr)
require(lubridate)

server <- function(input, output) {
  #Funções reativas
  #_________________________________________________________________________________________  
  
  
  leitura_old_legada<-reactive({
    file <- input$Entrada
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Formato de arquivo errado"))
    
    
    df=read.csv(file$datapath,encoding = 'UTF-8')
    names(df)[1:3]=c('Competencia','Descricao','Autoavaliacao')
    df=df %>% mutate(Media_Ajustada=(Gestor.direto*0.5 + Outros.Avaliadores*0.2)/0.7 )
    
    
    return(df)
  })
  
  leitura_old<-reactive({
    file <- input$Entrada
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Formato de arquivo errado"))
    
    
    df=read.csv(file$datapath,encoding = 'UTF-8')
    #names(df)[1:3]=c('Competencia','Descricao','Autoavaliacao')
    
    df=df %>%  mutate(
      Data.da.resposta=dmy(Data.da.resposta),
      month = month(Data.da.resposta),
      year = year(Data.da.resposta),
      data=dmy(paste0('01-0',month,'-',year))
    ) 

    #df1=df[,c('Competência','Tipo.de.Avaliador','Nota','Feedback','Data.da.resposta','Avaliador')]
    Comentarios=df %>% select(Competência,Avaliador,Feedback,Avaliado=Nome,data) %>% pivot_wider(id_cols=c('Competência','Avaliado','data'),values_from='Feedback',names_from = 'Avaliador') 
    
    dim(Comentarios)[2]
    names(Comentarios)[4:dim(Comentarios)[2]]=paste0('Avaliador.',1:(dim(Comentarios)[2]-3) )
    #head(Comentarios)
    
    Notas=df %>% group_by(Competência,Tipo.de.Avaliador,Avaliado=Nome,data) %>% summarize(Nota=mean(Nota)) %>% pivot_wider(id_cols=c('Competência','Avaliado','data'),values_from='Nota',names_from = 'Tipo.de.Avaliador')
    #head(Notas)
    
    Result=Notas %>% inner_join(Comentarios) %>%
      rename(Outros.Avaliadores=`Avaliação do avaliador/par`,Gestor.direto=`Avaliação do gestor`,Competencia=Competência,Autoavaliacao=Autoavaliação) %>%
      mutate(Media.Final=0.3*Autoavaliacao + 0.5*Gestor.direto + 0.2*Outros.Avaliadores
      )
    # Para os casos que os outros não opinão
    Logico=is.na(Result[,c('Outros.Avaliadores')] )
    if(sum(Logico)>0){
      Result[Logico,'Media.Final']= (0.5 * Result[Logico,'Gestor.direto'] + 0.3 * Result[Logico,'Autoavaliacao'] )/0.8 
    }
    
    Logico=is.na(Result[,c('Autoavaliacao')] ) | Result[,c('Autoavaliacao')]==0
    if(sum(Logico)>0){
      Result[Logico,'Media.Final']= (0.5 * Result[Logico,'Gestor.direto'] )/0.5
      Result[Logico,'Autoavaliacao']=NA
    }
    
    df=Result
    print(df)
    
    
    return(df)
  })


  
  
  leitura<-reactive({
    df = leitura_old()
    df=df %>% filter(data==max(data)) #%>% select(-data)
    return(df)
  })
  
  
  leitura_Valores=reactive({
    
    df=leitura()
    aux=Dicionario()
    df=df %>% inner_join(aux) %>% group_by(Dict,Avaliado) %>% 
      summarize(Autoavaliacao=mean(Autoavaliacao),Gestor.direto=mean(Gestor.direto),Outros.Avaliadores=mean(Outros.Avaliadores),Media.Final=mean(Media.Final)) %>% mutate(Competencia=Dict) %>% ungroup() %>% select(-Dict)
    df=df %>% mutate(Media_Ajustada=(Gestor.direto*0.5 + Outros.Avaliadores*0.2)/0.7 )
    
    Logico=is.na(df[,c('Outros.Avaliadores')] )
    if(sum(Logico)>0){
      df[Logico,'Media_Ajustada']= df[Logico,'Gestor.direto']  
    }
    
    return(df)
  })
  
  
  leitura_Valores_old=reactive({
    
    df=leitura_old()
    aux=Dicionario()
    df=df %>% inner_join(aux) %>% group_by(Dict,Avaliado,data) %>% 
      summarize(Autoavaliacao=mean(Autoavaliacao),Gestor.direto=mean(Gestor.direto),Outros.Avaliadores=mean(Outros.Avaliadores),Media.Final=mean(Media.Final)) %>% mutate(Competencia=Dict) %>% ungroup() %>% select(-Dict)
    df=df %>% mutate(Media_Ajustada=(Gestor.direto*0.5 + Outros.Avaliadores*0.2)/0.7 )
    Logico=is.na(df[,c('Outros.Avaliadores')] )
    if(sum(Logico)>0){
      df[Logico,'Media_Ajustada']= df[Logico,'Gestor.direto']  
    }
    
    return(df)
  })
  
  
  tdm<-reactive({
    
    df=leitura()
    
    Colaborador=input$Seletor_Pessoas
    df=df %>% filter(Avaliado==Colaborador)
    Matriz=Gerar_Matriz_Termo_Documento(df,Colaborador)
    return(Matriz)
  })

  
    
  
  
  
  #Funcoes geradoras de UI
  #_______________________________________________________________________________
  
  output$Gerador_Seletor_Pessoas<- renderUI({
    
    arquivo=leitura_old()
    if(!is.null(arquivo)){
      Pessoas=unique(arquivo$Avaliado)
      selectInput('Seletor_Pessoas','Selecione as pessoa que quer observar os resultados',choices=Pessoas)
    }
  })
  
  output$GeradorSobre<-renderUI({
    
    if(input$Tab=="Sobre"){
      h4('Desenvolvido pelo time de Data Science\n Para mais detalhes ou expansão, abrir tarefa no kambam e comunicar o lider atual ')
      
      
    }
  })
  
  #Funcoes que geram telas
  #__________________________________________________________________________________________
  output$Visao_Comparativa<-renderPlot({
    
    
    if(input$Seletor_Agrupador=="Sim"){
      df=leitura_Valores()
    }else{
      df=leitura()
    }
    df=df[,-which(names(df)=="Media.Final")]
    Colaborador=input$Seletor_Pessoas
    #Gerar_Visual_Deia(df,Colaborador)
    p<-Gerar_Visual_Comparativo(df,Colaborador)
    p
    #ggplotly(p)
  })
  
  output$Visao_Global<-renderPlot({
    
    if(input$Seletor_Agrupador=="Sim"){
      df=leitura_Valores()
    }else{
      df=leitura()
    }
    
    #df=df[,-which(names(df)=="Media.Final")]
    Colaborador=input$Seletor_Pessoas
    p<-Gerar_Visual_Deia(df,Colaborador)
    #p<-Gerar_Visual_Comparativo(df,Colaborador)
    p
    #ggplotly(p)
  })
  
  output$Visao_AutoAvaliacao<-renderPlot({
    
    if(input$Seletor_Agrupador=="Sim"){
      df=leitura_Valores()
    }else{
      df=leitura()
    }
    
    #df=df[,-which(names(df)=="Media.Final")]
    Colaborador=input$Seletor_Pessoas
    p<-Gerar_Visual_Deia_Lista(df,Colaborador)
    #p<-Gerar_Visual_Comparativo(df,Colaborador)
    p$AutoAvaliacao
    #ggplotly(p)
  })
  
  output$Visao_Gestor<-renderPlot({
    
    if(input$Seletor_Agrupador=="Sim"){
      df=leitura_Valores()
    }else{
      df=leitura()
    }
    
    #df=df[,-which(names(df)=="Media.Final")]
    Colaborador=input$Seletor_Pessoas
    p<-Gerar_Visual_Deia_Lista(df,Colaborador)
    #p<-Gerar_Visual_Comparativo(df,Colaborador)
    p$Gestor
    #ggplotly(p)
  })
  
  output$Visao_Outros<-renderPlot({
    
    if(input$Seletor_Agrupador=="Sim"){
      df=leitura_Valores()
    }else{
      df=leitura()
    }
    
    #df=df[,-which(names(df)=="Media.Final")]
    Colaborador=input$Seletor_Pessoas
    p<-Gerar_Visual_Deia_Lista(df,Colaborador)
    #p<-Gerar_Visual_Comparativo(df,Colaborador)
    p$Outros
    #ggplotly(p)
  })
  
  output$Visao_Media<-renderPlot({
    
    if(input$Seletor_Agrupador=="Sim"){
      df=leitura_Valores()
    }else{
      df=leitura()
    }
    
    #df=df[,-which(names(df)=="Media.Final")]
    Colaborador=input$Seletor_Pessoas
    p<-Gerar_Visual_Deia_Lista(df,Colaborador)
    #p<-Gerar_Visual_Comparativo(df,Colaborador)
    p$Media
    #ggplotly(p)
  })
  
  
  output$Tabela<-renderDataTable({
    if(input$Seletor_Agrupador=="Sim"){
      df=leitura_Valores()
    }else{
      df=leitura()
    }
    Colaborador=input$Seletor_Pessoas
    df %>% filter(Avaliado==Colaborador)
    
  })
  
  output$Correlacao<-renderDataTable({
    if(input$Tab=="Tabela"){
      if(input$Seletor_Agrupador=="Sim"){
        df=leitura_Valores()
      }else{
        df=leitura()
      }
      Colaborador=input$Seletor_Pessoas
      df=df %>% filter(Avaliado==Colaborador)
      df$Autoavaliacao=(df$Autoavaliacao<=3.5 & df$Autoavaliacao>=3.0 )*1 + (df$Autoavaliacao>3.5)*2#(df$Autoavaliacao==3)*1 + (df$Autoavaliacao>3)*2
      df$Gestor.direto=(df$Gestor.direto<=3.5 & df$Gestor.direto>=3.0 )*1 + (df$Gestor.direto>3.5)*2#(df$Gestor.direto==3)*1 + (df$Gestor.direto>3)*2
      df$Outros.Avaliadores=(df$Outros.Avaliadores<=3.5 & df$Outros.Avaliadores>=3.0 )*1 + (df$Outros.Avaliadores>3.5)*2
      lista=list(Origem=c('Autoavaliacao','Autoavaliacao','Gestor'),Destino=c('Gestor','Outros','Outros'),Score=c(mean(df$Autoavaliacao==df$Gestor.direto,na.rm=TRUE),mean(df$Autoavaliacao==df$Outros.Avaliadores,na.rm=TRUE),mean(df$Outros.Avaliadores==df$Gestor.direto,na.rm=TRUE)  ) )
      
      return(data.frame(lista))
      #Matriz=cor(df[,c('Autoavaliacao','Gestor.direto','Outros.Avaliadores')],method='pearson') %>% melt() %>% filter(Var1!=Var2)
      #Matriz=Matriz[!duplicated(Matriz$value),]
      #names(Matriz)[3]='pearson'
      #Matriz
    }
  })
  
  output$WordCloud<-renderPlot({
    Matriz=tdm()
    Frequencia=Gerar_WordCloud(Matriz)
    print(Frequencia)
    require(wordcloud)
    #print(data.frame(word=names(Frequencia),Contagem=as.numeric(Frequencia)) %>% inner_join(parts_of_speech) )
    
    wordcloud(names(Frequencia),as.numeric(Frequencia)) 
  })
  
  output$Grafo<-renderPlot({
    Matriz=tdm()
    Matriz=t(Matriz)
    Grafo=Gerar_Grafo(Matriz,input$OcorrenciaMinima)
    plot(Grafo)
  })
  
  output$Grafo_Dinamico<-renderVisNetwork({
    Matriz=tdm()
    Matriz=t(Matriz)
    Grafo=Gerar_Grafo(Matriz,input$OcorrenciaMinima)
    visIgraph(Grafo)
    
  })
  
  output$Temporal<-renderPlotly({
    #renderPlotly({
    if(input$Seletor_Agrupador=="Sim"){
      df=leitura_Valores_old()
    }else{
      df=leitura_old()
    }
    require(reshape2)
    Colaborador=input$Seletor_Pessoas
    df=df %>% filter(Avaliado==Colaborador)
    df=df %>% ungroup()
    df=df %>% select(Competencia,data,Outros.Avaliadores,Gestor.direto,Media.Final)
    df = df %>% melt(id=c('data','Competencia') )
    print(df)
    require(lubridate)
    p<-ggplot(df) + aes(x=data,y=value,colour=Competencia) + geom_line() + facet_wrap(~variable) + labs(x='Data de referencia') + ylim(1,5) + geom_point()
    ggplotly(p)
    
  })

  output$Segmentacao_Time<-renderPlotly({
        
      if(input$Seletor_Agrupador=="Sim"){
        df=leitura_Valores()
      }else{
        df=leitura()
      }
      #print(df)
      Resultado=df %>% select(Media.Final,Avaliado,Competencia)
      Resultado=Resultado[complete.cases(Resultado),]
      print(Resultado)
      Resultado=Resultado %>% pivot_wider(id_cols='Avaliado',values_from='Media.Final',names_from='Competencia')
      print(Resultado)
      PCA=prcomp(Resultado[,-1],scale=TRUE)
      sdev <- PCA$sdev
      variance_explained_manual <- (sdev^2) / sum(sdev^2)
      variance_first_two_manual <- sum(variance_explained_manual[1:2])
      pca_data=as.data.frame(PCA$x)
      loadings <- as.data.frame(PCA$rotation)
      pca_data[,'Avaliado']=Resultado[,1]

      p <- ggplot(pca_data, aes(x = PC1, y = PC2)) +
      geom_text(size = 3,aes(label = Avaliado)) +
      geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5), 
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
      geom_text(data = loadings, aes(x = PC1 * 5, y = PC2 * 5, label = rownames(loadings)), 
            color = "red", vjust = 1, hjust = 1,position = position_jitter(width = 0.3, height = 0.3)) +
      theme_minimal() +
      labs(title = "PCA do time",
         x = paste0("First Principal Component: ",variance_explained_manual[1] *100,'%'),
         y = paste0("Second Principal Component: ",variance_explained_manual[2] *100,'%'),subtitle=paste('Percentual de variabilide explicada é de ',variance_first_two_manual))

      ggplotly(p)

  })
  
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
