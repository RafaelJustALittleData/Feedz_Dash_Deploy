require(shiny)

output$Gerador_Seletor_Pessoas<- renderUI({
  
  arquivo=leitura()
  #if(!is.null(arquivo)){
    Pessoas=unique(arquivo$Avaliado)
    selectInput('Seletor_Pessoas','Selecione as pessoa que quer observar os resultados',choices=Pessoas)
  #}
})