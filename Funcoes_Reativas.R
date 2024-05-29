require(shiny)

leitura<-reactive({
  if(!is.null(input$Entrada$datapath) ){
    
    dataset=read.csv(input$Entrada$datapath)
    return(dataset)
  }
  return(NA)
  
  
  
})

leitura<-reactive({
file <- input$Entrada
ext <- tools::file_ext(file$datapath)

req(file)
validate(need(ext == "csv", "Formato de arquivo errado"))

return(read.csv(file$datapath))
})