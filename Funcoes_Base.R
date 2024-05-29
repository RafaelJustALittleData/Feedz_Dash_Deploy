require(dplyr)
require(reshape2)
require(ggplot2)


Dicionario<-function(){
  require(dplyr)
  Core=list(data.frame('Competencia'='Inquietude','Dict'='Valores'),
            data.frame('Competencia'='Conhecimento profundo do que faz','Dict'='Valores'),
            data.frame('Competencia'='Foco na solução','Dict'='Valores'),
            data.frame('Competencia'='Transparência','Dict'='Valores'),
            data.frame('Competencia'='Assertividade','Dict'='Valores'),
            
            data.frame('Competencia'='Ownership','Dict'='Empredimento'),
            data.frame('Competencia'='Resolver / Fazer','Dict'='Empredimento'),
            
            data.frame('Competencia'='Gestão do Tempo','Dict'='Disponibilidade'),
            data.frame('Competencia'='Pontualidade','Dict'='Disponibilidade'),
            data.frame('Competencia'='Acessibilidade','Dict'='Disponibilidade'),
            
            data.frame('Competencia'='Relacionamento interpessoal com outras áreas','Dict'='Colaboração'),
            data.frame('Competencia'='Relacionamento interpessoal com o time','Dict'='Colaboração'),
            data.frame('Competencia'='Comunicação','Dict'='Colaboração'),
            data.frame('Competencia'='Prontidão','Dict'='Colaboração'),
            
            data.frame('Competencia'='Domínio do Workspace','Dict'='Hard Skills'),
            data.frame('Competencia'='Domínio de Ferramentas / Softwares','Dict'='Hard Skills'),
            
            data.frame('Competencia'='Inteligência emocional','Dict'='Postura'),
            data.frame('Competencia'='Maturidade profissional','Dict'='Postura'),
            
            data.frame('Competencia'='Potencial','Dict'='Performance'),
            data.frame('Competencia'='Desempenho','Dict'='Performance')
  ) %>% bind_rows()
  
  Encoding(Core$Competencia) <-'UTF-8'
  return(Core)
  
}




Gerar_Visual_Deia<-function(df,Colaborador){
  require(reshape2)
  require(ggplot2)
  require(ggthemes)
  
  
  require(dplyr)
  Core_Comparacao = df %>% 
    filter(Avaliado!=Colaborador) %>% group_by(Competencia) %>% 
    summarize(Autoavaliacao=mean(Autoavaliacao),	Gestor=mean(Gestor.direto)	,Outros=mean(Outros.Avaliadores)	,Media=mean(Media.Final)	) %>%
    mutate(Avaliado="Referencia") %>%
    rbind( df %>% filter(Avaliado==Colaborador) %>% select(Avaliado,Competencia,Autoavaliacao,Gestor=Gestor.direto,Outros=Outros.Avaliadores,Media=Media.Final)  )
  
  
  Grafico<-Core_Comparacao %>% melt() %>%
    ggplot(aes(x=Competencia, y=value)) +
    geom_line(aes(col = Avaliado,group=Avaliado)) + geom_point(aes(col=Avaliado)) + coord_polar()+ geom_hline(yintercept = 3) + ylim(0,5) +
    theme_excel_new() + labs(x='Nota',y='') + scale_fill_manual(values = c( "red", "blue",'white')) + facet_wrap(~variable)

  return(Grafico)
}



Gerar_Visual_Deia_Lista<-function(df,Colaborador){
  require(reshape2)
  require(ggplot2)
  require(ggthemes)
  
  
  require(dplyr)
  Core_Comparacao = df %>% 
    filter(Avaliado!=Colaborador) %>% group_by(Competencia) %>% 
    summarize(Autoavaliacao=mean(Autoavaliacao),	Gestor=mean(Gestor.direto)	,Outros=mean(Outros.Avaliadores)	,Media=mean(Media.Final)	) %>%
    mutate(Avaliado="Referencia") %>%
    rbind( df %>% filter(Avaliado==Colaborador) %>% select(Avaliado,Competencia,Autoavaliacao,Gestor=Gestor.direto,Outros=Outros.Avaliadores,Media=Media.Final)  )
  
  
  AutoAvaliacao<-Core_Comparacao %>% melt() %>% filter(variable=='Autoavaliacao') %>%
    ggplot(aes(x=Competencia, y=value)) +
    geom_line(aes(col = Avaliado,group=Avaliado)) + geom_point(aes(col=Avaliado)) + coord_polar()+ geom_hline(yintercept = 3) + ylim(0,5) +
    theme_excel_new() + labs(x='Nota',y='') + scale_fill_manual(values = c( "red", "blue",'white')) + facet_wrap(~variable)
  
  
  Gestor<-Core_Comparacao %>% melt() %>% filter(variable=='Gestor') %>%
    ggplot(aes(x=Competencia, y=value)) +
    geom_line(aes(col = Avaliado,group=Avaliado)) + geom_point(aes(col=Avaliado)) + coord_polar()+ geom_hline(yintercept = 3) + ylim(0,5) +
    theme_excel_new() + labs(x='Nota',y='') + scale_fill_manual(values = c( "red", "blue",'white')) + facet_wrap(~variable)
  
  
  Outros<-Core_Comparacao %>% melt() %>% filter(variable=='Outros') %>%
    ggplot(aes(x=Competencia, y=value)) +
    geom_line(aes(col = Avaliado,group=Avaliado)) + geom_point(aes(col=Avaliado)) + coord_polar()+ geom_hline(yintercept = 3) + ylim(0,5) +
    theme_excel_new() + labs(x='Nota',y='') + scale_fill_manual(values = c( "red", "blue",'white')) + facet_wrap(~variable)
  
  
  
  Media<-Core_Comparacao %>% melt() %>% filter(variable=='Media') %>%
    ggplot(aes(x=Competencia, y=value)) +
    geom_line(aes(col = Avaliado,group=Avaliado)) + geom_point(aes(col=Avaliado)) + coord_polar()+ geom_hline(yintercept = 3) + ylim(0,5) +
    theme_excel_new() + labs(x='Nota',y='') + scale_fill_manual(values = c( "red", "blue",'white')) + facet_wrap(~variable) 
  
  Grafico=list(AutoAvaliacao,Gestor,Outros,Media)
  names(Grafico)=c('AutoAvaliacao','Gestor','Outros','Media')
  
  return(Grafico)
}




Gerar_Visual_Comparativo<-function(df,Colaborador){
  require(reshape2)
  require(ggplot2)
  require(ggthemes)
  Grafico=df %>% 
    filter(Avaliado==Colaborador) %>% melt() %>%
    ggplot(aes(x=Competencia, y=value)) +
    geom_line(aes(col = variable,group=variable)) + geom_point(aes(col=variable)) + coord_polar()+ geom_hline(yintercept = 3) + ylim(0,5) +
    theme_excel_new() + labs(x='Nota',y='') 
  return(Grafico)
}



Gerar_Matriz_Termo_Documento<-function(df,Colaborador){
  require(tm) 
  require(tidyr)
  Avaliacoes=df %>% 
    filter(Avaliado==Colaborador) %>%
    select(starts_with('Avaliador')) %>% gather() %>%
    select(Avaliador=value) %>% mutate(Avaliador=removeNumbers(Avaliador),
                                       Avaliador=tolower(Avaliador),
                                       Avaliador=removeWords(Avaliador,stopwords('pt')),
                                       Avaliador=removePunctuation(Avaliador) )
  Matriz=TermDocumentMatrix(Corpus(VectorSource(Avaliacoes$Avaliador)) )
  return(Matriz)
  
}


Gerar_WordCloud<-function(Matriz){
  require(wordcloud)
  Frequencia=rowSums(as.matrix(Matriz) )
  return(Frequencia)
  #return(wordcloud(names(Frequencia),as.numeric(Frequencia)) )

}

Gerar_Grafo<-function(Matriz,Corte){
  
  Adjacencia=as.matrix(t(Matriz)) %*% as.matrix(Matriz)
  
  Lista=melt(1*(Adjacencia>=Corte)) 
  
  names(Lista)=c('Origem','Destino','Valor')
  Lista=Lista %>% filter(Valor>0,Origem!=Destino)
  
  require(igraph)
  Grafo=graph_from_data_frame(Lista)
  return(Grafo)
}