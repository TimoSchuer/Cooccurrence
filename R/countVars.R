countVars <- function(Corpus, Variable= "Variable", Variante= "Variante"){
  if(length(Variante)>1){
    Corpus %>% tidyr::unite("Variante",Variante, sep="_")
  }
  Counts <- Corpus %>% filter(!is.na(V)) %>% group_by_(Variable, Variante) %>% count() %>% arrange(n)
  return(Counts)
}
