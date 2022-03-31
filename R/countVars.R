#' countVars Counting Variables in a EXB file
#'
#' @param Corpus
#' @param Variable
#' @param Variante
#'
#' @return
#' @export
#'
#' @examples
countVars <- function(Corpus, Variable= "Variable", Variante= "Variante"){
  if(length(Variante)>1){
    Counts <- Corpus %>% filter(!is.na(V)) %>%  tidyr::unite_("Variante",Variante, sep="_") %>% group_by_(Variable, Variante) %>% count() %>% arrange(n) %>% `names<-`(c("Variable","Variante","n"))
  }else{
  Counts <- Corpus %>% filter(!is.na(V)) %>% group_by_(Variable, Variante) %>% count() %>% arrange(n) %>% `names<-`(c("Variable","Variante","n"))
  }
  return(Counts)
}
