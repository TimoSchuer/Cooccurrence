#' Title
#'
#' @param Corpus
#' @param Variable
#' @param Variante
#' @param threshold
#' @param umap.config
#'
#' @return
#' @export
#'
#' @examples
plotCooC <- function(Corpus, Variable= "Variable", Variante="Variante", threshold= 10, umap.config=umap.defaults){
  if(length(Variante)>1){
    Corpus %>% tidyr::unite("Variante",Variante, sep="_")
  }
  Counts <- Corpus %>% filter(!is.na(V)) %>% group_by_(Variable, Variante) %>% count() %>% arrange(n)
  if(!"IPId" %in% names(Corpus)){
    countPerIP <-Corpus %>% left_join(Counts, by=c(Variable, Variante))  %>% filter(!is.na(.data[[Variable]])) %>%
      filter(n>= threshold) %>%   group_by(IpNumber,.data[[Variable]], .data[[Variante]]) %>% count() %>% arrange(n) %>%
      mutate(Variante= str_c(.data[[Variable]],"_",.data[[Variante]] )) %>% ungroup() %>%
      dplyr::select(-.data[[Variable]]) %>%  pivot_wider(names_from = Variante, values_from = n) %>%
      mutate(across(everything(), .fns = ~replace_na(.,0))) %>% dplyr::select(-IpNumber) %>% as.matrix() %>%  Matrix()
  }else{

  countPerIP <-Corpus %>% left_join(Counts, by=c(Variable, Variante))  %>% filter(!is.na(.data[[Variable]])) %>%
    filter(n>= threshold) %>%   group_by(IPId,.data[[Variable]], .data[[Variante]]) %>% count() %>% arrange(n) %>%
    mutate(Variante= str_c(.data[[Variable]],"_",.data[[Variante]] )) %>% ungroup() %>%
    dplyr::select(-.data[[Variable]]) %>%  pivot_wider(names_from = Variante, values_from = n) %>%
    mutate(across(everything(), .fns = ~replace_na(.,0))) %>% dplyr::select(-IPId) %>% as.matrix() %>%  Matrix()
  }
  CooCMat <- t(countPerIP) %*% countPerIP %>% Matrix()
  umap.cooc <- CooCMat %>% as.matrix() %>% umap(config= umap.config)
  data <- umap.cooc$layout %>% as.data.frame() %>% mutate(Var= rownames(.)) %>%
    separate(Var, into = c("Variable", "Variante"), sep="_")
  g <- ggplot(data, aes(x=V1, y= V2, label= rownames(data)), shape= factor(Variable))+
    geom_point(aes(colour= Variable))
  return(g)
}
