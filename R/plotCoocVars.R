plotCooCVars <- function(Corpus, Variable= "Variable", Variante= "Variante",facet= TRUE, umap.config= umap.defaults){
  if(length(Variante)>1){
    Corpus %>% tidyr::unite("Variante",Variante, sep="_")
  }
  if(!"IPId" %in% names(Corpus)){
    countPerIP <- Corpus %>% filter(!is.na(.data[[Variable]])) %>%
      group_by(IpNumber,.data[[Variable]], .data[[Variante]]) %>% count() %>% arrange(n) %>%
      mutate(Variante= str_c(.data[[Variable]],"_",.data[[Variante]] )) %>% ungroup() %>%
      dplyr::select(-.data[[Variable]]) %>%  pivot_wider(names_from = Variante, values_from = n) %>%
      mutate(across(everything(), .fns = ~replace_na(.,0))) %>% dplyr::select(-IpNumber) %>% as.matrix() %>%  Matrix()
  }else{
    countPerIP <- Corpus %>% filter(!is.na(.data[[Variable]])) %>%
      group_by(IPId,.data[[Variable]], .data[[Variante]]) %>% count() %>% arrange(n) %>%
      mutate(Variante= str_c(.data[[Variable]],"_",.data[[Variante]] )) %>% ungroup() %>%
      dplyr::select(-.data[[Variable]]) %>%  pivot_wider(names_from = Variante, values_from = n) %>%
      mutate(across(everything(), .fns = ~replace_na(.,0))) %>% dplyr::select(-IPId) %>% as.matrix() %>%  Matrix()
  }
  CooCMat <- t(countPerIP) %*% countPerIP %>% Matrix()
  umap.cooc <- CooCMat %>% as.matrix() %>% umap(config = umap.config)
  data <- umap.cooc$layout %>% as.data.frame() %>% mutate(Var= rownames(.)) %>% separate(Var, into = c("Variable", "Variante"), sep="_")
  if(facet==TRUE){
    g <- data %>% ggplot(aes(x= V1, y= V2, label=rownames(.))) + geom_point()+ geom_text() +
      facet_wrap(vars(Variable))
    plot(g)
  }else{
    vars <- Corpus %>% filter(!is.na(.data[[Variable]]))%>% pull(.data[[Variable]]) %>% unique()
    for (k in vars) {
      g <- data %>% filter(Variable==k) %>% ggplot(aes(x= V1, y= V2, label=rownames(.))) + geom_point()+
        geom_text()
      plot(g)
    }

  }


}
