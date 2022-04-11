#' Calculate coccurrence matrix
#'
#' @param exb
#' @param relative
#' @param format
#' @param Variable
#' @param Variante
#'
#' @return
#' @export
#'
#' @examples
calcCooc <- function(exb, relative= TRUE, format= "data.frame", Variable= "V", Variante= "Variante"){
  counts <- CountPerIP(exb, Variable=Variable, Variante=Variante, format= "matrix")
  CoocAbs <- t(as.matrix(counts)) %*% as.matrix(counts)
  if(relative==FALSE){
    if(format=="matrix"){
      return(CoocAbs)
    }else{
      return(CoocAbs %>% as.matrix() %>% as.data.frame())
    }
  }else{
    CoocMatRel <- CoocAbs %>%  as.matrix() %>% as.data.frame() %>% mutate(Vars= rownames(.), .before=1) %>% `row.names<-`(NULL)
    for (k in names(CoocMatRel)[-1] %>% str_extract_all("[^_]*_") %>% unlist() %>% unique()) {
      CoocMatRel <- CoocMatRel %>% select(starts_with(k)) %>% rowSums() %>% mutate(CoocMatRel,rowSum= .)
      CoocMatRel <-  CoocMatRel %>% mutate(across(starts_with(k),~.x/rowSum ))
      CoocMatRel <- CoocMatRel %>% select(-rowSum)
    }
    CoocMatRel <- CoocMatRel%>% mutate(across(everything(), .fns = ~replace_na(.,0)))
    if(format=="data.frame"){
      return(CoocMatRel)
    }else{
      return(CoocMatRel %>% `rownames<-`(CoocMatRel$Vars) %>% select(-Vars) %>% as.matrix())
    }


  }
}
