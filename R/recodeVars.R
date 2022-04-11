recodeVars <- function(Corpus, VarCol="V",VariantCol="Variante", Variable, oldVariants, newVariants){
  oldVariants <- stringr::str_flatten(oldVariants, collapse = "|")
  Corpus %>% mutate(VariantNew= dplyr::case_when({{VarCol}}=={{Variable}} & stringr::str_detect({{VariantCol}}, oldVariants)~ newVariants, TRUE ~ pull(Corpus,{{VariantCol}})))
}
