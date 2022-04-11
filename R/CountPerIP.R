CountPerIP <- function(exb, Variable="V", Variante="Variante", format="dataframe"){
  CountPerIP <- exb %>% dplyr::rename(Variable={{Variable}}) %>% dplyr::rename(Variante={{Variante}}) %>% dplyr::filter(!is.na(Variable)) %>%   dpylr::group_by(IpNumber,Variable, Variante) %>% dplyr::count() %>% dplyr::mutate(Variante= str_c(Variable,"_",Variante )) %>% dplyr::ungroup() %>% dplyr::select(-Variable) %>%  tidyr::pivot_wider(names_from = Variante, values_from = n) %>% dplyr::mutate(dplyr::across(everything(), .fns = ~tidyr::replace_na(.,0))) #%>% dplyr::select(-IpNumber)
  if(format== "dataframe"){
  return(CountPerIP)
  }else if(format=="matrix"){
    return(CountPerIP  %>%  dplyr::select(-IpNumber) %>%  base::as.matrix())
  }
}
