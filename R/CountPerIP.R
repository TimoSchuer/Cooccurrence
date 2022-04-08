CountPerIP <- function(exb, Variable="V", Variante="Variante", format="dataframe"){
  CountPerIP <- exb %>% rename(Variable={{Variable}}) %>% rename(Variante={{Variante}}) %>% dplyr::filter(!is.na(Variable)) %>%   group_by(IpNumber,Variable, Variante) %>% count() %>% mutate(Variante= str_c(Variable,"_",Variante )) %>% ungroup() %>% dplyr::select(-Variable) %>%  pivot_wider(names_from = Variante, values_from = n) %>% mutate(across(everything(), .fns = ~replace_na(.,0))) #%>% dplyr::select(-IpNumber)
  if(format== "dataframe"){
  return(CountPerIP)
  }else if(format=="matrix"){
    return(CountPerIP  %>%  select(-IpNumber) %>%  as.matrix())
  }
}
