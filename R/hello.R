# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

Test <- CorpusiClave %>% filter(V=="CH"| V=="S")
CountIpTest <- Test %>%   group_by(IpNumber,V, Variante) %>% count() %>% mutate(Variante= str_c(V,"_",Variante )) %>% ungroup() %>% dplyr::select(-V) %>%  pivot_wider(names_from = Variante, values_from = n) %>% mutate(across(everything(), .fns = ~replace_na(.,0))) %>% dplyr::select(-IpNumber) %>% ungroup() %>% slice_head(n=5) #%>% as.matrix() %>%  Matrix()
IpMat <- CountIpTest %>% as.matrix()
tIpMat <- t(CountIpTest)
cooctest <- tIpMat %*% IpMat
