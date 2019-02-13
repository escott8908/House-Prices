map(dumimpdata2, range) %>% 
  map_lgl(function(i){i[[1]] == 0 & i[[2]] == 1})
