
common_names <-
    function(x, y) {
        if(is.data.frame(x) == T & is.data.frame(y)){
            Reduce(intersect, list(names(x), names(y)))
        } else {
            print('both arguments must be data.frames')
        }
    }


strip_n <- function(x = a.data_frame,y = NUMBER.to.filter,z = y){
  require(tidyverse,quietly = T)
  
  x %>% 
    map_df(~n_distinct(.)) %>% 
    gather(key,value) %>% 
    filter(value >= y) %>% 
    filter(value <= z) %>% 
    pull(key) 
  
  
}

strip_percent_na <- function(x = a.data_frame,y = DECIMAL.to.filter){
  require(tidyverse,quietly = T)  

  x %>% 
    map_df(~sum(is.na(.))) %>% 
    gather(key,value) %>% 
    mutate(value = value/(nrow(.))*100) %>% 
    filter(value <= y) %>% 
    pull(key) 
}



