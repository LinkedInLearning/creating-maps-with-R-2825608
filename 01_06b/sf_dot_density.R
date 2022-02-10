sf_dot_density <-
  function(x,
           group_var,
           scale = 100,
           parallel = FALSE) {
    group_var <- enquo(group_var)
    
    group_var_values <- x %>%
      pull(!!group_var) %>%
      unique()
    
    if (parallel == FALSE) {
      st_dots_list <- map(group_var_values, ~ {
        grouped_x <- x %>%
          select(!!group_var, value) %>%
          filter(!!group_var == .x) %>%
          filter(!is.na(value)) %>%
          mutate(value = round(value / scale),
                 value > 0)
        
        sampled_x <- suppressMessages(st_sample(grouped_x,
                                                size = grouped_x$value))
        
        sampled_x %>%
          st_sf() %>%
          mutate(!!group_var := .x)
      })
      
      st_dots_list %>%
        bind_rows()
      
    }
    
    if (parallel == TRUE) {
      st_dots_list <- map(group_var_values, ~ future({
        grouped_x <- x %>%
          select(!!group_var, value) %>%
          filter(!!group_var == .x) %>%
          filter(!is.na(value)) %>%
          mutate(value = round(value / scale),
                 value > 0)
        
        sampled_x <- suppressMessages(st_sample(grouped_x,
                                                size = grouped_x$value))
        
        sampled_x %>%
          st_sf() %>%
          mutate(!!group_var := .x)
      }, seed = TRUE))
      
      map(st_dots_list, ~ value(.x)) %>%
        bind_rows()
      
    }
    
    
    
  }
