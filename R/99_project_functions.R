# Define project functions ------------------------------------------------

# Used in 02_clean.R
normalize<-function(x){
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}


knn <- function(x, y, var, k){
  as_tibble(as.matrix(pdist(x, 
                            y))) %>% 
    rename_all(~data %>%
                 filter(!is.na(.data[[var]])) %>% 
                 pull(seqn) %>% 
                 as.character) %>% 
    mutate(data %>%
             filter_all(any_vars(is.na(.))) %>% 
             select(seqn)) %>% 
    remove_rownames() %>% 
    column_to_rownames(var = "seqn") %>% 
    mutate(seqn_w_NA = rownames(.)) %>% 
    pivot_longer(cols = -seqn_w_NA, 
                 names_to = 'ID2') %>% 
    group_by(seqn_w_NA) %>% 
    arrange(value) %>% 
    slice(2:(k+1)) %>% 
    mutate(dist_rank = 1:5) %>% 
    select(seqn_w_NA, 
           ID2, 
           dist_rank) %>% 
    pivot_wider(names_from = dist_rank,
                names_prefix = "KNN_",
                values_from = ID2) %>% 
    ungroup() %>% 
    pivot_longer(cols = contains("KNN"),
                 names_to = "KNN",
                 values_to = "value") %>% 
    group_by(seqn_w_NA) %>% 
    summarise(KNN = list(as.numeric(value))) %>% 
    mutate_at(vars(seqn_w_NA),
              as.double) %>% 
    left_join(x = data, 
              y = ., 
              by = c("seqn" = "seqn_w_NA"))
}

indexing_function <- function(x, y, var){
  
  seqns <- x %>% 
    select(seqn) %>% 
    pull
  
  indeces <- (y %>% 
                filter(seqn == seqns))$KNN %>% 
    unlist(., use.names=FALSE)
  
  y %>%
    filter(seqn %in% indeces) %>% 
    summarise(mean = mean(.data[[var]])) %>% 
    pull
}