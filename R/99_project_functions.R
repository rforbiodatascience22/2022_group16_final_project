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



#K-means clustering plot
KMC_plot <- function(y_variable) {
  
  augmented_gathered <- augmented %>%
    gather(key = "variable", value = "value",
           -y_variable, -.cluster)   #key and value is names of the columns, y_variable is kept as a column, and the same for clusters. (from wide to long)
  
  p1 <- ggplot(augmented_gathered, aes_string(x = "value", y = y_variable)) +
    geom_point(aes(color = .cluster), alpha = 0.8) +
    facet_wrap(~variable)
  p1
  return(p1)
}



#Make labels for axes, when presenting in PC directions
label_PCs <- function(pca_eigen_matrix, PC_number){
  PC1_var = round(pca_matrix_eigen$percent[PC_number]*100, 2)
  
  PC_label = str_c(cbind("Principal Component ", 
                         PC_number, 
                         " (", 
                         PC1_var, 
                         " %)"), 
                   collapse = "")
  return(PC_label)
}


##Find highest and lowest values in PC1 and PC2 of 
#rotation matrix for plot window design
rot_plot_axes <- function(rotation_matrix){
  x_max = max(rotation_matrix$PC1)+0.1
  x_min = min(rotation_matrix$PC1)-0.1
  y_max = max(rotation_matrix$PC2)+0.1
  y_min = min(rotation_matrix$PC2)-0.1
  axes_boundaries <- c(x_min, x_max, y_min, y_max)
  
  return(axes_boundaries)
}

#Select subset of data for different PCA analyses
select_data_subset <- function(data, exclude){
  pca_prep_data <- data %>% 
    select(where(is.numeric) &  #only keep numeric data
             !all_of(exclude))
  
  return(pca_prep_data)
}