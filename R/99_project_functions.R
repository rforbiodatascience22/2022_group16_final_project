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
    mutate(dist_rank = 1:k) %>% 
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
    pivot_longer(c(-y_variable,
                   -.cluster), 
                 names_to = "variable",
                 values_to = "value")
   
  p1 <- ggplot(data = augmented_gathered, 
               mapping = aes_string(x = "value",
                                    y = y_variable)) +
    geom_point(mapping = aes(color = .cluster),
               alpha = 0.8) +
    facet_wrap(~variable) +
    theme(axis.text.x = element_text(size = 8),
          legend.position="bottom") +
    labs(x = "Value",
         y = str_to_title(y_variable))
    
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
  x_max = max(rotation_matrix$PC1)+0.2
  x_min = min(rotation_matrix$PC1)-0.2
  y_max = max(rotation_matrix$PC2)+0.2
  y_min = min(rotation_matrix$PC2)-0.2
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