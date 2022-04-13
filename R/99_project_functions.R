# Define project functions ------------------------------------------------

# Used in 02_clean.R
normalize<-function(x){
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

knn<-function(x, k){
  sorted_x <- sort(x, index.return=TRUE)
  as_tibble(t(c(sorted_x$x[2:(k+1)], 
                sorted_x$ix[2:(k+1)])))
}


