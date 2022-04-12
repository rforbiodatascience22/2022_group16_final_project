# Define project functions ------------------------------------------------

# Used in 02_clean.R
normalize<-function(x){
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
