library(Umatrix)
data('Hepta')

ir <- 
  iris %>% 
  select(-Species) %>% 
  as.matrix()

res <- esomTrain(ir, Key = 1:nrow(ir))
# res <- esomTrain(Hepta$Data, Key = 1:nrow(Hepta$Data))
res
Umatrix::plotMatrix(res$Umatrix, res$BestMatches, TransparentContours = TRUE)



map <- Umatrix::esomTrain(as.matrix(distances), Key = seq_along(trees),
                          Epochs = 5, # Increase for better results
                          Lines = 42,
                          Columns = 42,
                          Toroid = FALSE)

Umatrix::plotMatrix(Matrix = map$Umatrix,
                    Toroid = FALSE, FixedRatio = TRUE,
                    TransparentContours = FALSE, Clean = TRUE) +
  ggplot2::geom_point(data = data.frame(x = map$BestMatches[, 3],
                                        y = map$BestMatches[, 2]),
                      shape = 19, color = treeCols, size = 2)