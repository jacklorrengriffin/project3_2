#' Random Forest Cross Validation
#'
#' Predicting a variable using covariates with random forest cross validation
#'
#' @param k number of folds
#' @keywords prediction
#'
#' @return a numeric with the cross validation error
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  my_penguins <- na.omit(my_penguins)
  fold <- sample(rep(1:k, length = nrow(my_penguins)))
  fold_mse <- vector(mode = "numeric", length = k)

  for(i in 1:k) {
    train <- which(fold != i)
    test <- which(fold == i)
    train_data <- my_penguins[train,]
    test_data <- my_penguins[test,]
    model <- randomForest::randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, data = train_data, ntree = 100)
    predictions <- predict(model, test_data)
    fold_mse[i] <- mean((predictions - test_data$body_mass_g)^2)

  }
    return(mean(fold_mse))
}
