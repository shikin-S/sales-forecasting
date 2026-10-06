compare_models <- function(results) {
  best <- select_best_model(results)
  list(models = results, best_model = best)
}