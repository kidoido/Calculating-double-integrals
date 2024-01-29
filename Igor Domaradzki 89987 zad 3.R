# Igor Domaradzki 89987

# Zadanie 3 
integrate3d <- function(f, over) {
  search_for_x <- seq(over$x[1], over$x[2], length.out = 120)
  search_for_y <- seq(over$y[1], over$y[2], length.out = 120)
  
  suma_calki <- 0
  for (xx in 1:(length(search_for_x)-1)){
    for (yy in 1:(length(search_for_y)-1)) {
      smaller_x <- search_for_x[xx]
      bigger_x <- search_for_x[xx+1]
      smaller_y <- search_for_y[yy]
      bigger_y <- search_for_y[yy+1]
      avg_x <- mean(smaller_x,bigger_x)
      avg_y <- mean(smaller_y,bigger_y)
      z <- f(avg_x, avg_y)
      obj_prostokata <- (bigger_y - smaller_y)*(bigger_x - smaller_x)*z
      suma_calki <- suma_calki + obj_prostokata
    }
  }
  return(suma_calki)
} 
  
#Przykłady:
integrate3d(
  f = function(x, y) {cos(x) * y},
  over = list(x = c(0, pi / 2), y = c(0, 1)))

integrate3d(f = function(x, y) {x^2 *y^2}, over = list(x = c(0, 2), y = c(0, 2)))

integrate3d(f = function(x, y) {(-(x-1)^2+1) * (-(y-1)^2+1)}, over = list(x = c(0, 2), y = c(0, 2)))

