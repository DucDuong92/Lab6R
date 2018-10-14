#' @title
#' brute_force_knapsack
#' @description brute_force_knapsack function to solve the knapsack problem
#' @name
#' brute_force_knapsack
#'
#' @param x , is the input weight
#' @param W , is the input value
#'
#'
#' @return list , returns a list that contain the maximum value and the list of elements chosen
#' @export

brute_force_knapsack <- function(x, W)
{
  value <- 0
  elements <- c()
  for (i in 1:length(x$v)) {
    #scan the input value, combine all posible cases
    combine_v<-as.data.frame(combn(x[,1], i))
    combine_w<-as.data.frame(combn(x[,2], i))
    sumw<-colSums(combine_w)
    sumv<-colSums(combine_v)

    #take the cases that have w <= input W
    components <- which(sumw<=W)

    #remove the empty case
    if(length(components) != 0){

      #take the max value of this case, assign to value_temp
      values <- sumv[components]
      value_temp <- max(values)

      #compare with the global max value
      if (value_temp > value) {
        value <- value_temp
        index <- which(sumv == value)
        weight <- w[,index]
        elements <- which(x$w == weight)
      }
    }
  }
  output <- list("value" = value, "elements" = elements)
  return(output)
}

#brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
