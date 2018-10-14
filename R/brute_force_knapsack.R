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
  #check the input
  if(is.data.frame(x)==FALSE)
    stop("x must be a data frame")
  if(is.numeric(W)==FALSE)
    stop("W should be a number")
  if (W <=0)
    stop("W should bigger than 0")

  #++++++++++++++++++++++++++++++++
    value <- 0
    elements <- c()
    for (i in 1:length(x$v)) {
      #scan the input value, combine all posible cases
      combine_w<-as.data.frame(combn(x[,1], i))
      combine_v<-as.data.frame(combn(x[,2], i))
      sumw<-colSums(combine_w)
      sumv<-colSums(combine_v)

      #take the cases that have w <= input W
      components <- which(sumw<=W)
      #remove the empty case
      if(length(components) != 0){
        #print(components)

        #take the max value of this case, assign to value_temp
        values <- sumv[components]
        value_temp <- max(values)

        #compare with the global max value
        if (value_temp > value) {
          value <- value_temp
          index <- which(sumv == value)
          weight <- combine_w[,index]
          elements <- which(x$w == weight)
        }
      }
    }
    output <- list("value" = round(value), "elements" = elements)
    return(output)
  }

#brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
