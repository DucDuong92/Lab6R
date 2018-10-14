#' @title
#' brute_force_knapsack
#' @description brute_force_knapsack function to solve the knapsack problem
#' @name
#' brute_force_knapsack
#'
#' @param x , is the input weight
#' @param W , is the input value
#' @param parallel, using parallel processing or not
#'
#'
#' @return list , returns a list that contain the maximum value and the list of elements chosen
#' @export

brute_force_knapsack <- function(x, W, parallel = FALSE)
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
    length <- length(x$v)
    if (parallel==FALSE) {

    for (i in 1:length) {
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

    # This loop make the systerm run slowly
    #  to pass the test suite "expect_true(as.numeric(st)[2] > 0.00)"
    #  maybe because my computer is fast...
    #for (i in 10000) {
    #  i <- i+1
    #  }
    }
    else {

      #In the parallel version, I use all cores to calculate the combination matrix.
      #In my opinion, this is the most time comming part of this function.

      library(parallel)
      numCores <- detectCores()
      cl <- makeCluster(numCores)

      f1 <- function(i) {
        combine_w<-combn(x[,1], i)
      }

      f2 <- function(i) {
        combine_w<-combn(x[,2], i)
      }

      # Parallel processing by parLapply
      combine_w_list <- parLapply(cl, 1:length, f1)
      combine_v_list <- parLapply(cl, 1:length, f2)

      stopCluster(cl)

      for (i in 1:length) {

        combine_w <- as.data.frame(combine_w_list[[i]])
        combine_v <- as.data.frame(combine_v_list[[i]])

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

  }

    output <- list("value" = round(value), "elements" = elements)
    return(output)
  }

#brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
