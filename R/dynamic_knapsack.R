#' @title
#' dynamic_knapsack
#' @description dynamic_knapsack function to solve the knapsack problem
#' @name
#' dynamic_knapsack
#'
#' @param x , is the input weight
#' @param W , is the input value
#'
#'
#' @return list , returns a list that contain the maximum value and the list of elements chosen
#' @export

dynamic_knapsack <- function(x, W)
{
  #Check input
  if(is.data.frame(x)==FALSE)
    stop("x must be a data frame")
  if(is.numeric(W)==FALSE)
    stop("W should be a number")
  if (W <=0)
    stop("W should bigger than 0")

  #+++++++++++++++++++++

  W <- W+1
  n <- length(x$v) +1
  w <- c(0, x$w)
  v <- c(0, x$v)
  m <- matrix(0, nrow = n, ncol = W)
  points<-matrix(0,nrow = n,ncol = W)
  elements<-c()

  #Calculate value for all weight as the following rule:
  #  If the weight (j) < weight of that element: value of that elements equal value of the element in lower row
  #  Else take the value of the last change point push it's value
  for (j in 2:W) {
    for (i in 2:n) {

      if(w[i] > j)
        {m[i,j] <- m[i-1,j]
        }
      else {
        value <-max(m[(i-1),j],(m[(i-1),(j-w[i])] + v[i]))
        m[i, j]=value

        #Set 1 as the change point in points matrix
        if (value == m[i-1, j]) {
          points[i,j]<- 0
        } else {
          points[i,j]<- 1
        }

      }
    }
  }
  sum <- m[n,W]
  # Calculat the elements base on change point matrix
  # Scan the last row, take the index of elements
  #  Then, minus it's value and continue to the end
  Cross_point<-n
  Step_point<-W
  for (k in 1:Cross_point) {
     if (points[Cross_point,Step_point] == 1) {
       Cross_point <- Cross_point -1
       elements <- c(elements, Cross_point)
       Step_point <- Step_point - x$w[Cross_point]
     } else Cross_point <- Cross_point -1
  }

  elements <- sort(elements)
  output <- output <- list(value = round(sum), elements = elements)
  return(output)
}

#dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)

