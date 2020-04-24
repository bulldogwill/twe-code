
#' A function for k-means clustering
#'
#' \code{kmeans} runs kmeans algorithm on data
#'
#' @param data A matrix of data.
#' @param k The number of clusters.
#' @param inits A matrix of initial values for the clusters. If not provided will 
#'             be randomly chosen. Number of columns should match data.
#' @param tol Stopping tolerance.
#' @param iter Maximum number of iterations.
#' @export

kmeans <- function(data, k, inits, tol, iter){
  #tests for errors and such here....check ncols of data and inits match
  
  #things that need to be held outside loop
  n <- dim(data)[1] #get number of observations
  p <- dim(data)[2] #get number of columns
  cluster <- rep(NA, times = n) #vector to hold clusters
  data <- data.frame(cbind(data,cluster)) #add the couster column to data
  means <- inits #matrix of centroids...should be k rows and p cols
  distances <- matrix(NA, nrow = n, ncol = k) #matrix holds distances from each centroid
  means_old <- inits #matrix that holds the old centroids; used to calculate stopping tolerance
  toli <- tol #sets toli equal to tol to start, ensures at least 1 iteration
  count <- 1 #counter
  #check that every class has at least one observation? or suggest fewer clusters
  
  
  
  #while
  while(((tol <= toli) & count <= iter)){
    print(count)
    means_old <- means 
    #get distances
    for (i in 1:n) {
      for(j in 1:k){
        distances[i, j] <- norm(data[i,1:p]-means[j,], "2") #for each observation computes distance to each cluster
      }
    }
    #assign points to clusters based on closest centroid
    for (i in 1:n){
      data$cluster[i] <- which.min(distances[i, ])
    }
    
    #get new centroids
    for(i in 1:k){
      means[i,] <- colMeans(data[data$cluster==i,1:p])

    }
    
    toli <- sum(apply(X = means_old - means, MARGIN = 1, FUN = norm, "2")) #sums up the distance for each oldmean to new mean
    count <- count+1

  }
print(toli)

return(data)
}


x <- rnorm(100)
y <- rnorm(100)

data <- data.frame(x,y)
inits <- matrix(c(1,1,2,2), nrow=2,ncol=2)

kmeans.out1 <- kmeans(data, k=2, inits, tol=.01, iter = 100)

test <- as.data.frame(cbind(x,y))

plot <- ggplot(data, aes(x=x,y=y, color=as.factor(data$cluster))) + geom_point()





#using iris 2 d
iris_small <- iris[ , 3:4]
inits <- matrix(c(3,2,4,1,7,.1), nrow = 3,ncol=2, byrow = TRUE)


kmeans.out <- kmeans(iris_small, 3, inits, tol = .0001, iter = 100)

plot <- ggplot(kmeans.out, aes(x=Petal.Length,y=Petal.Width, color=as.factor(kmeans.out$cluster))) + geom_point()+scale_color_manual(values=c("red", "purple", "green"))
                                                                                            
plot

