library(lpSolve)
rm(list = ls())

EmptyNode <- function(){
  Node <- data.frame(matrix(rep(NA, 4), nrow=1))[numeric(0), ]
  colnames(Node) <- c("level", "profit","ubound","weight")
  return(Node)
}

EmptyItem <- function(){
  Node <- data.frame(matrix(rep(NA, 2), nrow=1))[numeric(0), ]
  colnames(Node) <- c("weight", "value")
  return(Node)
}

UpperBound <- function(x,W,Items){
  n <- nrow(Items)
  #calculate the upper bound by solving lp relaxation problem
  if(x$node$weight > W){
    ubound <- 0
  }else{
    if(x$node$level == n){
      ubound <- x$node$profit
    }else{
      f.obj  <- Items$value[(x$node$level+1):n]
      f.con  <- matrix(Items$weight[(x$node$level+1):n], nrow=1, byrow=TRUE)
      f.dir  <- "<="
      f.rhs  <- W-x$node$weight
      lpslt  <- lp("max", f.obj, f.con, f.dir, f.rhs) 
      ubound <- lpslt$objval + x$node$profit
    }
  }

  return(ubound)
}

Items <- data.frame(weight=c(2,3.14,1.98,5,3), value=c(40,50,100,95,30))
Items <- Items[order(Items$value/Items$weight,decreasing = T),] #order the items according to value per weight
W <- 10 #total weight
Q <- as.list(NULL) #stack
u <- list(data.frame(level=0, profit=0, weight=0, ubound=NA),EmptyItem()) #node
names(u) <- c("node","item")
v <- list(data.frame(level=NA,profit=NA,weight=NA,ubound=NA),EmptyItem()) #branch of the node u
names(v) <- c("node","item")
w <- list(data.frame(level=NA,profit=NA,weight=NA,ubound=NA),EmptyItem()) #branch of the node u
names(w) <- c("node","item")

#push
Q <- append(Q,list(u))

MaxProfit <- 0
OptItem   <- EmptyItem()
n         <- nrow(Items)
iter      <- 0

while(length(Q) > 0){
  iter <- iter + 1
  
  #pop
  u <- Q[[length(Q)]]
  Q[[length(Q)]] <- NULL
  
  print(u$item)
  
  if(u$node$level == 0){
    v$node$level <- 0
    w$node$level <- 0
  }
  
  if(u$node$level == n){
    next
  }else{#depth-first search
    
    ##branching
    #not taking the item
    v$node$level  <- u$node$level + 1
    v$item        <- u$item
    v$node$weight <- u$node$weight
    v$node$profit <- u$node$profit
    #taking the item
    w$node$level  <- u$node$level + 1
    w$item        <- rbind(u$item,Items[w$node$level,])
    w$node$weight <- u$node$weight + Items$weight[w$node$level]
    w$node$profit <- u$node$profit + Items$value[w$node$level]

    ##bounding
    v$node$ubound <- UpperBound(v,W,Items)
    w$node$ubound <- UpperBound(w,W,Items)
    if(w$node$weight <= W && w$node$profit > MaxProfit){
      MaxProfit <- w$node$profit
      OptItem   <- w$item
    }
    
    ##pruning
    if(v$node$ubound > MaxProfit){
      Q <- append(Q,list(v)) #push
    }
    if(w$node$ubound > MaxProfit){
      Q <- append(Q,list(w)) #push
    }
    
  }
  

}

