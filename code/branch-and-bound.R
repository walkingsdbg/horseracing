library(lpSolve)
rm(list = ls())

EmptyNode <- function(){
  Node <- data.frame(matrix(rep(NA, 4), nrow=1))[numeric(0), ]
  colnames(Node) <- c("level", "profit","ubound","weight")
  return(Node)
}

UpperBound <- function(x,W,Items){
  n <- nrow(Items)
  #calculate the upper bound by solving lp relaxation problem
  if(x$weight > W){
    ubound <- 0
  }else{
    if(x$level == n){
      ubound <- x$profit
    }else{
      f.obj  <- Items$value[(x$level+1):n]
      f.con  <- matrix(Items$weight[(x$level+1):n], nrow=1, byrow=TRUE)
      f.dir  <- "<="
      f.rhs  <- W-x$weight
      lpslt  <- lp("max", f.obj, f.con, f.dir, f.rhs) 
      ubound <- lpslt$objval + x$profit
    }
  }

  return(ubound)
}

Items <- data.frame(weight=c(2,3.14,1.98,5,3), value=c(40,50,100,95,30))
Items <- Items[order(Items$value/Items$weight,decreasing = T),] #order the items according to value per weight
W <- 10
Q <- EmptyNode() #stack
u <- data.frame(level=0, profit=0,  weight=0,  ubound=NA)  #node
v <- data.frame(level=NA, profit=NA, weight=NA, ubound=NA) #branch of the node u
w <- data.frame(level=NA, profit=NA, weight=NA, ubound=NA) #branch of the node u

Q <- rbind(Q,u) #push
MaxProfit <- 0
n <- nrow(Items)
iter <- 0

while(nrow(Q) > 0){
  iter <- iter + 1
  
  #pop
  u <- Q[nrow(Q),]
  Q <- Q[-nrow(Q),]
  print(u)
  
  if(u$level == 0){
    v$level <- 0
    w$level <- 0
  }
  
  if(u$level == n){
    next
  }else{#depth-first search
    
    #not taking the item
    v$level  <- u$level + 1
    v$weight <- u$weight
    v$profit <- u$profit

    #taking the item
    w$level  <- u$level + 1
    w$weight <- u$weight + Items$weight[w$level]
    w$profit <- u$profit + Items$value[w$level]

    #bounding
    v$ubound <- UpperBound(v,W,Items)
    w$ubound <- UpperBound(w,W,Items)
    if(w$weight <= W && w$profit > MaxProfit){
      MaxProfit <- w$profit
    }
    
    #pruning
    if(v$ubound > MaxProfit){
      Q <- rbind(Q,v)#push
    }
    if(w$ubound > MaxProfit){
      Q <- rbind(Q,w)#push
    }
    
  }
  

}

