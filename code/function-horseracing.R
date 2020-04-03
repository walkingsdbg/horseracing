library(lpSolve)
library(dplyr)
# rm(list = ls())

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

ProbWin <- function(horseID,horseIDs,ms_rank){
  idx  <- which(horseIDs == horseID)
  pWin <- sum(ms_rank[,idx]==1)/nrow(ms_rank)
  return(pWin)
}

ProbLoss <- function(Items){
  #calculate probability of ROI < 1 (only used for Win ticket)
  #must add to Items the types of betting tickets to extend this function
  
  pProfit <- Items %>% 
    # dplyr::group_by(ID) %>%
    # dplyr::mutate(r = n()/nrow(Items)*odds) %>%
    # dplyr::ungroup() %>%
    # dplyr::distinct(ID, .keep_all = TRUE) %>%
    dplyr::mutate(r = weight/sum(weight)*odds) %>%
    dplyr::filter(r >= 1) %>%
    dplyr::select(prob) %>%
    sum()
  
  pLoss <- 1-pProfit
  
  return(pLoss)
}

UpperBound <- function(x,W,alpha,Items){
  n      <- nrow(Items)
  IdxRes <- (x$node$level+1):n
  #calculate the upper bound by solving lp relaxation problem
  if( (x$node$weight > W) || any(duplicated(x$item$ID)) ){
    ubound <- 0
  }else{
    IDres  <- unique(Items$ID[IdxRes])
    IDres  <- setdiff(IDres,intersect(IDres,unique(x$item$ID)))
    if(x$node$level == n || length(IDres) == 0){
      ubound <- x$node$profit
    }else{
      #coef of objective function
      K <- length(IDres)
      f.obj  <- c(Items[Items$ID %in% IDres,"value"],numeric(K))  #Items$value[IdxRes]
      #coef of conditions
      coef_weight <- c(rep(1:W,length=W*K),numeric(K))
      coef_unit   <- cbind(matrix(numeric(W*K*K), nrow=K),diag(K))
      for (i in 1:K){
        coef_unit[i,(1:W)+(i-1)*W] <- rep(1,length=W)
      }
      Probres <- Items %>%
        dplyr::distinct(ID, .keep_all = TRUE) %>%
        dplyr::filter(ID %in% IDres)
      coef_pLoss <- c(numeric(W*K),1-Probres$prob)
      f.con  <- rbind(coef_weight,coef_unit,coef_pLoss) #matrix(Items$weight[IdxRes], nrow=1, byrow=TRUE)
      #rhs of conditions
      Probbet <- x$item %>%
        dplyr::distinct(ID, .keep_all = TRUE)
      f.rhs  <- c(W-x$node$weight,rep(1,length=K),alpha + sum(Probbet$prob) -1 + sum(Probres$prob))
      #solve lp relaxation problem
      f.dir  <- c("<=",rep("=",length=K),"<=")
      lpslt  <- lp("max", f.obj, f.con, f.dir, f.rhs) 
      ubound <- lpslt$objval + x$node$profit
    }
  }

  return(ubound)
}

# ConditionCheck(x,MaxProfit,W,alpha){
#   flag <- (x$node$profit > MaxProfit) && (x$node$weight <= W) && (ProbLoss(x$item) <= alpha) && !any(duplicated(x$item$ID))
#   return(flag)
# }


####test####
Items <- data.frame(weight=c(1,1), value=c(2.4,4)-1, prob=c(0.6,0.4), odds=c(4,10), ID = c(1,2))
W <- 100 #total weight
alpha <- 0.2
Items <- lapply(Items,rep,length=W*nrow(Items)) %>% as.data.frame()

Items2 <- data.frame(matrix(rep(NA, ncol(Items)), nrow=1))[numeric(0), ]
# colnames(df) <- colnames(Items)
for (i in 1:max(Items$ID)){
  ItemsTemp <- Items %>% dplyr::filter(ID == i)
  for (j in 2:nrow(ItemsTemp)){
    ItemsTemp$weight[j] <- j*ItemsTemp$weight[1]
    ItemsTemp$value[j] <- j*ItemsTemp$value[1]
  }
  Items2 <- rbind(Items2,ItemsTemp)
}
# x <- list(data.frame(level=NA,profit=NA,weight=NA,ubound=NA),EmptyItem()) #branch of the node u
# names(x) <- c("node","item")
# x$item <- Items2[3:4,]
###

BranchAndBound <- function(Items,W,alpha){
  #If you want to improve the performance of Branch and Bound,
  #you should change the functions of UpperBound() and ConditionCheck().
  
  Items <- Items[order(Items$value/Items$weight,decreasing = T),] #order the items according to value per weight
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
    # print(iter)
    
    #pop
    u <- Q[[length(Q)]]
    Q[[length(Q)]] <- NULL
    
    # print(u$item)
    
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
      v$node$ubound <- UpperBound(v,W,alpha,Items)
      w$node$ubound <- UpperBound(w,W,alpha,Items)
      if((w$node$profit > MaxProfit) && (w$node$weight <= W) && (ProbLoss(w$item) <= alpha) && !any(duplicated(w$item$ID))){
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
  return(list(OptItem = OptItem, MaxProfit = MaxProfit, Iter = iter))
}


