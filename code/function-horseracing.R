library(lpSolve)
library(dplyr)
# rm(list = ls())

EmptyNode <- function(){
  Node <- data.frame(matrix(rep(NA, 4), nrow=1))[numeric(0), ]
  colnames(Node) <- c("level", "profit","ubound","weight")
  return(Node)
}

EmptyItem <- function(Items){
  Node <- data.frame(matrix(rep(NA, ncol(Items)), nrow=1))[numeric(0), ]
  colnames(Node) <- colnames(Items) #c("weight", "value")
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
  UnitW  <- min(Items$weight)
  MaxW   <- max(Items$weight)
  nperID <- floor(MaxW/UnitW)
  IdxRes <- (x$node$level+1):n
  IDres  <- unique(Items$ID[IdxRes])
  IDres  <- setdiff(IDres,intersect(IDres,unique(x$item$ID)))
  #calculate the upper bound by solving lp relaxation problem
  if (nrow(x$item) == 0){
    Pnow <- 1
    Pres <- 1
  }else{
    Pnow  <- x$item %>% dplyr::filter(weight >= sum(weight)/odds) %>% dplyr::select(prob) %>% sum()
    if(length(IDres)==0 || is.na(IDres)){
      Pres  <- 0
    }else{
      Pres  <- Items  %>% dplyr::filter(ID %in% IDres) %>% dplyr::distinct(ID,.keep_all = TRUE) %>% dplyr::select(prob) %>% sum()
    }
  }
  
  
  if( (x$node$weight > W) || any(duplicated(x$item$ID)) || (Pres < 1-alpha-Pnow) ){
    ubound <- -10000
  }else{
    # IDres  <- unique(Items$ID[IdxRes])
    # IDres  <- setdiff(IDres,intersect(IDres,unique(x$item$ID)))
    if(x$node$level == n || length(IDres) == 0){
      ubound <- x$node$profit
    }else{
      #coef of objective function
      K <- length(IDres)
      f.obj  <- c(Items[Items$ID %in% IDres,"value"],numeric(K))  
      #coef of conditions
      coef_weight <- c(rep(seq(UnitW,MaxW,by=UnitW),length=nperID*K),numeric(K))#c(rep(1:MaxW,length=MaxW*K),numeric(K))
      coef_unit   <- cbind(matrix(numeric(nperID*K*K), nrow=K),diag(K))
      for (i in 1:K){
        coef_unit[i,(1:nperID)+(i-1)*nperID] <- rep(1,length=nperID)
      }
      Probres <- Items %>%
        dplyr::distinct(ID, .keep_all = TRUE) %>%
        dplyr::filter(ID %in% IDres)
      coef_pLoss <- c(numeric(nperID*K),Probres$prob) #c(numeric(nperID*K),1-Probres$prob)
      f.con  <- rbind(coef_weight,coef_unit,coef_pLoss) #matrix(Items$weight[IdxRes], nrow=1, byrow=TRUE)
      #rhs of conditions
      Probbet <- x$item %>%
        dplyr::distinct(ID, .keep_all = TRUE)
      f.rhs  <- c(W-x$node$weight,rep(1,length=K),alpha + sum(Probbet$prob) -1 + sum(Probres$prob)) #c(W-x$node$weight,rep(1,length=K),alpha + sum(1-Probbet$prob) -1 + sum(1-Probres$prob))
      #solve lp relaxation problem
      f.dir  <- c("<=",rep("=",length=K),"<=")
      lpslt  <- lp("max", f.obj, f.con, f.dir, f.rhs) 
      ubound <- lpslt$objval + x$node$profit
      # browser()
    }
  }

  return(ubound)
}

InitializeMaxProfit <- function(Items,W,alpha){
  UnitW  <- min(Items$weight)
  Kmax   <- length(unique(Items$ID))
  Kmin   <- 1
  Ptotal <- 0
  Dprob  <- sort(Items[!duplicated(Items$ID),"prob"],decreasing = T)
  while (Ptotal < 1-alpha){
    Kmin   <- Kmin + 1
    Ptotal <- sum(Dprob[1:Kmin])
  }

  MaxProfit <- 0
  OptItem   <- EmptyItem(Items)
  for(k in Kmin:Kmax){
    CombList <- combn(unique(Items$ID),k)
    for (l in 1:ncol(CombList)){
      Dcomb  <- Items %>% dplyr::filter(ID %in% CombList[,l] & weight == UnitW)
      Wlist  <- ceiling((W/Dcomb$odds) / UnitW) * UnitW
      if(sum(Dcomb$prob) >= 1-alpha && sum(Wlist) <= W){
        Profit <- sum(Dcomb$value * (Wlist/UnitW))
        if(Profit > MaxProfit){
           # browser()
           MaxProfit <- Profit
           OptItem   <- EmptyItem(Items)
           for (m in 1:nrow(Dcomb)){
             OptItem <- rbind(OptItem,Items %>% dplyr::filter(ID == Dcomb$ID[m] & weight == Wlist[m]))
           }
        }
      }
    }
  }

  return(list(MaxProfit = MaxProfit, OptItem = OptItem))
}

# ConditionCheck(x,MaxProfit,W,alpha){
#   flag <- (x$node$profit > MaxProfit) && (x$node$weight <= W) && (ProbLoss(x$item) <= alpha) && !any(duplicated(x$item$ID))
#   return(flag)
# }


####test####
# Items <- data.frame(weight=c(1,1), value=c(2.4,4)-1, prob=c(0.6,0.4), odds=c(4,10), ID = c(1,2))
# W <- 100 #total weight
# alpha <- 0.2
# Items <- lapply(Items,rep,length=W*nrow(Items)) %>% as.data.frame()
# 
# Items2 <- data.frame(matrix(rep(NA, ncol(Items)), nrow=1))[numeric(0), ]
# # colnames(df) <- colnames(Items)
# for (i in 1:max(Items$ID)){
#   ItemsTemp <- Items %>% dplyr::filter(ID == i)
#   for (j in 2:nrow(ItemsTemp)){
#     ItemsTemp$weight[j] <- j*ItemsTemp$weight[1]
#     ItemsTemp$value[j] <- j*ItemsTemp$value[1]
#   }
#   Items2 <- rbind(Items2,ItemsTemp)
# }


BranchAndBound <- function(Items,W,alpha){
  #If you want to improve the performance of Branch and Bound,
  #you should change the functions of UpperBound() and ConditionCheck().
  
  Items <- Items[order(Items$value/Items$weight,decreasing = T),] #order the items according to value per weight
  # Items <- rbind(Items %>% 
  #                dplyr::group_by(ID) %>% 
  #                dplyr::filter(weight >= W/odds) %>% #select the items ROI of which > 1
  #                dplyr::ungroup(ID) %>% 
  #                dplyr::arrange(desc(value/weight)), 
  #                Items %>% 
  #                dplyr::group_by(ID) %>% 
  #                dplyr::filter(weight < W/odds) %>% #select the items ROI of which <= 1
  #                dplyr::ungroup(ID) %>% 
  #                dplyr::arrange(desc(value/weight))) %>%
  #          as.data.frame()
  Q <- as.list(NULL) #stack
  u <- list(data.frame(level=0, profit=0, weight=0, ubound=NA),EmptyItem(Items)) #node
  names(u) <- c("node","item")
  v <- list(data.frame(level=NA,profit=NA,weight=NA,ubound=NA),EmptyItem(Items)) #branch of the node u
  names(v) <- c("node","item")
  w <- list(data.frame(level=NA,profit=NA,weight=NA,ubound=NA),EmptyItem(Items)) #branch of the node u
  names(w) <- c("node","item")
  
  #push
  Q <- append(Q,list(u))
  
  InitProfit <- InitializeMaxProfit(Items,W,alpha)
  MaxProfit  <- InitProfit$MaxProfit 
  OptItem    <- InitProfit$OptItem
  print(MaxProfit)
  print(OptItem)
  
  n         <- nrow(Items)
  iter      <- 0
  BreakFlag <- FALSE
  
  
  while(length(Q) > 0){
    iter <- iter + 1
    
    #pop
    u <- Q[[length(Q)]]
    Q[[length(Q)]] <- NULL
    
    # print(u$item)
    
    if(u$node$level == 0){
      v$node$level <- 0
      w$node$level <- 0
    }
    ItemRes <- Items %>% dplyr::mutate(rownumber = 1:n) %>% dplyr::filter(rownumber > u$node$level & !(ID %in% u$item$ID))
    if(nrow(ItemRes) == 0){ #(u$node$level == n){
      next
    }else{#depth-first search
      NextLevel <- ItemRes %>% dplyr::select(rownumber) %>% min()
      ####branching####
      #not taking the item
      v$node$level  <- NextLevel #u$node$level + 1
      v$item        <- u$item
      v$node$weight <- u$node$weight
      v$node$profit <- u$node$profit
      #taking the item
      w$node$level  <- NextLevel #u$node$level + 1
      w$item        <- rbind(u$item,Items[w$node$level,])
      w$node$weight <- u$node$weight + Items$weight[w$node$level]
      w$node$profit <- u$node$profit + Items$value[w$node$level]
  
      ####bounding####
      v$node$ubound <- UpperBound(v,W,alpha,Items)
      w$node$ubound <- UpperBound(w,W,alpha,Items)
      # print(MaxProfit)
      # print(v$node$ubound)
      # print(w$node$ubound)
      if((w$node$profit >= MaxProfit) && (w$node$weight <= W) && (ProbLoss(w$item) <= alpha) && !any(duplicated(w$item$ID))){
        MaxProfit <- w$node$profit
        OptItem   <- w$item
      }
      
      if(v$node$ubound >= MaxProfit){
        Q <- append(Q,list(v)) #push
      }
      if(w$node$ubound >= MaxProfit){
        Q <- append(Q,list(w)) #push
      }
    }
    
    if(iter >= 10^5){
      BreakFlag <- TRUE
      break;
    }
  }
  
  # browser()
  if(BreakFlag == TRUE){
    MaxUbound <- 0
    for (i in 1:length(Q)){
      Ubound <- Q[[i]]$node$ubound 
      if (Ubound > MaxUbound){
        MaxUbound <- Ubound
      }
    }
  }else{
    MaxUbound <- MaxProfit
  }

  
  
  return(list(OptItem = OptItem, MaxProfit = MaxProfit, MaxUpperBound = MaxUbound, Iter = iter))
}


