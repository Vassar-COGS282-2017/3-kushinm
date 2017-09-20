# use this file to generate your variation on the schelling model

rows <- 70 
cols <- 70
proportion.group.1 <- .2 
empty <- .30
min.similarity <- 7/8 
min.difference<- 1/8

area.grid <- function(rows, cols){
data<-c(rep(6,(rows/2)*(cols/2)),
        rep(7,(rows/2)*(cols/2)),
        rep(8,(rows/2)*(cols/2)),
        rep(9,(rows/2)*(cols/2)))
area<- matrix(data,nrow=rows, ncol= cols)
}


create.grid <- function(rows, cols, proportion.group.1, empty){
  pop.size.group.1 <- (rows*cols)*(1-empty)*proportion.group.1
  pop.size.group.2 <- (rows*cols)*(1-empty)*(1-proportion.group.1)
  
  initial.population <- sample(c(
    rep(1, pop.size.group.1), 
    rep(2, pop.size.group.2), 
    rep(0, (rows*cols)-pop.size.group.1-pop.size.group.2)
  ))
  grid <- matrix(initial.population, nrow=rows, ncol=cols)
}

visualize.grid <- function(grid){
  image(grid, col=c('black','red','blue'), xaxs=NULL, yaxs=NULL, xaxt='n', yaxt='n')
}

empty.locations <- function(grid){
  return(which(grid==0, arr.ind=T))
}



similarity.to.center <- function(grid.subset, center.val){
  if(center.val == 0){ return(NA) }
  same <- sum(grid.subset==center.val) - 1
  not.same <- sum(grid.subset!=center.val) - sum(grid.subset==0)
  return(same/(same+not.same))
}


segregation <- function(grid){
  same.count <- 0
  diff.count <- 0
  for(row in 1:(nrow(grid)-1)){
    for(col in 1:(ncol(grid)-1)){
      if(grid[row,col] != 0 && grid[row+1,col+1] != 0){
        if(grid[row,col] != grid[row+1,col+1]){
          diff.count <- diff.count + 1
        } else {
          same.count <- same.count + 1
        }
      }
      if(grid[row,col] != 0 && grid[row,col+1] != 0){
        if(grid[row,col] != grid[row,col+1]){
          diff.count <- diff.count + 1
        } else {
          same.count <- same.count + 1
        }
      }
      if(grid[row,col] != 0 && grid[row+1,col] != 0){
        if(grid[row,col] != grid[row+1,col]){
          diff.count <- diff.count + 1
        } else {
          same.count <- same.count + 1
        }
      }  
    }
  }
  return(same.count / (same.count + diff.count))
}


diff.from.center<- function(grid.subset, center.val){
  if(center.val==0){
    return(NA)}
  diff<- sum(grid.subset!=center.val)-sum(grid.subset==0)
  same<- sum(grid.subset==center.val)-1
  return(diff/(diff+same))
}

unhappy.agents <- function(grid, min.similarity, min.difference,locale){
 
  grid.copy <- grid
  for(row in 1:rows){
    for(col in 1:cols){
      stay<- 3
      similarity.score <- similarity.to.center(grid[max(0, row-1):min(rows,row+1), max(0,col-1):min(cols,col+1)], grid[row,col])
      difference.score <- diff.from.center(grid[max(0, row-1):min(rows,row+1), max(0,col-1):min(cols,col+1)], grid[row,col])
      if(is.na(similarity.score)){
        grid.copy[row,col] <- NA
      }
if(is.na(similarity.score)==F){
stay<- stay + (min.similarity-similarity.score) + (min.difference - difference.score)} #between 2 and -2 depending on parameters(?)
if(grid[rows,cols]!=0){
  if(grid[rows,cols]==1&&locale[rows,cols]==7){
    stay<- stay+2
  }
  if(grid[rows,cols]==2&&locale[rows,cols]==7){
    stay<- stay-2
  }
  if(grid[rows,cols]==1&&locale[rows,cols]==8){
    stay<- stay-2
  }
  if(grid[rows,cols]==2&&locale[rows,cols]==8){
    stay<- stay+2
  }
  if(grid[rows,cols]==1&&locale[rows,cols]==9){
    stay<- stay+1
  }
  if(grid[rows,cols]==2&&locale[rows,cols]==9){
    stay<- stay-1
  }
  if(grid[rows,cols]==1&&locale[rows,cols]==6){
    stay<- stay-1
  }
  if(grid[rows,cols]==2&&locale[rows,cols]==6){
    stay<- stay+1
  }

}
if(stay<=3){
grid.copy[rows,cols]=F
}else{
grid.copy[rows,cols]=T
}
  return(which(grid.copy==FALSE, arr.ind = T))
}}}

one.round <- function(grid, min.similarity){
  empty.spaces<- empty.locations(grid)
  unhappy<- unhappy.agents(grid, min.similarity,min.difference,locale)
  empty.spaces<- empty.spaces[ sample(1:nrow(empty.spaces)),  ]   
  for(i in 1:nrow(empty.spaces)){
    if(i>nrow(unhappy)){break;}
    grid[empty.spaces[i,1], empty.spaces[i,2]]<- grid[unhappy[i,1], unhappy[i,2]]
    grid[unhappy[i,1], unhappy[i,2]]<- 0
  }
}

done <- FALSE 
grid <- create.grid(rows, cols, proportion.group.1, empty)
locale <- area.grid(rows, cols)
seg.tracker <- c(segregation(grid)) 
while(!done){
  new.grid <- one.round(grid, min.similarity)
  seg.tracker <- c(seg.tracker, segregation(grid)) 
  if(all(new.grid == grid)){ 
    done <- TRUE 
  } else {
    grid <- new.grid 
  }
}
layout(1) 
visualize.grid(grid) 
plot(seg.tracker)