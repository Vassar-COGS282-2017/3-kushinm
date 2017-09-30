# use this file to generate your variation on the schelling model

rows <- 40 
cols <- 40
proportion.group.1 <- .5 
empty <- .30
min.similarity <- 1/8 
min.difference<- 3/8

area.grid <- function(rows, cols){
  area<- matrix(nrow=rows, ncol= cols)
  for(i in 1:((rows/2))){
    for(j in 1:((cols/2))){
      area[i,j]<-6
    }
  }
  for(i in 1:((rows/2))){
    for(j in((cols/2)+1):cols){
      area[i,j]<-7
    }
  }
  for(i in ((rows/2)+1):rows){
    for(j in((cols/2)+1):cols){
      area[i,j]<-8
    }
  }
  for(i in ((rows/2)+1):rows){
    for(j in 1:((cols/2))){
      area[i,j]<-9
    }
  }
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

unhappy.agents <- function(grid, min.similarity, min.difference){
  grid.copy <- grid
  for(row in 1:rows){
    for(col in 1:cols){
      similarity.score <- similarity.to.center(grid[max(0, row-1):min(rows,row+1), max(0,col-1):min(cols,col+1)], grid[row,col])
      difference.score <- diff.from.center(grid[max(0, row-1):min(rows,row+1), max(0,col-1):min(cols,col+1)], grid[row,col])
      if(is.na(similarity.score)&&is.na(difference.score)){
        grid.copy[row,col] <- NA
      }else if(similarity.score>= min.similarity && difference.score>= min.difference){
        grid.copy[row,col] <-TRUE
        }
      else{grid.copy[row,col] <- FALSE
      }
    }
  }
  return(which(grid.copy==FALSE, arr.ind = T))
}

one.round <- function(grid, min.similarity, min.difference){
  empty.spaces<- empty.locations(grid)
  unhappy<- unhappy.agents(grid, min.similarity,min.difference)
  empty.spaces<- empty.spaces[ sample(1:nrow(empty.spaces)),  ]   
  for(i in 1:nrow(empty.spaces)){
    if(i>nrow(unhappy)){break;}
    grid[empty.spaces[i,1], empty.spaces[i,2]]<- grid[unhappy[i,1], unhappy[i,2]]
    grid[unhappy[i,1], unhappy[i,2]]<- 0
  }
  return(grid)
}

done <- FALSE 
grid <- create.grid(rows, cols, proportion.group.1, empty)
seg.tracker <- c(segregation(grid)) 
while(!done){
  new.grid <- one.round(grid, min.similarity,min.difference)
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

grid
new.grid
