# use this file to generate your variation on the schelling model

#2 main variations here- 1. I set up a difference criterion as well as a similarity criterion
#its not really a minimum value, but more of a preference value for both
#The second major change is I create a copy of the grid and divide it into 4 sectors
#Agents become "unhappy" based on a "Stay" score. If the stay score falls below a certain level they 
#become unhappy. The stay score is affected by similarity and difference preferences as well as 
#how the agent feels about the sector its in. 
#The model always seems to just go through 2 to 3 iterations though, and the changes in segregation
#are small. This could be because the criteria for wanting to move are too high, and also because
#this is not an accurate model of how people decide to move, however introducing greater complexities
#such as locale preference and similarity and difference preferences appears to be a good step
#in making the model better reflect reality.

rows <- 40 
cols <- 40
proportion.group.1 <- .5
empty <- .40
min.similarity <- 3/8 #now its no longer really min.similarity, but more of similarity preference
min.difference<- 2/8 #same here for difference, its not a threshold and more of a preference value

area.grid <- function(rows, cols){ #creates a grid which defines "locales" where different agents have 
  #different preferences when it comes to staying or moving
  data<-c(rep(6,(rows/2)*(cols/2)),
          rep(7,(rows/2)*(cols/2)),
          rep(8,(rows/2)*(cols/2)),
          rep(9,(rows/2)*(cols/2)))
  area<- matrix(data,nrow=rows, ncol= cols,byrow = T)
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
      similarity.score <- similarity.to.center(grid[max(0, row-1):min(rows,row+1), max(0,col-1):min(cols,col+1)], grid[row,col])
      difference.score <- diff.from.center(grid[max(0, row-1):min(rows,row+1), max(0,col-1):min(cols,col+1)], grid[row,col])
      if(grid.copy[row,col]==0){#ignores blank spaces
        grid.copy[row,col] <- NA
      }
      if(is.na(similarity.score)==F){
        stay<-0.3 #Base stay rate; this determines whether an agent wants to move or stay
        stay<- stay - abs((min.similarity-similarity.score)) - abs((min.difference - difference.score)) #between 2 and -2 depending on parameters(?)
        #the line above finds out how much the current situation of any agent deviates from their ideal preference
        #This is subtracted from the stay variable making them more likely to move
        
        #The following segment checks for the type of agent and the locale they are in and assigns a change
        #in the stay variable accordingly. In some locales agent type 1 is very comfortable and type 2 is very
        #uncomfortable. The opposite happens in another locale. And then there are two more locales where one
        #type is mildly comfortable and the other is mildly uncomfortable. This is reflected by the amount
        #subtracted or added to the stay variable.
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
        
        if(stay<=0.3){
          grid.copy[rows,cols]=F
        }else{
          grid.copy[rows,cols]=T
        }}

    }}      
  return(which(grid.copy==FALSE, arr.ind = T))
  }

one.round <- function(grid, min.similarity,min.difference,locale){
  empty.spaces<- empty.locations(grid)
  unhappy<- unhappy.agents(grid, min.similarity,min.difference,locale)
  empty.spaces<- empty.spaces[ sample(1:nrow(empty.spaces)),  ]   
  for(i in 1:nrow(empty.spaces)){
    if(i>nrow(unhappy)){
      break}
    grid[empty.spaces[i,1], empty.spaces[i,2]]<- grid[unhappy[i,1], unhappy[i,2]]
    grid[unhappy[i,1], unhappy[i,2]]<- 0
  }
  return(grid)
}

done <- FALSE 
grid <- create.grid(rows, cols, proportion.group.1, empty)
locale <- area.grid(rows, cols)
seg.tracker <- c(segregation(grid)) 
while(!done){
  new.grid <- one.round(grid, min.similarity,min.difference,locale)
  seg.tracker <- c(seg.tracker, segregation(grid))
  if(all(new.grid == grid)){ 
    done <- TRUE 
  } else {
    grid <-new.grid
  } 
} 
layout(1) 
visualize.grid(grid) 
plot(seg.tracker)