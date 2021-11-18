#####Exercise09
rm(list=ls())
setwd("Biocomp notes/")

#make directory for testing function
dir.create("~/Biocomp notes/09testdir")
setwd("~/Biocomp notes/09testdir/")

###### 
#creating sample files to test function on
for(i in 1:10){
  matrix <- matrix(sample(1:100, 50), ncol=3, nrow=50)
  name=paste("file", i,".csv", sep="")
  write.table(matrix,file=name, sep=",")
}
#checking to see if it worked
list.files()

#####

#create function
coefvar <- function(dir, col){
  #setting working directory
  setwd(dir)
  #creating variable of all files in dir
  files <- list.files(pattern="*.csv")
  #creating empty vector for results
  results <- c()
  #creating for loop for files
  for(i in files){
    file <- read.table(i, sep=",")
    #checking number of columns
    if(ncol(file)<col){
      print("File doesn't have the number of columns")
      break
    }else{
      #checking for 50 observations
      if(length(file[,col])<50){
        response <- readline(prompt="Observations are less than 50, continue?(Y/N")
        if(response=="Y"){
          #checking for NA's in column
          if(sum(is.na(file[,col])>0)){
            print("selected column has NA's")
            break
          }else{
            #calculating coefficient
            coef=mean(file[,col])/sd(file[,col])
            #creating results vector
            results=c(results,coef) 
            
            
          }
          
        }else{
          break
        }
      }else{
        if(sum(is.na(file[,col])>0)){
          print("selected column has NA's")
          break
        }else{
          #calculating coefficient
          coef=mean(file[,col])/sd(file[,col])
          #creating results vector
          results=c(results,coef) 
          
        }
        
      }
    }
    }
   
  #returning results
  return(results)   
}

#testing function
coefvar(dir="~/Biocomp notes/09testdir", col=2)

#testing function
coefvar(dir="~/Biocomp notes/09testdir", col=10)





