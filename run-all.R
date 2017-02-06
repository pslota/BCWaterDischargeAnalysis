# Run all of the test cases


library(plyr)

# Get the directories
dir.list <- dir()
dir.list <- dir.list[ (grepl("^08", dir.list) | grepl("^10", dir.list)) ]
dir.list

savewd <- getwd()
# now for each directory, change the wd and delve deep down
l_ply(dir.list, function(dir){
   savewd <- getwd()
   setwd(file.path(savewd, dir, "MinFlows"))
   cat(rep("*********************************************\n",10),"Now in ", getwd(), "\n")
   source(file.path("annual.stat.R"), echo=TRUE)
   setwd(savewd)
})
setwd(savewd)