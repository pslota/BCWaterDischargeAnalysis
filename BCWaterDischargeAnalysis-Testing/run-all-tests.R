# Copyright 2017 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


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
