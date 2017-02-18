# Define the Pearson log III distribution functions and moment function
# These are needed for fitdistrplus to fit the distribuiton.

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

.onAttach <- function(libname, pkgname){
   packageStartupMessage("Defining functions for Pearson Log III distributions\n")
   dPIII <<-function(x, shape, location, scale) PearsonDS::dpearsonIII(x, shape, location, scale, log=FALSE)
   pPIII <<-function(q, shape, location, scale) PearsonDS::ppearsonIII(q, shape, location, scale, lower.tail = TRUE, log.p = FALSE)
   qPIII <<-function(p, shape, location, scale) PearsonDS::qpearsonIII(p, shape, location, scale, lower.tail = TRUE, log.p = FALSE)

   mPIII <<-function(order, shape, location, scale){
      # compute the empirical first 3 moments of the PIII distribution
      if(order==1) return( location + shape*scale)
      if(order==2) return(scale*scale*shape)
      if(order==3) return(2/sqrt(shape)*sign(scale))
   }
}
