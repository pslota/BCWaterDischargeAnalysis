# Define a scale 
norm_rev_trans <<- function(){
         scales::trans_new(name="norm_rev", 
                           function(x){qnorm(1-x)},  # forward transformation
                           function(x){1-pnorm(x)})}
