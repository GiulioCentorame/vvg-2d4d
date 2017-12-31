#modifications by John Christie
#03/10/2011
# The variable "slices" in each function is there so that one can adjust the quality of the measure of the area under the curve.  It's one of the main reasons that this code will give different results from online calculators.  It can also be lowered in order to overcome any possible performance issues (unwise in the cross_2â€¦ functions).    Everything should be fast enough as is.

Bf<-function(sd, obtained, uniform = FALSE, lower=0, upper=1, meanoftheory=0, sdtheory=1, tails=2)
{
  #Version 2.0.1
  #modifications by John Christie
  # modification means that this does not exactly replicate Dienes.  What it does
  # is do the right thing instead.  :)  The current version is more accurate.
  #03/10/2011
  #Authors Danny Kaye & Thom Baguley
  #Version 1.0
  #19/10/2009
  # test data can be found starting at p100
  # notes on how to use these functions can be found at
  # http://danny-kaye.co.uk/Docs/Dienes_notes.pdf
  #raised from 2000 for better accuracy - the speed of the new code allows it
  slices <- 20000
  if(uniform){
    range <- upper - lower
    dist_theta <- 1 / range
    # incr <- range / slices
    # theta <- seq(lower + incr, by = incr, length.out = slices+1)
    # height <- dist_theta * dnorm(obtained, theta, sd)
    # area <- sum(height * incr)
    # the commented code above replicates the original result but at the
    # limit (slices <- 5e6) it's actually equivalent to the followingâ€¦
    area <- dist_theta * diff(pnorm(c(lower, upper), obtained, sd)) 
  }else{
    # the code below again doesnt' replicate the original code.
    # incrementing in a scalar loop was causing an accumulation of tiny fp errors
    # the lower end was incremented prior to starting (that's fixed above too)
    zlim <- 5
    incr <- sdtheory / (slices/(zlim*2))
    newLower <- meanoftheory - zlim * sdtheory
    theta <- seq(newLower, by = incr, length.out = slices+1)
    dist_theta <- dnorm(theta, meanoftheory, sdtheory)
    if (tails == 1){
      dist_theta <- dist_theta[theta > 0]	* 2
      theta <- theta[theta > 0]	
    }
    height <- dist_theta * dnorm(obtained, theta, sd)
    area <- sum(height * incr)
  }
  LikelihoodTheory <- area
  Likelihoodnull <- dnorm(obtained, 0, sd)
  BayesFactor <- LikelihoodTheory / Likelihoodnull
  return( list("LikelihoodTheory" = LikelihoodTheory, "Likelihoodnull" = Likelihoodnull, "BayesFactor" = BayesFactor) )
}


contrast<-function(SEd, meand, nu, theta1, theta2)
{
  #modifications by John Christie
  #01/10/2011
  #Authors Danny Kaye & Thom Baguley
  #19/10/2009
  #The test data is found in Box 3.7 pp 74-5
  #SEd - contrast standard error
  #meand - contrast mean
  #nu - contrast  degrees of freedom
  #
  slices <- 10000
  zlim <- 5
  inc <- SEd / (slices/(zlim*2))
  
  theta <- seq(meand - zlim * SEd, by = inc, length.out = slices+1)
  likelihood <- (1 + (meand - theta)^2 / (nu * SEd^2))^(-(nu + 1) / 2)
  likelihoodmax <- max(likelihood)
  
  likelihood <- likelihood / likelihoodmax
  
  which32 <- which(likelihood >= 1/32)
  which8 <- which(likelihood >= 1/8)
  begin32 <- theta[which32[1]]
  begin8 <- theta[which8[1]]
  end8 <- theta[tail(which8,1)+1]
  end32 <- theta[tail(which32,1)+1]
  
  B1 <- round((theta1 - theta[1]) / inc + 1, 0)
  B2 <- round((theta2 - theta[1]) / inc + 1, 0)
  likelihoodratio <- likelihood[B1] / likelihood[B2]
  return( list("begin8"=begin8, "end8"=end8, "begin32"=begin32, "end32"=end32, "likelihoodratio"=likelihoodratio) )
}

meanUV<-function(SEd, meand, n, theta1, theta2)
{
  #modifications by John Christie
  #01/10/2011
  #Authors Danny Kaye & Thom Baguley
  #Version 1.0
  #19/10/2009
  #The test data is from page 138
  #SEd - sample standard error
  #meand - sample mean
  #n - number of subjects
  #theta1 - population mean assumed by first hypothesis
  #theta2 - population mean assumed by second hypothesis
  #
  Vard <- n * SEd^2
  SSd <- Vard * (n - 1)
  
  slices <- 10000
  zlim <- 5
  inc <- SEd / (slices/(zlim*2))
  
  theta <- seq(meand - zlim * SEd, by = inc, length.out = slices+1)
  likelihood <- (SSd + n * (meand - theta)^2)^(-(n - 2) / 2)
  likelihoodmax <- max(likelihood)
  
  likelihood <-likelihood / likelihoodmax
  
  #unlike the original I find a chunk that's the middle of the distribution
  which32 <- which(likelihood >= 1/32)
  which8 <- which(likelihood >= 1/8)
  begin32 <- theta[which32[1]]
  begin8 <- theta[which8[1]]
  end8 <- theta[ tail(which8, 1) + 1 ]
  end32 <- theta[ tail(which32, 1) + 1 ]
  
  B1 <- round((theta1 - theta[1]) / inc + 1, 0)
  B2 <- round((theta2 - theta[1]) / inc + 1, 0)
  likelihoodratio <- likelihood[B1] / likelihood[B2]
  return( list("begin8"=begin8, "end8"=end8, "begin32"=begin32, "end32"=end32, "likelihoodratio"=likelihoodratio) )
}


cross2x2_odds <- function(a, b, c, d, psi1, psi2)
{
  #modifications by John Christie
  #01/10/2011
  #Authors Danny Kaye & Thom Baguley
  #Version 1.0
  #19/10/2009
  #The test data is from figure 5.4 p 135
  #
  m <- a + b
  n <- c + d
  k <- a + c
  l <- b + d
  
  slices <- 10000
  psi <- (1:slices) / sqrt(slices)
  
  starting <- 0
  if((a - d) > 0) starting <- a - d
  ending <- m
  if((a + c) < m) ending <- a + c
  
  A <- starting:ending
  likelihood <- sapply( 1:slices, function(B) {
    sum( choose(m, A) * choose(n, (a + c - A)) * psi[B]^(A - a))})
  
  likelihood <- likelihood^-1
  likelihoodmax <- max(likelihood)
  psimax <- psi[which.max(likelihood)]
  
  likelihood <- likelihood / likelihoodmax
  
  which32 <- which(likelihood >= 1/32)
  which8 <- which(likelihood >= 1/8)
  begin32 <- psi[which32[1]]
  begin8 <- psi[which8[1]]
  end8 <- psi[ tail(which8, 1) + 1 ]
  end32 <- psi[ tail(which32, 1) + 1 ]
  
  psi1 <- psi1 * 100
  psi2 <- psi2 * 100
  likelihoodratio <- likelihood[psi1] / likelihood[psi2]
  return( list("psimax"=psimax, "begin8"=begin8, "end8"=end8, "begin32"=begin32, "end32"=end32, "likelihoodratio"=likelihoodratio) )
}

cross2x22<-function(a, b, c, d, gamma1, gamma2)
{
  #modifications by John Christie
  #01/10/2011
  #Authors Danny Kaye & Thom Baguley
  #Version 1.0
  #19/10/2009
  #The test data is from figure 5.4 p 135
  #
  m <- a + b
  n <- c + d
  k <- a + c
  l <- b + d
  
  slices <- 10000
  gammamax <- 0
  gamma <- (1:slices) / sqrt(slices)
  
  A <- b:(m + n - d)
  likelihood <- sapply( 1:slices, function(B){
    sum( choose(A - 1, b - 1) * choose(m + n - A - 1, d - 1) * gamma[B]^(A - m) ) } )
  
  likelihood <- likelihood^-1
  likelihoodmax <- max(likelihood)
  gammamax <- gamma[which.max(likelihood)]
  
  likelihood <- likelihood / likelihoodmax
  
  which32 <- which(likelihood >= 1/32)
  which8 <- which(likelihood >= 1/8)
  begin32 <- gamma[which32[1]]
  begin8 <- gamma[which8[1]]
  end8 <- gamma[ tail(which8, 1) + 1 ]
  end32 <- gamma[ tail(which32, 1) + 1 ]
  
  gamma1 <- gamma1 * 10
  gamma2 <- gamma2 * 10
  likelihoodratio <- likelihood[gamma1] / likelihood[gamma2]
  return( list("gammamax"=gammamax, "begin8"=begin8, "end8"=end8, "begin32"=begin32, "end32"=end32, "likelihoodratio"=likelihoodratio) )
}

proportion <- function(suc, fail)
{
  #modifications by John Christie
  #01/10/2011
  #Authors Danny Kaye & Thom Baguley
  #Version 1.0
  #19/10/2009
  #The test data is from figure 5.3 p131
  #not sure this does what he says it does, no theta input is asked for
  #in the book code, as implied in the text and no ratio is returned
  #note that the function is called with successes and failures not 
  #successes and trials
  #
  slices <- 10000
  theta <- (1:slices)/slices
  
  likelihood <- theta^suc*(1-theta)^fail
  likelihoodmax <- max(likelihood)
  thetamax <- theta[which.max(likelihood)]
  
  likelihood <- likelihood / likelihoodmax
  
  #unlike the original I find a chunk that's the middle of the distribution
  which32 <- which(likelihood >= 1/32)
  which8 <- which(likelihood >= 1/8)
  begin32 <- theta[which32[1]]
  begin8 <- theta[which8[1]]
  end8 <- theta[ tail(which8, 1) + 1 ]
  end32 <- theta[ tail(which32, 1) + 1 ]
  
  return( list("thetamax"=thetamax, "begin8"=begin8, "end8"=end8,
               "begin32"=begin32, "end32"=end32) )
}