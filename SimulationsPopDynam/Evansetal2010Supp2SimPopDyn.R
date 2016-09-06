### the code is commented throughout (following pound signs, ###)
### a path must be specified to the BUGS output files, for example:
BUGSpath <- "c:/Bayes/Dicerandra/WinBUGSCode/full-results-final"

# in that folder should be MCMC output files, named "results-1", "results-2", "results-3"

concatenate <- function(source, dest, n.row, n.col, base) {
  for (i in 1:n.row) {
    for (j in 1:n.col) {
      dest[i+base,j] <- source[i,j]
    }
  }
  dest
}

extract <- function(chain, parameter) {
  chain[,dimnames(chain)[[2]]==parameter]
}

import.MCMC <- function() {
  library(boa)

  cat("Reading MCMC output...\n")
  flush.console()
  super.1 <- boa.importBUGS("results-1", BUGSpath)
  cat("...chain 1\n")
  flush.console()
  super.2 <- boa.importBUGS("results-2", BUGSpath)
  cat("...chain 2\n")
  flush.console()
  super.3 <- boa.importBUGS("results-3", BUGSpath)
  cat("...chain 3\n")
  flush.console()

  thin <- 1          # thinned by WinBUGS before output

  # set up for concatenation
  n.samp <- length(super.1[,1])
  n.var  <- length(super.1[1,])
  super <- matrix(nrow=3*n.samp, ncol=n.var)

  # concatenate the chains into one
  super <- concatenate(super.1, super, n.samp, n.var, 0)
  super <- concatenate(super.2, super, n.samp, n.var, n.samp)
  super <- concatenate(super.3, super, n.samp, n.var, 2*n.samp)
  # set the column (variable) names
  colnames(super) <- colnames(super.1)

  # clean up a little
  rm(super.1, super.2, super.3, n.samp, n.var, thin)

  # define vectors for each input variable
  b0.br1 <- extract(super, "b0.br[1]")
  b0.br2 <- extract(super, "b0.br[2]")
  b0.br3 <- extract(super, "b0.br[3]")
  b0.g   <- extract(super, "b0.g")
  b0.sd   <- extract(super, "b0.sd")
  b0.s  <- extract(super, "b0.s")
  b1.br1 <- extract(super, "b1.br[1]")
  b1.br2 <- extract(super, "b1.br[2]")
  b1.br3 <- extract(super, "b1.br[3]")
  b1.g   <- extract(super, "b1.g")
  b1.s  <- extract(super, "b1.s")
  b2.br1 <- extract(super, "b2.br[1]")
  b2.br2 <- extract(super, "b2.br[2]")
  b2.g   <- extract(super, "b2.g")
  b2.sd   <- extract(super, "b2.sd")
  b2.s  <- extract(super, "b2.s")
  b2.tr32 <- extract(super, "b2.tr[1,1]")
  b2.tr42 <- extract(super, "b2.tr[1,2]")
  b2.tr52 <- extract(super, "b2.tr[1,3]")
  b2.tr62 <- extract(super, "b2.tr[1,4]")
  b2.tr72 <- extract(super, "b2.tr[1,5]")
  b2.tr33 <- extract(super, "b2.tr[2,1]")
  b2.tr43 <- extract(super, "b2.tr[2,2]")
  b2.tr53 <- extract(super, "b2.tr[2,3]")
  b2.tr63 <- extract(super, "b2.tr[2,4]")
  b2.tr73 <- extract(super, "b2.tr[2,5]")
  b2.tr34 <- extract(super, "b2.tr[3,1]")
  b2.tr44 <- extract(super, "b2.tr[3,2]")
  b2.tr54 <- extract(super, "b2.tr[3,3]")
  b2.tr64 <- extract(super, "b2.tr[3,4]")
  b2.tr74 <- extract(super, "b2.tr[3,5]")
  b2.tr35 <- extract(super, "b2.tr[4,1]")
  b2.tr45 <- extract(super, "b2.tr[4,2]")
  b2.tr55 <- extract(super, "b2.tr[4,3]")
  b2.tr65 <- extract(super, "b2.tr[4,4]")
  b2.tr75 <- extract(super, "b2.tr[4,5]")
  b2.tr36 <- extract(super, "b2.tr[5,1]")
  b2.tr46 <- extract(super, "b2.tr[5,2]")
  b2.tr56 <- extract(super, "b2.tr[5,3]")
  b2.tr66 <- extract(super, "b2.tr[5,4]")
  b2.tr76 <- extract(super, "b2.tr[5,5]")
  leap   <- extract(super, "b1.leap")
  pro.1  <- extract(super, "b1.pro[1]")
  pro.2  <- extract(super, "b1.pro[2]")
  pro.3  <- extract(super, "b1.pro[3]")
  pro.4  <- extract(super, "b1.pro[4]")
  prorep <- extract(super, "b1.pr")
  reg.1  <- extract(super, "b1.ret[1]")
  reg.2  <- extract(super, "b1.ret[2]")
  regnr.1 <- extract(super, "b1.rnr[1]")
  regnr.2 <- extract(super, "b1.rnr[2]")
  regnr.3 <- extract(super, "b1.rnr[3]")
  s.1    <- extract(super, "b1.s[1]")
  s.2    <- extract(super, "b1.s[2]")
  s.3    <- extract(super, "b1.s[3]")
  s.4    <- extract(super, "b1.s[4]")
  delta  <- extract(super, "delta")
  b0.32   <- extract(super, "b0.tr[1,1]")
  b0.42   <- extract(super, "b0.tr[1,2]")
  b0.52   <- extract(super, "b0.tr[1,3]")
  b0.62   <- extract(super, "b0.tr[1,4]")
  b0.33   <- extract(super, "b0.tr[2,1]")
  b0.43   <- extract(super, "b0.tr[2,2]")
  b0.53   <- extract(super, "b0.tr[2,3]")
  b0.63   <- extract(super, "b0.tr[2,4]")
  b0.34   <- extract(super, "b0.tr[3,1]")
  b0.44   <- extract(super, "b0.tr[3,2]")
  b0.54   <- extract(super, "b0.tr[3,3]")
  b0.64   <- extract(super, "b0.tr[3,4]")
  b0.35   <- extract(super, "b0.tr[4,1]")
  b0.45   <- extract(super, "b0.tr[4,2]")
  b0.55   <- extract(super, "b0.tr[4,3]")
  b0.65   <- extract(super, "b0.tr[4,4]")
  b0.36   <- extract(super, "b0.tr[5,1]")
  b0.46   <- extract(super, "b0.tr[5,2]")
  b0.56   <- extract(super, "b0.tr[5,3]")
  b0.66   <- extract(super, "b0.tr[5,4]")
  sd.year <- extract(super, "sd.year")
  sd.yr.br1 <- extract(super, "sd.yr.br[1]")
  sd.yr.br2 <- extract(super, "sd.yr.br[2]")
  sd.yr.br3 <- extract(super, "sd.yr.br[3]")
  sd.yr.g   <- extract(super, "sd.yr.g")
  sd.yr.sd  <- extract(super, "sd.yr.sd")
  sd.yr.s  <- extract(super, "sd.yr.s")
  sd.yr.tr32   <- extract(super, "sd.yr.tr[1,1]")
  sd.yr.tr42   <- extract(super, "sd.yr.tr[1,2]")
  sd.yr.tr52   <- extract(super, "sd.yr.tr[1,3]")
  sd.yr.tr62   <- extract(super, "sd.yr.tr[1,4]")
  sd.yr.tr72   <- extract(super, "sd.yr.tr[1,5]")
  sd.yr.tr33   <- extract(super, "sd.yr.tr[2,1]")
  sd.yr.tr43   <- extract(super, "sd.yr.tr[2,2]")
  sd.yr.tr53   <- extract(super, "sd.yr.tr[2,3]")
  sd.yr.tr63   <- extract(super, "sd.yr.tr[2,4]")
  sd.yr.tr73   <- extract(super, "sd.yr.tr[2,5]")
  sd.yr.tr34   <- extract(super, "sd.yr.tr[3,1]")
  sd.yr.tr44   <- extract(super, "sd.yr.tr[3,2]")
  sd.yr.tr54   <- extract(super, "sd.yr.tr[3,3]")
  sd.yr.tr64   <- extract(super, "sd.yr.tr[3,4]")
  sd.yr.tr74   <- extract(super, "sd.yr.tr[3,5]")
  sd.yr.tr35   <- extract(super, "sd.yr.tr[4,1]")
  sd.yr.tr45   <- extract(super, "sd.yr.tr[4,2]")
  sd.yr.tr55   <- extract(super, "sd.yr.tr[4,3]")
  sd.yr.tr65   <- extract(super, "sd.yr.tr[4,4]")
  sd.yr.tr75   <- extract(super, "sd.yr.tr[4,5]")
  sd.yr.tr36   <- extract(super, "sd.yr.tr[5,1]")
  sd.yr.tr46   <- extract(super, "sd.yr.tr[5,2]")
  sd.yr.tr56   <- extract(super, "sd.yr.tr[5,3]")
  sd.yr.tr66   <- extract(super, "sd.yr.tr[5,4]")
  sd.yr.tr76   <- extract(super, "sd.yr.tr[5,5]")

  temp.data <- data.frame(b0.g=b0.g, b1.g=b1.g, b2.g=b2.g,
                          b0.br1=b0.br1, b1.br1=b1.br1,
                          b0.br2=b0.br2, b1.br2=b1.br2,
                          b0.br3=b0.br3, b1.br3=b1.br3,
                          b2.br1=b2.br1, b2.br2=b2.br2,
                          b0.s=b0.s, b2.s=b2.s,
                          b0.s=b0.s, b1.s=b1.s, b2.s=b2.s,
                          b2.tr32=b2.tr32, b2.tr33=b2.tr33, b2.tr34=b2.tr34,
                          b2.tr35=b2.tr35, b2.tr36=b2.tr36,
                          b2.tr42=b2.tr42, b2.tr43=b2.tr43, b2.tr44=b2.tr44,
                          b2.tr45=b2.tr45, b2.tr46=b2.tr46,
                          b2.tr52=b2.tr52, b2.tr53=b2.tr53, b2.tr54=b2.tr54,
                          b2.tr55=b2.tr55, b2.tr56=b2.tr56,
                          b2.tr62=b2.tr62, b2.tr63=b2.tr63, b2.tr64=b2.tr64,
                          b2.tr65=b2.tr65, b2.tr66=b2.tr66,
                          b2.tr72=b2.tr72, b2.tr73=b2.tr73, b2.tr74=b2.tr74,
                          b2.tr75=b2.tr75, b2.tr76=b2.tr76,
                          leap=leap,
                          pro.1=pro.1, pro.2=pro.2, pro.3=pro.3, pro.4=pro.4,
                          prorep=prorep,
                          reg.1=reg.1, reg.2=reg.2,
                          regnr.1=regnr.1, regnr.2=regnr.2, regnr.3=regnr.3,
                          s.v=s.1, s.s=s.2, s.m=s.3, s.l=s.4,
                          delta=delta,
                          b0.32=b0.32, b0.33=b0.33, b0.34=b0.34, b0.35=b0.35,
                          b0.36=b0.36,
                          b0.42=b0.42, b0.43=b0.43, b0.44=b0.44, b0.45=b0.45,
                          b0.46=b0.46,
                          b0.52=b0.52, b0.53=b0.53, b0.54=b0.54, b0.55=b0.55,
                          b0.56=b0.56,
                          b0.62=b0.62, b0.63=b0.63, b0.64=b0.64, b0.65=b0.65,
                          b0.66=b0.66,
                          #b0.72=b0.72, b0.73=b0.73, b0.74=b0.74, b0.75=b0.75,
                          #b0.76=b0.76,
              sd.year=sd.year,
                          sd.yr.br1=sd.yr.br1, sd.yr.br2=sd.yr.br2,
                          sd.yr.br3=sd.yr.br3, sd.yr.g=sd.yr.g,
                          sd.yr.sd=sd.yr.sd, sd.yr.s=sd.yr.s,
                          sd.yr.tr32=sd.yr.tr32, sd.yr.tr33=sd.yr.tr33,
                          sd.yr.tr34=sd.yr.tr34, sd.yr.tr35=sd.yr.tr35,
                          sd.yr.tr36=sd.yr.tr36,
                          sd.yr.tr42=sd.yr.tr42, sd.yr.tr43=sd.yr.tr43,
                          sd.yr.tr44=sd.yr.tr44, sd.yr.tr45=sd.yr.tr45,
                          sd.yr.tr46=sd.yr.tr46,
                          sd.yr.tr52=sd.yr.tr52, sd.yr.tr53=sd.yr.tr53,
                          sd.yr.tr54=sd.yr.tr54, sd.yr.tr55=sd.yr.tr55,
                          sd.yr.tr56=sd.yr.tr56,
                          sd.yr.tr62=sd.yr.tr62, sd.yr.tr63=sd.yr.tr63,
                          sd.yr.tr64=sd.yr.tr64, sd.yr.tr65=sd.yr.tr65,
                          sd.yr.tr66=sd.yr.tr66,
                          sd.yr.tr72=sd.yr.tr72, sd.yr.tr73=sd.yr.tr73,
                          sd.yr.tr74=sd.yr.tr74, sd.yr.tr75=sd.yr.tr75,
                          sd.yr.tr76=sd.yr.tr76)
  temp.data
}


weib.b <- function(med, shape) {  # this gets called by fire.w (below)
  med/(log(2)^(1/shape))
}
weib.cv <- function(shape) {  # this gets called by plot.lambda (below)
  sqrt(gamma((shape+2)/shape)/(gamma((shape+1)/shape)^2) - 1)
}

fire.w <- function(n.gen, med, shape) {
# creates a TRUE/FALSE vector of fire occurence, according to a Weibull distribution
  fire <- vector(mode="logical", length=n.gen)
  b <- weib.b(med, shape)
  t <- 0
  while (t < n.gen) {
    t.next <- rweibull(1, shape, b)
    t <- t + t.next
    if (t <= n.gen) {
      fire[t] <- TRUE
    }
  }
  fire
}



construct.matrix <- function(p, tsf) {
# builds transition matrix as a function of fire and two types of year effects
# "model-wide" year effect (year) + "independent" year effects (yr)
  library(boot)
  year <- rnorm(1, 0.0, p$sd.year) # "model-wide" year effect

  # germination probability
  # tsf for g refers to *preceding* parental generation
  yr <- rnorm(1, 0.0, p$sd.yr.g) # random year effect on germ
  logit.g <- p$b0.g + p$b1.g*log(tsf) + p$b2.g*year + yr
  g <- inv.logit(logit.g)

  # use log-transformed tsf for model fits
  tsf <- log(tsf+1)

  a <- matrix(nrow=7,ncol=6)
  # note: we are building a matrix for "pre-reproductive census"
  # row 1
# old seeds that survive, don't germinate, and survive
  a[1,1] <- (exp(-(p$delta)*4))*(1-g)*(exp(-(p$delta)*8))
  yr <- rnorm(1, 0.0, p$sd.yr.sd)      # rnd year effect on seeds per branch
  seeds <- exp(p$b0.sd + p$b2.sd*year + yr)                 # no time-since-fire effect on sds/branch
  yr <- rnorm(1, 0.0, p$sd.yr.br1)    # rnd year effect on flowering branches on small flowering plants
  br.1 <- exp(p$b0.br1 + p$b1.br1*tsf + p$b2.br1*year + yr)
  yr <- rnorm(1, 0.0, p$sd.yr.br2)    # rnd year effect on flowering branches on medium flowering plants
  br.2 <- exp(p$b0.br2 + p$b1.br2*tsf + p$b2.br2*year + yr)
  yr <- rnorm(1, 0.0, p$sd.yr.br3)    # rnd year effect on flowering branches on large flowering plants
  br.3 <- exp(p$b0.br3 + p$b1.br3*tsf + year + yr) # b2.br3 = 1
# new seeds (produced by small flowering plants) that survive, don't germinate, and survive
  a[1,4] <- br.1*seeds*(exp(-(p$delta)*4))*(1-g)*(exp(-(p$delta)*8))
# new seeds (produced by small medium plants) that survive, don't germinate, and survive
  a[1,5] <- br.2*seeds*(exp(-(p$delta)*4))*(1-g)*(exp(-(p$delta)*8))
# new seeds (produced by small large plants) that survive, don't germinate, and survive
  a[1,6] <- br.3*seeds*(exp(-(p$delta)*4))*(1-g)*(exp(-(p$delta)*8))
# seedling survival
  yr <- rnorm(1, 0.0, p$sd.yr.s)
  logit.s <- p$b0.s + p$b1.s*tsf + p$b2.s*year + yr
  ss <- inv.logit(logit.s)

  # row 2
  a[2,1] <- (exp(-(p$delta)*4))*g*ss # seedlings from seed bank
# seedlings from new seeds generated by small flowering plants
  a[2,4] <- br.1*seeds*(exp(-(p$delta)*4))*g*ss
# seedlings from new seeds generated by medium flowering plants
  a[2,5] <- br.2*seeds*(exp(-(p$delta)*4))*g*ss
# seedlings from new seeds generated by large flowering plants
  a[2,6] <- br.3*seeds*(exp(-(p$delta)*4))*g*ss
  # row 3
  yr <- rnorm(1, 0.0, p$sd.yr.tr32) # random year effect on a[3,2]
  a[3,2] <- p$pro.1*tsf + p$b2.tr32*year + p$b0.32 + yr
  yr <- rnorm(1, 0.0, p$sd.yr.tr33) # random year effect on a[3,3]
  a[3,3] <- p$s.v*tsf + p$b2.tr33*year + p$b0.33 + yr
  yr <- rnorm(1, 0.0, p$sd.yr.tr34) # random year effect on a[3,4]
  a[3,4] <- p$regnr.1*tsf + p$b2.tr34*year + p$b0.34 + yr
  yr <- rnorm(1, 0.0, p$sd.yr.tr35) # random year effect on a[3,5]
  a[3,5] <- p$regnr.2*tsf + p$b2.tr35*year + p$b0.35 + yr
  yr <- rnorm(1, 0.0, p$sd.yr.tr36) # random year effect on a[3,6]
  a[3,6] <- p$regnr.3*tsf + p$b2.tr36*year + p$b0.36 + yr
  # row 4
  yr <- rnorm(1, 0.0, p$sd.yr.tr42)
  a[4,2] <- p$leap*tsf + p$pro.2*tsf + p$b2.tr42*year + p$b0.42 + yr
  yr <- rnorm(1, 0.0, p$sd.yr.tr43)
  a[4,3] <- p$prorep*tsf + p$pro.1*tsf + p$b2.tr43*year + p$b0.43 + yr
  yr <- rnorm(1, 0.0, p$sd.yr.tr44)
  a[4,4] <- p$s.s*tsf + p$b2.tr44*year + p$b0.44 + yr
  yr <- rnorm(1, 0.0, p$sd.yr.tr45)
  a[4,5] <- p$reg.1*tsf + p$b2.tr45*year + p$b0.45 + yr
  yr <- rnorm(1, 0.0, p$sd.yr.tr46)
  a[4,6] <- p$reg.2*tsf + p$b2.tr46*year + p$b0.46 + yr
  # row 5
  yr <- rnorm(1, 0.0, p$sd.yr.tr52)
  a[5,2] <- p$leap*tsf + p$pro.3*tsf + p$b2.tr52*year + p$b0.52 + yr # yr was missing here too
  yr <- rnorm(1, 0.0, p$sd.yr.tr53)
  a[5,3] <- p$prorep*tsf + p$pro.2*tsf + p$b2.tr53*year + p$b0.53 + yr
  yr <- rnorm(1, 0.0, p$sd.yr.tr54)
  a[5,4] <- p$pro.1*tsf + p$b2.tr54*year + p$b0.54 + yr
  yr <- rnorm(1, 0.0, p$sd.yr.tr55)
  a[5,5] <- p$s.m*tsf + p$b2.tr55*year + p$b0.55 + yr
  yr <- rnorm(1, 0.0, p$sd.yr.tr56)
  a[5,6] <- p$reg.1*tsf + p$b2.tr56*year + p$b0.56 + yr
  # row 6
  yr <- rnorm(1, 0.0, p$sd.yr.tr62)
  a[6,2] <- p$leap*tsf + p$pro.4*tsf + p$b2.tr62*year + p$b0.62 + yr # yr was missing here
  yr <- rnorm(1, 0.0, p$sd.yr.tr63)
  a[6,3] <- p$prorep*tsf + p$pro.3*tsf + p$b2.tr63*year + p$b0.63 + yr
  yr <- rnorm(1, 0.0, p$sd.yr.tr64)
  a[6,4] <- p$pro.2*tsf + p$b2.tr64*year + p$b0.64 + yr
  yr <- rnorm(1, 0.0, p$sd.yr.tr65)
  a[6,5] <- p$pro.1*tsf + p$b2.tr65*year + p$b0.65 + yr
  yr <- rnorm(1, 0.0, p$sd.yr.tr66)
  a[6,6] <- p$s.l*tsf + p$b2.tr66*year + p$b0.66 + yr

  # row 7  --- death row
  a[7,2] <- 0
  a[7,3] <- 0
  a[7,4] <- 0
  a[7,5] <- 0
  a[7,6] <- 0
  # normalizing to 1
  for (y in 2:6) {
    den <- sum(exp(a[3:7,y]))
    for (z in 3:7) {
      a[z,y] <- exp(a[z,y])/den
    }
  }
  list(a=a, g=g, ss=ss)
}

# this function creates demographic stochasticity for plant transitions
mat.sample <- function(n, m, stage) {
  if (n[stage] > 0) {
    samp <- rmultinom(1, n[stage], m[3:7,stage])
  } else {
    samp <- c(0, 0, 0, 0, 0)
  }
  samp
}

lambda <- function(n, n.gen) { # calculates stochastic population growth rate (lambda)
                               # see Caswell Eq. 14.61
  r <- numeric(0)
  for (t in 1:(n.gen-1)) {
    if ((n[t+1] > 0) && (n[t] > 0)) {
      r[t] <- n[t+1]/n[t]
    } else {
      r[t] <- NA
    }
  }
  exp(mean(log(r), na.rm=TRUE))
}

qE <- function(n, thresh, fire) { # tests whether population size fell below quasi-extinction threshold
   extinct <- FALSE
   val <- 0
   for (i in 1:length(n)) {
     if ((n[i] < thresh)) {
       extinct <- TRUE
       val <- 1
     }
   }
   val
}

simulate.one.replicate <- function(fire.t, parameters, n.gen, n.sdbank) {
  # to avoid integer overflow
  max.sd.bank <- 2^30

  n.lambda <- vector(mode="integer", length=n.gen) # a vector storing population sizes

  delta <- parameters$delta # get the probability of seed survival from object "parameters"
#  ss <- parameters$ss

  n <- c(n.sdbank, 0, 0, 0, 0, 0, 0)        # start simulation of pop growth with seeds only
  tsf <- 0                                  # start simulation the year of fire
  for (k in 1:n.gen) {
    if ((k == 1) || (n.lambda[k-1] > 0)) {  # if the population has not gone extinct
      tsf <- tsf + 1                        # step time-since-fire 1 forward
      m <- construct.matrix(parameters, tsf)

                                        # fates of plants are sampled
      n.2 <- mat.sample(n, m$a, 2) # n[2] = seedling
      n.3 <- mat.sample(n, m$a, 3) # n[3] = vegetative
      n.4 <- mat.sample(n, m$a, 4) # n[4] = small fl.
      n.5 <- mat.sample(n, m$a, 5) # n[5] = med. fl.
      n.6 <- mat.sample(n, m$a, 6) # n[6] = lg. fl.
    # n[7] = dead
                                        # fates of seeds are sampled
    # n[1] = seed bank
      p <- vector(mode="numeric", length=3) # multinomial describing the probabilities of 3 seed fates
      p[1] <- m$a[1,1]              # p[1] = seed to seed
      p[2] <- exp(-delta*4)*m$g*m$ss  # p[2] = seed to germ
      p[3] <- 1.0 - p[1] - p[2]     # p[3] = seed dies

      if (n[1] > 0) {               # if there are any seeds...
        if (n[1] >= max.sd.bank) {      # if the number of seeds has exceeded the max
          n[1] <- max.sd.bank - 1       # return one less than the max
        }
# sample seed fates according to vector of probabilities defined above, p
        n.sd.fate <- rmultinom(1, n[1], p)
      } else {                      # otherwise, return no seeds
        n.sd.fate <- c(0, 0, 0)
      }
# new seed input (rnd samples from Poisson's)
      n.sd.inp <- rpois(1, n[4]*m$a[1,4]) + rpois(1, n[5]*m$a[1,5])
                        + rpois(1, n[6]*m$a[1,6])
      if (n.sd.inp > 0) {           # if there are any new seeds...
        if (n.sd.inp >= max.sd.bank) {
          n.sd.inp <- max.sd.bank - 1
        }
        n.sd.inp.fate <- rmultinom(1, n.sd.inp, p) # sample fates of new seeds
      } else {
        n.sd.inp.fate <- c(0, 0, 0)
      }
# number of seeds = old seeds that survived + new seeds that survived
      n[1] <- n.sd.fate[1] + n.sd.inp.fate[1]

                                        # seedlings
# no. of seedlings  = old seeds that germinated + new seeds that germinated
      n[2] <- n.sd.fate[2] + n.sd.inp.fate[2]

                                        # adults
      n[3] <- n.2[1] + n.3[1] + n.4[1] + n.5[1] + n.6[1]
      n[4] <- n.2[2] + n.3[2] + n.4[2] + n.5[2] + n.6[2]
      n[5] <- n.2[3] + n.3[3] + n.4[3] + n.5[3] + n.6[3]
      n[6] <- n.2[4] + n.3[4] + n.4[4] + n.5[4] + n.6[4]
      if (fire.t[k]) {              # if fire occurs
        for (f in 2:6) {
          n[f] <- 0                 # all plants die
        }
        tsf <- 0                    # and tsf is set to zero
      }
    } else {
      n <- c(0, 0, 0, 0, 0, 0, 0)   # clean the slate
    }
    n.lambda[k] <- sum(n[1:6])      # population size = sum of seeds + plants (seedlings + veg + flowering)
  }                                 # k in 1:n.gen
  n.lambda                          # return the vector n.lambda (time series of population size)
}

run.simulation <- function(parameters, samples, n.replicates, med, shape,
                           do.plot)
{
  n.gen <- 50                       # each simulation of population growth will run for 50 years
  n.sdbank <- 1e6                   # the simulation starts with 1 million seeds in the soil
  n.samples <- length(samples)      # all 3000 posterior samples used

  # setting up for ANOVA on lambda
  lambda <- vector(mode="numeric", length=(n.samples*n.replicates))
  lambda.grp <- vector(mode="integer", length=(n.samples*n.replicates))  # this will have the labels (which posterior sample)

  # setting up for glm() on pqE
# this ends up containing pop sizes
  lambda.pop <- vector(mode="numeric", length=n.replicates*n.samples)
  final <- vector(mode="integer", length=n.replicates*n.samples)
  min <- vector(mode="integer", length=n.replicates*n.samples)
  min.sdb <- vector(mode="integer", length=n.samples)
  pqe.1 <- vector(mode="numeric", length=n.samples)
  pqe.10 <- vector(mode="numeric", length=n.samples)
  pqe.100 <- vector(mode="numeric", length=n.samples)
  pqe.1000 <- vector(mode="numeric", length=n.samples)

  # FALSE by default, set to TRUE when there is a fire
# this contains the fire histories that will be used for all posterior samples
  fire.t <- matrix(nrow=n.replicates, ncol=n.gen)
  for (j in 1:n.replicates) {
    fire.t[j,] <- fire.w(n.gen, med, shape)
  }

  # among posterior samples, i.e., parameter uncertainty
  for (i in 1:n.samples) {         # i = number of posterior samples
    if (do.plot && (i == 1)) {
      plot(c(1,n.gen), c(0,log10(10*n.sdbank)), typ="n", xlab="Generation",
           ylab="Population size", main=paste(med, " yrs."))
    }
    pqe.1.temp <- vector(mode="numeric", length=n.replicates)
    pqe.10.temp <- vector(mode="numeric", length=n.replicates)
    pqe.100.temp <- vector(mode="numeric", length=n.replicates)
    pqe.1000.temp <- vector(mode="numeric", length=n.replicates)

    # within a posterior sample, i.e., process variability
    for (j in 1:n.replicates) {    # j = number of replicates of population growth
      cat("median: ", med, ", sample: ", i, ", replicate: ", j, "\n")
      flush.console()

# runs simulation of population growth, using fire history in fire.t
      n.lambda <- simulate.one.replicate(fire.t[j,], parameters[samples[i],],
                                         n.gen, n.sdbank)              # returns vector of population sizes
  # calculates stochastic growth rate (using function "lambda")
      lambda.pop[(i-1)*n.replicates + j] <- lambda(n.lambda, n.gen)
      lambda.grp[(i-1)*n.replicates + j] <- i         # has the label indicating which posterior sample
      pqe.1.temp[j] <- qE(n.lambda, 1, fire.t[j,])
      pqe.10.temp[j] <- qE(n.lambda, 10, fire.t[j,])
      pqe.100.temp[j] <- qE(n.lambda, 100, fire.t[j,])
      pqe.1000.temp[j] <- qE(n.lambda, 1000, fire.t[j,])
      if (do.plot && (i == 1)) {
        lines(seq(1, n.gen), log10(n.lambda+1), col=j, lwd=2)
      }
    }                                                                  # j = number of replicates of population growth
# what fraction of the replicates fell below quasi-extinction threshold?
    pqe.1[i] <- sum(pqe.1.temp)/length(pqe.1.temp)
    pqe.10[i] <- sum(pqe.10.temp)/length(pqe.10.temp)
    pqe.100[i] <- sum(pqe.100.temp)/length(pqe.100.temp)
    pqe.1000[i] <- sum(pqe.1000.temp)/length(pqe.1000.temp)
  }                                                                    # i = number of posterior samples
  lambda.data <- data.frame(lambda=lambda.pop, lambda.grp=lambda.grp)
  list(lambda=lambda.data, pqe.1=pqe.1, pqe.10=pqe.10, pqe.100=pqe.100,
       pqe.1000=pqe.1000)
}


summarize.pqe <- function(x) {
  list(mu=mean(x), s.dev=sd(x))
}

summarize.results <- function(x) {  # runs nested ANOVA of growth rate, reports mn and sd of pqe's
   library(lme4)
   lambda <- lmer(lambda ~ (1|lambda.grp), family=gaussian, data=x$lambda,
                  na.action=na.omit)
   list(lambda=lambda,
        pqe.1=summarize.pqe(x$pqe.1), pqe.10=summarize.pqe(x$pqe.10),
        pqe.100=summarize.pqe(x$pqe.100), pqe.1000=summarize.pqe(x$pqe.1000))
}

plot.pqe <- function(x.2, x.4, x.6, x.8, x.10, x.12, x.14, x.16, x.18, x.20,
                     x.22, x.24, x.26, x.28, x.30, x.32, x.34, x.36, x.38,
                     x.40, shape)
{
  par(mfrow=c(1,1))
  plot(c(0,40), c(0, 1), typ="n", xlab="Median return interval",
       ylab="pQE", main=paste("c.v=",weib.cv(shape)))
  lines(seq(2,40,by=2),
        c(mean(x.2$pqe.1), mean(x.4$pqe.1), mean(x.6$pqe.1),
          mean(x.8$pqe.1), mean(x.10$pqe.1), mean(x.12$pqe.1),
          mean(x.14$pqe.1), mean(x.16$pqe.1), mean(x.18$pqe.1),
          mean(x.20$pqe.1), mean(x.22$pqe.1), mean(x.24$pqe.1),
          mean(x.26$pqe.1), mean(x.28$pqe.1), mean(x.30$pqe.1),
          mean(x.32$pqe.1), mean(x.34$pqe.1), mean(x.36$pqe.1),
          mean(x.38$pqe.1), mean(x.40$pqe.1)), col=1)
  points(seq(2,40,by=2),
        c(mean(x.2$pqe.1), mean(x.4$pqe.1), mean(x.6$pqe.1),
          mean(x.8$pqe.1), mean(x.10$pqe.1), mean(x.12$pqe.1),
          mean(x.14$pqe.1), mean(x.16$pqe.1), mean(x.18$pqe.1),
          mean(x.20$pqe.1), mean(x.22$pqe.1), mean(x.24$pqe.1),
          mean(x.26$pqe.1), mean(x.28$pqe.1), mean(x.30$pqe.1),
          mean(x.32$pqe.1), mean(x.34$pqe.1), mean(x.36$pqe.1),
          mean(x.38$pqe.1), mean(x.40$pqe.1)), pch=19, cex=2, col=1)
  lines(seq(2,40,by=2),
        c(mean(x.2$pqe.10), mean(x.4$pqe.10), mean(x.6$pqe.10),
          mean(x.8$pqe.10), mean(x.10$pqe.10), mean(x.12$pqe.10),
          mean(x.14$pqe.10), mean(x.16$pqe.10), mean(x.18$pqe.10),
          mean(x.20$pqe.10), mean(x.22$pqe.10), mean(x.24$pqe.10),
          mean(x.26$pqe.10), mean(x.28$pqe.10), mean(x.30$pqe.10),
          mean(x.32$pqe.10), mean(x.34$pqe.10), mean(x.36$pqe.10),
          mean(x.38$pqe.10), mean(x.40$pqe.10)), col=2)
  points(seq(2,40,by=2),
        c(mean(x.2$pqe.10), mean(x.4$pqe.10), mean(x.6$pqe.10),
          mean(x.8$pqe.10), mean(x.10$pqe.10), mean(x.12$pqe.10),
          mean(x.14$pqe.10), mean(x.16$pqe.10), mean(x.18$pqe.10),
          mean(x.20$pqe.10), mean(x.22$pqe.10), mean(x.24$pqe.10),
          mean(x.26$pqe.10), mean(x.28$pqe.10), mean(x.30$pqe.10),
          mean(x.32$pqe.10), mean(x.34$pqe.10), mean(x.36$pqe.10),
          mean(x.38$pqe.10), mean(x.40$pqe.10)), pch=19, cex=2, col=2)
  lines(seq(2,40,by=2),
        c(mean(x.2$pqe.100), mean(x.4$pqe.100), mean(x.6$pqe.100),
          mean(x.8$pqe.100), mean(x.10$pqe.100), mean(x.12$pqe.100),
          mean(x.14$pqe.100), mean(x.16$pqe.100), mean(x.18$pqe.100),
          mean(x.20$pqe.100), mean(x.22$pqe.100), mean(x.24$pqe.100),
          mean(x.26$pqe.100), mean(x.28$pqe.100), mean(x.30$pqe.100),
          mean(x.32$pqe.100), mean(x.34$pqe.100), mean(x.36$pqe.100),
          mean(x.38$pqe.100), mean(x.40$pqe.100)), col=3)
  points(seq(2,40,by=2),
        c(mean(x.2$pqe.100), mean(x.4$pqe.100), mean(x.6$pqe.100),
          mean(x.8$pqe.100), mean(x.10$pqe.100), mean(x.12$pqe.100),
          mean(x.14$pqe.100), mean(x.16$pqe.100), mean(x.18$pqe.100),
          mean(x.20$pqe.100), mean(x.22$pqe.100), mean(x.24$pqe.100),
          mean(x.26$pqe.100), mean(x.28$pqe.100), mean(x.30$pqe.100),
          mean(x.32$pqe.100), mean(x.34$pqe.100), mean(x.36$pqe.100),
          mean(x.38$pqe.100), mean(x.40$pqe.100)), pch=19, cex=2, col=3)
  lines(seq(2,40,by=2),
        c(mean(x.2$pqe.1000), mean(x.4$pqe.1000), mean(x.6$pqe.1000),
          mean(x.8$pqe.1000), mean(x.10$pqe.1000), mean(x.12$pqe.1000),
          mean(x.14$pqe.1000), mean(x.16$pqe.1000), mean(x.18$pqe.1000),
          mean(x.20$pqe.1000), mean(x.22$pqe.1000), mean(x.24$pqe.1000),
          mean(x.26$pqe.1000), mean(x.28$pqe.1000), mean(x.30$pqe.1000),
          mean(x.32$pqe.1000), mean(x.34$pqe.1000), mean(x.36$pqe.1000),
          mean(x.38$pqe.1000), mean(x.40$pqe.1000)), col=4)
  points(seq(2,40,by=2),
        c(mean(x.2$pqe.1000), mean(x.4$pqe.1000), mean(x.6$pqe.1000),
          mean(x.8$pqe.1000), mean(x.10$pqe.1000), mean(x.12$pqe.1000),
          mean(x.14$pqe.1000), mean(x.16$pqe.1000), mean(x.18$pqe.1000),
          mean(x.20$pqe.1000), mean(x.22$pqe.1000), mean(x.24$pqe.1000),
          mean(x.26$pqe.1000), mean(x.28$pqe.1000), mean(x.30$pqe.1000),
          mean(x.32$pqe.1000), mean(x.34$pqe.1000), mean(x.36$pqe.1000),
          mean(x.38$pqe.1000), mean(x.40$pqe.1000)), pch=19, cex=2, col=4)
}

plot.lambda <- function(x.2, x.4, x.6, x.8, x.10, x.12, x.14, x.16, x.18, x.20,
                     x.22, x.24, x.26, x.28, x.30, x.32, x.34, x.36, x.38,
                     x.40, shape)
{
   library(lme4)
   lambda <- vector(mode="numeric", length=20)
   lambda[1]  <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.2$lambda, na.action=na.omit))
   lambda[2]  <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.4$lambda, na.action=na.omit))
   lambda[3]  <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.6$lambda, na.action=na.omit))
   lambda[4]  <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.8$lambda, na.action=na.omit))
   lambda[5]  <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.10$lambda, na.action=na.omit))
   lambda[6]  <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.12$lambda, na.action=na.omit))
   lambda[7]  <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.14$lambda, na.action=na.omit))
   lambda[8]  <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.16$lambda, na.action=na.omit))
   lambda[9]  <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.18$lambda, na.action=na.omit))
   lambda[10] <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.20$lambda, na.action=na.omit))
   lambda[11] <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.22$lambda, na.action=na.omit))
   lambda[12] <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.24$lambda, a.action=na.omit))
   lambda[13] <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.26$lambda, a.action=na.omit))
   lambda[14] <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.28$lambda, a.action=na.omit))
   lambda[15] <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.30$lambda, a.action=na.omit))
   lambda[16] <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.32$lambda, a.action=na.omit))
   lambda[17] <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.34$lambda, a.action=na.omit))
   lambda[18] <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.36$lambda, a.action=na.omit))
   lambda[19] <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.38$lambda, a.action=na.omit))
   lambda[20] <- fixef(lmer(lambda ~ (1|lambda.grp), family=gaussian,
                            data=x.40$lambda, na.action=na.omit))
   x <- seq(2,40,by=2)
#   x11(width=7, height=5.25)
   png("lambda.png")
#   windows()
   par(mfrow=c(1,1))
   plot(x, lambda, typ="l", xlab="median time between fires (years)", ylab="stochastic population growth rate",
        lwd=2, main=paste("c.v.=", weib.cv(shape)))
   points(x, lambda, pch=19, cex=2)
}

######################################
#                 this is the one that does it all!!        #
######################################
simulation <- function(n.samp, n.rep, shape, do.plot) {
  posterior <- import.MCMC()
  if (do.plot) {
#   x11(width=7, height=5.25)
    png("simulation.png")
#    windows()
    par(mfrow=c(4,5), cex=0.4)
  }

  # select random sample of parameter values from posterior
  samples <- sample(length(posterior$delta), n.samp)
  x.2  <- run.simulation(posterior, samples, n.rep,  2, shape, do.plot)
  x.4  <- run.simulation(posterior, samples, n.rep,  4, shape, do.plot)
  x.6  <- run.simulation(posterior, samples, n.rep,  6, shape, do.plot)
  x.8  <- run.simulation(posterior, samples, n.rep,  8, shape, do.plot)
  x.10 <- run.simulation(posterior, samples, n.rep, 10, shape, do.plot)
  x.12 <- run.simulation(posterior, samples, n.rep, 12, shape, do.plot)
  x.14 <- run.simulation(posterior, samples, n.rep, 14, shape, do.plot)
  x.16 <- run.simulation(posterior, samples, n.rep, 16, shape, do.plot)
  x.18 <- run.simulation(posterior, samples, n.rep, 18, shape, do.plot)
  x.20 <- run.simulation(posterior, samples, n.rep, 20, shape, do.plot)
  x.22 <- run.simulation(posterior, samples, n.rep, 22, shape, do.plot)
  x.24 <- run.simulation(posterior, samples, n.rep, 24, shape, do.plot)
  x.26 <- run.simulation(posterior, samples, n.rep, 26, shape, do.plot)
  x.28 <- run.simulation(posterior, samples, n.rep, 28, shape, do.plot)
  x.30 <- run.simulation(posterior, samples, n.rep, 30, shape, do.plot)
  x.32 <- run.simulation(posterior, samples, n.rep, 32, shape, do.plot)
  x.34 <- run.simulation(posterior, samples, n.rep, 34, shape, do.plot)
  x.36 <- run.simulation(posterior, samples, n.rep, 36, shape, do.plot)
  x.38 <- run.simulation(posterior, samples, n.rep, 38, shape, do.plot)
  x.40 <- run.simulation(posterior, samples, n.rep, 40, shape, do.plot)
  if (do.plot) {
#  x11(width=7, height=5.25)
    png("pqe.png")
#  windows()
    plot.pqe(x.2, x.4, x.6, x.8, x.10, x.12, x.14, x.16, x.18, x.20,
             x.22, x.24, x.26, x.28, x.30, x.32, x.34, x.36, x.38, x.40, shape)
    plot.lambda(x.2, x.4, x.6, x.8, x.10, x.12, x.14, x.16, x.18, x.20,
             x.22, x.24, x.26, x.28, x.30, x.32, x.34, x.36, x.38, x.40, shape)
  }
  list(x.2=x.2, x.4=x.4, x.6=x.6, x.8=x.8, x.10=x.10, x.12=x.12, x.14=x.14,
       x.16=x.16, x.18=x.18, x.20=x.20, x.22=x.22, x.24=x.24, x.26=x.26,
       x.28=x.28, x.30=x.30, x.32=x.32, x.34=x.34, x.36=x.36, x.38=x.38,
       x.40=x.40,
       results.2=summarize.results(x.2),
       results.4=summarize.results(x.4),
       results.6=summarize.results(x.6),
       results.8=summarize.results(x.8),
       results.10=summarize.results(x.10),
       results.12=summarize.results(x.12),
       results.14=summarize.results(x.14),
       results.16=summarize.results(x.16),
       results.18=summarize.results(x.18),
       results.20=summarize.results(x.20),
       results.22=summarize.results(x.22),
       results.24=summarize.results(x.24),
       results.26=summarize.results(x.26),
       results.28=summarize.results(x.28),
       results.30=summarize.results(x.30),
       results.32=summarize.results(x.32),
       results.34=summarize.results(x.34),
       results.36=summarize.results(x.36),
       results.38=summarize.results(x.38),
       results.40=summarize.results(x.40))
}



plot.fire <- function(c) {  # plots the cdf of Weibull, 2 to 40 years btwn fires
  cv <- weib.cv(c)
  cv.string <- paste("c.v. = ", cv)
  plot(c(0,40), c(0,1), typ="n", xlab="Fire return interval",
       ylab="Cumulative probability", cex=1.25, cex.axis=1.25,
       cex.lab=1.25)
  med <- seq(2,40,by=2)
  p.f <- seq(0.001, 0.999, by=0.001)
  for (i in 1:length(med)) {
    b <- weib.b(med[i], c)
    tsf <- qweibull(p.f, c, b)
    lines(tsf, p.f, col=i, lwd=2)
  }
  lines(c(0,40), c(0.5, 0.5), lty=2, col="red")
}



pqe.add.to.matrix <- function(pqe, x, med) {
  n <- length(x$pqe.1)
  tmp <- matrix(nrow=4*n, ncol=3, dimnames=list(rep("", 4*n),
                c("med", "pQE", "thresh")))
  tmp[,1] <- rep(med, 4*n)
  tmp[,2] <- c(x$pqe.1, x$pqe.10, x$pqe.100, x$pqe.1000)
  tmp[,3] <- c(rep(1, n), rep(10,n), rep(100,n), rep(1000,n))
  pqe <- rbind(pqe, tmp)
  pqe
}

plot.pqe.lattice <- function(x) {
  library(lattice)
  pqe <- matrix(nrow=1, ncol=3, dimnames=list(c(""), c("med", "pQE", "thresh")))
  pqe <- pqe[FALSE,]
  pqe <- pqe.add.to.matrix(pqe, x$x.2, 2)
  pqe <- pqe.add.to.matrix(pqe, x$x.4, 4)
  pqe <- pqe.add.to.matrix(pqe, x$x.6, 6)
  pqe <- pqe.add.to.matrix(pqe, x$x.8, 8)
  pqe <- pqe.add.to.matrix(pqe, x$x.10, 10)
  pqe <- pqe.add.to.matrix(pqe, x$x.12, 12)
  pqe <- pqe.add.to.matrix(pqe, x$x.14, 14)
  pqe <- pqe.add.to.matrix(pqe, x$x.16, 16)
  pqe <- pqe.add.to.matrix(pqe, x$x.18, 18)
  pqe <- pqe.add.to.matrix(pqe, x$x.20, 20)
  pqe <- pqe.add.to.matrix(pqe, x$x.22, 22)
  pqe <- pqe.add.to.matrix(pqe, x$x.24, 24)
  pqe <- pqe.add.to.matrix(pqe, x$x.26, 26)
  pqe <- pqe.add.to.matrix(pqe, x$x.28, 28)
  pqe <- pqe.add.to.matrix(pqe, x$x.30, 30)
  pqe <- pqe.add.to.matrix(pqe, x$x.32, 32)
  pqe <- pqe.add.to.matrix(pqe, x$x.34, 34)
  pqe <- pqe.add.to.matrix(pqe, x$x.36, 36)
  pqe <- pqe.add.to.matrix(pqe, x$x.38, 38)
  pqe <- pqe.add.to.matrix(pqe, x$x.40, 40)
  pqe <- data.frame(med=as.factor(pqe[,1]), thresh=as.factor(pqe[,3]),
                    pQE=pqe[,2])
  bwplot(pQE ~ med | thresh, data=pqe, xlab="median time between fires (years)", ylab="probability of quasi-extinction",
         scales=list(x=list(draw=FALSE)))
}

lambda.add.to.matrix <- function(lambda, x, med) {
  n <- length(x$lambda$lambda)
  tmp <- matrix(nrow=n, ncol=2, dimnames=list(rep("", n),
                c("med", "lambda")))
  tmp[,1] <- rep(med, n)
  tmp[,2] <- c(x$lambda$lambda)
  lambda <- rbind(lambda, tmp)
  lambda
}

#### ACTUAL COMMANDS TO RUN SIMULATIONS AND MAKE PLOTS ####

results = simulation(1000, 100, 16, do.plot=T)
windows()
plot.lambda(results$x.2, results$x.4, results$x.6, results$x.8, results$x.10, results$x.12,
            results$x.14, results$x.16, results$x.18, results$x.20, results$x.22, results$x.24,
            results$x.26, results$x.28, results$x.30, results$x.32, results$x.34, results$x.36,
            results$x.38, results$x.40, shape=16)
windows()
plot.pqe.lattice(results)
#windows()
#plot.fire(16)

#### alternative way of plotting lambda (compared to plot.lambda)
lambdas = vector(mode="numeric", length=2000000)

lambdas[1:100000] = results$x.2$lambda[,1]
lambdas[100001:200000] = results$x.4$lambda[,1]
lambdas[200001:300000] = results$x.6$lambda[,1]
lambdas[300001:400000] = results$x.8$lambda[,1]
lambdas[400001:500000] = results$x.10$lambda[,1]
lambdas[500001:600000] = results$x.12$lambda[,1]
lambdas[600001:700000] = results$x.14$lambda[,1]
lambdas[700001:800000] = results$x.16$lambda[,1]
lambdas[800001:900000] = results$x.18$lambda[,1]
lambdas[900001:1000000] = results$x.20$lambda[,1]
lambdas[1000001:1100000] = results$x.22$lambda[,1]
lambdas[1100001:1200000] = results$x.24$lambda[,1]
lambdas[1200001:1300000] = results$x.26$lambda[,1]
lambdas[1300001:1400000] = results$x.28$lambda[,1]
lambdas[1400001:1500000] = results$x.30$lambda[,1]
lambdas[1500001:1600000] = results$x.32$lambda[,1]
lambdas[1600001:1700000] = results$x.34$lambda[,1]
lambdas[1700001:1800000] = results$x.36$lambda[,1]
lambdas[1800001:1900000] = results$x.38$lambda[,1]
lambdas[1900001:2000000] = results$x.40$lambda[,1]

tsflabel = rep(c(seq(2, 40, by=2)),each=100000)
lambda.data = cbind(tsflabel, lambdas)
for.boxplot = data.frame(tsflabel=tsflabel, lambdas=lambdas)
boxplot(lambdas ~ tsflabel, data=for.boxplot, las=1, ylab="stochastic growth rate", xlab="median time between fires (years)", cex.lab=1.5, cex.axis=1.3)