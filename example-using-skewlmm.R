library(skewlmm)

help(rsmsn.lmm)

## for only 1 subject
nj1 = 5
rsmsn.lmm(time1 = 1:nj1, 
          x1 = cbind(1, 1:nj1), 
          z1 = rep(1, nj1), 
          sigma2=.25, D1=diag(1),
          beta=c(1, 2), lambda=0, 
          depStruct="UNC",
          distr="sn")

# Generating a sample for m=25 individuals with 5 times
library(dplyr)
library(purrr)
library(ggplot2)
nj1 = 5
m = 25
gendatList = map(rep(nj1, m),
                 function(nj) rsmsn.lmm(time1 = 1:nj, 
                                        x1 = rnorm(nj),#x1 = cbind(1, 1:nj), 
                                        z1 = rep(1, nj),
                                        sigma2=.25, D1=.5*diag(1), 
                                        beta=.5, #c(1, 2),
                                        lambda=0))
gendat = bind_rows(gendatList, .id="ind")
ggplot(gendat, aes(x=x, y=y, group=ind)) + geom_line() +
  #stat_summary(aes(group=1), geom="line", fun=mean, col="blue", size=2) + 
  theme_bw()
#
fm1 = smsn.lmm(y ~ x, data=gendat, groupVar="ind", depStruct="ARp",
               pAR=1)
summary(fm1)