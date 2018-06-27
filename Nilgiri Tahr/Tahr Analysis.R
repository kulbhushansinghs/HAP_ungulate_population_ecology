
library(BBRecapture)
library(Hmisc)
#########
# Listing custom functions
## Double observer survey analysis
dos.bay<-function(dos, no.boot, max.pop){
  model1 <- BBRecap(dos[, 2:3], 
                    mod=c("Mt"), prior.N = c("Uniform"), 
                    output = c("complete.ML"), neval = max.pop)
  N.estimate<-model1$N.hat.mean * mean(dos[,1]) 
  est.arr<-rep(NA, no.boot)
  
  for(i in 1:no.boot){
    est.arr[i] <- mean(sample(dos[,1], length(dos[,1]), replace = T)) *
      sample(model1$N.range, size = 1, prob = model1$posterior.N)
  }
  LCI<- quantile(est.arr, 0.025)
  UCI<- quantile(est.arr, 0.975)
  #hist.plot<-hist(est.arr, xlab = "Population esimate", main = "Posterior distribution of estimated population")
  out.put<-c(N.estimate, model1$N.hat.mean, mean(dos[,1]), LCI, UCI, model1$pH.post.mean)
  return(out.put)
}

setwd("/Users/Kullu/Dropbox (Snow Leopard Trust)/Kullu_desktop/Git/HAP_ungulate_population_ecology/Nilgiri Tahr")
dat<-read.csv("Master DOS Data.csv")

dat1<-subset(dat, dat$Block == "Anamalai", select = c(Group.size, Obs1, Obs2))

dos.bay(dat1, 1000, 2000)

dat2 <- subset(dat, dat$Block == "Anamalai")
dat2$Males <- dat2$Obs1.Class1 + dat2$Obs1.Class2 + dat2$Obs1.Class3 + dat2$Obs1.Class4
dat2$Females <- dat2$Obs1.AF + dat2$Obs1.SA
dat2$Juv <- dat2$Obs1.Young + dat2$Obs1.Kid

sum(dat2$Males, na.rm = T)
sum(dat2$Females, na.rm = T)
sum(dat2$Juv, na.rm = T)
 
sum(dat2$Males, na.rm = T) / (sum(dat2$Males, na.rm = T) + sum(dat2$Females, na.rm = T) + sum(dat2$Juv, na.rm = T)) # Percentage males int e population
sum(dat2$Females, na.rm = T) / (sum(dat2$Males, na.rm = T) + sum(dat2$Females, na.rm = T) + sum(dat2$Juv, na.rm = T)) # Percentage females in the population
sum(dat2$Juv, na.rm = T) / (sum(dat2$Males, na.rm = T) + sum(dat2$Females, na.rm = T) + sum(dat2$Juv, na.rm = T)) # Percentage young in the population

sum(dat2$Males, na.rm = T)/sum(dat2$Females, na.rm = T) ## Male to female ratio
sum(dat2$Juv, na.rm = T)/sum(dat2$Females, na.rm = T) ## Young to female ratio

