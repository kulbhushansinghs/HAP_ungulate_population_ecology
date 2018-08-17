##Master DOS Data is a csv containing DOS data from all sites(India, Kyrgyzstan, Mongolia) and all species (Ibex, Blue Sheep, Argali, Urial & Nilgiri Tahr)
DOS <- read.csv("Master DOS Data_Munib.csv")

#testing
#testing
#testing
#testing
#testing

library(BBRecapture)
library(Hmisc)
dos.bay<-function(dos, no.boot, max.pop){
  
  model1 <- BBRecap(dos[, 2:3],
                    mod=c("Mt"), prior.N = c("Uniform"),
                    output = c("complete.ML"), neval = max.pop)
  
  N.estimate<-model1$N.hat.mean * mean(dos[,1])
  det1 <- model1$pH.post.mean[1]
  det2 <- model1$pH.post.mean[2]
  
  est.arr<-rep(NA, no.boot)
  for(i in 1:no.boot){
    est.arr[i] <- mean(sample(dos[,1], length(dos[,1]), replace = T)) *
      sample(model1$N.range, size = 1, prob = model1$posterior.N)
  }
  LCI<- quantile(est.arr, 0.025)
  UCI<- quantile(est.arr, 0.975)
  
  hist(est.arr, xlab = "Population esimate", main = "Histogram of bootstrapped estimate")
  
  out.put<-c(N.estimate, model1$N.hat.mean, mean(dos[,1]), LCI, UCI, det1, det2)
  return(out.put)
}

##Example to estimate population of Blue Sheep from the year 2017 from Kibber
dos.bluesheep.kibber.2017<-subset(DOS, DOS$Block == "Kibber" & DOS$Species == "Blue sheep" & DOS$Year == "2017")
dos.bay(dos.bluesheep.kibber.2017[,6:8], 10000, 500)

