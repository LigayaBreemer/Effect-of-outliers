#I first created the functions for the randomization tests. To do this, functions for the 
#absolute mean and median difference were required too. I then created the functions for
#my evaluation criteria.

AbsMeanDiff <- function(group1, group2){
  return(abs(mean(group1)-mean(group2)))
}

AbsMedianDiff <- function(group1, group2){
  return(abs(median(group1)-median(group2)))
}

RandomTestMean <- function(group1, group2, Permutations=1000){
  
  TestStatistic <- AbsMeanDiff
  Observed <- TestStatistic(group1, group2)
  
  AllValues <- c(group1, group2)
  GroupMemberShips <- c(rep(TRUE,length(group1)),rep(FALSE,length(group2)))
  Hypothetical <- rep(NA,Permutations)
  for (i in 1:Permutations){
    CurGroupMembersShips <- sample(GroupMemberShips)
    CurGroup1 <- AllValues[CurGroupMembersShips]
    CurGroup2 <- AllValues[!CurGroupMembersShips]
    Hypothetical[i] <- TestStatistic(CurGroup1,CurGroup2)
  }
  return(sum(Observed<=Hypothetical)/Permutations)
}

RandomTestMedian <- function(group1, group2, Permutations=1000){
  
  TestStatistic <- AbsMedianDiff
  Observed <- TestStatistic(group1, group2)
  
  AllValues <- c(group1, group2)
  GroupMemberShips <- c(rep(TRUE,length(group1)),rep(FALSE,length(group2)))
  Hypothetical <- rep(NA,Permutations)
  for (i in 1:Permutations){
    CurGroupMembersShips <- sample(GroupMemberShips)
    CurGroup1 <- AllValues[CurGroupMembersShips]
    CurGroup2 <- AllValues[!CurGroupMembersShips]
    Hypothetical[i] <- TestStatistic(CurGroup1,CurGroup2)
  }
  return(sum(Observed<=Hypothetical)/Permutations)
}


TypeIErrorRate <- function(p.values){
  return((sum(p.values<0.05))/(length(p.values)))
}

TypeIIErrorRate <-function(p.values){
  return((sum(p.values>=0.05))/(length(p.values)))
}

#I then made a grid of all the design factors, which are sample size (see Table 1 in my 
#article for corresponding group sizes), effect size, and percentage of outliers on the
#lower and higher half.

Samp <- c(50, 100, 150, 500)
ES <- c(0, 0.2, 0.5, 0.8)
Outliers <- c("0%", "8% one-sided", "4% two-sided", "16% one-sided", "8% two-sided", "24% one-sided", "12% two-sided", "32% one-sided", "16% two-sided")
Design <- expand.grid(Samp = Samp, ES = ES, Outliers = Outliers)


#I then created vectors for group sizes and effect size, and I created empty
#objects for the error rates per test.

samp1 <- c(rep(c(25, 25, 75, 250), 36))
samp2 <- c(rep(c(25, 75, 75, 250), 36))
es <- c(rep(c(rep(0,4), rep(0.2,4), rep(0.5,4), rep(0.8,4)),9))

ErrorRMean <- numeric()
ErrorRMedian <-numeric()
ErrorWelch <- numeric()
ErrorWilcoxon <- numeric()

#This is where the simulation starts.
#For each condition, the p-value objects are 'emptied' and reused. The error rates will 
#be saved separately, instead. The Type I and Type II Error Rates are combined in the 
#Error variables. At the end I create a table called Table1, in which you can see for which condition Type I and for which 
#conditions Type II Error Rates were used. You can tell from the effect size (ES).

for(condition in 1:144){
  set.seed(condition)
  pRMean <- numeric()
  pRMedian <- numeric()
  pWelch <- numeric()
  pWilcoxon <- numeric()
  for(simulation in 1:10000){
    group1 <- rnorm(samp1[condition], 0, 1)
    group2 <- rnorm(samp2[condition], 0 + es[condition], 1)
    
    if(condition >= 17 && condition <= 32){
      Replace1 <- sample(1:length(group1), round(.08*length(group1)))
      outliers <- rnorm(length(group1),-5,0.5)
      group1[Replace1] <- outliers[Replace1]
    }else if(condition >= 33 && condition <= 48){
      ReplaceL1 <- sample(1:length(group1), round(.04*length(group1)))
      outliers <- rnorm(length(group1),-5,0.5)
      group1[ReplaceL1] <- outliers[ReplaceL1]
      ReplaceH1 <- sample(1:length(group1), round(.04*length(group1)))
      outliers <- rnorm(length(group1),5,0.5)
      group1[ReplaceH1] <- outliers[ReplaceH1]
    }else if(condition >= 49 && condition <= 64){
      Replace1 <- sample(1:length(group1), round(.16*length(group1)))
      outliers <- rnorm(length(group1),-5,0.5)
      group1[Replace1] <- outliers[Replace1]
    }else if(condition >= 65 && condition <= 80){
      ReplaceL1 <- sample(1:length(group1), round(.08*length(group1)))
      outliers <- rnorm(length(group1),-5,0.5)
      group1[ReplaceL1] <- outliers[ReplaceL1]
      ReplaceH1 <- sample(1:length(group1), round(.08*length(group1)))
      outliers <- rnorm(length(group1),5,0.5)
      group1[ReplaceH1] <- outliers[ReplaceH1]
    }else if(condition >= 81 && condition <= 96){
      Replace1 <- sample(1:length(group1), round(.24*length(group1)))
      outliers <- rnorm(length(group1),-5,0.5)
      group1[Replace1] <- outliers[Replace1]
    }else if(condition >= 97 && condition <= 112){
      ReplaceL1 <- sample(1:length(group1), round(.12*length(group1)))
      outliers <- rnorm(length(group1),-5,0.5)
      group1[ReplaceL1] <- outliers[ReplaceL1]
      ReplaceH1 <- sample(1:length(group1), round(.12*length(group1)))
      outliers <- rnorm(length(group1),5,0.5)
      group1[ReplaceH1] <- outliers[ReplaceH1]
    }else if(condition >= 113 && condition <= 128){
      Replace1 <- sample(1:length(group1), round(.32*length(group1)))
      outliers <- rnorm(length(group1),-5,0.5)
      group1[Replace1] <- outliers[Replace1]
    }else if(condition >= 129 && condition <= 144){
      ReplaceL1 <- sample(1:length(group1), round(.16*length(group1)))
      outliers <- rnorm(length(group1),-5,0.5)
      group1[ReplaceL1] <- outliers[ReplaceL1]
      ReplaceH1 <- sample(1:length(group1), round(.16*length(group1)))
      outliers <- rnorm(length(group1),5,0.5)
      group1[ReplaceH1] <- outliers[ReplaceH1]
    }
    
    t <- t.test(group1,group2)
    pWelch[simulation] <- t$p.value
    w <- wilcox.test(group1, group2)
    pWilcoxon[simulation] <- w$p.value
    pRMean[simulation] <- RandomTestMean(group1, group2)
    pRMedian[simulation] <- RandomTestMedian(group1, group2)
  }
  if(es[condition]==0){
    ErrorRMean[condition] <- TypeIErrorRate(pRMean)
    ErrorRMedian[condition] <- TypeIErrorRate(pRMedian)
    ErrorWelch[condition] <- TypeIErrorRate(pWelch)
    ErrorWilcoxon[condition] <- TypeIErrorRate(pWilcoxon)
  }
  else{
    ErrorRMean[condition] <- TypeIIErrorRate(pRMean)
    ErrorRMedian[condition] <- TypeIIErrorRate(pRMedian)
    ErrorWelch[condition] <- TypeIIErrorRate(pWelch)
    ErrorWilcoxon[condition] <- TypeIIErrorRate(pWilcoxon) 
  }
}

Table1 <- cbind(Design, ErrorRMean, ErrorRMedian, ErrorWelch, ErrorWilcoxon)