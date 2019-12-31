# model selection ####

# suppose we have data from an experiment like this:
# mean RT correct = 250ms
# mean RT incorrect = 246ms
# accuracy = 0.80

# try to fit this data with both models by adjusting the parameters of the model
# HINT: you can speed up your parameter search by using a small number of samples
# initially, and then increasing the samples as you get closer to a viable set
# of parameters.
# 2nd HINT: Don't adjust the sdrw parameter of the random.walk.model or the criterion
# paramter of the accumulator model.

# You don't need to get a perfect match. Just get in the ballpark. 

# 1. Random Walk Model - I changed the parameter for criterion to be 5 so people have slower reaction times
# because they will hesitate to make a judgement with the same amount of evidence. I modified the drift rate 
# so people will sample more evidence, which increases their accuracy rates. 

ev <- numeric()
rt.array <- numeric()
accuracy.array <- numeric()
count=1

random.walk.model <-
  function(samples,
           drift = 0.013,
           sdrw = 0.3,
           criterion = 5) {
    for (i in 1: samples){
      ev = cumsum(c(0, rnorm(1000, drift, sdrw)))
      for (i in 1:1000) {
        if (abs(ev[i]) >= criterion) {
          break
        }
      }
      rt.array[count] = i
      if (sign(ev[i]) == 1) {
        accuracy.array[count] = TRUE
      } else{
        accuracy.array[count] = FALSE
      }
      count = count + 1
    }
    output <- data.frame(correct = accuracy.array,
                         rt = rt.array)
    return(output)
  }

initial.test <- random.walk.model(1000)
sum(initial.test$correct) / length(initial.test$correct) # should be close to 0.8

mean(initial.test$rt) # should be about 250

# visualize the RT distributions ####

library(dplyr)

correct.data.rw <- initial.test %>% filter(correct==TRUE)
incorrect.data.rw <- initial.test %>% filter(correct==FALSE)

# 2. Accumulator model 
# For the accumulator model, I changed the evidence accumulation rate for the correct response so that 
# more evidence is likely to accumulate on each step. As a result, the accuracy rate increases because we
# are sampling more evidence for the correct response. I changed the criterion to raise the threshold for
# responding to increase the reaction time. 

accumulator.model <- function(samples, rate.1=37, rate.2=40, criterion=6){
  for (i in 1:samples){
    evi.acc.1=0
    evi.acc.2=0
    rt.count=0
    while(evi.acc.1<criterion || evi.acc.2<criterion){
      evi.acc.1 <- evi.acc.1+rexp (1, rate.1)
      evi.acc.2 <-evi.acc.2+rexp (1, rate.2)
      rt.count=rt.count+1
    }
    rt.array[count] = rt.count
    if(evi.acc.1>evi.acc.2){
      accuracy.array[count] = TRUE
    }else{
      accuracy.array[count]=FALSE
    }
    count = count + 1
  }
  output <- data.frame(
    rt = rt.array,
    correct = accuracy.array
  )
  return(output)
}

initial.test <- accumulator.model(1000)
sum(initial.test$correct) / length(initial.test$correct) # should be close to 0.8
mean(initial.test$rt) # should be about 250

correct.data.ac <- initial.test %>% filter(correct == TRUE)
incorrect.data.ac <- initial.test %>% filter(correct == FALSE)

# Can both models do a reasonable job of accounting for the mean RT and accuracy? Report the
# results of your efforts:

# For the random walk model, parameters of drift= 0.013 and criterion= 5 produce mean RTs 
# of 249.506. The overall accuracy is 81.2%. 

# For the accumulator model, parameters of rate.1 = 31 and rate.2 = 40 produce mean RTs
# of 243.597, respectively. The overall accuracy is 81.1%.

# Both models do a reasonal job of accounting for the mean RT and accuracy. They both approximate the numbers relatively
# well. 

# Using the parameters that you found above, plot histograms of the distribution of RTs
# predicted by each model. Based on these distributions, what kind of information could
# we use to evaluate which model is a better descriptor of the data for the experiment?
# Describe briefly how you might make this evaluation.

# Random walk
layout(matrix(1:4, nrow=2, byrow=T))
hist(correct.data.rw$rt)
hist(incorrect.data.rw$rt)

# Accumulator
hist(correct.data.ac$rt)
hist(incorrect.data.ac$rt)

# The accumulator model seems to be better because there is less spread around the mean. The standard deviation of the 
# data is much smaller so the data is more centered around the mean with no extreme outliers. 
