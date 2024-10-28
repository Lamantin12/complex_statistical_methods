# control statements

mydata <- read.csv('evals.csv')



# if

a <- -1

if (a > 0){
  print('positive')
} else {
  print('not positive')
}


if (a > 0){
  print('positive')
} else print('not positive')


if (a > 0){
  print('positive')
} else if (a < 0) {
  print('negative')
} else print('zero')



# ifelse

a <- 10


ifelse(a > 0, 'positive', 'not positive')

a <- c(1, -1)


# for 

for (i in 1:100){
  print(i)
}


for (i in 1:nrow(mydata)){
  print(mydata$score[i])
}


print("")
# for + if
for (i in 1:nrow(mydata)){
  if (mydata$gender[i] == 'male'){
    print(mydata$score[i]) 
  }
}



# for + if  VS  ifelse

mydata$quality <- rep(NA, nrow(mydata))

for (i in 1:nrow(mydata)){
  if (mydata$score[i] > 4){
    mydata$quality[i] <- 'good'
  } else mydata$quality[i] <- 'bad'
}





mydata$quality2 <- ifelse(mydata$score > 4, 'good', 'bad')







# while

i <- 1

while(i < 51){
  print(mydata$score[i])
  i <- i+1
}


-- EX
-- 1

library(datasets)
data(mtcars) 

mtcars$new_var <- ifelse((mtcars$carb >= 4) | (mtcars$cyl > 6), 1, 0)
mtcars

-- 2
my_vector <- c(0, 23.34)
if (mean(my_vector) > 20) {
  result <- "My mean is great"
} else {
  result <- "My mean is not so great"
} 
result

-- 3 
df <- AirPassengers
good_months<-c()
for (i in 1:(length(df)-1)) {
  if (df[i+1]>df[i]) {
    good_months<-append(good_months, i+1)
  }
}
good_months

AirPassengers[-1][AirPassengers[-144] > AirPassengers[-1]]

-- 4
moving_avg_window <- 10
moving_average <- numeric(length(df) - moving_avg_window + 1)
for (i in 1:(length(df) - moving_avg_window + 1)){
  moving_average[i] <- mean(df[c(i:(i+moving_avg_window-1))])
}
moving_average


