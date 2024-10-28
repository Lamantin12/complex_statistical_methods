#Step 2: Data preprocessing

?mtcars

df  <- mtcars
View(df)

str(df)

df$vs  <- factor(df$vs  , labels = c("V", "S"))
df$am  <- factor(df$am  , labels = c("Auto", "Manual"))


#Step 3: Descriptive statistics

median(df$mpg)
mean(df$disp)
sd(df$hp)
range(df$cyl)

mean_disp  <- mean(df$disp)

mean(df$mpg[df$cyl == 6])

mean(df$mpg[df$cyl == 6 & df$vs == "V"])

sd(df$hp[df$cyl != 3 & df$am == "Auto"])

#Step 5: Aggregation

?aggregate

mean_hp_vs  <- aggregate(x = df$hp, by = list(df$vs), FUN = mean)
mean_hp_vs

colnames(mean_hp_vs)  <- c("VS", "Mean HP")

aggregate(hp ~ vs, df, mean)

aggregate(hp ~ vs + am, df, mean)
aggregate(x = df$hp, by = list (df$vs, df$am), FUN = mean)

aggregate(x = df[,-c(8,9)], by = list(df$am), FUN = median)

aggregate(df[,c(1,3)], by = list(df$am, df$vs), FUN = sd)

aggregate(cbind(mpg, disp) ~ am + vs, df, sd)

my_stats  <- aggregate(cbind(mpg, disp) ~ am + vs, df, sd)
my_stats

#Step 8, 9: Library "psych"


library(psych)

?describe

describe(x = df)

descr  <- describe(x = df[,-c(8,9)])
descr

descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs)
descr2

descr2$V
descr2$S

descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1)
descr2

descr3  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1, fast = T)

describeBy(df$qsec, group = list(df$vs, df$am), digits = 1, 
           fast = T)


#Step 10: NA values

sum(is.na(df))

df$mpg[1:10]  <- NA

mean(df$mpg, na.rm = T)

aggregate(mpg ~am, df, sd)

describe(na.rm = )





-- EX
-- 1
result <- mean(mtcars$qsec[mtcars$cyl != 3 & mtcars$mpg > 20])
result


-- 2
descriptions_stat <- aggregate(cbind(hp, disp) ~ am, mtcars, sd )
descriptions_stat

-- 3
df <- airquality
sbset = subset(df, Month %in% c(7:9))
aggregate(Ozone ~ Month, sbset, FUN=length)

-- 4
df <- airquality
describeBy(df, df$Month)

-- 5
df <- iris
describe(iris)

-- 6
df <- iris
describeBy(df, df$Species)$virginica


-- 7 
?replace

my_vector <- rnorm(30)
my_vector[sample(1:30, 10)] <- NA
fixed_vector <- replace(my_vector, which(is.na(my_vector)), mean(my_vector[!is.na(my_vector)]))
result

?which

