library(ggplot2)

str(diamonds)
# tibble[,10] [53,940 x 10] (S3: tbl_df/tbl/data.frame)
# $ carat  : num [1:53940] 0.23 0.21 0.23 0.29 0.31 0.24 0.24 0.26 0.22 0.23 ...
# $ cut    : Ord.factor w/ 5 levels "Fair"<"Good"<..: 5 4 2 4 2 3 3 3 1 3 ...
# $ color  : Ord.factor w/ 7 levels "D"<"E"<"F"<"G"<..: 2 2 2 6 7 7 6 5 2 5 ...
# $ clarity: Ord.factor w/ 8 levels "I1"<"SI2"<"SI1"<..: 2 3 5 4 2 6 7 3 4 5 ...
# $ depth  : num [1:53940] 61.5 59.8 56.9 62.4 63.3 62.8 62.3 61.9 65.1 59.4 ...
# $ table  : num [1:53940] 55 61 65 58 58 57 57 55 61 61 ...
# $ price  : int [1:53940] 326 326 327 334 335 336 336 337 337 338 ...
# $ x      : num [1:53940] 3.95 3.89 4.05 4.2 4.34 3.94 3.95 4.07 3.87 4 ...
# $ y      : num [1:53940] 3.98 3.84 4.07 4.23 4.35 3.96 3.98 4.11 3.78 4.05 ...
# $ z      : num [1:53940] 2.43 2.31 2.31 2.63 2.75 2.48 2.47 2.53 2.49 2.39 ...

head(diamonds)
# A tibble: 6 x 10
# carat cut       color clarity depth table price     x     y     z
# <dbl> <ord>     <ord> <ord>   <dbl> <dbl> <int> <dbl> <dbl> <dbl>
#   1 0.23  Ideal     E     SI2      61.5    55   326  3.95  3.98  2.43
# 2 0.21  Premium   E     SI1      59.8    61   326  3.89  3.84  2.31
# 3 0.23  Good      E     VS1      56.9    65   327  4.05  4.07  2.31
# 4 0.290 Premium   I     VS2      62.4    58   334  4.2   4.23  2.63
# 5 0.31  Good      J     SI2      63.3    58   335  4.34  4.35  2.75
# 6 0.24  Very Good J     VVS2     62.8    57   336  3.94  3.96  2.48

names(diamonds)
# [1] "carat"   "cut"     "color"   "clarity" "depth"   "table"   "price"   "x"      
# [9] "y"       "z" 

range(diamonds$price)
# [1]   326 18823

range(diamonds$carat)
# [1] 0.20 5.01

range(diamonds$cut)
# [1] Fair  Ideal
# Levels: Fair < Good < Very Good < Premium < Ideal

range(diamonds$color)
# [1] D J
# Levels: D < E < F < G < H < I < J

range(diamonds$clarity)
# [1] I1 IF
# Levels: I1 < SI2 < SI1 < VS2 < VS1 < VVS2 < VVS1 < IF

summary(diamonds)
# carat               cut             color        clarity          depth      
# Min.   :0.2000   Fair     : 1610   D: 6775   SI1    :13065   Min.   :43.00  
# 1st Qu.:0.4000   Good     : 4906   E: 9797   VS2    :12258   1st Qu.:61.00  
# Median :0.7000   Very Good:12082   F: 9542   SI2    : 9194   Median :61.80  
# Mean   :0.7979   Premium  :13791   G:11292   VS1    : 8171   Mean   :61.75  
# 3rd Qu.:1.0400   Ideal    :21551   H: 8304   VVS2   : 5066   3rd Qu.:62.50  
# Max.   :5.0100                     I: 5422   VVS1   : 3655   Max.   :79.00  
#                                    J: 2808   (Other): 2531  
#
# table           price             x                y                z         
# Min.   :43.00   Min.   :  326   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
# 1st Qu.:56.00   1st Qu.:  950   1st Qu.: 4.710   1st Qu.: 4.720   1st Qu.: 2.910  
# Median :57.00   Median : 2401   Median : 5.700   Median : 5.710   Median : 3.530  
# Mean   :57.46   Mean   : 3933   Mean   : 5.731   Mean   : 5.735   Mean   : 3.539  
# 3rd Qu.:59.00   3rd Qu.: 5324   3rd Qu.: 6.540   3rd Qu.: 6.540   3rd Qu.: 4.040  
# Max.   :95.00   Max.   :18823   Max.   :10.740   Max.   :58.900   Max.   :31.800

diamonds[is.na(diamonds) == TRUE]
# <unspecified> [0]
#nema nedefiniranih vrijednosti

categorical = data.frame(diamonds$cut, diamonds$color, diamonds$clarity)
head(categorical)
#         cut color clarity
# 1     Ideal     E     SI2
# 2   Premium     E     SI1
# 3      Good     E     VS1
# 4   Premium     I     VS2
# 5      Good     J     SI2
# 6 Very Good     J    VVS2

numerical = data.frame(diamonds$carat, diamonds$depth, 
                       diamonds$table, diamonds$price, diamonds$x, 
                       diamonds$y, diamonds$z)
head(numerical)
#   carat depth table price    x    y    z
# 1  0.23  61.5    55   326 3.95 3.98 2.43
# 2  0.21  59.8    61   326 3.89 3.84 2.31
# 3  0.23  56.9    65   327 4.05 4.07 2.31
# 4  0.29  62.4    58   334 4.20 4.23 2.63
# 5  0.31  63.3    58   335 4.34 4.35 2.75
# 6  0.24  62.8    57   336 3.94 3.96 2.48

# sredine uzoraka

statmod <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

for(i in 1:7){
  cat(names(numerical)[i],
    "\n aritmeticka sredina: ", mean(numerical[,i]),
    "\n medijan: ", median(numerical[,i]),
    "\n mod: ", statmod(numerical[,i]))
  readline(prompt = "[enter]")
}
# carat 
# aritmeticka sredina:  0.7979397 
# medijan:  0.7 
# mod:  0.3

# depth 
# aritmeticka sredina:  61.7494 
# medijan:  61.8 
# mod:  62

# table 
# aritmeticka sredina:  57.45718 
# medijan:  57 
# mod:  56

# price 
# aritmeticka sredina:  3932.8 
# medijan:  2401 
# mod:  605

# x 
# aritmeticka sredina:  5.731157 
# medijan:  5.7 
# mod:  4.37

# y 
# aritmeticka sredina:  5.734526 
# medijan:  5.71 
# mod:  4.34

# z 
# aritmeticka sredina:  3.538734 
# medijan:  3.53 
# mod:  2.7

# mjere rasprsenosti, asimetrije i zaobljenosti

library(e1071)
for(i in 1:7){
  cat(names(numerical)[i],
      "\n varijanca: ", var(numerical[,i]),
      "\n standardna devijacija: ", sd(numerical[,i]),
      "\n koeficijent asimetrije: ", skewness(numerical[,i]),
      "\n koeficijent zaobljenosti: ", kurtosis(numerical[,i]))
  readline(prompt = "[enter]")
}

# carat 
# varijanca:  0.2246867 
# standardna devijacija:  0.4740112 
# koeficijent asimetrije:  1.116584 
# koeficijent zaobljenosti:  1.25625

# depth 
# varijanca:  2.052404 
# standardna devijacija:  1.432621 
# koeficijent asimetrije:  -0.08228945 
# koeficijent zaobljenosti:  5.738447

# table 
# varijanca:  4.992948 
# standardna devijacija:  2.234491 
# koeficijent asimetrije:  0.7968515 
# koeficijent zaobljenosti:  2.801271

# price 
# varijanca:  15915629 
# standardna devijacija:  3989.44 
# koeficijent asimetrije:  1.618305 
# koeficijent zaobljenosti:  2.177191

# x 
# varijanca:  1.258347 
# standardna devijacija:  1.121761 
# koeficijent asimetrije:  0.3786553 
# koeficijent zaobljenosti:  -0.6183029

# y 
# varijanca:  1.304472 
# standardna devijacija:  1.142135 
# koeficijent asimetrije:  2.434031 
# koeficijent zaobljenosti:  91.2025

# z 
# varijanca:  0.4980109 
# standardna devijacija:  0.7056988 
# koeficijent asimetrije:  1.522338 
# koeficijent zaobljenosti:  47.08029

# stupcasti dijagram (apsolutnih) frekvencija kategorickih varijabli
for(i in 1:3){
  barplot(table(categorical[,i]), main = names(categorical)[i])
  readline(prompt = "[enter]")
}

# histogram numerickih varijabli
for(i in 1:7){
  hist(numerical[,i], main = names(numerical)[i])
  readline(prompt = "[enter]")
}

# histogram numerickih varijabli (empirijska gustoca) uz teorijsku gustocu
for(i in 1:7){
  hist(numerical[,i], main = names(numerical)[i], probability = TRUE)
  curve(dnorm(x, mean(numerical[,i]), sd(numerical[,i])), 
        col= "blue", add=TRUE, lwd=2)
  readline(prompt = "[enter]")
}

# dijagram gustoce podataka uz teorijsku gustocu
for(i in 1:7){
  plot(density(numerical[,i]), main = names(numerical)[i])
  curve(dnorm(x, mean(numerical[,i]), sd(numerical[,i])), 
        col= "blue", add=TRUE, lwd=2)
  readline(prompt = "[enter]")
}

# iz grafickih prikaza vidimo da ne mozemo zakljuciti normalnu distribuiranost
# podataka
# provjerimo ovu hipotezu statistickim testom normalnosti
# koristi se anderson-darling test jer prima vece uzorke
# (shapiro test ne preko 5000)
library(nortest)
for(i in 1:7){
  Sys.sleep(0.1)
  cat(names(numerical)[i], ad.test(numerical[,i])$p.value)
  flush.console()
  readline(prompt = "[enter]")
}
# carat 3.7e-24
# depth 3.7e-24
# table 3.7e-24
# price 3.7e-24
# x 3.7e-24
# y 3.7e-24
# z 3.7e-24

# buduci da su sve p vrijednosti znacajno manje od 0.01, na nivou znacajnosti
# od 99% ne mozemo zakljuciti da su podaci normalno distribuirani

# iz tog razloga cemo za test korelacije medu varijablama koristiti
# Spearmanovu metodu umjesto Pearsonove

cor(numerical, method="spearman")
#          carat       depth      table      price           x           y         z
# carat 1.00000000  0.03010375  0.1949803 0.96288280  0.99611660  0.99557175 0.9931834
# depth 0.03010375  1.00000000 -0.2450611 0.01001967 -0.02344221 -0.02542522 0.1034984
# table 0.19498032 -0.24506114  1.0000000 0.17178448  0.20223061  0.19573406 0.1598782
# price 0.96288280  0.01001967  0.1717845 1.00000000  0.96319611  0.96271882 0.9572323
# x     0.99611660 -0.02344221  0.2022306 0.96319611  1.00000000  0.99789493 0.9873553
# y     0.99557175 -0.02542522  0.1957341 0.96271882  0.99789493  1.00000000 0.9870675
# z     0.99318344  0.10349836  0.1598782 0.95723227  0.98735532  0.98706751 1.0000000

cormat <- round(cor(numerical, method="spearman"),2)
cormat
# carat depth table price     x     y    z
# carat  1.00  0.03  0.19  0.96  1.00  1.00 0.99
# depth  0.03  1.00 -0.25  0.01 -0.02 -0.03 0.10
# table  0.19 -0.25  1.00  0.17  0.20  0.20 0.16
# price  0.96  0.01  0.17  1.00  0.96  0.96 0.96
# x      1.00 -0.02  0.20  0.96  1.00  1.00 0.99
# y      1.00 -0.03  0.20  0.96  1.00  1.00 0.99
# z      0.99  0.10  0.16  0.96  0.99  0.99 1.00

library(reshape2)
melted <- melt(cormat)
ggplot(data=melted, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

# kutijasti dijagrami
for(i in 1:7){
  boxplot(numerical[,i], main = names(numerical)[i])
  readline(prompt = "[enter]")
}
# svi zajedno
boxplot(numerical)
# cijena ima najvecu varijabilnost u vrijednostima

# kovarijanca

cov(numerical, method="spearman")
#          carat     depth     table     price         x         y         z
# carat 242339584   7294553  46793232 233404848 241459057 241326989 240744552
# depth   7294553 242287949 -58805838   2428531  -5681796  -6162426  25085006
# table  46793232 -58805838 237662225  41237114  48545402  46985908  38378172
# price 233404848   2428531  41237114 242464705 233539381 233423661 232089991
# x     241459057  -5681796  48545402 233539381 242461218 241950825 239391886
# y     241326989  -6162426  46985908 233423661 241950825 242461229 239322111
# z     240744552  25085006  38378172 232089991 239391886 239322111 242454156

# train test split

library(healthcareai)
split <- split_train_test(diamonds, price, percent_train = 0.8)

dim(split$train)
# [1] 43152    10
dim(split$test)
# [1] 10788    10

train = split$train
test = split$test

# kodiranje kategorickih varijabli
# one-hot

kodiraj <- function(podaci){
  coded <- cbind(podaci, 
                       model.matrix( ~ 0 + podaci$cut,
                                     data = podaci))
  coded <- cbind(coded, 
                       model.matrix( ~ 0 + podaci$color,
                                     data = podaci))
  coded <- cbind(coded, 
                       model.matrix( ~ 0 + podaci$clarity,
                                     data = podaci))
  
  coded <- coded[,-(2:4)] # uklonili stare kategoricke varijable
  
  # promjena imena
  for (i in 1:20){
    duljina <- nchar(names(coded)[8:27][i])
    names(coded)[8:27][i] <- substr(names(coded)[8:27][i], 8, duljina)
  }
  names(coded)[10] <- "cutVery" # izbjegavamo razmake u imenu
  return(coded)
}
train_coded <- kodiraj(train)
test_coded <- kodiraj(test)

head(train_coded)
head(test_coded)

library(cleandata)
kodiraj_ordinalno <- function(podaci, poredak){
  return(encode_ordinal(data.frame(podaci), out.int=T, 
         order=poredak))
}

train_coded_ord <- data.frame(train)
test_coded_ord <- data.frame(test)

train_coded_ord[,2] <- kodiraj_ordinalno(train_coded_ord[,2],c("Fair","Good","Very Good","Premium","Ideal"))
train_coded_ord[,3] <- kodiraj_ordinalno(train_coded_ord[,3],c("D","E","F","G","H","I","J"))
train_coded_ord[,4] <- kodiraj_ordinalno(train_coded_ord[,4],c("I1","SI2","SI1","VS2","VS1","VVS2","VVS1","IF"))
test_coded_ord[,2] <- kodiraj_ordinalno(test_coded_ord[,2],c("Fair","Good","Very Good","Premium","Ideal"))
test_coded_ord[,3] <-kodiraj_ordinalno(test_coded_ord[,3],c("D","E","F","G","H","I","J"))
test_coded_ord[,4] <- kodiraj_ordinalno(test_coded_ord[,4],c("I1","SI2","SI1","VS2","VS1","VVS2","VVS1","IF"))

# standardizacija
library(caret)
preProcValues <- preProcess(train_coded, method = c("center", "scale"))
train_standardized <- predict(preProcValues, train_coded)
test_standardized <- predict(preProcValues, test_coded)

head(train_standardized)
# carat      depth      table      price         x         y         z    cutFair
# 1 -1.197162 -0.1776173 -1.1001572 -0.9041800 -1.585529 -1.524961 -1.590814 -0.1762416
# 2 -1.239315 -1.3685035  1.5864336 -0.9041800 -1.638961 -1.646663 -1.763122 -0.1762416
# 3 -1.197162 -3.4000152  3.3774941 -0.9039293 -1.496477 -1.446724 -1.763122 -0.1762416
# 4 -1.028553  1.0833209  0.2431382 -0.9019240 -1.238225 -1.203320 -1.131327 -0.1762416
# 5 -1.176086  0.7330603 -0.2046270 -0.9016733 -1.594434 -1.542347 -1.519019 -0.1762416
# 6 -1.176086  0.3827997 -0.2046270 -0.9016733 -1.585529 -1.524961 -1.533378 -0.1762416
# cutGood    cutVery cutPremium   cutIdeal     colorD     colorE     colorF     colorG
# 1 -0.3160064 -0.5370837 -0.5874769  1.2282737 -0.3801192  2.1264017 -0.4660734 -0.5129112
# 2 -0.3160064 -0.5370837  1.7021550 -0.8141319 -0.3801192  2.1264017 -0.4660734 -0.5129112
# 3  3.1644195 -0.5370837 -0.5874769 -0.8141319 -0.3801192  2.1264017 -0.4660734 -0.5129112
# 4  3.1644195 -0.5370837 -0.5874769 -0.8141319 -0.3801192 -0.4702671 -0.4660734 -0.5129112
# 5 -0.3160064  1.8618641 -0.5874769 -0.8141319 -0.3801192 -0.4702671 -0.4660734 -0.5129112
# 6 -0.3160064  1.8618641 -0.5874769 -0.8141319 -0.3801192 -0.4702671 -0.4660734 -0.5129112
# colorH     colorI     colorJ  clarityI1 claritySI2 claritySI1 clarityVS2 clarityVS1
# 1 -0.4275053 -0.3328486 -0.2333262 -0.1186433  2.1996227 -0.5668533 -0.5386235 -0.4242018
# 2 -0.4275053 -0.3328486 -0.2333262 -0.1186433 -0.4546129  1.7640839 -0.5386235 -0.4242018
# 3 -0.4275053 -0.3328486 -0.2333262 -0.1186433 -0.4546129 -0.5668533 -0.5386235  2.3573140
# 4 -0.4275053 -0.3328486  4.2857458 -0.1186433  2.1996227 -0.5668533 -0.5386235 -0.4242018
# 5 -0.4275053 -0.3328486  4.2857458 -0.1186433 -0.4546129 -0.5668533 -0.5386235 -0.4242018
# 6 -0.4275053  3.0042991 -0.2333262 -0.1186433 -0.4546129 -0.5668533 -0.5386235 -0.4242018
# clarityVVS2 clarityVVS1  clarityIF
# 1  -0.3217397   -0.269748 -0.1836535
# 2  -0.3217397   -0.269748 -0.1836535
# 3  -0.3217397   -0.269748 -0.1836535
# 4  -0.3217397   -0.269748 -0.1836535
# 5   3.1080308   -0.269748 -0.1836535
# 6  -0.3217397    3.707077 -0.1836535

head(test_standardized)
# carat      depth      table      price         x         y         z    cutFair
# 1 -1.070705  0.4528518  0.2431382 -0.9021746 -1.362898 -1.307636 -1.303635 -0.1762416
# 2 -1.197162  0.7330603 -0.6523921 -0.9006707 -1.603340 -1.594505 -1.547737 -0.1762416
# 3 -1.028553  0.3127476 -1.5479224 -0.8996680 -1.229319 -1.185934 -1.188763 -0.1762416
# 4 -1.197162  1.4335816 -1.1001572 -0.8976627 -1.674582 -1.577119 -1.519019 -0.1762416
# 5 -1.049629  0.3127476 -0.2046270 -0.8964093 -1.291656 -1.246785 -1.246199 -0.1762416
# 6 -1.197162 -1.3685035 -0.2046270 -0.8851294 -1.505382 -1.455417 -1.605173 -0.1762416
# cutGood    cutVery cutPremium   cutIdeal     colorD     colorE     colorF     colorG
# 1 -0.3160064 -0.5370837  1.7021550 -0.8141319 -0.3801192 -0.4702671 -0.4660734 -0.5129112
# 2 -0.3160064 -0.5370837 -0.5874769  1.2282737 -0.3801192 -0.4702671 -0.4660734 -0.5129112
# 3 -0.3160064 -0.5370837 -0.5874769  1.2282737 -0.3801192 -0.4702671 -0.4660734 -0.5129112
# 4 -0.3160064  1.8618641 -0.5874769 -0.8141319 -0.3801192  2.1264017 -0.4660734 -0.5129112
# 5 -0.3160064  1.8618641 -0.5874769 -0.8141319 -0.3801192 -0.4702671 -0.4660734 -0.5129112
# 6 -0.3160064  1.8618641 -0.5874769 -0.8141319 -0.3801192 -0.4702671  2.1455352 -0.5129112
# colorH     colorI     colorJ  clarityI1 claritySI2 claritySI1 clarityVS2 clarityVS1
# 1 -0.4275053  3.0042991 -0.2333262 -0.1186433 -0.4546129 -0.5668533  1.8565415 -0.4242018
# 2 -0.4275053 -0.3328486  4.2857458 -0.1186433 -0.4546129 -0.5668533 -0.5386235  2.3573140
# 3 -0.4275053 -0.3328486  4.2857458 -0.1186433  2.1996227 -0.5668533 -0.5386235 -0.4242018
# 4 -0.4275053 -0.3328486 -0.2333262 -0.1186433 -0.4546129 -0.5668533  1.8565415 -0.4242018
# 5 -0.4275053 -0.3328486  4.2857458 -0.1186433 -0.4546129 -0.5668533  1.8565415 -0.4242018
# 6 -0.4275053 -0.3328486 -0.2333262 -0.1186433 -0.4546129 -0.5668533 -0.5386235  2.3573140
# clarityVVS2 clarityVVS1  clarityIF
# 1  -0.3217397   -0.269748 -0.1836535
# 2  -0.3217397   -0.269748 -0.1836535
# 3  -0.3217397   -0.269748 -0.1836535
# 4  -0.3217397   -0.269748 -0.1836535
# 5  -0.3217397   -0.269748 -0.1836535
# 6  -0.3217397   -0.269748 -0.1836535

preProcValues_ord <- preProcess(train_coded_ord, method = c("center", "scale"))
train_standardized_ord <- predict(preProcValues_ord, train_coded_ord)
test_standardized_ord <- predict(preProcValues_ord, test_coded_ord)

head(train_standardized_ord)
# carat        cut      color     clarity      depth      table      price
# 1 -1.196842  0.9812227 -0.9347173 -1.24186303 -0.1740390 -1.0956964 -0.9033038
# 2 -1.196842 -1.7032772 -0.9347173  0.57378574 -3.3698990  3.3655160 -0.9030534
# 3 -1.070453  0.0863894  1.4176876 -0.03143052  0.4512379  0.2426673 -0.9013008
# 4 -1.028323 -1.7032772  2.0057888 -1.24186303  1.0765149  0.2426673 -0.9010504
# 5 -1.175777 -0.8084439  2.0057888  1.17900200  0.7291388 -0.2034539 -0.9008001
# 6 -1.133648 -0.8084439  0.8295863 -0.63664678  0.1038618 -1.0956964 -0.9005497
# x         y         z
# 1 -1.585929 -1.525303 -1.561681
# 2 -1.496886 -1.447073 -1.730708
# 3 -1.363320 -1.307997 -1.279970
# 4 -1.238659 -1.203690 -1.110943
# 5 -1.594834 -1.542688 -1.491253
# 6 -1.479077 -1.412304 -1.420825

head(test_standardized_ord)
# carat        cut      color    clarity      depth      table      price
# 1 -1.238972  0.0863894 -0.9347173 -0.6366468 -1.3551177  1.5810310 -0.9033038
# 2 -1.175777 -0.8084439  1.4176876  1.7842183  0.3817627 -0.2034539 -0.9008001
# 3 -1.049388 -1.7032772  2.0057888 -0.6366468  1.5628414 -1.0956964 -0.9000489
# 4 -1.007258  0.0863894 -0.9347173 -1.8470793 -0.5908903  0.2426673 -0.8985467
# 5 -1.049388 -1.7032772  1.4176876 -1.2418630  1.0765149 -0.6495751 -0.8970444
# 6 -1.028323 -0.8084439  2.0057888 -0.6366468 -2.5361964  2.0271523 -0.8965437
# x         y         z
# 1 -1.639356 -1.646995 -1.730708
# 2 -1.585929 -1.525303 -1.505339
# 3 -1.318799 -1.264535 -1.139114
# 4 -1.203042 -1.142844 -1.209542
# 5 -1.309894 -1.247151 -1.167285
# 6 -1.149616 -1.099383 -1.336312

# linear regr
model_lm <- lm(price ~ ., data=train_standardized)
summary(model_lm)
# 
# Call:
#   lm(formula = price ~ ., data = train_standardized)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.3303 -0.1488 -0.0460  0.0957  2.6975 
# 
# Coefficients: (3 not defined because of singularities)
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.369e-14  1.365e-03   0.000    1.000    
# carat        1.329e+00  6.438e-03 206.405  < 2e-16 ***
#   depth       -2.395e-02  1.796e-03 -13.337  < 2e-16 ***
#   table       -1.439e-02  1.823e-03  -7.893 3.02e-15 ***
#   x           -2.745e-01  1.010e-02 -27.184  < 2e-16 ***
#   y           -3.976e-04  6.230e-03  -0.064    0.949    
# z           -5.613e-03  6.095e-03  -0.921    0.357    
# cutFair     -3.372e-02  1.598e-03 -21.105  < 2e-16 ***
#   cutGood     -1.846e-02  1.632e-03 -11.305  < 2e-16 ***
#   cutVery     -1.079e-02  1.664e-03  -6.480 9.29e-11 ***
#   cutPremium  -7.363e-03  1.782e-03  -4.131 3.62e-05 ***
#   cutIdeal            NA         NA      NA       NA    
# colorD       1.962e-01  2.443e-03  80.321  < 2e-16 ***
#   colorE       2.092e-01  2.715e-03  77.052  < 2e-16 ***
#   colorF       2.007e-01  2.671e-03  75.145  < 2e-16 ***
#   colorG       1.924e-01  2.795e-03  68.841  < 2e-16 ***
#   colorH       1.252e-01  2.531e-03  49.484  < 2e-16 ***
#   colorI       6.734e-02  2.241e-03  30.050  < 2e-16 ***
#   colorJ              NA         NA      NA       NA    
# clarityI1   -1.585e-01  1.670e-03 -94.933  < 2e-16 ***
#   claritySI2  -2.465e-01  3.207e-03 -76.852  < 2e-16 ***
#   claritySI1  -1.761e-01  3.505e-03 -50.247  < 2e-16 ***
#   clarityVS2  -1.088e-01  3.402e-03 -31.970  < 2e-16 ***
#   clarityVS1  -6.594e-02  3.002e-03 -21.963  < 2e-16 ***
#   clarityVVS2 -2.478e-02  2.541e-03  -9.754  < 2e-16 ***
#   clarityVVS1 -1.802e-02  2.290e-03  -7.870 3.62e-15 ***
#   clarityIF           NA         NA      NA       NA    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.2835 on 43128 degrees of freedom
# Multiple R-squared:  0.9196,	Adjusted R-squared:  0.9196 
# F-statistic: 2.146e+04 on 23 and 43128 DF,  p-value: < 2.2e-16

plot(model_lm$residuals ~ train_standardized$price)
hist(model_lm$residuals)
qqnorm(model_lm$residuals, main="model_lm reziduali")
qqline(model_lm$residuals)

ad.test(residuals(model_lm))
# 
# Anderson-Darling normality test
# 
# data:  residuals(model_lm)
# A = 1499.5, p-value < 2.2e-16

# zakljucujemo da reziduali nisu normalno distribuirani...

model_lm_ord <- lm(price ~ ., data=train_standardized_ord)
summary(model_lm_ord)
# Call:
#   lm(formula = price ~ ., data = train_standardized_ord)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.9154 -0.1575 -0.0318  0.1245  2.4812 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  6.684e-15  1.466e-03   0.000    1.000    
# carat        1.281e+00  6.824e-03 187.713  < 2e-16 ***
#   cut          3.416e-02  1.782e-03  19.164  < 2e-16 ***
#   color       -1.375e-01  1.552e-03 -88.566  < 2e-16 ***
#   clarity      2.076e-01  1.626e-03 127.736  < 2e-16 ***
#   depth       -2.960e-02  1.882e-03 -15.726  < 2e-16 ***
#   table       -1.445e-02  1.836e-03  -7.871 3.61e-15 ***
#   x           -2.470e-01  1.037e-02 -23.830  < 2e-16 ***
#   y            8.154e-03  5.998e-03   1.359    0.174    
# z           -4.234e-03  6.455e-03  -0.656    0.512    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.3045 on 43142 degrees of freedom
# Multiple R-squared:  0.9073,	Adjusted R-squared:  0.9073 
# F-statistic: 4.693e+04 on 9 and 43142 DF,  p-value: < 2.2e-16

# vidimo da kodiranje kategorickih varijabli ordinalno nije napravilo
# poboljsanje

plot(residuals(model_lm_ord), main="Reziduali model_lm_ord")
curve(rnorm(x,mean(residuals(model_lm_ord)),sd(residuals(model_lm_ord))),add=T, 
      col="yellow")
# ...sto bolje ilustrira slika

model_lm_carat <- lm(price ~ carat, data=train_standardized)
summary(model_lm_carat)
# 
# Call:
#   lm(formula = price ~ carat, data = train_standardized)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.6621 -0.2017 -0.0046  0.1342  3.2004 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.380e-14  1.873e-03     0.0        1    
# carat        9.212e-01  1.873e-03   491.9   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.389 on 43150 degrees of freedom
# Multiple R-squared:  0.8487,	Adjusted R-squared:  0.8487 
# F-statistic: 2.42e+05 on 1 and 43150 DF,  p-value: < 2.2e-16

plot(train_standardized$price~train_standardized$carat, col="red")
abline(model_lm_carat)

plot(model_lm_carat$residuals ~ train_standardized$price)
hist(model_lm_carat$residuals)
qqnorm(model_lm_carat$residuals)
qqline(model_lm_carat$residuals)

model_lm_depth <- lm(price ~ depth, data=train_standardized)
summary(model_lm_depth)
# 
# Call:
#   lm(formula = price ~ depth, data = train_standardized)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.9411 -0.7483 -0.3806  0.3513  3.7501 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -5.161e-15  4.814e-03   0.000   1.0000  
# depth       -1.000e-02  4.814e-03  -2.078   0.0378 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1 on 43150 degrees of freedom
# Multiple R-squared:  0.0001,	Adjusted R-squared:  7.684e-05 
# F-statistic: 4.316 on 1 and 43150 DF,  p-value: 0.03776

# ovi rezultati su losi. zakljucujemo da cijena sigurno ne ovisi
# (barem ne linearno) o parametru depth.
plot(train_standardized$price ~ train_standardized$depth, col="red")
abline(model_lm_depth)

plot(model_lm_depth$residuals ~ train_standardized$price)
hist(model_lm_depth$residuals)
qqnorm(model_lm_depth$residuals)
qqline(model_lm_depth$residuals)

# slika potvrduje zakljucak

model_lm_sve_osim_depth <- lm(price ~ ., data=train_standardized[,-2])
summary(model_lm_sve_osim_depth)
# 
# Call:
#   lm(formula = price ~ ., data = train_standardized[, -2])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.2976 -0.1491 -0.0456  0.0951  2.6841 
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.784e-14  1.362e-03   0.000 1.000000    
# carat        1.324e+00  6.273e-03 211.004  < 2e-16 ***
#   table       -6.415e-03  1.693e-03  -3.790 0.000151 ***
#   x           -2.425e-01  9.152e-03 -26.494  < 2e-16 ***
#   y            5.981e-03  5.570e-03   1.074 0.282923    
# z           -3.897e-02  5.511e-03  -7.070 1.57e-12 ***
#   cutFair     -4.383e-02  1.499e-03 -29.230  < 2e-16 ***
#   cutGood     -2.388e-02  1.573e-03 -15.184  < 2e-16 ***
#   cutVery     -1.378e-02  1.640e-03  -8.401  < 2e-16 ***
#   cutPremium  -8.964e-03  1.772e-03  -5.059 4.23e-07 ***
#   cutIdeal            NA         NA      NA       NA    
# colorD       1.972e-01  2.425e-03  81.341  < 2e-16 ***
#   colorE       2.105e-01  2.688e-03  78.311  < 2e-16 ***
#   colorF       2.016e-01  2.653e-03  76.006  < 2e-16 ***
#   colorG       1.925e-01  2.769e-03  69.531  < 2e-16 ***
#   colorH       1.260e-01  2.508e-03  50.240  < 2e-16 ***
#   colorI       6.816e-02  2.211e-03  30.821  < 2e-16 ***
#   colorJ              NA         NA      NA       NA    
# clarityI1   -1.581e-01  1.663e-03 -95.083  < 2e-16 ***
#   claritySI2  -2.517e-01  3.197e-03 -78.746  < 2e-16 ***
#   claritySI1  -1.821e-01  3.485e-03 -52.261  < 2e-16 ***
#   clarityVS2  -1.146e-01  3.392e-03 -33.776  < 2e-16 ***
#   clarityVS1  -7.027e-02  2.972e-03 -23.642  < 2e-16 ***
#   clarityVVS2 -2.916e-02  2.531e-03 -11.520  < 2e-16 ***
#   clarityVVS1 -2.157e-02  2.291e-03  -9.417  < 2e-16 ***
#   clarityIF           NA         NA      NA       NA    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.283 on 43129 degrees of freedom
# Multiple R-squared:   0.92,	Adjusted R-squared:  0.9199 
# F-statistic: 2.254e+04 on 22 and 43129 DF,  p-value: < 2.2e-16

plot(train_standardized$price ~ train_standardized$x + train_standardized$y +
       train_standardized$z, col="red")
# iz slike vidimo da je mozda najbolje probati s x.
model_lm_x <- lm(price ~ x, data=train_standardized)
summary(model_lm_x)
# 
# Call:
#   lm(formula = price ~ x, data = train_standardized)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.1128 -0.3177 -0.0462  0.2434  8.0667 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -6.974e-15  2.247e-03     0.0        1    
# x            8.844e-01  2.247e-03   393.5   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.4668 on 43150 degrees of freedom
# Multiple R-squared:  0.7821,	Adjusted R-squared:  0.7821 
# F-statistic: 1.549e+05 on 1 and 43150 DF,  p-value: < 2.2e-16

plot(train_standardized$price ~ train_standardized$x, col="red")
abline(model_lm_x)

plot(model_lm_x$residuals ~ train_standardized$price)
hist(model_lm_x$residuals)
qqnorm(model_lm_x$residuals)
qqline(model_lm_x$residuals)
# model je OK, ali imali smo bolje

# probajmo cisto zbog usporedbe s y i z:
model_lm_y <- lm(price ~ y, data=train_standardized)
summary(model_lm_y)
# 
# Call:
#   lm(formula = price ~ y, data = train_standardized)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -38.251  -0.309  -0.061   0.209   7.894 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -4.360e-15  2.409e-03     0.0        1    
# y            8.658e-01  2.409e-03   359.4   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5004 on 43150 degrees of freedom
# Multiple R-squared:  0.7496,	Adjusted R-squared:  0.7496 
# F-statistic: 1.292e+05 on 1 and 43150 DF,  p-value: < 2.2e-16

plot(train_standardized$price ~ train_standardized$y, col="red")
abline(model_lm_y)

plot(model_lm_y$residuals ~ train_standardized$price)
hist(model_lm_y$residuals)
qqnorm(model_lm_y$residuals)
qqline(model_lm_y$residuals)

model_lm_z <- lm(price ~ z, data=train_standardized)
summary(model_lm_z)
# 
# Call:
#   lm(formula = price ~ z, data = train_standardized)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -34.727  -0.308  -0.062   0.200   8.021 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -9.073e-15  2.476e-03     0.0        1    
# z            8.576e-01  2.476e-03   346.4   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5143 on 43150 degrees of freedom
# Multiple R-squared:  0.7355,	Adjusted R-squared:  0.7355 
# F-statistic: 1.2e+05 on 1 and 43150 DF,  p-value: < 2.2e-16

plot(train_standardized$price ~ train_standardized$z, col="red")
abline(model_lm_z)

plot(model_lm_z$residuals ~ train_standardized$price)
hist(model_lm_z$residuals)
qqnorm(model_lm_z$residuals)
qqline(model_lm_z$residuals)

# vidimo da su i y i z isto OK.
# na grafovima vidimo outliere. kad bismo ih uklonili, rezultati bi
# vjerojatno bili jos bolji.

model_lm_table <- lm(price ~ table, data=train_standardized)
summary(model_lm_table)
# 
# Call:
#   lm(formula = price ~ table, data = train_standardized)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.4370 -0.6906 -0.3737  0.3462  3.9556 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -5.447e-15  4.776e-03    0.00        1    
# table        1.262e-01  4.776e-03   26.42   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.992 on 43150 degrees of freedom
# Multiple R-squared:  0.01592,	Adjusted R-squared:  0.0159 
# F-statistic: 698.2 on 1 and 43150 DF,  p-value: < 2.2e-16

plot(train_standardized$price ~ train_standardized$table, col="red")
abline(model_lm_table)

plot(model_lm_table$residuals ~ train_standardized$price)
hist(model_lm_table$residuals)
qqnorm(model_lm_table$residuals)
qqline(model_lm_table$residuals)

model_lm_cutIdeal <- lm(price ~ cutIdeal, data=train_standardized)
summary(model_lm_cutIdeal)
# 
# Call:
#   lm(formula = price ~ cutIdeal, data = train_standardized)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.9832 -0.6887 -0.3753  0.3447  3.8600 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -5.172e-15  4.791e-03    0.00        1    
# cutIdeal    -9.729e-02  4.791e-03  -20.31   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9953 on 43150 degrees of freedom
# Multiple R-squared:  0.009466,	Adjusted R-squared:  0.009443 
# F-statistic: 412.4 on 1 and 43150 DF,  p-value: < 2.2e-16

plot(train_standardized$price ~ train_standardized$cutIdeal, col="red")
abline(model_lm_cutIdeal)

plot(model_lm_cutIdeal$residuals ~ train_standardized$price)
hist(model_lm_cutIdeal$residuals)
qqnorm(model_lm_cutIdeal$residuals)
qqline(model_lm_cutIdeal$residuals)
# iz slika se posebno dobro vidi kako one-hot kodirane kategoricke varijable
# nece davati dobre rezultate, sto je i ocekivano

# za ovakav tip regresije potrebno je drugacije kodirati kategoricke varijable
# probajmo s drugom vrstom kodiranja:
model_lm_cutIdeal_ord <- lm(price ~ cut, data=train_standardized_ord)
summary(model_lm_cutIdeal_ord)
# 
# Call:
#   lm(formula = price ~ cut, data = train_standardized_ord)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.0434 -0.7237 -0.3852  0.3477  3.7776 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.292e-15  4.807e-03    0.00        1    
# cut         -5.496e-02  4.807e-03  -11.44   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9985 on 43150 degrees of freedom
# Multiple R-squared:  0.003021,	Adjusted R-squared:  0.002998 
# F-statistic: 130.8 on 1 and 43150 DF,  p-value: < 2.2e-16

plot(train_standardized_ord$price ~ train_standardized_ord$cut, col="red")
abline(model_lm_cutIdeal_ord)

plot(model_lm_cutIdeal_ord$residuals ~ train_standardized_ord$price)
hist(model_lm_cutIdeal_ord$residuals)
qqnorm(model_lm_cutIdeal_ord$residuals)
qqline(model_lm_cutIdeal_ord$residuals)

# zakljucujemo kako problem nije u vrsti kodiranja kategoricke varijable
# nego u samoj cinjenici da je varijabla kategoricka, sto je i ocekivan ishod
# intuitivno, graficki se najbolje vidi zasto se ne moze provuci smisleni
# regresijski pravac kroz tocke organizirane u stroge segmente

# iz ovog razloga probajmo izbaciti kategoricke varijable iz analize:
model_lm_bez_kat <- lm(price ~ carat+depth+table+x+y+z, data=train_standardized_ord)
summary(model_lm_bez_kat)
# Call:
#   lm(formula = price ~ carat + depth + table + x + y + z, data = train_standardized_ord)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.9886 -0.1530 -0.0116  0.0871  3.1967 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  8.986e-15  1.804e-03   0.000   1.0000    
# carat        1.272e+00  8.322e-03 152.895   <2e-16 ***
#   depth       -7.283e-02  2.158e-03 -33.748   <2e-16 ***
#   table       -5.630e-02  1.925e-03 -29.246   <2e-16 ***
#   x           -3.664e-01  1.267e-02 -28.909   <2e-16 ***
#   y            1.343e-02  7.380e-03   1.820   0.0688 .  
# z            6.648e-03  7.942e-03   0.837   0.4026    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.3747 on 43145 degrees of freedom
# Multiple R-squared:  0.8596,	Adjusted R-squared:  0.8596 
# F-statistic: 4.403e+04 on 6 and 43145 DF,  p-value: < 2.2e-16

# probajmo izbaciti varijable table i depth:
model_lm_cxyz <- lm(price ~ carat+x+y+z, data=train_standardized_ord)
summary(model_lm_cxyz)
# Call:
#   lm(formula = price ~ carat + x + y + z, data = train_standardized_ord)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.8684 -0.1612 -0.0036  0.0899  3.6299 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  7.554e-15  1.837e-03    0.00        1    
# carat        1.217e+00  8.281e-03  146.92  < 2e-16 ***
#   x           -2.536e-01  1.201e-02  -21.13  < 2e-16 ***
#   y            3.779e-02  7.484e-03    5.05 4.44e-07 ***
#   z           -8.797e-02  7.202e-03  -12.21  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.3815 on 43147 degrees of freedom
# Multiple R-squared:  0.8544,	Adjusted R-squared:  0.8544 
# F-statistic: 6.332e+04 on 4 and 43147 DF,  p-value: < 2.2e-16

# vidimo da model nije bolji od prethodnog
# iz svega ovoga zakljucujemo da je najbolja regresija provedena modelom
# koji sadrzi sve varijable kao prediktore
# zato se nijedna varijabla nije izbacila iz daljnje analize

# provodimo i odabir znacajki ugradenim R metodama zbog usporedbe:

# vaznost varijabli
vaznost <- varImp(model_lm_ord)
print(vaznost)
#         Overall
# carat   187.7132624
# cut      19.1635859
# color    88.5659175
# clarity 127.7361908
# depth    15.7264447
# table     7.8707792
# x        23.8298915
# y         1.3593617
# z         0.6558866

varImp(model_lm)
#             Overall
# carat       209.7237150
# depth        13.1381718
# table         8.2638750
# x            29.3835815
# y             0.0091024
# z             1.2898033
# cutFair      23.1747592
# cutGood      11.4255510
# cutVery       6.2712624
# cutPremium    3.9260719
# colorD       81.3284453
# colorE       78.2199865
# colorF       75.9853478
# colorG       69.6857838
# colorH       50.3763804
# colorI       30.9020590
# clarityI1    94.3519493
# claritySI2   78.0960445
# claritySI1   51.3616152
# clarityVS2   33.1188761
# clarityVS1   23.1873855
# clarityVVS2  11.1495658
# clarityVVS1   9.1843949

# trazenje vaznih znacajki pomocu slucajne sume:
library(randomForest)
fit_rf = randomForest(price ~ ., data=train_standardized_ord)
# za stvaranje poretka znacajnosti se koristi gini indeks necistoce
importance(fit_rf)
#         IncNodePurity
# carat      12869.5472
# cut          135.8091
# color       1332.9136
# clarity     2423.5190
# depth        232.7244
# table        175.8075
# x           8298.3771
# y          11908.1023
# z           5647.1423

varImpPlot(fit_rf)

# neuralnet (one-hot zbog neural net)
library(neuralnet)

# dugo se trenira
model_1 <- neuralnet(price ~ carat + depth + table + x + y + z +
                       cutFair + cutGood + cutVery + 
                       cutPremium + cutIdeal + colorD + colorE + colorF +
                       colorG + colorH + colorI + colorJ + clarityI1 +
                       claritySI2 + claritySI1 + clarityVS2 + clarityVS1 +
                       clarityVVS2 + clarityVVS1 + clarityIF,
                     data = train_standardized, hidden=c(5,2),
                     linear.output=T)
# Warning message:
#   Algorithm did not converge in 1 of 1 repetition(s) within the stepmax.
# Error in plot.nn(model_1) : weights were not calculated

# treniranje mreze nije proslo
# probajmo s drugacije kodiranim kategorickim varijablama. time smo dobili
# i manji broj znacajki koje se koriste u modelu jer se u ordinalnom kodiranju
# provodi preslikavanje jedan-na-jedan, a ne jedan-na-vise stupaca

model_1_1 <- neuralnet(price ~ carat + depth + table + x + y + z +
                         cut + color + clarity,
                       data = train_standardized_ord, hidden=c(5,2))
# Warning message:
#   Algorithm did not converge in 1 of 1 repetition(s) within the stepmax.

# nnet
library(nnet)

model_nnet <- nnet(price/18823 ~ ., data=train,
                   size=5, rang=0.1, decay=5e-4, maxit=1000)
# weights:  126
# initial  value 5514.767972 
# iter  10 value 3831.570201
# iter  20 value 3822.900389
# iter  30 value 1939.015989
# iter  40 value 1927.419750
# iter  50 value 1035.122647
# iter  60 value 494.836564
# iter  70 value 216.481353
# iter  80 value 138.789682
# iter  90 value 85.831965
# iter 100 value 77.434921
# iter 110 value 74.336271
# iter 120 value 73.010998
# iter 130 value 72.032263
# iter 140 value 71.487818
# iter 150 value 70.997382
# iter 160 value 69.810159
# iter 170 value 68.956505
# iter 180 value 68.718492
# iter 190 value 68.457365
# iter 200 value 62.480852
# iter 210 value 56.488999
# iter 220 value 53.868598
# iter 230 value 52.813461
# iter 240 value 51.332219
# iter 250 value 49.510327
# iter 260 value 48.751259
# iter 270 value 48.041125
# iter 280 value 47.871886
# iter 290 value 47.009327
# iter 300 value 45.597908
# iter 310 value 44.380297
# iter 320 value 43.338817
# iter 330 value 42.854790
# iter 340 value 42.619271
# iter 350 value 42.473921
# iter 360 value 42.368948
# iter 370 value 42.202093
# iter 380 value 42.074248
# iter 390 value 41.993503
# iter 400 value 41.923443
# iter 410 value 41.856416
# iter 420 value 41.829111
# iter 430 value 41.821479
# iter 440 value 41.813671
# iter 450 value 41.806323
# iter 460 value 41.800561
# iter 470 value 41.789782
# iter 480 value 41.760774
# iter 490 value 41.694211
# iter 500 value 41.605213
# iter 510 value 41.515767
# iter 520 value 41.475299
# iter 530 value 41.436737
# iter 540 value 41.314101
# iter 550 value 40.980648
# iter 560 value 40.673737
# iter 570 value 40.432313
# iter 580 value 40.326963
# iter 590 value 40.265364
# iter 600 value 40.237419
# iter 610 value 40.198326
# iter 620 value 40.137616
# iter 630 value 39.941888
# iter 640 value 39.783012
# iter 650 value 39.587572
# iter 660 value 39.448873
# iter 670 value 39.365117
# iter 680 value 39.341435
# iter 690 value 39.241545
# iter 700 value 39.114949
# iter 710 value 39.030667
# iter 720 value 38.970656
# iter 730 value 38.897479
# iter 740 value 38.726636
# iter 750 value 38.604153
# iter 760 value 38.494830
# iter 770 value 38.440252
# iter 780 value 38.406330
# iter 790 value 38.374948
# iter 800 value 38.362563
# iter 810 value 38.352391
# iter 820 value 38.347667
# iter 830 value 38.345530
# iter 840 value 38.344713
# iter 850 value 38.343897
# iter 860 value 38.343359
# iter 870 value 38.342762
# iter 880 value 38.341572
# iter 890 value 38.332819
# iter 900 value 38.241479
# iter 910 value 38.123660
# iter 920 value 38.035952
# iter 930 value 37.998075
# iter 940 value 37.994754
# iter 950 value 37.991853
# iter 960 value 37.987382
# iter 970 value 37.981015
# iter 980 value 37.970429
# iter 990 value 37.955595
# iter1000 value 37.931580
# final  value 37.931580 
# stopped after 1000 iterations

predicted_nnet <- predict(model_nnet)*18823

mean((predicted_nnet - train$price)^2)
# [1] 339428.8
sqrt(mean((predicted_nnet - train$price)^2))
# [1] 582.6052

plot(train$price, predicted_nnet, main="prediction vs actual", xlab="actual")

model_nnet
# a 23-5-1 network with 126 weights

library(caret)

mygrid <- expand.grid(.decay=c(0.5, 0.1), .size=c(4,5,6))
nnetfit <- caret::train(train_standardized[,-4], as.numeric(train_standardized$price), 
                 method="nnet",
                 maxit=10, tuneGrid=mygrid, trace=T)
print(nnetfit)
# Neural Network 
# 
# 43152 samples
# 26 predictor
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 43152, 43152, 43152, 43152, 43152, 43152, ... 
# Resampling results across tuning parameters:
#   
#   decay  size  RMSE       Rsquared   MAE      
# 0.1    4     0.9569577  0.1795658  0.7495959
# 0.1    5     0.9774162  0.1894467  0.7528410
# 0.1    6     0.9842012  0.1284469  0.7632721
# 0.5    4     0.9464352  0.2771378  0.7504176
# 0.5    5     0.9377478  0.2904293  0.7499564
# 0.5    6     0.9483520  0.2309230  0.7560172
# 
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were size = 5 and decay = 0.5.

# vidimo da su rezultati prilicno losi. probajmo s drugim opcijama parametara.
mygrid <- expand.grid(.decay=c(0.5, 0.55, 0.6, 0.7), .size=c(5,8,10))
nnetfit <- caret::train(train_standardized[,-4], as.numeric(train_standardized$price), 
                        method="nnet",
                        maxit=10, tuneGrid=mygrid, trace=T)
print(nnetfit)
# Neural Network 
# 
# 43152 samples
# 26 predictor
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 43152, 43152, 43152, 43152, 43152, 43152, ... 
# Resampling results across tuning parameters:
#   
#   decay  size  RMSE       Rsquared   MAE      
# 0.50    5    0.9401667  0.2415991  0.7514371
# 0.50    8    0.9375709  0.3316593  0.7512005
# 0.50   10    0.9246537  0.4088948  0.7348918
# 0.55    5    0.9303431  0.2895997  0.7503190
# 0.55    8    0.8825048  0.4377542  0.7285253
# 0.55   10    0.8864466  0.4393302  0.7191751
# 0.60    5    0.9440614  0.2911949  0.7403949
# 0.60    8    0.9433828  0.2566675  0.7344857
# 0.60   10    0.9492487  0.2725225  0.7367964
# 0.70    5    0.9181762  0.3740114  0.7275885
# 0.70    8    0.9816514  0.1624759  0.7521667
# 0.70   10    0.9457260  0.2986010  0.7390233
# 
# RMSE was used to select the optimal model using the smallest value.
# The final values used for the model were size = 8 and decay = 0.55.

# vidimo da smo dobili nesto bolje rezultate sada.

# regresijsko stablo odluke
library(rpart)
library(rpart.plot)
stablo <- rpart(price ~ ., data=train_standardized_ord, 
                control=rpart.control(minsplit=10))

rpart.plot(stablo)

stablo$variable.importance
#   carat           y           x           z     clarity        
# 36258.17809 35443.97892 35124.44379 34127.07331  4559.12719 
# 
#   color       table         cut       depth
# 1643.83637    58.59962    42.28090    18.79831 

stablo$cptable
#           CP nsplit  rel error    xerror        xstd
# 1 0.60784715      0 1.00000000 1.0000265 0.009847608
# 2 0.18681490      1 0.39215285 0.3921776 0.004389817
# 3 0.03359112      2 0.20533795 0.2053892 0.002306157
# 4 0.02566435      3 0.17174683 0.1718600 0.002304532
# 5 0.02545006      4 0.14608248 0.1387149 0.001960198
# 6 0.01021766      5 0.12063242 0.1221254 0.001798044
# 7 0.01002529      6 0.11041476 0.1181678 0.001738577
# 8 0.01000000      8 0.09036417 0.1132136 0.001675692

# random forest
library(randomForest)
suma <- randomForest(price ~ ., data=train_standardized_ord)
print(suma)
# Call:
#   randomForest(formula = price ~ ., data = train_standardized_ord)
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 3
#
# Mean of squared residuals: 0.0187394
# % Var explained: 98.13

plot(suma)

# knn
model_knn <- knnreg(train_standardized[-4], as.numeric(unlist(train_standardized[4])))

str(model_knn)
# List of 3
# $ learn  :List of 2
# ..$ y: num [1:43152] -0.904 -0.904 -0.904 -0.902 -0.902 ...
# ..$ X:'data.frame':	43152 obs. of  26 variables:
#   .. ..$ carat      : num [1:43152] -1.2 -1.24 -1.2 -1.07 -1.03 ...
# .. ..$ depth      : num [1:43152] -0.175 -1.36 -3.382 0.453 1.08 ...
# .. ..$ table      : num [1:43152] -1.1 1.58 3.36 0.24 0.24 ...
# .. ..$ x          : num [1:43152] -1.59 -1.64 -1.5 -1.36 -1.24 ...
# .. ..$ y          : num [1:43152] -1.53 -1.65 -1.45 -1.31 -1.2 ...
# .. ..$ z          : num [1:43152] -1.59 -1.77 -1.77 -1.31 -1.13 ...
# .. ..$ cutFair    : num [1:43152] -0.174 -0.174 -0.174 -0.174 -0.174 ...
# .. ..$ cutGood    : num [1:43152] -0.314 -0.314 3.18 -0.314 3.18 ...
# .. ..$ cutVery    : num [1:43152] -0.536 -0.536 -0.536 -0.536 -0.536 ...
# .. ..$ cutPremium : num [1:43152] -0.588 1.702 -0.588 1.702 -0.588 ...
# .. ..$ cutIdeal   : num [1:43152] 1.224 -0.817 -0.817 -0.817 -0.817 ...
# .. ..$ colorD     : num [1:43152] -0.378 -0.378 -0.378 -0.378 -0.378 ...
# .. ..$ colorE     : num [1:43152] 2.122 2.122 2.122 -0.471 -0.471 ...
# .. ..$ colorF     : num [1:43152] -0.463 -0.463 -0.463 -0.463 -0.463 ...
# .. ..$ colorG     : num [1:43152] -0.514 -0.514 -0.514 -0.514 -0.514 ...
# .. ..$ colorH     : num [1:43152] -0.426 -0.426 -0.426 -0.426 -0.426 ...
# .. ..$ colorI     : num [1:43152] -0.337 -0.337 -0.337 2.964 -0.337 ...
# .. ..$ colorJ     : num [1:43152] -0.234 -0.234 -0.234 -0.234 4.27 ...
# .. ..$ clarityI1  : num [1:43152] -0.118 -0.118 -0.118 -0.118 -0.118 ...
# .. ..$ claritySI2 : num [1:43152] 2.206 -0.453 -0.453 -0.453 2.206 ...
# .. ..$ claritySI1 : num [1:43152] -0.563 1.776 -0.563 -0.563 -0.563 ...
# .. ..$ clarityVS2 : num [1:43152] -0.543 -0.543 -0.543 1.841 -0.543 ...
# .. ..$ clarityVS1 : num [1:43152] -0.425 -0.425 2.355 -0.425 -0.425 ...
# .. ..$ clarityVVS2: num [1:43152] -0.321 -0.321 -0.321 -0.321 -0.321 ...
# .. ..$ clarityVVS1: num [1:43152] -0.27 -0.27 -0.27 -0.27 -0.27 ...
# .. ..$ clarityIF  : num [1:43152] -0.185 -0.185 -0.185 -0.185 -0.185 ...
# $ k      : num 5
# $ theDots: list()
# - attr(*, "class")= chr "knnreg"

pred_y = predict(model_knn, test_standardized[,-4])

print(data.frame(test_standardized[,4], pred_y))

mse = mean((test_standardized[,4] - pred_y)^2)
mae = caret::MAE(test_standardized[,4], pred_y)
rmse = caret::RMSE(test_standardized[,4], pred_y)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)
# MSE:  0.04489283 MAE:  0.1059654  RMSE:  0.2118793

model_knn$k
# [1] 5

x = 1:length(test_standardized[,4])

plot(x, test_standardized[,4], col = "red", type = "l", lwd=2,
     main = "Diamonds pricing data prediction")
points(x, pred_y, col = "blue")
legend("topright",  legend = c("original-price", "predicted-price"), 
       fill = c("red", "blue"), col = 2:3,  adj = c(0, 0.6))

# ridge
lambda_seq <- 10^seq(2, -2, by = -.1)

train_X_ridge <- train_standardized[,-4]
train_y_ridge <- train_standardized[,4]

library(glmnet)
model_ridge <- glmnet(as.matrix(train_X_ridge), as.matrix(train_y_ridge), alpha = 0, lambda  = lambda_seq)

summary(model_ridge)
# Length Class     Mode   
# a0          41   -none-    numeric
# beta      1066   dgCMatrix S4     
# df          41   -none-    numeric
# dim          2   -none-    numeric
# lambda      41   -none-    numeric
# dev.ratio   41   -none-    numeric
# nulldev      1   -none-    numeric
# npasses      1   -none-    numeric
# jerr         1   -none-    numeric
# offset       1   -none-    logical
# call         5   -none-    call   
# nobs         1   -none-    numeric

# najbolji lambda:
ridge_cv <- cv.glmnet(as.matrix(train_X_ridge), train_y_ridge, alpha = 0)

plot(ridge_cv)

best_lambda <- ridge_cv$lambda.min
best_lambda
# [1] 0.09217505

best_fit <- ridge_cv$glmnet.fit
head(best_fit)
# $lambda
# [1] 921.64666700 839.77011734 765.16725469 697.19190473 635.25529751
# [6] 578.82096777 527.40010833 480.54733633 437.85683545 398.95884101
# [11] 363.51643718 331.22263882 301.79773250 274.98685376 250.55777959
# [16] 228.29891704 208.01747048 189.53777174 172.69975850 157.35758795
# [21] 143.37837355 130.64103402 119.03524463 108.46048158  98.82515134
# [26]  90.04579727  82.04637683  74.75760284  68.11634345  62.06507524
# [31]  56.55138502  51.52751584  46.94995336  42.77904890  38.97867611
# [36]  35.51591795  32.36078169  29.48593904  26.86648948  24.47974461
# [41]  22.30503157  20.32351404  18.51802907  16.87293840  15.37399306
# [46]  14.00820989  12.76375913  11.62986195  10.59669707   9.65531571
# [51]   8.79756407   8.01601273   7.30389225   6.65503460   6.06381966
# [56]   5.52512664   5.03428962   4.58705720   4.17955568   3.80825548
# [61]   3.46994056   3.16168060   2.88080561   2.62488278   2.39169543
# [66]   2.17922380   1.98562756   1.80922988   1.64850289   1.50205443
# [71]   1.36861606   1.24703198   1.13624910   1.03530786   0.94333396
# [76]   0.85953077   0.78317243   0.71359755   0.65020351   0.59244122
# [81]   0.53981038   0.49185511   0.44816006   0.40834675   0.37207035
# [86]   0.33901665   0.30889934   0.28145758   0.25645366   0.23367103
# [91]   0.21291234   0.19399779   0.17676356   0.16106037   0.14675221
# [96]   0.13371515   0.12183627   0.11101267   0.10115061   0.09216467
# 
# $dev.ratio
# [1] 6.478145e-36 7.580347e-03 8.313408e-03 9.116723e-03 9.996894e-03
# [6] 1.096112e-02 1.201724e-02 1.317379e-02 1.444005e-02 1.582611e-02
# [11] 1.734291e-02 1.900232e-02 2.081721e-02 2.280146e-02 2.497015e-02
# [16] 2.733938e-02 2.992662e-02 3.275056e-02 3.583125e-02 3.919013e-02
# [21] 4.285004e-02 4.683523e-02 5.117141e-02 5.588567e-02 6.100647e-02
# [26] 6.656352e-02 7.258770e-02 7.911088e-02 8.616566e-02 9.378519e-02
# [31] 1.020027e-01 1.108513e-01 1.203633e-01 1.305695e-01 1.414991e-01
# [36] 1.531783e-01 1.656302e-01 1.788731e-01 1.929203e-01 2.077788e-01
# [41] 2.234487e-01 2.399213e-01 2.571794e-01 2.751959e-01 2.939335e-01
# [46] 3.133440e-01 3.333683e-01 3.539366e-01 3.749691e-01 3.963765e-01
# [51] 4.180613e-01 4.399197e-01 4.618431e-01 4.837204e-01 5.054401e-01
# [56] 5.268931e-01 5.479743e-01 5.685858e-01 5.886379e-01 6.080511e-01
# [61] 6.267746e-01 6.447227e-01 6.618653e-01 6.781714e-01 6.936220e-01
# [66] 7.082095e-01 7.219365e-01 7.348144e-01 7.468624e-01 7.581062e-01
# [71] 7.685887e-01 7.783122e-01 7.873301e-01 7.956809e-01 8.034040e-01
# [76] 8.105391e-01 8.171254e-01 8.232016e-01 8.287878e-01 8.339519e-01
# [81] 8.387179e-01 8.431207e-01 8.471937e-01 8.509907e-01 8.544942e-01
# [86] 8.577596e-01 8.608136e-01 8.636859e-01 8.663973e-01 8.689832e-01
# [91] 8.714498e-01 8.738083e-01 8.760947e-01 8.783037e-01 8.804492e-01
# [96] 8.825387e-01 8.845704e-01 8.865666e-01 8.885143e-01 8.904175e-01

best_ridge <- glmnet(as.matrix(train_X_ridge), train_y_ridge, alpha = 0, lambda = best_lambda)

coef(best_ridge)
# 27 x 1 sparse Matrix of class "dgCMatrix"
# s0
# (Intercept)  6.029396e-15
# carat        5.657987e-01
# depth        1.403182e-03
# table       -7.034497e-03
# x            1.950494e-01
# y            1.178544e-01
# z            1.179028e-01
# cutFair     -3.124439e-02
# cutGood     -1.257761e-02
# cutVery     -4.827029e-04
# cutPremium   7.085168e-03
# cutIdeal     1.226185e-02
# colorD       3.877335e-02
# colorE       2.881801e-02
# colorF       2.332346e-02
# colorG       1.093157e-02
# colorH      -2.365371e-02
# colorI      -4.225734e-02
# colorJ      -7.234987e-02
# clarityI1   -9.637323e-02
# claritySI2  -1.055617e-01
# claritySI1  -4.268820e-02
# clarityVS2   2.241508e-02
# clarityVS1   4.080485e-02
# clarityVVS2  6.538972e-02
# clarityVVS1  6.262482e-02
# clarityIF    5.661552e-02

# predvidanje i R^2:
prediction_ridge <- predict(best_ridge, s = best_lambda, newx = as.matrix(test_standardized[,-4]))

# R squared formula
y_predicted <- predict(model_ridge, s = best_lambda, newx = as.matrix(test_standardized[,-4]))

rss <- sum((y_predicted - test_standardized[,4]) ^ 2)
tss <- sum((test_standardized[,4] - mean(test_standardized[,4])) ^ 2)
rsq <- 1 - rss/tss
rsq
# [1] 0.893151

RMSE(prediction_ridge, test_standardized[,4]) # [1] 0.326149
MAE(prediction_ridge, test_standardized[,4]) # [1] 0.2330374
R2(prediction_ridge, test_standardized[,4]) # 0.8942873

# pls
library(pls)

set.seed(123)
model_pls <- plsr(price ~ ., data=train_standardized, scale=F, validation="CV")

summary(model_pls)
# Data: 	X dimension: 43152 26 
# Y dimension: 43152 1
# Fit method: kernelpls
# Number of components considered: 26
# 
# VALIDATION: RMSEP
# Cross-validated using 10 random segments.
# (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps
# CV               1   0.4796   0.3827   0.3597   0.3504   0.3073   0.2880   0.2869
# adjCV            1   0.4796   0.3827   0.3597   0.3503   0.3071   0.2879   0.2869
# 8 comps  9 comps  10 comps  11 comps  12 comps  13 comps  14 comps
# CV      0.2863   0.2856    0.2841    0.2828     0.283     0.283    0.2827
# adjCV   0.2864   0.2857    0.2840    0.2828     0.283     0.283    0.2826
# 15 comps  16 comps  17 comps  18 comps  19 comps  20 comps  21 comps
# CV       0.2825    0.2824    0.2824    0.2824    0.2824    0.2824    0.2824
# adjCV    0.2824    0.2824    0.2824    0.2824    0.2824    0.2824    0.2824
# 22 comps  23 comps  24 comps  25 comps  26 comps
# CV       0.2824    0.2824    0.2825    0.2825    0.2825
# adjCV    0.2824    0.2824    0.2824    0.2824    0.2824
# 
# TRAINING: % variance explained
# 1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
# X        16.69    22.21    27.23    32.19    34.22    37.69    41.78    43.94
# price    77.01    85.39    87.12    87.82    90.54    91.61    91.68    91.77
# 9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps
# X        46.80     48.63     51.67     55.94     59.93     60.86     64.64
# price    91.87     91.99     92.02     92.02     92.02     92.04     92.05
# 16 comps  17 comps  18 comps  19 comps  20 comps  21 comps  22 comps
# X         69.04     73.57     77.84     82.32     86.64     91.18     95.47
# price     92.05     92.05     92.05     92.05     92.05     92.05     92.05
# 23 comps  24 comps  25 comps  26 comps
# X        100.00    100.07    100.13    100.20
# price     92.05     92.05     92.05     92.05

validationplot(model_pls) # RMSE
validationplot(model_pls, val.type = "MSEP")

plot(model_pls, plottype="correlation")
plot(model_pls, plottype="loadings")
plot(model_pls, plottype="biplot")

predicted_pls <- predict(model_pls, test_standardized, n_comp=2)

RMSE(test_standardized$price, predicted_pls)
# [1] 0.3043098

# pls iz drugog paketa
library(plsdof)
model_pcr_plsdof <- pcr(train_standardized[,-4], train_standardized$price,
                        scale=F)
summary(model_pcr_plsdof)
#               Length Class  Mode   
# intercept     27    -none- numeric
# coefficients 702    -none- numeric

myfolds <- createMultiFolds(train_standardized$price, k = 5, times = 10)
control <- trainControl("repeatedcv", index = myfolds, selectionFunction = "oneSE")
# Train PLS model
mod1 <- train(price ~ ., data = train_standardized,
              method = "pls",
              metric = "R2",
              tuneLength = 20,
              trControl = control)
# Check CV profile
plot(mod1)

model_pcr <- pcr(price ~ ., data=train_standardized, scale=F)

model_mvr <- mvr(price~., data=train_standardized, method="simpls")
summary(model_mvr)
# Data: 	X dimension: 43152 26 
# Y dimension: 43152 1
# Fit method: simpls
# Number of components considered: 26
# TRAINING: % variance explained
# 1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
# X        16.59    22.07    27.09    32.00    34.02    37.53    41.71    44.24
# price    76.91    85.32    87.04    87.78    90.58    91.62    91.67    91.71
# 9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps
# X        47.13     49.09     51.63     55.84     59.25     60.41     64.58
# price    91.76     91.88     91.95     91.96     91.96     91.96     91.96
# 16 comps  17 comps  18 comps  19 comps  20 comps  21 comps  22 comps
# X         68.89     73.47     77.76     82.20     86.53     91.12     95.45
# price     91.96     91.96     91.96     91.96     91.96     91.96     91.96
# 23 comps  24 comps  25 comps  26 comps
# X         100.7   117.510     134.3     151.1
# price      87.8     8.751    -211.3    -572.5

validationplot(model_mvr)

# podjela skupa podataka prema vrijednostima varijable price
kutija <- boxplot(diamonds$price)
outlieri <- kutija$out
indeksi_outliera <- which(diamonds$price %in% outlieri)
length(indeksi_outliera)
# [1] 3538

jeftiniji <- diamonds[-indeksi_outliera,]
skuplji <- diamonds[indeksi_outliera,]

# provjera:
dim(jeftiniji)[1] + dim(skuplji)[1] == dim(diamonds)[1]
# [1] TRUE

jeftiniji <- data.frame(jeftiniji)
skuplji <- data.frame(skuplji)

# provedimo najuspjesnija dva algoritma nad novim podacima

# promotrimo izgled novih podataka:
hist(jeftiniji[,1], main="carat")
hist(jeftiniji[,5], main="depth")
hist(jeftiniji[,6], main="table")
hist(jeftiniji[,7], main="price")
hist(jeftiniji[,8], main="x")
hist(jeftiniji[,9], main="y")
hist(jeftiniji[,10], main="z")

barplot(table(jeftiniji[,2]), main="cut")
barplot(table(jeftiniji[,3]), main="color")
barplot(table(jeftiniji[,4]), main="clarity")

hist(skuplji[,1], main="carat")
hist(skuplji[,5], main="depth")
hist(skuplji[,6], main="table")
hist(skuplji[,7], main="price")
hist(skuplji[,8], main="x")
hist(skuplji[,9], main="y")
hist(skuplji[,10], main="z")

barplot(table(skuplji[,2]), main="cut")
barplot(table(skuplji[,3]), main="color")
barplot(table(skuplji[,4]), main="clarity")

library(nortest)
ad.test(jeftiniji$price)
# Anderson-Darling normality test
# 
# data:  jeftiniji$price
# A = 2441.2, p-value < 2.2e-16

ad.test(skuplji$price)
# Anderson-Darling normality test
# 
# data:  skuplji$price
# A = 49.191, p-value < 2.2e-16

# normalnost nije zadovoljena

# ponovimo postupak pripreme podataka:
library(healthcareai)
split_1 <- split_train_test(jeftiniji, price, percent_train = 0.8)

dim(split_1$train)
# [1] 40323    10
dim(split_1$test)
# [1] 10079    10

train_jeftiniji = split_1$train
test_jeftiniji = split_1$test

split_2 <- split_train_test(skuplji, price, percent_train = 0.8)

dim(split_2$train)
# [1] 2832   10
dim(split_2$test)
# [1] 706  10

train_skuplji = split_2$train
test_skuplji = split_2$test

library(cleandata)
kodiraj_ordinalno <- function(podaci, poredak){
  return(encode_ordinal(data.frame(podaci), out.int=T, 
                        order=poredak))
}
train_coded_ord_jeftiniji <- train_jeftiniji
test_coded_ord_jeftiniji <- test_jeftiniji

train_coded_ord_jeftiniji[,2] <- kodiraj_ordinalno(train_coded_ord_jeftiniji[,2],c("Fair","Good","Very Good","Premium","Ideal"))
train_coded_ord_jeftiniji[,3] <- kodiraj_ordinalno(train_coded_ord_jeftiniji[,3],c("D","E","F","G","H","I","J"))
train_coded_ord_jeftiniji[,4] <- kodiraj_ordinalno(train_coded_ord_jeftiniji[,4],c("I1","SI2","SI1","VS2","VS1","VVS2","VVS1","IF"))
test_coded_ord_jeftiniji[,2] <- kodiraj_ordinalno(test_coded_ord_jeftiniji[,2],c("Fair","Good","Very Good","Premium","Ideal"))
test_coded_ord_jeftiniji[,3] <-kodiraj_ordinalno(test_coded_ord_jeftiniji[,3],c("D","E","F","G","H","I","J"))
test_coded_ord_jeftiniji[,4] <- kodiraj_ordinalno(test_coded_ord_jeftiniji[,4],c("I1","SI2","SI1","VS2","VS1","VVS2","VVS1","IF"))

train_coded_ord_skuplji <- train_skuplji
test_coded_ord_skuplji <- test_skuplji

train_coded_ord_skuplji[,2] <- kodiraj_ordinalno(train_coded_ord_skuplji[,2],c("Fair","Good","Very Good","Premium","Ideal"))
train_coded_ord_skuplji[,3] <- kodiraj_ordinalno(train_coded_ord_skuplji[,3],c("D","E","F","G","H","I","J"))
train_coded_ord_skuplji[,4] <- kodiraj_ordinalno(train_coded_ord_skuplji[,4],c("I1","SI2","SI1","VS2","VS1","VVS2","VVS1","IF"))
test_coded_ord_skuplji[,2] <- kodiraj_ordinalno(test_coded_ord_skuplji[,2],c("Fair","Good","Very Good","Premium","Ideal"))
test_coded_ord_skuplji[,3] <-kodiraj_ordinalno(test_coded_ord_skuplji[,3],c("D","E","F","G","H","I","J"))
test_coded_ord_skuplji[,4] <- kodiraj_ordinalno(test_coded_ord_skuplji[,4],c("I1","SI2","SI1","VS2","VS1","VVS2","VVS1","IF"))

head(train_coded_ord_jeftiniji)
# carat cut color clarity depth table price    x    y    z
# 2  0.21   4     2       3  59.8    61   326 3.89 3.84 2.31
# 3  0.23   2     2       5  56.9    65   327 4.05 4.07 2.31
# 4  0.29   4     6       4  62.4    58   334 4.20 4.23 2.63
# 6  0.24   3     7       6  62.8    57   336 3.94 3.96 2.48
# 7  0.24   3     6       7  62.3    57   336 3.95 3.98 2.47
# 8  0.26   3     5       3  61.9    55   337 4.07 4.11 2.53
head(train_coded_ord_skuplji)
# carat cut color clarity depth table price    x    y    z
# 1  1.70   4     6       4  62.2    58 11888 7.65 7.60 4.74
# 2  1.09   5     3       8  61.6    55 11888 6.59 6.65 4.08
# 3  1.68   5     2       2  60.4    55 11888 7.79 7.70 4.68
# 4  1.54   4     3       3  60.9    59 11897 7.56 7.46 4.57
# 5  2.00   4     7       2  61.9    55 11899 8.09 8.06 5.00
# 8  2.01   4     3       2  62.0    60 11903 7.88 7.82 4.87

# standardizacija
library(caret)
preProcValues_1 <- preProcess(train_coded_ord_jeftiniji, method = c("center", "scale"))
train_standardized_jeftiniji <- predict(preProcValues_1, train_coded_ord_jeftiniji)
test_standardized_jeftiniji <- predict(preProcValues_1, test_coded_ord_jeftiniji)

preProcValues_2 <- preProcess(train_coded_ord_skuplji, method = c("center", "scale"))
train_standardized_skuplji <- predict(preProcValues_2, train_coded_ord_skuplji)
test_standardized_skuplji <- predict(preProcValues_2, test_coded_ord_skuplji)

# ridge
library(glmnet)
lambda_seq_1 <- 10^seq(2, -2, by = -.1)

train_X_ridge_jeftiniji <- train_standardized_jeftiniji[,-4]
train_y_ridge_jeftiniji <- train_standardized_jeftiniji[,4]

model_ridge_jeftiniji <- glmnet(as.matrix(train_X_ridge_jeftiniji), as.matrix(train_y_ridge_jeftiniji),
                                alpha = 0, lambda  = lambda_seq_1)

summary(model_ridge_jeftiniji)
#           Length Class     Mode   
# a0         41    -none-    numeric
# beta      369    dgCMatrix S4     
# df         41    -none-    numeric
# dim         2    -none-    numeric
# lambda     41    -none-    numeric
# dev.ratio  41    -none-    numeric
# nulldev     1    -none-    numeric
# npasses     1    -none-    numeric
# jerr        1    -none-    numeric
# offset      1    -none-    logical
# call        5    -none-    call   
# nobs        1    -none-    numeric

train_X_ridge_skuplji <- train_standardized_skuplji[,-4]
train_y_ridge_skuplji <- train_standardized_skuplji[,4]

model_ridge_skuplji <- glmnet(as.matrix(train_X_ridge_skuplji), as.matrix(train_y_ridge_skuplji),
                                alpha = 0, lambda  = lambda_seq_1)

summary(model_ridge_skuplji)
#           Length Class     Mode   
# a0         41    -none-    numeric
# beta      369    dgCMatrix S4     
# df         41    -none-    numeric
# dim         2    -none-    numeric
# lambda     41    -none-    numeric
# dev.ratio  41    -none-    numeric
# nulldev     1    -none-    numeric
# npasses     1    -none-    numeric
# jerr        1    -none-    numeric
# offset      1    -none-    logical
# call        5    -none-    call   
# nobs        1    -none-    numeric

ridge_cv_jeftiniji <- cv.glmnet(as.matrix(train_X_ridge_jeftiniji), train_y_ridge_jeftiniji, alpha = 0)

plot(ridge_cv_jeftiniji)

best_lambda_jeftiniji <- ridge_cv_jeftiniji$lambda.min
best_lambda_jeftiniji
# [1] 0.03899967

best_fit_jeftiniji <- ridge_cv_jeftiniji$glmnet.fit

ridge_cv_skuplji <- cv.glmnet(as.matrix(train_X_ridge_skuplji), train_y_ridge_skuplji, alpha = 0)

plot(ridge_cv_skuplji)

best_lambda_skuplji <- ridge_cv_skuplji$lambda.min
best_lambda_skuplji
# [1] 0.06801911

best_fit_skuplji <- ridge_cv_skuplji$glmnet.fit

best_ridge_jeftiniji <- glmnet(as.matrix(train_X_ridge_jeftiniji), train_y_ridge_jeftiniji, 
                               alpha = 0, lambda = best_lambda_jeftiniji)

best_ridge_skuplji <- glmnet(as.matrix(train_X_ridge_skuplji), train_y_ridge_skuplji, 
                               alpha = 0, lambda = best_lambda_skuplji)

prediction_ridge_jeftiniji <- predict(best_ridge_jeftiniji, s = best_lambda_jeftiniji, 
                                      newx = as.matrix(test_standardized_jeftiniji[,-4]))

RMSE(prediction_ridge_jeftiniji, test_standardized_jeftiniji[,4]) # [1] 0.7548218
MAE(prediction_ridge_jeftiniji, test_standardized_jeftiniji[,4]) # [1] 0.5979697
R2(prediction_ridge_jeftiniji, test_standardized_jeftiniji[,4]) # 0.4275972

prediction_ridge_skuplji <- predict(best_ridge_skuplji, s = best_lambda_skuplji, 
                                      newx = as.matrix(test_standardized_skuplji[,-4]))

RMSE(prediction_ridge_skuplji, test_standardized_skuplji[,4]) # [1] 0.7005069
MAE(prediction_ridge_skuplji, test_standardized_skuplji[,4]) # [1] 0.5233988
R2(prediction_ridge_skuplji, test_standardized_skuplji[,4]) # 0.5642041

# losije nego svi podaci skupa

# pls
library(pls)
set.seed(123)

model_pls_jeftiniji <- plsr(price ~ ., data=train_standardized_jeftiniji, scale=F, validation="CV")
summary(model_pls_jeftiniji)
# Data: 	X dimension: 40323 9 
# Y dimension: 40323 1
# Fit method: kernelpls
# Number of components considered: 9
# 
# VALIDATION: RMSEP
# Cross-validated using 10 random segments.
# (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps
# CV               1   0.4626   0.3668    0.352   0.3450   0.3210   0.3185   0.3179   0.3163   0.3183
# adjCV            1   0.4626   0.3668    0.352   0.3449   0.3207   0.3183   0.3178   0.3163   0.3180
# 
# TRAINING: % variance explained
# 1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps
# X        46.98    58.71    70.10    80.56    84.67    90.49    99.39    99.89   100.00
# price    78.61    86.58    87.67    88.28    89.69    89.83    89.86    90.03    90.06

model_pls_skuplji <- plsr(price ~ ., data=train_standardized_skuplji, scale=F, validation="CV")
summary(model_pls_skuplji)
# Data: 	X dimension: 2832 9 
# Y dimension: 2832 1
# Fit method: kernelpls
# Number of components considered: 9
# 
# VALIDATION: RMSEP
# Cross-validated using 10 random segments.
# (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps
# CV               1   0.9474   0.8838   0.8727   0.8677   0.8603   0.8590   0.8627   0.9161    2.073
# adjCV            1   0.9473   0.8811   0.8692   0.8642   0.8570   0.8559   0.8589   0.9075    1.984
# 
# TRAINING: % variance explained
# 1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps  9 comps
# X        42.51    47.85    58.56    66.40    74.03    86.75    87.93    94.67   100.00
# price    10.73    28.21    30.76    31.78    32.31    32.41    32.67    32.67    32.67

validationplot(model_pls_jeftiniji, main="price - jeftiniji") # RMSE
validationplot(model_pls_jeftiniji, val.type = "MSEP", main="price - jeftiniji")

plot(model_pls_jeftiniji, plottype="correlation")
plot(model_pls_jeftiniji, plottype="loadings")
plot(model_pls_jeftiniji, plottype="biplot")

predicted_pls_jeftiniji <- predict(model_pls_jeftiniji, test_standardized_jeftiniji, n_comp=3)

RMSE(test_standardized_jeftiniji$price, predicted_pls_jeftiniji)
# [1] 0.3435771

validationplot(model_pls_skuplji, main="price - skuplji") # RMSE
validationplot(model_pls_skuplji, val.type = "MSEP", main="price - skuplji")

plot(model_pls_skuplji, plottype="correlation")
plot(model_pls_skuplji, plottype="loadings")
plot(model_pls_skuplji, plottype="biplot")

predicted_pls_skuplji <- predict(model_pls_skuplji, test_standardized_skuplji, n_comp=2)

RMSE(test_standardized_skuplji$price, predicted_pls_skuplji)
# [1] 0.8980163

# losiji rezultati, cak i ako ukljucimo vise komponenti:
predicted_pls_skuplji <- predict(model_pls_skuplji, test_standardized_skuplji, n_comp=6)

RMSE(test_standardized_skuplji$price, predicted_pls_skuplji)
# [1] 0.8574603

