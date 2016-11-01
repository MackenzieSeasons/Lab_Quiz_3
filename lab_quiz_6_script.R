library(tidyverse)
library(cocor)
library(apaTables)

#QUESTION 4
#rating-raises correlation compared to rating-critical correlation
rating_raises_cor <- .59
rating_critical_cor <- .16
raises_critical_cor <- .38

cocor.dep.groups.overlap(rating_raises_cor, rating_critical_cor, raises_critical_cor, 30)
#the rating raises correlation differs from rating-critical cor by delta r = .43, 95% CI [.07, .80], p = .03

#QUESTION 5
rating_raises_cor <- .59
complaints_critical_cor <- .19

rating_complaints_cor <- .83
rating_critical_cor <- .16
raises_complaints_cor <- .67
raises_critical_cor <- .38

cocor.dep.groups.nonoverlap(rating_raises_cor, complaints_critical_cor, 
                            rating_complaints_cor, rating_critical_cor, 
                            raises_complaints_cor, raises_critical_cor, 30)
#the rating raises correlation differs from the complaints-critical correlation by delta r = .4, 95% CI [.01, .80], p = .04

#QUESTION 6
library(predictionInterval)
cocor.indep.groups(.59, .03, 30, 3000)
#the difference between our rating-raises correlation and the new study is delta r = .56, 95% CI [.26, .76], p = .00
# they differ!

#QUESTION 7
#we can conclude that...use n that's bigger, come back to it


#QUESTION 1

bfi <- read_csv("bfi2.csv")
glimpse(bfi)

library(apaTables)
apa.cor.table(bfi)

library(cocor)
cocor(~A1+C1|E1+O1, data=as.data.frame(bfi))
# the difference between the correlations (A1C1) and (E1O1) was delta r = -.01, 95% CI [-.11, .09], p = .84
# they are not difference from one another 

#QUESTION 2
cocor(~A1+C1|A1+E1, data=as.data.frame(bfi))
# the difference between A1C1 and A1E1 was delta r = -.08, 95% CI [-.18, .02], p = .13
# they are not difference from one another

#QUESTION 3
bfi_men <- bfi %>% filter(gender==1) %>% select(-gender)
bfi_women <- bfi %>% filter(gender==2) %>% select(-gender)

apa.cor.table(bfi_men)
apa.cor.table(bfi_women)

bfi_men_dataframe <- as.data.frame(bfi_men)
bfi_women_dataframe <- as.data.frame(bfi_women)
cocor(~A1+E1|A1+E1, data=list(bfi_men_dataframe, bfi_women_dataframe))
# the correlation difference for men vs women on A1E1 was delta r = .02, 95% CI [-.13, .17], p = .82
# not different




