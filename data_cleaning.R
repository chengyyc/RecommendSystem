setwd("C:/Users/yingy/Desktop/math/Data Mining 5330/project/data")

library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

#----------------training set------------------------

train3 <- fread("train_ver2.csv",nrows=-1,na.strings = c("",NA,"NA"))
sapply(train3, function(x)sum(is.na(x)))

##convert fecha_dato and fecha_alta to date
train3$fecha_dato <- as.POSIXct(strptime(train3$fecha_dato, format = "%Y-%m-%d"))
train3$fecha_alta <- as.POSIXct(strptime(train3$fecha_alta, format = "%Y-%m-%d"))

##############Lets deal with missing values###############################
###At least the customers with NA ind_nuevo seem to be new customers. Replave NA with 1
train3$ind_nuevo[is.na(train3$ind_nuevo)] <- 1

##substitude missing fecha_alta with median of other new customers
train3$fecha_alta[is.na(train3$fecha_alta)] <- median(train3$fecha_alta[train3$ind_nuevo == 1], na.rm = TRUE)

##create a month column
train3$month_dato <- month(train3$fecha_dato)
train3$month_alta <- month(train3$fecha_alta)
##create a year column
train3$year_dato <- year(train3$fecha_dato)
train3$year_alta <- year(train3$fecha_alta)

##substitude the outliers(<18 or >100) into the median of their nearest age group
train3$age[train3$age <18] <- median(train3$age[(train3$age >=18) & (train3$age <= 30)], na.rm = TRUE) 
train3$age[train3$age >100] <- median(train3$age[(train3$age > 30) & (train3$age <= 100)], na.rm = TRUE) 
##substitude NAs with median age of sample
train3$age[is.na(train3$age)] <- median(train3$age, na.rm = TRUE)

##substitude  NA to 0. -999999 to 6
train3$antiguedad[is.na(train3$antiguedad)]<- 0
train3$antiguedad[(train3$antiguedad == -999999)]<- 6

##fill in missing value with 1
train3$indrel[is.na(train3$indrel)]<- 1

###deal with ind_actividad_cliente
train3$ind_actividad_cliente[is.na(train3$ind_actividad_cliente)] <- median(train3$ind_actividad_cliente[train3$ind_nuevo ==1],na.rm=TRUE)

###deal with nomprov, NA as UNKOWN
train3$nomprov[is.na(train3$nomprov)] <- "UNKNOWN" 


##fill renta with province median income
new.incomes <-train3 %>%
  select(nomprov) %>%
  merge(train3 %>%
          group_by(nomprov) %>%
          summarise(med.income=median(renta,na.rm=TRUE)),by="nomprov") %>%
  select(nomprov,med.income) %>%
  arrange(nomprov)
train3 <- arrange(train3,nomprov)
train3$renta[is.na(train3$renta)] <- new.incomes$med.income[is.na(train3$renta)]
rm(new.incomes)

train3$renta[is.na(train3$renta)] <- median(train3$renta,na.rm=TRUE)
train3 <- arrange(train3,fecha_dato)

train3$ind_nomina_ult1[is.na(train3$ind_nomina_ult1)] <- 0
train3$ind_nom_pens_ult1[is.na(train3$ind_nom_pens_ult1)] <- 0

train3$indfall[is.na(train3$indfall)]                 <- "N"

train3$tiprel_1mes[is.na(train3$tiprel_1mes)]         <- "I"

train3$indrel_1mes[train3$indrel_1mes=="P"]        <- "5" # change to just numbers because it currently contains letters and numbers
#ATTENTION!
train3$indrel_1mes                             <- as.factor(as.integer(train3$indrel_1mes))

train3$pais_residencia[is.na(train3$pais_residencia)] <- "UNKNOWN"
train3$canal_entrada[is.na(train3$canal_entrada)]     <- "UNKNOWN"
train3$sexo[is.na(train3$sexo)]                       <- "UNKNOWN"
train3$ult_fec_cli_1t[is.na(train3$ult_fec_cli_1t)]   <- "UNKNOWN"
train3$ind_empleado[is.na(train3$ind_empleado)]       <- "UNKNOWN"
train3$indext[is.na(train3$indext)]                   <- "UNKNOWN"
train3$indresi[is.na(train3$indresi)]                 <- "UNKNOWN"
train3$conyuemp[is.na(train3$conyuemp)]               <- "UNKNOWN"
train3$segmento[is.na(train3$segmento)]               <- "UNKNOWN"

str(train3)
summary(train3)
sapply(train3, function(x)sum(is.na(x)))


write.table(train3,"mydata_train.csv",row.names=FALSE,col.names=TRUE,sep=",")


########------testing set-------########

test3 <- fread("test_ver2.csv",nrows=-1,na.strings = c("",NA,"NA"))
sapply(test3, function(x)sum(is.na(x)))

##convert fecha_dato and fecha_alta to date
test3$fecha_dato <- as.POSIXct(strptime(test3$fecha_dato, format = "%Y-%m-%d"))
test3$fecha_alta <- as.POSIXct(strptime(test3$fecha_alta, format = "%Y-%m-%d"))

##############Lets deal with missing values###############################
###At least the customers with NA ind_nuevo seem to be new customers. Replave NA with 1
test3$ind_nuevo[is.na(test3$ind_nuevo)] <- 1

##substitude missing fecha_alta with median of other new customers
test3$fecha_alta[is.na(test3$fecha_alta)] <- median(test3$fecha_alta[test3$ind_nuevo == 1], na.rm = TRUE)

##create a month column
test3$month_dato <- month(test3$fecha_dato)
test3$month_alta <- month(test3$fecha_alta)
##create a year column
test3$year_dato <- year(test3$fecha_dato)
test3$year_alta <- year(test3$fecha_alta)

##substitude the outliers(<18 or >100) into the median of their nearest age group
test3$age[test3$age <18] <- median(test3$age[(test3$age >=18) & (test3$age <= 30)], na.rm = TRUE) 
test3$age[test3$age >100] <- median(test3$age[(test3$age > 30) & (test3$age <= 100)], na.rm = TRUE) 
##substitude NAs with median age of sample
test3$age[is.na(test3$age)] <- median(test3$age, na.rm = TRUE)

##substitude  NA to 0. -999999 to 6
test3$antiguedad[is.na(test3$antiguedad)]<- 0
test3$antiguedad[(test3$antiguedad == -999999)]<- 6

##fill in missing value with 1
test3$indrel[is.na(test3$indrel)]<- 1

###deal with ind_actividad_cliente
test3$ind_actividad_cliente[is.na(test3$ind_actividad_cliente)] <- median(test3$ind_actividad_cliente[test3$ind_nuevo ==1],na.rm=TRUE)

###deal with nomprov, NA as UNKOWN
test3$nomprov[is.na(test3$nomprov)] <- "UNKNOWN" 


##fill renta with province median income
new.incomes <-test3 %>%
  select(nomprov) %>%
  merge(test3 %>%
          group_by(nomprov) %>%
          summarise(med.income=median(renta,na.rm=TRUE)),by="nomprov") %>%
  select(nomprov,med.income) %>%
  arrange(nomprov)
test3 <- arrange(test3,nomprov)
test3$renta[is.na(test3$renta)] <- new.incomes$med.income[is.na(test3$renta)]
rm(new.incomes)

test3$renta[is.na(test3$renta)] <- median(test3$renta,na.rm=TRUE)
test3 <- arrange(test3,fecha_dato)

test3$ind_nomina_ult1[is.na(test3$ind_nomina_ult1)] <- 0
test3$ind_nom_pens_ult1[is.na(test3$ind_nom_pens_ult1)] <- 0

test3$indfall[is.na(test3$indfall)]                 <- "N"

test3$tiprel_1mes[is.na(test3$tiprel_1mes)]         <- "I"

test3$indrel_1mes[test3$indrel_1mes=="P",]        <- "5" # change to just numbers because it currently contains letters and numbers

#ATTENTION!
test3$indrel_1mes                             <- as.factor(as.integer(test3$indrel_1mes))

test3$pais_residencia[is.na(test3$pais_residencia)] <- "UNKNOWN"
test3$canal_entrada[is.na(test3$canal_entrada)]     <- "UNKNOWN"
test3$sexo[is.na(test3$sexo)]                       <- "UNKNOWN"
test3$ult_fec_cli_1t[is.na(test3$ult_fec_cli_1t)]   <- "UNKNOWN"
test3$ind_empleado[is.na(test3$ind_empleado)]       <- "UNKNOWN"
test3$indext[is.na(test3$indext)]                   <- "UNKNOWN"
test3$indresi[is.na(test3$indresi)]                 <- "UNKNOWN"
test3$conyuemp[is.na(test3$conyuemp)]               <- "UNKNOWN"
test3$segmento[is.na(test3$segmento)]               <- "UNKNOWN"

str(test3)
summary(test3)
sapply(test3, function(x)sum(is.na(x)))

write.table(test3,"mydata_test.csv",row.names=FALSE,col.names=TRUE,sep=",")















