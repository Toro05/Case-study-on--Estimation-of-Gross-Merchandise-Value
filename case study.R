
pt = read.csv("potential_restaurants_20220113.csv", encoding = "UTF-8", stringsAsFactors = T, na.strings = "")
ex = read.csv("existing_restaurants_20220113.csv", encoding = "UTF-8", stringsAsFactors = T, na.strings = "")
or = read.csv("orders_20220113.csv", stringsAsFactors = T, na.strings = "")

### libraries ###
library(corrplot)
library(plyr)
library(skimr)
library(tidyverse)
library(mice)
library(VIM)
library(dplyr)
library(cluster)
library(fastDummies)
library(caret)
library(randomForest)
library(Metrics)
library(lattice)
library(factoextra)
library(ggplot2)
library(data.table)
library(SynClustR)
library(diagram)
library(MLmetrics)

############################ POTENTIAL RESTAURANTS ##################################
#####################################################################################
skim(pt)

### drop 3 obs from primary cuisine ###
which(is.na(pt$primary_cuisine))
pt = pt[-which(is.na(pt$primary_cuisine)),]

### drop variables country and restaurant ###
pt = pt[,-c(1,5)]

### restaurant and delivery as junk ###
pt <- pt[!(pt$primary_cuisine=="restaurant"),] 
pt <- pt[!(pt$primary_cuisine=="delivery"),]

pt$secondary_cuisine <- mapvalues(pt$secondary_cuisine, from = c("restaurant", "delivery" ),
                                  to = c(NA, NA))

### probably needs fixing the levels ###
levels(pt$price_range)
levels(pt$price_range)[4] = "£ & ££ - £££"

### fixing negative review values ###
which(pt$review_rating<0)
pt = pt[-420,]

### transform zipcode to factor variable ###
pt$masked_zipcode = as.factor(pt$masked_zipcode)

### transform hashed_restaurant_id to character variable ###
pt$hashed_restaurant_id = as.character(pt$hashed_restaurant_id)

### merging cuisine levels ###
pt$primary_cuisine <- as.factor(tolower(pt$primary_cuisine))
levels(pt$primary_cuisine)

pt$secondary_cuisine <- as.factor(tolower(pt$secondary_cuisine))
levels(pt$secondary_cuisine)

### merging city levels ###
pt$city <- mapvalues(pt$city, from = c("Amsterdam-Duivendrecht", "Rotterdam-Albrandswaard" ),
                     to = c("Amsterdam", "Rotterdam"))

### imputation ###
init = mice(pt, maxit=0) 
meth = init$method
predM = init$predictorMatrix

### remove variable as predictor ###
predM[, c("secondary_cuisine", "price_range")]=0

### skip a variable from imputation ###
meth[c("secondary_cuisine", "price_range")]=""

### specify different imputation methods per variable ###
meth[c("review_rating")]= "pmm"

### complete potential ###
imputed <- mice(pt ,m=5 , maxit = 50, method = meth, predictorMatrix = predM ,seed=500)
pt_imputed = complete(imputed,1)
View(pt_imputed)

### checking imputation distribution ###
xyplot(imputed, review_rating ~ total_reviews + primary_cuisine + city + masked_zipcode, pch=18, cex=1 )
densityplot(imputed)

#####################################
### calculating the mode ### not used 
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


try = aggregate(price_range ~ primary_cuisine, FUN = getmode, data = pt_imputed)

pt_imputed <-pt_imputed[pt_imputed$primary_cuisine  %in% try$primary_cuisine,]
pt_imputed$primary_cuisine = droplevels(pt_imputed$primary_cuisine)

i<-1

while (i <=  nrow(pt_imputed)){
  if (is.na(pt_imputed$price_range[i])) {
    list3<-aggregate(primary_cuisine ~ price_range, data=pt_imputed,FUN=length, subset=primary_cuisine==primary_cuisine[i])
    final<-list3[which.max(list3$primary_cuisine),]
    pt_imputed$price_range[i]<-final$price_range
    
  }
  i<-1+i
}
View(pt_imputed)

########################################################################
########################################################################


############################ EXISTING RESTAURANTS ###################################
#####################################################################################
skim(ex)
ex$time_window_start = as.Date(ex$time_window_start)
ex$time_window_end = as.Date(ex$time_window_end)

sum(ex$orders)
sum(ex$gmv_for_case)

### drop 3 obs from primary cuisine ###
which(is.na(ex$primary_cuisine))
ex = ex[-which(is.na(ex$primary_cuisine)),]

### drop variables country and restaurant ###
ex = ex[,-c(1,5)]

### restaurant and delivery as junk ###
ex <- ex[!(ex$primary_cuisine=="restaurant"),] 
ex <- ex[!(ex$primary_cuisine=="delivery"),]

ex$secondary_cuisine <- mapvalues(ex$secondary_cuisine, from = c("restaurant", "delivery" ),
                                  to = c(NA, NA))

### probably needs fixing the levels ###
levels(ex$price_range)
levels(ex$price_range)[4] = "£ & ££ - £££"

### fixing negative review values ###
which(ex$review_rating<0)
ex = ex[-737,]

### transform zipcode to factor variable ###
ex$masked_zipcode = as.factor(ex$masked_zipcode)

### transform hashed_restaurant_id to character variable ###
ex$hashed_restaurant_id = as.character(ex$hashed_restaurant_id)

### merging cuisine levels ###
ex$primary_cuisine <- as.factor(tolower(ex$primary_cuisine))

ex$secondary_cuisine <- as.factor(tolower(ex$secondary_cuisine))
levels(ex$secondary_cuisine)

### fix poke bowl ###
ex$primary_cuisine <- mapvalues(ex$primary_cuisine, from = "pok� bowl",
                                to = "poké bowl")
### fix cafe bowl ###
ex$primary_cuisine <- mapvalues(ex$primary_cuisine, from = "café",
                                to = "cafe")
levels(ex$primary_cuisine)
### imputation ###
init2 = mice(ex, maxit=0) 
meth2 = init2$method
predM2 = init2$predictorMatrix

### remove variable as predictor ###
predM2[, c("secondary_cuisine", "price_range", "primary_cuisine")]=0

### skip a variable from imputation ###
meth2[c("secondary_cuisine", "price_range")]=""

### specify different imputation methods per variable ###
meth2[c("review_rating")]= "pmm"

### complete potential ###
imputed2 <- mice(ex ,m=5 , maxit = 50, method = meth2, predictorMatrix = predM2 ,seed=500)
imputed2$loggedEvents
ex_imputed = complete(imputed2,1)
View(ex_imputed)

### checking imputation distribution ###
xyplot(imputed2, review_rating ~ total_reviews + primary_cuisine + city + masked_zipcode, pch=18, cex=1 )
densityplot(imputed2)

try2 = aggregate(price_range ~ primary_cuisine, FUN = getmode, data = ex_imputed)

ex_imputed <-ex_imputed[ ex_imputed$primary_cuisine  %in% try2$primary_cuisine,]
ex_imputed$primary_cuisine = droplevels(ex_imputed$primary_cuisine)

j<-1

while (j <=  nrow(ex_imputed)){
  if (is.na(ex_imputed$price_range[j])) {
    list3<-aggregate(primary_cuisine ~ price_range, data=ex_imputed,FUN=length, subset=primary_cuisine==primary_cuisine[j])
    final<-list3[which.max(list3$primary_cuisine),]
    ex_imputed$price_range[j]<-final$price_range
    
  }
  j<-1+j
}
View(ex_imputed)


############################ ORDERS ###################################
#######################################################################
skim(or)
or$orderdatetime = as.Date(or$orderdatetime)

or_final = left_join(or, ex_imputed[,c(3, 6:10)], by = "hashed_restaurant_id")
or_final = subset(or_final,or_final$orderdatetime >= or_final$time_window_start & or_final$orderdatetime < or_final$time_window_end )

sum(or_final$orders)
sum(or_final$gmv_for_case_order)

### transform restaurant id to factor ###
or_final$hashed_restaurant_id = as.factor(or_final$hashed_restaurant_id)

### masked_zipcode_restaurant to factor ###
or_final$masked_zipcode_restaurant = as.factor(or_final$masked_zipcode_restaurant)
skim(or_final)

levels(or_final$primary_cuisine)

### restaurant and delivery as junk ###
or_final <- or_final[!(or_final$primary_cuisine=="restaurant"),] 
or_final <- or_final[!(or_final$primary_cuisine=="delivery"),]

or_final$secondary_cuisine <- mapvalues(or_final$secondary_cuisine, from = c("restaurant", "delivery" ),
                                        to = c(NA, NA))
### lower case letters ###
or_final$primary_cuisine <- as.factor(tolower(or_final$primary_cuisine))

or_final$secondary_cuisine <- as.factor(tolower(or_final$secondary_cuisine))

### fix the names of poke bowl and cafe ###
or_final$primary_cuisine <- mapvalues(or_final$primary_cuisine, from = c("pok� bowl"),
                                      to = c("poké bowl"))
or_final$primary_cuisine <- mapvalues(or_final$primary_cuisine, from = "café",
                                      to = "cafe")

or_final$secondary_cuisine <- mapvalues(or_final$secondary_cuisine, from = "café",
                                        to = "cafe")

### fix masked_zipcode_customer ###
or_final$masked_zipcode_customer <- mapvalues(or_final$masked_zipcode_customer, from = "(No0",
                                              to = NA)
or_final[is.na(or_final$masked_zipcode_customer),]
or_final = or_final[!is.na(or_final$masked_zipcode_customer),]

### drop empty levels ###
or_final$hashed_customer_id = droplevels(or_final$hashed_customer_id) 
or_final$masked_zipcode_customer = droplevels(or_final$masked_zipcode_customer)
or_final$hashed_restaurant_id = droplevels(or_final$hashed_restaurant_id)


##############################################################################################################################
########################################### predictor customers per location #################################################
or_final = as.data.table(or_final)
customers = or_final[ , .N, by = .(hashed_customer_id, masked_zipcode_customer,primary_cuisine)]
customers$N = 1
acc_cus = aggregate(N ~ masked_zipcode_customer + primary_cuisine, FUN = sum, data = customers)
names(acc_cus)[1] =  "masked_zipcode"

pt_imputed = left_join(pt_imputed, acc_cus, by = c("masked_zipcode","primary_cuisine"))
pt_imputed$masked_zipcode = droplevels(pt_imputed$masked_zipcode)
pt_imputed$primary_cuisine = droplevels(pt_imputed$primary_cuisine)
names(pt_imputed)[9] =  "cus_per_location"

ex_imputed = left_join(ex_imputed, acc_cus, by = c("masked_zipcode","primary_cuisine"))
ex_imputed$masked_zipcode = droplevels(ex_imputed$masked_zipcode)
ex_imputed$primary_cuisine = droplevels(ex_imputed$primary_cuisine)
names(ex_imputed)[13] =  "cus_per_location"

########################################### TAGS predictor ####################################################################
##### 0 = single tag / 1 = two tags #####
pt_imputed$tags = ifelse(is.na(pt_imputed$secondary_cuisine), 0, 1) 
pt_imputed$tags = as.factor(pt_imputed$tags)
ex_imputed$tags = ifelse(is.na(ex_imputed$secondary_cuisine), 0, 1)
ex_imputed$tags = as.factor(ex_imputed$tags)
or_final$tags = ifelse(is.na(or_final$secondary_cuisine), 0, 1)

############################################ ELASTICITY ######################################################################
df = aggregate(cbind(orders, gmv_for_case_order)~ masked_zipcode_restaurant + primary_cuisine, FUN = sum, data = or_final)
df$gmv_for_case_order = log(df$gmv_for_case_order)
df$orders = log(df$orders)
reg2 = lm(gmv_for_case_order ~ orders, data = df)
summary(reg2)
df$elasticity = reg2$coefficients["orders"] * (df$gmv_for_case_order/df$orders)
names(df)[1] =  "masked_zipcode"
pt_imputed = left_join(pt_imputed, df[,c(1:2,5)], by = c("masked_zipcode","primary_cuisine"))
ex_imputed = left_join(ex_imputed, df[,c(1:2,5)], by = c("masked_zipcode","primary_cuisine"))

###############################################################################################################################
########################################### Imputation elasticity and cus_per_location ########################################

### imputation ###
init3 = mice(pt_imputed, maxit=0) 
meth3 = init3$method
predM3 = init3$predictorMatrix

### remove variable as predictor ###
predM3[, c("secondary_cuisine")]=0

### skip a variable from imputation ###
meth3[c("secondary_cuisine")]=""

### specify different imputation methods per variable ###
meth3[c("elasticity")]= "pmm"
meth3[c("cus_per_location")]= "pmm"

### complete potential ###
imputed3 <- mice(pt_imputed ,m=5 , maxit = 50, method = meth3, predictorMatrix = predM3 ,seed=500)
pt_imputed = complete(imputed3,1)
View(pt_imputed)

### checking imputation distribution ###
xyplot(imputed3, elasticity ~ total_reviews + review_rating + primary_cuisine + city + masked_zipcode + price_range + tags, pch=18, cex=1 )
xyplot(imputed3, cus_per_location ~ total_reviews + review_rating + primary_cuisine + city + masked_zipcode + price_range + tags, pch=18, cex=1 )
densityplot(imputed3)

#################################################################################################################

####################################  CLUSTERING    ##########################################################################

#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

#######################################################################################################################
y= ex_imputed
y = y[,-c(9:12)]
y = rbind(y,pt_imputed)

gower_d = daisy(y[,c(1, 2, 4, 8, 10, 11)], metric = "gower")

gower_mat = as.matrix(gower_d)

### most similar cases
y[,c(1, 2, 4, 8, 10, 11)][which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]

### most dissimilar cases
y[,c(1, 2, 4, 8, 10, 11)][which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]

### optimal number of k
set.seed(777)
sil_width <- c(NA)
for(i in 2:20){  
  pam_fit <- pam(gower_d, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:20, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:20, sil_width)

### PAM clustering
library(tidyverse)
k <- 7
pam_fit <- pam(gower_d, diss = TRUE, k)
y$cluster = pam_fit$clustering
pam_results <- y %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

install.packages("Rtsne")
library(Rtsne)
### cluster visualization
tsne_obj <- Rtsne(gower_d, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

y$cluster = as.factor(y$cluster)

###############fviz visualize####################
install.packages("factoextra")
library(factoextra)


#################################################
hist(y$elasticity[y$cluster==1])
hist(y$elasticity[y$cluster==2])
hist(y$elasticity[y$cluster==3])
hist(y$elasticity[y$cluster==4])
hist(y$elasticity[y$cluster==5])
hist(y$elasticity[y$cluster==6])
hist(y$elasticity[y$cluster==7])



ex_imputed = left_join(ex_imputed, y[,c(3,12)], by = "hashed_restaurant_id")
pt_imputed = left_join(pt_imputed, y[,c(3,12)], by = "hashed_restaurant_id")


trial_pt = pt_imputed
trial_ex = ex_imputed

trial_ex = trial_ex[,-c(1, 3, 5, 9:11, 14)]
trial_pt = trial_pt[, -c(1, 3, 5, 10)]

### omit na from trial_ex ###
sum(is.na(trial_ex))
trial_ex<-na.omit(trial_ex)
### check for na's in potential ###
sum(is.na(trial_pt))

###########
library(corrplot)
corrplot.mixed(cor(trial_ex[,c(3,4,7,8)]))

################# p value is small so null hypothesis is rejected so variables are not normally distributed

########### cor between total reviews and review rating is 0.0329 with p 0.3858 is not statistically significant 
cor.test(train.data$total_reviews, train.data$review_rating, method = "pearson")

########### null hypothesis is that cor is by chance and alternative is that cor is also in the population 
cor.test(train.data$cus_per_location, train.data$elasticity, method = "pearson")

#split
hist(trial_ex$review_rating)
hist(trial_ex$total_reviews)
hist(trial_ex$gmv_for_case)
hist(trial_ex$cus_per_location)
hist(trial_ex$elasticity)


trial_ex$review_rating = log(max(trial_ex$review_rating+1) - trial_ex$review_rating)
hist(trial_ex$review_rating)

trial_ex$total_reviews[trial_ex$total_reviews==0] = 1
trial_ex$total_reviews = log(trial_ex$total_reviews)
hist(trial_ex$total_reviews)

trial_ex$gmv_for_case = log(trial_ex$gmv_for_case)
hist(trial_ex$gmv_for_case)

trial_ex$cus_per_location = log(trial_ex$cus_per_location)
hist(trial_ex$cus_per_location)

trial_ex$elasticity = log(trial_ex$elasticity)
hist(trial_ex$elasticity)

hist(trial_ex$elasticity, xlab = "elasticity", main = "Histogram of Elasticity")

set.seed(123)
train.prop = 0.8
indice = sample (1: nrow(trial_ex), nrow(trial_ex)*train.prop)
train.data = trial_ex[indice ,]
test.data = trial_ex[-indice ,]



### linear with all variables ###
set.seed(777)
reg = lm(gmv_for_case ~., data = train.data)
summary(reg)
sum(reg$fitted.values<0)
preds = sapply(1:nrow(test.data), function(i) tryCatch(predict(reg, newdata = test.data[i,]), error = function (e) NA))
preds
sum(is.na(preds))
preds <- ifelse(is.na(preds),0,preds)

MAPE(y_pred=preds,y_true=test.data$gmv_for_case)
MAE(preds, test.data$gmv_for_case)
MSE(preds, test.data$gmv_for_case)
RMSE(preds, test.data$gmv_for_case)
RMSLE(preds, test.data$gmv_for_case)
RMSPE(preds, test.data$gmv_for_case)

VIF(reg)


### linear with some variables ###
reg2 = lm(gmv_for_case ~ review_rating + total_reviews + elasticity + cluster + cus_per_location, data = train.data)
summary(reg2)
### ISLR interpretation ###
preds2 = predict(reg2, newdata = test.data, interval = "prediction")
preds2
sum(is.na(preds2))
confint(reg2)

MAPE(preds2,test.data$gmv_for_case)
MAE(preds2, test.data$gmv_for_case)
MSE(preds2, test.data$gmv_for_case)
RMSE(preds2, test.data$gmv_for_case)
RMSLE(preds2, test.data$gmv_for_case)
RMSPE(preds2, test.data$gmv_for_case)


VIF(reg2)


################### RANDOM FOREST #################################
customRF <- list(type = "Regression", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=2:4, .ntree=c(100, 200, 300, 400, 500, 600, 700, 800, 900))
set.seed(777)
custom <- train(gmv_for_case ~ review_rating + total_reviews + elasticity + cluster + cus_per_location, data=train.data,
                method=customRF, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)
custom
varImp(custom)
plot(varImp(custom))

pred.custom<- predict(custom, test.data)
pred.custom
rmse(test.data$gmv_for_case, pred.custom)
mae(test.data$gmv_for_case, pred.custom)
(cor(test.data$gmv_for_case,pred.custom))^2
MAPE(y_true =test.data$gmv_for_case, y_pred = pred.custom)

set.seed(777)
rf = randomForest(gmv_for_case ~ review_rating + total_reviews + elasticity + 
                    cluster + cus_per_location + price_range , data=train.data, ntree = 500, mtry = 3, importance = T)
plot(rf)
rf
pred.rf<- predict(rf, test.data)


MAPE(pred.rf,test.data$gmv_for_case)
MAE(pred.rf, test.data$gmv_for_case)
MSE(pred.rf, test.data$gmv_for_case)
RMSE(pred.rf, test.data$gmv_for_case)
RMSLE(pred.rf, test.data$gmv_for_case)
RMSPE(pred.rf, test.data$gmv_for_case)
library(ranger)
importance(rf)
varImpPlot(rf)


# Partial Dependence Plot

library(iml)
model = Predictor$new(rf, data = trial_ex)
effect = FeatureEffects$new(model, method = "pdp")
effect$plot(features = "elasticity")
effect$plot(features = "cus_per_location")
effect$plot(features = "review_rating")
effect$plot(features = "total_reviews")

# Compute partial dependence data for lstat and rm
library(pdp)
pd <- partial(rf, pred.var = c("review_rating", "total_reviews"))

# Default PDP
pdp1 <- plotPartial(pd)
pdp1

# Add contour lines and use a different color palette
rwb <- colorRampPalette(c("red", "white", "blue"))
pdp2 <- plotPartial(pd, contour = TRUE, col.regions = rwb)

# 3-D surface
pdp3 <- plotPartial(pd, levelplot = FALSE, zlab = "GMV", colorkey = TRUE, 
                    screen = list(z = -20, x = -60))

pdp3
# Figure 5
grid.arrange(pdp1, pdp2, pdp3, ncol = 3)

##*************************************************************************************************************************************
#' KNOBSynC
#' 
#' @title Kernel-estimated Nonparametric Overlap-Based Syncytial Clustering
#' 
#' @description Merge clustering components using smooth estimation of the missclassification probabilities
#' 
#' @param X dataset of size n x p
#' @param kmns.results clustering solution. Default is NULL, if provided a class kmeans can be input. This can be a list with centers and cluster ids  
#' @param min.gen.overlap Minimum desired generalized overlap of Maitra (2010). Algorithm will stop when this overlap has been reached. Default is 0.00001. 
#' @param kappa  Syncytial clustering parameter. As kappa increase fewer clusters will be merge. For more details see Almodovar-Rivera and Maitra (2020).
#' @param Kmax Maximum value of the range of the number of cluster. Default is NULL, if n >= 50, Kmax = max{50, sqrt(n)} otherwise Kmax=sqrt(n)
#' @param EstK If \code{kmns.results} is NULL, which methodology will be used to estimated the number of groups, options are "jump" of Sugar and James (2003) and "KL" of Krzanoswki and Lai (1985)
#' @param kernel kernel estimator to be use to estimate missclassification probabilites. Default is reciprocal inverse gaussian (RIG), other choices see \code{kcdf}.
#' @param b Smoothing parameter to be use in the estimation of the probabilities. Default is NULL, bandwidth to be use is the one that minimize the mean integrated squared error (MISE).
#' @param inv.roles if TRUE will use gamma kernel of Kim (2013), default is Chen 2000
#' @param desired.ncores Desired number of cores to be used. Default is 2, however the function will determine min(detectCores(),desired.ncores)
#' @param ret.steps If TRUE will return all the information at each step of the algorithm
#' @param verbose If TRUE will print each step  
#' @return A list with the following
#' \begin{itemize}
#' \item KmeansSoln - Return the clustering solution based on k-means. If \code{kmns.results} was NULL, then the clustering solution will be based on EstK. If provided by the user it will be the same as the input. 
#' \item OmegaMapKmns - Overlap matrix based on the k-means solution. 
#' \item OmegaMapKNS - Overlap matrix based on KNOB-SynC solution. 
#' \item Ids - Cluster membership based on k-means.
#' \item IdsMerge - Cluster membership based on KNOB-SynC.
#' \item Groups - A list containing the Groups that were merge together.
#' \item MaxOverlap - Maximum pairwise overlap for the KNOB-SynC Overlap matrix.
#' \item MeanOverlap - Average pairwise overlap for the KNOB-SynC Overlap matrix.
#' \item GenOverlap - Generalized pairwise overlap for the KNOB-SynC Overlap matrix.
#' \item Psi - Normed residuals for the k-means solution
#' \item Fhat - Estimation of the missclassification probability for each observation
#' \item bw - Numeric value of the smoothing parameter. If the default was NULL, then the use estimated value is return, otherwise it returns the user input.
#' \end{itemize}
#' @examples 
#' \dontrun{
#' set.seed(787)
#' ## Example 1
#' data(Bullseye)
#' oo <- KNOBSynC(x = Bullseye[,-3],verbose = TRUE)
#' Bullseye$IdsKmeans <- oo$Ids
#' Bullseye$IdsKNOBSynC <- oo$IdsMerge
#' par(mfrow=c(1,3))
#' with(Bullseye,plot(x = x,y = y, col=Ids,main="True"))
#' with(Bullseye,plot(x = x,y = y, col=IdsKmeans,main="k-means"))
#' with(Bullseye,plot(x = x,y = y, col=IdsKNOBSynC,main="KNOB-SynC"))
#' ## Example 2  
#' data(Spherical7)
#' oo <- KNOBSynC(x = Spherical7[,-3],verbose = TRUE)
#' Spherical7$IdsKmeans <- oo$Ids
#' Spherical7$IdsKNOBSynC <- oo$IdsMerge
#' par(mfrow=c(1,3))
#' with(Spherical7,plot(x = x,y = y, col=Ids,main="True"))
#' with(Spherical7,plot(x = x,y = y, col=IdsKmeans,main="k-means"))
#' with(Spherical7,plot(x = x,y = y, col=IdsKNOBSynC,main="KNOB-SynC"))
#' }
#' @export
##***********************************************************************************************************************************

which.max.matrix <- function(mat) (which(x = mat == max(mat), arr.ind=T))

is.integer0 <- function(x){is.integer(x) && length(x) == 0L}

KNOBSynC <- function(x, kmns.results=NULL, min.gen.overlap = 1e-5,kappa = NULL, 
                     Kmax = NULL,EstK=NULL,kernel = "RIG", b = NULL,inv.roles=FALSE, 
                     desired.ncores=2, 
                     ret.steps = FALSE,verbose=FALSE,...)
{
  X <- as.matrix(x)
  n <- nrow(X);p <- ncol(X)
  if(verbose){
    cat(paste("#",paste(rep("=",100),collapse=""),"#\n",sep=""))
    cat(" \t \t Kernel-estimated Nonparametric Overlap-Based Syncytial Clustering  \n\n")  
  }
  if((is.null(kmns.results))) {
    EstK <- ifelse(is.null(EstK),ifelse(p^2 <= n,"jump","KL" ),match.arg(EstK,choices = c("jump","KL")))
    if(is.null(Kmax)){
      Kmax <- ifelse(n < 50, round(sqrt(n),digits = 0),max(round(sqrt(n),digits = 0),50))
    }
    if(verbose){
      cat("Printing set-up:\n")
      cat(sprintf("Performing k-means range of number of groups using %s method [1,%d]. \n",EstK,Kmax))
    }
    
    kmeans.results <- kmeans.all(x = X,maxclus = Kmax, desired.ncores = desired.ncores,...)
    if(EstK=="jump"){
      K <- which.max(unlist(kmeans.results$jump.stat))
      kmns.results <- kmeans.results$kmns.results[[K]]
      Means <- kmns.results$centers
      ids <- kmns.results$cluster
      wss <- kmns.results$tot.withinss
    }  else{
      K <- which.max(unlist(kmeans.results$kl.stat))
      kmns.results <- kmeans.results$kmns.results[[K]]
      Means <- kmns.results$centers
      ids <- kmns.results$cluster     
      wss <- kmns.results$tot.withinss
    }
  }  else{
    if(verbose){
      cat("k-means solution was provided.\n")
    }
    Means <- kmns.results$centers
    ids <- kmns.results$cluster
    K <- max(unique(ids))
  }
  
  kernel <- match.arg(kernel,choices = c("gamma","RIG","gaussian"))
  if(verbose){
    if(!is.null(b)){
      cat("Kernel: ", kernel, "\nBandwidth:", b,"\n")
      cat("Initial number of groups:", K, "\n")
    }else{
      cat("Kernel: ", kernel, "\nBandwidth will be estimated based on ruled-of-thumb for MISE. \n")
      cat("Initial number of groups:", K, "\n")
    }
  }
  if(is.null(kappa)){
    if(verbose){
      cat("Kappa was not provided following, 1,2,3,4,5 \n")
    }
    Kappa <- c(1,2,3,4,5)
  } else{
    if(verbose){
      cat(sprintf("Kappa was provided merging clusters will occur %s times generalized overlap \n", as.character(kappa)))
    }
    ##    Kappa <- length(kappa)
    Kappa <- kappa
  }
  ##************************************************
  ## compute Psi = || X_i - \mu_ik|| and pseudo Psi
  ##************************************************
  if(verbose){
    cat("Computing normed residuals and pseudo-normed residuals. \n")
  }
  desired.ncores <- min(detectCores(),desired.ncores)
  residuals.norm <- norm.res(X = X, Means = Means, ids = ids,desired.ncores=desired.ncores)
  
  psi <- residuals.norm$Psi 
  pseudo.psi <- residuals.norm$PseudoPsi
  
  ## minimum generazid overlap
  ##  min.gen.overlap <- 1/prod(dim(X))
  ##*************************************
  ##
  ## Choice of the kernel to estimate
  ## the missclasification probabilities
  ##
  ##*************************************
  if(verbose){
    cat("Estimating the missclasification probabilities using kernel estimation.\n")
  }
  if(is.null(b)){
    b <- gsl_bw_mise_cdf((psi*psi/(sum(psi*psi)/(p*(n-K)))))
  }
  
  cl <- makeCluster(desired.ncores,...)
  clusterExport(cl, list("kcdf"))
  Fhat.Psi <- t(parApply(cl = cl,X = pseudo.psi,MARGIN = 1,FUN = function(z){kcdf(x = psi,b = b,kernel=kernel,
                                                                                  xgrid=z,inv.roles=inv.roles)$Fhat}))
  stopCluster(cl)
  
  
  ##*********************************
  ##
  ## compute \hat{omega}_{kl}
  ##
  ##*********************************
  
  iter <- 1
  gen.overlap <- rep(0,K)  ## store compute generalized overlap
  max.overlap <- rep(0,K)  ## store compute max overlap
  mean.overlap <- rep(0,K)  ## store compute bar overlap
  Kstep <- rep(0,K)
  Groups.step <- list()
  Ids.step <- list()  
  Omega.lk <- array(0,dim=c(K,K))
  for(k in 1:(K-1)){
    for(l in (k+1):K){
      Omega.lk[k,l] <- 1-mean(Fhat.Psi[ids==k,l]) ## Omega_{l|k}
    }
  }
  
  # for(k in 1:K){
  #   for(l in 1:K){
  #     Omega.lk[k,l] <- 1-mean(Fhat.Psi[ids==k,l]) ## Omega_{l|k}
  #   }
  # }
  
  omega.mat <- Omega.lk
  Omega <- (t(omega.mat) + (omega.mat))
  diag(Omega) <- 1.00
  ##***************************************************
  ##
  ## this asymmetric kernel does not integrate to 1
  ## therefore some values can be larger than 1.
  ## compute generalized overlap of Maitra 2010
  ##
  ##***************************************************
  
  idsMerge <- ids
  Kmerge <- K
  Kstep[iter] <- Kmerge
  Omega.lk.merge <- Omega.lk
  Omega.initial <- Omega
  Omega.initial.merge <- Omega.lk.merge
  GG <- list()
  gen.overlap[iter] <- generalized.overlap(overlap.mat = Omega)
  max.overlap[iter] <- max(Omega[!lower.tri(Omega,diag = TRUE)])
  mean.overlap[iter] <- mean(Omega[!lower.tri(Omega,diag = TRUE)])
  Groups.step[[iter]] <- GG
  Ids.step[[iter]] <- idsMerge 
  ##************************************************************************************
  ## If the generalized overlap for the k-means clustering is very low, there's not need to merge them.
  ## Forcing them to merge will create unstable clustering.
  ## \check{\omega}/\genoverlap >= 5 is the same as Infinity see Almodovar and Maitra 2018
  ##**********************************************************************************
  if(max.overlap[iter] <= 4* gen.overlap[iter]){
    if(verbose){
      cat(sprintf("|Iteration |  K  |  Max Overlap  | Mean Overlap |  Generalized Overlap \t| \n"))
      cat(sprintf("| %d \t   | %d  |  %.6f \t |  %.6f \t|  %.6f \t        |\n",iter,Kmerge,max.overlap[iter],mean.overlap[iter],gen.overlap[iter]))
    }
    knob.sync.kappa <- list(KmeansSoln = kmns.results,OmegaMapKmns = Omega.lk, OmegaMapKNS=Omega.lk.merge,Ids = ids, IdsMerge = idsMerge, StepGroups = Groups.step,
                            Groups = GG, MaxOverlap=max.overlap[1:iter],MeanOverlap = mean.overlap[1:iter], GenOverlap = gen.overlap[1:(iter)],Psi=psi, Fhat = Fhat.Psi,bw = b)
    
    Kmerge <- max(unique(knob.sync.kappa$IdsMerge))
    if(verbose){
      cat("C =", Kmerge,"\n") 
      cat(paste("#",paste(rep("=",100),collapse=""),"#\n",sep=""))
    }
    return(knob.sync.kappa)
    
  } else {
    
    knob.sync.kappa <- list()
    for(kappa in Kappa){
      if(verbose){
        cat(sprintf("Kappa = %s,\n", as.character(kappa)))
      }
      iter <- 1
      idsMerge <- ids
      Kmerge <- K
      Kstep[iter] <- Kmerge
      GG <- list()
      gen.overlap[iter] <- generalized.overlap(overlap.mat = Omega.initial)
      max.overlap[iter] <- max(Omega.initial.merge[!lower.tri(Omega.initial.merge,diag = TRUE)])
      mean.overlap[iter] <- mean(Omega.initial.merge[!lower.tri(Omega.initial.merge,diag = TRUE)])
      Groups.step <- list()
      Groups.step[[iter]] <- unique(idsMerge)
      if(verbose){
        cat(sprintf("|Iteration |  K  |  Max Overlap  | Mean Overlap |  Generalized Overlap \t| \n"))
        cat(sprintf("| %d \t   | %d  |  %.6f \t |  %.6f \t|  %.6f \t        |\n",iter,Kmerge,max.overlap[iter],mean.overlap[iter],gen.overlap[iter]))
      }
      
      index <- which(Omega.lk > min(Omega.lk[Omega.lk  > kappa*gen.overlap[iter]]),arr.ind = TRUE)
      if(length(index) > 0){
        index.order <- index[order(index[,1]),]
        if(!is.null(dim(index.order))){
          idstobeMerge <- unique(index.order[,1])
        } else{
          index.order <- t(as.matrix(index.order))
          idstobeMerge <- unique(index.order[,1])
        }
        
        index <- index.order[,2]
        names(index) <- paste(index.order[,1])
        
        idstobeMerge <- (as.numeric(names(index)))
        ids.unique <- unique(index)
        Kp <- length(ids.unique)
        K.ids <- length(index)
        group.merge <- NULL
        idsMerge[idsMerge == index[K.ids]] <- idstobeMerge[K.ids]
        ggMerge <- c(index[K.ids],idstobeMerge[K.ids])
        group.merge <- c(group.merge,ggMerge)
        GG[[1]] <- group.merge
        
        for(i in (K.ids-1):1){
          ggMerge <- c(index[i],idstobeMerge[i])
          
          index.GG <- which(unlist(lapply(lapply(GG,function(z) ggMerge %in% z),function(w) any(w==TRUE))))
          
          if(is.integer0(index.GG)){
            group.merge <- NULL
            ng <- length(GG)
            GG[[ng+1]] <- ggMerge
            group.merge <- c(group.merge,ggMerge)
            group.merge <- unique(group.merge)
            GG[[ng+1]] <- group.merge
          } else{
            if(length(index.GG) > 1){
              np <- length(index.GG)
              sort.index.GG <- sort(index.GG)
              min.index.GG <- min(sort.index.GG)
              for(j in sort.index.GG[sort.index.GG>min.index.GG]){
                GG[[min.index.GG]] <- unique(c(GG[[min.index.GG]],GG[[j]]))
                GG <- GG[-j]
                if(length(GG)==1){
                  break;
                }
              }
            } else{
              group.merge <- GG[[index.GG]]
              group.merge <- c(group.merge,ggMerge)
              group.merge <- unique(group.merge)
              GG[[index.GG]] <- group.merge
            }
          }
        }
        
        ## unique members
        unique.g <- unique(ids)
        uni.g <- unique.g[!(unique.g %in% unlist(GG))]
        if(!is.integer0(uni.g)){
          N.GG <- length(GG)
          for(i in 1:length(uni.g)){
            GG[[N.GG+i]] <- uni.g[i]
          }
        }
        
        Kp <- length(GG)
        
        for(i in 1:Kp){
          ids.of.GG <- GG[[i]]
          nk.gg <- length(ids.of.GG)
          for(j in 1:nk.gg){
            idsMerge[idsMerge==ids.of.GG[j]] <- K+i # to avoid any conflict with ids
          }
        }
        
        idsMerge[idsMerge > K] <- idsMerge[idsMerge > K] - K
        Kmerge <- length(names(table(idsMerge)))
        
        idstobeMerge <- as.numeric(names(table(idsMerge)))
        for(i in 1:Kmerge){
          idsMerge[idsMerge==idstobeMerge[i]] <- i
        }
        ##*********************************
        ## Compute omega_{C_k C_l}
        ##********************************
        Omega.lk.merge <- array(0,dim=c(Kmerge,Kmerge))
        
        for(k in 1:(Kmerge-1)){
          for(l in (k+1):(Kmerge)){
            ids.of.GG <- GG[[l]]
            nk <- length(ids.of.GG)
            max.omega.gg <- rep(0,nk)
            for(j in 1:nk){
              max.omega.gg[j] <- (1-mean(apply(as.matrix(Fhat.Psi[ids == ids.of.GG[j],GG[[k]]]),1,min)))^nk
            }
            Omega.lk.merge[k,l] <- max(max.omega.gg)
          }
        }
        
        iter <- iter+1
        Kstep[iter] <- Kmerge
        omega.mat <- Omega.lk.merge
        Omega <- (t(omega.mat) + (omega.mat))
        diag(Omega) <- 1.00
        
        gen.overlap[iter] <- generalized.overlap(overlap.mat = Omega)
        max.overlap[iter] <- max(Omega.lk.merge[!lower.tri(Omega.lk.merge,diag = TRUE)])
        mean.overlap[iter] <- mean(Omega.lk.merge[!lower.tri(Omega.lk.merge,diag = TRUE)])
        if(verbose){
          cat(sprintf("| %d \t   | %d  |  %.6f \t |  %.6f \t|  %.6f \t        |\n",iter,Kmerge,max.overlap[iter],mean.overlap[iter],gen.overlap[iter]))
        }
        Groups.step[[iter]] <- GG
        Ids.step[[iter]] <- idsMerge 
        while((max.overlap[iter] > kappa*gen.overlap[iter]) & (gen.overlap[iter] > min.gen.overlap) & (gen.overlap[iter] <= gen.overlap[iter-1])) {
          ##***********************************************************
          ##
          ## If merging occurs, then generalized overlap will start
          ## decreasing. If we merge cluster with the highest overlap.
          ##
          ##***********************************************************
          min.merge <- Omega.lk.merge[Omega.lk.merge > kappa*gen.overlap[iter]]
          if((length(min.merge) == 1)&(length(min.merge) != 0) ){
            index <- which(Omega.lk.merge == min.merge,arr.ind = TRUE)
          } else{
            index <- which(Omega.lk.merge > min(min.merge),arr.ind = TRUE)
          }
          index.order <- index[order(index[,1]),]
          if(!is.null(dim(index.order))){
            idstobeMerge <- unique(index.order[,1])
          } else{
            index.order <- t(as.matrix(index.order))
            idstobeMerge <- unique(index.order[,1])
          }
          
          index <- index.order[,2]
          names(index) <- paste(index.order[,1])
          idstobeMerge <- (as.numeric(names(index)))
          ids.unique <- unique(index)
          Kp <- length(ids.unique)
          K.ids <- length(index)
          
          if(length(index)> 0){
            max.ggMerge <- rep(0,K.ids)
            for(i in (K.ids):1){
              ggMerge <- c(index[i],idstobeMerge[i])
              min.ggMerge <- min(ggMerge)
              max.ggMerge[i] <- max(ggMerge)
              GG[[min.ggMerge]] <- unique(c(GG[[min.ggMerge]],GG[[max.ggMerge[i]]]))
              GG[[max.ggMerge[i]]] <- NA
              idsMerge[idsMerge == max.ggMerge[i]] <- min.ggMerge
            }
            
            GG <- GG[-max.ggMerge]
            if(any(unlist(lapply(GG,is.na)))){
              GG <- lapply(GG, function(x) x[!is.na(x)])
            }
            Kmerge <- length(names(table(idsMerge)))
            idstobeMerge <- as.numeric(names(table(idsMerge)))
            
            for(i in 1:Kmerge){
              idsMerge[idsMerge==idstobeMerge[i]] <- i
            }
            
            
            ##**************************************
            ##
            ## generalized overlap is defined as
            ## omega = (\lambda_{(1)}-1)/(K-1)
            ##
            ## Compute overlap measure
            ## \hat{\omega}_{C_k C_l}
            ## Almodovar and Maitra 2018
            ##
            ##*********************************
            Omega.lk.merge <- array(0,dim=c(Kmerge,Kmerge))
            if(Kmerge==1){
              ids.of.GG <- GG[[1]]
              nk <- length(ids.of.GG)
              max.omega.gg <- rep(0,nk)
              
              Omega.lk.merge[1,1] <- 1
              iter <- iter+1
              Kstep[iter] <- Kmerge
              gen.overlap[iter] <- 1 ## compute generalized overlap
              max.overlap[iter] <- 1 ## compute maximum overlap
              mean.overlap[iter] <- 1 ## compute mean overlap            
            } else{
              for(k in 1:(Kmerge-1)){
                for(l in (k+1):(Kmerge)){
                  ids.of.GG <- GG[[l]]
                  nk <- length(ids.of.GG)
                  max.omega.gg <- rep(0,nk)
                  for(j in 1:nk){
                    max.omega.gg[j] <- (1-mean(apply(as.matrix(Fhat.Psi[ids == ids.of.GG[j],GG[[k]]]),1,min)))^nk
                  }
                  Omega.lk.merge[k,l] <- max(max.omega.gg)
                }
              }
              
              iter <- iter+1
              Kstep[iter] <- Kmerge
              omega.mat <- Omega.lk.merge
              Omega <- (t(omega.mat) + (omega.mat))
              diag(Omega) <- 1
              gen.overlap[iter] <- generalized.overlap(overlap.mat = Omega) ## compute generalized overlap
              max.overlap[iter] <- max(Omega.lk.merge[!lower.tri(Omega.lk.merge,diag = TRUE)]) ## compute maximum overlap
              mean.overlap[iter] <- mean(Omega.lk.merge[!lower.tri(Omega.lk.merge,diag = TRUE)]) ## compute mean overlap
            }
            Groups.step[[iter]] <- GG
            Ids.step[[iter]] <- idsMerge 
            if(verbose){
              cat(sprintf("| %d \t   | %d  |  %.6f \t |  %.6f \t|  %.6f \t        |\n",iter,Kmerge,max.overlap[iter],mean.overlap[iter],gen.overlap[iter]))
              
            }
          } else{
            break;
          }
        }
      }
      
      Omega.lk2 <- Omega.lk
      Omega.lk2 <- Omega.lk2+ t(Omega.lk2)
      diag(Omega.lk2) <- 1
      
      Omega.lk2.merge <- Omega.lk.merge
      Omega.lk2.merge <- Omega.lk2.merge+ t(Omega.lk2.merge)
      diag(Omega.lk2.merge) <- 1
      if(ret.steps){
        names(Groups.step) <- paste("Step",1:iter,sep="")
        names(Ids.step) <- paste("Step",1:iter,sep="")
        knob.sync.kappa[[kappa]] <- list(KmeansSoln = kmns.results,OmegaMapKmns = Omega.lk2,
                                         OmegaMapKNS=Omega.lk2.merge,Ids = ids, IdsMerge = idsMerge, 
                                         GroupsStep = Groups.step,  IdsStep = Ids.step,
                                         Groups = GG, MaxOverlap=max.overlap[1:iter],
                                         MeanOverlap = mean.overlap[1:iter],
                                         GenOverlap = gen.overlap[1:(iter)],
                                         Psi=psi, Fhat = Fhat.Psi, bw = b)
      } else{
        knob.sync.kappa[[kappa]] <- list(KmeansSoln = kmns.results,OmegaMapKmns = Omega.lk2,
                                         OmegaMapKNS=Omega.lk2.merge,Ids = ids, IdsMerge = idsMerge, 
                                         Groups = GG, MaxOverlap=max.overlap[1:iter],
                                         MeanOverlap = mean.overlap[1:iter],
                                         GenOverlap = gen.overlap[1:(iter)],
                                         Psi=psi, Fhat = Fhat.Psi, bw = b)
      }
    }
    ##
    ## choose best kappa solution by
    ## finding the minimum generalized overlap
    ##
    if(length(Kappa) > 1){
      kappa.min <- which.min(sapply(knob.sync.kappa,function(z) z$GenOverlap[length(z$GenOverlap)]))
    } else{
      kappa.min <- which(sapply(knob.sync.kappa,is.null)==FALSE)
    }
    Kmerge <- max(unique(knob.sync.kappa[[kappa.min]]$IdsMerge))
    if(verbose){
      cat("C =", Kmerge,"\n") 
    }
    cat(paste("#",paste(rep("=",100),collapse=""),"#\n",sep=""))
  }
  knob.sync.kappa[[kappa.min]]
}

KNOBSynC(ex_imputed, kmax = sqrt(nrow(y)), kmns.results = 9)

### Lasso regression ###
library(glmnet)
install.packages("gglasso")
library(gglasso)
gmv = as.vector(trial_ex$gmv_for_case)
predictors = model.matrix(gmv_for_case ~. , data = trial_ex)
predictors = predictors[,-1]

group.v = vector()
for (i in 1:ncol(predictors)) {
  if ( i <= 27) {
    group.v[i] = 1 
  } else if ( i <= 86) {
    group.v[i] = 2
  } else if ( i <= 87) {
    group.v[i] = 3
  } else if ( i <= 88) {
    group.v[i] = 4
  } else if ( i <= 91) {
    group.v[i] = 5
  } else if ( i <= 92) {
    group.v[i] = 6
  } else if ( i <= 93) {
    group.v[i] = 7
  }  else {
    group.v[i] = 8
  }  
}

group.v 

# 10-fold cross validation for lasso regression 
result.lasso.cv <- cv.glmnet(predictors, gmv, alpha = 1, 
                             lambda = 10^seq(-2, 5, length.out = 50), nfolds = 10)  
print(result.lasso.cv$lambda.min)      # Best cross validated lambda
print(result.lasso.cv$lambda.1se)      # Conservative estimate of best lambda (1 stdev) 


# 10-fold cross validation for group lasso regression
set.seed(777)
gr_cv = cv.gglasso(predictors, gmv, group=group.v, 
                   loss="ls", pred.loss="L1", 
                   intercept = F, nfolds=10)

print(gr_cv$lambda.min)     
print(gr_cv$lambda.1se) 

# To plot Root Mean Squared Error (RMSE) to be on the same scale as y:
gr_cv$cvm <- gr_cv$cvm^0.5
gr_cv$cvup <- gr_cv$cvup^0.5
gr_cv$cvlo <- gr_cv$cvlo^0.5
plot(gr_cv, ylab = "Root Mean-Squared Error") 


# Final run with best cross validated lambda
result.lasso.cv$lambda.min
result.lasso.best <- glmnet(predictors, gmv, alpha = 1, 
                            lambda = result.lasso.cv$lambda.1se)   
round(result.lasso.best$beta, digits = 2)

# Final run with best cross validated lambda in group lasso
gr = gglasso(predictors, gmv, lambda = gr_cv$lambda.min,
             group = group.v, loss="ls",
             intercept = F)
round(gr$beta, digits = 2)

### BORUTA ###
hante_telione<-which(ex$primary_cuisine=="other")
hante_ksimerose<-ex[hante_telione,]
mean(hante_ksimerose$gmv_for_case)
which(ex[263,]>hante_ksimerose$gmv_for_case)
which()
set.seed(777)
library(Boruta)
x<-Boruta(gmv_for_case ~.,data=trial_ex,doTrace=3)


##################shapiro-wilk test about normality(null hypothesis corresponds to normal distribution)
shapiro.test(train.data$review_rating)$p
hist(train.data$review_rating)
shapiro.test(train.data$elasticity)$p
shapiro.test(train.data$cus_per_location)$p
shapiro.test(train.data$total_reviews)$p
hist(train.data$total_reviews)

ggplot(test.data,                                    
       aes(x = exp(preds2),
           y = gmv_for_case)) +
  geom_point() +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 2)
hist(exp(preds2))
hist(abs((test.data$gmv_for_case-exp(preds2))/(test.data$gmv_for_case)))

plot(reg2)