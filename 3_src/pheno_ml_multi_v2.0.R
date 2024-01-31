pacman::p_load(raster, rgdal, parallel, snow, terra, tools, RStoolbox)
library(tidyverse)
library(caret)
library(zoo)
library(ggcorrplot)
library(car)
library(gridExtra)
#library(terra)
#library(raster)
theme_set(theme_classic())

##################################################################
# 2nd method by random sampling

# Load the data
jo1920 <- read.csv("./2_data/3_processed/train/final/jo1920.csv")
set.seed(7)

validindex <- createDataPartition(jo1920f$foliation, p = 0.80, list = FALSE)
jo_valid <- jo1920[-validindex, ]
jo_train <- jo1920[validindex, ]
#View(jotrain)
#View(jovalid)
jo19 <- read.csv("./2_data/3_processed/train/final/jo19.csv")
jo20 <- read.csv("./2_data/3_processed/train/final/jo20.csv")
jo1920 <- read.csv("./2_data/3_processed/train/final/jo1920.csv")
jo21 <- read.csv("./2_data/3_processed/train/final/jo21.csv")
jo2021 <- read.csv("./2_data/3_processed/train/final/jo2021.csv")
joall <- read.csv("./2_data/3_processed/train/final/joall.csv")
##################################################
##################################################
#GAM phase
# change to gam
set.seed(77)
tc <- trainControl(method = "cv",
                   number = 10)
#m <- train(phase ~ gndvi,
#m <- train(phase ~ ndrei,
m <- train(phase ~ ndvi,
#m <- train(phase ~ ndre,
#m <- train(phase ~ gcc,
#m <- train(phase ~ ndrei + gcc,
#m <- train(phase ~ ndwi + gcc,
#m <- train(phase ~ ndre + ndwi,
#m <- train(phase ~ gcc_uc,
           data = jo1920, 
           trControl=tc, 
           #method='gamboost',
           method='gam',
           na.action = na.pass,
           preProc=c("center","scale"))
predictions <- m %>% predict(jo21)
df <- data.frame(RMSE = caret::RMSE(predictions, jo21$phase),
                 MAE = caret::MAE(predictions, jo21$phase),
                 R2 = caret::R2(predictions, jo21$phase))

df

#GAM foliation
set.seed(77)
tc <- trainControl(method = "cv",
                   number = 10)
#m <- train(foliation ~ gndvi,
#m <- train(foliation ~ ndrei,
m <- train(foliation ~ ndvi,
#m <- train(foliation ~ ndre,
#m <- train(foliation ~ gcc,
#m <- train(foliation ~ ndrei + gcc,
#m <- train(foliation ~ ndwi + gcc,
#m <- train(foliation ~ ndre + ndwi,
#m <- train(foliation ~ gcc_uc,
           data = jo1920, 
           trControl=tc, 
           method='gam',
           na.action = na.pass,
           preProc=c("center","scale"))
predictions <- m %>% predict(jo21)
df <- data.frame(RMSE = caret::RMSE(predictions, jo21$foliation),
                 MAE = caret::MAE(predictions, jo21$foliation),
                 R2 = caret::R2(predictions, jo21$foliation))

df

##################################################
#GAMBOOST phase
# change to gam
set.seed(77)
tc <- trainControl(method = "cv",
                   number = 10)
#m <- train(phase ~ gndvi,
#m <- train(phase ~ ndrei,
#m <- train(phase ~ ndvi,
#m <- train(phase ~ ndre,
#m <- train(phase ~ gcc,
#m <- train(phase ~ ndrei + gcc,
#m <- train(phase ~ ndwi + gcc,
#m <- train(phase ~ ndre + ndwi,
m <- train(phase ~ gcc_uc,
           data = jo1920, 
           trControl=tc, 
           method='gamboost',
           na.action = na.pass,
           preProc=c("center","scale"))
predictions <- m %>% predict(jo21)
df <- data.frame(RMSE = caret::RMSE(predictions, jo21$phase),
                 MAE = caret::MAE(predictions, jo21$phase),
                 R2 = caret::R2(predictions, jo21$phase))

df

#GAMBOOST foliation
set.seed(77)
tc <- trainControl(method = "cv",
                   number = 10)
#m <- train(foliation ~ gndvi,
#m <- train(foliation ~ ndrei,
#m <- train(foliation ~ ndvi,
#m <- train(foliation ~ ndre,
#m <- train(foliation ~ gcc,
#m <- train(foliation ~ ndrei + gcc,
#m <- train(foliation ~ ndwi + gcc,
#m <- train(foliation ~ ndre + ndwi,
m <- train(foliation ~ gcc_uc,
           data = jo1920, 
           trControl=tc, 
           #method='gamboost',
           method='gam',
           na.action = na.pass,
           preProc=c("center","scale"))
predictions <- m %>% predict(jo21)
df <- data.frame(RMSE = caret::RMSE(predictions, jo21$foliation),
                 MAE = caret::MAE(predictions, jo21$foliation),
                 R2 = caret::R2(predictions, jo21$foliation))

df

#######################################################################
#################################################################################
################################################################################
#---------------------------------------------------------------
#Boosting 

#install.packages("gbm")
library(gbm)

# Boosting phase
tc <- trainControl(method = "cv",
                   number = 10)
set.seed(77)
#m <- train(phase ~ gndvi,
#m <- train(phase ~ ndrei,
#m <- train(phase ~ ndvi,
#m <- train(phase ~ ndre,
#m <- train(phase ~ gcc,
#m <- train(phase ~ ndrei + gcc,
#m <- train(phase ~ ndwi + gcc,
m <- train(phase ~ ndre + ndwi,
#m <- train(phase ~ gcc_uc,
           data = jo1920,
           #data = joall,
           trControl = tc,
           method = "gbm",
           tuneGrid = expand.grid(
             interaction.depth = 1:3,
             n.trees = seq(20, 200, 10), 
             shrinkage = 0.1,
             n.minobsinnode = 10),
           verbose = FALSE,
           na.action=na.exclude,
           preProc=c("center","scale"))
#saveRDS(m, "gbm_phase_ndvi.rds")
#gcc_uc <- readRDS("model.rds")
# Model performance
predictions <- m %>% predict(jo21)
df <- data.frame(RMSE = caret::RMSE(predictions, jo21$phase),
                 MAE = caret::MAE(predictions, jo21$phase),
                 R2 = caret::R2(predictions, jo21$phase))

df

#---------------------------------------------------------------
#Boosting 

#install.packages("gbm")
library(gbm)

# Boosting foliaiton
tc <- trainControl(method = "cv",
                   number = 10)
set.seed(77)
#m <- train(foliation ~ gndvi,
#m <- train(foliation ~ ndrei,
#m <- train(foliation ~ ndvi,
#m <- train(foliation ~ ndre,
#m <- train(foliation ~ gcc,
#m <- train(foliation ~ ndrei + gcc,
#m <- train(foliation ~ ndwi + gcc,
#m <- train(foliation ~ ndre + ndwi,
m <- train(foliation ~ gcc_uc,
           data = jo1920,
           #data = joall,
           trControl = tc,
           method = "gbm",
           tuneGrid = expand.grid(
             interaction.depth = 1:3,
             n.trees = seq(20, 200, 10), 
             shrinkage = 0.1,
             n.minobsinnode = 10),
           verbose = FALSE,
           na.action=na.exclude,
           preProc=c("center","scale"))
#saveRDS(m, "gbm_phase_ndvi.rds")
#gcc_uc <- readRDS("model.rds")
# Model performance
predictions <- m %>% predict(jo21)
df <- data.frame(RMSE = caret::RMSE(predictions, jo21$foliation),
                 MAE = caret::MAE(predictions, jo21$foliation),
                 R2 = caret::R2(predictions, jo21$foliation))

df
####predict raster
ls <- list.files("./2_data/3_processed/climate", pattern = "RGB", full.names = TRUE)
lsr <- ls[ c(2, 4, 5, 6, 7, 9)]
lsr
#i <- 1
#lsr[1]
cores <- makeCluster(detectCores() - 1)
beginCluster(cores)
for (i in 1:6) {
  ra <- rast(lsr[i])
  names(ra[[5]]) <- "gcc_uc"
  predict_m <- terra::predict(object = ra[["gcc_uc"]], model = m, type = 'raw', na.rm = TRUE)
  str <- sub("\\_w_climate.tif$", "", paste0("./4_analysis/output/", basename(lsr[i])))
  terra::writeRaster(predict_m, paste0(str, "_fol_gcc_uc.tif"), overwrite = TRUE)
}
endCluster()

csv <- read.csv("C:/root/projects/phenospring/2_data/3_processed/train/train2021fix.csv")
csvsub <- subset(csv, csv$tree == 385)
csvdoy <- subset(csv, csv$doy == 131)
csvsub
csvdoy
#################################################
# Boosting phase
names(jo_trainp)
tc <- trainControl(method = "cv",
                   number = 10)
set.seed(77)
#m <- train(phase ~ gndvi,
#m <- train(phase ~ ndrei,
#m <- train(phase ~ ndvi,
#m <- train(phase ~ ndre,
# <- train(phase ~ gcc,
#m <- train(phase ~ gndvi + gcc, # 0.5
#m <- train(phase ~ ndrei + gcc, # 0.46
#m <- train(phase ~ ngrdi + ndrei, # 0.44
m <- train(phase ~ gcc_uc,
           data = jo1920,
           trControl = tc,
           method = "gbm",
           tuneGrid = expand.grid(
             interaction.depth = 1:3,
             n.trees = seq(20, 200, 10), 
             shrinkage = 0.1,
             n.minobsinnode = 10),
           verbose = FALSE,
           na.action=na.exclude,
           preProc=c("center","scale"))

#saveRDS(m, "gcc_phall.rds")
predictions <- m %>% predict(jo21)
m
#options(digits=2)
df <- data.frame(RMSE = caret::RMSE(predictions, jo21$phase),
                 MAE = caret::MAE(predictions, jo21$phase),
                 R2 = caret::R2(predictions, jo21$phase))
df
###################################################################################
###polynomial
i <- "ndvi"
# Build the model
for(j in 1:5){
  #j <- 1
  train_control <- trainControl(method='CV')
  v <- i
  f <- paste0("phase ~ poly(", v, ", ", j, ", raw = TRUE)")
  m <- train(as.formula(f), data = jo1920, trControl=train_control, method='glm', na.action = na.pass)
  assign(paste0("m", j), m)
  #er[i] <- (models$results$RMSE)^2
}
#?caret::RMSE
pred1 <- m1 %>% predict(jo21)        
pred2 <- m2 %>% predict(jo21)
pred3 <- m3 %>% predict(jo21)
pred4 <- m4 %>% predict(jo21)
pred5 <- m5 %>% predict(jo21)

# Model performance
df  <- data.frame()
df1 <- data.frame(Model = "1st Degree",
                  RMSE = caret::RMSE(pred1, jo21$phase),
                  MAE = caret::MAE(pred1, jo21$phase),
                  R2 = caret::R2(pred1, jo21$phase))
df2 <- data.frame(Model = "2nd Degree",
                  RMSE = caret::RMSE(pred2, jo21$phase),
                  MAE = caret::MAE(pred2, jo21$phase),
                  R2 = caret::R2(pred2, jo21$phase))
df3 <- data.frame(Model = "3rd Degree",
                  RMSE = caret::RMSE(pred3, jo21$phase),
                  MAE = caret::MAE(pred3, jo21$phase),
                  R2 = caret::R2(pred3, jo21$phase))
df4 <- data.frame(Model = "4th Degree",
                  RMSE = caret::RMSE(pred4, jo21$phase),
                  MAE = caret::MAE(pred4, jo21$phase),
                  R2 = caret::R2(pred4, jo21$phase))
df5 <- data.frame(Model = "5th Degree",
                  RMSE = caret::RMSE(pred5, jo21$phase),
                  MAE = caret::MAE(pred5, jo21$phase),
                  R2 = caret::R2(pred5, jo21$phase))

df <- rbind(df1, df2, df3, df4, df5)
options(digits=2)
print(df)

ggplot(jo21, aes(ndvi, phase)) + 
  geom_point(size = 1) +
  stat_smooth(method = lm, formula = y ~ poly(x, 1, raw = TRUE), se = FALSE,
              aes(colour = "blue", linetype = "dotted"), size = 0.8) +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE), se = FALSE,
              aes(colour = "red", linetype = "dotted"), size = 0.8) +
  stat_smooth(method = lm, formula = y ~ poly(x, 3, raw = TRUE), se = FALSE,
              aes(colour = "black", linetype = "solid"), size = 0.8) +
  stat_smooth(method = lm, formula = y ~ poly(x, 4, raw = TRUE), se = FALSE,
              aes(colour = "green", linetype = "dotted"), size = 1) +
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE), se = FALSE,
              aes(colour = "brown", linetype = "dotted"), size = 1) +
  scale_color_manual(name="",
                     breaks = c("blue", "red", "black", "green", "brown"),
                     values = c("blue", "red", "black", "green", "brown"),
                     labels = c("1st Order Polynomial", "2nd Order Polynomial", "3rd Order Polynomial", 
                                "4th Order Polynomial", "5th Order Polynomial")) +
  #guides(linetype=FALSE) +
  scale_linetype_manual(breaks = c("dotted", "dotted", "solid", "dotted", "dotted"),
                        values = c("dotted", "dotted", "solid", "dotted", "dotted"),
                        labels = c("1st", "2nd", "3rd", "4th", "5th"),
                        guide = "none") +
  annotation_custom(tableGrob(df, rows=NULL, theme = ttheme_minimal(base_size = 8, base_colour = "black")),
                    xmin = 0.38, xmax=0.5, ymin= 2.8, ymax= 4) + 
  theme(legend.position = c(0.2, 0.9))


ggsave(paste0("./4_analysis/figures/phase_poly_", i, "_19_20_", ".jpg"), dpi = 300)

##################################
#FINAL POly
###polynomial
i <- "ndvi"
# Build the model
for(j in 1:5){
  #j <- 1
  train_control <- trainControl(method='CV')
  v <- i
  f <- paste0("phase ~ poly(", v, ", ", j, ", raw = TRUE)")
  m <- train(as.formula(f), data = jo2021, trControl=train_control, method='glm', na.action = na.pass)
  assign(paste0("m", j), m)
  #er[i] <- (models$results$RMSE)^2
}
#?caret::RMSE
# saveRDS(m2, "./4_analysis/models/poly2_ndvi_20_21.rds")   
# saveRDS(m2, "./4_analysis/models/poly2_ndvi_20_21.rds")   
# saveRDS(m3, "./4_analysis/models/poly3_ndvi_20_21.rds")   
# 
# saveRDS(m2, "./4_analysis/models/poly2_ndvi_all.rds")   
# saveRDS(m2, "./4_analysis/models/poly2_ndvi_all.rds")   
# saveRDS(m3, "./4_analysis/models/poly3_ndvi_all.rds")   
# 
# saveRDS(m2, "./4_analysis/models/poly2_ndvi_19_20.rds")   
# saveRDS(m2, "./4_analysis/models/poly2_ndvi_19_20.rds")   
# saveRDS(m3, "./4_analysis/models/poly3_ndvi_19_20.rds")   









#finish###########
###############################################
####### jo1920
tc <- trainControl(method = "cv",
                   number = 10)
set.seed(77)
m <- train(phase ~ ndre,
#m <- train(phase ~ poly(ndre, 3, raw = TRUE),
#m <- train(phase ~ ndre + airtemp,
#m <- train(phase ~ gcc_uc,
#m <- train(phase ~ poly(gcc_uc, 3, raw = TRUE),
#m <- train(phase ~ gcc_uc + airtemp,
#m <- train(phase ~ ndre + gcc_uc,
data = jo1920,
trControl = tc,
method = "gbm",
tuneGrid = expand.grid(
  interaction.depth = 1:3,
  n.trees = seq(20, 200, 10), 
  shrinkage = 0.1,
  n.minobsinnode = 10),
verbose = FALSE,
na.action=na.exclude,
preProc=c("center","scale"))

predictions <- m %>% predict(jo21)
#options(digits=2)
df <- data.frame(RMSE = caret::RMSE(predictions, jo21$phase),
                 MAE = caret::MAE(predictions, jo21$phase),
                 R2 = caret::R2(predictions, jo21$phase))
df


###############################################################################
# create full models
View(joall)
#Boosting phase
joall
tc <- trainControl(method = "cv",
                   number = 10)
set.seed(77)
          #m <- train(phase ~ gcc_uc,
          m <- train(phase ~ ndre,
          #m <- train(foliation ~ gcc_uc, 
          #m <- train(foliation ~ ndre,
           
           data = joall,
           trControl = tc,
           method = "gbm",
           tuneGrid = expand.grid(
             interaction.depth = 1:3,
             n.trees = seq(20, 200, 10), 
             shrinkage = 0.1,
             n.minobsinnode = 10),
           verbose = FALSE,
           na.action=na.exclude,
           preProc=c("center","scale"))

#saveRDS(m, "./4_analysis/models/gcc_uc_phase.rds")
saveRDS(m, "./4_analysis/models/ndre_phase.rds")
#saveRDS(m, "./4_analysis/models/gcc_uc_foliation.rds")
#saveRDS(m, "./4_analysis/models/ndre_foliation.rds")



####predict raster
ls <- list.files("./2_data/3_processed/climate", pattern = "altum", full.names = TRUE)
lsr <- ls[ c(2, 4, 5, 6, 7, 9)]
lsr
#i <- 1
lsr[1]
cores <- makeCluster(detectCores() - 1)
beginCluster(cores)
for (i in 1:6) {
  ra <- rast(lsr[i])
  #names(ra[[5]]) <- "gcc_uc"
  predict_m <- terra::predict(object = ra[["ndre"]], model = m, type = 'raw', na.rm = TRUE)
  str <- sub("\\_w_climate.tif$", "", paste0("./4_analysis/output/", basename(lsr[i])))
  terra::writeRaster(predict_m, paste0(str, "_phase_ndre.tif"), overwrite = TRUE)
}
endCluster()







###################################
# predictions <- m %>% predict(ma20)
# #options(digits=2)
# df <- data.frame(RMSE = caret::RMSE(predictions, ma20$phase),
#                  MAE = caret::MAE(predictions, ma20$phase),
#                  R2 = caret::R2(predictions, ma20$phase))
# df

#ra <- rast("./2_data/3_processed/climate/0241_altum_2021_128_w_climate.tif")
#ra <- rast("./2_data/3_processed/climate/0242_altum_2021_131_w_climate.tif")
ra <- rast("./2_data/3_processed/climate/0245_altum_2021_139_w_climate.tif")
#ra <- rast("./2_data/3_processed/climate/0241_altum_2021_128_w_climate.tif")
#plot(ra)
cores <- makeCluster(detectCores() - 1)
beginCluster(cores)
predict_m <- terra::predict(object = ra[[11]],
                             model = m, type = 'raw', na.rm = TRUE)
endCluster()
plot(predict_m)
#writeRaster(predict_m, "./4_analysis/output/0241_altum_2021_128_phase.tif", overwrite = TRUE)
#writeRaster(predict_m, "./4_analysis/output/0242_altum_2021_131_phase.tif", overwrite = TRUE)
writeRaster(predict_m, "./4_analysis/output/0245_altum_2021_139_phase.tif", overwrite = TRUE)
library(RColorBrewer)
#https://www.datanovia.com/en/blog/the-a-z-of-rcolorbrewer-palette/
#RDYlGn
cuts=c(0, 1.0, 2.0, 3.0, 4.0, 5.0) #set breaks
pal <- colorRampPalette(c("red", "orange", "yellow", "yellowgreen", "darkgreen"))
#pal <- colorRampPalette("RdGn")
plot(predict_m, breaks = cuts, col = pal(5), axes = FALSE)
#plot(predict_m, col = pal(9))
terra::plot(ls[[1]])
ls <- list.files("./4_analysis/output", pattern = "*.tif", full.names = TRUE)
ls <- lapply(ls, rast)

tiff(filename = "./4_analysis/figures/map.tif", units="in", width=11, height=8.5, res = 300)
#par(mfrow = c(1,3))
layout(matrix(c(1,2, 3, 4, 4, 4), ncol=3, byrow=TRUE), heights = c(0.5,0.1))
layout.show(n=4) # number of subplots to show on the layout
for (i in 1:3) {
  terra::plot(ls[[i]], axes = FALSE, xlab = "2019/2020", rm.na = TRUE, legend = FALSE)  
  #plot(ls[i], main = names(jo1920)[i], xlab = "2019/2020", rm.na = TRUE)  
}
#plot(1, type = "n", axes=F, xlab="", ylab="") # Create empty plot
legend("top", breaks = cuts, col = pal(5), axes = FALSE)


n = 3
#par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend('bottom',legend = c("0", "1.0", "2.0", "3.0", "4.0", "5.0"), col = c("red", "orange", "yellow", "yellowgreen", "darkgreen"))
#legend('bottom',legend = c("Fabricated Metal", "Iron and Steel", "Paper", "Beverages", "Tobacco"), col = c("blue","black", "green", "orange", "pink"), lwd = 5, xpd = TRUE, horiz = TRUE, cex = 1, seg.len=1, bty = 'n')
# xpd = TRUE makes the legend plot to the figure
dev.off()





#











#ggsave("./4_analysis/figures/phase_gbm_residuals.jpg", dpi = 300) 
# Boosting Foliation
tc <- trainControl(method = "cv",
                   number = 10)

m <- train(foliation ~ ndrei,
           data = jo_train,
           trControl = tc,
           method = "gbm",
           tuneGrid = expand.grid(
              interaction.depth = 1:3,
              n.trees = seq(20, 200, 10), 
              shrinkage = 0.1,
              n.minobsinnode = 10),
           verbose = FALSE)

m

pred <- m %>% predict(jo_valid)
# Model performance
data.frame(RMSE = caret::RMSE(predictions, jo_valid$foliation),
           R2 = caret::R2(predictions, jo_valid$foliation),
           MAE = caret::MAE(predictions, jo_valid$foliation))

#########################################################################
####FINAL###############################################################
#Boosting 


#library(gbm)

# Boosting Phases
tc <- trainControl(method = "cv",
                   number = 10)

set.seed(77)
#m <- train(phase ~ ndvi + ndre + ndrei + airtemp + zmean,
#m <- train(phase ~ ndvi + ndre + ndrei + zmean,
#m <- train(phase ~ ngrdi + gcc + airtemp,
m <- train(phase ~ ngrdi + gcc,
           data = jo,
           trControl = tc,
           method = "gbm",
           tuneGrid = expand.grid(
             interaction.depth = 1:3,
             n.trees = seq(20, 200, 10), 
             shrinkage = 0.1,
             n.minobsinnode = 10),
           verbose = FALSE)

predictions <- m %>% predict(testp4)
#predictions <- m %>% predict(jo_valid)
# Model performance
data.frame(RMSE = caret::RMSE(predictions, jo_valid$phase),
           MAE = caret::MAE(predictions, jo_valid$phase),
           R2 = caret::R2(predictions, jo_valid$phase))


importance <- varImp(m, useModel = FALSE)
print(importance)
plot(importance)




####
####################################################################
####################################################################
# to try
# https://www.r-bloggers.com/2020/11/r-xgboost-regression/
# https://www.r-bloggers.com/2021/04/how-to-plot-xgboost-trees-in-r/

#-----------------------------------------------------------
# XGBoost phases
#from https://www.projectpro.io/recipes/apply-xgboost-r-for-regression
install.packages("xgboost")
library(xgboost)
#define predictor and response variables in training set
train_x = data.matrix(jo_train[ , 21])
train_y = jo_train[ ,14]

#define predictor and response variables in testing set
test_x = data.matrix(jo_valid[ , 21])
test_y = jo_valid[, 14]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = test_x, label = test_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#defining a watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each iteartion
model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 100)

#define final model
model_xgboost = xgboost(data = xgb_train, max.depth = 3, nrounds = 86, verbose = 0)

summary(model_xgboost)


pred_y <- predict(model_xgboost, xgb_test)
# Model performance
data.frame(RMSE = caret::RMSE(test_y, pred_y),
           R2 = caret::R2(test_y, pred_y))

# performance metrics on the test data

mean((test_y - pred_y)^2) #mse - Mean Squared Error

caret::RMSE(test_y, pred_y) #rmse - Root Mean Squared Error

y_test_mean = mean(test_y)
# Calculate total sum of squares
tss =  sum((test_y - y_test_mean)^2 )
# Calculate residual sum of squares
residuals <- test_y - y_test_mean
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')

x = 1:length(test_y)                   # visualize the model, actual and predicted data
plot(x, test_y, col = "red", type = "l")
lines(x, pred_y, col = "blue", type = "l")
legend(x = 1, y = 38,  legend = c("original test_y", "predicted test_y"), 
       col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))

#-----------------------------------------------------------
# XGBoost FOliation
names(jo_train)
#define predictor and response variables in training set
train_x = data.matrix(jo_train[ , 22])
train_y = jo_train[ ,14]

#define predictor and response variables in testing set
test_x = data.matrix(jo_valid[ , 22])
test_y = jo_valid[, 14]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = test_x, label = test_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#defining a watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each iteartion
model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 100)

#define final model
model_xgboost = xgboost(data = xgb_train, max.depth = 3, nrounds = 86, verbose = 0)

summary(model_xgboost)


pred_y <- predict(model_xgboost, xgb_test)
# Model performance
data.frame(RMSE = caret::RMSE(test_y, pred_y),
           R2 = caret::R2(test_y, pred_y))

# performance metrics on the test data

mean((test_y - pred_y)^2) #mse - Mean Squared Error

caret::RMSE(test_y, pred_y) #rmse - Root Mean Squared Error

y_test_mean = mean(test_y)
# Calculate total sum of squares
tss =  sum((test_y - y_test_mean)^2 )
# Calculate residual sum of squares
residuals <- test_y - y_test_mean
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n')

x = 1:length(test_y)                   # visualize the model, actual and predicted data
plot(x, test_y, col = "red", type = "l")
lines(x, pred_y, col = "blue", type = "l")
legend(x = 1, y = 38,  legend = c("original test_y", "predicted test_y"), 
       col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))

plot(model_xgboost$)
##
# plot the first tree
xgb.plot.tree(model = model_xgboost$, trees = 1)

#


#######################################################################
# Random forest Foliation
set.seed(77)
tc <- trainControl(method = "cv",
                   number = 10)
#m <- train(foliation ~ gndvi,
#m <- train(foliation ~ ndrei,
#m <- train(foliation ~ ndvi,
#m <- train(foliation ~ ndre,
#m <- train(foliation ~ gcc,
#m <- train(foliation ~ ndrei + gcc,
#m <- train(foliation ~ ndwi + gcc,
#m <- train(foliation ~ ndre + ndwi,
m <- train(foliation ~ gcc_uc,
           data = jo1920,
           trControl = tc,
           method = "rf",
           tuneGrid = expand.grid(mtry = 4:10),
           na.action=na.exclude,
           preProc=c("center","scale"))

# Model performance
predictions <- m %>% predict(jo21)
df <- data.frame(RMSE = caret::RMSE(predictions, jo21$foliation),
                 MAE = caret::MAE(predictions, jo21$foliation),
                 R2 = caret::R2(predictions, jo21$foliation))

df

#########################################################################
# Random forest Phase
cores <- makeCluster(detectCores() - 1)
beginCluster(cores)
names(jo_1920p)

tc <- trainControl(method = "cv",
                   number = 10)
set.seed(77)
m <- train(phase ~ gndvi,
           #m <- train(phase ~ ndrei,
           #m <- train(phase ~ ndvi,
           #m <- train(phase ~ ndre,
           #m <- train(phase ~ gcc,
           #m <- train(phase ~ ndrei + gcc,
           #m <- train(phase ~ ndwi + gcc,
           #m <- train(phase ~ ndre + ndwi,
           #m <- train(phase ~ gcc_uc,
           data = jo1920,
           trControl = tc,
           method = "rf",
           tuneGrid = expand.grid(mtry = 4:10),
           na.action=na.exclude,
           preProc=c("center","scale"))
predictions <- m %>% predict(jo21)
#m
# Model performance
df <- data.frame(RMSE = caret::RMSE(predictions, jo21$phase),
                 MAE = caret::MAE(predictions, jo21$phase),
                 R2 = caret::R2(predictions, jo21$phase))
df







library(hydroGOF)
f <- csv2019$phase
v1 <- predictions
v1rmse <- rmse(f, v1, na.rm=T)
v1mean <- mean(v1, na.rm=T)
v1rmseperc <- (v1rmse/v1mean) * 100 
# Calculate RMSE for V1
v1test = data.frame(f, v1)
#v1mod <- lm(v1~f, data = v1test)
v1mod <- lm(model)
v1rmse <- rmse(f, v1, na.rm=T) 
options(digits=2)
v1rmseperc <- (v1rmse/v1mean) * 100 
options(digits=2)

textv1rmse <- bquote(RMSE == .(v1rmse))
textv1rmseperc <- bquote(RMSE["%"] == .(v1rmseperc))
dev.new()
plot.new()
tiff("v1_rmse.tiff", compression = "lzw", height = 2770, width = 2820, res=400)
plot(v1 ~ f, data = v1test, col = "black", pch = 21, bg = "blue", cex = 1.2, ylab = "Observed", font.lab = 2, 
     xlab = "Predicted", main = "RMSE XY") 
abline(v1mod, col="black", lwd = 2)
mtext(textv1rmse, adj=0.9, line=-18, ps = 2, font=2, cex = 1)
mtext(textv1rmseperc, adj=0.9, line=-19, ps = 2, font=2, cex = 1)
dev.off()
