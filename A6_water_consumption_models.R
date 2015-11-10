# Author: Char Tamason
# Desc:   modelling water consumption
# DEPENDENCIES: Requires M1, M2, M3, M4, & B1, B2 to have been run

#load libraries
library(lme4)
library(chron)
library(lattice)
library(ggplot2)
library(multcomp)

# Load data ---------------------------------------------------------------
ifelse(grepl("zrc340", getwd()), 
       Q11.path<- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\Q11_all.Rdata",
       Q11.path<- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\Q11_all.Rdata")
ifelse(grepl("zrc340", getwd()), 
       m4.path<- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\clean-monthly-baseline_join.Rdata",
       m4.path<- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\clean-monthly-baseline_join.Rdata")
ifelse(grepl("zrc340", getwd()), 
       data.out.path<- "C:\\Users\\zrc340\\Desktop\\C5 for Git\\C5_R_Codes\\Rdata\\monthly-water.Rdata",
       data.out.path<- "C:\\Users\\wrz741\\Dropbox\\C5_R_Codes\\Rdata\\monthly-water.Rdata")

load(m4.path)
load(Q11.path)

#subset only data from monthly visits (gets ride of unmatched x-2 entries)
monthly<-m4[which(!(is.na(m4$FRA))),]
monthly<-as.data.frame(monthly)
monthly<-monthly[monthly$date_visit<as.Date("2015-10-01"),]
#View(monthly)

# Variables needed --------------------------------------------------------

#change h2o_collect to character for model
monthly$h2o_collect1<-with(monthly, ifelse(h2o_collect1==1,"tap", 
                                 ifelse(h2o_collect1==2,"handpump", "well")))
                                        
# Asset calculation 
#shared facilities (water q17, kitchen q31, latrines q35) 0=all shared, 1=2of3 shared, 2= 1of3 shared
monthly$shared_facilities<- with(monthly, ifelse(q17==1& q31 >=1 & q35==1, 0, 
                                                 ifelse(q17==0& q31 >=1 & q35==1, 1,
                                                        ifelse(q17==1& q31 >=1 & q35==0, 1,
                                                               ifelse(q17==1& q31 ==0 & q35==1, 1,
                                                                      ifelse(q17==0& q31 >=1 & q35==0, 2,
                                                                             ifelse(q17==0& q31 ==0 & q35==1, 2,
                                                                                    ifelse(q17==1& q31 ==0 & q35==0, 2, NA))))))))
#table(monthly$shared_facilities)

#### Assets continued: coding ownership of items
# in q9_20_oth1, q9_20_oth2, q9_20_oth3 meatshelf (cupboard), dressing table (dresser), and sofa should earn 1 point, rickshaw should earn 2, 
monthly$q9_20_oth1<-as.character(monthly$q9_20_oth1)
monthly$q9_20_oth2 <-as.character(monthly$q9_20_oth2)
monthly$q9_20_oth3 <-as.character(monthly$q9_20_oth3)

#recode spelling errors, everything that starts with M becomes MEAT SHELF, D becomes DRESSING TABLE
#grep("^C", monthly$q9_20_oth1, value=T) #to search for resopnses by start letter

monthly[grep("^M", monthly$q9_20_oth1), "q9_20_oth1"] <- as.character("MEAT SHELF")
monthly[grep("^DRE", monthly$q9_20_oth1), "q9_20_oth1"] <- as.character("DRESSING TABLE")

monthly[grep("^IRON", monthly$q9_20_oth2), "q9_20_oth2"] <- as.character("MEAT SHELF")
monthly[grep("^M", monthly$q9_20_oth2), "q9_20_oth2"] <- as.character("MEAT SHELF")
monthly[grep("^DRE", monthly$q9_20_oth2), "q9_20_oth2"] <- as.character("DRESSING TABLE")

monthly[grep("^M", monthly$q9_20_oth3), "q9_20_oth3"] <- as.character("MEAT SHELF")
monthly[grep("^IRON", monthly$q9_20_oth3), "q9_20_oth3"] <- as.character("MEAT SHELF")

monthly$q9_20_oth1p <- with(monthly, ifelse(q9_20_oth1=="COMPUTER",3,
                                            ifelse(q9_20_oth1=="VEDEO CAMERA",2,
                                                   ifelse(q9_20_oth1=="STEEL MITSHAFE",1,
                                                          ifelse(q9_20_oth1=="DRESSING TABLE",1,
                                                                 ifelse(q9_20_oth1=="MEAT SAFE",1,
                                                                        ifelse(q9_20_oth1=="EATSSAFE",1,
                                                                               ifelse(q9_20_oth1=="SOFA",1,
                                                                                      ifelse(q9_20_oth1=="SUFA",1,
                                                                                             ifelse(q9_20_oth1=="SOFA SET",1,
                                                                                                    ifelse(q9_20_oth1=="RICKSHAW.",2,0 # in 9_20_oth2
                                                                                                    )))))))))))

monthly$q9_20_oth2p <- with(monthly, ifelse(q9_20_oth2=="COMPUTER",3,
                                            ifelse(q9_20_oth2=="VEDEO CAMERA",2,
                                                   ifelse(q9_20_oth2=="STEEL MITSHAFE",1,
                                                          ifelse(q9_20_oth2=="DRESSING TABLE",1,
                                                                 ifelse(q9_20_oth2=="MEAT SAFE",1,
                                                                        ifelse(q9_20_oth2=="EATSSAFE",1,
                                                                               ifelse(q9_20_oth2=="SOFA",1,
                                                                                      ifelse(q9_20_oth2=="SUFA",1,
                                                                                             ifelse(q9_20_oth2=="SOFA SET",1,
                                                                                                    ifelse(q9_20_oth2=="RICKSHAW.",2,0 # in 9_20_oth2
                                                                                                    )))))))))))

monthly$q9_20_oth3p <- with(monthly, ifelse(q9_20_oth3=="COMPUTER",3,
                                            ifelse(q9_20_oth3=="VEDEO CAMERA",2,
                                                   ifelse(q9_20_oth3=="STEEL MITSHAFE",1,
                                                          ifelse(q9_20_oth3=="DRESSING TABLE",1,
                                                                 ifelse(q9_20_oth3=="MEAT SAFE",1,
                                                                        ifelse(q9_20_oth3=="EATSSAFE",1,
                                                                               ifelse(q9_20_oth3=="SOFA",1,
                                                                                      ifelse(q9_20_oth3=="SUFA",1,
                                                                                             ifelse(q9_20_oth3=="SOFA SET",1,
                                                                                                    ifelse(q9_20_oth3=="RICKSHAW.",2,0 # in 9_20_oth2
                                                                                                    )))))))))))
monthly$q9_other_sum<-0
monthly$q9_other_sum<-with(monthly, q9_20_oth1p+q9_20_oth2p+q9_20_oth3p)

#count assets
monthly$asset_score<-NA
monthly$asset_score<- (monthly$q9_other_sum + 
                         as.numeric(monthly$q9_2) + as.numeric(monthly$q9_3) +
                         as.numeric(monthly$q9_5) + as.numeric(monthly$q9_7) + 
                         as.numeric(monthly$q9_8) + as.numeric(monthly$q9_9) + 
                         as.numeric(monthly$q9_16) + as.numeric(monthly$q9_18)+ 
                         ((as.numeric(monthly$q9_1) + as.numeric(monthly$q9_6) +
                            as.numeric(monthly$q9_10) + as.numeric(monthly$q9_11) + 
                            as.numeric(monthly$q9_12) + as.numeric(monthly$q9_14) + 
                            as.numeric(monthly$q9_15) + as.numeric(monthly$q9_17))*2) + 
                             ((as.numeric(monthly$q9_13) + as.numeric(monthly$q9_19) + 
                                 monthly$shared_facilities)*3))

#asset quintiles, note: probs=0:5/5 is same as c(.2,.4,.6,.8,1)
monthly$asset_score[is.na(monthly$asset_score)]<-0

monthly$asset_quintile<-as.integer(cut(monthly$asset_score,
                                       quantile(monthly$asset_score,probs=0:5/5,include.lowest=TRUE)))
#distance to primary water source
monthly$distance<-with(monthly, ifelse(distance_to_source1==0,1,
                                       ifelse(distance_to_source1>0&distance_to_source1<=10,2,
                                              ifelse(distance_to_source1>10&distance_to_source1<=20,3,4))))
monthly$in_home_water<-with(monthly, ifelse(distance_to_source1==0,"in_home","out_of_home"))
#day of the week
monthly$day<-weekdays(as.Date(monthly$date_visit))

#check day
#View(monthly[,c("day","date_visit")])

#month variable
monthly$date_visit_character<-as.character(monthly$date_visit)
temp<-strsplit(monthly$date_visit_character, "-")
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)
df <- as.data.frame(mat)
colnames(df) <- c("year", "month", "day")
monthly<- cbind(monthly,df)

monthly$year.month<-as.numeric(with(monthly, ifelse(month=="09"|month=="10"|month=="11"|
                                                      month=="12",paste("14.",month,sep=""),paste("15.",month,sep=""))))

#season variable
monthly$season<-with(monthly, ifelse(month=="01"|month=="02"|month=="03","ld",
                                     ifelse(month=="04"|month=="05"|month=="06","pm",
                                            ifelse(month=="07"|month=="08"|month=="09","m","ed"))))
monthly$season2<-with(monthly, ifelse(month=="01"|month=="02"|month=="11"|month=="12","dry_cold",
                                      ifelse(month=="04"|month=="05","spring",
                                             ifelse(month=="07"|month=="08"|month=="09"|month=="06","m","oct"))))

#set categorical variables as characters for models
monthly$asset_quintile<-as.character(monthly$asset_quintile)
monthly$distance<-as.character(monthly$distance)
monthly$infrastructure<-ifelse(monthly$q14_recoded==1,"tap",
                               ifelse(monthly$q14_recoded==2,"handpump","bucket"))


#handwashing variable
monthly$other_water_in.wash_hands_in[is.na(monthly$other_water_in.wash_hands_in)]<-0
monthly$other_water_out.wash_hands_out[is.na(monthly$other_water_out.wash_hands_out)]<-0
monthly$handwash_per_capita<-(monthly$other_water_in.wash_hands_in+monthly$other_water_out.wash_hands_out)/monthly$ppl

#clothes per capita variable
monthly$clothes_pc<-monthly$clothes/monthly$ppl

monthly$infrastructure_routine<-with(monthly,ifelse(water_point1.wa_pt1==1|water_point1.wa_pt1==2,"tap",
                                                    ifelse(water_point1.wa_pt1==3|water_point1.wa_pt1==4,"handpump",
                                                           ifelse(water_point1.wa_pt1==5|water_point1.wa_pt1==777,"well",water_point1.wa_pt1))))

#table(monthly$infrastructure_routine)

#table(monthly$daily_h2o_percapita)
# Linear mixed models -----------------------------------------------------

# modelx<-lmer(daily_h2o_percapita ~ season  +  checkwater +  distance  + water_point1.wa_source1 + day + asset_quintile
#              + ppl + (1|HH_key) +(1|listing), data=monthly)
# 
# modely<-lmer(daily_h2o_percapita ~ season + checkwater*distance +   infrastructure + day + asset_quintile
#              + ppl + (1|HH_key) +(1|listing), data=monthly)
# anova(modely,modelx) # significant difference (p=.24) indicates that we shouldn't use checkwater*distance


## check if tank is interdependent with season
# modela<-lmer(daily_h2o_percapita ~ season*h2o_tank1 + checkwater +  h2o_distance1 +  h2o_collect1 + day + asset_quintile
#              + ppl + (1|HH_key) +(1|listing), data=monthly)
# modelb<-lmer(daily_h2o_percapita ~ season + h2o_tank1 + checkwater +  h2o_distance1 +  h2o_collect1 + asset_quintile + day 
#              + ppl + (1|HH_key) +(1|listing), data=monthly)
# anova(modela,modelb) #p=0.5, independent
  
##  model 1 daily h2o consumption over seasons Jan-Mar, Apr-Jun, Jul-Sep, Oct-Dec
model1=lmer(daily_h2o_percapita ~ season + checkwater  +  h2o_distance1   + h2o_collect1 + day + asset_quintile
            + ppl + (1|HH_key)+(1|Listing.number.x), data=monthly)
summary(model1)


model.null=lmer(daily_h2o_percapita ~ checkwater + h2o_distance1 + h2o_collect1  + day + asset_quintile
                + ppl + (1|HH_key) +(1|Listing.number.x), data=monthly)
summary(model.null)

anova(model1,model.null)

mean(monthly$daily_h2o_percapita)

### model 2 daily consumption with different seasons
model2=lmer(daily_h2o_percapita ~ season2 + checkwater  +  h2o_distance1   + h2o_collect1 + day + asset_quintile
            + ppl + (1|HH_key) +(1|listing), data=monthly)
summary(model1)

model2.null=lmer(daily_h2o_percapita ~   h2o_distance1 +  checkwater + h2o_collect1  + day + asset_quintile
                + ppl + (1|HH_key) +(1|listing), data=monthly)

anova(model2,model2.null)

coef(lmer(daily_h2o_percapita ~ season2*checkwater  +  h2o_distance1 + h2o_collect1 + day + asset_quintile
          + ppl + (1|HH_key) +(1|listing), data=monthly))

#handwashing over the seasons

modelhand=lmer( handwash_per_capita ~ season + checkwater +   h2o_distance1 + h2o_collect1 + asset_quintile
            + ppl + (1|HH_key) +(1|listing), data=monthly)
summary(modelhand)

modelhand.null=lmer(handwash_per_capita ~  h2o_distance1 +  checkwater + h2o_collect1  + asset_quintile
                + ppl + (1|HH_key) +(1|listing), data=monthly)

summary(modelhand)

anova(modelhand,modelhand.null)

coef(lmer(handwash_per_capita ~ season + checkwater  +    h2o_distance1 + h2o_collect1 + asset_quintile
          + ppl + (1|HH_key) +(1|listing), data=monthly))

modelhand2<-lmer(handwash_per_capita ~ season + daily_h2o_percapita + (1|HH_key) +(1|listing), data=monthly)
modelhand2null<-lmer(handwash_per_capita ~ daily_h2o_percapita + (1|HH_key) +(1|listing), data = monthly)

anova(modelhand2, modelhand2null) # p<.001
summary(modelhand2)

###
clothesmodel<-lmer(clothes_pc~season + checkwater + h2o_distance1 + h2o_collect1 + asset_quintile
                   + ppl + (1|HH_key) +(1|listing), data=monthly)
summary(clothesmodel)
clothesmodelnull<-lmer(clothes_pc~checkwater + h2o_distance1 + h2o_collect1 + asset_quintile
                       + ppl + (1|HH_key) +(1|listing), data=monthly)

anova(clothesmodelnull,clothesmodel)

##
dishmodel<-lmer(dishes~season*checkwater + h2o_distance1 + h2o_collect1 + asset_quintile
                + ppl + (1|HH_key) +(1|listing), data=monthly)

summary(dishmodel)

dishmodelnull<-lmer(dishes~checkwater + h2o_distance1 + h2o_collect1 + asset_quintile
                       + ppl + (1|HH_key) +(1|listing), data=monthly)

anova(dishmodel,dishmodelnull)
coef(dishmodel)



# statistical significance of variables -----------------------------------

#hours water was running (checkwater)

#distance to source

#infrastructure

#assets

#day of the week

#ppl




# Sensitivity analysis ----------------------------------------------------

models1=lmer(daily_h2o_percapita_se ~ season + checkwater  +  h2o_distance1   + h2o_collect1 + day + asset_quintile
            + ppl + (1|HH_key)+(1|Listing.number.x), data=monthly)
summary(models1)


models.null=lmer(daily_h2o_percapita_se ~ checkwater + h2o_distance1 + h2o_collect1  + day + asset_quintile
                + ppl + (1|HH_key) +(1|Listing.number.x), data=monthly)


anova(models1,models.null) # p = 0.05

mean(monthly$daily_h2o_percapita_se)

#look at subsets based on whether baths were taken or not
sub1<-monthly[monthly$number_adult_baths==0,]
#sub2<-monthly[monthly$number_adult_baths==0&monthly$clothes==0,] # analysis showed this doesn't vary from adult bath ==0 alone

models2=lmer(daily_h2o_percapita_se ~ season + checkwater  +  h2o_distance1   + h2o_collect1 + day + asset_quintile
             + ppl + (1|HH_key)+(1|Listing.number.x), data=sub1)
summary(models2)


#models3=lmer(daily_h2o_percapita_se ~ season + checkwater + h2o_distance1 + h2o_collect1  + day + asset_quintile
                 + ppl + (1|HH_key) +(1|Listing.number.x), data=sub2)
#summary(models3)



# Visualizing models ------------------------------------------------------

#Visualize random effects (HH_key and listing number)
dotplot(ranef(model1, condVar = TRUE))
qqmath(ranef(model1, condVar=TRUE))


#visualization of the effect of variables and their CIs
tmp <- as.data.frame(confint(glht(model1))$confint)
tmp$Comparison<-rownames(tmp)
ggplot(tmp, aes(x = Comparison, y = Estimate, ymin = lwr, ymax = upr)) + geom_errorbar() + geom_point()


# Save data ---------------------------------------------------------------

save(monthly, file = data.output.path)
