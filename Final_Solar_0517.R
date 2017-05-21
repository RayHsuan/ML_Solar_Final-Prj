graphics.off()

#----------solar file----------
data = read.csv("C:/Data/T102_103_301_401_3.csv", header = T, sep = ",")
sum(is.na(data))
str(data)
solar = within(data, rm("id","TotalKWH","TodayKWH","school_name","Irradiance","from_unixtime.time.",
                        "PVperDay","year","month","day","hour","minute","INV_No","INV_Brand",
                        "INV_Type","Site_Capacity","PV_No","PV_W","PV_Style"))
solar #data.frame
sum(is.na(solar)) #0


#----------描述性統計----------
colSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=2, FUN=sd, na.rm=na.rm)
Info = c("Mean", "Stdev", "Median", "IQR")
SumInfo <- matrix(data = 0, ncol = 5, nrow = 4)
rownames(SumInfo) <- Info
colnames(SumInfo) = c("kwh_kwp", "Temp", "PR", "averageIrr", "solarHour")
SumInfo["Mean",] = round(colMeans(solar[1:5]), digits = 3)
SumInfo["Stdev",] = round(colSd(solar[1:5]), digits = 3)
SumInfo["Median",] = round(c(median(solar$kwh_kwp), median(solar$Temp), median(solar$PR),
                             median(solar$averageIrr), median(solar$solarHour)), digits = 3)
SumInfo["IQR",] = round(c(IQR(solar$kwh_kwp),IQR(solar$Temp),IQR(solar$PR),IQR(solar$averageIrr),
                          IQR(solar$solarHour)), digits = 3)
SumInfo


#----------qqplot----------
qqnorm(solar$kwh_kwp, cex.main = 1.5, col="green", main = "Normal Q-Q Plot_平均發電量")
qqline(solar$kwh_kwp, col="red", lwd=2)
qqnorm(solar$Temp, cex.main = 1.5, col="green", main = "Normal Q-Q Plot_模組溫度")
qqline(solar$Temp, col="red", lwd=2)
qqnorm(solar$PR, cex.main = 1.5, col="green", main = "Normal Q-Q Plot_系統效率")
qqline(solar$PR, col="red", lwd=2)
qqnorm(solar$averageIrr, cex.main = 1.5, col="green", main = "Normal Q-Q Plot_平均日照量")
qqline(solar$averageIrr, col="red", lwd=2)
qqnorm(solar$solarHour, cex.main = 1.5, col="green", main = "Normal Q-Q Plot_日照時數")
qqline(solar$solarHour, col="red", lwd=2)


#----------bwplot----------
library(lattice)
bwplot(solar$kwh_kwp, col="red",fill = "blue", xlab="", main="kwh_kwp")
bwplot(solar$Temp, col="red",fill = "blue", xlab="", main="Temp")
bwplot(solar$PR, col="red",fill = "blue", xlab="", main="PR")
bwplot(solar$averageIrr, col="red",fill = "blue", xlab="", main="averageIrr")
bwplot(solar$solarHour, col="red",fill = "blue", xlab="", main="solarHour")


#----------boxplot----------
#boxplot(solar$kwh_kwp, main="kwh_kwp", col="#32FF32", horizontal=T)
#boxplot(solar$Temp, main="Temp", col="#32FF32", horizontal=T)
#boxplot(solar$PR, main="PR", col="#32FF32", horizontal=T)
#boxplot(solar$averageIrr, main="averageIrr", col="#32FF32", horizontal=T)
#boxplot(solar$solarHour, main="solarHour", col="#32FF32", horizontal=T)


#----------histogram----------
library(ggplot2)
ggplot(solar, aes(x=solar$kwh_kwp)) + geom_histogram(aes(y =..density..), binwidth = .5, alpha = .3,
                                                     col="blue", fill="blue") + xlab("平均發電量") +
  geom_density(col="red") + labs(title="Histogram for kwh_kwp")
ggplot(solar, aes(x=solar$Temp)) + geom_histogram(aes(y =..density..), binwidth = 5, alpha = .3,
                                                     col="blue", fill="blue") + xlab("模組溫度") +
  geom_density(col="red") + labs(title="Histogram for Temp")
ggplot(solar, aes(x=solar$PR)) + geom_histogram(aes(y =..density..), binwidth = 5, alpha = .3,
                                                  col="blue", fill="blue") + xlab("系統效率") +
  geom_density(col="red") + labs(title="Histogram for PR")
ggplot(solar, aes(x=solar$averageIrr)) + geom_histogram(aes(y =..density..), binwidth = 50, alpha = .3,
                                                col="blue", fill="blue") + xlab("平均日照量") +
  geom_density(col="red") + labs(title="Histogram for averageIrr")
ggplot(solar, aes(x=solar$solarHour)) + geom_histogram(aes(y =..density..), binwidth = 0.5, alpha = .3,
                                                        col="blue", fill="blue") + xlab("日照小時") +
  geom_density(col="red") + labs(title="Histogram for solarHour")


#----------常態分配檢定----------
shapiro.test(solar$kwh_kwp)
shapiro.test(solar$Temp)
shapiro.test(solar$PR)
shapiro.test(solar$averageIrr)
shapiro.test(solar$solarHour)
library("nortest")
ad.test(solar$kwh_kwp)
ad.test(solar$Temp)
ad.test(solar$PR)
ad.test(solar$averageIrr)
ad.test(solar$solarHour)


#----------PCA----------
head(solar[2:5])
sdv = apply(solar[2:5],2,sd)
sdv
cor(solar[2:5])
eg = eigen(cor(solar[2:5]))
eg

rownames(solar) = solar$id
PCA = princomp(solar[2:5], cor = T)
print(summary(PCA), loadings = TRUE)
plot(PCA, col="pink")
PCA
biplot(PCA, col = c("green", "red"))

par(mfrow=c(2,1), mai=c(0.5,0.5,0.5,0.5))
plot(PCA$sdev^2, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(PCA$sdev^2), xlab = "Component number",ylab = "log(Component variance)", type="l",
     main = "Log(eigenvalue) diagram")


#----------MDS and MAP----------
prj = read.csv("C:\\Users\\Dell\\Documents\\R_Data\\BigSis_3.csv",header = T,sep = ",")
prj = prj * 1000
rownames(prj)= c("1","2","3","4","5","6","7","8","9","10","11","12")
write.csv(prj, "C:/Data/prj.csv", row.names = T)
prj.dist <- as.dist(prj)
#prj.dist < - dist(x = prj, method = "")
prj.dist

prj_mds <- cmdscale(prj.dist, k = 6, eig = TRUE)
prj_mds
write.csv(prj_mds$points, "C:/Data/prj_mds.csv", row.names = T)
prj_mds$eig
prj_mds$points

prjEigs <- prj_mds$eig
prjEigs
PM1 = cumsum(abs(prjEigs)) / sum(abs(prjEigs))
PM1
PM2 = cumsum(prjEigs^2) / sum(prjEigs^2)
PM2


#----------斯皮爾曼等級相關係數----------
library("psych") #Sample Size=1383
corr.test(solar[1:5], method = "spearman")
#cor(solar[1:5], method = "spearman")


#----------簡單線性迴歸分析----------
solarSimplelm = lm(solar$kwh_kwp~solar$averageIrr, data = solar)
summary(solarSimplelm)


#----------太陽能板廠牌發電差異檢定：(獨立)雙樣本中位數差異檢定----------
(kwh_kwp_pvbrand_KT = kruskal.test(solar$kwh_kwp~solar$PV_Brand))
wilcox.test(solar$kwh_kwp~solar$PV_Brand, solar) #p-value = 0.1821 > 0.05 無法拒絕H0 但 < 0.25


#----------選模----------
library("leaps")
labels = c(0,1)
lm_solar = lm(solar$kwh_kwp~solar$Temp + solar$PR + solar$averageIrr + solar$solarHour + solar$PV_Brand)
summary(lm_solar) #Adjusted R-squared:  0.9619 
summary(step(lm_solar), k=2, method = "backward")
summary(step(lm_solar), k=log(n), method = "backward")
summary(step(lm_solar), k=2, method = "forward")
summary(step(lm_solar), k=log(n), method = "forward")
summary(step(lm_solar), k=2, method = "both")
summary(step(lm_solar), k=log(n), method = "both")
x = model.matrix(lm_solar)[,-1]
out.backward = regsubsets(x, y = solar$kwh_kwp, nbest = 1, method="backward")
s.bward = summary(out.backward)
#par(mfrow = c(2, 2))
plot(out.backward, scale = "r2")
plot(out.backward, scale = "adjr2")
plot(out.backward, scale = "Cp")
plot(out.backward, scale = "bic")
out = leaps(x, y = solar$kwh_kwp, method = "Cp" )
out


#----------多元迴歸----------
solarMultiplelm2 = lm(solar$kwh_kwp~solar$PR + solar$averageIrr + solar$solarHour)
summary(solarMultiplelm2) #djusted R-squared:  0.9612 


#----------PCA迴歸----------
print(summary(PCA), loadings = TRUE)
head(PCA$scores[,1:2])
solarPCAlm = lm(solar$kwh_kwp~PCA$scores[,1:2], data = solar)
summary(solarPCAlm)





#----------時間序列分析----------
solar = read.csv("C:\\Users\\Dell\\Documents\\R_Data\\Solar.csv",header = T,sep = ",")
prj80102 = subset(solar,school_name=="80102",select=Date:solarHour2)
prj80103 = subset(solar,school_name=="80103",select=Date:solarHour2)
prj531301 = subset(solar,school_name=="531301",select=Date:solarHour2)
prj531401 = subset(solar,school_name=="531401",select=Date:solarHour2)

getMedian = function(data) 
{ 
  for(i in 3:length(data)){
    median.value <- median(data[, i], na.rm = T)  # 第一欄位的中位數
    na.rows <- is.na(data[, i])           # 第一欄位中，有遺漏值存在的資料
    # 用第一欄位的平均數，填補第一欄位的遺漏值
    data[na.rows, i] <- median.value 
  }
  return (data)
} 

prj80102 = getMedian(prj80102)
prj80103 = getMedian(prj80103)
prj531301 = getMedian(prj531301)
prj531401 = getMedian(prj531401)

acf(prj80102$TodayKWH)
pacf(prj80102$TodayKWH)

abc<-prj80102[,c(3,5,6,7,8,9,10,12)]
ff<-prcomp(abc,scale=TRUE)
dd<-eigen(cor(abc))
(dd$values[1]+dd$values[2])/sum(dd$values)

arima(prj80102$kwh_kwp,order=c(0,0,1))

#eval(parse(text=paste("ee<-data.frame(",Sheets[1],"=Solar$",Sheets[1],"$kwh_kwp)",sep="")))
#eval(parse(text=paste("ee<-data.frame(ee,",Sheets[-1],"=Solar$",Sheets[-1],"$kwh_kwp)",sep="")))