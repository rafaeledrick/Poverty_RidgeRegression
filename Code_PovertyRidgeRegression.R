library(readxl)
Kemiskinan <- read_excel("C:/Users/edric/Downloads/Kemiskinan.xlsx")
View(Kemiskinan)

library(car)
library(lmtest)
library(nortest)
install.packages("lmridge")
library(lmridge)
install.packages("robustbase")
library(robustbase)
install.packages("performance")
library(performance)
install.packages("orcutt")
library(orcutt)

str(Kemiskinan)

# plot x dan y
plot(Kemiskinan$IPM, Kemiskinan$Persentase_Kemiskinan)
plot(Kemiskinan$TPT, Kemiskinan$Persentase_Kemiskinan)
plot(Kemiskinan$PDRB, Kemiskinan$Persentase_Kemiskinan)
plot(Kemiskinan$Upah_Rata, Kemiskinan$Persentase_Kemiskinan)
plot(Kemiskinan$Selesai_SD, Kemiskinan$Persentase_Kemiskinan)
plot(Kemiskinan$Selesai_SMP, Kemiskinan$Persentase_Kemiskinan)
plot(Kemiskinan$Selesai_SMA, Kemiskinan$Persentase_Kemiskinan)
plot(Kemiskinan$LamaSekolah, Kemiskinan$Persentase_Kemiskinan)
plot(Kemiskinan$Melek_Huruf, Kemiskinan$Persentase_Kemiskinan)
plot(Kemiskinan$Merokok, Kemiskinan$Persentase_Kemiskinan)
plot(Kemiskinan$Seluler, Kemiskinan$Persentase_Kemiskinan)

# analisis korelasi (bisa antara x dengan y, bisa antara x dengan x)
cor.test(Kemiskinan$IPM, Kemiskinan$Persentase_Kemiskinan)
cor.test(Kemiskinan$TPT, Kemiskinan$Persentase_Kemiskinan)
cor.test(Kemiskinan$PDRB, Kemiskinan$Persentase_Kemiskinan)
cor.test(Kemiskinan$Upah_Rata, Kemiskinan$Persentase_Kemiskinan)
cor.test(Kemiskinan$Selesai_SD, Kemiskinan$Persentase_Kemiskinan)
cor.test(Kemiskinan$Selesai_SMP, Kemiskinan$Persentase_Kemiskinan)
cor.test(Kemiskinan$Selesai_SMA, Kemiskinan$Persentase_Kemiskinan)
cor.test(Kemiskinan$LamaSekolah, Kemiskinan$Persentase_Kemiskinan)
cor.test(Kemiskinan$Melek_Huruf, Kemiskinan$Persentase_Kemiskinan)
cor.test(Kemiskinan$Merokok, Kemiskinan$Persentase_Kemiskinan)
cor.test(Kemiskinan$Seluler, Kemiskinan$Persentase_Kemiskinan)

# boxplot
boxplot(Kemiskinan$IPM) # left skewed dan ada outlier
boxplot(Kemiskinan$TPT) # normal dan tidak ada outlier
boxplot(Kemiskinan$PDRB) # right skewed dan ada outlier
boxplot(Kemiskinan$Upah_Rata) # right skewed dan ada outlier
boxplot(Kemiskinan$Selesai_SD) # left skewed dan ada outlier
boxplot(Kemiskinan$Selesai_SMP) # left skewed dan ada outlier
boxplot(Kemiskinan$Selesai_SMA) # left skewed dan ada outlier
boxplot(Kemiskinan$LamaSekolah) # normal dan tidak ada outlier
boxplot(Kemiskinan$Melek_Huruf) # left skewed dan ada outlier
boxplot(Kemiskinan$Merokok) # normal dan tidak ada outlier
boxplot(Kemiskinan$Seluler) # normal dan ada outlier
boxplot(Kemiskinan$Persentase_Kemiskinan) # right skewed dan ada outlier

model <- lm(Kemiskinan$Persentase_Kemiskinan~IPM+TPT+PDRB+Upah_Rata+Selesai_SD+Selesai_SMP+Selesai_SMA+LamaSekolah+Melek_Huruf+Merokok+Seluler, data=Kemiskinan)
summary(model)
# Uji F -> Tolak H0 ketika p-value < alpha (0.00000001312 < 0.05) -> terdapat minimal satu variabel yang signifikan
# Uji T -> TPT, PDRB, Selesai_SMA, LamaSekolah, Merokok, Seluler -> variabel yang signifikan

error <- resid(model)
qqline(error)
plot(density(error)) # normal

# Asumsi Residual
lillie.test(error) # Gagal Tolak H0 -> Berdistribusi normal
bptest(model) # Gagal Tolak H0 -> Mempunyai variance konstan (Homoskedastis)
dwtest(model) # Gagal Tolak H0 -> Tidak berautokorelasi
vif(model) # Terjadi multikolinearitas

check_collinearity(model)
check_autocorrelation(model)
check_normality(model)
check_heteroscedasticity(model)
check_outliers(model)

# Karena model mengalami multikolinearitas, maka kita coba menggunakan ridge regression
mridge <- lmridge(Persentase_Kemiskinan~IPM+TPT+PDRB+Upah_Rata+Selesai_SD+Selesai_SMP+Selesai_SMA+LamaSekolah+Melek_Huruf+Merokok+Seluler, data=as.data.frame(Kemiskinan, scaling="sc", K=seq(0,1,0.001)))
summary(mridge)
# Uji F -> Tolak H0 ketika p-value < alpha (0.0000000557 < 0.05) -> terdapat minimal satu variabel yang signifikan
# Uji T -> TPT, PDRB, Selesai_SMA, LamaSekolah, Merokok, Seluler -> variabel yang signifikan

AIC(model)
BIC(model)
# Nilai AIC model dengan mridgenya lebih baik Nilai AIC mridgenya (168.8447 > 65.51896) 
# Nilai BIC model dengan mridgenya lebih baik Nilai BIC mridgenya (189.0642 < 207.06497)