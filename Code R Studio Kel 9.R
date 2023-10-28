#STATISTIKA DESKRIPTIF
library(readxl)
library(Hmisc)
library(psych)
data1 <- read_excel("E:/ITB/Semester 3/Andat/Tubes/Semoga Indonesia damai sejahtera dan inflasi terkontrol.xlsx", sheet = "Inflasi")
data2 <- read_excel("E:/ITB/Semester 3/Andat/Tubes/Semoga Indonesia damai sejahtera dan inflasi terkontrol.xlsx", sheet = "BI_Rate")
data3 <- read_excel("E:/ITB/Semester 3/Andat/Tubes/Semoga Indonesia damai sejahtera dan inflasi terkontrol.xlsx", sheet = "IHP")
data4 <- read_excel("E:/ITB/Semester 3/Andat/Tubes/Semoga Indonesia damai sejahtera dan inflasi terkontrol.xlsx", sheet = "IHK")
inflasi <- data1$Inflasi
BiRate <- data2$BI_RATE
ihp <- data3$IHP
ihk <- data4$IHK

##Sari Numerik
sum(inflasi)
stat.desc(inflasi)
describe(inflasi)

sum(BiRate)
stat.desc(BiRate)
describe(BiRate)

sum(ihp)
stat.desc(ihp)
describe(ihp)

sum(ihk)
stat.desc(ihk)
describe(ihk)

#Boxplot
boxplot(inflasi, horizontal=T, main="Box Plot: Inflasi")
boxplot(BiRate, horizontal=T, main="Box Plot: Bank Indonesia Rate")
boxplot(ihp, horizontal=T, main="Box Plot: Indeks Harga Produsen")
boxplot(ihk, horizontal=T, main="Box Plot: Indeks Harga Konsumen")

#Density Plot
d <- density(inflasi)
plot(d, main="Density Plot: Inflasi")
polygon(d, col="yellow", border="blue")

d <- density(BiRate)
plot(d, main="Density Plot: Bank Indonesia Rate")
polygon(d, col="green", border="yellow")

d <- density(ihp)
plot(d, main="Density Plot: Indeks Harga Konsumen")
polygon(d, col="red", border="green")

d <- density(ihk)
plot(d, main="Density Plot: Indeks Harga Produsen")
polygon(d, col="blue", border="red")

#REGRESI LINEAR SEDERHANA
library(readxl)
Data = read_excel("C:/Users/Farhan/Downloads/Data Tubes.xlsx", sheet = 'Inflasi')

View(Data)
attach(Data)

#1. Pengaruh BI_Rate terhadap Inflasi
linearMod = lm(Inflasi ~ BI_RATE)
summary(linearMod)
modelSummary = summary(linearMod)

scatter.smooth(x = BI_RATE,y = Inflasi, main = 'Pengaruh BI Rate terhadap Inflasi, ')
abline(linearMod, col = 'Blue')

cor(BI_RATE, Inflasi)

(t_tabel = qt(0.95, df = 9, lower.tail = FALSE))

(f = modelSummary$fstatistic)

(f_tabel = qf(0.95, f[2],f[3]))


#2. Pengaruh IHP terhadap Inflasi
linearMod = lm(Inflasi ~ IHP)
summary(linearMod)
modelSummary = summary(linearMod)

scatter.smooth(x = IHP, y = Inflasi, main = 'Pengaruh IHP terhadap Inflasi')
abline(linearMod, col = 'Red')

cor(IHP, Inflasi)

(t_tabel = qt(0.95, df = 9, lower.tail = FALSE))

(f = modelSummary$fstatistic)

(f_tabel = qf(0.95, f[2], f[3]))

#3. Pengaruh IHK terhadap Inflasi
linearMod = lm(Inflasi~IHK)
summary(linearMod)
modelSummary = summary(linearMod)

scatter.smooth(x = IHK, y = Inflasi, main = 'Pengaruh IHK terhadap Inflasi')
abline(linearMod, col='Purple')

cor(IHK,Inflasi)

#ANALISIS DERET WAKTU
library(readxl)
data <- read_excel("C:/Users/LENOVO/Downloads/Semoga Indonesia damai sejahtera
dan inflasi terkontrol.xlsx", sheet ="Lengkap")

library(forecast)
inflasi_1 = ts(data$Inflasi)
plot(inflasi_1, main = "Grafik Inflasi Tahun 2011 - 2020", ylab = "Inflasi",
     xlab = "tahun ke-", type='o')

#Plot ACF
library(tseries)
adf.test(inflasi_1)

#Identifikasi Orde
acf(inflasi_1, main = "Grafik ACF")
pacf(inflasi_1, main = "Grafik PACF")

#Estimasi Parameter
modelari = arima(inflasi_1, order = c(1,0,1))
summary(modelari)

#Cara Otomatis
model = auto.arima(inflasi_1)
summary(model)

#Uji Diagnostik
checkresiduals(modelari) #Menggunakan model ini karena ternyata setelah diuji diagnostik, model ini yang paling cocok

#Prediksi
(prediksi = forecast(modelari, h = 3))
plot(prediksi, main = "Prediksi Grafik Inflasi 2011 - 2023", ylab = "Inflasi",
     xlab = "Tahun ke- ", type = 'o')