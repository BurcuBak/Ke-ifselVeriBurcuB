# Rastgele veri oluşturma için MASS dan faydalanılır
install.packages("MASS")
library(MASS)
#seed ile tekrar üretilebilir bir veri setini oluşturulur
set.seed(123)
#gözlem sayısı belirlenir
n <- 24
#4 bağımsız değişken oluşturulur
UYkuSüresi1 <-  rnorm(n, mean = 12, sd = 2) # Bağımsız değişken 1
UYkuSüresi2 <-  rnorm(n, mean = 10, sd = 1) # Bağımsız değişken 2
UYkuSüresi3 <-  rnorm(n, mean = 5, sd = 0.5) # Bağımsız değişken 3
UYkuSüresi4 <-  rnorm(n, mean = 8, sd = 3) # Bağımsız değişken 4
# Bağımlı değişken       # rnorm(n, mean = 0, sd = 2) ile hata terimi çin yazarız
BaşarıNotu <- 3+2*UYkuSüresi1 - 1.5*UYkuSüresi2 + 0.5*UYkuSüresi3 - 1.2*UYkuSüresi4 + rnorm(n, mean = 0, sd = 2)

#veri setini veri çerçevesine dönüştürmek için
data <- data.frame(BaşarıNotu, UYkuSüresi1, UYkuSüresi2, UYkuSüresi3, UYkuSüresi4)
#veri setini görmek için
head(data)

#Factor ve numeric üzerinden değişkenleri dönüştürmek için;
set.seed(123)
n <- 24
UYkuSüresi1_fac <-  rnorm(n, mean = 12, sd = 2)
UYkuSüresi2_num <-  rnorm(n, mean = 10, sd = 1)
BaşarıNotu <- rnorm(n, mean = 3+2*UYkuSüresi1 - 1.5*UYkuSüresi2, sd=2)

# UYkuSüresi1 değişkenini faktörel değişkene dönüştürmek için
UYkuSüresi1_fac_factor <- as.factor(UYkuSüresi1_fac)

#UYkuSüresi2 değişkenini sayısal değişkene dönüştürme
UYkuSüresi2_num_numeric <- as.numeric(UYkuSüresi2_num)

#str ile değişken türlerini kontrol etmek için
str(UYkuSüresi1_fac_factor)
str(UYkuSüresi2_num_numeric)

#Veri kalitesi için ölçüm ve test yapmak gerekir. 
#na veya aykırı değer var mı kontrol edilmeli


set.seed(123)
n <- 24
UYkuSüresi1_ev <-  rnorm(n, mean = 12, sd = 2)
UYkuSüresi2_ev <-  rnorm(n, mean = 10, sd = 1)
BaşarıNotu_ev <- rnorm(n, mean = 3+2*UYkuSüresi1 - 1.5*UYkuSüresi2, sd=2)

#eksik veri tespiti için is_na kullanılır
is_na_UYkuSüresi1 <- is.na(UYkuSüresi1_ev)
is_na_UYkuSüresi2 <- is.na(UYkuSüresi2_ev)

# Eksik verileri ortalama ile doldurma
#içinde na değeri varsa düşürmek için na.rm yazılır
mean_UYkuSüresi1_ev <- mean(UYkuSüresi1, na.rm = TRUE)
UYkuSüresi1_filled <- ifelse(is.na(UYkuSüresi1_ev), mean_UYkuSüresi1, UYkuSüresi1)

#na.locf() fonksiyonu R'da kullanılan bir fonksiyondur ve 
#"sonraki değeri kullanarak eksik değerleri doldurma" anlamına gelir. 
#Yani, eksik değer bir gözlemde bulunuyorsa, bu gözlemin bir önceki gözleminin değeriyle doldurulur.
#zoo paketiyle gelir
install.packages("zoo")
library(zoo)

UYkuSüresi2_filled <- na.locf(UYkuSüresi2_ev)

# Eksik verileri medyan ile doldurma
median_BaşarıNotu_ev <- median(BaşarıNotu_ev, na.rm = TRUE)
BaşarıNotu_filled <- ifelse(is.na(BaşarıNotu_ev), median_BaşarıNotu_ev, BaşarıNotu_ev)

median_BaşarıNotu_ev2 <- quantile(BaşarıNotu_ev, 0.5)


#aykırı değer tespiti
set.seed(123)
data <- rnorm(24)

# Kutu grafiği oluşturma
boxplot(data)
sort(data)

# Z-puanı hesaplama
z_scores <- scale(data)

z_scores == scale(data)

# Aykırı değerleri belirleme abs ile yapılır.
#z scorlar 3 den büyükse mutlak değerce ona aykırı değer ata
outliers <- abs(z_scores) > 3

#-3 veya 3 değeri yoktu o yüzden false

# Alt ve üst çeyreklikleri hesaplama
Q1 <- quantile(data, 0.25)
Q3 <- quantile(data, 0.75)


# Alt ve üst sınırı hesaplama
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Aykırı değerleri belirleme
outliers <- data < lower_bound | data > upper_bound


#4.sütun Dağılımların Keşfi
# Örnek veri setini oluşturma
set.seed(123)
data <- rnorm(100)

# Histogram oluşturma
hist(data)

# Kutu grafiği oluşturma
boxplot(data)

# Q-Q plot oluşturma

qqnorm(data)
qqline(data)

# Kantillerden yararlanma
summary(data)


# Örnek veri setini oluşturma
set.seed(123)
x <- rnorm(100) # Bağımsız değişken
y <- 2*x + rnorm(100, mean = 0, sd = 0.5) # Bağımlı değişken

# Korelasyon katsayısını hesaplama
correlation <- cor(x, y)

# Korelasyon katsayısını ekrana yazdırma
print(correlation)

#multicollinearity
# Örnek veri setini oluşturma
set.seed(123)
x1 <- rnorm(100) # Bağımsız değişken 1
x2 <- rnorm(100) # Bağımsız değişken 2
x3 <- rnorm(100) # Bağımsız değişken 3
y <- 2*x1 + 3*x2 - 1.5*x3 + rnorm(100, mean = 0, sd = 0.5) # Bağımlı değişken

# Çoklu doğrusal regresyon modelini oluşturma
model <- lm(y ~ x1 + x2 + x3)

# Model özetini alma
summary(model)


# Bağımsız değişkenler arasındaki korelasyonu hesaplama
correlation_matrix <- cor(data.frame(x1, x2, x3))
print(correlation_matrix)
# corrplot paketini yükleme
install.packages("coorplot")
library(corrplot)
# Korelasyon matrisini görselleştirme
corrplot(correlation_matrix, method = "color")

# Bağımsız değişkenlerin grafiğini çizme
par(mfrow=c(1,3)) # Grafiklerin yan yana yerleştirilmesi için ayar
plot(x1, y, main = "x1 vs. y", xlab = "x1", ylab = "y", col = "blue", pch = 16)
plot(x2, y, main = "x2 vs. y", xlab = "x2", ylab = "y", col = "red", pch = 16)
plot(x3, y, main = "x3 vs. y", xlab = "x3", ylab = "y", col = "green", pch = 16)


# Örnek veri setini oluşturma
set.seed(123)
x1 <- rnorm(100) # Bağımsız değişken 1
x2 <- rnorm(100) # Bağımsız değişken 2
x3 <- rnorm(100) # Bağımsız değişken 3
y <- 2*x1 + 3*x2 - 1.5*x3 + rnorm(100, mean = 0, sd = 0.5) # Bağımlı değişken


# Veriyi standartlaştırma
x1_standardized <- scale(x1)
x2_standardized <- scale(x2)
x3_standardized <- scale(x3)
y_standardized <- scale(y)

# Standartlaştırılmış veriyi kontrol etme
summary(x1_standardized)
summary(x2_standardized)
summary(x3_standardized)
summary(y_standardized)


#kategorik keşif
#---

#---
#son değişiklik
# caTools paketini yükleme
install.packages("caTools")
library(caTools)

# Veri setini oluşturma (örnek veri)
set.seed(123)
x1 <- rnorm(100) # Bağımsız değişken 1
x2 <- rnorm(100) # Bağımsız değişken 2
x3 <- rnorm(100) # Bağımsız değişken 3
y <- 2*x1 + 3*x2 - 1.5*x3 + rnorm(100, mean = 0, sd = 0.5) # Bağımlı değişken

# Veriyi test ve eğitim alt kümelerine böleme
split <- sample.split(y, SplitRatio = 0.7) # 70% eğitim, 30% test
train_data <- subset(data.frame(x1, x2, x3, y), split == TRUE)
test_data <- subset(data.frame(x1, x2, x3, y), split == FALSE)

# Verinin boyutlarını kontrol etme
dim(train_data)
dim(test_data)










