library(readxl)
b1 <- read_excel("Book1.xlsx")

list1 = c(b1$Entry)
list2 = c(b1$Data)
mean.default(list1)
mean(list2)

summary(b1)


BookLaban <- read_excel("Laban_raw_XY.xlsx")
View(BookLaban)
