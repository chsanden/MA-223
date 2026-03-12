#Oppgave 40 i læreboka
#a)
P_knepet = 1/4
P_knepet


#b)
P_2av5 = dbinom(2, 5, 1/4)
P_2av5


#c)
ganger_tatt_totalt = 2
antall_passeringer = ganger_tatt_totalt / P_knepet
standardavvik = sqrt(antall_passeringer * P_knepet * (1 - P_knepet))

antall_passeringer
standardavvik


#d)
passeringer = 12
leser_ved_passering = 7
antall_tatt = 4
ønsket_tatt_og_leser = 3
P_leste_og_tatt = dhyper(ønsket_tatt_og_leser, leser_ved_passering, 
                        ( passeringer - leser_ved_passering ), antall_tatt)
P_leste_og_tatt


#f)
antall_biler = 16384
P_wunderbaum = 1 / 1024
lambda = antall_biler * P_wunderbaum

P_nøyaktig_15 = dpois(15, lambda)
P_nøyaktig_15


#2a)
rbinom(1, 10, 2/3)
rbinom(10, 1, 2/3)
sum(rbinom(10, 1, 2/3))


#b) 
data = c()

for (i in 1:50) {
  k = rbinom(1, 10, 2/3)
  data = c(data, k)
}

table(data)
hist(data, breaks = 100, col = "purple")

#d)
for (j in 1:5) {
  data = c()
  
  for (i in 1:50) {
    k = rbinom(1, 10, 2/3)
    data = c(data, k)
  }
  
  hist(data, breaks = 100, col = "purple",
       main = paste("Histogram, kjøring", j),
       xlab = "Antall suksesser")
}


#e)
for (j in 1:5) {
  data = c()
  
  for (i in 1:100000) {
    k = rbinom(1, 10, 2/3)
    data = c(data, k)
  }
  
  hist(data, breaks = 100, col = "purple",
       main = paste("Histogram, 100000 simuleringer, kjøring", j),
       xlab = "Antall suksesser")
}


#f)
x = 0:10
y = dbinom(x, 10, 2/3)

plot(x, y, type = "h", lwd = 3, col = "maroon",
     main = "Bin(10, 2/3)",
     xlab = "Antall suksesser", ylab = "P(X = x)")
points(x, y, pch = 19, col = "maroon")



#3)
for (j in 1:5) {
  data = c()
  
  for (i in 1:200) {
    k = rpois(1, 2)
    data = c(data, k)
  }
  
  hist(data,
       breaks = seq(-0.5, max(data) + 0.5, 1),
       col = "purple",
       main = paste("Poisson(2), kjøring", j),
       xlab = "Antall hendelser")
}



x = 0:10
y = dpois(x, 2)

plot(x, y, type = "h", lwd = 3, col = "maroon",
     main = "Poisson(2)",
     xlab = "Antall hendelser", ylab = "P(X = x)")
points(x, y, pch = 19, col = "maroon")



data = c()

for (i in 1:100000) {
  k = rpois(1, 2)
  data = c(data, k)
}

hist(data,
     breaks = seq(-0.5, max(data) + 0.5, 1),
     col = "purple",
     main = "Poisson(2), 100000 simuleringer",
     xlab = "Antall hendelser")



par(mfrow = c(1, 1))

# 4a
png("/Users/chris/Maths/MA-223/Oblig/2b/oppg4a.png", width = 1400, height = 700)
par(mfrow = c(1, 2))

mu = 37
sigma = 5

# Sannsynlighet
P_X_ge_33 = pnorm(33, mean = mu, sd = sigma, lower.tail = FALSE)
P_X_ge_33

# PDF med skyggelegging
xVals = seq(15, 60, 0.01)
yVals = dnorm(xVals, mean = mu, sd = sigma)

plot(xVals, yVals, type = "l", lwd = 2,
     main = "PDF for N(37,5)",
     xlab = "x", ylab = "f(x)")

shade_x = seq(33, 60, 0.01)
shade_y = dnorm(shade_x, mean = mu, sd = sigma)

polygon(c(33, shade_x, 60),
        c(0, shade_y, 0),
        col = "plum", border = NA)

lines(xVals, yVals, lwd = 2)
abline(v = 33, col = "maroon", lwd = 2)

# CDF med markering
yCumVals = pnorm(xVals, mean = mu, sd = sigma)

plot(xVals, yCumVals, type = "l", lwd = 2,
     main = "CDF for N(37,5)",
     xlab = "x", ylab = "F(x)")

F_33 = pnorm(33, mean = mu, sd = sigma)

points(33, F_33, pch = 19, col = "maroon")
abline(v = 33, col = "maroon", lty = 2)
abline(h = F_33, col = "maroon", lty = 2)

dev.off()


# 4b
png("/Users/chris/Maths/MA-223/Oblig/2b/oppg4b.png", width = 1400, height = 700)
par(mfrow = c(1, 2))

mu = 2
sigma = 1

a = qnorm(0.05, mean = mu, sd = sigma)
a

# PDF
xVals = seq(-2, 6, 0.01)
yVals = dnorm(xVals, mean = mu, sd = sigma)

plot(xVals, yVals, type = "l", lwd = 2,
     main = "PDF for N(2,1)",
     xlab = "x", ylab = "f(x)")

shade_x = seq(-2, a, 0.01)
shade_y = dnorm(shade_x, mean = mu, sd = sigma)

polygon(c(-2, shade_x, a),
        c(0, shade_y, 0),
        col = "plum", border = NA)

lines(xVals, yVals, lwd = 2)
abline(v = a, col = "maroon", lwd = 2)

# CDF
yCumVals = pnorm(xVals, mean = mu, sd = sigma)

plot(xVals, yCumVals, type = "l", lwd = 2,
     main = "CDF for N(2,1)",
     xlab = "x", ylab = "F(x)")

points(a, 0.05, pch = 19, col = "maroon")
abline(v = a, col = "maroon", lty = 2)
abline(h = 0.05, col = "maroon", lty = 2)

dev.off()


# 4c
png("/Users/chris/Maths/MA-223/Oblig/2b/oppg4c.png", width = 1400, height = 700)
par(mfrow = c(1, 2))

lambda = 4.4

mu_T = 1 / lambda
sigma_T = 1 / lambda
P_interval = pexp(0.28, rate = lambda) - pexp(0.15, rate = lambda)

mu_T
sigma_T
P_interval

# PDF
xVals = seq(0, 1.2, 0.001)
yVals = dexp(xVals, rate = lambda)

plot(xVals, yVals, type = "l", lwd = 2,
     main = "PDF for Exp(4.4)",
     xlab = "t", ylab = "f(t)")

shade_x = seq(0.15, 0.28, 0.001)
shade_y = dexp(shade_x, rate = lambda)

polygon(c(0.15, shade_x, 0.28),
        c(0, shade_y, 0),
        col = "plum", border = NA)

lines(xVals, yVals, lwd = 2)
abline(v = 0.15, col = "maroon", lty = 2)
abline(v = 0.28, col = "maroon", lty = 2)

# CDF
yCumVals = pexp(xVals, rate = lambda)

plot(xVals, yCumVals, type = "l", lwd = 2,
     main = "CDF for Exp(4.4)",
     xlab = "t", ylab = "F(t)")

F_015 = pexp(0.15, rate = lambda)
F_028 = pexp(0.28, rate = lambda)

points(c(0.15, 0.28), c(F_015, F_028), pch = 19, col = "maroon")
abline(v = 0.15, col = "maroon", lty = 2)
abline(v = 0.28, col = "maroon", lty = 2)
abline(h = F_015, col = "maroon", lty = 3)
abline(h = F_028, col = "maroon", lty = 3)

dev.off()


# 4d
png("/Users/chris/Maths/MA-223/Oblig/2b/oppg4d.png", width = 1400, height = 700)
par(mfrow = c(1, 2))

t_verdi = qt(0.1, df = 4)
t_verdi

# PDF
xVals = seq(-5, 5, 0.01)
yVals = dt(xVals, df = 4)

plot(xVals, yVals, type = "l", lwd = 2,
     main = "PDF for t(4)",
     xlab = "x", ylab = "f(x)")

shade_x = seq(-5, t_verdi, 0.01)
shade_y = dt(shade_x, df = 4)

polygon(c(-5, shade_x, t_verdi),
        c(0, shade_y, 0),
        col = "plum", border = NA)

lines(xVals, yVals, lwd = 2)
abline(v = t_verdi, col = "maroon", lwd = 2)

# CDF
yCumVals = pt(xVals, df = 4)

plot(xVals, yCumVals, type = "l", lwd = 2,
     main = "CDF for t(4)",
     xlab = "x", ylab = "F(x)")

points(t_verdi, 0.1, pch = 19, col = "maroon")
abline(v = t_verdi, col = "maroon", lty = 2)
abline(h = 0.1, col = "maroon", lty = 2)

dev.off()


# 4e
png("/Users/chris/Maths/MA-223/Oblig/2b/oppg4e.png", width = 1400, height = 700)
par(mfrow = c(1, 2))

a = 3
b = 2

mu_X = a / (a + b)
sigma_X = sqrt((a * b) / (((a + b)^2) * (a + b + 1)))
P_interval = pbeta(0.65, a, b) - pbeta(0.4, a, b)

mu_X
sigma_X
P_interval

# PDF
xVals = seq(0, 1, 0.001)
yVals = dbeta(xVals, a, b)

plot(xVals, yVals, type = "l", lwd = 2,
     main = "PDF for Beta(3,2)",
     xlab = "x", ylab = "f(x)")

shade_x = seq(0.4, 0.65, 0.001)
shade_y = dbeta(shade_x, a, b)

polygon(c(0.4, shade_x, 0.65),
        c(0, shade_y, 0),
        col = "plum", border = NA)

lines(xVals, yVals, lwd = 2)
abline(v = 0.4, col = "maroon", lty = 2)
abline(v = 0.65, col = "maroon", lty = 2)

# CDF
yCumVals = pbeta(xVals, a, b)

plot(xVals, yCumVals, type = "l", lwd = 2,
     main = "CDF for Beta(3,2)",
     xlab = "x", ylab = "F(x)")

F_04 = pbeta(0.4, a, b)
F_065 = pbeta(0.65, a, b)

points(c(0.4, 0.65), c(F_04, F_065), pch = 19, col = "maroon")
abline(v = 0.4, col = "maroon", lty = 2)
abline(v = 0.65, col = "maroon", lty = 2)
abline(h = F_04, col = "maroon", lty = 3)
abline(h = F_065, col = "maroon", lty = 3)

dev.off()


# 4f
png("/Users/chris/Maths/MA-223/Oblig/2b/oppg4f.png", width = 1400, height = 700)
par(mfrow = c(1, 2))

k = 0.5
lambda = 3

mu_T = lambda * gamma(1 + 1/k)
sigma_T = sqrt(lambda^2 * (gamma(1 + 2/k) - gamma(1 + 1/k)^2))
P_interval = pweibull(4, shape = k, scale = lambda) - pweibull(2, shape = k, scale = lambda)

mu_T
sigma_T
P_interval

# PDF
xVals = seq(0, 20, 0.01)
yVals = dweibull(xVals, shape = k, scale = lambda)

plot(xVals, yVals, type = "l", lwd = 2,
     main = "PDF for Weibull(0.5,3)",
     xlab = "t", ylab = "f(t)")

shade_x = seq(2, 4, 0.01)
shade_y = dweibull(shade_x, shape = k, scale = lambda)

polygon(c(2, shade_x, 4),
        c(0, shade_y, 0),
        col = "plum", border = NA)

lines(xVals, yVals, lwd = 2)
abline(v = 2, col = "maroon", lty = 2)
abline(v = 4, col = "maroon", lty = 2)

# CDF
yCumVals = pweibull(xVals, shape = k, scale = lambda)

plot(xVals, yCumVals, type = "l", lwd = 2,
     main = "CDF for Weibull(0.5,3)",
     xlab = "t", ylab = "F(t)")

F_2 = pweibull(2, shape = k, scale = lambda)
F_4 = pweibull(4, shape = k, scale = lambda)

points(c(2, 4), c(F_2, F_4), pch = 19, col = "maroon")
abline(v = 2, col = "maroon", lty = 2)
abline(v = 4, col = "maroon", lty = 2)
abline(h = F_2, col = "maroon", lty = 3)
abline(h = F_4, col = "maroon", lty = 3)

dev.off()

# Reset layout afterwards
par(mfrow = c(1, 1))



# ============================================================
# Oppgave 5 - lagrer alle figurer i valgt mappe
# ============================================================

setwd("/Users/chris/Maths/MA-223/Oblig/2b")
set.seed(223)

# Reset plotting layout
par(mfrow = c(1, 1))

# ------------------------------------------------------------
# 5a) Standard normalfordeling
# ------------------------------------------------------------
png("oppg5a_standard_normal.png", width = 1400, height = 900)

x = seq(-4, 4, length = 1000)
y = dnorm(x, mean = 0, sd = 1)

plot(x, y, type = "l", lwd = 2,
     main = "Standard normalfordeling",
     xlab = "x", ylab = "Tetthet")

dev.off()

# ------------------------------------------------------------
# 5b) Ett tilfeldig trekk
# ------------------------------------------------------------
tilfeldig_trekk = rnorm(1, mean = 0, sd = 1)
tilfeldig_trekk

# ------------------------------------------------------------
# 5c) 50 trekk + teoretisk kurve
# ------------------------------------------------------------
png("oppg5c_50_trekk.png", width = 1400, height = 900)

data_50 = rnorm(50, mean = 0, sd = 1)

hist(data_50, probability = TRUE, breaks = 10,
     col = "purple",
     main = "50 trekk fra N(0,1)",
     xlab = "Verdi")

xVals = seq(-4, 4, 0.01)
yVals = dnorm(xVals, mean = 0, sd = 1)
lines(xVals, yVals, col = "maroon", lwd = 2)

dev.off()

# ------------------------------------------------------------
# 5d) 500 trekk + teoretisk kurve
# ------------------------------------------------------------
png("oppg5d_500_trekk.png", width = 1400, height = 900)

data_500 = rnorm(500, mean = 0, sd = 1)

hist(data_500, probability = TRUE, breaks = 20,
     col = "purple",
     main = "500 trekk fra N(0,1)",
     xlab = "Verdi")

xVals = seq(-4, 4, 0.01)
yVals = dnorm(xVals, mean = 0, sd = 1)
lines(xVals, yVals, col = "maroon", lwd = 2)

dev.off()

# ------------------------------------------------------------
# 5d) 50000 trekk + teoretisk kurve
# ------------------------------------------------------------
png("oppg5d_50000_trekk.png", width = 1400, height = 900)

data_50000 = rnorm(50000, mean = 0, sd = 1)

hist(data_50000, probability = TRUE, breaks = 40,
     col = "purple",
     main = "50000 trekk fra N(0,1)",
     xlab = "Verdi")

xVals = seq(-4, 4, 0.01)
yVals = dnorm(xVals, mean = 0, sd = 1)
lines(xVals, yVals, col = "maroon", lwd = 2)

dev.off()

# ------------------------------------------------------------
# 5e) Oppgitt kode med N = 100
# ------------------------------------------------------------
png("oppg5e_N100.png", width = 1400, height = 900)

N = 100
h = (rbinom(N, 100, 0.5) - 50) / 5
hist(h, probability = TRUE, breaks = seq(-8.05, 8.05, 0.1),
     col = "purple",
     main = "Standardisert binomisk fordeling, N = 100",
     xlab = "h")
xVals = seq(-4, 4, 0.01)
yVals = 2 * dnorm(xVals, 0, 1)
lines(xVals, yVals, col = "maroon", type = "l", lwd = 2)

dev.off()

# ------------------------------------------------------------
# 5f) N = 1000
# ------------------------------------------------------------
png("oppg5f_N1000.png", width = 1400, height = 900)

N = 1000
h = (rbinom(N, 100, 0.5) - 50) / 5
hist(h, probability = TRUE, breaks = seq(-8.05, 8.05, 0.1),
     col = "purple",
     main = "Standardisert binomisk fordeling, N = 1000",
     xlab = "h")
xVals = seq(-4, 4, 0.01)
yVals = 2 * dnorm(xVals, 0, 1)
lines(xVals, yVals, col = "maroon", type = "l", lwd = 2)

dev.off()

# ------------------------------------------------------------
# 5f) N = 10000
# ------------------------------------------------------------
png("oppg5f_N10000.png", width = 1400, height = 900)

N = 10000
h = (rbinom(N, 100, 0.5) - 50) / 5
hist(h, probability = TRUE, breaks = seq(-8.05, 8.05, 0.1),
     col = "purple",
     main = "Standardisert binomisk fordeling, N = 10000",
     xlab = "h")
xVals = seq(-4, 4, 0.01)
yVals = 2 * dnorm(xVals, 0, 1)
lines(xVals, yVals, col = "maroon", type = "l", lwd = 2)

dev.off()

# ------------------------------------------------------------
# 5f) N = 100000
# ------------------------------------------------------------
png("oppg5f_N100000.png", width = 1400, height = 900)

N = 100000
h = (rbinom(N, 100, 0.5) - 50) / 5
hist(h, probability = TRUE, breaks = seq(-8.05, 8.05, 0.1),
     col = "purple",
     main = "Standardisert binomisk fordeling, N = 100000",
     xlab = "h")
xVals = seq(-4, 4, 0.01)
yVals = 2 * dnorm(xVals, 0, 1)
lines(xVals, yVals, col = "maroon", type = "l", lwd = 2)

dev.off()

# Reset plotting layout afterwards
par(mfrow = c(1, 1))


#6a)
x = seq(0, 1, length = 1000)
y = dbeta(x, 3, 7)

plot(x, y, type = "l", lwd = 2,
     main = expression(beta(3,7)),
     xlab = "x", ylab = "Tetthet")

#b)
par(mfrow = c(3, 3))

verdier = c(1, 2, 5)

for (a in verdier) {
  for (b in verdier) {
    x = seq(0, 1, length = 1000)
    y = dbeta(x, a, b)
    
    plot(x, y, type = "l", lwd = 2,
         main = paste("Beta(", a, ",", b, ")", sep = ""),
         xlab = "x", ylab = "Tetthet")
  }
}