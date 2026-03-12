#Twist-posen
#a) Du skal rydde opp etter en fest, og finner en Twist-pose med kun 7 kokos 
#og 13 lakris igjen. Du plukker en tilfeldig twist. Hva er sannsynligheten
#for at den blir en kokos?
kokos <- 7
lakris <- 13
total = kokos + lakris

P_kokos = kokos / total


#b) Du liker verken lakris eller kokos, så du legger den du har plukket tilbake
#i posen for å plukke en ny. Du prøver deg slik 5 ganger, og legger tilbake 
#for hver gang. Lag stoplediagram over sannsynligheten for at du har plukket
#kokos respektivt 0, 1, 2, 3, 4 og 5 ganger
N <- total
S <- kokos
p <- S / N
n <- 5

#Formel 4.2.3 i formelheftet
ProbComb = function(k){
  choose(n, k) * (p^k) * (1 - p)^(n - k)
}
ProbComb(0)
ProbComb(1)
ProbComb(2)
ProbComb(3)
ProbComb(4)
ProbComb(5)




#Litt senere på morran er du så sultern at selv kokos og lakris går an, så
#du plukker 5 twist og spiser dem. Lag stolpediagram over sannsynligheten for
#at du spiser respektive 0, 1, 2, 3, 4 og 5 kokos
ProbCombNoReplace = function(k){
  (choose(S, k) * choose((N - S), n - k) / choose(N, n))
}
ProbCombNoReplace(0)
ProbCombNoReplace(1)
ProbCombNoReplace(2)
ProbCombNoReplace(3)
ProbCombNoReplace(4)
ProbCombNoReplace(5)


#Diagrammer
k_values <- 0:n
probs_binom <- sapply(k_values, ProbComb)
probs_hyper <- sapply(k_values, ProbCombNoReplace)

max_b <- max(probs_binom)
max_h <- max(probs_hyper)

max_y <- max(max_b, max_h)

ylim_val <- c(0, max_y * 1.1)


par(mfrow = c(1, 2))

barplot(probs_binom, names.arg = k_values, col = "steelblue",
        ylim = ylim_val, xlab = "Antall kokos", ylab = "Sannsynlighet", 
        main = "Med tilbakelegging")

barplot(probs_hyper, names.arg = k_values, col = "darkorange", 
        ylim = ylim_val, xlab = "Antall kokos", ylab = "Sannsynlighet", 
        main = "Uten tilbakelegging")
par(mfrow = c(1, 1))

