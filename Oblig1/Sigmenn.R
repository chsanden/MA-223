library(readxl)
library(dplyr)
library(ggplot2)

laban <- read_excel("Laban_raw_98.xlsx")
coop <- read_excel("Coop_raw_98.xlsx")


#Finner kolonne-navn
names(laban)
names(coop)


#Frekvenser
freq_laban <- laban %>%
  count(Lengde) %>%
  arrange(Lengde)

#Velger å trekke fra de tre første radene i coop tabellen
#Gjør dette for å få et mer representativt bilde av dataen
#De tre første strekkene var ikke like metode som resten
freq_coop <- coop %>%
  slice(4:n()) %>%
  count(Lengde) %>%
  arrange(Lengde)

freq_laban
freq_coop


freq_diag_laban <- ggplot(freq_laban, aes(x=`Lengde`, y=n)) +
  geom_col() +
  geom_vline(aes(xintercept=mid_laban, color="Middelverdi (x̄)"), linewidth=0.9) +
  geom_vline(aes(xintercept=med_laban, color="Median (x̃)"), linewidth=0.9) +
  geom_vline(aes(xintercept=type_laban, color="Typetall (x_max)"), linewidth=0.9) +
  geom_vline(aes(xintercept=mid_laban - sd_laban, color="x̄ − s"), linewidth=0.9, linetype="dashed") +
  geom_vline(aes(xintercept=mid_laban + sd_laban, color="x̄ + s"), linewidth=0.9, linetype="dashed") +
  labs(title="Frekvensdiagram - Laban", x="Lengde", y="Antall", color="Linjer") +
  theme_minimal() + theme(legend.position="bottom")

freq_diag_coop <- ggplot(freq_coop, aes(x=`Lengde`, y=n)) +
  geom_col() +
  geom_vline(aes(xintercept=mid_coop, color="Middelverdi (x̄)"), linewidth=0.9) +
  geom_vline(aes(xintercept=med_coop, color="Median (x̃)"), linewidth=0.9) +
  geom_vline(aes(xintercept=type_coop, color="Typetall (x_max)"), linewidth=0.9) +
  geom_vline(aes(xintercept=mid_coop - sd_laban, color="x̄ − s"), linewidth=0.9, linetype="dashed") +
  geom_vline(aes(xintercept=mid_coop + sd_laban, color="x̄ + s"), linewidth=0.9, linetype="dashed") +
  labs(title="Frekvensdiagram - Coop", x="Lengde", y="Antall", color="Linjer") +
  theme_minimal() + theme(legend.position="bottom")
  

freq_diag_laban
freq_diag_coop


#Kumulative frekvenser
cum_laban <- freq_laban %>%
  mutate(Kumulativ_frekvens = cumsum(n))

cum_coop <- freq_coop %>%
  mutate(Kumulativ_frekvens = cumsum(n))

cum_laban
cum_coop


#Kumulative frekvensdiagrammer
cum_diag_laban <- ggplot(cum_laban, aes(x=`Lengde`, y=Kumulativ_frekvens)) +
  geom_step(linewidth=0.9) +
  geom_vline(aes(xintercept=mid_laban, color="Middelverdi (x̄)"), linewidth=0.9) +
  geom_vline(aes(xintercept=med_laban, color="Median (x̃)"), linewidth=0.9) +
  geom_vline(aes(xintercept=type_laban, color="Typetall (x_max)"), linewidth=0.9) +
  geom_vline(aes(xintercept=mid_laban - sd_laban, color="x̄ − s"), linewidth=0.9, linetype="dashed") +
  geom_vline(aes(xintercept=mid_laban + sd_laban, color="x̄ + s"), linewidth=0.9, linetype="dashed") +
  labs(title="Kumulativt frekvensdiagram - Laban", x="Lengde", y="Kumulativ frekvens", color="Linjer") +
  theme_minimal() + theme(legend.position="bottom")

cum_diag_coop <- ggplot(cum_coop, aes(x=`Lengde`, y=Kumulativ_frekvens)) +
  geom_step(linewidth=0.9) +
  geom_vline(aes(xintercept=mid_coop, color="Middelverdi (x̄)"), linewidth=0.9) +
  geom_vline(aes(xintercept=med_coop, color="Median (x̃)"), linewidth=0.9) +
  geom_vline(aes(xintercept=type_coop, color="Typetall (x_max)"), linewidth=0.9) +
  geom_vline(aes(xintercept=mid_coop - sd_laban, color="x̄ − s"), linewidth=0.9, linetype="dashed") +
  geom_vline(aes(xintercept=mid_coop + sd_laban, color="x̄ + s"), linewidth=0.9, linetype="dashed") +
  labs(title="Kumulativt frekvensdiagram - Laban", x="Lengde", y="Kumulativ frekvens", color="Linjer") +
  theme_minimal() + theme(legend.position="bottom") 

cum_diag_laban
cum_diag_laban




#Medianer
mid_laban <- sum(cum_laban$Lengde*cum_laban$n) / sum(cum_laban$n) 
mid_laban

mid_coop <- sum(cum_coop$Lengde*cum_coop$n) / sum(cum_coop$n)
mid_coop

med_pos_laban <- sum(cum_laban$n + 1) / 2
med_pos_laban
med_laban <- cum_laban$Lengde[which(cumsum(cum_laban$n) >= med_pos_laban)[1]]

med_pos_coop <- sum(cum_coop$n + 1) /2
med_pos_coop
med_coop <- cum_coop$Lengde[which(cumsum(cum_coop$n) >= med_pos_coop)[1]]

med_coop
med_laban


#Typetall
type_laban <- freq_laban$Lengde[which.max(freq_laban$n)]
type_coop <- freq_coop$Lengde[which.max(freq_coop$n)]

type_laban
type_coop


#Standardavvik
sd_laban <- sd(laban$Lengde)
sd_coop <- sd(coop$Lengde)

sd_laban
sd_coop


#Oppsummering
tabell_laban <- data.frame(Mål = c("Middelverdi", "Median", "Typetall",  "Standardavvik"),
                           Verdi = c(mid_laban, med_laban, type_laban, sd_laban))
tabell_coop <- data.frame(Mål = c("Middelverdi", "Median", "Typetall", "Standardavvik"),
                          Verdi = c(mid_coop, med_coop, type_coop, sd_coop))
tabell_laban
tabell_coop

oppsummering <- data.frame(Mål = c("Middelverdi", "Median", "Typetall",  "Standardavvik"),
                           Laban = c(mid_laban, med_laban, type_laban, sd_laban),
                           Coop = c(mid_coop, med_coop, type_coop, sd_coop))

oppsummering$Laban <- round(oppsummering$Laban, 2)
oppsummering$Coop <- round(oppsummering$Coop, 2)

oppsummering
write.csv2(oppsummering, "oppsummering.csv", row.names = FALSE)



