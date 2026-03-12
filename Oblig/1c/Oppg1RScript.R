#Hvor mange måter kan du plukke 5 elementer fra 12
#når trekket er...

n <- 12
k <- 5

#a) ordnet, med tilbakelegging
n^k

#b) uordnet, med tilbakelegging
choose((n + k - 1), k)

#c) ordnet, uten tilbakelegging
factorial(n) / factorial(n - k)

#d) uordnet, uten tilbakelegging
choose(n, k)

#Eksempel på a) - PIN-kode med 5 siffer. Man kan gjenbruke ett siffer flere
#ganger

#Eksempel på b) - Velg 5 is-kuler fra et utvalg av 5 smaker. Hver kule trenger
#ikke være unik

#Eksempel på c) - Velg 5 studenter fra en gruppe på 12 og sorter dem etter alder

#Eksempel på d) - Trekk 5 kort fra en bunke på 12 uten å legge dem tilbake