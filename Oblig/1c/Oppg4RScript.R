#4a
myChoose = function(n, k){
  factorial(n) / (factorial(k) * factorial(n - k))
}

choose(4, 2)
myChoose(4, 2)


#4b
#Definerer funksjonen bd med parameter n
bd = function(n){
  
  #Setter antall "frie" dager
  dager = 365 + 1 - 1:n
  
  #Deler antall "frie" dager på antall
  #dager i et år - gir et desimaltall som
  #ganges med 100 for å få den eksakte prosentverdien
  #for sannsynligheten at ingen i samme set har 
  #samme bursdag
  sjanser = dager / 365
  
  #Utfører utregningen og lagrer resultatet til 
  #variablen output
  output = prod(sjanser)
  
  #Printer resultatet
  output
}
#Regner ut og printer sannsynligheten for at 
#to personer i samme set har samme bursdag
#Vi gjør "1 - bd()" for å finne sannsynligheten
1 - bd(22)
1 - bd(23)
#Denne funksjonen regner sannsynligheten for at ingen
#personer i samme set har bursdag på samme dag
#Bursdagsparadokset er et berømt statistikk eksempel


#4c
#Setter startpunktet til en Random Number Generator(RNG)
set.seed(314)

#Lager en mengde / sekvens fra 3 til 27 (3, 4, 5, ..., 27)
base = 3:27

#Trekker 14 tall fra sekvensen "base" med tilbakelegging
a = sample(base, 14, replace=TRUE)


#4d
totalSum = 0
for(verdi in a){
  totalSum = totalSum + verdi
}
totalSum
sum(a)


#4e
totalProd = 1
for (verdi in a) {
  totalProd = totalProd * verdi
}
totalProd
prod(a)


#4f
#Med tilbakelegging - kan få samme verdi flere gange
a = sample(base, 14, replace=TRUE)
sort(a)

#Uten tilbakelegging - kan kun få atomære verdier
a = sample(base, 14, replace=FALSE)
sort(a)







