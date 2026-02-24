install.packages("VennDiagram")

library(VennDiagram)

P_A <- 0.8
P_B <- 0.2
P_AB <- 0.1

bare_A <- P_A - P_AB
bare_B <- P_B - P_AB
utenfor <- 1 - (bare_A + bare_B + P_AB)
bare_A
bare_B
utenfor

#Sjekk at verdier stemmer
bare_A + bare_B + utenfor + P_AB


#3b
P_union <- P_A + P_B - P_AB
P_union

P_A_uten_B <- bare_A
P_A_uten_B


#3c
P_komplement_intersection <- 1 - P_AB
P_komplement_intersection
#DeMorgan's lov sier at P(AB)^c = P(A^c u B^c)
#Derfor er sannsynligheten den samme


#3d
P_A_gitt_B <- P_AB / P_B
P_B_gitt_A <- P_AB / P_A
P_A_gitt_B
P_B_gitt_A
#Forskjellen mellom disse to er at P_A_gitt_B er
#sannsynligheten for at A skjer gitt at B allerede
#har skjedd - P_B_gitt_A er da sannsynligheten for
#at B skjer gitt at A allerede har skjedd


#Venn Diagrammet
draw.pairwise.venn(area1 = P_A, area2 = P_B,
                   cross.area = P_AB, category = c("A", "B"),
                   fill = c("lightblue", "pink"),
                   lty = "blank")
