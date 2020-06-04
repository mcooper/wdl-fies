##############################################3
# Make example CSV for Poli
######################################################

#Fields
# -GDLCODE
# -2020.fies.urb
# -2020.fies.rur
# -2025.fies.urb
# -2025.fies.rur
# -2030.fies.urb
# -2030.fies.rur
# -2020.pop.urb
# -2020.pop.rur
# -2025.pop.urb
# -2025.pop.rur
# -2030.pop.urb
# -2030.pop.rur
# -2020.stunt
# -2025.stunt
# -2030.stunt

example <- data.frame(GDLCODE=unique(ssp_past$GDLCODE))

cols <- c("2025.fies.urb",
"2025.fies.rur",
"2030.fies.urb",
"2030.fies.rur",
"2020.pop.urb",
"2020.pop.rur",
"2025.pop.urb",
"2025.pop.rur",
"2030.pop.urb",
"2030.pop.rur",
"2020.stunt",
"2025.stunt",
"2030.stunt")

for (c in cols){
	if (grepl('fies|stunt', c)){
		d <- runif(nrow(example))
	}
	if (grepl('pop', c)){
		d <- rpois(nrow(example), 100000)
	}
	
	example[ , c] <- d	

}

write.csv(example, 'figures/example_hunger_data.csv', row.names=F)

