library(compareGroups)



library(readxl)
dig <- read_excel("Desktop/ISED/Synthèse Projet de recherche/USS Canada/Analyse étude de base/basededonnesetanalyseprliminaire/Dossier analyse des données/Base ménage.xlsx")
View(Base_me_nage)




library(readr)
dig <- read_delim("Desktop/ISED/Synthèse Projet de recherche/USS Canada/Analyse étude de base/basededonnesetanalyseprliminaire/Dossier analyse des données/Base ménage CSV.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(dig)

dig<-read.csv2(file.choose(),stringsAsFactors = T)  

library(readr)
dig <- read_delim("Desktop/ISED/Synthèse Projet de recherche/USS Canada/Analyse étude de base/basededonnesetanalyseprliminaire/Base Aout/base ménage_2024-08-19 CSV.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
str(dig)

# Analyse descriptive
res5 <- compareGroups( ~., dig)
restab5 <- createTable(res5, hide.no = "no")
restab5
print(restab5, header.labels = c(p.overall = "p-value"))
export2md(restab5, first.strip = TRUE, header.labels = c(p.overall = "p-value"))

export2word(restab5, file='descUSS.docx')

names(dig)

# Stratifier par sexe
res5 <- compareGroups(Genre ~., dig)
restab5 <- createTable(res5, hide.no = "no",show.all=TRUE, show.p.overall=TRUE)
restab5
print(restab5, header.labels = c(p.overall = "p-value"))
export2md(restab5, first.strip = TRUE, header.labels = c(p.overall = "p-value"))

export2word(restab5, file='desc glob dig strat sexe.docx')

# Stratifier par village
res5 <- compareGroups(Village ~., dig)
restab5 <- createTable(res5, hide.no = "no", show.all=TRUE, show.p.mul=TRUE)
restab5
print(restab5, header.labels = c(p.overall = "p-value"))
export2md(restab5, strip=TRUE,first.strip = TRUE, header.labels = c(p.overall = "p-value"))

export2word(restab5, file='desc USS strat Village.docx')



# Stratifier par village
res5 <- compareGroups(Village ~., dig)
restab5 <- createTable(res5, hide.no = "no", show.all=TRUE)
restab5
print(restab5, header.labels = c(p.overall = "p-value"))
export2md(restab5, first.strip = TRUE, header.labels = c(p.overall = "p-value"))

export2word(restab5, file='desc dig strat Village.docx')




str(dig)



res <- compareGroups(Village ~., dig)
restab <- createTable(res, hide.no="no")

ta=strataTable(restab, "Genre")

export2md(ta, first.strip = TRUE, header.labels = c(p.overall = "p-value"))

export2word(ta, file='desc USS strat Village par sexe.docx')



res <- compareGroups(Genre ~., dig)
restab <- createTable(res, hide.no="no")

ta=strataTable(restab, "Village")

export2md(ta, first.strip = TRUE, header.labels = c(p.overall = "p-value"))

export2word(ta, file='desc USS strat Village par sexe.docx')



restab <- strataTable(descrTable(Village ~ . -id, regicor), "sex")




library(table1)

a=table1(~ ., data=dig)

a

vil=table1(~ . | Village, data=dig)
vil

sex=vil=table1(~ . | genre, data=dig)
sex

sex=vil=table1(~ . | genre, data=dig, overall=T)
sex


vilsex=table1(~ . | Village*Genre, data=dig)









