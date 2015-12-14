
# Lendo o csv
dados <- read.csv("dados_para_R.csv", header = T, sep = ",")

# Organizando os dados
separaDados <- function(stringDia){
  nameData <- dados[dados$diaSemana == stringDia,]
  nameData <- nameData[order(nameData$hora, decreasing=c(FALSE)), ]
}

Domingo <- separaDados("Domingo")
Segunda <- separaDados("Segunda")
Terca <- separaDados("Terca")
Quarta <- separaDados("Quarta")
Quinta <- separaDados("Quinta")
Sexta <- separaDados("Sexta")
Sabado <- separaDados("Sabado")


# Calculando todas probabilidades e colocando em uma matriz
probFridge <- matrix(nrow=24,ncol=7)
probSmallFridge <- matrix(nrow=24,ncol=7)
probAirConditioning <- matrix(nrow=24,ncol=7)
probDesktop_Computer <- matrix(nrow=24,ncol=7)
probFan <- matrix(nrow=24,ncol=7)
probGrill <- matrix(nrow=24,ncol=7)
probMicrowave <- matrix(nrow=24,ncol=7)
probOther <- matrix(nrow=24,ncol=7)
probShower <- matrix(nrow=24,ncol=7)
probToaster <- matrix(nrow=24,ncol=7)

# As colunas V1, V2, ..., V7.
# Representam: Domingo, Segunda, ..., Sabado.
# Cada linha eh uma hora do dia. Por exemplo:
#   - linha 1 = 00 hora (meia noite)
#   - linha 24 = 23 horas (onze da noite)

# DOMINGO
# verificar cada equipamento por vez (todas as horas)
for(i in 0:23){
  probFridge[i+1,1] <- sum(Domingo$hora[Domingo$equipamento == "Fridge"] == i) / sum(Domingo$hora == i)
  probSmallFridge[i+1,1] <- sum(Domingo$hora[Domingo$equipamento == "SmallFridge"] == i) / sum(Domingo$hora == i)
  probAirConditioning[i+1,1] <- sum(Domingo$hora[Domingo$equipamento == "AirConditioning"] == i) / sum(Domingo$hora == i)
  probDesktop_Computer[i+1,1] <- sum(Domingo$hora[Domingo$equipamento == "Desktop_Computer"] == i) / sum(Domingo$hora == i)
  probFan[i+1,1] <- sum(Domingo$hora[Domingo$equipamento == "Fan"] == i) / sum(Domingo$hora == i)
  probGrill[i+1,1] <- sum(Domingo$hora[Domingo$equipamento == "Grill"] == i) / sum(Domingo$hora == i)
  probMicrowave[i+1,1] <- sum(Domingo$hora[Domingo$equipamento == "Microwave"] == i) / sum(Domingo$hora == i)
  probOther[i+1,1] <- sum(Domingo$hora[Domingo$equipamento == "Other"] == i) / sum(Domingo$hora == i)
  probShower[i+1,1] <- sum(Domingo$hora[Domingo$equipamento == "Shower"] == i) / sum(Domingo$hora == i)
  probToaster[i+1,1] <- sum(Domingo$hora[Domingo$equipamento == "Toaster"] == i) / sum(Domingo$hora == i)
}

# SEGUNDA
for(i in 0:23){
  probFridge[i+1,2] <- sum(Segunda$hora[Segunda$equipamento == "Fridge"] == i) / sum(Segunda$hora == i)
  probSmallFridge[i+1,2] <- sum(Segunda$hora[Segunda$equipamento == "SmallFridge"] == i) / sum(Segunda$hora == i)
  probAirConditioning[i+1,2] <- sum(Segunda$hora[Segunda$equipamento == "AirConditioning"] == i) / sum(Segunda$hora == i)
  probDesktop_Computer[i+1,2] <- sum(Segunda$hora[Segunda$equipamento == "Desktop_Computer"] == i) / sum(Segunda$hora == i)
  probFan[i+1,2] <- sum(Segunda$hora[Segunda$equipamento == "Fan"] == i) / sum(Segunda$hora == i)
  probGrill[i+1,2] <- sum(Segunda$hora[Segunda$equipamento == "Grill"] == i) / sum(Segunda$hora == i)
  probMicrowave[i+1,2] <- sum(Segunda$hora[Segunda$equipamento == "Microwave"] == i) / sum(Segunda$hora == i)
  probOther[i+1,2] <- sum(Segunda$hora[Segunda$equipamento == "Other"] == i) / sum(Segunda$hora == i)
  probShower[i+1,2] <- sum(Segunda$hora[Segunda$equipamento == "Shower"] == i) / sum(Segunda$hora == i)
  probToaster[i+1,2] <- sum(Segunda$hora[Segunda$equipamento == "Toaster"] == i) / sum(Segunda$hora == i)
}

# TERCA
for(i in 0:23){
  probFridge[i+1,3] <- sum(Terca$hora[Terca$equipamento == "Fridge"] == i) / sum(Terca$hora == i)
  probSmallFridge[i+1,3] <- sum(Terca$hora[Terca$equipamento == "SmallFridge"] == i) / sum(Terca$hora == i)
  probAirConditioning[i+1,3] <- sum(Terca$hora[Terca$equipamento == "AirConditioning"] == i) / sum(Terca$hora == i)
  probDesktop_Computer[i+1,3] <- sum(Terca$hora[Terca$equipamento == "Desktop_Computer"] == i) / sum(Terca$hora == i)
  probFan[i+1,3] <- sum(Terca$hora[Terca$equipamento == "Fan"] == i) / sum(Terca$hora == i)
  probGrill[i+1,3] <- sum(Terca$hora[Terca$equipamento == "Grill"] == i) / sum(Terca$hora == i)
  probMicrowave[i+1,3] <- sum(Terca$hora[Terca$equipamento == "Microwave"] == i) / sum(Terca$hora == i)
  probOther[i+1,3] <- sum(Terca$hora[Terca$equipamento == "Other"] == i) / sum(Terca$hora == i)
  probShower[i+1,3] <- sum(Terca$hora[Terca$equipamento == "Shower"] == i) / sum(Terca$hora == i)
  probToaster[i+1,3] <- sum(Terca$hora[Terca$equipamento == "Toaster"] == i) / sum(Terca$hora == i)
}

# QUARTA
for(i in 0:23){
  probFridge[i+1,4] <- sum(Quarta$hora[Quarta$equipamento == "Fridge"] == i) / sum(Quarta$hora == i)
  probSmallFridge[i+1,4] <- sum(Quarta$hora[Quarta$equipamento == "SmallFridge"] == i) / sum(Quarta$hora == i)
  probAirConditioning[i+1,4] <- sum(Quarta$hora[Quarta$equipamento == "AirConditioning"] == i) / sum(Quarta$hora == i)
  probDesktop_Computer[i+1,4] <- sum(Quarta$hora[Quarta$equipamento == "Desktop_Computer"] == i) / sum(Quarta$hora == i)
  probFan[i+1,4] <- sum(Quarta$hora[Quarta$equipamento == "Fan"] == i) / sum(Quarta$hora == i)
  probGrill[i+1,4] <- sum(Quarta$hora[Quarta$equipamento == "Grill"] == i) / sum(Quarta$hora == i)
  probMicrowave[i+1,4] <- sum(Quarta$hora[Quarta$equipamento == "Microwave"] == i) / sum(Quarta$hora == i)
  probOther[i+1,4] <- sum(Quarta$hora[Quarta$equipamento == "Other"] == i) / sum(Quarta$hora == i)
  probShower[i+1,4] <- sum(Quarta$hora[Quarta$equipamento == "Shower"] == i) / sum(Quarta$hora == i)
  probToaster[i+1,4] <- sum(Quarta$hora[Quarta$equipamento == "Toaster"] == i) / sum(Quarta$hora == i)
}

# QUINTA
for(i in 0:23){
  probFridge[i+1,5] <- sum(Quinta$hora[Quinta$equipamento == "Fridge"] == i) / sum(Quinta$hora == i)
  probSmallFridge[i+1,5] <- sum(Quinta$hora[Quinta$equipamento == "SmallFridge"] == i) / sum(Quinta$hora == i)
  probAirConditioning[i+1,5] <- sum(Quinta$hora[Quinta$equipamento == "AirConditioning"] == i) / sum(Quinta$hora == i)
  probDesktop_Computer[i+1,5] <- sum(Quinta$hora[Quinta$equipamento == "Desktop_Computer"] == i) / sum(Quinta$hora == i)
  probFan[i+1,5] <- sum(Quinta$hora[Quinta$equipamento == "Fan"] == i) / sum(Quinta$hora == i)
  probGrill[i+1,5] <- sum(Quinta$hora[Quinta$equipamento == "Grill"] == i) / sum(Quinta$hora == i)
  probMicrowave[i+1,5] <- sum(Quinta$hora[Quinta$equipamento == "Microwave"] == i) / sum(Quinta$hora == i)
  probOther[i+1,5] <- sum(Quinta$hora[Quinta$equipamento == "Other"] == i) / sum(Quinta$hora == i)
  probShower[i+1,5] <- sum(Quinta$hora[Quinta$equipamento == "Shower"] == i) / sum(Quinta$hora == i)
  probToaster[i+1,5] <- sum(Quinta$hora[Quinta$equipamento == "Toaster"] == i) / sum(Quinta$hora == i)
}

# SEXTA
for(i in 0:23){
  probFridge[i+1,6] <- sum(Sexta$hora[Sexta$equipamento == "Fridge"] == i) / sum(Sexta$hora == i)
  probSmallFridge[i+1,6] <- sum(Sexta$hora[Sexta$equipamento == "SmallFridge"] == i) / sum(Sexta$hora == i)
  probAirConditioning[i+1,6] <- sum(Sexta$hora[Sexta$equipamento == "AirConditioning"] == i) / sum(Sexta$hora == i)
  probDesktop_Computer[i+1,6] <- sum(Sexta$hora[Sexta$equipamento == "Desktop_Computer"] == i) / sum(Sexta$hora == i)
  probFan[i+1,6] <- sum(Sexta$hora[Sexta$equipamento == "Fan"] == i) / sum(Sexta$hora == i)
  probGrill[i+1,6] <- sum(Sexta$hora[Sexta$equipamento == "Grill"] == i) / sum(Sexta$hora == i)
  probMicrowave[i+1,6] <- sum(Sexta$hora[Sexta$equipamento == "Microwave"] == i) / sum(Sexta$hora == i)
  probOther[i+1,6] <- sum(Sexta$hora[Sexta$equipamento == "Other"] == i) / sum(Sexta$hora == i)
  probShower[i+1,6] <- sum(Sexta$hora[Sexta$equipamento == "Shower"] == i) / sum(Sexta$hora == i)
  probToaster[i+1,6] <- sum(Sexta$hora[Sexta$equipamento == "Toaster"] == i) / sum(Sexta$hora == i)
}

# SABADO
for(i in 0:23){
  probFridge[i+1,7] <- sum(Sabado$hora[Sabado$equipamento == "Fridge"] == i) / sum(Sabado$hora == i)
  probSmallFridge[i+1,7] <- sum(Sabado$hora[Sabado$equipamento == "SmallFridge"] == i) / sum(Sabado$hora == i)
  probAirConditioning[i+1,7] <- sum(Sabado$hora[Sabado$equipamento == "AirConditioning"] == i) / sum(Sabado$hora == i)
  probDesktop_Computer[i+1,7] <- sum(Sabado$hora[Sabado$equipamento == "Desktop_Computer"] == i) / sum(Sabado$hora == i)
  probFan[i+1,7] <- sum(Sabado$hora[Sabado$equipamento == "Fan"] == i) / sum(Sabado$hora == i)
  probGrill[i+1,7] <- sum(Sabado$hora[Sabado$equipamento == "Grill"] == i) / sum(Sabado$hora == i)
  probMicrowave[i+1,7] <- sum(Sabado$hora[Sabado$equipamento == "Microwave"] == i) / sum(Sabado$hora == i)
  probOther[i+1,7] <- sum(Sabado$hora[Sabado$equipamento == "Other"] == i) / sum(Sabado$hora == i)
  probShower[i+1,7] <- sum(Sabado$hora[Sabado$equipamento == "Shower"] == i) / sum(Sabado$hora == i)
  probToaster[i+1,7] <- sum(Sabado$hora[Sabado$equipamento == "Toaster"] == i) / sum(Sabado$hora == i)
}


# Escrevendo os resultados em arquivo csv
write.csv(probFridge, "probFridge.csv")
write.csv(probSmallFridge, "probSmallFridge.csv")
write.csv(probAirConditioning, "probAirConditioning.csv")
write.csv(probDesktop_Computer, "probDesktop_Computer.csv")
write.csv(probFan, "probFan.csv")
write.csv(probGrill, "probGrill.csv")
write.csv(probOther, "probOther.csv")
write.csv(probMicrowave, "probMicrowave.csv")
write.csv(probShower, "probShower.csv")
write.csv(probToaster, "probToaster.csv")


#------------------------- OBTER A QTD DE EQUIPAMENTOS TAL DIA E HORA -------------------------

dom <- c("Domingo","Domingo","Domingo","Domingo","Domingo","Domingo","Domingo","Domingo","Domingo","Domingo",
         "Domingo","Domingo","Domingo","Domingo","Domingo","Domingo","Domingo","Domingo","Domingo","Domingo",
         "Domingo","Domingo","Domingo","Domingo")
seg <- c("Segunda","Segunda","Segunda","Segunda","Segunda","Segunda","Segunda","Segunda","Segunda","Segunda",
         "Segunda","Segunda","Segunda","Segunda","Segunda","Segunda","Segunda","Segunda","Segunda","Segunda",
         "Segunda","Segunda","Segunda","Segunda")
ter <- c("Terca","Terca","Terca","Terca","Terca","Terca","Terca","Terca","Terca","Terca","Terca","Terca",
         "Terca","Terca","Terca","Terca","Terca","Terca","Terca","Terca","Terca","Terca","Terca","Terca")
qua <- c("Quarta","Quarta","Quarta","Quarta","Quarta","Quarta","Quarta","Quarta","Quarta","Quarta","Quarta",
         "Quarta","Quarta","Quarta","Quarta","Quarta","Quarta","Quarta","Quarta","Quarta","Quarta","Quarta",
         "Quarta","Quarta")
qui <- c("Quinta","Quinta","Quinta","Quinta","Quinta","Quinta","Quinta","Quinta","Quinta","Quinta","Quinta","Quinta",
         "Quinta","Quinta","Quinta","Quinta","Quinta","Quinta","Quinta","Quinta","Quinta","Quinta","Quinta","Quinta")
sext <- c("Sexta","Sexta","Sexta","Sexta","Sexta","Sexta","Sexta","Sexta","Sexta","Sexta","Sexta","Sexta",
          "Sexta","Sexta","Sexta","Sexta","Sexta","Sexta","Sexta","Sexta","Sexta","Sexta","Sexta","Sexta")
sab <- c("Sabado","Sabado","Sabado","Sabado","Sabado","Sabado","Sabado","Sabado","Sabado","Sabado","Sabado",
         "Sabado","Sabado","Sabado","Sabado","Sabado","Sabado","Sabado","Sabado","Sabado","Sabado","Sabado",
         "Sabado","Sabado")

x <- c()
DomingoQtd <- c()
for(i in 0:23){
  x <- cbind(Domingo$equipamento[Domingo$hora == i])
  DomingoQtd <- rbind(DomingoQtd, length(x[!duplicated(x), ]))
}

x <- c()
SegundaQtd <- c()
for(i in 0:23){
  x <- cbind(Segunda$equipamento[Segunda$hora == i])
  SegundaQtd <- rbind(SegundaQtd, length(x[!duplicated(x), ]))
}

x <- c()
TercaQtd <- c()
for(i in 0:23){
  x <- cbind(Terca$equipamento[Terca$hora == i])
  TercaQtd <- rbind(TercaQtd, length(x[!duplicated(x), ]))
}

x <- c()
QuartaQtd <- c()
for(i in 0:23){
  x <- cbind(Quarta$equipamento[Quarta$hora == i])
  QuartaQtd <- rbind(QuartaQtd, length(x[!duplicated(x), ]))
}

x <- c()
QuintaQtd <- c()
for(i in 0:23){
  x <- cbind(Quinta$equipamento[Quinta$hora == i])
  QuintaQtd <- rbind(QuintaQtd, length(x[!duplicated(x), ]))
}

x <- c()
SextaQtd <- c()
for(i in 0:23){
  x <- cbind(Sexta$equipamento[Sexta$hora == i])
  SextaQtd <- rbind(SextaQtd, length(x[!duplicated(x), ]))
}

x <- c()
sabadoQtd <- c()
for(i in 0:23){
  x <- cbind(Sabado$equipamento[Sabado$hora == i])
  sabadoQtd <- rbind(sabadoQtd, length(x[!duplicated(x), ]))
}

resultData <- data.frame(
  diaSemana = c(as.vector(dom), as.vector(seg), as.vector(ter), as.vector(qua), as.vector(qui), as.vector(sext), as.vector(sab)),
  hora = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
  qtdEquipON = c(DomingoQtd, SegundaQtd, TercaQtd, QuartaQtd, QuintaQtd, SextaQtd, sabadoQtd)
)

write.csv(resultData, "inMLP.csv", row.names = FALSE, quote = FALSE)
#-------------------------------------------------------------------------------------------------

# Correlacao
install.packages("moments")
require("moments")
install.packages("nortest")
require("nortest")

# Normalidade
shapiro.test(resultData$qtdEquipON) # p-value = 2.572e-05
shapiro.test(resultData$hora) # p-value = 1.29e-05

# rho = 0.528708, correlacao moderada.
cor.test(resultData$qtdEquipON, resultData$hora, method = "spearman")
# tau = 0.3526339, correlacao fraca.
cor.test(resultData$qtdEquipON, resultData$hora, method = "kendall")


# Formatando os dias da semana no fromato int
# Domingo = 1
# Segunda = 2
# ...
tmp <- c()
for(i in 1:7){
  for(j in 0:23){
    tmp <- rbind(tmp, i)
  }
}
result2 <- resultData
result2$diaSemanaNumeric <- tmp

# rho = -0.009386061, muito fraca.
cor.test(result2$qtdEquipON, result2$diaSemanaNumeric, method = "spearman")
# tau = -0.005031964, muito fraca.
cor.test(result2$qtdEquipON, result2$diaSemanaNumeric, method = "kendall")


plot(resultData$qtdEquipON, resultData$hora, xlab = "Qtd de equipamentos ON", ylab = "Hora")
plot(resultData$qtdEquipON, resultData$diaSemana, xlab = "Qtd de equipamentos ON", ylab = "Dia da Semana")



# Contruindo uma matriz para mostrar os equipamentos em determindados dias e horas
matrixDefault <- matrix(nrow=7,ncol=12)
colnames(matrixDefault) = c("diaSemana","hora","AirConditioning","Desktop_Computer","Fan","Fridge","Grill","Microwave",
                       "Other","Shower","SmallFridge","Toaster")
matrixDefault[1,1] = "Domingo"; matrixDefault[2,1] = "Segunda"; matrixDefault[3,1] = "Terca"; 
matrixDefault[4,1] = "Quarta"; matrixDefault[5,1] = "Quinta"; matrixDefault[6,1] = "Sexta"; 
matrixDefault[7,1] = "Sabado";

# Arquivos com scripts para construir as matrizes
source("matriz_hora00.R")
source("matriz_hora12.R")
source("matriz_hora13.R")
source("matriz_hora14.R")
source("matriz_hora15.R")
source("matriz_hora16.R")






