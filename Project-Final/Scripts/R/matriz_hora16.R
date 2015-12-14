horaQuatroTarde = matrixDefault

# Meia-noite
horaQuatroTarde[,2] = "16"

# AirConditioning Desktop_Computer Fan Fridge Grill Microwave Other Shower SmallFridge Toaster
x <- c()
x2 <- c()

# Domingo
x <- cbind(Domingo$equipamento[Domingo$hora == 16])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaQuatroTarde)-2)){
  horaQuatroTarde[1,(x2[i]+2)] = "x"
}

# Segunda
x <- cbind(Segunda$equipamento[Segunda$hora == 16])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaQuatroTarde)-2)){
  horaQuatroTarde[2,(x2[i]+2)] = "x"
}

# Terca
x <- cbind(Terca$equipamento[Terca$hora == 16])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaQuatroTarde)-2)){
  horaQuatroTarde[3,(x2[i]+2)] = "x"
}

# Quarta
x <- cbind(Quarta$equipamento[Quarta$hora == 16])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaQuatroTarde)-2)){
  horaQuatroTarde[4,(x2[i]+2)] = "x"
}

# Quinta
x <- cbind(Quinta$equipamento[Quinta$hora == 16])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaQuatroTarde)-2)){
  horaQuatroTarde[5,(x2[i]+2)] = "x"
}

# Sexta
x <- cbind(Sexta$equipamento[Sexta$hora == 16])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaQuatroTarde)-2)){
  horaQuatroTarde[6,(x2[i]+2)] = "x"
}

# Sabado
x <- cbind(Sabado$equipamento[Sabado$hora == 16])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaQuatroTarde)-2)){
  horaQuatroTarde[7,(x2[i]+2)] = "x"
}




