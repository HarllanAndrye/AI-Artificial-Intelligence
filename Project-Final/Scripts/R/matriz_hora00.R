horaZero = matrixDefault

# Meia-noite
horaZero[,2] = "00"

# AirConditioning Desktop_Computer Fan Fridge Grill Microwave Other Shower SmallFridge Toaster
x <- c()
x2 <- c()

# Domingo
x <- cbind(Domingo$equipamento[Domingo$hora == 0])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaZero)-2)){
  horaZero[1,(x2[i]+2)] = "x"
}

# Segunda
x <- cbind(Segunda$equipamento[Segunda$hora == 0])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaZero)-2)){
  horaZero[2,(x2[i]+2)] = "x"
}

# Terca
x <- cbind(Terca$equipamento[Terca$hora == 0])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaZero)-2)){
  horaZero[3,(x2[i]+2)] = "x"
}

# Quarta
x <- cbind(Quarta$equipamento[Quarta$hora == 0])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaZero)-2)){
  horaZero[4,(x2[i]+2)] = "x"
}

# Quinta
x <- cbind(Quinta$equipamento[Quinta$hora == 0])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaZero)-2)){
  horaZero[5,(x2[i]+2)] = "x"
}

# Sexta
x <- cbind(Sexta$equipamento[Sexta$hora == 0])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaZero)-2)){
  horaZero[6,(x2[i]+2)] = "x"
}

# Sabado
x <- cbind(Sabado$equipamento[Sabado$hora == 0])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaZero)-2)){
  horaZero[7,(x2[i]+2)] = "x"
}




