horaMeioDia = matrixDefault

# Meia-noite
horaMeioDia[,2] = "12"

# AirConditioning Desktop_Computer Fan Fridge Grill Microwave Other Shower SmallFridge Toaster
x <- c()
x2 <- c()

# Domingo
x <- cbind(Domingo$equipamento[Domingo$hora == 12])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaMeioDia)-2)){
  horaMeioDia[1,(x2[i]+2)] = "x"
}

# Segunda
x <- cbind(Segunda$equipamento[Segunda$hora == 12])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaMeioDia)-2)){
  horaMeioDia[2,(x2[i]+2)] = "x"
}

# Terca
x <- cbind(Terca$equipamento[Terca$hora == 12])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaMeioDia)-2)){
  horaMeioDia[3,(x2[i]+2)] = "x"
}

# Quarta
x <- cbind(Quarta$equipamento[Quarta$hora == 12])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaMeioDia)-2)){
  horaMeioDia[4,(x2[i]+2)] = "x"
}

# Quinta
x <- cbind(Quinta$equipamento[Quinta$hora == 12])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaMeioDia)-2)){
  horaMeioDia[5,(x2[i]+2)] = "x"
}

# Sexta
x <- cbind(Sexta$equipamento[Sexta$hora == 12])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaMeioDia)-2)){
  horaMeioDia[6,(x2[i]+2)] = "x"
}

# Sabado
x <- cbind(Sabado$equipamento[Sabado$hora == 12])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaMeioDia)-2)){
  horaMeioDia[7,(x2[i]+2)] = "x"
}




