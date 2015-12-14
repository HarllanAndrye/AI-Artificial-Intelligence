horaTresTarde = matrixDefault

# Meia-noite
horaTresTarde[,2] = "15"

# AirConditioning Desktop_Computer Fan Fridge Grill Microwave Other Shower SmallFridge Toaster
x <- c()
x2 <- c()

# Domingo
x <- cbind(Domingo$equipamento[Domingo$hora == 15])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaTresTarde)-2)){
  horaTresTarde[1,(x2[i]+2)] = "x"
}

# Segunda
x <- cbind(Segunda$equipamento[Segunda$hora == 15])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaTresTarde)-2)){
  horaTresTarde[2,(x2[i]+2)] = "x"
}

# Terca
x <- cbind(Terca$equipamento[Terca$hora == 15])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaTresTarde)-2)){
  horaTresTarde[3,(x2[i]+2)] = "x"
}

# Quarta
x <- cbind(Quarta$equipamento[Quarta$hora == 15])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaTresTarde)-2)){
  horaTresTarde[4,(x2[i]+2)] = "x"
}

# Quinta
x <- cbind(Quinta$equipamento[Quinta$hora == 15])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaTresTarde)-2)){
  horaTresTarde[5,(x2[i]+2)] = "x"
}

# Sexta
x <- cbind(Sexta$equipamento[Sexta$hora == 15])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaTresTarde)-2)){
  horaTresTarde[6,(x2[i]+2)] = "x"
}

# Sabado
x <- cbind(Sabado$equipamento[Sabado$hora == 15])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaTresTarde)-2)){
  horaTresTarde[7,(x2[i]+2)] = "x"
}




