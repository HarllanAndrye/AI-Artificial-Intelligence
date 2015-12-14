horaDuasTarde = matrixDefault

# Meia-noite
horaDuasTarde[,2] = "14"

# AirConditioning Desktop_Computer Fan Fridge Grill Microwave Other Shower SmallFridge Toaster
x <- c()
x2 <- c()

# Domingo
x <- cbind(Domingo$equipamento[Domingo$hora == 14])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaDuasTarde)-2)){
  horaDuasTarde[1,(x2[i]+2)] = "x"
}

# Segunda
x <- cbind(Segunda$equipamento[Segunda$hora == 14])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaDuasTarde)-2)){
  horaDuasTarde[2,(x2[i]+2)] = "x"
}

# Terca
x <- cbind(Terca$equipamento[Terca$hora == 14])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaDuasTarde)-2)){
  horaDuasTarde[3,(x2[i]+2)] = "x"
}

# Quarta
x <- cbind(Quarta$equipamento[Quarta$hora == 14])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaDuasTarde)-2)){
  horaDuasTarde[4,(x2[i]+2)] = "x"
}

# Quinta
x <- cbind(Quinta$equipamento[Quinta$hora == 14])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaDuasTarde)-2)){
  horaDuasTarde[5,(x2[i]+2)] = "x"
}

# Sexta
x <- cbind(Sexta$equipamento[Sexta$hora == 14])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaDuasTarde)-2)){
  horaDuasTarde[6,(x2[i]+2)] = "x"
}

# Sabado
x <- cbind(Sabado$equipamento[Sabado$hora == 14])
x2 <- x[!duplicated(x), ]
for(i in 1:(ncol(horaDuasTarde)-2)){
  horaDuasTarde[7,(x2[i]+2)] = "x"
}




