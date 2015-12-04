# http://radio.feld.cvut.cz/matlab/toolbox/fuzzy/fuzzyt22.html
# http://www.mathworks.com/help/fuzzy/evalfis.html

# http://ima.ac.uk/home/garibaldi/
# http://www.cs.nott.ac.uk/~pszjmg/fuzzy-v0_8.r
# http://www.cs.nott.ac.uk/~pszjmg/fuzzy-v1_7.r

source("fuzzy-v0_8.R") # Type-1 fuzzy
#source("fuzzy-v1_7.R") # Type-2 fuzzy

# fis (fuzzy inference system)
fis= newfis('resort')

fis= addvar(fis, 'input', 'temperature', c(0,50))
fis= addvar(fis, 'input', 'sunLight', c(0,110))
fis= addvar(fis, 'output', 'tourists', c(0,100))

fis= addmf(fis, 'input', 1, 'cold', 'trapmf', c(0,0,17,20))
fis= addmf(fis, 'input', 1, 'warm', 'trapmf', c(17,20,26,29))
fis= addmf(fis, 'input', 1, 'hot', 'trapmf', c(26,29,50,50))

fis= addmf(fis, 'input', 2, 'cloudy', 'trapmf', c(0,0,30,50))
#fis= addmf(fis, 'input', 2, 'partlySunny', 'trapmf', c(30,50,50,100))
fis= addmf(fis, 'input', 2, 'partlySunny', 'trimf', c(30,50,100))
fis= addmf(fis, 'input', 2, 'sunny', 'trapmf', c(50,100,110,110))

fis= addmf(fis, 'output', 1, 'low', 'trimf', c(0,0,50))
fis= addmf(fis, 'output', 1, 'middle', 'trimf', c(0,50,100))
fis= addmf(fis, 'output', 1, 'high', 'trimf', c(50,100,100))

# (temperature,sunLight,tourists,1,[and/or])
# temperature: 1=cold, 2=warm and 3=hot     (0=nothing)
# sunLight: 1=cloudy, 2=warm and 3=sunny  (0=nothing)
# tourists: 1=low, 2=middle and 3=high
# [and/or]: 1=and, 2=or
rules= rbind(c(3,3,3,1,2), c(2,2,2,1,1), c(1,1,1,1,2))

fis= addrule(fis, rules)

#showfis(fis)
showrule(fis)

# evalfis(c(temperature,sunLight), fis)
evalfis(c(19,60), fis)

# Return multiple outputs
#    evalfis(rbind(c(temperature,sunLight), c(temperature,sunLight)), fis)
evalfis(rbind(c(19,60), c(25,50)), fis)

# Chart 3D
gensurf(fis)

# Chart
plotmf(fis, 'input', 1) # Temperature
plotmf(fis, 'input', 2) # Sun Light
plotmf(fis, 'output', 1) # Tourists



#------------------- Other Script ----------------------
# http://stackoverflow.com/questions/15197613/fuzzy-logic-function-in-r-as-in-matlab
install.packages("sets")
library("sets")

# set universe
sets_options("universe", seq(from = 0, to = 100, by = 0.1))

# set up fuzzy variables
variables <-
  set(temperature = fuzzy_variable(cold = fuzzy_trapezoid(corners = c(-2, 0, 17, 20)),
                            warm = fuzzy_trapezoid(corners = c(17, 20, 26, 29)),
                            hot = fuzzy_trapezoid(corners = c(26, 29, 100, 101))),
      sunLight = fuzzy_variable(cloudy = fuzzy_trapezoid(corners = c(-2, 0, 30, 50)),
                            partlySunny = fuzzy_trapezoid(corners = c(30, 50, 50, 100)),
                            sunny = fuzzy_trapezoid(corners = c(50, 100, 110, 120))),
      tourists = fuzzy_partition(varnames = c(low = 0, middle = 50, high = 100),
                            FUN = fuzzy_cone, radius = 50)
  )

# set up rules
rules <- set(
    fuzzy_rule(temperature %is% hot || sunLight %is% sunny, tourists %is% high),
    fuzzy_rule(temperature %is% warm && sunLight %is% partlySunny, tourists %is% middle),
    fuzzy_rule(temperature %is% cold || sunLight %is% cloudy, tourists %is% low)
  )

# combine to a system
system <- fuzzy_system(variables, rules)
print(system)
plot(system) # plots variables

# do inference
fi <- fuzzy_inference(system, list(temperature = 19, sunLight = 60))

# plot resulting fuzzy set
plot(fi)
abline(v=48, col=4)

# defuzzify
gset_defuzzify(fi, "centroid")

# reset universe
sets_options("universe", NULL)


