

#install.packages("dplyr")

#install.packages("pracma")
library(pracma)
library(dplyr)

gen_infor <- function(modelo_EGD_0, alfa, delta, gamma, ano_calib, por_calib) {
  sis_eq <- function(p) {
    s_theta_S <- p[1]
    s_ns <- p[2]
    aux1 <- s_theta_F * (s_stock_k^alfa) * (s_nf^(1 - alfa))
    aux2 <- gamma / ((1 - s_carga) * (1 - alfa) * s_theta_F)
    aux3 <- (((1 + s_g_cp) / beta - 1 + delta) / (alfa * (1 - s_carga) * s_theta_F))^(alfa / (1 - alfa))
    
    return(c((s_theta_S * (s_ns^gamma) / aux1) - por_calib, 
             (s_theta_S * aux2 * aux3)^(1 / (1 - gamma)) - s_ns))
  }
  
  modelo_EGD <- modelo_EGD_0
  
  modelo_EGD <- rbind(modelo_EGD, data.frame(fecha = modelo_EGD$fecha[which.max(modelo_EGD$fecha)] + 1, stock_k=0, fbc=0, cp=0, pib_real = 0, rec_total=0, pib_nom=0, nf=0))
  
  print(class(modelo_EGD))
  head(modelo_EGD)
  tail(modelo_EGD)
  
  modelo_EGD$carga <- modelo_EGD$rec_total / modelo_EGD$pib_nom
  modelo_EGD$g_cp <- modelo_EGD$cp / lag(modelo_EGD$cp)
  modelo_EGD$r_pib_k <- modelo_EGD$pib_real / modelo_EGD$stock_k
  modelo_EGD$abeta <- (lead(modelo_EGD$cp, 1)/modelo_EGD$cp)/((1-modelo_EGD$carga)*alfa*lead(modelo_EGD$r_pib_k,1)+1-delta)
  
  beta <- mean(modelo_EGD$abeta, na.rm =T)
  cat("beta: ", beta, "\n")
  
  modelo_EGD$theta_F <- modelo_EGD$pib_real / (modelo_EGD$stock_k^alfa * modelo_EGD$nf^(1 - alfa))
  
  modelo_EGD$crec_theta_S <- ((modelo_EGD$stock_k / lag(modelo_EGD$stock_k,1)) + modelo_EGD$theta_F / lag(modelo_EGD$theta_F,1)) / 2
  
  valores_orig <- modelo_EGD
  
  # Obtaining values to calibrate the pivot year
  s_theta_F<-valores_orig[which(valores_orig$fecha == ano_calib),'theta_F']
  s_carga<-valores_orig[which(valores_orig$fecha == ano_calib),'carga']
  s_stock_k<-valores_orig[which(valores_orig$fecha == ano_calib),'stock_k']
  s_nf<-valores_orig[which(valores_orig$fecha == ano_calib),'nf']
  s_g_cp<-valores_orig[which(valores_orig$fecha == ano_calib),'g_cp']


  result <- fsolve(sis_eq, c(s_theta_S = 1000, s_ns = 4000))
  s_theta_S <- as.numeric(result$x[1])
  
  s_ns <- as.numeric(result$x[2])

  cat("s_theta_S:", s_theta_S, ", s_ns:", s_ns, "\n")
  
  
  #valores_orig$theta_S <- ifelse(valores_orig$fecha == ano_calib, s_theta_S, NA)
  valores_orig$theta_S[valores_orig$fecha == ano_calib] <- s_theta_S
  
  
  # Calculating values for the remaining years
  for (f in seq(which(valores_orig$fecha == ano_calib) -1, 1, -1)) {
    valores_orig$theta_S[f] <- valores_orig$theta_S[f + 1] / valores_orig$crec_theta_S[f + 1]
  }
  
  for (f in seq(which(valores_orig$fecha == ano_calib)+1, nrow(valores_orig))) {
    valores_orig$theta_S[f] <- valores_orig$theta_S[f - 1] * valores_orig$crec_theta_S[f]
  }
  
  valores_orig$ns <- (((gamma * valores_orig$theta_S) / ((1 - valores_orig$carga) * (1 - alfa) * valores_orig$theta_F)) * 
                        (((1 + valores_orig$g_cp) / beta - 1 + delta) / (alfa * (1 - valores_orig$carga) * valores_orig$theta_F))^(alfa / (1 - alfa)))^(1 / (1 - gamma))
  
  valores_orig$informalidad <- (valores_orig$theta_S * valores_orig$ns^gamma) / 
    (valores_orig$theta_F * valores_orig$stock_k^alfa * valores_orig$nf^(1 - alfa))
  
  valores_orig$informalidad <- valores_orig$informalidad * 100
  
  return(valores_orig)
}


#modelo_EGD_0 <- read.csv("/cloud/project/modelo_EGD.csv")
modelo_EGD_0<-data.frame(fecha=c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
                stock_k=c(276879.1749, 292620.6007, 309938.489, 331080.1948, 347218.0617, 356338.734, 378854.5901, 404264.3442, 427223.7848, 448283.7133, 468350.0983, 486461.2072, 502352.337, 520029.398, 537999.7662, 551170.8707, 569773.4586, 588468.0006),
                fbc=c(25087.57342, 26706.35254, 29529.69171, 35044.41927, 31169.34979, 34631.23846, 39848.76184, 44634.59829, 45224.48862, 43391.1457, 43437.55851, 42412.43929, 41002.10428, 43664.34414, 45622.92286, 40680.27009, 47071.80168, 48395.23288),
                cp=c(63223.95901, 67782.56381, 72182.15402, 74488.11139, 74113.83112, 82553.93694, 90159.87865, 95898.93976, 101027.5711, 103419.58, 105894.3045, 109391.3679, 113277.9367, 117562.1591, 118424.8785, 109646.7513, 132502.3673, 136286.872),
                pib_real=c(120347.7565, 127628.7847, 134224.9351, 139311.2451, 137753.6935, 145814.559, 154889.9066, 164423.907, 169863.8855, 172908.9496, 176629.8508, 179726.2405, 182166.3753, 189434.8674, 190842.6148, 179114.865, 200138.3488, 205022.5319),
                rec_total=c(11184791, 13220515, 16165759, 16473295, 13346556, 17577714, 21101202, 22770029.71, 22953042.73, 24485055.79, 27677815.89, 28998166.9, 30754067.04, 34304059.01, 34579222.39, 32302484.25, 45312420.91, 55407519.53),
                pib_nom=c(68467.93984, 81577.53348, 90159.47921, 93867.1213, 96138.47728, 110777.8669, 121509.2985, 129973.394, 137309.192, 147951.29, 158622.9029, 168764.6879, 179314.9101, 189434.8674, 195752.2285, 201428.8942, 240371.4731, 262593.3558),
                nf=c(4733.085646, 4829.090556, 4996.336469, 5269.109053, 5279.647354, 5379.715399, 5651.639946, 5887.924604, 5983.332034, 6041.485278, 6151.613547, 6170.244961, 6166.735746, 6288.415917, 6423.707525, 5844.661404, 6050.294477, 6451.122202))

modelo_EGD_0$rec_total=modelo_EGD_0$rec_total/1000


delta=0.06 #Tasa de depreciación

#ano_calib=2018
#por_calib=0.187179431915283 #en esta version se recoge la ultima estimacion disponible para Chile de la Informalidad MIMIC del Banco Mundial (https://www.worldbank.org/en/research/brief/informal-economy-database)

ano_calib=2020
por_calib=0.194429550170898

alfa=0.36
gamma=0.24

bd_inform <- gen_infor(modelo_EGD_0, alfa, delta, gamma, ano_calib, por_calib)

bd_inform$informalidad
