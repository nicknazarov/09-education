
# 1/0/1   3/0/3   6/0/6   9/0/9 12/0/12   24/0/24  48/0/48  2 года/0/2 года   3 года/0/3 года этих стратегий на средн дох, t стат p value  10% процентиль
# 
# Проверяем наличие реверсивного эффекта 

rm(list=ls())
library(XLConnect)
library(pander)
library(DT)
panderOptions('table.split.table', Inf) 
library(ggplot2)
library(scales)
source("R/reality_func2.R")


#############################################################################
# Параметры, которые зависят от изучаемой страны
#country_name <- "Индия"
#country_name_eng <- "India"

#country_name <- "Россия"
#country_name_eng <- "Russia"

#country_name <- "Бразилия"
#country_name_eng <- "brazil"

#country_name <- "Бразилия"
#country_name_eng <- "china"

#country_name <- "Бразилия"
country_name_eng <- "malaysia"

#temp <-  ret(44, 0, 44, STEP, N, price_d5, UP1, UP2, 0.1) 
 

#india
#T <- 166 

#russia
#T <- 313

#brazil
#T <- 147

#china
#T <- 288

#malaysia
T <- 305



#############################################################################
# Загрузка 
setwd("/home/nazarov/02-fmlab.hse.ru/05 - reverse/")
#price_d5<- readWorksheet(loadWorkbook(paste("data","/stocks_china.xlsx",sep="")),sheet=1)
price_d5<- read.csv("data/stocks_malaysia.csv")
row.names(price_d5) <- price_d5[,1]
price_d5 <-price_d5[,-1]


#############################################################################
# Константы
# перебор от 1 до 12 мес , ждем от 0 до 8 недель, держим от 1 до 12 мес
# таким образом  последние 25 месяцев понадобятся только для одной модели
# Шаг для подсчета разниц в группах победителей и проигравших
STEP=1
# Периоды отбора (в месяцах), удержания (в неделях), инвестирования (в месяцах)
UP1=48
UP2=0
UP3=48
# N - с учетом отступа
N <- (nrow(price_d5)-(2+UP3*4))%/%STEP 
#n <- T-R+1
#############################################################################
# Процедура P&R stationary bootstrap
# Фиксация рандома
set.seed(42)
# Константы для reality check
R <- 1
N_rc <- 500
Q <- 0.1  
P_R_ind <- data.frame(P_R(R,T,Q))
for (i in 2:N_rc){
  P_R_ind[,i]  <-  P_R(R,T,Q)
}
#############################################################################
# Инициализация
V_star <- c()
f_bar  <- c() 
V_star_sharp <- c()
f_sharp  <- c() 
#list_of_restable <- list() # для сохранения результатов по каждому дню
#length(periods)
#periods[[2]][1]

periods <- list (c(1,0,1),c(3,0,3),c(6,0,6),c(9,0,9),c(12,0,12),c(24,0,24),c(36,0,36),c(48,0,48))
#############################################################################
# Процедура формирования портфелей, подсчета статистик и bootstrap
start_time <- Sys.time()

resultDataFull <- price_d5
m <- 1
realityCheckData <- data.frame(1,1,1,1,1,1,1,1,1,1)
colnames(realityCheckData) <-c("mean","t","p-value","hist_per","moment_per","invest_per","percent","winners","losers", "Amount_of_negative")

for (percent in c(0.1) ){ 
    for (t in 1:length(periods)){    
     
        p1<- periods[[t]][1]
        p2<- periods[[t]][2]
        p3<- periods[[t]][3]
          #вектор дельт    
          temp <- ret(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
          return.winner<- ret.winner(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
          return.loser<- ret.loser(p1, p2, p3, STEP, N, resultDataFull, UP1, UP2, percent) 
          n <- length(temp)
          #realityCheckData[m, ] <- list(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, percent,
          #                              mean(return.winner), mean(return.loser))    
          #cat(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, n, percent, mean(return.winner), mean(return.loser), "\n")
          
          realityCheckData[m, ] <- list(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, percent,
                                        mean(return.winner), mean(return.loser),length(temp[temp<0]))    
          cat(mean(temp),abs(mean(temp))/sd(temp)*sqrt(n), (1-pt(q = abs(mean(temp))/sd(temp)*sqrt(n),df = n-1))*2 ,p1*4, p2, p3*4, n, percent, mean(return.winner), mean(return.loser),length(temp[temp<0]), "\n")
          
          
          #############################################################REALITY_CHECK##########################################################################      
          temp <- temp-0.06/12
          f_bar <- mean(temp)
          
          if(m==1){
            V_bar <- sqrt(n)*mean(temp)
            for (k in 1:N_rc){
              V_star[k] <- sqrt(n)*(mean(temp[P_R_ind[,k]]) - f_bar)
            }         
          }
          else {
            V_bar <- max(sqrt(n)*f_bar,V_bar)         
            for (k in 1:N_rc){
              t <- sqrt(n)*(mean(temp[P_R_ind[,k]]) - f_bar)
              V_star[k] <-max(t,V_star[k]) 
            }
          }
          
          retBoot <- c(V_star,V_bar)
          realityCheckData[m, "pBoot"] <- 1-(rank(retBoot, ties.method = "first")[501]-1)/500
          #  list(V_bar,V_star)            
          ####################################################################################################################################################  
          #############################################################REALITY_CHECK - SHARP##################################################################      
          f_sharp <- mean(temp)/sd(temp)
          
          if(m==1){
            V_bar_sharp <- sqrt(n)*f_sharp
            for (k in 1:N_rc){
              #     V_star[k] <- sqrt(n)*(mean(temp[P_R_ind[,k]]) - f_bar)
              V_star_sharp[k] <- sqrt(n)*(mean(temp[P_R_ind[,k]])/sd(temp[P_R_ind[,k]]) - f_sharp)
            }         
          }
          else {
            V_bar_sharp <- max(sqrt(n)* f_sharp ,V_bar_sharp)         
            for (k in 1:N_rc){
              #      t <- sqrt(n)*(mean(temp[P_R_ind[,k]]) - f_bar)
              t <- sqrt(n)*(mean(temp[P_R_ind[,k]])/sd(temp[P_R_ind[,k]]) - f_sharp)
              V_star_sharp[k] <-max(t,V_star_sharp[k]) 
            }
          }
          
          retBoot.sharp <- c(V_star_sharp,V_bar_sharp)
          realityCheckData[m, "pBoot-Sharp"] <- 1-(rank(retBoot.sharp, ties.method = "first")[501]-1)/500
          #  list(V_bar,V_star)            
          ####################################################################################################################################################  
          
          m <- m+1      
    }
}



  
#Сохранение результатов
short_res <- list(data=realityCheckData, num=N, V_bar=V_bar,V_star=V_star, V_bar_sharp=V_bar_sharp,V_star_sharp=V_star_sharp)  # список ценных объектов
saveRDS(file = paste("results/",country_name_eng,"_short_res",".RDS",sep=""),short_res) # сохраняем всё ценное в файл


#mylist <- readRDS("tz_india_v1.RDS") # читаем из файла что там есть 

#print(sprintf("%s Start", Sys.time()))
end_time <- Sys.time()



