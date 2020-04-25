library(readr)

initial <- c(9000,900, 300)

q=1/4
b=4/5
c=1/100

SIR_data <- data.frame(matrix(nrow = 101, ncol = 4))
colnames(SIR_data) <- c("n","S","I","R")

SIR_data$n <- 0:100
SIR_data[1, 2:4] <- initial


for (x in 2:101) {
    s <- SIR_data[x-1,2]
    i <- SIR_data[x-1,3]
    r <- SIR_data[x-1,4]
    SIR_data[x,2] <- q * s + c * r
    SIR_data[x,3] <- (1-q) * s + b * i
    SIR_data[x,4] <- (1-c) * r + (1 - b) * i
}

SIR_data$`S/I` <- as.numeric(unlist(SIR_data$S))/as.numeric(unlist(SIR_data$I))

write_csv(SIR_data, "SIR_data.csv")

ggplot(data = SIR_data)+
  geom_point(mapping = aes(x = n, y = S), color = "darkolivegreen3")+
  geom_line(mapping = aes(x = n, y = S), color = "darkolivegreen3")+
  annotate(geom = "text", x = 50, y = -150, label = "Susceptibles", size = 4) +
  geom_point(mapping = aes(x = n, y = I), color = "darkorange")+
  geom_line(mapping = aes(x = n, y = I), color = "darkorange")+
  annotate(geom = "text", x = 50, y = 800, label = "Infected", size = 4) +
  geom_point(mapping = aes(x = n, y = R), color = "cornflowerblue")+
  geom_line(mapping = aes(x = n, y = R), color = "cornflowerblue")+
  annotate(geom = "text", x = 50, y = 9300, label = "Recovered", size = 4) + 
  ylab("Population") + xlab("Weeks")

