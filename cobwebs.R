two_B <- function(x) {
     (r * x)/(x + A)
}

straight <- function(x) {
    x
}

A <- 4.0
r <- 2.7

cobweb<-data.frame(matrix(nrow = 10, ncol = 2))
colnames(cobweb) <- c("x","y")
cobweb$x <- c(1:10)
cobweb[1,2] <- -.7


for (i in 2:10) {
    x <- cobweb[i-1,2]
    cobweb[i,2]<- two_B(x)
}


ggplot(data.frame(x = c(-1.5:.4)), aes(x=x)) + 
    stat_function(fun = two_B, color = "orange") +
    stat_function(fun = straight, color = "purple") +
    xlab(NULL) + ylab(NULL) + 
    ylim(-1.5,.4) +
    ggtitle("A = 4.0, r = 2.7") +
    theme(plot.title= element_text(hjust = 0.5)) +
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = r-A) +
    annotate(geom = "text",x = r-A+.15, y = (0), label = paste0("x = ",r-A)) +
    geom_point(data= cobweb, mapping = aes(x = y, y= y))
ggsave("cobweb_3.png")

