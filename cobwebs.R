two_B <- function(x) {
     (r * x)/(x + A)
}

straight <- function(x) {
    x
}

A <- 4.0
r <- 2.7

ggplot(data.frame(x = c(-2:1)), aes(x=x)) + 
    stat_function(fun = two_B, color = "orange") +
    stat_function(fun = straight, color = "purple") +
    xlab(NULL) + ylab(NULL) + 
    ylim(-2,1) +
    ggtitle("A = 4.0, r = 2.7") +
    theme(plot.title= element_text(hjust = 0.5)) +
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = r-A) +
    annotate(geom = "text",x = r-A+.15, y = (0), label = paste0("x = ",r-A))
ggsave("cobweb_3.png")


