quest_three <- function(x) {
  (b/(b-a))^x 
}

b = 1
a = 1.5

ggplot(data.frame(x = c(0:10)), aes(x=x)) + 
  stat_function(fun = quest_three, geom = "point", color = "blue") +
  ggtitle("β = 1, α = 1.5") +
  theme(plot.title= element_text(hjust = "0.5"))