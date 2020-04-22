packages <- c("data.table","broom","gridExtra",
              "here","VIM","haven","skimr","beepr",
              "GGally","ranger","tmle","xtable","caret")

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package,repos='http://lib.stat.cmu.edu/R/CRAN') 
  }
}

for (package in packages) {
  library(package, character.only=T)
}

devtools::install_github("hadley/tidyverse")
devtools::install_github("hadley/ggplot2")
devtools::install_github("ropensci/plotly")

for (package in c("tidyverse","ggplot2","plotly")) {
  library(package, character.only=T)
}

thm <- theme_classic() +
  theme(
    legend.position = "top",
    #legend.title=element_blank(),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)


e <- read_csv("~/Dropbox/eager_base_imputed.csv") %>% filter(GA<90,GA>30,outcome=="live birth")

e

e %>% filter(GA>=32,GA<=37) %>% count()

e %>% filter(GA>=32,GA<=37) %>% summarize(mGA=mean(GA), mGA_se=sd(GA)/sqrt(n()))

plot1 <- ggplot(e) + 
  geom_rect(aes(xmin=32, xmax=37, ymin=0, ymax=Inf),fill="lightgrey",alpha=.2) +
  geom_histogram(aes(x=GA)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(expand=c(0,0)) +
  theme(panel.grid.major.x = element_line(colour = "lightgrey",size=.5), 
        axis.title.y=element_text(color="white"),
        axis.text.y=element_text(color="white"),
        axis.title.x=element_text(size=15),
        axis.text.x=element_text(size=15)) +
  xlab("Gest. Age")

pdf("figures/scatter1.pdf",width=5,height=4)
  plot1
dev.off()

hist(e$BMI)

e %>% filter(GA>=32,GA<=37,BMI>=18.5,BMI<=25) %>% count()

e %>% filter(GA>=32,GA<=37,BMI>=18.5,BMI<=25) %>% summarize(mGA=mean(GA), mGA_se=sd(GA)/sqrt(n()))

plot2 <- ggplot(e) + 
  geom_rect(aes(xmin=32, xmax=37, ymin=18.5, ymax=25),fill="lightgrey",alpha=.2) +
  geom_point(aes(y=BMI,x=GA)) + 
  theme(panel.grid.major = element_line(colour = "lightgrey",size=.5), 
        axis.title.y=element_text(size=15),
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=15),
        axis.text.x=element_text(size=15)) +
  ylab("BMI") + xlab("Gest. Age")

pdf("figures/scatter2.pdf",width=5,height=4)
  plot2
dev.off()

hist(e$age)

e %>% filter(GA>=32,GA<=37,BMI>=18.5,BMI<=25,age>=30,age<=35) %>% count()

e %>% filter(GA>=32,GA<=37,BMI>=18.5,BMI<=25,age>=30,age<=35) %>% summarize(mGA=mean(GA), mGA_se=sd(GA)/sqrt(n()))

trace1 <- list(
  mode = "markers",
  type = "scatter3d", 
  x = e$BMI, 
  y = e$GA, 
  z = e$age, 
  size = 1,
  colors = "black"
)

trace2 <- list(
            type = "mesh3d", 
            x = c(18.5, 18.5, 25, 25, 18.5, 18.5, 25, 25),
            y = c(32, 37, 37, 32, 32, 37, 37, 32),
            z = c(30, 30, 30, 30, 35, 35, 35, 35),
            i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
            j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
            k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6),
                    color = "lightgray",
                    colors = "lightgray",
                    opacity = 0.1,
                    alphahull = 5,
                    showscale = F
              )

p <- plot_ly()
p <- add_trace(p,
               type=trace2$type,
               x=trace2$x,
               y=trace2$y,
               z=trace2$z,
               i=trace2$i,
               j=trace2$j,
               k=trace2$k,
               color="#808080",
               colors="#808080",
               opacity=trace2$opacity,
               alphanull=trace2$alphanull)
p <- add_trace(p,
               mode=trace1$mode,
               size=trace1$size,
               color="black",
               colors="black",
               surfacecolor="black",
               type=trace1$type,
               x=trace1$x,
               y=trace1$y,
               z=trace1$z,
               opacity=1,
               showlegend=F) %>% 
  layout(
    font = list(size=15),
    scene = list(
      xaxis = list(title = "BMI"),
      yaxis = list(title = "Gest. Age"),
      zaxis = list(title = "Age")
    ))

plot_ly(data = e, x = ~BMI, y = ~GA, z=~age, size=~1, color="black", colors="black") %>% 
  add_trace(p,name=trace$name,type=trace$type, x=trace$x, y=trace$y, z=trace$z, marker=trace$marker) %>% 
  layout(
    font = list(size=15),
    scene = list(
      xaxis = list(title = "BMI"),
      yaxis = list(title = "Gest. Age"),
      zaxis = list(title = "Age")
    ))

##' MSE plot

d <- seq(10,20,1)
del <- .1
n <- del^(-d/4)

n_ratio <- n/n[1] 

d_ratio <- d/d[1] 

cbind(n,d,n_ratio,d_ratio)

plot(d_ratio,n_ratio)

print(xtable(cbind(d_ratio,n_ratio)),include.rownames=F)


##' kNN

kNN1pred <- function(x){
  fit <- knnreg(cbind(e$GA), e$GA, k = x)
  pGA <- predict(fit, cbind(e$GA))
}

fit <- knnreg(cbind(e$GA), e$GA, k = 50)

k <- c(5,20,100)

res<-do.call(cbind,lapply(k,function(x) kNN1pred(x)))

res <- data.frame(res)
names(res) <- c("kNN1","kNN2","kNN3")#,"kNN5","kNN10","kNN20")

f <- cbind(subset(e,select=c(age,BMI,GA)),res)


ggplot(f) +
  geom_point(aes(x=GA,y=GA),size=1,color="darkgrey") +
  geom_line(aes(x=GA,y=kNN1),color="red") +
  geom_line(aes(x=GA,y=kNN2),color="blue") +
  geom_line(aes(x=GA,y=kNN3),color="green") # +
  # geom_line(aes(x=GA,y=kNN5),color="cyan") +
  # geom_line(aes(x=GA,y=kNN10),color="magenta") +
  # geom_line(aes(x=GA,y=kNN20),color="orange")


kNN2pred <- function(x){
  fit <- knnreg(cbind(e$BMI), e$GA, k = x)
  pGA <- predict(fit, cbind(e$BMI))
}

k <- c(5,20,100)

res<-do.call(cbind,lapply(k,function(x) kNN2pred(x)))

res <- data.frame(res)
names(res) <- c("kNN1","kNN2","kNN3")#,"kNN5","kNN10","kNN20")

f <- cbind(subset(e,select=c(age,BMI,GA)),res)

ggplot(f) +
  geom_point(aes(x=BMI,y=GA),size=1,color="darkgrey") +
  geom_line(aes(x=BMI,y=kNN1),color="red") +
  geom_line(aes(x=BMI,y=kNN2),color="blue") +
  geom_line(aes(x=BMI,y=kNN3),color="green") #+
  # geom_line(aes(x=BMI,y=kNN5),color="cyan") +
  # geom_line(aes(x=BMI,y=kNN10),color="magenta") +
  # geom_line(aes(x=BMI,y=kNN20),color="orange")

  kNN3pred <- function(x){
    fit <- knnreg(cbind(e$age,e$BMI), e$GA, k = x)
    pGA <- predict(fit, data.frame(age=seq(min(e$age),max(e$age),length.out=20),
                                   BMI=seq(min(e$BMI),max(e$BMI),length.out=20)))
  }
  
  k <- c(5,20,100)
  
  res<-do.call(cbind,lapply(k,function(x) kNN3pred(x)))
  
  res <- data.frame(res)
  names(res) <- c("kNN1","kNN2","kNN3")#,"kNN5","kNN10","kNN20")
  
  f <- cbind(data.frame(age=seq(min(e$age),max(e$age),length.out=20),
                        BMI=seq(min(e$BMI),max(e$BMI),length.out=20)),res)
  
  plot_ly(f, x = ~BMI, y = ~kNN3, z=~age, type = 'scatter3d', mode = 'lines') %>% 
    add_trace(f, x = ~BMI, y = ~kNN2, z=~age) %>% 
    add_trace(f, x = ~BMI, y = ~kNN1, z=~age) 
  
  
  ggplot(f) +
    geom_point(aes(x=BMI,y=GA),size=.5,color="grey") +
    geom_line(aes(x=BMI,y=kNN1),color="red") +
    geom_line(aes(x=BMI,y=kNN2),color="blue") +
    geom_line(aes(x=BMI,y=kNN3),color="green")

##' predicting gestational age

mod1 <- lm(GA ~ BMI + age, data = subset(e,GA>=32&GA<=37))
boot_func <- function(i){
  set.seed(i)
  index <- sample(1:nrow(e),nrow(e),replace=T)
  bootDat <- e[index,]
  mod <- lm(GA ~ BMI + age, data = subset(bootDat,GA>=35&GA<=37))
  mu_pred <- mean(predict(mod,newdata=subset(bootDat,BMI>=18.5&BMI<=25&age>=30&age<=35),type="response"))
  mu_pred
}
res <- lapply(1:200,function(x) boot_func(x))
res <- do.call(rbind,res)
mean_boot <- mean(res)
se_boot <- sd(res)
mean_boot
se_boot

mean(predict(mod1,newdata = subset(e,BMI>=40&BMI<=45&age>=25&age<=30)))

mean(subset(e,BMI>=40&BMI<=45&age>=25&age<=30,select = GA)$GA)
