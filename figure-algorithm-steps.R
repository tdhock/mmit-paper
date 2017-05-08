source("packages.R")

f <- function(from, to, const, linear){
  pred <- seq(from, to, l=2)
  data.table(pred, cost=const+linear*pred)
}
step.vec <- c(
  "$t=1$ before optimization",
  "$t=1$ after optimization",
  "$t=2$")
step <- function(x){
  factor(step.vec[x], step.vec)
}
cost1 <- rbind(
    f(0, 3, 0, 0),
    f(3, 5, -3, 1)
  )
cost.lines <- rbind(
  data.table(step=step(1), cost1),
  data.table(step=step(2), cost1),
  data.table(step=step(3), rbind(
    f(0, 2, 2, -1),
    f(2, 3, 0, 0),
    f(3, 5, -3, 1)
  ))
)
ab <- function(const, linear){
  data.table(const, linear)
}
ab1 <- rbind(
    ab(0, 0),
    ab(-3, 1)
  )
ab.lines <- rbind(
  data.table(step=step(1), ab1),
  data.table(step=step(2), ab1),
  data.table(step=step(3), rbind(
    ab(2, -1),
    ab(0, 0),
    ab(-3, 1)
  ))
)
break.points <- data.table(cost=0, rbind(
  data.table(pred=2, step=step(3)),
  data.table(pred=3, step=step(1:3))))
pointers <- data.table(
  pred=c(3, 3, 3),
  pred.to=c(Inf, 3, 3),
  cost.from=1.5,
  vjust=c(0.5, -0.5, -0.5),
  cost.to=c(1.4, 0, 0)+0.1,
  hjust=c(1.1, 0.5, 0.5),
  label=c("$j_1=2$", "$J_1=1$", "$j_2=J_2=2$"),
  step=step(1:3))
break.labels <- rbind(data.table(
  pred=3.3,
  step=step(1),
  hjust=0.5,
  cost=-c(0.2, 0.6),
  label=c("$b_{1,1}=3$", "$f_{1,1}(\\mu)=\\mu-3$")
), data.table(
  pred=rep(c(2, 3)+0.3*c(-1,1), each=2),
  step=step(3),
  hjust=0.5,
  cost=-c(0.2, 0.6),
  label=c(
    "$b_{2,1}=2$", "$f_{2,1}(\\mu)=\\mu-2$"
   ,"$b_{2,2}=3$", "$f_{2,2}(\\mu)=\\mu-3$"
  )          
))
cost.labels <- data.table(
  cost=c(-0.2, 0.3),
  pred=c(1, 1),
  label=paste0("$\\underline C_", 1:2, "(\\mu)$"),
  step=step(c(1, 3)),
  hjust=0)
line.labels <- rbind(data.table(
  pred=c(2, 3.9),
  hjust=c(0.5, 1),
  cost=c(0.2, 1.05),
  label=c("$c_{1,1}(\\mu)$", "$m_1(\\mu)=c_{1,2}(\\mu)=\\mu-3$"),
  step=step(1)
), data.table(
  pred=1.9,
  hjust=0.5,
  cost=0.2,
  label="$M_1(\\mu)=c_{1,1}(\\mu)=0$",
  step=step(2)
), data.table(
  pred=c(1.05, 2.5, 2.5, 2.5, 3.95),
  hjust=c(0, 0.5, 0.5, 0.5, 1),
  cost=c(1.1, 0.2, 0.45, 0.7, 1.1),
  label=c(
    "$c_{2,1}(\\mu)$",
    "$m_2(\\mu)=0$",
    "$M_2(\\mu)=$",
    "$c_{2,2}(\\mu)=$",
    "$c_{2,3}(\\mu)$"
  ),
  step=step(3)
))
lab.size <- 3
gg <- ggplot()+
  theme_grey()+
  theme(
    ##panel.margin=grid::unit(0, "lines"),
    panel.grid.minor=element_blank()
  )+
  facet_grid(. ~ step)+
  geom_abline(aes(
    slope=linear, intercept=const),
    data=ab.lines,
    color="grey",
    size=3)+
  geom_label(aes(
    pred, cost, label=label, hjust=hjust),
    size=lab.size,
    data=break.labels)+
  geom_text(aes(
    pred, cost, label=label, hjust=hjust),
    color="red",
    data=cost.labels)+
  geom_text(aes(
    pred, cost, label=label, hjust=hjust),
    color="grey40",
    size=lab.size,
    data=line.labels)+
  geom_text(aes(
    pred, cost.from, label=label, hjust=hjust, vjust=vjust),
    size=lab.size,
    color="blue",
    data=pointers)+
  geom_line(aes(
    pred, cost),
    size=1,
    color="red",
    data=cost.lines)+
  geom_segment(aes(
    pred, cost.from,
    xend=pred.to, yend=cost.to),
    color="blue",
    arrow=grid::arrow(type="closed", length=grid::unit(0.15, "lines")),
    data=pointers)+
  geom_point(aes(
    pred, cost),
    shape=1,
    data=break.points)+
  scale_y_continuous("cost")+
  scale_x_continuous("predicted value $\\mu$")+
  coord_cartesian(xlim=c(0.9,4.1), ylim=c(-1, 1.8), expand=FALSE)
print(gg)
w <- 5.5
h <- 2
tikz("figure-algorithm-steps-standalone.tex", w, 2, standAlone=TRUE)
print(gg)
dev.off()
code <- system("pdflatex figure-algorithm-steps-standalone && convert -density 150 -trim figure-algorithm-steps-standalone.pdf -quality 100 -flatten -sharpen 0x1.0 figure-algorithm-steps.png")
if(code!=0)stop(code)
tikz("figure-algorithm-steps.tex", w, 2, standAlone=FALSE)
print(gg)
dev.off()

