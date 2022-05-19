library(ragg)
library(Rcpp)
library(ggplot2)

#an attractor adapted from the work of Antonio Sánchez Chinchón - @aschinchon
# https://github.com/aschinchon/general-2D-map

#ggplot theme blank canvas
opt = theme(legend.position  = "none",
            panel.background = element_rect(fill="#FFFFFF"),
            axis.ticks       = element_blank(),
            panel.grid       = element_blank(),
            axis.title       = element_blank(),
            axis.text        = element_blank())

#Rcpp attractor function
cppFunction('DataFrame createTrajectory(int n, double x0, double y0,
            double a, double b, double c, double d) {
            // create the columns
            NumericVector x(n);
            NumericVector y(n);
            x[0]=x0;
            y[0]=y0;
            for(int i = 1; i < n; ++i) {
            x[i] = sin(a*y[i-1])+c*cos(a*x[i-1]);
            y[i] = sin(b*x[i-1])+d*cos(b*y[i-1]);
            }
            // return a new data frame
            return DataFrame::create(_["x"]= x, _["y"]= y);
            }
            ')

#cp#define constants
a=1.24
b=-1.25
c=-1.81
d=-1.91


a=1.5
b=-1.3
c=-1.1
d=-1.41

#make dataframe and plot the points
df=createTrajectory(10000000, 0, 0, a, b, c, d)

a <- ggplot(df, aes(x, y)) + geom_point(color="#0072CE", shape=46, alpha=.01) + opt

plot(a)

ggsave("./assets/img/artsy.png", plot=a, height=150, width=165, units = "mm")
