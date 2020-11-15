## Intro to chaos theory and dynamical systems in R

## The first thing you'll want to do is install the differential equations
## solver Desolve
## Only run this if you need it
#install.packages('deSolve')
library(deSolve)
library(tidyverse)
library(scatterplot3d)
library(plotly)
theme_preset<-theme_bw()+
  theme(text=element_text(size=16))


## So first let's talk about very simple discrete dynamical systems

## Unimpeded population growth
## Can be modeled with a simple equation
## N(t) = r*N(t-1)
## Using some recursive logic
## N(t)=r*r*N(t-2)
## OR,,,
## N(t)=r*r*r*N(t-3)....
## If you go the whole down
# N(t)=r^(t-1)N(0)
## This is the system of every bunny has 2 babies forever
unimpeded_growth<-function(r,t,N0){
  N_t=N0*r^(t)
  return(N_t)
}
sample_dynamics<-unimpeded_growth(r=1.1,
                                  t=0:100,
                                  N0=5)
## See what this looks like
plot(0:100,
     sample_dynamics,
     xlab='Time in generations',
     ylab='N(t) population size',
     main='Unimpeded Growth r=1.1')

## But what if we have a cap and the growth slows down around the cap
logistic_growth<-function(r,tmax,N0){
  N_t<-c()
  N_t[1]<-N0
  for(i in 2:tmax){
    N_t[i]<-N_t[i-1]*(1-N_t[i-1])*r
  }
  return(N_t)
}
logistic_dynamics<-logistic_growth(r=1.1,tmax=100,N0=0.01)
## Changing the graphical parameters for base R plots
par(mfrow=c(2,1))
plot(0:99,
     logistic_dynamics,
     xlab='Time in generations',
     ylab='Relative N(t)',
     main='Capped population size (logistic growth)')

r_range<-seq(0.5,4,by=0.1)
output<-lapply(r_range,function(x) logistic_growth(r=x,tmax=500,N0=0.01))
burned_in<-list()
for(i in 1:length(output)){
  burned_in[[i]]<-data.frame(r=r_range[i],values=output[[i]][-c(1:100)])
}
burned_in<-do.call(rbind,burned_in)

logistic_map<-ggplot(burned_in)+
  geom_point(aes(x=r,y=values))+
  theme_preset+
  ggtitle('Map of Logistic Growth\nfor Different Values of r')

## Ayyy we got there - this is the logistic map, which
## is the simplest chaotic dynamical system being 1d and discrete. 
## Once r surpasses a critical threshold, the final population size
## does not converge and cannot be predicted on long timescales

## Let's talk about a slightly more complicated system
## So this is a lorenz system used for very simplified fluid mixing
lorenz<-function(t,state,parameters){
  with(as.list(c(state,parameters)),{
    dX<-a*X+Y*Z
    dY<-b*(Y-Z)
    dZ<--X*Y+c*Y-Z
    list(c(dX,dY,dZ))
  })
}
## Now we initialize and solve
parameter<-c(a=-8/3,b=-10,c=28)
state<-c(X=1,Y=1,Z=1)
times<-seq(0,100,by=0.01)

## Solving dynamical system
evolution<-ode(y=state,
               times=times,
               func=lorenz,
               parms=parameter)

## Tidying for ggplot
evolution_frame<-data.frame(evolution) %>%
  pivot_longer(cols=c('X','Y','Z'),
               names_to='state',
               values_to='value')

## Plotting time series of state variables
ggplot(evolution_frame)+
  geom_line(aes(x=time,y=value,group=state))+
  facet_wrap(~state,ncol=1)+
  theme_preset+
  xlab('Time')+
  ylab('State Variable')

## Making 3d plot
scatterplot3d(x=evolution[,2],
              y=evolution[,3],
              z=evolution[,4],
              type='l',
              color='darkblue',
              xlab='X',
              ylab='Y',
              zlab='Z',
              main='Lorenz Attractor')

## Making a nicer 3d plot using plotly functionalities
plot_ly(data=data.frame(evolution),
        x=~X,y=~Y,z=~Z,
        type='scatter3d',
        mode='lines')



