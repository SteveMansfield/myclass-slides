pf<-function(p){
  sprintf("%0.4f",round(p,4)) %>% sub("^0+","", .)
}

tf<-function(t){
  sprintf("%0.3f",round(p,3)) %>% sub("^0+","", .)
}

tt<-function(from){
  d<-tibble(df=seq(from,from+4,by=1))
  d %>% mutate(tcrit=qt(p=.975,df=df)) %>% 
    knitr::kable(digits=c(3),align='cc',col.names = c("$df$","$t_\\text{crit}$")) %>% 
    column_spec(1:2,width="6em")
  
}

unt<-function(from){
  d<-tibble(z=seq(from,from+.04,by=.01))
  
  d %>% mutate(B=pf(pnorm(z)),C=pf(1-pnorm(z)),D=pf(pnorm(z)-.5)) %>% 
    knitr::kable(digits=c(2),align='cccc') %>% 
    column_spec(1:4,width="6em")
} 

dnorm_limit<-function(f,t){
  function(x){
    y<-dnorm(x)
    y[x<f | x>t]<-NA
    return(y)
  }
}

nd<-function(f,t){
  base<-ggplot()+xlim(-3,3)
  base+
    stat_function(fun=dnorm_limit(f,t),geom = "area", fill = "bisque1",n=200)+
    geom_function(fun=dnorm)+
    theme_classic(base_size=20, base_line_size = .5)+
    guides(y="none")+labs(y=NULL) +
    scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3),limits = c(-3,3),name="z score")+
    scale_y_continuous(expand=c(0,0))
}

nd_blank<-function(){
  base<-ggplot()+xlim(-3,3)
  base+
    geom_function(fun=dnorm)+
    theme_classic(base_size=20, base_line_size = .5)+
    guides(y="none")+labs(x=NULL, y=NULL) +
    scale_y_continuous(expand=c(0,0))+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank()
    )
}

dsm<-function(data,n,reps){
  m=replicate(reps,mean(sample(data,n,TRUE)))
  tibble(m)
}
dsmp<-function(data,n,reps=1000){
  dm=mean(data)
  ds=sd(data)
  dsm(data,n,reps) %>% 
    ggplot(aes(x=m))+
    geom_histogram(fill="gray80",binwidth=1/n,center=0)+
    #   stat_function(fun = function(x) dnorm(x, mean = dm, sd = ds/sqrt(n)) * reps/n,color = "red", size = .5)+
    xlim(0.5,7.5)+
    theme_classic(base_size = 20, base_line_size = .1)+
    scale_y_continuous(expand=c(0,0))+
    xlab('sample mean')+ylab('frequency')
}

dsmpc<-function(data,n,reps=1000){
  dm=mean(data)
  ds=sd(data)
  dsm(data,n,reps) %>% 
    ggplot(aes(x=m))+
    geom_histogram(fill="gray80",binwidth=1/n,center=0)+
    stat_function(fun = function(x) dnorm(x, mean = dm, sd = ds/sqrt(n)) * reps/n,color = "red", size = .5)+
    xlim(0.5,7.5)+
    theme_classic(base_size = 20, base_line_size = .1)+
    scale_y_continuous(expand=c(0,0))+
    xlab('sample mean')+ylab('frequency')
}

orig<-function(data){
  tibble(x=data) %>% 
    ggplot(aes(x))+
    geom_histogram(fill="gray80",binwidth=1,center=0)+
    xlim(0.5,7.5)+
    theme_classic(base_size = 20, base_line_size = .1)+
    scale_y_continuous(expand=c(0,0))+
    xlab('rating')+ylab('frequency') 
}

h1<-c( 2, 4, 7, 3, 3, 4, 5, 2, 3, 5, 1, 5, 3, 3, 4, 6, 1, 5, 6, 5, 3, 4, 6, 3, 5, 1, 3)
h2<-rep(1:7,c(4,3,2,1,2,3,4))
h3<-rep(1:7,7:1)

