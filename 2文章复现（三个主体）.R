library(tidyverse)
library(data.table)
library(EvolutionaryGames)
library(ggthemes)
library(latex2exp)
library(ggtext)

setwd("/home/zuo_r/involution")

duplicate_dynamic<-function(y,x,M,beta,d,N,c,e){
  
  #-----参数解释说明------#
  # #N个个体
  # N<-4
  # #M资源 c(5,15,25)
  # M<-5
  # #less effort的成本
  # c<-1
  # #投入效用
  # beta<-1
  # #more effort的成本
  # d<-4
  #-----------------------#
  
  
  
  #在没有解决累加前提下的尝试
  
  
  #这里决定用N-1
  #需要取整
  Nd<-floor(y*(N-1)) 
  Nc<-floor(x*(N-1-Nd))
  Ne<-N-1-Nd-Nc
  
  #策略c（cooperate, less effort）和策略d(defect，more effort)和策略e(躺平)的收益
  #（1）个体选择策略c的期望收益
  pai_c<-(beta*c*M)/(Ne*e+(Nc+1)*beta*c)-c
  #（2）个体选择策略d的期望收益
  pai_d<-(beta*d*M)/(Nc*c+(Nd+1)*beta*d)-d
  #（3）个体选择策略e的期望收益
  pai_e<-(e*M)/((Ne+1)*e+Nc*beta*c)-e
  
  
  #群体体选择策略c或d的收益  = 概率*pai
  #Pc与Pe等价
  Pd<-(choose(N-1, Nd)*(y^Nd)*((1-y)^(N-1-Nd)))*pai_d #choose计算组合数
  Pc<-(choose(N-1, Nc)*(y^Nd)*(((1-y)*x)^Nc)*(((1-y)*(1-x))^Ne))*pai_c
  Pe<-(choose(N-1, Ne)*(y^Nd)*(((1-y)*x)^Nc)*(((1-y)*(1-x))^Ne))*pai_e

  
  #（3）群体选择策略d的平均收益
  R_<-y*Pd+x*Pc+(1-x)*Pe
  
  
  #(4)复制动态方程
  #y.<-y(Pd-R_) = y(Pd-y*Pd-x*Pc-(1-x)*Pe) = (1-y)*Pd-x*(Pc-Pe)-Pe
  y.<-(1-y)*Pd-x*(Pc-Pe)-Pe
  
}



#----------------------------------------------------------------------#
#------------------------------图1-------------------------------------#
#----------------------------------------------------------------------#
plot_y._y<-function(M){
  y.<-c()
  for(y in seq(0, 1, 0.01)){ 
    for(x in seq(0, 1, 0.01)){
      temp<-duplicate_dynamic(y,M,beta=1,d=4,N=100,c=1)
    
    # N: 个体数
    # M: 资源 c(5,15,25)
    # c: less effort的成本
    # beta: 投入效用
    # d: more effort的成本
    
      y.<-c(y.,temp)
  
    }
  }
  df <- data.frame(x = seq(0, 1, 0.01), y = y.) %>% 
    mutate(y1=c(y.[-1],1)) %>% 
    mutate(y0=y*y1)
  
  #ystar<-df$x[df$y0<0]+0.005 #即横坐标
  
  ystar<-ifelse(sum(df$x[df$y0<0])==0,0,df$x[df$y0<0]+0.005)
  
  ggplot(df,aes(x=x,y=y))+
    geom_line(color="black",size=0.3)+
    theme_few() +
    geom_point(aes(x=ystar,y=0))+
    geom_text(aes(x=ystar,y=0),label=paste("y*:",ystar),size=4,nudge_y = 0.001,nudge_x = 0.03,color="red")+
    geom_hline(aes(yintercept = 0),size=0.3)+
    labs(x = "y", y = "y.")
}


#---1. fig1 (a1)---#
plot_y._y(M=100)

#---2. fig1 (b1)---#
plot_y._y(M=200)

#---3. fig1 (c1)---#
plot_y._y(M=398)#右临界资源值


#----------------------------------------------------------------------#
#------------------------------图2-------------------------------------#
#----------------------------------------------------------------------#
ystar_collect<-function(M,beta,d){
  
  y.<-c()
  
  for(y in seq(0, 1, 0.01)){
    temp<-duplicate_dynamic(y,M,beta,d,N=100,c=1)
    
    # N: 个体数
    # M: 资源 c(5,15,25)
    # c: less effort的成本
    # beta: 投入效用
    # d: more effort的成本
    
    y.<-c(y.,temp)
  }
  
  df <- data.frame(x = seq(0, 1, 0.01), y = y.) %>% 
    mutate(y1=c(y.[-1],1)) %>% 
    mutate(y0=y*y1)
  
  #ystar<-ifelse(sum(df$x[df$y0<0])==0,0,df$x[df$y0<0]+0.005)
  if(all(df$y<=0)){
    ystar<-0 
  }else if(all(df$y>=0)){
    ystar<-1
  }else{
    ystar<-df$x[df$y0<0]+0.005
  }
  
  return(ystar)
}

#均衡ystar值收集
parameters<-data.frame(beta=c(1,1,1,0.6,0.6,0.6),d=c(2,4,8,2,4,8))

result<-data.frame()

for(i in 1:6){
  beta<-parameters$beta[i]
  d<-parameters$d[i]
  
  ystars<-c()
  
  for (M in seq(0,1000,10)){
    temp<-ystar_collect(M,beta,d)
    ystars<-c(ystars,temp)
  }
  
  data<-data.frame(beta=rep(beta,length(ystars)),d=rep(d,length(ystars)),M=seq(0,1000,10),ystar=ystars)
  
  result<-rbind(result,data)
}

#数据可视化
df<-result %>% 
  mutate(label=paste("beta=",beta,",","d=",d,sep=""))

ggplot(df,aes(x=M,y=ystar,color=label))+
  geom_line()+
  geom_point()+
  theme_few()




