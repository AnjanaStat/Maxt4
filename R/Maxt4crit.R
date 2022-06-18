#' Find test statistic value and corresponding critical value
#'
#' More detailed description
#'
#' @param g1 a real vector
#' @param g2 a real vector
#' @param g3 a real vector
#' @param g4 a real vector
#' @param alpha a real number between 0 and 1
#'
#' @return a numerical vector
#'
#' @examples
#' g1=c(1,2,4,1,1,3,4,0,2,4,1,3,5,2,1,4)
#' g2=c(0.4,2,-4,0.9,4.6,8,9.8,9,4,5,6,3)
#' g3=c(0,1,2,5,7,3,7.9,3,2)
#' g4=c(0,-3,-4,5,1,3,5,7,2,1,2,0)
#' Maxt4crit(g1,g2,g3,g4,0.05)
#'
#' @export
Maxt4crit<-function(g1,g2,g3,g4,alpha)
{
  fun1<-function(n1,n2,n3,n4,v1,v2,v3,v4)
  {
    g1<-rnorm(n1,0,sqrt(v1));g2<-rnorm(n2,0,sqrt(v2));g3<-rnorm(n3,0,sqrt(v3));g4<-rnorm(n4,0,sqrt(v4))
    X1=mean(g1);X2=mean(g2);X3=mean(g3);X4=mean(g4)
    S1=var(g1);S2=var(g2);S3=var(g3);S4=var(g4)
    v1<-sqrt(S1/n1+S2/n2);v2<-sqrt(S2/n2+S3/n3);v3<-sqrt(S3/n3+S4/n4)
    T1=(X2-X1)/v1;T2=(X3-X2)/v2;T3=(X4-X3)/v3
    T=max(T1,T2,T3,na.rm = FALSE)
    return(T)
  }
  fun2<-function(n1,n2,n3,n4,alpha,v1,v2,v3,v4)
  {
    x<-replicate(5000,fun1(n1,n2,n3,n4,v1,v2,v3,v4))
    y<-sort(x,decreasing=FALSE)
    m=(1-alpha)*5000
    c<-y[m]
    return(c)
  }
  fun3<-function(n1,n2,n3,n4,alpha,v1,v2,v3,v4)
  {
    z=replicate(10,fun2(n1,n2,n3,n4,alpha,v1,v2,v3,v4))
    cri=mean(z)
    return(cri)
  }
  fun4<-function(g1,g2,g3,g4)
  {
    X1=mean(g1);X2=mean(g2);X3=mean(g3);X4=mean(g4)
    n1=length(g1);n2=length(g2);n3=length(g3);n4=length(g4)
    S1=var(g1);S2=var(g2);S3=var(g3);S4=var(g4)
    v1<-sqrt(S1/n1+S2/n2);v2<-sqrt(S2/n2+S3/n3);v3<-sqrt(S3/n3+S4/n4)
    T1=(X2-X1)/v1;T2=(X3-X2)/v2;T3=(X4-X3)/v3
    T=max(T1,T2,T3,na.rm = FALSE)
    return(T)
  }
  set.seed(20)
  n1=length(g1);n2=length(g2);n3=length(g3);n4=length(g4)
  v1=var(g1);v2=var(g2);v3=var(g3);v4=var(g4)
  statistic_value<-fun4(g1,g2,g3,g4)
  crit_value<-fun2(n1,n2,n3,n4,alpha,v1,v2,v3,v4)
  result<-c(statistic_value,crit_value)
  return(result)
}
