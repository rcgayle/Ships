
library(Imap)


#distance between consecutive observations

DN<-function(H,i){
        return(gdist(H[i, 2], H[i, 1], H[i+1, 2], H[i+1, 1], units='m'))
                   }
                   
#index of most recent largest movement btw consecutive observations

idobs<-function(H){
        D<-0
        I<-1 
        for (i in 1:(dim(H)[1]-1)){
              if  (DN(H, i)>=D){
                     D<-DN(H, i)
                     I<-i
                                }
              else{}
                                   }
        return(I)
                   }

#format table for largest movement btw consecutive observations                        
         
PosTable<-function(S){
    S<-as.data.frame(as.matrix(S), row.names=c('Position 1','Position 2'))
    return(S)
                      }       