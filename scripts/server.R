
library(shiny)




selection =function (src_fw,protocole,port){
  switch(protocole, 
         TCP= 
           switch(port, 
                  Tous     = subset( src_fw,proto=='TCP' , select=c(portdst,action)),
                  Inf1024  = subset( src_fw, proto=='TCP' &  portdst < 1024,select=c(portdst,action)),
                  Sup1024  = subset( src_fw, proto=='TCP' &  portdst > 1024,select=c(portdst,action))
           ),
         
         
         UDP= 
           switch(port, 
                  Tous     =  subset( src_fw,proto=='UDP' , select=c(portdst,action)),
                  Inf1024  =  subset( src_fw, proto=='UDP' &  portdst < 1024,select=c(portdst,action)),
                  Sup1024  =  subset( src_fw, proto=='UDP' &  portdst > 1024,select=c(portdst,action))
           ),
         
         
         ALL=
           switch(port, 
                  Tous     =   subset( src_fw , select=c(portdst,action)),
                  Inf1024  =   subset( src_fw, portdst < 1024,select=c(portdst,action)),
                  Sup1024  =   subset( src_fw,  portdst > 1024,select=c(portdst,action))
           )
  )
  
}



comportement_ip=function (src_fw,ip)
{
  
  z=subset( src_fw , ipdst  == ip )
  
 

  ztraitement=sqldf("select ipsrc, count(*) as nombre, count(distinct ipdst) as cnbripdst, count(distinct portdst) as cnportdst,
                  sum( case when action  like 'Permit' then 1 ENd) as permit, 
                  sum( case when action  like 'Permit' and portdst < 1024 then 1 ENd) as inf1024permit, 
                  sum( case when action  like 'Permit' and portdst >= 1024 then 1 ENd) as sup1024permit, 
                  sum( case when action  like 'Permit' and (portdst = 21 OR portdst = 22 OR portdst = 3389 OR portdst = 3306) then 1 ENd) as adminpermit,
                  sum(case when action like 'Deny' then 1 ENd) as deny, 
                  sum(case when action like 'Deny' and portdst < 1024  then 1 ENd) as inf1024deny, 
                  sum(case when action like 'Deny' and portdst >= 1024  then 1 ENd) as sup1024deny,
                  sum( case when action  like 'Deny' and (portdst = 21 OR portdst = 22 OR portdst = 3389 OR portdst = 3306) then 1 ENd) as admindeny
                  
                  from z  group by ipsrc ")
  ztraitement[is.na(ztraitement)]<-0
  
  #typage
  ztraitement$permit<-as.integer(ztraitement$permit)
  ztraitement$inf1024permit<-as.integer(ztraitement$inf1024permit)
  ztraitement$sup1024permit<-as.integer(ztraitement$sup1024permit)
  ztraitement$adminpermit <-as.integer(ztraitement$adminpermit )
  ztraitement$deny<-as.integer(ztraitement$deny)
  ztraitement$inf1024deny<-as.integer(ztraitement$inf1024deny)
  ztraitement$sup1024deny<-as.integer(ztraitement$sup1024deny)
  ztraitement$admindeny<-as.integer(ztraitement$admindeny)
  
  #ipsrc = identifiant
  rownames(ztraitement) <- ztraitement$ipsrc
  ztraitement <- ztraitement[-1]

}
















function(input,output){

output$barplotPort=renderPlot({ 
    
    res=selection(src_fw,input$protocole,input$ports)
    
    counts= table(res$portdst)
    
    d= as.data.frame(counts)
  

    if (input$ordre==TRUE) 
      counts=tail(d[ order(d$Freq),],input$nbr_port)
    else
      counts=head(d[ order(d$Freq),],input$nbr_port)
    
    res= subset(res,portdst %in% counts$Var1)
    counts= table(res$portdst)
    barplot(counts,col='gray',main="Les Port ",xlab=" Ports")
    
    
  })
  
output$permitVSdeny=renderPlot({ 

res=selection(src_fw,input$protocole,input$ports)
counts= table(res$action,res$portdst)
d= as.data.frame(counts)

if (input$ordre==TRUE)
  counts=tail(d[ order(d$Freq),],input$nbr_port)
else
  counts=head(d[ order(d$Freq),],input$nbr_port)
 

res= subset(res,portdst %in% counts$Var2,select=c(portdst,action))
counts= table(res$action,res$portdst)
barplot(counts,col=rainbow(2),legend=rownames(counts),main="les ports utilises vs Permit/Deny",xlab=" Ports")

})

output$seBalader=renderPlot({ 
  
  input$parcours
  
  tmp= traitement[,c('cnbripdst','deny')]

 
  index = seq(1,dim(traitement)[1])
  
  ip_id=as.integer(input$parcours)
  
  plot(index, tmp[,1],col='blue',pch='o',ylim=c(0, max(tmp))   )
  par(new=TRUE)
  abline(v=ip_id,col='green')
  par(new=TRUE)
  plot(index,tmp[,2],col='red',pch='+',add=TRUE,ylim=c(0, max(tmp)))
  
 
  text(100,170,paste('Nombre IP destination contactees:',tmp[ip_id,1]),col='blue')
  text(100,145,paste('Nombre de deny:', tmp[ip_id,2]),col='red')
  text(100,120,paste('Adresses IP source:',rownames(tmp)[ip_id]),col='black')
  
  })

output$cah=renderPlot({  
  
 
  
  donnes_ip = comportement_ip(src_fw,input$choix_ip)
  
  
  
  matrice_distances= dist(donnes_ip)
  
 
  
  clusters=hclust(matrice_distances,method=input$method)
  


if(input$inertie==TRUE) {
 
   agg=numeric(length(clusters$height))
  
   for (i in 1: length(agg)-1){
     agg[i]=clusters$height[i+1]-clusters$height[i]
   }
   
  plot(seq(1,length(agg)),rev(agg),type="l",main="Ecarts entre hauteurs d'aggregation",xlab="nombre de partition",ylab="Ecart d'aggregation")
  print(length(agg))
  
 }
  
   else{
  
     plot(clusters, main = paste("Vue globle des comportements sur l adresse : ",input$choix_ip ,  ", nombre de connections: ", dim(donnes_ip)[1])   , xlab="IP Source") 
  
     if(input$encadrer==TRUE)
     {   
     rect.hclust(clusters,k=as.integer(input$class))
     }
  
   
   }
  
  
  
 
  
 

})

output$pie=renderPlot({
 
   
   
 res=selection(src_fw,input$protocole,input$ports)
  
  
  res=table(res$action) 
 
  pct= round(res/sum(res)*100)
  labels=c("Deny","Permit")
  labels=paste(labels,pct)
  labels=paste(labels,"%", sep="")
 
  pie(res,labels,col=rainbow(length(labels)),main='Repartition par action')


   
   
   
 })
 
output$donnees=renderDataTable(src_fw)
 
output$tablebrute=renderTable(ztraitement)
 
 
}


