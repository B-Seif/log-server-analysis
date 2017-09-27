
src_fw<-read.table("Jeu-2-projet.csv", sep=",", header = F ) 


colnames(src_fw) <- c("ipsrc","ipdst","portdst","proto","action","date","regle")


library(sqldf)


traitement<-sqldf("select ipsrc, count(*) as nombre, count(distinct ipdst) as cnbripdst, count(distinct portdst) as cnportdst,
                  sum( case when action  like 'Permit' then 1 ENd) as permit, 
                  sum( case when action  like 'Permit' and portdst < 1024 then 1 ENd) as inf1024permit, 
                  sum( case when action  like 'Permit' and portdst >= 1024 then 1 ENd) as sup1024permit, 
                  sum( case when action  like 'Permit' and (portdst = 21 OR portdst = 22 OR portdst = 3389 OR portdst = 3306) then 1 ENd) as adminpermit,
                  sum(case when action like 'Deny' then 1 ENd) as deny, 
                  sum(case when action like 'Deny' and portdst < 1024  then 1 ENd) as inf1024deny, 
                  sum(case when action like 'Deny' and portdst >= 1024  then 1 ENd) as sup1024deny,
                  sum( case when action  like 'Deny' and (portdst = 21 OR portdst = 22 OR portdst = 3389 OR portdst = 3306) then 1 ENd) as admindeny
                  
                  from src_fw  group by ipsrc ")


traitement[is.na(traitement)]<-0


traitement$permit<-as.integer(traitement$permit)
traitement$inf1024permit<-as.integer(traitement$inf1024permit)
traitement$sup1024permit<-as.integer(traitement$sup1024permit)
traitement$adminpermit <-as.integer(traitement$adminpermit )
traitement$deny<-as.integer(traitement$deny)
traitement$inf1024deny<-as.integer(traitement$inf1024deny)
traitement$sup1024deny<-as.integer(traitement$sup1024deny)
traitement$admindeny<-as.integer(traitement$admindeny)


rownames(traitement) <- traitement$ipsrc
traitement <- traitement[-1]