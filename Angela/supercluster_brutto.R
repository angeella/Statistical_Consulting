
library(cluster)

datAA$number <- as.numeric(datAA$date)

dataCluster <- reshapami(dataset = datAA)

#school_closing
rownames(dataCluster$school_closing) <- unique(datAA$id)
A<- as.data.frame(dataCluster$school_closing)
d.pol <- daisy(A[,2:81], metric = "gower", stand = FALSE)#Gower's generalized similarity

agn2 <- hclust(d.pol)
Cl_school <- cutree(tree = agn2,k = 4)

#Workplace
rownames(dataCluster$workplace_closing) <- unique(datAA$id)
B<- as.data.frame(dataCluster$workplace_closing)
d.pol <- daisy(A[,2:81], metric = "gower", stand = FALSE)#Gower's generalized similarity

agn2 <- hclust(d.pol)
Cl_work <- cutree(tree = agn2,k = 4)

#cancel_events
rownames(dataCluster$cancel_events) <- unique(datAA$id)
A<- as.data.frame(dataCluster$cancel_events)
d.pol <- daisy(A[,2:81], metric = "gower", stand = FALSE)#Gower's generalized similarity

agn2 <- hclust(d.pol)
Cl_events <- cutree(tree = agn2,k = 4)

#gatherings_restrictions
rownames(dataCluster$gatherings_restrictions) <- unique(datAA$id)
A<- as.data.frame(dataCluster$gatherings_restrictions)
d.pol <- daisy(A[,2:81], metric = "gower", stand = FALSE)#Gower's generalized similarity

agn2 <- hclust(d.pol)
Cl_gath <- cutree(tree = agn2,k = 4)

#transport_closing
rownames(dataCluster$ransport_closing) <- unique(datAA$id)
A<- as.data.frame(dataCluster$ransport_closing)
d.pol <- daisy(A[,2:81], metric = "gower", stand = FALSE)#Gower's generalized similarity

agn2 <- hclust(d.pol)
Cl_transport <- cutree(tree = agn2,k = 4)

#stay_home_restrictions
rownames(dataCluster$stay_home_restrictions) <- unique(datAA$id)
A<- as.data.frame(dataCluster$stay_home_restrictions)
d.pol <- daisy(A[,2:81], metric = "gower", stand = FALSE)#Gower's generalized similarity

agn2 <- hclust(d.pol)
Cl_home <- cutree(tree = agn2,k = 4)

#internal_movement_restrictions
rownames(dataCluster$internal_movement_restrictions) <- unique(datAA$id)
A<- as.data.frame(dataCluster$internal_movement_restrictions)
d.pol <- daisy(A[,2:81], metric = "gower", stand = FALSE)#Gower's generalized similarity

agn2 <- hclust(d.pol)
Cl_int <- cutree(tree = agn2,k = 4)


#international_movement_restrictions
rownames(dataCluster$international_movement_restrictions) <- unique(datAA$id)
A<- as.data.frame(dataCluster$internal_movement_restrictions)
d.pol <- daisy(A[,2:81], metric = "gower", stand = FALSE)#Gower's generalized similarity

agn2 <- hclust(d.pol)
Cl_inter <- cutree(tree = agn2,k = 4)

#information_campaigns
rownames(dataCluster$information_campaigns) <- unique(datAA$id)
A<- as.data.frame(dataCluster$information_campaigns)
d.pol <- daisy(A[,2:81], metric = "gower", stand = FALSE)#Gower's generalized similarity

agn2 <- hclust(d.pol)
Cl_info <- cutree(tree = agn2,k = 4)

#testing_policy
rownames(dataCluster$testing_policy) <- unique(datAA$id)
A<- as.data.frame(dataCluster$testing_policy)
d.pol <- daisy(A[,2:81], metric = "gower", stand = FALSE)#Gower's generalized similarity

agn2 <- hclust(d.pol)
Cl_test <- cutree(tree = agn2,k = 4)

#contact_tracing
rownames(dataCluster$contact_tracing) <- unique(datAA$id)
A<- as.data.frame(dataCluster$contact_tracing)
d.pol <- daisy(A[,2:81], metric = "gower", stand = FALSE)#Gower's generalized similarity

agn2 <- hclust(d.pol)
Cl_tracing <- cutree(tree = agn2,k = 4)

SUPER_CLUSTER <- cbind(Cl_school, Cl_work,Cl_home,Cl_info,Cl_int,Cl_inter,Cl_test,Cl_tracing,Cl_transport,Cl_events,Cl_gath)
View(SUPER_CLUSTER)

datAA2$contact_tracing <- as.factor(datAA2$contact_tracing)

out <- curveRep(datAA$date, datAA$contact_tracing,id = datAA$id)
out$res
ggplot(datAA2, aes(x= date, y = id, fill = contact_tracing)) + geom_tile()
