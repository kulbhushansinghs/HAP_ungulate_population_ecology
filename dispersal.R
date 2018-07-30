install.packages("igraph")
library(igraph)
#library(Matrix)
setwd("C:/Users/Nipun/Desktop")
d<-0.25 #dispersal rate
n<-4 #vertex whose connections are to be deleted

#creating graph
gat<-read.csv("adjacency matrix.csv",header=TRUE,row.names=1,check.names=FALSE)
A<-as.matrix(gat)
g=graph.adjacency(A,mode="undirected",weighted=NULL); #g

#Make lay as comment after running first program 
lay <- layout.fruchterman.reingold(g)
V(g)$x <- lay[,1]
#V(g)$x<-c(18.67520071,17.70480162,17.97045949,17.25514661,16.66229553,15.69642522,14.9537731,14.66451846,14.53080147,13.87448607,16.43676307)
V(g)$y <- lay[,2]
#V(g)$y <- c(24.07019467,24.2762812,25.4814695,23.20063441,24.03530515,24.893654,25.66012857,24.58604864,23.42829487,24.02118076,27.24489613)
plot.igraph(g)
#plot(g)

B<-matrix(c(1.02,0.46,0.46,0,0.78,0,0,0,0.76),3,3) #transition matrix
PS<-matrix(rep(0,495),45,11) #patch wise total population
MS<-matrix(rep(0,495),45,11) #sum after growth
DS<-matrix(rep(0,495),45,11) #sum after dispersal
#t controls the no. of vertices to be deleted
for (t in 1:1){
  
  #Standardising the Adjacency matrix
  c<-colSums(A)
  for (i in 1:11){
    if (c[i]!=0){A[,i]<-A[,i]/c[i]}}
  A<-d*A; c<-colSums(A)
  for (i in 1:11){A[i,i]<-1-c[i]}
  #check colSums(A) = 1
  
  len<-15 #based on range of values of w
  for (w in 1:15){
    dat<-read.csv("pop.csv",header=TRUE)
    val<-colnames(dat)
    val<-val[2:12]
    
    #Population after Growth cycle and Dispersal
    for (j in 1:11){l<-0
    for (i in 2:12)
    {#k<-val[i]
      #print(k)
      assign(paste("p_",i-1,sep=""),dat[1:3,i]) #original popln.
      assign(paste("ps_",i-1,sep=""),sum(dat[1:3,i])) #sum of original popln.
      assign(paste("m_",i-1,sep=""),B%*%(dat[1:3,i])) #popln. after growth cycle
      assign(paste("ms_",i-1,sep=""),sum(B%*%(dat[1:3,i]))) #sum of popln. after growth cycle
      #assign(paste("p_",i-1,sep=""),B%*%(dat[1:3,i])) #update population no.s
      l <- l + (A[j,i-1]*(B%*%(dat[1:3,i])))
    } 
    assign(paste("d_",j,sep=""),l) #popln. post dispersal
    assign(paste("ds_",j,sep=""),sum(l)) #sum of popln. post dispersal
    }
    
    ps <- c(ps_1,ps_2,ps_3,ps_4,ps_5,ps_6,ps_7,ps_8,ps_9,ps_10,ps_11)
    ms <- c(ms_1,ms_2,ms_3,ms_4,ms_5,ms_6,ms_7,ms_8,ms_9,ms_10,ms_11)
    ds <- c(ds_1,ds_2,ds_3,ds_4,ds_5,ds_6,ds_7,ds_8,ds_9,ds_10,ds_11)
  
    x<-data.frame(d_1,d_2,d_3,d_4,d_5,d_6,d_7,d_8,d_9,d_10,d_11)
    colnames(x)<-val
    write.table(x, file = "pop.csv", sep = ",", col.names = NA, qmethod = "double")
  
    
    print((t-1)*len+w)
    PS[(t-1)*len+w,]<-ps; MS[(t-1)*len+w,]<-ms;DS[(t-1)*len+w,]<-ds  
    #print(ps)
    ns<-ps
    ns<-30*ns/max(ns)
    node.size<-setNames(ns,val)
    #plot(df,layout=l,vertex.label=V(df)$names,
    #edge.arrow.size=0.01,vertex.label.color = "black",vertex.size=node.size)
    
    #mypath=file.path("C:", "Users", "Nipun","Desktop","Images", paste("myplot_", w, ".jpeg", sep="")
    mypath <- file.path("C:","Users","Nipun","Desktop","Images",paste("plot_", (t-1)*len+ w, ".jpg", sep = ""))
    #print((t-1)*len+w)
    print(paste("myplot_", (t-1)*len+ w, ".jpg", sep = ""))
    jpeg(file=mypath)                  
    plot.igraph(g,vertex.size=node.size)
    dev.off()
  }
  
  deg_B <- degree(g) 
  #betw_B <- betweenness(g)
  #g delete_vertices or g-c("PM")
  assign(paste("del_",t,sep=""),order(deg_B,decreasing=T)[n])
  g<-(delete_edges(g,incident(g,order(deg_B,decreasing=T)[n])))
  A<-as.matrix(as_adjacency_matrix(g))
}

for (t in 2:2){
  
  #Standardising the Adjacency matrix
  c<-colSums(A)
  for (i in 1:11){
    if (c[i]!=0){A[,i]<-A[,i]/c[i]}}
  A<-d*A; c<-colSums(A)
  for (i in 1:11){A[i,i]<-1-c[i]}
  #check colSums(A) = 1
  
  len<-15 #based on previous range of values of w
  for (w in 1:30){
    dat<-read.csv("pop.csv",header=TRUE)
    val<-colnames(dat)
    val<-val[2:12]
    
    #Population after Growth cycle and Dispersal
    for (j in 1:11){l<-0
    for (i in 2:12)
    {#k<-val[i]
      #print(k)
      assign(paste("p_",i-1,sep=""),dat[1:3,i]) #original popln.
      assign(paste("ps_",i-1,sep=""),sum(dat[1:3,i])) #sum of original popln.
      assign(paste("m_",i-1,sep=""),B%*%(dat[1:3,i])) #popln. after growth cycle
      assign(paste("ms_",i-1,sep=""),sum(B%*%(dat[1:3,i]))) #sum of popln. after growth cycle
      #assign(paste("p_",i-1,sep=""),B%*%(dat[1:3,i])) #update population no.s
      l <- l + (A[j,i-1]*(B%*%(dat[1:3,i])))
    } 
    assign(paste("d_",j,sep=""),l) #popln. post dispersal
    assign(paste("ds_",j,sep=""),sum(l)) #sum of popln. post dispersal
    }
    
    ps <- c(ps_1,ps_2,ps_3,ps_4,ps_5,ps_6,ps_7,ps_8,ps_9,ps_10,ps_11)
    ms <- c(ms_1,ms_2,ms_3,ms_4,ms_5,ms_6,ms_7,ms_8,ms_9,ms_10,ms_11)
    ds <- c(ds_1,ds_2,ds_3,ds_4,ds_5,ds_6,ds_7,ds_8,ds_9,ds_10,ds_11)
    
    x<-data.frame(d_1,d_2,d_3,d_4,d_5,d_6,d_7,d_8,d_9,d_10,d_11)
    colnames(x)<-val
    write.table(x, file = "pop.csv", sep = ",", col.names = NA, qmethod = "double")
    
    
    print((t-1)*len+w)
    PS[(t-1)*len+w,]<-ps; MS[(t-1)*len+w,]<-ms;DS[(t-1)*len+w,]<-ds  
    #print(ps)
    ns<-ps
    ns<-30*ns/max(ns)
    node.size<-setNames(ns,val)
    #plot(df,layout=l,vertex.label=V(df)$names,
    #edge.arrow.size=0.01,vertex.label.color = "black",vertex.size=node.size)
    
    #mypath=file.path("C:", "Users", "Nipun","Desktop","Images", paste("myplot_", w, ".jpeg", sep="")
    mypath <- file.path("C:","Users","Nipun","Desktop","Images",paste("plot_", (t-1)*len+ w, ".jpg", sep = ""))
    #print((t-1)*len+w)
    print(paste("myplot_", (t-1)*len+ w, ".jpg", sep = ""))
    jpeg(file=mypath)                  
    plot.igraph(g,vertex.size=node.size)
    dev.off()
  }
  
  deg_B <- degree(g) 
  #betw_B <- betweenness(g)
  #g delete_vertices or g-c("PM")
  assign(paste("del_",t,sep=""),order(deg_B,decreasing=T)[n])
  g<-(delete_edges(g,incident(g,order(deg_B,decreasing=T)[n])))
  A<-as.matrix(as_adjacency_matrix(g))
}

#del <- c(del_1,del_2,del_3)
del <- c(del_1)
colnames(PS)<-val;colnames(MS)<-val;colnames(DS)<-val
#data<-data.frame(del, PS, MS, DS)

##

mylist<-list(del, PS, MS, DS)
names(mylist)<-c("del","PS","MS","DS")
sink("data.csv", type="output")
invisible(lapply(names(mylist), 
                 function(x) { print(x)
                   dput(write.csv(mylist[[x]])) } ))
sink()


