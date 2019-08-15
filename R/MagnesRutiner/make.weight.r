#source("/nr/project/stat/Smitte/Lakselus_stad/Rfunc-magne/make.weight.new.r")
make.weight <- function(antall0,datadir.lok){
  #moving fish from one cage to another
  #We have an array of dimension 3.
  #First dimension is time
  #Second dimension is the cage we move from and
  #third dimension is the dimension we move to.
  #cage 0 means fish that are slaugtered, while cage 1 are fish that are "utsett"

  
###  if (datadir.lok!="2009-2010") {
  if (datadir.lok!="Langskjaera0910") {
    stop("make.weight ikke generell ennå")
  }
###  if(datadir.lok=="2009-2010"){
  if(datadir.lok=="Langskjaera0910"){
    ii<-0
    for(i in c(2,3,4,6,7,8,9,10)){
      ii<-ii+1
      colnames(antall0)[ii]<-paste("Antall",i,sep="")
      }
    row.names0 <- rownames(antall0)
    row.names <- c(paste(as.numeric(row.names0[1])-1,sep=""),row.names0)
    n <- length(antall0[,1])
    antall <- matrix(0,ncol=16,nrow=nrow(antall0)+1, dimnames=list(row.names,c(paste("Antall1.",c(2,3,4,6,7,8,9,10),sep=""),paste("Antall",c(2,3,4,6,7,8,9,10),sep=""))))
    for(i in c(2,3,4,6,7,8,9,10)){
      col1 <- paste("Antall",i,sep="")
      antall[row.names0,col1] <- c(antall0[,col1])
    }
    antall["20090504",c("Antall1.3")] <- 360500+3871 #3871 er lagt til for at det ikke blir feil ved flytting den 20091106
    antall["20090511",c("Antall1.2")] <- c(424651)
    antall["20090521",c("Antall1.8")] <- c(422000)
    antall["20090505",c("Antall1.9")] <- c(404730)
           
    utsett.antall <- matrix(NA,ncol=2,nrow=4,dimnames=list(c("20090511","20090504","20090521","20090505"),c("Antall","Merd")))
    utsett.antall["20090504",] <- c(360500,3)
    utsett.antall["20090511",] <- c(424651,2)
    utsett.antall["20090521",] <- c(422000,8)
    utsett.antall["20090505",] <- c(404730,9)
 
    
    weight2 <- matrix(0,ncol=length(c(0,1,2,3,4,6,7,8,9,10)),nrow=dim(antall0)[1]+1, dimnames=list(row.names,paste(c(0,1,2,3,4,6,7,8,9,10))))
    weight2 <- weight2[-(n+1),]
    weight3 <- matrix(0,ncol=length(c(0,1,2,3,4,6,7,8,9,10)),nrow=dim(antall0)[1]+1, dimnames=list(row.names,paste(c(0,1,2,3,4,6,7,8,9,10))))
    weight3 <- weight3[-(n+1),]
    weight4 <- matrix(0,ncol=length(c(0,1,2,3,4,6,7,8,9,10)),nrow=dim(antall0)[1]+1, dimnames=list(row.names,paste(c(0,1,2,3,4,6,7,8,9,10))))
    weight4 <- weight4[-(n+1),]
    weight6 <- matrix(0,ncol=length(c(0,1,2,3,4,6,7,8,9,10)),nrow=dim(antall0)[1]+1, dimnames=list(row.names,paste(c(0,1,2,3,4,6,7,8,9,10))))
    weight6 <- weight6[-(n+1),]
    weight7 <- matrix(0,ncol=length(c(0,1,2,3,4,6,7,8,9,10)),nrow=dim(antall0)[1]+1, dimnames=list(row.names,paste(c(0,1,2,3,4,6,7,8,9,10))))
    weight7 <- weight7[-(n+1),]
    weight8 <- matrix(0,ncol=length(c(0,1,2,3,4,6,7,8,9,10)),nrow=dim(antall0)[1]+1, dimnames=list(row.names,paste(c(0,1,2,3,4,6,7,8,9,10))))
    weight8 <- weight8[-(n+1),]
    weight9 <- matrix(0,ncol=length(c(0,1,2,3,4,6,7,8,9,10)),nrow=dim(antall0)[1]+1, dimnames=list(row.names,paste(c(0,1,2,3,4,6,7,8,9,10))))
    weight9 <- weight9[-(n+1),]
    weight10<- matrix(0,ncol=length(c(0,1,2,3,4,6,7,8,9,10)),nrow=dim(antall0)[1]+1,dimnames=list(row.names,paste(c(0,1,2,3,4,6,7,8,9,10))))
    weight10<- weight10[-(n+1),]

    row.names0 <- rownames(antall0[-n,])
    w <- c(antall0[-1,"Antall2"]/antall0[-n,"Antall2"])
    weight2["20090511","1"] <- 1
    weight2[row.names0,"2"] <- w
    weight2[row.names0,"0"] <- (1-w)
    
    w <- c(antall0[-1,"Antall2"]/antall0[-n,"Antall2"])
    weight2["20090511","1"] <- 1
    weight2[row.names0,"2"] <- w
    weight2[row.names0,"0"] <- (1-w)
    
    w <- c(antall0[-1,"Antall3"]/antall0[-n,"Antall3"])
    weight3["20090504","1"] <- 1
    weight3[row.names0,"3"] <- w
    weight3[row.names0,"0"] <- 1-w
    w <- c(antall0[-1,"Antall4"]/antall0[-n,"Antall4"])
    weight4[row.names0,"4"] <- w
    weight4[row.names0,"0"] <- 1-w
    w <- c(antall0[-1,"Antall6"]/antall0[-n,"Antall6"])
    weight6[row.names0,"6"] <- w
    weight6[row.names0,"0"] <- 1-w
    w <- c(antall0[-1,"Antall7"]/antall0[-n,"Antall7"])
    weight7[row.names0,"7"] <- w
    weight7[row.names0,"0"] <- 1-w
    w <- c(antall0[-1,"Antall8"]/antall0[-n,"Antall8"])
    weight8["20090521","1"] <- 1
    weight8[row.names0,"8"] <- w
    weight8[row.names0,"0"] <- 1-w
    w <- c(antall0[-1,"Antall9"]/antall0[-n,"Antall9"])
    weight9["20090505","1"] <- 1
    weight9[row.names0,"9"] <- w
    weight9[row.names0,"0"] <- 1-w
    w <- c(antall0[-1,"Antall10"]/antall0[-n,"Antall10"])
    weight10[row.names0,"10"] <- w
    weight10[row.names0,"0"] <- 1-w

    id <- antall0[,"Antall2"]==0
    weight2[id,"2"] <- 0
    id <- weight2==Inf | weight2==-Inf | is.na(weight2)
    weight2[id] <- 0  
  
    id <- antall0[,"Antall3"]==0
    weight3[id,"3"] <- 0
    id <- weight3==Inf | weight3==-Inf | is.na(weight3)
    weight3[id] <- 0

    id <- antall0[,"Antall4"]==0
    weight4[id,"4"] <- 0
    id <- weight4==Inf | weight4==-Inf | is.na(weight4)
    weight4[id] <- 0

    id <- antall0[,"Antall6"]==0
    weight6[id,"6"] <- 0
    id <- weight6==Inf | weight6==-Inf | is.na(weight6)
    weight6[id] <- 0

    id <- antall0[,"Antall7"]==0
    weight7[id,"7"] <- 0
    id <- weight7==Inf | weight7==-Inf | is.na(weight7)
    weight7[id] <- 0
    
    id <- antall0[,"Antall8"]==0
    weight8[id,"8"] <- 0
    id <- weight8==Inf | weight8==-Inf | is.na(weight8)
    weight8[id] <- 0

    id <- antall0[,"Antall9"]==0
    weight9[id,"9"] <- 0
    id <- weight9==Inf | weight9==-Inf | is.na(weight9)
    weight9[id] <- 0

    id <- antall0[,"Antall10"]==0
    weight10[id,"10"] <- 0
    id <- weight10==Inf | weight10==-Inf | is.na(weight10)
    weight10[id] <- 0


  
    #utsett
    weight2["20090511","1"] <- 1
    weight3["20090504","1"] <- 1
    weight8["20090521","1"] <- 1
    weight9["20090505","1"] <- 1
  
    
    q <- antall0["20100522","Antall7"]/antall0["20100521","Antall7"]
    weight7["20100521","7"] <- q
    weight7["20100521","10"] <- 1-q
    weight7["20100521","0"] <- 0
            
   
    q <- antall0["20100701","Antall8"]/antall0["20100630","Antall8"]
    q1 <- antall0["20100701","Antall2"]/((1-q)*antall0["20100630","Antall8"])
    weight8["20100630","8"] <- q
    weight8["20100630","2"] <- (1-q)*q1
    weight8["20100630","0"] <- (1-q)*(1-q1)
    

    
    q1 <- antall0["20100325","Antall2"]/(antall0["20100324","Antall2"])
    q3 <- antall0["20100325","Antall8"]/(antall0["20100324","Antall8"])
    q2 <- (antall0["20100325","Antall8"]-antall0["20100324","Antall8"])/((1-q1)*antall0["20100324","Antall2"]+1*antall0["20100324","Antall10"])
    q4 <- antall0["20100325","Antall8"]/((1-q1)*antall0["20100324","Antall2"]+antall0["20100324","Antall10"]+antall0["20100324","Antall8"])

    weight2["20100324","8"] <- (1-q1)*q4
    weight2["20100324","0"] <- (1-q1)*(1-q4)

    weight8["20100324","8"] <- q4
    weight8["20100324","0"] <- (1-q4)
 
    weight10["20100324","8"] <- q4
    q <- (antall0["20100325","Antall6"]-antall0["20100324","Antall6"])/((1-q4)*antall0["20100324","Antall10"])
    weight10["20100324","0"] <- (1-q4)*(1-q)
    weight10["20100324","6"] <- (1-q4)*q


    weight6["20100324","6"] <- 1
    weight6["20100324","0"] <- 0
    
 
    

    q <- antall0["20101110","Antall3"]/antall0["20101109","Antall3"]
    weight3["20101109","3"] <- q
    weight3["20101109","0"] <- (1-q)
    q <- antall0["20101111","Antall3"]/antall0["20101110","Antall3"]
    weight3["20101110","3"] <- q
    weight3["20101110","0"] <- (1-q)
    weight3["20101111","0"] <- 1

    q <- (antall0["20100819","Antall3"])/antall0["20100818","Antall3"]
    q1 <- (antall0["20100819","Antall9"])/antall0["20100818","Antall9"]
    
    q2 <- antall0["20100819","Antall6"]/((1-q)*antall0["20100818","Antall3"]+(1-q1)*antall0["20100818","Antall9"])
    weight3["20100818","3"] <- q
    weight3["20100818","6"] <- (1-q)*q2
    weight3["20100818","0"] <- (1-q)*(1-q2)

    weight9["20100818","9"] <- q1
    weight9["20100818","6"] <- (1-q1)*q2
    weight9["20100818","0"] <- (1-q1)*(1-q2)


    q <- antall0["20100812","Antall4"]/antall0["20100811","Antall4"]
    weight4["20100811","4"] <- q
    weight4["20100811","0"] <- (1-q)

    q <- antall0["20100813","Antall4"]/antall0["20100812","Antall4"]
    weight4["20100812","4"] <- q
    weight4["20100812","0"] <- (1-q)

  
    q <- antall0["20100914","Antall4"]/antall0["20100913","Antall4"]
    weight4["20100913","4"] <- q
    weight4["20100913","0"] <- (1-q)
    q <- antall0["20100915","Antall4"]/antall0["20100914","Antall4"]
    weight4["20100914","4"] <- q
    weight4["20100914","0"] <- (1-q)
    q <- antall0["20100916","Antall4"]/antall0["20100915","Antall4"]
    weight4["20100915","4"] <- q
    weight4["20100915","0"] <- (1-q)
    weight4["20100916","0"] <- 1


    q <- antall0["20100813","Antall6"]/antall0["20100812","Antall6"]
    weight6["20100812","6"] <- q
    weight6["20100812","0"] <- (1-q)
    q <- antall0["20100816","Antall6"]/antall0["20100815","Antall6"]
    weight6["20100815","6"] <- q
    weight6["20100815","0"] <- (1-q)
    q <- antall0["20100817","Antall6"]/antall0["20100816","Antall6"]
    weight6["20100816","6"] <- q
    weight6["20100816","0"] <- (1-q)

    weight6["20100817","0"] <- 1


    q <- antall0["20100907","Antall7"]/antall0["20100906","Antall7"]
    weight7["20100906","7"] <- q
    weight7["20100906","0"] <- (1-q)
    
    q <- antall0["20100908","Antall7"]/antall0["20100907","Antall7"]
    weight7["20100907","7"] <- q
    weight7["20100907","0"] <- (1-q)
    weight7["20100908","0"] <- 1

    q <- antall0["20100907","Antall7"]/antall0["20100906","Antall7"]
    weight7["20100906","7"] <- q
    weight7["20100906","0"] <- (1-q)
    

    qq22 <- antall0["20091124","Antall2"]/antall0["20091123","Antall2"]
    qq88 <- antall0["20091124","Antall8"]/(antall0["20091123","Antall8"])    

    weight2["20091123","2"] <- qq22         # 166494 antall fisk i Merd2
    weight2["20091123","3"] <- (1-qq22)*0.5 # 120385 antall fisk flyttet fra merd2 til merd3
    weight2["20091123","4"] <- (1-qq22)*0.5 # 120385 antall fisk flyttet fra merd2 til merd4
    weight2["20091123","0"] <- 0

    q4 <- (antall0["20091124","Antall4"]-(1-qq22)*0.5*antall0["20091123","Antall2"])/((1-qq88)*antall0["20091123","Antall8"])
    q3 <- (antall0["20091124","Antall3"]-(1-qq22)*0.5*antall0["20091123","Antall2"])/((1-qq88)*antall0["20091123","Antall8"])
    
    weight8["20091123","8"] <- qq88               # 168859 antall fisk flyttet fra merd8 til merd8
    weight8["20091123","3"] <- (1-qq88)*q3        # 121013 antall fisk flyttet fra merd8 til merd3
    weight8["20091123","4"] <- (1-qq88)*q4        # 42489  antall fisk flyttet fra merd8 til merd3
    weight8["20091123","0"] <- (1-qq88)*(1-q3-q4) # 3044   antall fisk flyttet fra merd8 til merd0

  
    q <-  antall0["20091107","Antall9"]/antall0["20091106","Antall9"]
    q1 <- antall0["20091107","Antall10"]/((1-q)*antall0["20091106","Antall9"])
    weight9["20091106","9"]  <- q
    weight9["20091106","10"] <- (1-q)*q1
    weight9["20091106","6"]  <- (1-q)*(1-q1)
    weight9["20091106","0"]  <-  0


    q0 <- antall0["20091107","Antall7"]/antall0["20091106","Antall3"]
    weight3["20091106","7"] <- q0
    weight3["20091106","6"] <- (1-q0)
    weight3["20091106","0"] <- 0

    row.names.w <- rownames(weight2)
    
    
    w <- array(0,c(length(weight2[,1])+1,8+1,8+1))
    n <- length(antall0[,1])
    row.names0 <- rownames(antall0)
    row.names <- c(paste(as.numeric(row.names0[1])-1,sep=""),row.names0)

    dimnames(w) <- list(row.names,paste("Merd",c(1,2:4,6:10),sep=""),paste("Merd",c(0,2:4,6:10),sep=""))
    row.names.w <- dimnames(weight2)[[1]]
    r.n.w <- rownames(w)[is.element(rownames(w),row.names.w)]
    w[r.n.w,"Merd1","Merd2"]  <- c(weight2[r.n.w,paste(1,sep="")])
    w[r.n.w,"Merd2","Merd2"]  <- c(weight2[r.n.w,paste(2,sep="")])
    w[r.n.w,"Merd2","Merd0"]  <- c(weight2[r.n.w,paste(0,sep="")])
    w[r.n.w,"Merd3","Merd2"]  <- c(weight3[r.n.w,paste(2,sep="")])
    w[r.n.w,"Merd4","Merd2"]  <- c(weight4[r.n.w,paste(2,sep="")])
    w[r.n.w,"Merd6","Merd2"]  <- c(weight6[r.n.w,paste(2,sep="")])
    w[r.n.w,"Merd7","Merd2"]  <- c(weight7[r.n.w,paste(2,sep="")])
    w[r.n.w,"Merd8","Merd2"]  <- c(weight8[r.n.w,paste(2,sep="")])
    w[r.n.w,"Merd9","Merd2"]  <- c(weight9[r.n.w,paste(2,sep="")])
    w[r.n.w,"Merd10","Merd2"]  <- c(weight10[r.n.w,paste(2,sep="")])

    w[r.n.w,"Merd1","Merd3"]  <- c(weight3[r.n.w,paste(1,sep="")])
    w[r.n.w,"Merd3","Merd3"]  <- c(weight3[r.n.w,paste(3,sep="")])
    w[r.n.w,"Merd3","Merd0"]  <- c(weight3[r.n.w,paste(0,sep="")])
    w[r.n.w,"Merd2","Merd3"]  <- c(weight2[r.n.w,paste(3,sep="")])
    w[r.n.w,"Merd4","Merd3"]  <- c(weight4[r.n.w,paste(3,sep="")])
    w[r.n.w,"Merd6","Merd3"]  <- c(weight6[r.n.w,paste(3,sep="")])
    w[r.n.w,"Merd7","Merd3"]  <- c(weight7[r.n.w,paste(3,sep="")])
    w[r.n.w,"Merd8","Merd3"]  <- c(weight8[r.n.w,paste(3,sep="")])
    w[r.n.w,"Merd9","Merd3"]  <- c(weight9[r.n.w,paste(3,sep="")])
    w[r.n.w,"Merd10","Merd3"] <- c(weight10[r.n.w,paste(3,sep="")])

    w[r.n.w,"Merd1","Merd4"]  <- c(weight4[r.n.w,paste(1,sep="")])
    w[r.n.w,"Merd4","Merd4"]  <- c(weight4[r.n.w,paste(4,sep="")])
    w[r.n.w,"Merd4","Merd0"]  <- c(weight4[r.n.w,paste(0,sep="")])
    w[r.n.w,"Merd2","Merd4"]  <- c(weight2[r.n.w,paste(4,sep="")])
    w[r.n.w,"Merd3","Merd4"]  <- c(weight3[r.n.w,paste(4,sep="")])
    w[r.n.w,"Merd6","Merd4"]  <- c(weight6[r.n.w,paste(4,sep="")])
    w[r.n.w,"Merd7","Merd4"]  <- c(weight7[r.n.w,paste(4,sep="")])
    w[r.n.w,"Merd8","Merd4"]  <- c(weight8[r.n.w,paste(4,sep="")])
    w[r.n.w,"Merd9","Merd4"]  <- c(weight9[r.n.w,paste(4,sep="")])
    w[r.n.w,"Merd10","Merd4"] <- c(weight10[r.n.w,paste(4,sep="")])

    w[r.n.w,"Merd1","Merd6"]  <- c(weight6[r.n.w,paste(1,sep="")])
    w[r.n.w,"Merd6","Merd6"]  <- c(weight6[r.n.w,paste(6,sep="")])
    w[r.n.w,"Merd6","Merd0"]  <- c(weight6[r.n.w,paste(0,sep="")])
    w[r.n.w,"Merd2","Merd6"]  <- c(weight2[r.n.w,paste(6,sep="")])
    w[r.n.w,"Merd3","Merd6"]  <- c(weight3[r.n.w,paste(6,sep="")])
    w[r.n.w,"Merd4","Merd6"]  <- c(weight4[r.n.w,paste(6,sep="")])
    w[r.n.w,"Merd7","Merd6"]  <- c(weight7[r.n.w,paste(6,sep="")])
    w[r.n.w,"Merd8","Merd6"]  <- c(weight8[r.n.w,paste(6,sep="")])
    w[r.n.w,"Merd9","Merd6"]  <- c(weight9[r.n.w,paste(6,sep="")])
    w[r.n.w,"Merd10","Merd6"] <- c(weight10[r.n.w,paste(6,sep="")])

    w[r.n.w,"Merd1","Merd7"]  <- c(weight7[r.n.w,paste(1,sep="")])
    w[r.n.w,"Merd7","Merd7"]  <- c(weight7[r.n.w,paste(7,sep="")])
    w[r.n.w,"Merd7","Merd0"]  <- c(weight7[r.n.w,paste(0,sep="")])
    w[r.n.w,"Merd2","Merd7"]  <- c(weight2[r.n.w,paste(7,sep="")])
    w[r.n.w,"Merd3","Merd7"]  <- c(weight3[r.n.w,paste(7,sep="")])
    w[r.n.w,"Merd4","Merd7"]  <- c(weight4[r.n.w,paste(7,sep="")])
    w[r.n.w,"Merd6","Merd7"]  <- c(weight6[r.n.w,paste(7,sep="")])
    w[r.n.w,"Merd8","Merd7"]  <- c(weight8[r.n.w,paste(7,sep="")])
    w[r.n.w,"Merd9","Merd7"]  <- c(weight9[r.n.w,paste(7,sep="")])
    w[r.n.w,"Merd10","Merd7"] <- c(weight10[r.n.w,paste(7,sep="")])

    w[r.n.w,"Merd1","Merd8"]  <- c(weight8[r.n.w,paste(1,sep="")])
    w[r.n.w,"Merd8","Merd8"]  <- c(weight8[r.n.w,paste(8,sep="")])
    w[r.n.w,"Merd8","Merd0"]  <- c(weight8[r.n.w,paste(0,sep="")])
    w[r.n.w,"Merd2","Merd8"]  <- c(weight2[r.n.w,paste(8,sep="")])
    w[r.n.w,"Merd3","Merd8"]  <- c(weight3[r.n.w,paste(8,sep="")])
    w[r.n.w,"Merd4","Merd8"]  <- c(weight4[r.n.w,paste(8,sep="")])
    w[r.n.w,"Merd6","Merd8"]  <- c(weight6[r.n.w,paste(8,sep="")])
    w[r.n.w,"Merd7","Merd8"]  <- c(weight7[r.n.w,paste(8,sep="")])
    w[r.n.w,"Merd9","Merd8"]  <- c(weight9[r.n.w,paste(8,sep="")])
    w[r.n.w,"Merd10","Merd8"] <- c(weight10[r.n.w,paste(8,sep="")])

    w[r.n.w,"Merd1","Merd9"]  <- c(weight9[r.n.w,paste(1,sep="")])
    w[r.n.w,"Merd9","Merd9"]  <- c(weight9[r.n.w,paste(9,sep="")])
    w[r.n.w,"Merd9","Merd0"]  <- c(weight9[r.n.w,paste(0,sep="")])
    w[r.n.w,"Merd2","Merd9"]  <- c(weight2[r.n.w,paste(9,sep="")])
    w[r.n.w,"Merd3","Merd9"]  <- c(weight3[r.n.w,paste(9,sep="")])
    w[r.n.w,"Merd4","Merd9"]  <- c(weight4[r.n.w,paste(9,sep="")])
    w[r.n.w,"Merd6","Merd9"]  <- c(weight6[r.n.w,paste(9,sep="")])
    w[r.n.w,"Merd7","Merd9"]  <- c(weight7[r.n.w,paste(9,sep="")])
    w[r.n.w,"Merd8","Merd9"]  <- c(weight8[r.n.w,paste(9,sep="")])
    w[r.n.w,"Merd10","Merd9"] <- c(weight10[r.n.w,paste(9,sep="")])

    w[r.n.w,"Merd1","Merd10"]  <- c(weight10[r.n.w,paste(1,sep="")])
    w[r.n.w,"Merd10","Merd10"] <- c(weight10[r.n.w,paste(10,sep="")])
    w[r.n.w,"Merd10","Merd0"]  <- c(weight10[r.n.w,paste(0,sep="")])
    w[r.n.w,"Merd2","Merd10"]  <- c(weight2[r.n.w,paste(10,sep="")])
    w[r.n.w,"Merd3","Merd10"]  <- c(weight3[r.n.w,paste(10,sep="")])
    w[r.n.w,"Merd4","Merd10"]  <- c(weight4[r.n.w,paste(10,sep="")])
    w[r.n.w,"Merd6","Merd10"]  <- c(weight6[r.n.w,paste(10,sep="")])
    w[r.n.w,"Merd7","Merd10"]  <- c(weight7[r.n.w,paste(10,sep="")])
    w[r.n.w,"Merd8","Merd10"]  <- c(weight8[r.n.w,paste(10,sep="")])
    w[r.n.w,"Merd9","Merd10"]  <- c(weight9[r.n.w,paste(10,sep="")])

    return(list(w=w,antall=antall))
  }
 }
