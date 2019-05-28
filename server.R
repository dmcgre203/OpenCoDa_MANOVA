library(shiny)

server <- function(input, output) {
  
  require(ggplot2)
  require(ggtern)  
  require(compositions)
  require(reshape)
  require(lpSolve)
  
  # Setup reactive data
  dat                              <- reactive({
    
    # If missing input, return to avoid error later in function
    if(is.null(input$file1)) return()
    
    read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote,
             dec = input$deci)
  })
  
  output$choose_response           <- renderUI({ 
    if(is.null(dat())) return()
    df        <- dat()
    col_names <- colnames(df)
    
    radioButtons("model_response",
                 "Identify grouping variable",
                 choices=col_names,
                 selected = col_names[1])
    
  })
  output$choose_responseCats       <- renderUI({ 
    
    options   <- c("Uniform quantiles", "Specified thresholds","Categories")
    radioButtons("responseCatType",
                 "Method for categorising responses",
                 choices=options,
                 selected = options[1])
    
  }) 
  responseDat                      <- reactive({
    # If missing input, return to avoid error later in function
    if(is.null(dat())) return()
    if(is.null(input$model_response)) return()
    
    # Read file
    Y <- dat()[,input$model_response]
    Y
    
  })
  output$responseCats_numQuantiles <- renderUI({
    if(is.null(dat()))                      return()
    if(is.null(input$model_response))       return()
    if(is.null(responseDat()))              return() 
    if(is.null(input$responseCatType))      return() 
    if(input$responseCatType=="Categories") return()
    
    K <- max(min(floor(length(responseDat())/2),8),2)
    sliderInput("numqs", label = h3("Number of intervals"), 
                min = 2, max = K, value = 4)
    
  })  
  output$responseCats_thresholds   <- renderUI({
    if(is.null(input$model_response))                  return()
    if(is.null(responseDat()))                         return() 
    if(is.null(input$responseCatType))                 return() 
    
    if((input$responseCatType=="Uniform quantiles")|(input$responseCatType=="Categories")) {    
      return()
    } else {
      K    <- input$numqs
      minY <- min(responseDat())
      maxY <- max(responseDat())
      qv   <- seq(K-1)/K
      qs   <- quantile(responseDat(),qv)
      lapply(seq((K-1)),function(i){
        numericInput(paste0("threshold_",i), paste("Threshold",i), min=minY, max=maxY, val=qs[i])
      }) 
    }
    
  })
  response_qs                      <- reactive({
    if(is.null(input$model_response))       return()
    if(is.null(responseDat()))              return()    
    if(is.null(input$responseCatType))      return() 
    if(input$responseCatType=="Categories") return()
    
    K    <- input$numqs
    
    if(input$responseCatType=="Uniform quantiles") {
      qv   <- seq(K-1)/K
      quantile(responseDat(),qv)
    } else{
      qs <- lapply(seq(K-1), function(i) {eval(parse(text=paste0("input$threshold_",i)))})
      unlist(qs)    
    }
  })  
  responseGroups                   <- reactive({
    if(is.null(input$model_response)) return()
    if(is.null(responseDat()))        return() 
    if((is.null(response_qs()))&(input$responseCatType!="Categories"))  return()
    
    if(input$responseCatType=="Categories"){
      Group <- as.factor(responseDat())
    } else{
      Group <- as.factor(catvar2(responseDat(),response_qs()))
    }
    Group
  })
  fullData                         <- reactive({
    if(is.null(dat()))                return()
    if(is.null(responseGroups()))     return()
    data.frame(dat(),Group=responseGroups())  
  })
  output$choose_covariates         <- renderUI({ 
    if(is.null(dat())) return()
    df        <- dat()
    col_names <- colnames(df)
    checkboxGroupInput("model_covars", 
                       "Choose continuous covariates",
                       choices = col_names,
                       selected = NULL)
  })
  output$choose_cofactors          <- renderUI({ 
    if(is.null(dat())) return()
    df        <- dat()
    col_names <- colnames(df)
    checkboxGroupInput("model_cofactors", 
                       "Choose categorical covariates",
                       choices = col_names,
                       selected = NULL)
  })

  output$rawData_Header            <- renderText({
    if(is.null(dat())) return()
    "Raw Data"
  })
  output$rawDataSummary_Header     <- renderText({
    if(is.null(dat())) return()
    "Raw Data Summary Table"
  })
  output$contents                  <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    df <- dat()
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  output$rawSummary                <- renderTable({
    if(is.null(dat())) return()
    
    df   <- dat()
    out1 <- do.call(cbind, lapply(df, summary))
    out  <- data.frame(rownames(out1),out1)
    colnames(out)[1] <- "Statistic"
    out
    
  })  
  output$histogram1                <- renderPlot({
    if(is.null(input$file1))                return()
    if(is.null(input$model_response))       return()
    if(is.null(responseDat()))              return() 
    if(is.null(response_qs()))              return()
    if(is.null(input$responseCatType))      return()
    if(input$responseCatType=="Categories") return()
    
    xtitle        <- input$model_response
    response_data <- data.frame(y=responseDat())
    breaks1       <- c(min(response_data),response_qs(),max(response_data))
    
    output <- ggplot(data=response_data, aes(y)) + 
      geom_histogram(breaks=breaks1, 
                     col="red", 
                     aes(fill=..count..)) +
      scale_fill_gradient("Count", low = "green", high = "red")+ 
      labs(title=paste("Histogram for",input$model_response), x=input$model_response, y="Count")
    
    print(output)
    
  })
  output$avgCODA_Title             <- renderText({
    if(is.null(input$file1))          return()
    if(is.null(input$model_response)) return()
    if(is.null(responseDat()))        return()
    if(is.null(input$model_CODAvars))  return()
    if(length(input$model_CODAvars)<3) return()
    "Geometric/CODA Averages by group (and overall)"
  }) 
  output$avgCODA                   <- renderTable({
    if(is.null(input$file1))           return()
    if(is.null(input$model_response))  return()
    if(is.null(responseDat()))         return()
    if(is.null(input$model_CODAvars))  return()
    if(length(input$model_CODAvars)<3) return()    
    if(is.null(responseGroups()))      return()
    if(is.null(fullData()))            return()
    
    df   <- fullData()
    Grps <- levels(responseGroups())
    K    <- length(Grps)
    
    gms <- lapply(seq(K), function(i){
      apply(df[df$Group==Grps[i],input$model_CODAvars],2,gm)
    })
    
    df_gm  <- as.data.frame(do.call(rbind, gms))
    df_gm  <- df_gm/rowSums(df_gm)
    gmall  <- apply(df[,input$model_CODAvars],2,gm)
    gmall  <- gmall/sum(gmall)
    
    geo_avgs     <- data.frame(rbind(df_gm,gmall))
    df_grouped   <- data.frame(Group=c(Grps,"all"),geo_avgs)
    df_grouped
    
  },digits=4)
  output$avgStandard_Title         <- renderText({
    if(is.null(input$file1))          return()
    if(is.null(input$model_response)) return()
    if(is.null(responseDat()))        return()
    if(is.null(input$model_CODAvars))  return()
    if(length(input$model_CODAvars)<3) return()
    "Arithmetic/Standard Averages by group (and overall)"
  }) 
  output$avgStandard               <- renderTable({
    if(is.null(input$file1))           return()
    if(is.null(input$model_response))  return()
    if(is.null(responseDat()))         return()
    if(is.null(input$model_CODAvars))  return()
    if(length(input$model_CODAvars)<3) return()    
    if(is.null(responseGroups()))      return()
    if(is.null(fullData()))            return()
    
    df   <- fullData()
    Grps <- levels(responseGroups())
    K    <- length(Grps)
    
    ams <- lapply(seq(K), function(i){
      apply(df[df$Group==Grps[i],input$model_CODAvars],2,mean)
    })
    
    df_am  <- as.data.frame(do.call(rbind, ams))
    amall  <- apply(df[,input$model_CODAvars],2,mean)
    
    ari_avgs     <- data.frame(rbind(df_am,amall))
    df_grouped   <- data.frame(Group=c(Grps,"all"),ari_avgs)
    df_grouped
    
  },digits=4)
  output$choose_group_VarMat       <- renderUI({ 
    if(is.null(input$file1))          return()
    if(is.null(input$model_response)) return()
    if(is.null(responseDat()))        return()
    if(is.null(input$model_CODAvars))  return()
    if(length(input$model_CODAvars)<3) return()
    if(is.null(responseGroups()))        return()
    
    Grps <- levels(responseGroups())
    
    col_names <- c("All",Grps)
    
    radioButtons("group_VarMat","Identify response",
                 choices=col_names,selected = col_names[1])
    
  })
  output$VariationMatrix_Title     <- renderText({
    if(is.null(input$file1))           return()
    if(is.null(input$model_response))  return()
    if(is.null(responseDat()))         return()
    if(is.null(input$model_CODAvars))  return()
    if(length(input$model_CODAvars)<3) return()
    if(is.null(input$group_VarMat))    return()

    if(input$group_VarMat=="All") {
      text1   <- "Overall Variation Matrix"
    } else {
      text1   <- paste0("Variation Matrix for Group ", input$group_VarMat)
    }
    
    text1
  })   
  output$VariationMatrix           <- renderTable({
    if(is.null(input$file1))           return()
    if(is.null(input$model_response))  return()
    if(is.null(responseDat()))         return() 
    if(is.null(input$model_CODAvars))  return()
    if(length(input$model_CODAvars)<3) return()
    if(is.null(input$group_VarMat))    return()
    if(is.null(responseGroups()))      return()
    
    df <- fullData()
    
    if(input$group_VarMat=="All") {
        df   <- df[,input$model_CODAvars]
    } else {
        df   <- df[df$Group==input$group_VarMat,input$model_CODAvars]
    }

    Cmat    <- acomp(df)
    VarMat  <- variation(Cmat)
    row_nam <- as.data.frame(rownames(VarMat))
    
    V       <- data.frame(row_nam,VarMat)
    colnames(V)[1] <- "_"
    
    V
  },digits=4)   
  output$groupPlots_Title          <- renderText({
    if(is.null(input$file1))           return()
    if(is.null(input$model_response))  return()
    if(is.null(responseDat()))         return()
    if(is.null(input$model_CODAvars))  return()
    if(length(input$model_CODAvars)<3) return()
    "Comparison of composition by response category"
  })    
  output$groupPlots <- renderPlot({
    if(is.null(input$file1))           return()
    if(is.null(input$model_response))  return()
    if(is.null(responseDat()))         return()
    if(is.null(input$model_CODAvars))  return()
    if(length(input$model_CODAvars)<3) return()
    if(is.null(responseGroups()))      return()
    
    df   <- fullData()
    Grps <- levels(responseGroups())
    K    <- length(Grps)
    
    gms <- lapply(seq(K), function(i){
      apply(df[df$Group==Grps[i],input$model_CODAvars],2,gm)
    })
    
    df_gm  <- as.data.frame(do.call(rbind, gms))
    df_gm  <- df_gm/rowSums(df_gm)
    
    gmall     <- apply(df[,input$model_CODAvars],2,gm)
    gmall     <- gmall/sum(gmall)
    gmall_rep <- matrix(rep(gmall,K),nrow=K,byrow=T)
    df_lgm    <- log(df_gm/gmall_rep)
    
    df_grouped   <- data.frame(Group=Grps,df_lgm)
    melted       <- melt(df_grouped,id.vars="Group")
    colnames(melted)[2] <-"Behaviour"
    
    ggplot(melted, aes(fill=Behaviour, y=value, x=Group)) + 
      geom_bar(position="dodge", stat="identity") +
      ylab("log (group mean / overall mean)")
    
  })

  output$choose_CODA <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(dat())) return()
    
    # Read file
    df <- dat()
    
    # Get the data set with the appropriate name
    col_names <- colnames(df)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("model_CODAvars", "Choose physical activity compositional variables",
                       choices = col_names,
                       selected = NULL)
  })
  scale_CODAdat <- reactive({
    
    if(is.null(dat())) return() 
    if(is.null(input$model_CODAvars)) return() 
    
    df      <- dat()    
    df.CODA <- data.frame(df[,input$model_CODAvars])
    totals  <- rowSums(df.CODA)
    df.CODA/totals
  })
  CODAdat <- reactive({
    
    if(is.null(dat())) return()  
    
    df      <- dat()
    df.CODA <- data.frame(df[,input$model_CODAvars])
    
    if(length(df.CODA)<3) return()  
    library(robCompositions)
    
    if(length(df.CODA)==3) {
      z1      <- pivotCoord(df.CODA,pivotvar=1)
      z2      <- pivotCoord(df.CODA,pivotvar=2)
      z3      <- pivotCoord(df.CODA,pivotvar=3)
      
      zz      <- cbind(z1,z2,z3)
      zz      <- data.frame(zz)
    }
    
    if(length(df.CODA)==4) {
      z1      <- pivotCoord(df.CODA,pivotvar=1)
      colnames(z1) <- gsub("-", ".", colnames(z1))
      z11     <- pivotCoord(df.CODA[,-1],pivotvar=1)
      z12     <- pivotCoord(df.CODA[,-1],pivotvar=2)
      z13     <- pivotCoord(df.CODA[,-1],pivotvar=3)
      z2      <- pivotCoord(df.CODA,pivotvar=2)
      colnames(z2) <- gsub("-", ".", colnames(z2))
      z21     <- pivotCoord(df.CODA[,-2],pivotvar=1)
      z22     <- pivotCoord(df.CODA[,-2],pivotvar=2)
      z23     <- pivotCoord(df.CODA[,-2],pivotvar=3)
      z3      <- pivotCoord(df.CODA,pivotvar=3)
      colnames(z3) <- gsub("-", ".", colnames(z3))
      z31     <- pivotCoord(df.CODA[,-3],pivotvar=1)
      z32     <- pivotCoord(df.CODA[,-3],pivotvar=2)
      z33     <- pivotCoord(df.CODA[,-3],pivotvar=3)
      z4      <- pivotCoord(df.CODA,pivotvar=4)
      colnames(z4) <- gsub("-", ".", colnames(z4))
      z41     <- pivotCoord(df.CODA[,-4],pivotvar=1)
      z42     <- pivotCoord(df.CODA[,-4],pivotvar=2)
      z43     <- pivotCoord(df.CODA[,-4],pivotvar=3)
      
      #      zz      <- cbind(z1,z2,z3,z4)
      zzA      <- cbind(z1[,1],z11,z12,z13,z2[,1],z21,z22,z23,
                        z3[,1],z31,z32,z33,z4[,1],z41,z42,z43)
      zzB      <- data.frame(zzA)
      colnames(zzB)[c(1,8,15,22)] <- c(colnames(z1)[1],colnames(z2)[1],
                                       colnames(z3)[1],colnames(z4)[1])
      zz <- zzB
    }
    
    zz
  })
  output$ilrCoords <- renderTable({
    
    if(is.null(dat())) return()     
    if(is.null(CODAdat())) return()
    
    CODAdat.display <- data.frame(dat()[,1],CODAdat())
    colnames(CODAdat.display)[1] <- colnames(dat())[1]
    
    if(input$disp == "head") {
      return(head(CODAdat.display))
    }
    else {
      return(CODAdat.display)
    }
    
  }) 
  output$choose_ilr <- renderUI({ 
    
    # If missing input, return to avoid error later in function
    if(is.null(CODAdat())) return()
    
    # Get the data set with the appropriate name
    col_names2 <- colnames(CODAdat())
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("model_ilr_coords", "Choose ilr coordinates",
                       choices = col_names2,
                       selected = NULL)
  })  
  FullCODAdat <- reactive({
    
    if(is.null(dat())) return()  
    if(is.null(CODAdat())) return()
    
    data.frame(dat(),CODAdat(),Group=responseGroups())
    
  })  
  
  # fit linear model
  runMANOVA <- reactive({
    
    if(is.null(dat())) return()  
    if(is.null(FullCODAdat())) return() 
    if(is.null(input$model_response)) return()
    if(is.null(input$model_ilr_coords)) return()
    
    cofacs1 <- paste("as.factor(",input$model_cofactors,")") 
    cofacs2 <- paste(cofacs1,collapse="+")
    covars1 <- paste(input$model_covars, collapse="+") 
    covars2 <- paste(c(covars1,cofacs2),collapse="+")
    coda1   <- paste0("cbind(",paste0(input$model_ilr_coords,collapse=","),")")
    grp     <- "Group" 
    
    if(is.null(input$model_cofactors) && is.null(input$model_covars)) {
      coda2   <- grp
    } else if(is.null(input$model_cofactors)) {
      coda2   <- paste(c(covars1,grp),collapse="+")
    } else if(is.null(input$model_covars)) {
      coda2   <- paste(c(cofacs1,grp),collapse="+")
    } else {
      coda2   <- paste(c(covars2,grp),collapse="+")
    }
    
    if (length(input$model_ilr_coords)==1){
        aov(as.formula(paste0(coda1," ~ ",coda2)),data=FullCODAdat())  
    } else {
        manova(as.formula(paste0(coda1," ~ ",coda2)),data=FullCODAdat())
    }
    
  })
  
  # Show cox regression summary for selecting covariates
  output$MANOVASummary <- renderPrint({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(FullCODAdat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    if(is.null(runMANOVA())) return() 
    
    summary(runMANOVA())
    
  })
  
  # Show cox regression summary for selecting covariates
  output$MANOVASummaryAOV <- renderPrint({
    
    if(is.null(input$file1)) return()
    if(is.null(dat())) return() 
    if(is.null(FullCODAdat())) return() 
    if(is.null(input$model_ilr_coords)) return()
    
    if (length(input$model_ilr_coords)==1) return() 

    summary.aov(runMANOVA())
    
  })
  
  # Setup UI for selecting compositional variables
  output$choose_ternVar <- renderUI({ 
    
    if(is.null(input$model_CODAvars)) return()
    if(length(input$model_CODAvars)<3) return()
    
    # Get the data set with the appropriate name
    col_names <- input$model_CODAvars
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("ternVars", "Choose variables to use in ternary plot",
                       choices = col_names,
                       selected = NULL)
  })
  
  # Setup UI for selecting groups
  output$choose_ternGroups <- renderUI({ 
    
    if(is.null(input$model_CODAvars))  return()
    if(length(input$model_CODAvars)<3) return()
    if(is.null(responseGroups()))      return()
    
    # Get the data set with the appropriate name
    Grps <- levels(responseGroups())
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("ternGroups", "Choose groups to include on ternary plot",
                       choices = Grps,
                       selected = Grps)
  })  
  
  output$ternPlot <- renderPlot({
    if(is.null(input$file1))           return()
    if(is.null(input$model_response))  return()
    if(is.null(responseDat()))         return()
    if(is.null(input$model_CODAvars))  return()
    if(length(input$model_CODAvars)<3) return()
    if(length(input$ternVars)!=3)      return()
    if(is.null(input$ternGroups))      return()
    
    Group <- responseGroups()
    df    <- dat()[,input$ternVars]
    colnames(df) <- c("x","y","z")
    norm  <- 1/rowSums(df)
    df    <- norm * df
    df    <- data.frame(df,Group)
    
    a     <- ceiling(max(df[,1])*10)/10
    b     <- ceiling(max(df[,2])*10)/10
    c     <- ceiling(max(df[,3])*10)/10
    d     <- floor(min(df[,1])*10)/10
    e     <- floor(min(df[,2])*10)/10
    f     <- floor(min(df[,3])*10)/10
    
    # Setup problem: minimize
    # tmax + lmax + rmax - tmin - lmin - rmin  subj. to
    #
    # tmax <= 1
    # lmax <= 1
    # rmax <= 1
    # tmin >= 0
    # lmin >= 0
    # rmin >= 0
    # tmax >= a
    # lmax >= b
    # rmax >= c
    # tmin <= d
    # lmin <= e
    # rmin <= f
    # tmax + lmin + rmin = 1
    # tmin + lmax + rmin = 1
    # tmin + lmin + rmax = 1
    f.obj <- c(1, 1, 1,-1,-1,-1)
    f.con <- matrix (c(1, 0, 0, 0, 0, 0,
                       0, 1, 0, 0, 0, 0,
                       0, 0, 1, 0, 0, 0,
                       0, 0, 0, 1, 0, 0,
                       0, 0, 0, 0, 1, 0,
                       0, 0, 0, 0, 0, 1,
                       1, 0, 0, 0, 0, 0,
                       0, 1, 0, 0, 0, 0,
                       0, 0, 1, 0, 0, 0,
                       0, 0, 0, 1, 0, 0,
                       0, 0, 0, 0, 1, 0,
                       0, 0, 0, 0, 0, 1,
                       1, 0, 0, 0, 1, 1,
                       0, 1, 0, 1, 0, 1,
                       0, 0, 1, 1, 1, 0), nrow=15, byrow=TRUE)
    f.dir <- c("<=", "<=", "<=", 
               ">=", ">=", ">=",
               ">=", ">=", ">=",
               "<=", "<=", "<=",
               "==", "==", "==")
    f.rhs <- c(1, 1, 1, 0, 0, 0, a, b, c, d, e, f, 1, 1, 1)
    lp_test<-lp ("min", f.obj, f.con, f.dir, f.rhs)
    limitssol <- lp_test$solution
    
    included <- which(Group %in% input$ternGroups)
    
    df <- df[included,]
    
    plot <- ggtern(data = df, aes(x=x, y=y, z=z)) + 
      geom_point(aes(fill = Group),
                 size = 4, 
                 shape = 21, 
                 color = "black") + 
      tern_limit(T=limitssol[1],L=limitssol[2],R=limitssol[3]) +
      labs(x = input$ternVars[2],y=input$ternVars[1],z=input$ternVars[3])
    
    print(plot)
    
  })
  
  catvar <- function(xvec,qvec=c(0.333333,0.666667)) {
    
    qs  <- quantile(xvec,qvec,na.rm=T)
    
    catvar <- 0 * xvec
    catvar <- catvar + 1*(xvec  < qs[1])
    catvar <- catvar + (length(qs)+1)*(xvec > qs[length(qs)])
    
    for (i in 2:(length(qs))) {
      catvar <- catvar + i *( (xvec >= qs[i-1])&(xvec < qs[i]) ) 
    }
    
    catvar[which(is.na(xvec))] <- 0
    catvar <- as.factor(catvar)
    
    return(catvar)
    
  }
  
  catvar2 <- function(xvec,qs) {
    
    catvar <- 0 * xvec
    catvar <- catvar + 1*(xvec  <= qs[1])
    catvar <- catvar + (length(qs)+1)*(xvec > qs[length(qs)])

    if(length(qs)>1) {
      for (i in 2:(length(qs))) {
        catvar <- catvar + i *( (xvec > qs[i-1])&(xvec <= qs[i]) ) 
      }
    }
      
    catvar[which(is.na(xvec))] <- 0
    catvar <- as.factor(catvar)
    
    return(catvar)
    
  }
  
  #calculate geometric average
  gm <- function(x) {
    exp(mean(log(x)))
  }
  
  # End server  
}


