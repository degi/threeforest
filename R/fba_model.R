#compute the branches dimensions and return the list of branches segments
fba.branches <- function(
  initial_d = 10, 
  showProgress = FALSE,
  
  Intercept = 21.944,
  Slope = 8.462,
  RangeL = 0.200,
  
  LocalityTC = 0.001,
  ScaleTC = 0.0005,
  Thickening = 0,
  
  DMin = 0.8,
  RangeDmin = 0.5,
  AvNsub = 2.687150838,
  
  HowP = 0,
  
  LocalityP=1,
  ScaleP=0.1,
  
  Prob1P=0.0279329608938547,
  Prob2P=0.134078212290503,
  Prob3P=0.318435754189944,
  Prob4P=0.240223463687151,
  Prob5P=0.134078212290503,
  Prob6P=0.145251396648045,
  
  Prob1Q=0.206703910614525,
  Prob2Q=0.201117318435754,
  Prob3Q=0.162011173184358,
  Prob4Q=0.122905027932961,
  Prob5Q=0.150837988826816,
  
  BranchQ=NULL,
  
  InterceptTwig=22.657,
  SlopeTwig=1.4108,
  RangeLTwig=0.3,
  
  InterceptBranch=23,
  SlopeBranch=0,
  RangeLBranch=0.5,
  
  InterceptWood=120,
  SlopeWood=0,
  RangeLWood=0.8,
  
  InterceptStem=66,
  SlopeStem=20,
  RangeLStem=0.5,
  
  MaxDiamTwig=2,
  MaxDiamBranch=8,
  singleLD=0,
  
  LBaretip=0,
  Dmaxfin=1.533333333,
  Dzerofin=2.9,
  Maxfindens=0.499987708,
  
  #  1=Differentiate between Orthotropic and Plagiotropic branches (as, e.g., in coffee)
  OrthoPlagio = 0,
  #  1 = use a linear relationship between link diameter and length, 0 = use mean link lengths per diam class
  DLcontinuous = 1,
  Randseed=NULL
) {
  set.seed(Randseed)
  if(showProgress) pb <- txtProgressBar(style=3)
  
  ILatDmin <- Intercept + Slope * DMin
  NLow <- floor(AvNsub) 
  PNLow <- 1 + NLow - AvNsub
  
  #preparing the table for output
  branches <- data.frame( 
    "LinkNumber" = numeric(),
    "Length" = numeric(),
    "Diam1" = numeric(),
    "Diam2" = numeric(),
    "ConnectedTo" = numeric(),
    "FinStruc" = numeric(),
    "Largest" = numeric(),
    stringsAsFactors=FALSE)
  linkParent <- 0
  linkNumber <- 1
  
  LengthD0 <- (InterceptStem+SlopeStem*initial_d)*(1+RangeLStem*(runif(1)-0.5))
  LengthD0 <- max(LengthD0,0)
  
  #Tapering coefficient
  TapCoef <- dcauchy(runif(1), LocalityTC, ScaleTC) 
  if(Thickening == 0) TapCoef <- abs(TapCoef);
  Dprox <- initial_d/DMin
  Ddist <- Dprox - (TapCoef * LengthD0)
  if(Ddist < 0) Ddist = Dprox
  
  branches[linkNumber,"LinkNumber"] <- linkNumber
  branches[linkNumber,"Length"] <- LengthD0
  branches[linkNumber,"Diam1"] <- initial_d
  branches[linkNumber,"Diam2"] <- Ddist*DMin
  branches[linkNumber,"ConnectedTo"] <- 0
  branches[linkNumber,"FinStruc"] <- 0
  branches[linkNumber,"Largest"] <- 1
  
  #Initial stem diameter is based on tappered distal diameter 
  DDMin <- DMin*DMin; 
  D0 <- Ddist
  DDinit <- D0*D0
  DD <- c(DDinit)
  parentIDs <- c(1)
  
  i <- 1
  while(i <= length(DD)) {
    linkParent <- parentIDs[i]
    linkGrandParent <- max(1, branches[linkParent,"ConnectedTo"])
    linkBase <- nrow(branches)
    
    Q <- 1
    P <- 0
    
    #looped to make sure the sub branches smaller than their grand parent! (Meine 14/04/2014)
    DD_gparent <- (branches[linkGrandParent,"Diam2"]/DMin)^2
    DD_parent <- DD[i] 
    
    DD_limit <- max(DD_parent, DD_gparent)
    DD_child <- DD_limit+1
    while(DD_child > DD_limit) {
      if(HowP == 0) {
        RNumb <- runif(1);
        if(RNumb < Prob1P) {P <- 0.5; }
        else if(RNumb < (Prob1P + Prob2P)) {P <- 0.5 + (runif(1) * 0.25);}
        else if(RNumb < (Prob1P + Prob2P + Prob3P)) {P <- 0.75 + (runif(1) * 0.15);}
        else if(RNumb < (Prob1P + Prob2P + Prob3P + Prob4P)) {P <- 0.9 + (runif(1) * 0.2);}
        else if(RNumb < (Prob1P + Prob2P + Prob3P + Prob4P + Prob5P)) {P <- 1.1 + (runif(1) * 0.15);}
        else if(RNumb < (Prob1P + Prob2P + Prob3P + Prob4P + Prob5P + Prob6P)) {P <- 1.25;}
      } else {
        P <- 0
        while(P < 0.5 || P > 1.25) {
          P <- dcauchy(runif(1), LocalityP, ScaleP)
        }
      }
      if(is.null(BranchQ)) {
        Q <- 0.5
        RNumb <- runif(1);
        if(RNumb < Prob1Q) {Q <- 0.5 + (runif(1) * 0.1); }
        else if(RNumb < (Prob1Q + Prob2Q)) {Q <- 0.6 + (runif(1) * 0.1);}
        else if(RNumb < (Prob1Q + Prob2Q + Prob3Q)) {Q <- 0.7 + (runif(1) * 0.1);}
        else if(RNumb < (Prob1Q + Prob2Q + Prob3Q + Prob4Q)) {Q <- 0.8 + (runif(1) * 0.1);}
        else if(RNumb < (Prob1Q + Prob2Q + Prob3Q + Prob4Q + Prob5Q)) {Q <- 0.9 + (runif(1) * 0.1);}
      } else {
        #Q <- rnorm(1,BranchQ,0.05)
        Q <- dcauchy(runif(1), BranchQ, 0.1)
      }
      Q <- max(Q, 0.5)
      Q <- min(Q, 0.99)
      
      DD_child <- Q * DD_parent/ P
    }
    
    #number of sub branches
    if(runif(1) < PNLow){Nsub <- NLow} else {Nsub <- NLow + 1}
    
    K <- log(Q)/log(1/Nsub)
    
    DDsub <- vector(length=Nsub)
    #sub branches
    for (jsub in 1:Nsub)  {
      Q = (jsub / Nsub) ^ K - ((jsub - 1) / Nsub) ^ K
      DDsub[jsub] = Q * DD[i] / P
      
      #Tapering coefficient
      TapCoef <- dcauchy(runif(1), LocalityTC, ScaleTC)
      if(Thickening == 0) TapCoef <- abs(TapCoef);
      
      #Calculating distal diameter
      DDprox <- DDsub[jsub]
      Dprox <- DDprox ^ 0.5
      
      if(singleLD == 1) {
        Length <- (Intercept+Slope*(Dprox*DMin)) * (1+RangeL*(runif(1)-0.5))
      } else if(Dprox < (MaxDiamTwig/DMin)) {
        Length <- (InterceptTwig+SlopeTwig*(Dprox*DMin)) * (1+RangeLTwig*(runif(1)-0.5))
      } else if(Dprox < (MaxDiamBranch/DMin)) {
        Length <- (InterceptBranch+SlopeBranch*(Dprox*DMin)) * (1+RangeLBranch*(runif(1)-0.5))
      } else {
        Length <- (InterceptWood+SlopeWood*(Dprox*DMin)) * (1+RangeLWood*(runif(1)-0.5))
      }
      Length <- max(Length, 0)
      
      
      Ddist <- Dprox - (TapCoef * Length)
      if(Ddist < 0) Ddist <- Dprox
      DDdist <- Ddist^2
      DDsub[jsub] <- DDdist
      
      
      #Determine the density of final structures
      Dcur <- Dprox
      Finstruc <- 0
      FLength <- Length
      if(Dcur < Dzerofin) {
        if(Dcur < 1) FLength <- Length - LBaretip * ILatDmin
        FLength <- max(FLength, 0)
        if(Dcur > Dmaxfin) {
          Finstruc = Maxfindens * FLength * (Dzerofin - Dcur) / (Dzerofin - Dmaxfin)
        } else {
          Finstruc = Maxfindens * FLength
        }
      }
      
      
      #store the data
      linkNumber <- linkBase+jsub
      branches[linkNumber,"LinkNumber"] <- linkNumber
      branches[linkNumber,"Length"] <- Length
      branches[linkNumber,"Diam1"] <- Dprox*DMin
      branches[linkNumber,"Diam2"] <- Ddist*DMin
      branches[linkNumber,"ConnectedTo"] <- linkParent
      branches[linkNumber,"FinStruc"] <- Finstruc
      branches[linkNumber,"Largest"] <- 0
      if(jsub == 1) branches[linkNumber,"Largest"] <- 1
      
      
    }
    
    #only the 2nd branches (idx=2) tested?
    if(DDsub[2] >= ((1 + RangeDmin*(runif(1)-0.5))*DMin)/DMin) {
      for (jsub in 1:Nsub) {
        if((DDsub[jsub] > 1) && length(DD) < 100000) {
          DD <- c(DD, DDsub[jsub]) #appending the array
          parentIDs <- c(parentIDs, linkBase+jsub)
        }
        
      }
    }
    if(showProgress) setTxtProgressBar(pb, i/length(DD))
    i<-i+1
  }
  if(showProgress) cat("\nNumber of Paths:", nrow(branches),"\n")
  return(branches)
}


fba.sim <- function (
        NDstep = 10,
        xlsInputFile = NULL,
        
        RoShoSel=1,
        Dlow=2,
        Dhigh=50,
        
        DWperVTwig=0.604926301167267,
        DWperVBranch=0.609585574100232,
        DWperVWood=0.659889368278869,
        
        SLA=85.26298942,
        Areaperleaf=19.7418100885226,
        MaxLAICanopy=4,
        
        FinrootL=3,
        Specrol=30000,
        
        NLogstep=1,
        Ncal=10,
        Randseed=NULL,
        
        DMin = 0.88,
        MaxDiamTwig=2,
        MaxDiamBranch=8,
        ...
    ) {
    pb <- txtProgressBar(style=3)
    TIME <- Sys.time()
    
    if(!is.null(xlsInputFile)) {
        require("XLConnect")
        wb <- loadWorkbook(xlsInputFile)
        fba_param <- readWorksheet(wb, sheet = "Input", startRow = 0, endRow = 76, startCol = 0, endCol = 2)
        RoShoSel <- fba_param[2,2]
        Dlow <- fba_param[4,2]
        Dhigh <- fba_param[5,2]
        
        AvNsub <- fba_param[8,2]
        HowP <- fba_param[9,2]
        
        LocalityP <- fba_param[11,2]
        ScaleP <- fba_param[12,2]
        
        Prob1P <- fba_param[14,2]
        Prob2P <- fba_param[15,2]
        Prob3P <- fba_param[16,2]
        Prob4P <- fba_param[17,2]
        Prob5P <- fba_param[18,2]
        Prob6P <- fba_param[19,2]
        
        Prob1Q <- fba_param[21,2]
        Prob2Q <- fba_param[22,2]
        Prob3Q <- fba_param[23,2]
        Prob4Q <- fba_param[24,2]
        Prob5Q <- fba_param[25,2]
        DMin <- fba_param[26,2]
        RangeDmin <- fba_param[27,2]
        
        singleLD <- fba_param[29,2]
        
        Intercept <- fba_param[31,2]
        Slope <- fba_param[32,2]
        RangeL <- fba_param[33,2]
        
        InterceptTwig <- fba_param[35,2]
        SlopeTwig <- fba_param[36,2]
        RangeLTwig <- fba_param[37,2]
        
        InterceptBranch <- fba_param[39,2]
        SlopeBranch <- fba_param[40,2]
        RangeLBranch <- fba_param[41,2]
        
        InterceptWood <- fba_param[43,2]
        SlopeWood <- fba_param[44,2]
        RangeLWood <- fba_param[45,2]
        
        DWperVTwig <- fba_param[47,2]
        DWperVBranch <- fba_param[48,2]
        DWperVWood <- fba_param[49,2]
        MaxDiamTwig <- fba_param[50,2]
        MaxDiamBranch <- fba_param[51,2]
        
        LBaretip <- fba_param[53,2]
        Dmaxfin <- fba_param[54,2]
        Dzerofin <- fba_param[55,2]
        Maxfindens <- fba_param[56,2]
        
        SLA <- fba_param[58,2]
        Areaperleaf <- fba_param[59,2]
        MaxLAICanopy <- fba_param[60,2]
        
        FinrootL <- fba_param[62,2]
        Specrol <- fba_param[63,2]
        
        NLogstep <- fba_param[65,2]
        Ncal <- fba_param[66,2]
        Randseed <- fba_param[67,2]
        
        LocalityTC <- fba_param[69,2]
        ScaleTC <- fba_param[70,2]
        Thickening <- fba_param[71,2]
        
        InterceptStem <- fba_param[73,2]
        SlopeStem <- fba_param[74,2]
        RangeLStem <- fba_param[75,2]
    }
    
    set.seed(Randseed)
    #prepare the output table
    if(RoShoSel == 0) {
        output <- data.frame("Dbh_cm" = numeric(),
                             "Number_of_links" = numeric(),
                             "Total_length_cm" = numeric(),
                             "Total_weight_g" = numeric(),
                             "Leaf_weight_g" = numeric(),
                             "Twig_weight_g" = numeric(),
                             "Branch_weight_g" = numeric(),
                             "Wood_weight_g" = numeric(),
                             "cv_Nlinks" = numeric(),
                             "cv_lenght" = numeric(),
                             "cv_weight" = numeric(),
                             "Fine_root_length_cm2" = numeric(),
                             "Length_weight" = numeric(),
                             "Rel.fine_root_length" = numeric());
    } else {
        output <- data.frame("Dbh_cm" = numeric(),
                         "Number_of_links" = numeric(),
                         "Total_length_cm" = numeric(),
                         "Total_weight_g" = numeric(),
                         "Leaf_weight_g" = numeric(),
                         "Twig_weight_g" = numeric(),
                         "Branch_weight_g" = numeric(),
                         "Wood_weight_g" = numeric(),
                         "cv_Nlinks" = numeric(),
                         "cv_lenght" = numeric(),
                         "cv_weight" = numeric(),
                         "Leafarea_cm2" = numeric(),
                         "Leaf_weight_ratio" = numeric(),
                         "MinCrownRad"=numeric());
    }
    Dlow <- Dlow/DMin 
    Dhigh <- Dhigh/DMin
    Dlogstep <- 2.7182818 ^ (log(Dhigh/Dlow)/NDstep)
    Dlinstep <- (Dhigh-Dlow)/NDstep
    
    DTwig <- MaxDiamTwig/DMin  
    DBranch <- MaxDiamBranch/DMin
    
    Nr <- 0 
    #Starts a series with a new value for initial tree diameter
    while(Nr <= NDstep) {
        
        Sumlin<-0; Sumlinsq<-0; SumLength<-0; Sumlensq<-0; Sumvol<-0;
        Sumvolsq<-0; SumVolTwig<-0; Sumwght<-0; Sumwghtsq<-0; Sumleafa<-0; SumLWR<-0;
        Sumfinroleng<-0; Sumspecrolall<-0; Sumrelfinroot<-0;
        SumWTwigAll<-0; SumWBranchAll<-0; SumWWoodAll<-0; 
        
        if(NLogstep == 1) {
            D0 <- Dlow * (Dlogstep ^ Nr)
        } else {
            D0 <- Dlow + Nr * Dlinstep
        }
        
        cal <- 1
        #Starts a new tree with the same initial tree diameter
        while(cal <= Ncal) {
            init_diameter <- D0*DMin
            #call the fba_branches() to compute the simulated branches for defined initial diameter
            tree_branches <- fba.branches(
                                    init_diameter, 
                                    DMin=DMin, 
                                    MaxDiamTwig=MaxDiamTwig,
                                    MaxDiamBranch=MaxDiamTwig,
                                    ...)
            #the returned value tree_branches is a table with list of branches segmens
            
            #summarize the result
            nLink <- nrow(tree_branches)
            Sumlin <- Sumlin + nLink
            Sumlinsq <- Sumlinsq + nLink^2
            
            #calculate the branches volume based on truncated cone formula
            tree_branches$vol <- with(tree_branches, pi * Length *(Diam1^2 + Diam2^2 + Diam1*Diam2)/12)
            
            #sum up the segmen dimensions
            SumVTwig <- sum(tree_branches[tree_branches$Diam1 <= MaxDiamTwig, "vol"])
            SumLTwig <- sum(tree_branches[tree_branches$Diam1 <= MaxDiamTwig, "Length"])
            SumVBranch <- sum(tree_branches[tree_branches$Diam1 <= MaxDiamBranch, "vol"])
            SumLBranch <- sum(tree_branches[tree_branches$Diam1 <= MaxDiamBranch, "Length"])
            SumVWood <- sum(tree_branches[tree_branches$Diam1 > MaxDiamBranch, "vol"])
            SumLWood <- sum(tree_branches[tree_branches$Diam1 > MaxDiamBranch, "Length"])
            
            #compute the weight
            SumWTwig <- SumVTwig * DWperVTwig
            SumWBranch <- SumVBranch * DWperVBranch
            SumWWood <- SumVWood * DWperVWood
            
            #compute the final structure
            Suml <- 0
            Sumw <- 0
            Numfin <- sum(tree_branches[,"FinStruc"])
            if(RoShoSel == 0) {
                Finroleng <- Numfin * FinrootL
                Finrowght <- Finroleng / Specrol
                Suml <- SumLTwig + SumLBranch + SumLWood + Finroleng
                Sumw <- SumWTwig + SumWBranch + SumWWood + Finrowght
                SpecRolall <- Suml / Sumw
                Relfinroot <- Finroleng / Suml
            } else if(RoShoSel == 1) {
                Leafarea <- Numfin * Areaperleaf
                Leafweight <- Leafarea / SLA
                Suml <- SumLTwig + SumLBranch + SumLWood
                Sumw <- SumWTwig + SumWBranch + SumWWood + Leafweight
                LWR <- Leafweight / Sumw
            }
            
            Sumwght <- Sumwght + Sumw
            Sumwghtsq <- Sumwghtsq + Sumw^2
            SumWTwigAll <- SumWTwigAll + SumWTwig
            SumWBranchAll <- SumWBranchAll + SumWBranch
            SumWWoodAll <- SumWWoodAll + SumWWood
            SumLength <- SumLength + Suml
            Sumlensq <- Sumlensq + Suml^2
            if(RoShoSel == 1) {
                Sumleafa <- Sumleafa + Leafarea
                SumLWR <- SumLWR + Leafweight
            } else {
                Sumfinroleng <- Sumfinroleng + Finroleng
                Sumspecrolall <- Sumspecrolall + SpecRolall
                Sumrelfinroot <- Sumrelfinroot + Relfinroot
            }
            
            progress <- (cal+(Nr*Ncal))/(Ncal*(NDstep+1))
            setTxtProgressBar(pb, progress)
            cal <- cal+1
        }
    
        #Finish simulating current D0 and start the new initial diameter
        Avgnlin <- Sumlin / Ncal
        Avglength <- SumLength / Ncal
        Avgwght <- Sumwght / Ncal
        AvgWTwig <- SumWTwigAll / Ncal
        AvgWBranch <- SumWBranchAll / Ncal
        AvgWWood <- SumWWoodAll / Ncal
        
        if(Ncal > 1) {
            Stdnlin <- ((Sumlinsq - Sumlin * Sumlin / Ncal) / (Ncal - 1)) ^ 0.5;
            Stdlen <- ((Sumlensq - SumLength * SumLength / Ncal) / (Ncal - 1)) ^ 0.5;
            Stdwght <- ((Sumwghtsq - Sumwght * Sumwght / Ncal) / (Ncal - 1)) ^ 0.5;
        }
        
        Coefvarnlin <- Stdnlin / Avgnlin
        Coefvarlen <- Stdlen / Avglength
        Coefvarwght <- Stdwght / Avgwght
        Avgleafarea <- Sumleafa / Ncal
        AvgLWR <- SumLWR / Ncal
        Avgfinroleng <- Sumfinroleng / Ncal
        AvgSpecrol <- Sumspecrolall / Ncal
        AvgRelfinroot <- Sumrelfinroot / Ncal
        
        MinCrownRad <- log10(sqrt(max(0.1, Avgleafarea)/(pi*MaxLAICanopy)))
        
        Prow <- nrow(output)+1
        
        output[Prow, 1] <- D0 * DMin
        output[Prow, 2] <- Avgnlin
        output[Prow, 3] <- Avglength
        output[Prow, 4] <- Avgwght
        output[Prow, 5] <- max(0.1, Avgwght-AvgWTwig-AvgWBranch-AvgWWood)
        output[Prow, 6] <- AvgWTwig
        output[Prow, 7] <- AvgWBranch
        output[Prow, 8] <- AvgWWood
        output[Prow, 9] <- Coefvarnlin
        output[Prow, 10] <- Coefvarlen
        output[Prow, 11] <- Coefvarwght
        
        if(RoShoSel == 1) {
            output[Prow, 12] <- Avgleafarea
            output[Prow, 13] <- AvgLWR
            output[Prow, 14] <- MinCrownRad
        } else if(RoShoSel == 0) {
            output[Prow, 12] <- Avgfinroleng
            output[Prow, 13] <- AvgSpecrol
            output[Prow, 14] <- AvgRelfinroot
        }
        
        
        Nr<-Nr+1
    }
    elapsed <- Sys.time() - TIME
    cat("\nElapsed time ->",format(.POSIXct(elapsed,tz="GMT"), "%H:%M:%S"))
    return(output)
}

fba.summary <- function (fba_sim_table) {
    isShoot <- FALSE
    if(colnames(fba_sim_table)[12] == "Leafarea_cm2") isShoot <- TRUE
    t <- fba_sim_table
    
    lms <- summary(lm(log(t$Total_weight_g) ~ log(t$Dbh_cm)))
    cf <- exp((lms$sigma^2)/2)
    #apply to either above biomass or root biomass
    a_tot <- exp(lms$coefficients[1,1])*cf/1000
    b_tot <- lms$coefficients[2,1]
    
    if(isShoot) {    
        lms <- summary(lm(log(t$Branch_weight_g) ~ log(t$Dbh_cm)))
        cf <- exp((lms$sigma^2)/2)
        a_branch <- exp(lms$coefficients[1,1])*cf/1000
        b_branch <- lms$coefficients[2,1]
        
        lms <- summary(lm(log(t$Leaf_weight_g + t$Twig_weight_g) ~ log(t$Dbh_cm)))
        cf <- exp((lms$sigma^2)/2)
        a_leaf <- exp(lms$coefficients[1,1])*cf/1000
        b_leaf <- lms$coefficients[2,1]
        
        lw_ratio <- mean(t$Leaf_weight_g/(t$Leaf_weight_g+t$Twig_weight_g))
        
        lms <- summary(lm(log(t$MinCrownRad) ~ log(t$Dbh_cm)))
        cf <- exp((lms$sigma^2)/2)
        a_cr_rad <- exp(lms$coefficients[1,1])*cf/100
        b_cr_rad <- lms$coefficients[2,1]
    }
    
    lms <- summary(lm(log(t$Total_length_cm) ~ log(t$Dbh_cm)))
    cf <- exp((lms$sigma^2)/2)
    a_root_l <- exp(lms$coefficients[1,1])*cf
    b_root_l <- lms$coefficients[2,1]
     
    
    output <- data.frame("Variable" = character(), "Value" = numeric(), stringsAsFactors=FALSE)
    
    if(isShoot) { 
    #SHOOT SECTION
    #MAIN OUTPUTS: allometric equations
        output[1, 1] <- "Intercept for total biomass"
        output[2, 1] <- "Power for total biomass"
        
        output[3, 1] <- "Intercept for branch biomass"
        output[4, 1] <- "Power for branch biomass"
        
        output[5, 1] <- "Intercept for twig and leaf"
        output[6, 1] <- "Power for twig and leaf"
        
        #Parameters for tree library in WaNuLCAS:
        output[7, 1] <- "Leaf weight ratio"
        output[8, 1] <- "Intercept for crown radius"
        output[9, 1] <- "Slope for crown radius"
    } else {
    #ROOT SECTION
    #MAIN OUTPUTS: allometric equations
        output[1, 1] <- "Intercept for total root length"
        output[2, 1] <- "Power for total root length"
        output[3, 1] <- "Intercept for total root weight"
        output[4, 1] <- "Power for total root weight"
    }
    #VALUE
    if(isShoot) {
        output[1, 2] <- a_tot
        output[2, 2] <- b_tot
        
        output[3, 2] <- a_branch
        output[4, 2] <- b_branch
        
        output[5, 2] <- a_leaf
        output[6, 2] <- b_leaf
        
        #Parameters for tree library in WaNuLCAS:
        output[7, 2] <- lw_ratio
        output[8, 2] <- a_cr_rad
        output[9, 2] <- a_cr_rad
    } else {
    #ROOT SECTION
    #MAIN OUTPUTS: allometric equations
        output[1, 2] <- a_root_l
        output[2, 2] <- b_root_l
        output[3, 2] <- a_tot
        output[4, 2] <- b_tot
    }
    
    return(output)
}

fba.plot <- function (fba_sim_table) {
    isShoot <- FALSE
    if(colnames(fba_sim_table)[12] == "Leafarea_cm2") isShoot <- TRUE
    t <- fba_sim_table
    
    ylim <- range(t$Total_weight_g, t$Total_length_cm)
    if(isShoot) ylim <- range(t$Total_weight_g, t$Total_length_cm, t$Leafarea_cm2)
        
    plot(t$Dbh_cm, t$Total_weight_g, log="xy", xlab="Initial Diameter (cm)", ylab="", ylim=ylim)
    par(new=TRUE)
    plot(t$Dbh_cm, t$Total_length_cm, log="xy", xlab="", ylab="", yaxt='n', xaxt='n', ylim=ylim, pch=2)
    if(isShoot) {
        par(new=TRUE)
        plot(t$Dbh_cm, t$Leafarea_cm2, log="xy", xlab="", ylab="", yaxt='n', xaxt='n', ylim=ylim, pch=0)
        legend("topleft", c( "Total length", "Total weight", "Leaf area"), pch=c(2,1,0), title="Shoot (log-log scale)", bty="n")
    } else {
        legend("topleft", c( "Total length", "Total weight"), pch=c(2,1), title="Root (log-log scale)", bty="n")
    }    
}

#add the angles to the branch segments for the visualization
fba.angles <- function(
  fba_branches_table,
  orthoVertAnglesRange = 20,
  plagioVertAnglesMean = 45,
  plagioVertAnglesSD = 15,
  TwigVertAnglesRange = 90,
  showProgress = FALSE
  ) {
  branches <- fba_branches_table
  if(showProgress) pb <- txtProgressBar(style=3)
  branches$Ortho <- 0
  branches$VertAng <- 0
  branches$RotAng <- 0
  
  for(linkNumber in branches$LinkNumber) {
    
    #The first branch (the largets diameter one) wil 'inherit'
    #the orthotropic property from an orthotropic parent
    linkParent <- branches[linkNumber,"ConnectedTo"]
    if(linkNumber == 1) {
      branches[linkNumber,"Ortho"] <- 1
    } else {
      ortho <- 0
      if(branches[linkNumber,"Largest"] == 1) ortho <- branches[linkParent, "Ortho"]
      branches[linkNumber,"Ortho"] <- ortho
    }
    
    VertAng <- 0
    RotAng <- 0
    if(branches[linkNumber,"Ortho"] == 1) {
      VertAng <- runif(1)*orthoVertAnglesRange
      RotAng <- runif(1)*360
      if(linkNumber == 1) VertAng <- 0 
    } else {
      if(branches[linkParent,"Ortho"] == 1) {
        VertAng <-rnorm(1, plagioVertAnglesMean, plagioVertAnglesSD)
        RotAng <- runif(1)*360
      } else if(branches[linkNumber,"Largest"] == 1) {
        VertAng <-rnorm(1, branches[linkParent,"VertAng"], plagioVertAnglesSD)
        RotAng <-rnorm(1, branches[linkParent,"RotAng"], plagioVertAnglesSD)
      } else {
        VertAng <- runif(1)*TwigVertAnglesRange
        RotAng <- runif(1)*360
      } 
    }
    branches[linkNumber,"VertAng"] <- VertAng
    branches[linkNumber,"RotAng"] <- RotAng
    if(showProgress) setTxtProgressBar(pb, linkNumber/length(branches$LinkNumber))
  }
  return(branches)
}


fba.visual <- function (fba_branches_angle_table, texture = TRUE, leaves = TRUE, animation = TRUE, leafSize = 10) {
  options(java.parameters = "-Xmx2g")
    if(!require(rJava)) {
      install.packages(rJava)
      library(rJava)
    }
    .jinit()
    vb <- .jnew("dharja/branches/gui3d/Branch3DCanvas")
    br <- fba_branches_angle_table
    ret <- .jcall(vb,"Z","loadBranches", as.integer(br$LinkNumber),.jfloat(br$Length), .jfloat(br$Diam1), 
           .jfloat(br$Diam2), as.integer(br$ConnectedTo), as.integer(round(br$FinStruc)),
           .jfloat(br$VertAng), .jfloat(br$RotAng), texture, leaves, animation, .jfloat(leafSize))
    .jcall(vb,, "setVisible", TRUE)
}


fba.branches.visual <- function (...) {
  br <- fba.branches(...)
  bra <- fba.angles(br)
  fba.visual(bra)
}

fba.branches.angles <- function (...) {
  br <- fba.branches(...)
  bra <- fba.angles(br)
  return(bra)
}

