library('stringr')
library('sensitivity')
library('hydromad')
working_directory="C:/UserData/seol/SWAT/Lynn/parallel/"
swatmodel.directory=str_c(working_directory,'Example/')
setwd(working_directory)
source("swat_hydromad.R")


#NSE* custom function
NSE_star <- function(obs,sim,...)
{
  nse_s = (1 -    sum( (obs[,1]-sim)^2 ) / sum ( (obs[,1]-mean(obs[,1]) )^2) )/
    (1  +   sum( (obs[,1]-sim)^2 )/sum ( (obs[,1]-mean(obs[,1]) )^2 ) )
  
  return(nse_s)
}


#############################
# Load initial parameter set

#column names for all parameters
col_header <- read.table(str_c(working_directory,'List of Parameters.txt'), skip=2, fill=NA)[1]
col_header <- col_header[ col_header != "change(%)" ]
col_header <- col_header[ col_header != "value" ]   
col_header[24]<-"SurLag" #Capitalisation was wrong

# Parameter values
par_1st=read.table(str_c(working_directory,'lyn_initial.out'))
colnames(par_1st) <- col_header

col_header=c("Alpha_Bf","Biomix",  "Blai"  ,   "Canmax",   "Ch_K2"  ,  "Ch_N2"  , "Cn2","Epco"   ,  "Esco", "Gw_Delay" ,"Gw_Revap", "Gwqmn"  ,  "Rchrg_Dp",
 "Revapmn",  "Sftmp"   , "Slope" ,  "Slsubbsn", "Smfmn"   , "Smfmx"  ,  "Smtmp"   , "Sol_Alb" , "Sol_Awc" , "Sol_K" ,
 "SurLag"  , "Tlaps"   , "Timp")

#flow related parameters
frp=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26)
## Load observed flow as zoo object
## P is set to NA to make clear to hydromad that only Q is provided
t_obs <- read.table(str_c(working_directory, 'lotdataObs.dat'))
t_obs$date<-as.Date(paste(t_obs$V1,t_obs$V2),"%Y %j")
obst=cbind(Q=zoo(t_obs$V3,order.by=t_obs$date),P=NA)

target_year='2001'   #######the year simulated

start_target_year=str_c(target_year,'-01-01')
end_target_year=str_c(target_year,'-12-31')
obs=window(obst,start=as.Date(start_target_year),end=as.Date(end_target_year))
head(obs)



# obs$Q=sprintf('%12.3E',obs$Q)
# head(obs)
# 
# read.csv('reformatted.csv')


# ###########################quasi MC sampling###########
# library('randtoolbox')
# n=8000
# n*28*6.5/3/60/60/24 #days
# 
# 
# no.seed=1221
# qmc_sobol=sobol(n, dim = 52, init = TRUE, scrambling =1, seed = no.seed, normal = FALSE)
# 
# # plot(seq(1000),qmc_sobol[,29])
# 
# # qmc_halton=halton(n, dim = 52, init = TRUE, normal = FALSE, usetime=FALSE)
# # qmc_halton=halton(n, dim = 52, init = FALSE,normal = FALSE, usetime=FALSE)
# # qmc_torus=torus(n, dim = 52, init = TRUE,  mixed=FALSE,  usetime=FALSE)
# # qmc_torus=torus(n, dim = 52, init = FALSE, mixed=FALSE,  usetime=FALSE)
# ######description
# 
# qmc=qmc_sobol
# result_description=str_c('the year ',target_year,', sobol QMC, ',n)
# 
# 
# min.max=swat.ranges()
# q1=data.frame(matrix(0,nrow=n,ncol=26))
# q2=data.frame(matrix(0,nrow=n,ncol=26))
# for (i in 1:n)
#   {
#     for (j in 1:26)
#       {
#       q_qmc=qunif(qmc[i,j],min.max[[j]][1],min.max[[j]][2])
#       q1[i,j]=q_qmc
#       q_qmc=qunif(qmc[i,j+26],min.max[[j]][1],min.max[[j]][2])
#       q2[i,j]=q_qmc
#     }
# }
# 
# 
# names(q1)=col_header
# names(q2)=col_header
# row.names(q1) <- NULL
# row.names(q2) <- NULL
# ######################################################



#############################
## Setup hydromad object
hydromad.options(swatmodel.directory=swatmodel.directory)

# Hydromad object with fixed non-flow related parameters
obj=hydromad(DATA=obs,sma="swat",newpars=par_1st[-frp])

obj

##################################
## Setup parallelisation

nworkers=4

## First make copies of the model directory

run.directories=sprintf("C:/UserData/seol/SWAT/Lynn/parallel/Example%d/",1:nworkers)
for(dd in run.directories) {
print(dd)
    unlink(str_sub(run.directories,1,-2),recursive=TRUE,force=TRUE)  
}


for(dd in run.directories) {
  print(dd)
  dir.create(dd)
  file.copy( list.files(swatmodel.directory,full.names=TRUE), dd)
}

## Set up workers
library(parallel)
cl <- makeCluster(nworkers, type="SOCK")
clusterCall(cl,function() library("hydromad"))
## Remove local swatmodel.directory, set the directory on the workers
hydromad.options(swatmodel.directory=NULL)
clusterApply(cl, seq_along(cl), function(i) hydromad.options(swatmodel.directory=sprintf("C:/UserData/seol/SWAT/Lynn/parallel/Example%d",i)))
# Define parallelisation settings for the evalPars function
hydromad.options(parallel=list(evalPars=list(method="clusterApply",export=c("swat.sim","NSE_star"))))
## Check that workers have different directories
hydromad.getOption("swatmodel.directory") #should be NULL
clusterCall(cl,function() hydromad.getOption("swatmodel.directory"))
## Check parallelisation settings
hydromad.getOption("parallel")[["evalPars"]]
##################################
## Sobol
## see ?hydromad_sensitivity

exact_par=read.csv('exact_par.csv')
exact_par['no.rows.pars']=NULL
head(exact_par)

#read PAWN input
pars2=read.csv('PAWN_par1.csv')
head(pars2)

parss=cbind(pars2,exact_par[-c(1,5,6,7,16)])
head(parss)
tail(parss)
pars1=parss[col_header]
head(pars1)

#attach serial number 
no.rows.pars=seq(nrow(pars1))
pars1=cbind(no.rows.pars,pars1)
head(pars1)
pars=pars1
head(pars)

# #iteration for all the parameters
# # for (i in 4:4){
# i=4
# no.para=i#the order of parameter to vary
# no.para=no.para+1





#create directory to save output files and input parameter sets
dir.create(str_c(working_directory,'swat output/'))
dir.create(str_c(working_directory,'swat output/flow'))
#write parameter set
write.csv(pars,str_c(working_directory,'swat output/par.csv'),row.names=FALSE)
pars=read.csv(str_c(working_directory,'swat output/par.csv'))

#run swat
system.time(results<-evalPars(pars,object=obj,objective=NSE_star))

# #name of folder to save outputs
# name_para=colnames(pars[no.para])
# WDD <<- str_c(working_directory,name_para,'_swat output/')
# dir.create(WDD)
# dir.create(str_c(WDD,'/flow/'))
# #plotting
# tiff(str_c(WDD,name_para,'.png'))
# plot(pars[,no.para],results,type='p',xlab=colnames(pars[no.para]),ylab='NSE*',main='model evaluation with obs')
# dev.off()
# #write results
# PR=cbind(pars,results)
# write.csv(PR,str_c(WDD,name_para,'.csv'),row.names=FALSE)

PR=cbind(pars,results)
write.csv(PR,'pawn_swat.csv',row.names=FALSE)


file.copy( list.files(str_c(working_directory,'swat output/flow') ,full.names=TRUE),str_c(WDD,'/flow/')  )


unlink(str_c(working_directory,'swat output'),recursive=TRUE)

# }



print('==================================finished======================================')
save.image(str_c(working_directory,'sobol.RData'))