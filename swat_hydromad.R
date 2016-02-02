## SWAT for hydromad
## Using SWAT model provided by Ann van Griensven
## Adapted from R code by Lynn Seo

swat.sim<-function(DATA,
                   no.rows.pars,Alpha_Bf,Biomix,Blai,Canmax,Ch_K2,Ch_N2,Cn2,Epco,Esco,Gw_Delay,Gw_Revap,Gwqmn,Rchrg_Dp,Revapmn,Sftmp,Slope,Slsubbsn,Smfmn,Smfmx,Smtmp,Sol_Alb,Sol_Awc,Sol_K,SurLag,Tlaps,Timp,
                   swatmodel.directory=hydromad.getOption("swatmodel.directory"),
                   quiet=TRUE){
  
  if(is.null(swatmodel.directory)) stop("swatmodel.directory not specified. Run hydromad.options(swatmodel.directory='absolute/path/to/swat/directory')")
  
  par.vals=c(Alpha_Bf,Biomix,Blai,Canmax,Ch_K2,Ch_N2,Cn2,Epco,Esco,Gw_Delay,Gw_Revap,Gwqmn,Rchrg_Dp,Revapmn,Sftmp,Slope,Slsubbsn,Smfmn,Smfmx,Smtmp,Sol_Alb,Sol_Awc,Sol_K,SurLag,Tlaps,Timp)
  
  #change the bestpar file
  #in each run, the SWAT reads the "bestpar.out' file, then updates the parameters
  #in the model with the new ones
  par.text=paste('    1',
                 paste(sprintf('%+12.5E',par.vals),collapse=''),
                 sep=''
  )
  
  wd <- setwd(swatmodel.directory)
  on.exit(setwd(wd))
  
  cat(par.text,file="bestpar.out")
  
  system('SWAT09OF.exe',ignore.stdout = quiet)
  
  #extract numeric matrix from output
  # temp_lotout <<- read.table('lotout.dat', skip=6)
   
#   
  library('stringr')
#   #extract output.std which is always daily
#   file_input =  'output.std'
#   #Open file
#   output.std = file(file_input, open='r')
#   #Read file contents in (by line)
#   file_content = readLines(output.std)
#   close(output.std)
#   #set up and find the marker
#   marker_text = "AVE ANNUAL VALUES"
#   marker_row <- grep(marker_text, file_content)
#   skip_rows = marker_row+2
#   #extract 'AVE annual values' table only as character vector
#   table_text <- file_content[skip_rows:(skip_rows+195)]
#   # to separate columns where necessary
#   table_text <- str_replace(table_text, 'ropo.', "    .")
#   table_text <- str_replace(table_text, 'yloa.', "    .")
#   #Find the first 15 characters in every row, skipping the header
#   row_sub <- str_sub(table_text[2:length(table_text)], 1, 15)
#   #Add a space after first 15 characters, skipping the header
#   table_content <- str_replace(table_text[2:length(table_text)], row_sub, str_c(row_sub, "    "))
#   #Join modified table
#     table_text <- c(table_text[1], table_content)
#     table=read.table(text=table_text, header=TRUE, strip.white=TRUE )
#     extract.header=c('HRU','SUB','SURQmm','GWQmm')
#     extracted.from.output.std=table[,extract.header]
#    
    
   # write.table(extracted.from.output.std,str_c('C:/UserData/seol/SWAT/Lynn/parallel/swat output/HRUs/',no.rows.pars,'HRU.txt'))
   # write.table(zoo(temp_lotout[,4],order.by=index(DATA)),str_c('C:/UserData/seol/SWAT/Lynn/parallel/swat output/flow/',no.rows.pars,'flow.txt'))

    
    #extract output.rch
    # output.rch=read.table(str_c(dd,'output.rch'),skip=9)
    output.rch=read.table('output.rch',skip=9)
    output.rch2=output.rch[output.rch$V2==1 & output.rch$V4<1000,] #output.rch$V2 -> reach no.
    output.rch2=output.rch2[1:365,]
    write.table(output.rch2$V7,str_c('C:/UserData/seol/SWAT/Lynn/parallel/swat output/flow/',no.rows.pars,'flow.txt'))

    #extract alpha_bf values
    par1=readLines('000010001.gw',n=5)
    write.table(str_sub(par1[5],1,18),str_c('C:/UserData/seol/SWAT/Lynn/parallel/swat output/flow/',no.rows.pars,'alpha.txt'))
    
#    temp_lotout <<- read.table('lotout.dat', skip=6)
#    return(zoo(temp_lotout[,4],order.by=index(DATA)))
#    
   return(zoo(output.rch2$V7,order.by=index(DATA)))      
  }

swat.ranges<-function(){
  list(Alpha_Bf = c(0, 1), 
       Biomix = c(0, 1), 
       Blai = c(0.5,10),
       Canmax = c(0, 10), 
       Ch_K2 = c(0, 150), 
       Ch_N2 = c(0, 1),
       Cn2 = c(-50, 25), 
       Epco = c(0.1, 1),
       Esco = c(0, 1),
       Gw_Delay = c(1, 60),
       Gw_Revap = c(0.02,0.2),
       Gwqmn = c(10, 500), 
       Rchrg_Dp = c(0, 1),
       Revapmn = c(1,500), 
       Sftmp = c(-5, 5), 
       Slope = c(0, 1),
       Slsubbsn = c(10, 150),
       Smfmn = c(0, 10), 
       Smfmx = c(0, 10), 
       Smtmp = c(-5, 5), 
       Sol_Alb = c(0,0.25), 
       Sol_Awc = c(-25, 60),
       Sol_K = c(0, 2000), 
       SurLag = c(0.5, 10), 
       Tlaps = c(-10, 10), 
       Timp = c(-10, 10)
  )
}