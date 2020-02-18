#===============================File storage=============================

#get working directory
wd <- getwd()

# folders for storing data outputs and figures
# store names of the folders in an object
output.folder.names <- c("figures", "data.output")
# and make the folders if they don't exist yet. 
for(i in 1:length(output.folder.names)) 
  if(file.exists(output.folder.names[i]) == FALSE) 
    dir.create(output.folder.names[i])

#path to figures folder
path.figures <- paste(wd,"/",output.folder.names[1],"/", sep = "")

#path to data output folder
path.data.output <- paste(wd,"/",output.folder.names[2],"/", sep = "")