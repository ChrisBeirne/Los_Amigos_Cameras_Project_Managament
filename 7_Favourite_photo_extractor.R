# Code to extract all of the photos marked as favourites and upload them to google drive

# First import the raw image data

dat <- read.csv("data/raw-data/images.csv", header=T)

head(dat)
faves <- dat[dat$highlighted=="true", ]


# File path to the images
img_loc <- "F:/LosAmigosBackup-18042022/Camera_data/Photos/0_Uploaded To Wildlife Insights"

# File path to the identification folder
img_dest <- "F:/WI_favourite_images/"
library(stringr)

i <- 60
for(i in 1:nrow(faves))
{
  tmp_name <- paste0(faves$common_name[i], "_",substr(faves$deployment_id[i],1,6), "_", substr(faves$timestamp[i],1,10),"_" ,substr(faves$timestamp[i],12,19))
  tmp_name <- str_replace_all(tmp_name, ":", "-")
  # Check if it exists in the file
  existing <- list.files(paste0("F:/WI_favourite_images/"))
  
  if(!tmp_name %in% existing)
  {
    # Get the file flocation
    tmp_file <- paste0(img_loc, "/" ,faves$deployment_id[i],"/",faves$filename[i])
    
    # Some files are nested within other folders (e.g. )
    if(length(tmp_file[file.exists(tmp_file)==FALSE])>0)
    {
      tmp_sub <- list.files(substr(tmp_file[file.exists(tmp_file)==FALSE], 1,97))
      tmp_file[file.exists(tmp_file)==FALSE] <- paste0(img_loc, "/" ,tmp_event$deployment_id[file.exists(tmp_file)==FALSE],"/",tmp_sub,"/",tmp_event$filename[file.exists(tmp_file)==FALSE])
      
    }
    # Move and rename
    
    file.copy(tmp_file, img_dest)  
    file.rename(paste0(img_dest, gsub('.*/ ?(\\w+)', '\\1', tmp_file) ),
                paste0(img_dest, tmp_name, ".JPG"))
    
    
  }  
  print(i)
}

# Upload to Google drive
to_upload <- list.files(img_dest, full.names =T)

for(i in 1:length(to_upload))
{
  drive_upload(
    media,
    path = NULL,
    name = NULL,
    type = NULL,
    ...,
    overwrite = NA,
    verbose = deprecated()
  )
  
  
}





