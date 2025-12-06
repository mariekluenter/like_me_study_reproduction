
# script that we used for making gifs, may be useful to somebody...

#install.packages("magick")
library(magick)
#install.packages("gifski")
library(gifski)




make_gif = function(imagename,folder1,folder2){ #retrieve image name from 1 folder, then the other

newlogo <- image_scale(image_read(paste("still_list",folder1,"/",imagename,".jpg",sep = "")))
oldlogo <- image_scale(image_read(paste("still_list",folder2,"/",imagename,".jpg",sep = "")))


img <- (c(oldlogo, newlogo))
        
#gif_name = image_animate(image_scale(img, "400x400"), fps = 2)
gif_name = image_animate(image_scale(img), fps = 2)


#image_write(gif_name,path = paste("C:/Users/Jessic Brough/Dropbox/PhD/Replication/gifs/",
                                  #imagename,"_",folder1,folder2,".gif",sep=""),format = 'gif')

image_write(gif_name,path = paste("C:/Users/Jessic Brough/Dropbox/PhD/Replication/gifs/",
                                  imagename,".gif",sep=""),format = 'gif')
}


picture_names = c(#"argue1","argue2", "argue3", "argue4", "argue5", "argue6", "argue7", "argue8",
                  #"dance1", "dance2", "dance3", "dance4", "dance5", "dance6", "dance7", "dance8",
                  #"fight1", "fight2", "fight3", "fight4", "fight5", "fight6", "fight7", "fight8",
                  #"flirt1", "flirt2", "flirt3", "flirt4", "flirt5", "flirt6", "flirt7", "flirt8",
                  #"hug1", "hug2", "hug3", "hug4", "hug5", "hug6", "hug7", "hug8",
                  #kiss1", "kiss2", "kiss3", "kiss4", "kiss5", "kiss6", "kiss7", "kiss8",
                  #"look1", "look2", "look3", "look4", "look5", "look6", "look7", "look8",
                  #marry1", "marry2", "marry3", "marry4", "marry5", "marry6", "marry7", "marry8",
                  #"meet1", "meet2", "meet3", "meet4", "meet5", "meet6", "meet7", "meet8",
                  #play1", "play2", "play3", "play4", "play5", "play6", "play7", "play8",
                  #"shout1", "shout2", "shout3", "shout4", "shout5", "shout6", "shout7", "shout8",
                  #talk1", "talk2", "talk3", "talk4", "talk5", "talk6", "talk7", "talk8",
                  #"touch1", "touch2", "touch3", "touch4", "touch5", "touch6", "touch7", "touch8",
                    "meet_eg", "look_eg")
for (i in picture_names){
  #make_gif(i,"A","B")
  make_gif(i,"B","A")  #comment out as appropriate for saving purposes. B,A = listA; A,B = listB
}


