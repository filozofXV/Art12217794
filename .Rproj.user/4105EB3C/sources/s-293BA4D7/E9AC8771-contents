make_art <- function(seed = round(runif(1,0,100000)),
                     palette_type = sample(names(wes_palettes),1)) {
  require(ggplot2)
  require(wesanderson)
  set.seed(seed)
  ngroup <- 30
  names  <- paste("G_",seq(1,ngroup),sep="")
  DAT <- data.frame()

  for(i in 1:40){
    data <- data.frame( matrix(0, ngroup , 3))
    data[,1] <- i
    data[,2] <- sample(names, nrow(data))
    arg1 <- runif(1, 1, 50)
    arg2 <- runif(1, 51, 100)
    if (seed %% 3 == 0) {
      data[,3] <- sin(runif(30, arg1, arg2))
    } else if (seed %% 3 == 1){
      data[,3] <- cos(runif(30, arg1, arg2))
    } else {
      data[,3] <- prop.table(sample( c(rep(0,100),c(1:ngroup)) ,nrow(data)))
    }


    DAT <- rbind(DAT,data)
  }
  colnames(DAT)<- c("a","b","c")




  ggplot(DAT, aes(x = a, y = c, fill = b)) +
    geom_area(alpha = 1)+
    {if(seed %% 2 == 0) geom_line()} +
    {if(seed %% 2 == 1) geom_smooth()} +
    theme_minimal() +
    scale_fill_manual(values = wesanderson::wes_palette(palette_type, ngroup, type = "continuous"))+
    theme(
      text = element_blank(),
      line = element_blank(),
      title = element_blank(),
      legend.position = "none",
      panel.border = element_blank(),
      panel.background = element_blank())
}

