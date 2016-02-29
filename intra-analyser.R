df <- intra

a<- cut(intra$timeseq , breaks = 25 , labels = c("night" , "day" ,  "eve" ,"late"))

plot(df$timeseq , log(df$intra.calorie))
