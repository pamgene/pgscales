#' Create a jet colormap for use in e.g. heatmaps
#' @return vector of hex color defining the jet colormap
#' @example mymap = cjet()
#' @import ggplot2
#' @export
cjet = function(){
  jet = c("#00008F", "#00009F", "#0000AF", "#0000BF", "#0000CF", "#0000DF",
          "#0000EF", "#0000FF", "#0010FF", "#0020FF", "#0030FF", "#0040FF",
          "#0050FF", "#0060FF", "#0070FF", "#0080FF", "#008FFF", "#009FFF",
          "#00AFFF", "#00BFFF", "#00CFFF", "#00DFFF", "#00EFFF", "#00FFFF",
          "#10FFEF", "#20FFDF", "#30FFCF", "#40FFBF", "#50FFAF", "#60FF9F",
          "#70FF8F", "#80FF80", "#8FFF70", "#9FFF60", "#AFFF50", "#BFFF40",
          "#CFFF30", "#DFFF20", "#EFFF10", "#FFFF00", "#FFEF00", "#FFDF00",
          "#FFCF00", "#FFBF00", "#FFAF00", "#FF9F00", "#FF8F00", "#FF8000",
          "#FF7000", "#FF6000", "#FF5000", "#FF4000", "#FF3000", "#FF2000",
          "#FF1000", "#FF0000", "#EF0000", "#DF0000", "#CF0000", "#BF0000",
          "#AF0000", "#9F0000", "#8F0000", "#800000")
  return(as.vector(jet))
}
#' Create a linear colormap for use in e.g. heatmaps
#' @return vector of hex color defining the linear colormap
#' @example mymap = clincol()
#' @export
clincol = function(){
   cscale = c("#359C6B","#2ACFAE","#8706BE","#36A067","#30CDB7","#9306BD","#36A464","#37CAC0","#A006BC","#35A760","#3DC6C8","#AD07BC",
   "#32A95D","#43C2D1","#BA0ABB","#2CAC59","#4ABED9","#C70FBA","#20AE56","#53B9E1","#D415B9","#0FB152","#5CB4E9","#DD1DB9",
   "#03B34E","#63AFF1","#E125B9","#02B54A","#68A9F8","#E12EBB","#04B744","#6DA4FD","#E038BE","#08B93D","#719EFF","#DE42C3",
   "#0DBB37","#7598FE","#DC4DC8","#10BC32","#7992FC","#DA59CE","#12BD2E","#7D8CFA","#D865D3","#14BE2A","#8186F7","#D671D8",
   "#14BF26","#8580F5","#D47CDE","#13BF21","#897BF5","#D387E4","#10BF1D","#8E77F5","#D292EB","#0CBF18","#9373F6","#D29CF3",
   "#09BF13","#986FF9","#D1A5FB","#07BE0E")
   return(as.vector(cscale))
}
#' Create a redness colormap for use in e.g. heatmaps
#' @return vector of hex color defining the linear colormap
#' @example mymap = pbmccol
#' @export
pbmccol = function(){
  cscale = c(clear = "#a4abb5",
             pink = "#f4bee7",
             pinker = "#f722c4",
             red = "#f72121",
             yellowish = "#f7f28c")
  return(as.vector(cscale))
}

#' map data to a divergent colormap
#' @return vector of hex colors corresponding to the input data
#' @example colors = mapToDivColorMap(randn(10))
#' @export
mapToDivColorMap = function(x, clScale = list(low = -1, mid = 0, high = 1), clValues = c("#3bef3b", "#000000", "#f42411") ){
  scaled.color = rescale(x - clScale$mid, from = c(clScale$low, clScale$high), clip = TRUE)
  aScale = scale_color_gradient2(low = clValues[1], mid = clValues[2], high = clValues[3])
  color = aScale$palette(scaled.color)
  return(color)
}
