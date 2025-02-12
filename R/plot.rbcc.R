#-----------------------------------------------------------------------------#
#                                                                             #
#              RISK-BASED CONTROL CHARTS                                      #
#                                                                             #
#  Written by: Aamir Saghir, Attila I. Katona, Zsolt T. Kosztyan              #
#              Department of Quantitative Methods                             #
#              University of Pannonia, Hungary                                #
#              kosztyan.zsolt@gtk.uni-pannon.hu                               #
#                                                                             #
# Last modified: January 2025                                                 #
#-----------------------------------------------------------------------------#

#' @export
plot.rbcc <- function(x,...)
 {
  if (methods::is(x,"rbcc")){
    H_opt<-x
    LCL=H_opt$LCLx
    UCL=H_opt$UCLx
    LCLopt=H_opt$LCLy
    UCLopt=H_opt$UCLy
    Groups<-value<-variable<-NULL
    df <- data.frame(Groups = c(1:length(H_opt$real)), y1= H_opt$real,
                     y2=H_opt$Observed, y3= LCL, y4= UCL, y5=LCLopt, y6=UCLopt)
    big_data <- reshape2::melt(df, id = "Groups")
    ggplot2::ggplot(big_data, ggplot2::aes(x = Groups,  y = value,
                                           color = variable)) +
      ggplot2::geom_line()+ ggplot2::scale_color_manual(
        labels = c("real","observed","LCL","UCL", "LCLopt", "UCLopt"),
        values=c("black", "green1", "blue","blue", "red","red"))+
      ggplot2::labs (x= "Groups", y= "Group_Statistic")+
      ggplot2::theme_bw()+ ggplot2::theme(legend.title =
                                            ggplot2::element_blank()) +
      ggplot2::ggtitle("Univariate Control Chart for traditional and risk-based Statistics")
  }
}
