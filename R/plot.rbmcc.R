#-----------------------------------------------------------------------------#
#                                                                             #
#  RISK-BASED MULTIVARIATE CONTROL CHARTS                                     #
#                                                                             #
#  Written by: Aamir Saghir, Attila I. Katona, Zsolt T. Kosztyan              #
#              Department of Quantitative Methods                             #
#              University of Pannonia, Hungary                                #
#              kzst@gtk.uni-pannon.hu                                         #
#                                                                             #
# Last modified: March 2022                                                   #
#-----------------------------------------------------------------------------#

#' @export
plot.rbmcc <- function(x,...){
  if ("rbmcc" %in% class(x)){
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
      stop(
        "Package \"ggplot2\" must be installed to use this function.",
        call. = FALSE
      )
    }
    if (!requireNamespace("reshape2", quietly = TRUE)) {
      stop(
        "Package \"reshape2\" must be installed to use this function.",
        call. = FALSE
      )
    }
    H_opt<-x
    UCL=H_opt$baselimit
    UCLopt=H_opt$limit
    Groups<-value<-variable<-NULL
    df <- data.frame(Groups = c(1:length(H_opt$real)), y1= H_opt$real, y2=H_opt$Observed, y3= UCL, y4=UCLopt)
    big_data <- reshape2::melt(df, id = "Groups")
    ggplot2::ggplot(big_data, ggplot2::aes(x = Groups,  y = value, color = variable)) +
      ggplot2::geom_line()+ ggplot2::scale_color_manual(labels = c("real","observed","UCL", "UCLopt"), values=c("black", "green1", "blue", "red"))+
      ggplot2::labs (x= "Groups", y= "Group_Statistic")+ ggplot2::theme_bw()+ ggplot2::theme(legend.title = ggplot2::element_blank()) +
      ggplot2::ggtitle(" Multivariate Control chart for Hotelling's and risk-based Statistics")
  }else{
    plot(x,...)
  }
}
