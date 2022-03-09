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
summary.rbmcc <- function(object, digits =  getOption("digits"), ...) {
  if (!requireNamespace("qcc", quietly = TRUE)) {
    stop(
      "Package \"qcc\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (class(object)=="rbmcc"){
    real <- object$real
    Observed <- object$Observed
    cost0 <- object$cost0 # calculation of total cost during the process
    cost1 <- object$cost1 # total cost related to decision 1 (c11)
    cost2 <- object$cost2 # total cost related to decision 2 (c10)
    cost3 <- object$cost3 # total cost related to decision 3 (c01)
    cost4 <- object$cost4 # total cost related to decision 4 (c00)
    baselimit <- object$baselimit
    limit <- object$limit
    Kopt <- object$Kopt
    #chartx <- object$cchartx
    #charty <- object$ccharty
    #class(chartx)<-"mqcc"
    #class(charty)<-"mqcc"
    cat("\nSummary of the cost structure:\n")
    cat("\nTotal cost: ",cost0)
    cat("\nc11: ",cost1)
    cat("\nc10: ",cost2)
    cat("\nc01: ",cost3)
    cat("\nc00: ",cost4)
    cat("\n\nSummary of control limits:\n")
    cat("\nUCL for T2: ",baselimit)
    cat("\nUCL for RBT2: ",limit)
    if (!is.null(Kopt)){
            cat("\n\nOptimal correction factor",Kopt)
    }
    cat("\n\nSummary of group statistics of real values:\n")
    print(summary(real), digits = digits, ...)
    cat("\n\nSummary of group statistics of observed values:\n")
    print(summary(Observed), digits = digits, ...)
    #cat("\n\nSummary of original T2 chart:\n")
    #qcc::summary.mqcc(chartx)
    #cat("\n\nSummary of original RB-T2 chart:\n")
    #qcc::summary.mqcc(charty)
  }else{
    summary(object,...)
  }
}
