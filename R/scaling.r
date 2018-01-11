#' Rescale numeric vector data with the option to clip data to the max and min of the output range
#' @return rescaled and clipped data vector
#' @param x numeric vector of values to manipulate
#' @param to = c(0,1) output range (numeric vector of length two)
#' @param from = range(x, na.rm = TRUE, finite = TRUE)
#' @param clip = FALSE, if TRUE the result will be clipped to the max and min of the output range.
#' @import scales
#' @export
rescale = function (x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE), clip = FALSE)
{
  if (zero_range(from) || zero_range(to)) {
    return(ifelse(is.na(x), NA, mean(to)))
  }
  x = (x - from[1])/diff(from) * diff(to) + to[1]

  if(clip){
    # elegant clipping by Josh O'Brien @ Stackoverflow
    #http://stackoverflow.com/questions/13868963/clip-values-between-a-minimum-and-maximum-allowed-value-in-r
    x = to[1] + (x > to[1])*(x-to[1])  - (x > to[2]) * (x - to[2])
  }
  return(x)
}
