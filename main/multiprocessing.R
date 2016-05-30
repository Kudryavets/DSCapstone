require(doParallel)

#' Title
#'
#' @param text : text corpus in any format which provide slicing 
#' @param part.size : max chunk size (int)
#' @param func : fucntion to implement on data
#' @param param : function parametr to implement on data
#' @param export_f : functions which should be exported from environment (string or vector of strings)
#' @param cores : number of cores to use for multiprocessing
#'
#' @return vector of @func results
#' 
apply.func.par <- function(text, part.size, func, param, export_f, cores) {
    
    parts = ceiling(length(text)/part.size)
    scope = if (cores<parts) parts else if (length(text)<cores) length(text) else cores
    size = length(text)/scope
    
    cl <- makeCluster(cores)
    registerDoParallel(cl)
    
    result <- foreach (i = 1:scope, .combine=c, .export= export_f) %dopar% {
        start <- floor((i-1)*size) + 1
        end <- floor(i*size)
        func(text[start:end], param)
    }
    
    stopCluster(cl)
    
    return(result)
}