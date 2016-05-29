require(doParallel)

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