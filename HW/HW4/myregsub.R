my.regsub <- function(matrix,y,nbest,method,nvmax=8){
	temp <- regsubsets(matrix,y,nbest=nbest,method=method,nvmax=nvmax)
	temp.mat <- cbind(summary(temp)$which,
		summary(temp)$rsq,summary(temp)$rss,
		summary(temp)$adjr2,summary(temp)$cp,
		summary(temp)$bic)
	dimnames(temp.mat)[[2]] <- c(dimnames(summary(temp)$which)[[2]],
		"rsq", "rss", "adjr2", "cp", "bic")
	return(temp.mat)
}
