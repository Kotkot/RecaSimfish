##' Makes a distance or contact matrix consistent with list of farm id's. Default values are filled in for farm id's without information.
##'
##' 
##' @title Makes a distance or contact matrix consistent with list of farm id's
##' @param matr Matrix or data frame with dimension farm1 x farm1 and farm id's as rom- and columnnames
##' @param locno Integer vector with farm id's, of length farm2
##' @param default.val.diag Value on dialog elements for farm id's not found in input matr
##' @param default.val.offdiag Value on off-dialog elements for farm id's not found in input matr
##' @param txt Text string to be printed, typical the type of matrix
##' @return Matrix with dimension farm2 x farm2, with rows and columns corresponding to locno at input
##' @author Magne Aldrin
make.consistent.matrix <- function(matr,locno,default.val.diag,default.val.offdiag,txt="") {

m<-length(locno)
res<-matrix(default.val.offdiag,m,m,dimnames=list(as.character(locno),as.character(locno)))
diag(res)<-default.val.diag
locno.matr<-as.numeric(rownames(matr))
if (any(locno.matr != as.numeric(colnames(matr)))) {stop("colnames !=rownames")}

id<-is.element(locno,locno.matr)
locno.red<-locno[id]
locno.red.place<-(1:m)[id]

res[locno.red.place,locno.red.place]<-matr[as.character(locno.red),as.character(locno.red)]

missing<-sum(!id)
print(paste(missing,"of elements in locno are missing in matr, and are replaced by default values!",txt))

id<-is.element(locno.matr,locno)
missing<-sum(!id)
print(paste(missing,"of elements in matr are missing in locno, and are deleted!",txt))


res
}
