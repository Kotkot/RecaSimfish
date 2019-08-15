extract.numbers.from.stringvec<-function(strvec){
### Extract the first number in in each string in a vector of strings.
### The number can have one or more digits.

res<-as.numeric(regmatches(strvec, regexpr('\\d+',strvec)))

res
}
