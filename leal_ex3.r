AugCoeffMatrix <- function(system){
  if (!isValid(system))
    return (NA)
  else{
  rownames = NULL;
  colnames = NULL;
  
  for (i in 1:length(system)){
    rownames = c(rownames,i)
  }
  for (i in 1:length(getVariables(system))){
    colnames = c(colnames,paste("x",sep="",i))
  }
  colnames = c(colnames,"RHS")

  #instantiate matrix
  m = matrix(0,nrow=length(rownames),ncol=length(colnames),dimnames = list(rownames,colnames), byrow = TRUE)
  for (row in 1:length(rownames)){
    equation_string = deparse(system[[row]])[2];
    equation_term = strsplit(equation_string," + ",fixed=TRUE);
    equation_term = list(strsplit(equation_term[[1]]," ",fixed=TRUE));
    
    #assign values of the matrix
    for (col in 1:length(colnames)){
      if(is.na(equation_term[[1]][[col]][3])){
        m[row,"RHS"] = equation_term[[1]][[col]][1]
      }
      else{
      m[row,equation_term[[1]][[col]][3]] = equation_term[[1]][[col]][1]
      }
    }
  }
  
 return (list(augcoeffmatrix = m,variables = getVariables(system)))
  }
}

#gets variables of the functions
getVariables <- function (system){
  var = deparse(system[[1]])[1];
  var = substring(var,11,nchar(var)-2)
  var = strsplit(var,", ",fixed=TRUE)
  return (var[[1]])
}

#checks validity of the functions
isValid <- function (system) {
  numOfTerms = deparse(system[[1]])[2];
  numOfTerms = strsplit(numOfTerms," + ",fixed = TRUE)
  numOfTerms = length(numOfTerms[[1]])
  for (s in system){
    temp = deparse(s)[2];
    temp = strsplit(temp," + ",fixed = TRUE)
    temp = length(temp[[1]])
    if (temp != numOfTerms)
      return (FALSE)
  }
  return (TRUE)
}

E1 <- function (x1, x2, x3, x4) 11 * x1 + 13 * x3 + 14 + 12 * x2 + 1 * x4;
E2 <- function (x1, x2, x3, x4) 21 * x1 + 22 * x2 + 23 * x3 + 24 + 2 * x4;
E3 <- function (x1, x2, x3, x4) 31 * x1 + 32 * x2 + 33 * x3  + 34 + 3 * x4;
E4 <- function (x1, x2, x3, x4) 41 * x1 + 42 * x2 + 43 * x3  + 44  + 4 * x4;

system <- list(E1, E2, E3, E4);

result = AugCoeffMatrix(system);
print(result)
print(result$augcoeffmatrix)
