AugCoeffMatrix <- function(system){
  if (!isValid(system))
    return (NA)
  else{
  rownames = NULL;
  colnames = NULL;
  size = length(system);
  i=1;
  
  while (i <= size){
    rownames = c(rownames,i);
    colnames = c(colnames,paste("x",i,sep=""));
    i = i + 1;
  }
  
  colnames = c(colnames,"RHS")

  #instantiate matrix
  m = matrix(0,nrow=size,ncol=size+1,dimnames = list(rownames,colnames), byrow = TRUE)
  variables = substring(deparse(system[[1]])[1],11,nchar(deparse(system[[1]])[1])-2)

  for (s in 1:length(system)){
    equation_string = deparse(system[[s]])[2];
    equation_term = strsplit(equation_string," + ",fixed=TRUE);
    equation_term=list(strsplit(equation_term[[1]]," ",fixed=TRUE));

    for (col in 1:length(colnames)){
      if(is.na(equation_term[[1]][[col]][3])){
        m[s,"RHS"] = equation_term[[1]][[col]][1]
      }
      else{
      m[s,equation_term[[1]][[col]][3]] = equation_term[[1]][[col]][1]
      }
    }
  }
  
  return (list(augcoeffmatrix = m,variables = getVariables(system)))
  }
}

getVariables <- function (system){
  var = deparse(system[[1]])[1];
  var = substring(var,11,nchar(var)-2)
  var = strsplit(var,", ",fixed=TRUE)
  return (var[[1]])
}
isValid <- function (system) {
  numOfTerms = deparse(system[[1]])[2];
  numOfTerms = strsplit(numOfTerms," + ",fixed = TRUE)
  numOfTerms = length(numOfTerms[[1]])
  for (s in system){
    temp = deparse(s)[2];
    temp = strsplit(temp," + ",fixed = TRUE)
    temp = length(temp[[1]])
    print(temp)
    if (temp != numOfTerms)
      return (FALSE)
  }
  return (TRUE)
}

E1 <- function (x1, x2, x3) 0.3 * x1 + 10 * x3 + -71.4+ -0.2 * x2;
E2 <- function (x1, x2, x3) 3 * x1 + -0.2 * x2 + -0.1 * x3 + -7.85;
E3 <- function (x1, x2, x3) 0.1 * x1 + 7 * x2 + -0.3 * x3  + 19.3;

system <- list(E1, E2, E3);

result = AugCoeffMatrix(system);
#result
#result$variables
#result$augcoeffmatrix
isValid(system)
