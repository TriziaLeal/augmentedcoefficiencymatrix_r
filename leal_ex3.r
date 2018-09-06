AugCoeffMatrix <- function(system){
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
  print(m)
  
  for (s in system){
    equation_string = deparse(s)[2];
    equation_term = strsplit(equation_string," + ",fixed=TRUE);
    count = 1
    matrix_in_list = NULL
    while (count <= size){

      for (term in equation_term[[1]]){
        if(substring(term[1],nchar(term[1]),nchar(term[1])) == count){
          coeff = strsplit(term[1], " ");
          #print(coeff[[1]][1]);
          matrix_in_list = c(matrix_in_list,coeff);
        }
        print(matrix_in_list);
      }
      count = count + 1
    }
  }
  

}


E1 <- function (x1, x2, x3) 0.3 * x1 + -0.2 * x2 + 10 * x3 + -71.4;
E2 <- function (x1, x2, x3) 3 * x1 + -0.2 * x2 + -0.1 * x3 + -7.85;
E3 <- function (x1, x2, x3) 0.1 * x1 + 7 * x2 + -0.3 * x3 + 19.3;

system <- list(E1, E2, E3);

result = AugCoeffMatrix(system);
result
result$variables
result$augcoeffmatrix
