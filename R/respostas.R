##' Para conferir as respostas dos exercícios continuos no "Exercicios.Rmd"
##' @export

#Operadores e operações básicas
sessao1 <- function(a1=NULL, b1=NULL, c1=NULL){

  ## Ex 1
  vec <- c(1.45, 1.73, 1.98, 2.10, 2.03, 1.65, 2.15, 1.63, 1.53, 1.98, 2.05, 1.52)
  med_vec <- mean(vec)
  var_vec <- var(vec)

  if(all(a1 == vec)){
    a1 = TRUE
    cat("1a ok\n")
  } else {
    a1 = FALSE}

  if(b1 == med_vec){
    b1 = TRUE
    cat("1b ok\n")
  } else { b1 = FALSE}

  if(c1 == var_vec){
    c1 = TRUE
    cat("1c ok\n")
  } else { c1 = FALSE}


  ## Ex 2


  ## Ex 3


  resp <- c(a1,b1,c1)


  form_id <- "1Ae79N_Oio1AewGb1eH3lvjw5CiBjTNGt8zEhDQqsmh0"
  post_answers <- googleformr::gformr(form_id)
  post_answers(post_content= resp)
}

##'@export
