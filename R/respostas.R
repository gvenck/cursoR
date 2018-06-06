##' Para conferir as respostas dos exercícios continuos no "Exercicios.Rmd"
##' @export

#Operadores e operações básicas
sessao1 <- function(nome= FALSE, a1=FALSE, b1=FALSE, c1=FALSE, a2 = FALSE, b2 = FALSE, c2 = FALSE, d2 = FALSE, a3 = FALSE){

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

  peso <- c(72,57,49,62,57,90,53,76,63,92)
  altura <- c(1.65,1.68,1.53,1.7,1.52,1.82,1.63,1.78,1.72,1.98)

  IMC <- peso/altura^2
  md_IMC <- mean(IMC)
  var_IMC <- var(IMC)
  sd_IMC <- sd(IMC)

    if(all(a2 == IMC)){
      a2 = TRUE
      cat("2a ok\n")
    } else {
      a2 = FALSE}

      if(b2 == md_IMC){
      b2 = TRUE
      cat("2b ok\n")
    } else { b2 = FALSE}


    if(c2 == var_IMC){
      c2 = TRUE
      cat("2c ok\n")
    } else { c2 = FALSE}

      if(d2 == sd_IMC){
      d2 = TRUE
      cat("2d ok\n")
    } else { d2 = FALSE}

  ## Ex 3

  x1 <- (-8 - sqrt(8^2 - 4*2*6))/2*4
  x2 <- (-8 + sqrt(8^2 - 4*2*6))/2*4

  x <- c(x1,x2)


    if(all(a3 == x)){
      a3 = TRUE
      cat("a3 ok\n")
    } else { a3 = FALSE}

  resp <- c(nome, a1,b1,c1, a2,b2,c2,d2,a3)

  form_id <- "1rYos2doItQUfxNXh29AlCUACnKAa58D8om1WHSr2LXE"
  post_answers <- googleformr::gformr(form_id)
  post_answers(post_content= resp)
}

