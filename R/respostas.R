##' Para conferir as respostas dos exercícios continuos no "Exercicios.Rmd"

##' Operadores e operações básicas
##' @export
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

  x <- c(round(x1),round(x2))
  xrev <- c(round(x2),round(x1))

  if(all(a3 == x) || all(a3 == xrev)){
    a3 = TRUE
    cat("a3 ok\n")
  } else { a3 = FALSE}

  resp <- c(nome, a1,b1,c1, a2,b2,c2,d2,a3)

  form_id <- "1rYos2doItQUfxNXh29AlCUACnKAa58D8om1WHSr2LXE"
  post_answers <- googleformr::gformr(form_id)
  post_answers(post_content= resp)
}

##' Classe de objetos
##' @export
sessao2 <- function(nome= FALSE, a1=FALSE, b1=FALSE, c1=FALSE, 
                    a2 = FALSE, b2 = FALSE, 
                    a3 = FALSE, b3 = FALSE, c3 = FALSE){
  ## Ex 1
  attach(mtcars)
  cl_mt <- class(mtcars)
  cl_cyl_hp <- c(class(mtcars$cyl),class(mtcars$hp))
  cl_rnames <- class(rownames(mtcars))
  
  if(a1 == cl_mt){
    a1 = TRUE
    cat("1a ok\n")
  } else {
    a1 = FALSE}
  
  if(all(b1 == cl_cyl_hp)){
    b1 = TRUE
    cat("1b ok\n")
  } else { b1 = FALSE}
  
  if(c1 == cl_rnames){
    c1 = TRUE
    cat("1c ok\n")
  } else { c1 = FALSE}
  
  ## Ex 2
  hp_mean <- mean(mtcars$hp)
  #hp_fac_mean <- mean(as.factor(mtcars$hp)) #Resulta em NA
  
  if(a2 == hp_mean){
    a2 = TRUE
    cat("2a ok\n")
  } else {
    a2 = FALSE}
  
  if(is.na(b2)){ #Confere se é NA
    b2 = TRUE
    cat("2b ok\n")
  } else { b2 = FALSE}
  
  ##Ex 3
  list_mam <- list(Nomes= c("Dinho", "Bento", "Samuel", "Sérgio", "Rasec"),
                   Funcao=c("Vocalista","Guitarrista", "Baixista", "Baterista","Tecladista"),
                   Idade = c(24, 25, 22, 26, 28))
  l_idade <- mean(list_mam$Idade)
  l_ordered <- lapply(list_mam, function(x){ x[order(list_mam$Nomes)]})

  if(identical(a3, length(list_mam))){
    a3 = TRUE
    cat("a3 ok\n")
  } else { a3 = FALSE}
  
  if(b3 == l_idade){
    b3 = TRUE
    cat("b3 ok\n")
  } else { b3 = FALSE}
  
  if(identical(c3, l_ordered)){
    c3 = TRUE
    cat("c3 ok\n")
  } else { c3 = FALSE}
  
  resp <- c(nome, a1,b1,c1, a2,b2,a3, b3, c3)
  
  form_id <- "1LyBlGVe8-IK6hpEp40DHIdn10HBcMRGpKhi55nMx1zg"
  post_answers <- googleformr::gformr(form_id)
  post_answers(post_content= resp)

}

##' Indexação
##' @export
sessao3 <- function(nome= FALSE, a1=FALSE, b1=FALSE, c1=FALSE){
  
  ## Ex 1
  set.seed(123456)
  rn_val <- rnorm(n=3000, mean = 20, sd = 5)
  rn_soma <- sum(rn_val[rn_val>30])
  rn_soma_sel <- sum(rn_val[rn_val<mean(rn_val)])
  rn_rem_min <- rn_val[rn_val > min(rn_val)]

  if(a1 == rn_soma){
    a1 = TRUE
    cat("1a ok\n")
  } else {
    a1 = FALSE}
  
  if(b1 == rn_soma_sel){
    b1 = TRUE
    cat("1b ok\n")
  } else { b1 = FALSE}
  
  if(all(c1 == rn_rem_min)){
    c1 = TRUE
    cat("1c ok\n")
  } else { c1 = FALSE}
  
  resp <- c(nome, a1,b1,c1)
  
  form_id <- "1gtTOnXdz1CseC3jBXuLLsA9GNG7fPmX2pWzl-lNg69Y"
  post_answers <- googleformr::gformr(form_id)
  post_answers(post_content= resp)
  
}

##' Estruturas condicionais e de repetição
##'@export
sessao4 <- function(nome= FALSE, a1=FALSE, a2=FALSE){
  ## Ex 1
  a = seq(1,5,1)
  while(sum(a) < 10000){
    a = a + 8
  }
  if(all(a1 == a)){
    a1 = TRUE
    cat("1 ok\n")
  } else {
    a1 = FALSE}
  
  ## Ex 2
  set.seed(123456)
  produtividade = rnorm(50, mean = 3500.00, sd = 800)
  resultado = c()
  for(i in 1:length(produtividade)){
    if (produtividade[i] > 3500.00){
      resultado[i] <- ("acima da média")
    } else {resultado[i] <- ("abaixo da média")}
  }
  
  if(all(a2 == resultado)){
    a2 = TRUE
    cat("2 ok\n")
  } else {
    a2 = FALSE}
  
  resp <- c(nome, a1,a2)
  
  form_id <- "1khpC_c-F9ZpHsxs1r1G-njDT-Hx05xjQMgigCOpaV6g"
  post_answers <- googleformr::gformr(form_id)
  post_answers(post_content= resp)
}




