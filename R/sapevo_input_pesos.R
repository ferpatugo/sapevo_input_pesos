#' @title teste
#'
#' @description teste
#'
#' @param symbol
#'
#' @return NULL
#'
#' @examples
#'
#' @export

#SAPEVO INPUT PESOS

sapevo_input_pesos=function(){
  
  
  projeto = readline("Qual o nome do projeto? ")  
  
  decisores = readline("Quem sao os decisores? ")
  decisores = unlist(strsplit(decisores, ","))
  
  alternativas = readline("Quais sao as alternativas? ")
  alternativas = unlist(strsplit(alternativas, ","))
  
  criterios = readline("Quais são os critérios do projeto? ")
  criterios = unlist(strsplit(criterios, ","))
  
  numero_decisores=length(unlist(strsplit(decisores, ",")))
  numero_alternativas=length(unlist(strsplit(alternativas, ",")))
  
  
  criterios = unlist(strsplit(criterios, ","))
  numero_criterios=length(unlist(strsplit(criterios, ",")))
  
  vetor_final=list()
  
  for (i in 1:numero_decisores) {
    
    
    if (numero_criterios==2) {
      
      nota_decisor1_comparacao1 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[1],"vs",criterios[2],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      
      
      nota_decisor1_comparacao1=as.numeric(nota_decisor1_comparacao1)
      
      
      
      vetor_final[[i]]=c(0,nota_decisor1_comparacao1,-nota_decisor1_comparacao1,0)
      
      assign("vetor_final",vetor_final,envir = .GlobalEnv)
      assign("decisores",decisores,envir = .GlobalEnv)
      assign("alternativas",alternativas,envir = .GlobalEnv)
      assign("criterios",criterios,envir = .GlobalEnv)
      
      cat("O nome do seu projeto ?:",projeto,"\n")
      cat("Os decisores do seu projeto ?:",decisores,"\n")
      cat("Aternativas do seu projeto:",alternativas,"\n")
      cat("Criterios do seu projeto:",criterios,"\n")
      cat("O input para a funcao sapevo_pesos é o vetor:\n")
      print(vetor_final)
      
    } else if (numero_criterios==3) {
      
      
      nota_decisor1_comparacao1 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[1],"vs",criterios[2],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao1=as.numeric(nota_decisor1_comparacao1)
      
      
      nota_decisor1_comparacao2 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[1],"vs",criterios[3],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao2=as.numeric(nota_decisor1_comparacao2)
      
      
      nota_decisor1_comparacao3 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[2],"vs",criterios[3],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao3=as.numeric(nota_decisor1_comparacao3)
      
      vetor_final[[i]]=c(0,nota_decisor1_comparacao1,nota_decisor1_comparacao2,
                         -nota_decisor1_comparacao1,0,nota_decisor1_comparacao3,
                         -nota_decisor1_comparacao2,-nota_decisor1_comparacao3,0)
      
      assign("vetor_final",vetor_final,envir = .GlobalEnv)
      assign("decisores",decisores,envir = .GlobalEnv)
      assign("alternativas",alternativas,envir = .GlobalEnv)
      assign("criterios",criterios,envir = .GlobalEnv)
      
      cat("O nome do seu projeto ?:",projeto,"\n")
      cat("Os decisores do seu projeto ?:",decisores,"\n")
      cat("Aternativas do seu projeto:",alternativas,"\n")
      cat("Criterios do seu projeto:",criterios,"\n")
      cat("O input para a funcao sapevo_pesos é o vetor:\n")
      print(vetor_final)
      
    } else if (numero_criterios==4) {
      
      nota_decisor1_comparacao1 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[1],"vs",criterios[2],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao1=as.numeric(nota_decisor1_comparacao1)
      
      
      nota_decisor1_comparacao2 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[1],"vs",criterios[3],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao2=as.numeric(nota_decisor1_comparacao2)
      
      
      nota_decisor1_comparacao3 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[1],"vs",criterios[4],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao3=as.numeric(nota_decisor1_comparacao3)
      
      
      nota_decisor1_comparacao4 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[2],"vs",criterios[3],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao4=as.numeric(nota_decisor1_comparacao3)
      
      
      nota_decisor1_comparacao5 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[2],"vs",criterios[4],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao5=as.numeric(nota_decisor1_comparacao3)
      
      nota_decisor1_comparacao6 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[3],"vs",criterios[4],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao6 = as.numeric(nota_decisor1_comparacao3)
      
      nota_decisor1_comparacao7 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[4],"vs",criterios[4],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao7 = as.numeric(nota_decisor1_comparacao3)
      
      
      vetor_final[[i]]=c(0,nota_decisor1_comparacao1,nota_decisor1_comparacao2,nota_decisor1_comparacao3,
                         -nota_decisor1_comparacao1,0,nota_decisor1_comparacao4,nota_decisor1_comparacao5,
                         -nota_decisor1_comparacao2,-nota_decisor1_comparacao4,0,nota_decisor1_comparacao6,
                         -nota_decisor1_comparacao3,-nota_decisor1_comparacao5, -nota_decisor1_comparacao7,0)
      
      assign("vetor_final",vetor_final,envir = .GlobalEnv)
      assign("decisores",decisores,envir = .GlobalEnv)
      assign("alternativas",alternativas,envir = .GlobalEnv)
      assign("criterios",criterios,envir = .GlobalEnv)
      
      cat("O nome do seu projeto ?:",projeto,"\n")
      cat("Os decisores do seu projeto ?:",decisores,"\n")
      cat("Aternativas do seu projeto:",alternativas,"\n")
      cat("Criterios do seu projeto:",criterios,"\n")
      cat("O input para a funcao sapevo_pesos é o vetor:\n")
      print(vetor_final)
      
    } else if (numero_criterios==5) {
      
      
      nota_decisor1_comparacao1 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[1],"vs",criterios[2],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      
      nota_decisor1_comparacao1=as.numeric(nota_decisor1_comparacao1)
      
      
      nota_decisor1_comparacao2 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[1],"vs",criterios[3],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      
      nota_decisor1_comparacao2=as.numeric(nota_decisor1_comparacao2)
      
      
      nota_decisor1_comparacao3 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[1],"vs",criterios[4],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao3=as.numeric(nota_decisor1_comparacao3)
      
      
      nota_decisor1_comparacao4 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[1],"vs",criterios[5],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao4=as.numeric(nota_decisor1_comparacao3)
      
      
      nota_decisor1_comparacao5 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[2],"vs",criterios[3],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao5=as.numeric(nota_decisor1_comparacao3)
      
      
      nota_decisor1_comparacao6 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[2],"vs",criterios[4],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao6=as.numeric(nota_decisor1_comparacao3)
      
      
      nota_decisor1_comparacao7 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[2],"vs",criterios[5],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao7 = as.numeric(nota_decisor1_comparacao7)
      
      
      nota_decisor1_comparacao8 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[3],"vs",criterios[4],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao8 = as.numeric(nota_decisor1_comparacao3)
      
      
      nota_decisor1_comparacao9 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[3],"vs",criterios[5],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao9 = as.numeric(nota_decisor1_comparacao3)
      
      
      nota_decisor1_comparacao10 = readline(as.character(cat("Avaliar critérios:",decisores[i],"\n\n",criterios[4],"vs",criterios[5],"\n\n","Diga uma Nota comparativa : Abolutamente pior (-3), Muito pior (-2), Pior (-1), Equivalente (0), Melhor (1), Muito melhor (2), Absolutamente melhor (3)")))
      
      nota_decisor1_comparacao10 = as.numeric(nota_decisor1_comparacao3)
      
      
      vetor_final[[i]]=c(0,nota_decisor1_comparacao1,nota_decisor1_comparacao2,nota_decisor1_comparacao3,nota_decisor1_comparacao4,
                         -nota_decisor1_comparacao1,0,nota_decisor1_comparacao5,nota_decisor1_comparacao6,nota_decisor1_comparacao7,
                         -nota_decisor1_comparacao2,-nota_decisor1_comparacao5,0,nota_decisor1_comparacao8,nota_decisor1_comparacao9 ,
                         -nota_decisor1_comparacao3,-nota_decisor1_comparacao6, -nota_decisor1_comparacao7 ,0,nota_decisor1_comparacao10,
                         -nota_decisor1_comparacao4,-nota_decisor1_comparacao7,-nota_decisor1_comparacao9,-nota_decisor1_comparacao10,0)
      
      
      assign("vetor_final",vetor_final,envir = .GlobalEnv)
      assign("decisores",decisores,envir = .GlobalEnv)
      assign("alternativas",alternativas,envir = .GlobalEnv)
      assign("criterios",criterios,envir = .GlobalEnv)
      
      cat("O nome do seu projeto ?:",projeto,"\n")
      cat("Os decisores do seu projeto ?:",decisores,"\n")
      cat("Aternativas do seu projeto:",alternativas,"\n")
      cat("Criterios do seu projeto:",criterios,"\n")
      cat("O input para a funcao sapevo_pesos é o vetor:\n")
      print(vetor_final)
      
    } else {
      
      cat("")
      
      
    }
    
  }     
}     
