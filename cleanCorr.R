#------------------------------------------------------------------------------
# nombre de la función: cleanCorr
# nombre del fichero: cleanCorr.R
# autor: vcastellar
# fecha: 2017-11-25
#
# argumentos de la función: 
#  - x: data.frame de datos, incluye variables explicativas numéricas y target binario
#  - target: variable binaria respuesta
#  - umbral: umbral por de correlación máxima permitida por variable
#  - IV: por defecto = NULL, en este caso se calcula el IV en la función. Si ya se disponen
#        de estos valores para todas las variables, IV es un data.frame con dos columnas:
#          1º columna: Variable -> Nombre de la variable
#          2º columna:       IV -> Valor IV de esa variable
#
#
# descripción: dado el data.frame M, devuelve una lista de variables explicativas 
#              a eliminar,l de tal forma que las variables restantes ya no 
#              presenten entre sí correlaciones superiores al Umbral.
#              Dado un par de variables con correlación superior al umbral, se
#              elimina aquella cuyo IV  en relación al Target sea menor
#------------------------------------------------------------------------------

cleanCorr <- function(x, target, umbral = 0.80, IV = NULL){
  # carga de librerías necesarias
  require('Information')
  
  # inicializamos vector de variables a eliminar
  vars_deleted <- c()
  
  # añadir validaciones
  #----------------------------------------------------------------------------
  # 1. verificar que son variables numéricas, excepto, en todo caso el target
  # 
  #----------------------------------------------------------------------------
  
  

  # calcular matriz de correlaciones (M) de las variables numéricas
  # esto es, todas excepto el target
  #----------------------------------------------------------------------------
  # obtenemos data.frame de variables numéricas
  xNum <- x[ , !names(x) %in% target]
  # calcula correlaciones
  M <- cor(xNum, use = "pairwise.complete.obs")
  #----------------------------------------------------------------------------
  
  

  # calcular la importancia de cada variable con relación al target. Para ello
  # usaremos el Information Value (IV)
  #----------------------------------------------------------------------------
  if (is.null(IV)){
    IV <- create_infotables(data = x, y = target)$Summary
  }
  #----------------------------------------------------------------------------
  
  
  
  # bucle de filtrado
  #----------------------------------------------------------------------------
  # hacemos ceros en la diagonal de la matriz de correlaciones, para que cuando
  # calculemos el máximo de correlaciones de una variable no salga con ella misma
  diag(M) <- 0
  M <- abs(M)
  repeat{
    
    # Obtenemos pares de variables con máxima correlación que superen el umbral
    maxCorrByVar <- apply(M, 1, max)
    maxCorr <- max(maxCorrByVar)
    if (maxCorr > umbral){
      varsCorr <- names(maxCorrByVar)[maxCorrByVar == maxCorr]
    } else {
      break
    }
    
    # del par de variables obtenido, eliminamos aquella con menor IV:
    IV_varsCorr <- subset(IV, Variable %in% varsCorr)
    var_delete <- with(IV_varsCorr, Variable[which.min(IV)])
    
    # de la matriz de correlaciones, eliminamos la fila y columna de la variable 
    # correspondiente obtenida anteriormente
    index <- which(colnames(M) == var_delete, arr.ind = TRUE)
    M <- M[-index, -index]
    
    # actualizamos vector de variables eliminadas
    vars_deleted <- c(vars_deleted, var_delete)
    
  }
  
  return(list(vars_to_deleted = vars_deleted, M = M))
  
}