stepR2 <- function(Null=Null, Full=Full, umbral=0.0005){
    repeat {
        VarsFull <- attributes(terms(Full))$term.labels
        VarsNull <- attributes(terms(Null))$term.labels
        VarsDiff <- setdiff(VarsFull, VarsNull)
        if(length(VarsDiff)==0) {
            break
        }     
        FormulaNull <- formula(Null)
        VarSelect <- data.frame(var="None", R2=summary(Null)$r.squared, Dif=0)
        for(var in VarsDiff){
            FormulaNew <- update.formula(FormulaNull, formula(paste("~.+",var)))
            NullNew <- update(Null,FormulaNew)
            R2Gain <- summary(NullNew)$r.squared - summary(Null)$r.squared
            VarSelect <- rbind(VarSelect, data.frame(var=var, R2=summary(NullNew)$r.squared, Dif=R2Gain))
        }
        VarSelect <- VarSelect[order(VarSelect$Dif, decreasing=TRUE),]
        if (VarSelect$Dif[1] > umbral ) {
            FormulaNew <- update.formula(FormulaNull, formula(paste("~.+", VarSelect$var[1])))
            Null <- update(Null,FormulaNew)
            cat("\n")
            print(Null$call)
            print(VarSelect)
            cat("\n")
        }
        else {
            return(Null)
            break
        }
    }
return(Null)
}

library(nycflights13)
data(flights)
trainIndex <- sample(1:nrow(flights), 30000)
train <- flights[trainIndex, c("month", "dep_time", "carrier", "origin", "dest", "distance", "hour", "arr_delay")]
test <- flights[-trainIndex, c("month", "dep_time", "carrier", "origin", "dest", "distance", "hour", "arr_delay")]
Null <- lm(arr_delay ~ 1, data = train)
Full <- lm(arr_delay ~ ., data = train)
stepR2(Null,Full, umbral=0.0005)
stepR2(Null,Full, umbral=0.00025) 
