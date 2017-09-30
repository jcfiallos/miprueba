

if (!("shiny" %in% names(installed.packages()[,"Package"]))) {install.packages("shiny")}
suppressMessages(library(shiny, quietly = TRUE))

if (!("openintro" %in% names(installed.packages()[,"Package"]))) {install.packages("openintro")}
suppressMessages(library(openintro, quietly = TRUE))

if (!("plotrix" %in% names(installed.packages()[,"Package"]))) {install.packages("plotrix")}
suppressMessages(library(plotrix, quietly = TRUE))

library(stats)
library(dygraphs)
library(datasets)
library(xts)
library(forecast)
library(fANCOVA)


#Aqui empieza 

shinyServer(function(input, output,session) {
  
#Carga de datos desde un archivo
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    if (!(is.null(inFile))){
      #Se lee un archivo con los parametros selecionados y se se carga en un dataframe
      df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                     quote = input$quote, dec= input$deci )
      ejex <-  1:nrow(df)
      ejey <- as.numeric(df$V1)
      if (input$frec == 'days') f <- 365
      if (input$frec == 'month') f <- 12
      if (input$frec == 'quarter') f <- 4
      if (input$frec == 'years') f <- 1
      serie=ts(ejey,freq=f, start=input$fecha)
      return(serie)
    }
    else{return = NULL}
  })
  
  #Carga ventana de datos 
  dataw <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    d <- data()
    #ventana de tiempo
    inicioS <- strftime(input$fecha, "%Y-%m-%d")
    inicioV <- strftime(req(input$dygraph_date_window[[1]]), "%Y-%m-%d")
    finV <- strftime(req(input$dygraph_date_window[[2]]), "%Y-%m-%d")
    dia.nuevo.inicio <- difftime(inicioV, inicioS, units = "days") + 1
    dias.nuevaserie <- difftime(finV, inicioV, units = "days")
    dia.nuevo.fin <- dia.nuevo.inicio + dias.nuevaserie
    nuevadata <- as.matrix(d[dia.nuevo.inicio:dia.nuevo.fin])
    ejex <-  1:nrow(nuevadata)
    if (input$frec == 'days') f <- 365
    if (input$frec == 'month') f <- 12
    if (input$frec == 'quarter') f <- 4
    if (input$frec == 'years') f <- 1
    serie=ts(nuevadata,freq=f, start=input$fecha)
    return(serie)
  })
  
  
# grafica la serie con dygraphs   
  output$dygraph <- renderDygraph({
    inFile <- input$file1 
    if (!(is.null(inFile))){
    df <- data()
    ny <- as.matrix(df)
    Date <- seq(input$fecha, length= length(ny), by=input$frec)#preguntar a Aldo para cambiar con frecu
    ndf <- data.frame(ny, Date)
    yxts <- xts(ndf$ny, order.by = ndf$Date, frecuency = input$frec)
    
    dygraph(yxts, main = "Serie Usuario") %>% 
    dyRangeSelector()
    }
  })

  output$from <- renderText({
    strftime(req(input$dygraph_date_window[[1]]), "%Y-%m-%d")      
  })
  
  output$to <- renderText({
       strftime(req(input$dygraph_date_window[[2]]), "%Y-%m-%d")
  })

  
# Modelos para evaluar
  
  #Regresion lineal
  lmResults <- reactive({
    d <- data()
    t <- seq(1:length(d))
    It <- seasonaldummy(d)
    It2 <- seasonaldummy(log(d))
    
    if (!(input$est)){
      if (input$tipo == "Aditivos"){
        if(input$metodo == "Lineal"){
          return <- lm(formula = d ~ t)    
        }
        else{
          if(input$metodo == "Cuadratico"){
            tt <- t^2
            return <- lm(formula = d ~ t + tt)      
          }
          else{
            if(input$metodo == "Cubico"){
              tt <- t^2
              ttt <- t^3
              return <- lm(formula = d ~ t + tt + ttt)      
            }
            else{
              if(input$metodo == "Orden 4"){
                #TENDENCIA Grado cuarta
                tt <- t^2
                ttt <- t^3
                tttt <- t^4
                return <- lm(formula = d ~ t + tt + ttt + tttt)      
              }
            }
          }  
        }
      }# fin if aditivos
      else{
        if (input$tipo == "Multiplicativos"){
          if(input$metodo == "Lineal"){
            return <- lm(formula = log(d) ~ t)
          }
          else{
            if(input$metodo == "Cuadratico"){
              tt <- t^2
              return <- lm(formula = log(d) ~ t + tt)      
            }
            else{
              if(input$metodo == "Cubico"){
                #TENDENCIA CUBICA
                tt <- t^2
                ttt <- t^3
                return <- lm(formula = log(d) ~ t + tt + ttt)      
              }
              else{
                if(input$metodo == "Orden 4"){
                  #TENDENCIA Grado cuarta
                  tt <- t^2
                  ttt <- t^3
                  tttt <- t^4
                  return <- lm(formula = log(d) ~ t + tt + ttt + tttt)      
                }
              }
            }  
          }
        }# fin if multiplicativos        
      }
    }
    else{ # es el bloque para los estacionarios
      if (input$tipo == "Aditivos"){
        if(input$metodo == "Lineal"){
          return <- lm(formula = d ~ t + It)    
        }
        else{
          if(input$metodo == "Cuadratico"){
            tt <- t^2
            return <- lm(formula = d ~ t + tt + It )      
          }
          else{
            if(input$metodo == "Cubico"){
              tt <- t^2
              ttt <- t^3
              return <- lm(formula = d ~ t + tt + ttt + It)      
            }
            else{
              if(input$metodo == "Orden 4"){
                #TENDENCIA Grado cuarta
                tt <- t^2
                ttt <- t^3
                tttt <- t^4
                return <- lm(formula = d ~ t + tt + ttt + tttt + It)      
              }
            }
          }  
        }
      }# fin if aditivos
      else{
        if (input$tipo == "Multiplicativos"){
          if(input$metodo == "Lineal"){
            return <- lm(formula = log(d) ~ t + It2)
          }
          else{
            if(input$metodo == "Cuadratico"){
              tt <- t^2
              return <- lm(formula = log(d) ~ t + tt + It2)      
            }
            else{
              if(input$metodo == "Cubico"){
                #TENDENCIA CUBICA
                tt <- t^2
                ttt <- t^3
                return <- lm(formula = log(d) ~ t + tt + ttt + It2)      
              }
              else{
                if(input$metodo == "Orden 4"){
                  #TENDENCIA Grado cuarta
                  tt <- t^2
                  ttt <- t^3
                  tttt <- t^4
                  return <- lm(formula = log(d) ~ t + tt + ttt + tttt + It2)      
                }
              }
            }  
          }
        }# fin if multiplicativos        
      }
    }
    

  })
  
  #REGRESION_show plots
  output$scatter <- renderPlot({
    y <- data()
    x <- seq(1:length(y))
    
    datos.entreno <- dataw()
    
    #used for confidence interval
    xcon <- seq(min
                (x), max(x), 1)
    
    predictor <- data.frame(x=xcon)
    
    if(input$tipo == "Multiplicativos"){
      yhat2 <- predict(lmResults())
      yhat <- exp(yhat2)
      yline2 <- predict(lmResults(), predictor)
      yline <- exp(yline2)
      r.squared = round(summary((lmResults()))$r.squared, 4)
      newx <- seq(min(x), max(x), length.out=length(x))
      confs2 <- predict((lmResults()), newdata = data.frame(x=newx), 
                       interval = 'confidence')
      confs <- exp(confs2)
      preds2 <- predict(lmResults(), newdata = data.frame(x=newx), 
                       interval = 'predict')
      preds <- exp(preds2)
    }
    else{
      if(input$tipo =="Aditivos"){
        yhat <- predict(lmResults()) 
        yline <- predict(lmResults(), predictor)
        r.squared = round(summary(lmResults())$r.squared, 4)
        newx <- seq(min(x), max(x), length.out=length(x))
        confs <- predict(lmResults(), newdata = data.frame(x=newx), 
                         interval = 'confidence')
        preds <- predict(lmResults(), newdata = data.frame(x=newx), 
                         interval = 'predict')
        
      }
    }
    
    par(cex.main=1, cex.lab=1, cex.axis=1, mar = c(4,4,4,1))
    corr.coef = round(sqrt(r.squared), 4)
  
    plot(c(min(x),max(x)) 
         ,c(min(y,yline),max(y,yline)), 
         type="n",
         xlab="x",
         ylab="y",
         main=paste0("Modelo de Regresion\n","(R = ", corr.coef,", ", "R-cuadrado = ", r.squared,")"))
    
    polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = grey(.95), border = NA)
    polygon(c(rev(newx), newx), c(rev(confs[ ,3]), confs[ ,2]), col = grey(.60), border = NA)
    
    
    points(x,y,pch=21, col=COL[1,4])
    lines(xcon, yline, lwd=1, col="red")
    
    if (input$show.resid) for (j in 1:length(x)) 
      lines(rep(x[j],2), c(yhat[j],y[j]), col=COL[4])
    
    legend_pos = ifelse(lmResults()$coefficients[1] < 1, "topleft", "topright")
    # if(input$type == "linear.down") legend_pos = "topright"
    #if(input$type == "fan.shaped") legend_pos = "topleft"   
    legend(legend_pos, inset=.05,
           legend=c(length(datos.entreno), "AQUI SE LEEN LOS RESULTADOS", "AQUI SE LEEN LOS RESULTADOS"), 
           fill=c(COL[1],grey(.75),grey(.95)))
    box()
  })
  
  output$residuals <- renderPlot({
    par(mfrow=c(1,3), cex.main=1.3, cex.lab=1.3, cex.axis=1.3, mar=c(4,5,2,2))
    residuals = residuals(lmResults())
    predicted = predict(lmResults(), newdata = data.frame(x=data()))
    plot(residuals ~ predicted, 
         main="Residuales vs. Valores Ajustados", xlab="Valores Ajustados", ylab="Residuales", 
         pch=19, col = COL[1,2])
    abline(h = 0, lty = 2)
    d = density(residuals)$y
    h = hist(residuals, plot = FALSE)
    hist(residuals, main="Histograma de Residuales", xlab="Residuales", 
         col=COL[1,2], prob = TRUE, ylim = c(0,max(max(d), max(h$density))))
    lines(density(residuals), col = COL[1], lwd = 2)
    qqnorm(residuals, pch=19, col = COL[1,2], main = "Normal Q-Q Plot de Residuales")
    qqline(residuals, col = COL[1], lwd = 2)
  }, height=280 )
  
  
  #Modelo Holt Winters
  hwResults <- reactive({
    p <- data()
    demand <- ts(p, start = c(2000, 1), frequency = 365)
    t <- seq(1:length(demand))
    
    if (input$tipohw == "Multiplicativos"){
      return <- HoltWinters(demand, alpha=NULL, beta=NULL, gamma=NULL, seasonal = "multiplicative")
    }
    else{
      if(input$tipohw == "Aditivos"){
        return <- HoltWinters(demand, alpha=NULL, beta=NULL, gamma=NULL, seasonal = "additive")
      }
    }
    
  })
  
  
  # HOLT WINTERS_Show plot 
  output$scatter2 <- renderPlot({
    o <- data()
    demand2 <- ts(log(o), start = c(2000, 1), frequency = 365)
    t <- seq(1:length(demand2))
    
    pred <- predict(hwResults(), n.ahead = 730, prediction.interval = T, level = 0.95)
    
    plot(hwResults(),pred, col=1)
    lines(fitted(hwResults())[,1], col=2)
    #plot(x=t,y=o,col=1, type="l")
    #plot(x=t,y=hwResults(), col=3)
    #lines(fitted(hwResults())[,1], col=2)
    
    #if (input$tipohw == "Multiplicativos"){
    #y <- log(y)
    #}
    #x <- seq(1:length(y))
    #plot(x,y, col=1)
    #lines(hwResults(), col=3)
    #lines(fitted(hwResults())[,1], col=2)
    #all <- cbind(yy)
    #ny <- as.matrix(y)
    #Date <- seq(1, length= length(ny), by=365)#preguntar a Aldo para cambiar con frecu
    #ndf <- data.frame(ny, Date)
    #yxts <- xts(ndf$ny, order.by = ndf$Date, frecuency = input$frec)
    #dygraph(yxts, pred) %%
    #dySeries("y", label="Actual") %%
    #dySeries(c("p.lwr", "p.fit", "p.upr"), label = "Predicted")
  })
  
  output$residuals2 <- renderPlot({
    par(mfrow=c(1,3), cex.main=1.3, cex.lab=1.3, cex.axis=1.3, mar=c(4,5,2,2))
    residuals = residuals(hwResults())
    plot(residuals, 
         main="Residuales", xlab="Valores Ajustados", ylab="Residuales", 
         pch=19, col = COL[1,2])
    abline(h = 0, lty = 2)
    d = density(residuals)$y
    h = hist(residuals, plot = FALSE)
    hist(residuals, main="Histograma de Residuales", xlab="Residuales", 
         col=COL[1,2], prob = TRUE, ylim = c(0,max(max(d), max(h$density))))
    lines(density(residuals), col = COL[1], lwd = 2)
    qqnorm(residuals, pch=19, col = COL[1,2], main = "Normal Q-Q Plot de Residuales")
    qqline(residuals, col = COL[1], lwd = 2)
  }, height=280 )
  
  
  
  #LOESS y Descomposicion
  loResults1 <- reactive({
    d <- data()
    o <- ts(d, start = c(2000, 1), frequency = 365)
    t <- seq(1:length(d))
    tnuevo <- (length(d)+1):(length(d)+730)
    Tt <- 1
    St <- 1
    
    
    factoresdeltai=function(descom,s,estacioni){
      if(estacioni==1){
        deltasi=descom$figure
      }
      if(estacioni!=1){
        j=estacioni;deltasi=c(descom$figure[(s-j+2):s],descom$figure[1:(s-j+1)])
      }
      deltasi
    }
    
    if (input$tipolo == "Lineal") grado <- 1 
    if (input$tipolo == "Cuadratico") grado <- 2
    if (input$tipob == "Aditivos"){
      comp <- "additive" 
      ecu <- o-St
    } 
    if (input$tipob == "Multiplicativos") {
      comp <- "multiplicative"
      ecu <- o/St
    }
    
    descom=decompose(o,type = comp)
    St=descom$seasonal
    deltas_i=factoresdeltai(descom=descom,s=365,estacioni = 1)
    data.frame(deltas_i)
    o.no.St=(ecu)
    
    ajusteloess=loess.as(t,o.no.St,degree = grado,criterion = "aicc",family = "gaussian",plot = F)
    Tt=ts(fitted(ajusteloess),frequency = 365,start = c(2000,1))
    alpha.optimo=ajusteloess$pars$span #guardando el alpha optimo
    
    if (input$tipob == "Aditivos"){
      modelo <- Tt+St
    } 
    if (input$tipob == "Multiplicativos"){
      modelo <- Tt*St
    }
    return(modelo)
  })    
  
  loResults2 <- reactive({
    d <- data()
    o <- ts(d, start = c(2000, 1), frequency = 365)
    t <- seq(1:length(d))
    tnuevo <- (length(o)+1):(length(o)+1395)
    Ttnuevo <- 1
    Stnuevo <- 1
    
    factoresdeltai=function(descom,s,estacioni){
      if(estacioni==1){
        deltasi=descom$figure
      }
      if(estacioni!=1){
        j=estacioni;deltasi=c(descom$figure[(s-j+2):s],descom$figure[1:(s-j+1)])
      }
      deltasi
    }
    
    if (input$tipolo == "Lineal") grado <- 1 
    if (input$tipolo == "Cuadratico") grado <- 2
    if (input$tipob == "Aditivos"){
      comp <- "additive" 
      ecu <- o-Stnuevo
    } 
    if (input$tipob == "Multiplicativos") {
      comp <- "multiplicative"
      ecu <- o/Stnuevo
    }
    
    descom=decompose(o,type = comp)
    St=descom$seasonal
    deltas_i=factoresdeltai(descom=descom,s=365,estacioni = 1)
    data.frame(deltas_i)
    o.no.St=(ecu)
    
    ajusteloess=loess.as(t,o.no.St,degree = grado,criterion = "aicc",family = "gaussian",plot = F)
    Tt=ts(fitted(ajusteloess),frequency = 365,start = c(2000,1))
    alpha.optimo=ajusteloess$pars$span #guardando el alpha optimo
    
    l <- seq(1:365)
    i=c(l,l,l) #identificando la estacion correspondiente a los m=15 periodos de pronostico
    Stnuevo=deltas_i[i] #asignando los delta_i a los periodos a pronosticar
    Stnuevo=ts(Stnuevo,frequency=365,start=c(2010,365)) #convertimos en serie el pronostico de St
    
    o.no.St=(ecu)
    
    Ttnuevo=predict(loess(o.no.St~t,span = alpha.optimo, degree = grado,control = loess.control(surface = "direct")),data.frame(t=tnuevo),se=FALSE)
    Ttnuevo=ts(Ttnuevo,frequency=365,start = c(2010,365)) #Convirtiendo en serie de tiempo al pronostico de Tt
    
    if (input$tipob == "Aditivos"){
      modelopron <- Ttnuevo+Stnuevo
    } 
    if (input$tipob == "Multiplicativos"){
      modelopron <- Ttnuevo*Stnuevo
    }
    return(modelopron)
  })
  
  
  #LOESS
  output$scatter3 <- renderPlot({
    y <- data()
    o <- ts(y, start = c(2000, 1), frequency = 365)
    
    plot(o,col=1)
    lines(loResults1(), col=2)
    lines(loResults2(), col=3)
    #lines(y.fit.hat2,col=2)
    #legend("topleft",legend=c("Original","Ajustada", "Pronostico"),col=c(1,2,3),lty = 1)
    
  })
})
