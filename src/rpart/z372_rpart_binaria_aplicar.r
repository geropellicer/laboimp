#Este script está pensado para correr en Google Cloud
# debe  cambiase el setwd() si se desea correr en Windows

#Aplicacion de los mejores hiperparametros encontrados en una bayesiana
#Utilizando clase_binaria =  [  SI = { "BAJA+1", "BAJA+2"} ,  NO="CONTINUA ]

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")


#Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/home/devsar/Documents/Austral/Laboratorio de Implementación")  #Establezco el Working Directory

#cargo el dataset
dataset  <- fread("./datasets/competencia1_2022_FE_3.csv" )


#creo la clase_binaria SI={ BAJA+1, BAJA+2 }    NO={ CONTINUA }
dataset[ foto_mes==202101, 
         clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]



dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dapply  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo


# Entreno el modelo
# obviamente rpart no puede ve  clase_ternaria para predecir  clase_binaria
variables = "ctrx_quarter + g_saldo_total + mpayroll + mv_status02 + mpasivos_margen + mcaja_ahorro + mv_status06 + ctarjeta_master_debitos_automaticos + mtarjeta_visa_consumo + mrentabilidad_annual + g_ratio_90_30 + mprestamos_personales + mcuentas_saldo + tcallcenter + mcuenta_corriente + cpayroll_trx + ccallcenter_transacciones + cliente_antiguedad + mv_msaldototal + cprestamos_personales + mcuenta_corriente_adicional + cdescubierto_preacordado + cliente_edad + mtarjeta_master_consumo + ccomisiones_mantenimiento + ccuenta_debitos_automaticos + mv_mpagominimo + mv_Fvencimiento + mtransferencias_recibidas + mactivos_margen + ctarjeta_master + catm_trx_other + mtransferencias_emitidas + mv_status04 + ctarjeta_visa_debitos_automaticos + mrentabilidad + mcaja_ahorro_adicional + mttarjeta_visa_debitos_automaticos + ccajas_depositos + numero_de_cliente + ccomisiones_otras + mv_mpagado + ctarjeta_visa_transacciones + mpayroll2 + mcaja_ahorro_dolares + ctarjeta_debito + mttarjeta_master_debitos_automaticos + mforex_sell + ctarjeta_visa + ccajas_consultas + mcomisiones_otras + mcomisiones + matm + mcomisiones_mantenimiento + tmobile_app + g_ctxs_virtuales + ctarjeta_master_descuentos + ctarjeta_debito_transacciones + g_prop_txs_app + mpagomiscuentas + mcheques_emitidos + internet + g_ctxs_remotas + mv_mconsumospesos + mtarjeta_master_descuentos + ccajas_extracciones + ctransferencias_emitidas + cpagomiscuentas + cmobile_app_trx + cproductos + mcheques_depositados + mautoservicio + mcuenta_debitos_automaticos + ctarjeta_visa_descuentos + mprestamos_hipotecarios + cinversion2 + mextraccion_autoservicio + mv_status03 + matm_other + ccajeros_propios_descuentos + mv_mconsumosdolares + cseguro_vivienda + g_prom_90_mayor_prom_30 + g_esta_complicado + g_es_activo_virtual + g_es_activo_remoto + g_usa_pago_mis_cuentas + mv_FechaAlta"
my_formula = paste("clase_binaria ~", variables)
modelo  <- rpart(formula=   my_formula,
                 data=      dtrain,  #los datos donde voy a entrenar
                 xval=          0,
                 cp=           -0.38,
                 minsplit=    897,
                 minbucket=   3,
                 maxdepth=     14   )



#aplico el modelo a los datos nuevos
prediccion  <- predict( object=  modelo,
                        newdata= dapply,
                        type = "prob")

#prediccion es una matriz con DOS columnas, llamadas "NO", "SI"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dfinal  <- copy( dapply[ , list(numero_de_cliente) ] )
dfinal[ , prob_SI := prediccion[ , "SI"] ]


# por favor cambiar por una semilla propia
# que sino el Fiscal General va a impugnar la prediccion
set.seed(700027)  
dfinal[ , azar := runif( nrow(dapply) ) ]

# ordeno en forma descentente, y cuando coincide la probabilidad, al azar
setorder( dfinal, -prob_SI, azar )


dir.create( "./exp/" )
dir.create( "./exp/KA3720" )


for( corte  in  c( 7500, 8000, 8500, 9000, 9500, 10000, 10500, 11000 ) )
{
  #le envio a los  corte  mejores,  de mayor probabilidad de prob_SI
  dfinal[ , Predicted := 0L ]
  dfinal[ 1:corte , Predicted := 1L ]


  fwrite( dfinal[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
           file= paste0( "./exp/KA3720/KA3720_002_",  corte, ".csv"),
           sep=  "," )
}
