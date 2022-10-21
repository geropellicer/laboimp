#require vm con
#   8 vCPU
#  64 GB  memoria RAM
# 256 GB  espacio en disco


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")



#Parametros del script
kexperimento  <- "DR7141"

kexp_input  <- "CA7060"
  
#valores posibles "ninguno" "rank_simple" , "rank_cero_fijo" , "deflacion"
kmetodo <- "deflacion"

# FIN Parametros del script


#------------------------------------------------------------------------------
#Esta es la parte que los alumnos deben desplegar todo su ingenio

AgregarVariables  <- function( dataset )
{
  gc()
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas

  #creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
  dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
  dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
  dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
  dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

  #variable extraida de una tesis de maestria de Irlanda
  dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , vm_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , vm_status02       := Master_status +  Visa_status ]
  dataset[ , vm_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , vm_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , vm_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

  dataset[ , vm_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]

  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]


  #combino MasterCard y Visa
  dataset[ , vm_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

  dataset[ , vm_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , vm_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , vm_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , vm_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , vm_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , vm_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , vm_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , vm_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , vm_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , vm_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , vm_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , vm_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , vm_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , vm_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , vm_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , vm_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , vm_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , vm_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , vm_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , vmr_Master_mlimitecompra:= Master_mlimitecompra / vm_mlimitecompra ]
  dataset[ , vmr_Visa_mlimitecompra  := Visa_mlimitecompra / vm_mlimitecompra ]
  dataset[ , vmr_msaldototal         := vm_msaldototal / vm_mlimitecompra ]
  dataset[ , vmr_msaldopesos         := vm_msaldopesos / vm_mlimitecompra ]
  dataset[ , vmr_msaldopesos2        := vm_msaldopesos / vm_msaldototal ]
  dataset[ , vmr_msaldodolares       := vm_msaldodolares / vm_mlimitecompra ]
  dataset[ , vmr_msaldodolares2      := vm_msaldodolares / vm_msaldototal ]
  dataset[ , vmr_mconsumospesos      := vm_mconsumospesos / vm_mlimitecompra ]
  dataset[ , vmr_mconsumosdolares    := vm_mconsumosdolares / vm_mlimitecompra ]
  dataset[ , vmr_madelantopesos      := vm_madelantopesos / vm_mlimitecompra ]
  dataset[ , vmr_madelantodolares    := vm_madelantodolares / vm_mlimitecompra ]
  dataset[ , vmr_mpagado             := vm_mpagado / vm_mlimitecompra ]
  dataset[ , vmr_mpagospesos         := vm_mpagospesos / vm_mlimitecompra ]
  dataset[ , vmr_mpagosdolares       := vm_mpagosdolares / vm_mlimitecompra ]
  dataset[ , vmr_mconsumototal       := vm_mconsumototal  / vm_mlimitecompra ]
  dataset[ , vmr_mpagominimo         := vm_mpagominimo  / vm_mlimitecompra ]

  #Aqui debe usted agregar sus propias nuevas variables
  dataset[, g_prom_90_mayor_prom_30 := ifelse(
    (ctrx_quarter_normalizado / 90) > 
      (rowSums( cbind( catm_trx_other,  ctarjeta_debito_transacciones, cforex_buy, cforex_sell, cpagodeservicios, ctransferencias_emitidas, cextraccion_autoservicio, catm_trx) , na.rm=TRUE )/30),
    TRUE,
    FALSE)]
  dataset[, g_ratio_90_30 := (ctrx_quarter_normalizado / 90) / (rowSums( cbind( catm_trx_other, ctarjeta_debito_transacciones, cforex_buy, cforex_sell, cpagodeservicios, ctransferencias_emitidas, cextraccion_autoservicio, catm_trx) , na.rm=TRUE )/30)]
  dataset[, g_esta_complicado := ifelse(rowSums( cbind(Master_delinquency,  Visa_delinquency, Master_status, Visa_status, na.rm = TRUE)) > 0, TRUE, FALSE )]
  dataset[, g_m_saldo_total := rowSums( cbind( mcuentas_saldo,  vm_msaldototal) , na.rm=TRUE ) ]
  dataset[, g_ctxs_virtuales := rowSums( cbind( chomebanking_transacciones,  cmobile_app_trx) , na.rm=TRUE ) ]
  dataset[, g_ctxs_remotas := rowSums( cbind( chomebanking_transacciones,  cmobile_app_trx, ccallcenter_transacciones) , na.rm=TRUE ) ]
  dataset[, g_prop_txs_app :=  ctrx_quarter_normalizado / g_ctxs_virtuales]
  dataset[, g_es_activo_virtual := ifelse(g_ctxs_virtuales > 5, TRUE, FALSE)]
  dataset[, g_es_activo_remoto := ifelse(g_ctxs_remotas > 5, TRUE, FALSE)]
  dataset[, g_usa_pago_mis_cuentas := ifelse(cpagomiscuentas > 0, TRUE, FALSE)]
  dataset[, g_promedio_mensual_quarter := ctrx_quarter_normalizado /3]
  dataset[, g_cobra_salario := rowSums( cbind( mpayroll,  mpayroll2) , na.rm=TRUE ) ]
  dataset[, g_tiene_prestamos := rowSums( cbind( mpayroll,  mpayroll2) , na.rm=TRUE ) ]
  dataset[, g_tiene_inversiones := ifelse(rowSums( cbind( cinversion1,  cinversion2) , na.rm=TRUE ) > 0, TRUE, FALSE) ]
  dataset[, g_posee_prestamos := ifelse(rowSums( cbind( cprestamos_personales,  cprestamos_prendarios, cprestamos_hipotecarios) , na.rm=TRUE ) > 0, TRUE, FALSE) ]
  dataset[, g_tiene_saldos_negativos := ifelse(pmin(mcaja_ahorro, mcaja_ahorro_adicional, mcaja_ahorro_dolares, mcuenta_corriente, mcuenta_corriente_adicional, na.rm=TRUE) < 0, TRUE, FALSE)]
  dataset[, g_na_count := rowSums(is.na(dataset))]
  dataset[, g_cliente_porcentaje_vida_adulta := cliente_antiguedad / (cliente_edad-18) ]
  dataset[, g_m_mpayrolls_sobre_edad  := rowSums( cbind( mpayroll,  mpayroll2) , na.rm=TRUE ) / cliente_edad ]
  dataset[, g_m_mpayroll2_sobre_edad  := mpayroll2 / cliente_edad ]
  dataset[, g_cpayrolls_sobre_edad  := rowSums( cbind( cpayroll_trx,  cpayroll2_trx) , na.rm=TRUE ) / cliente_edad ]
  dataset[, g_m_monto_inversiones_totales := rowSums( cbind( minversion1_pesos,  minversion2) , na.rm=TRUE )]
  dataset[, g_cantidad_inversiones_totales := rowSums( cbind( cinversion1,  cinversion2) , na.rm=TRUE )]
  dataset[, g_m_monto_inversiones_sobre_saldo := g_m_monto_inversiones_totales / g_m_saldo_total]
  dataset[, g_cantidad_inversiones_sobre_saldo := g_cantidad_inversiones_totales / g_m_saldo_total]
  dataset[, g_m_monto_promedio_por_inversion := g_m_monto_inversiones_totales / g_cantidad_inversiones_totales]
  dataset[, g_ratio_rentabilidad_mensual_sobre_anual := mrentabilidad / mrentabilidad_annual]
  dataset[, g_m_rentabilidad_historica := rowSums( cbind( mactivos_margen,  mpasivos_margen) , na.rm=TRUE )]
  dataset[, g_ratio_rentabilidad_anual_sobre_historica := mrentabilidad_annual / g_m_rentabilidad_historica]
  dataset[, g_m_rentabilidad_mensual_versus_promedio_rentabilidad_anual :=  rowSums( cbind( -mrentabilidad,  mrentabilidad_annual/12) , na.rm=TRUE )]
  dataset[, g_m_rentabilidad_mensual_versus_promedio_rentabilidad_historica :=  rowSums( cbind( -mrentabilidad,  g_m_rentabilidad_historica/cliente_antiguedad) , na.rm=TRUE )]
  

  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }

}

#------------------------------------------------------------------------------
#corrige drift por Dolarización

drift_dolarizacion  <- function( campos_monetarios )
{
  # Me aseguro que los "Campos monetarios" son los que estoy esperando imprimiendolos
  for( campo in campos_monetarios )
  {
    cat( campo, " " )
  }

  vfoto_mes <- c( 201901, 201902, 201903, 201904, 201905, 201906,
                  201907, 201908, 201909, 201910, 201911, 201912,
                  202001, 202002, 202003, 202004, 202005, 202006,
                  202007, 202008, 202009, 202010, 202011, 202012,
                  202101, 202102, 202103, 202104, 202105 )

  # El valor del dólar de cada mes está calculado de la siguiente manera: 
  # por cada día de cotización disponible en https://www.ambito.com/contenidos/dolar-informal-historico.html
  # entre el 1 de enero y el 31 de mayo de 2021, se agrupan por mes y se calcula el promedio de los valores
  # de todos los días del mes, tanto compra como venta. 
  vDolar <- c( 38.051, 37.778, 40.982, 43.747, 45.409, 44.313, 
               43.233, 53.414, 59.220, 64.136, 65.250, 70.237, 
               75.295, 76.192, 80.276, 97.413, 121.237, 120.857, 
               126.978, 131.040, 135.364, 167.619, 157.400, 150.053, 
               155.150, 147.000, 141.115, 143.750, 150.763 )

  tb_dolar  <- data.table( "foto_mes"= vfoto_mes,
                         "dolar" = vDolar )

  dataset[ tb_dolar,
           on= c("foto_mes"),
           (campos_monetarios) :=  .SD / i.dolar ,
           .SDcols = campos_monetarios ]

}


#------------------------------------------------------------------------------
#deflaciona por IPC
#momento 1.0  31-dic-2020 a las 23:59

drift_deflacion  <- function( campos_monetarios )
{
  # Me aseguro que los "Campos monetarios" son los que estoy esperando imprimiendolos
  for( campo in campos_monetarios )
  {
    cat( campo, " " )
  }

  vfoto_mes <- c( 201901, 201902, 201903, 201904, 201905, 201906,
                  201907, 201908, 201909, 201910, 201911, 201912,
                  202001, 202002, 202003, 202004, 202005, 202006,
                  202007, 202008, 202009, 202010, 202011, 202012,
                  202101, 202102, 202103, 202104, 202105 )

  vIPC  <- c( 1.9903030878, 1.9174403544, 1.8296186587,
              1.7728862972, 1.7212488323, 1.6776304408,
              1.6431248196, 1.5814483345, 1.4947526791,
              1.4484037589, 1.3913580777, 1.3404220402,
              1.3154288912, 1.2921698342, 1.2472681797,
              1.2300475145, 1.2118694724, 1.1881073259,
              1.1693969743, 1.1375456949, 1.1065619600,
              1.0681100000, 1.0370000000, 1.0000000000,
              0.9680542110, 0.9344152616, 0.8882274350,
              0.8532444140, 0.8251880213 )

  tb_IPC  <- data.table( "foto_mes"= vfoto_mes,
                         "IPC" = vIPC )

  dataset[ tb_IPC,
           on= c("foto_mes"),
           (campos_monetarios) :=  .SD * i.IPC ,
           .SDcols = campos_monetarios ]

}

#------------------------------------------------------------------------------

drift_rank_simple  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[ , paste0(campo,"_rank") :=  (frank( get(campo), ties.method="random") - 1) / ( .N -1 ), by= foto_mes]
    dataset[ , (campo) := NULL ]
  }
}
#------------------------------------------------------------------------------
#El cero se transforma en cero
#los positivos se rankean por su lado
#los negativos se rankean por su lado

drift_rank_cero_fijo  <- function( campos_drift )
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[ get(campo) ==0, paste0(campo,"_rank") := 0 ]
    dataset[ get(campo) > 0, paste0(campo,"_rank") :=   frank(  get(campo), ties.method="random")  / .N, by= foto_mes ]
    dataset[ get(campo) < 0, paste0(campo,"_rank") :=  -frank( -get(campo), ties.method="random")  / .N, by= foto_mes ]
    dataset[ , (campo) := NULL ]
  }
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui comienza el programa

setwd("~/buckets/b1")

#cargo el dataset donde voy a entrenar
#esta en la carpeta del exp_input y siempre se llama  dataset.csv.gz
dataset_input  <- paste0( "./exp/", kexp_input, "/dataset.csv.gz" )
dataset  <- fread( dataset_input )

#creo la carpeta donde va el experimento
dir.create( paste0( "./exp/", kexperimento, "/"), showWarnings = FALSE )
setwd(paste0( "./exp/", kexperimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO



#primero agrego las variables manuales
AgregarVariables( dataset )

setorder( dataset, foto_mes, numero_de_cliente )

#por como armé los nombres de campos, estos son los campos que expresan variables monetarias
campos_monetarios  <- colnames(dataset)
campos_monetarios  <- campos_monetarios[campos_monetarios %like% "^(m|Visa_m|Master_m|vm_m|g_m_)"]

#aqui aplico un metodo para atacar el data drifting
#hay que probar experimentalmente cual funciona mejor
switch( 
kmetodo,
  "ninguno"        = cat( "No hay correccion del data drifting" ),
  "rank_simple"    = drift_rank_simple( campos_monetarios ),
  "rank_cero_fijo" = drift_rank_cero_fijo( campos_monetarios ),
  "deflacion"      = drift_deflacion( campos_monetarios ),
  "dolarizacion"   = drift_dolarizacion( campos_monetarios ) # se agrega esta linea y la coma final de la linea anterior
)



fwrite( dataset,
        file="dataset.csv.gz",
        sep= "," )
