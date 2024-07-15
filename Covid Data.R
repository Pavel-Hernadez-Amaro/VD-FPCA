
id_Temp=id_muchas_ctes_Temp[which(id_muchas_ctes_Temp%in%intersect(id_muchas_ctes_Sat_capi,intersect(id_muchas_ctes_Temp,id_muchas_ctes_Frec_Card)))]
id_Frec_Card=id_muchas_ctes_Frec_Card[which(id_muchas_ctes_Frec_Card%in%intersect(id_muchas_ctes_Sat_capi,intersect(id_muchas_ctes_Temp,id_muchas_ctes_Frec_Card)))]
id_Capi=id_muchas_ctes_Sat_capi[which(id_muchas_ctes_Sat_capi%in%intersect(id_muchas_ctes_Sat_capi,intersect(id_muchas_ctes_Temp,id_muchas_ctes_Frec_Card)))]

ID=id_Temp

Sat_capi_pos=which(as.double(ctes_x_persona$V9[-1])>60)

Sat_capi_number_of_tests=as.double(ctes_x_persona[Sat_capi_pos+1,9][1:(length(Sat_capi_pos)-1)])
Sat_capi_results=Sat_capi_fecha=Sat_capi_hora=Sat_capi_minuto=matrix(nrow=length(ID), ncol=max(Sat_capi_number_of_tests))

for (ind in 1:length(ID)) {

  print(ind)
  aux=which(which(datos_ctes$Id_Paciente==ID[ind]) %in% which(datos_ctes$Etiqueta__form_cte_=="saturaciono")==TRUE)

  aux_Sat_Capi=datos_ctes$Valor__form_cte_[which(datos_ctes$Id_Paciente==ID[ind])][aux]

  Sat_capi_results[ind,1:length(aux_Sat_Capi)]=aux_Sat_Capi

  Sat_capi_fecha[ind,1:length(aux_Sat_Capi)]=as.character(datos_ctes$Fecha__form_cte_[which(datos_ctes$Id_Paciente==ID[ind])][aux])
  Sat_capi_hora[ind,1:length(aux_Sat_Capi)]=datos_ctes$hora[which(datos_ctes$Id_Paciente==ID[ind])][aux]
  Sat_capi_minuto[ind,1:length(aux_Sat_Capi)]=datos_ctes$minuto[which(datos_ctes$Id_Paciente==ID[ind])][aux]

  # fecha[ind,1:length(aux_Sat_Capi)]=order(Sat_capi_fecha[ind,],na.last = NA)
  # hora[ind,1:length(aux_Sat_Capi)]=order(Sat_capi_hora[ind,],na.last = NA)
  # minuto[ind,1:length(aux_Sat_Capi)]=order(Sat_capi_minuto[ind,],na.last = NA)

}

aux_col=ceiling(which.min(as.double(as.Date(Sat_capi_fecha)))/nrow(Sat_capi_fecha))
aux_row=which.min(as.double(as.Date(Sat_capi_fecha)))-nrow(Sat_capi_fecha)*(aux_col-1)


aux_date=which(Sat_capi_fecha==Sat_capi_fecha[aux_row,aux_col])
Sat_capi_hora[aux_date]
Sat_capi_minuto[aux_date]

# ceiling(14711/nrow(Sat_capi_fecha))
# 14711-nrow(Sat_capi_fecha)*(94-1)

## first test at 21:46 of 2020-03-10 and it is located in [14,97]

date_start=min(as.double(as.Date(Sat_capi_fecha)),na.rm=1)
time_start=min(Sat_capi_hora[aux_date])+(min(Sat_capi_minuto[aux_date])/60)

orden_Sat_capi=observation_points_Sat_capi=matrix(nrow = nrow(Sat_capi_fecha), ncol = ncol(Sat_capi_fecha))

for (i in 1:nrow(Sat_capi_fecha)) {

  date_diff=as.double(as.Date(Sat_capi_fecha[i,]))-date_start
  current_time=Sat_capi_hora[i,] + (Sat_capi_minuto[i,]/60)

  observation_points_Sat_capi[i,]=(24*date_diff)-time_start+current_time
  orden_Sat_capi[i,] = order(observation_points_Sat_capi[i,])
  # observation_points_Sat_capi[i,]=observation_points_Sat_capi[i,orden_Sat_capi[i,]]
}

Sat_capi_results[7,orden_Sat_capi[7,]]

plot(Sat_capi_results[13,orden_Sat_capi[13,]], type="l", ylim=c(50,100))
lines(Sat_capi_results[1,orden_Sat_capi[1,]], col=2)
lines(Sat_capi_results[2,orden_Sat_capi[2,]], col=3)
lines(Sat_capi_results[3,orden_Sat_capi[3,]], col=4)

################################
################################
################################
################################

Temp_pos=which(as.double(ctes_x_persona$V16[-1])>60)

Temp_number_of_tests=as.double(ctes_x_persona[Temp_pos+1,16][1:(length(Temp_pos)-1)])
Temp_results=Temp_fecha=Temp_hora=Temp_minuto=matrix(nrow=length(ID), ncol=max(Temp_number_of_tests))

# fecha=hora=minuto=matrix(nrow=length(ID), ncol=max(Temp_number_of_tests))

for (ind in 1:length(ID)) {

  print(ind)
  aux=which(which(datos_ctes$Id_Paciente==ID[ind]) %in% which(datos_ctes$Etiqueta__form_cte_=="temperatura")==TRUE)

  aux_Temp=datos_ctes$Valor__form_cte_[which(datos_ctes$Id_Paciente==ID[ind])][aux]

  Temp_results[ind,1:length(aux_Temp)]=aux_Temp

  Temp_fecha[ind,1:length(aux_Temp)]=as.character(datos_ctes$Fecha__form_cte_[which(datos_ctes$Id_Paciente==ID[ind])][aux])
  Temp_hora[ind,1:length(aux_Temp)]=datos_ctes$hora[which(datos_ctes$Id_Paciente==ID[ind])][aux]
  Temp_minuto[ind,1:length(aux_Temp)]=datos_ctes$minuto[which(datos_ctes$Id_Paciente==ID[ind])][aux]

  # fecha[ind,1:length(aux_Temp)]=order(Temp_fecha[ind,],na.last = NA)
  # hora[ind,1:length(aux_Temp)]=order(Temp_hora[ind,],na.last = NA)
  # minuto[ind,1:length(aux_Temp)]=order(Temp_minuto[ind,],na.last = NA)



}

aux_col=ceiling(which.min(as.double(as.Date(Temp_fecha)))/nrow(Temp_fecha))
aux_row=which.min(as.double(as.Date(Temp_fecha)))-nrow(Temp_fecha)*(aux_col-1)


aux_date=which(Temp_fecha==Temp_fecha[aux_row,aux_col])
Temp_hora[aux_date]
Temp_minuto[aux_date]

# ceiling(11204/nrow(Temp_fecha))
# 11204-nrow(Temp_fecha)*(100-1)

## first test at 21:46 of 2020-03-10 and it is located in [17,100]

date_start=min(as.double(as.Date(Temp_fecha)),na.rm=1)
time_start=min(Temp_hora[aux_date])+(min(Temp_minuto[aux_date])/60)

orden_Temp=observation_points_Temp=matrix(nrow = nrow(Temp_fecha), ncol = ncol(Temp_fecha))

for (i in 1:nrow(Temp_fecha)) {

  date_diff=as.double(as.Date(Temp_fecha[i,]))-date_start
  current_time=Temp_hora[i,] + (Temp_minuto[i,]/60)

  observation_points_Temp[i,]=(24*date_diff)-time_start+current_time
  orden_Temp[i,] = order(observation_points_Temp[i,])
  # observation_points_Temp[i,]=observation_points_Temp[i,orden_Temp[i,]]
}

Temp_results[63,orden_Temp[63,]]
Temp_fecha[13,orden_Temp[13,]]
Temp_hora[17,orden_Temp[17,]]

plot(Temp_results[10,orden_Temp[10,]], type="l", ylim=c(30,40), xlim=c(0,150))
# lines(Temp_results[83,orden_Temp[83,]], col=2)
lines(Temp_results[2,orden_Temp[2,]], col=3)
# lines(Temp_results[3,orden_Temp[3,]], col=4)
lines(Temp_results[4,orden_Temp[4,]], col=5)

################################
################################
################################
################################

Frec_Card_pos=which(as.double(ctes_x_persona$V12[-1])>60)

Frec_Card_number_of_tests=as.double(ctes_x_persona[Frec_Card_pos+1,12][1:(length(Frec_Card_pos)-1)])
Frec_Card_results=Frec_Card_fecha=Frec_Card_hora=Frec_Card_minuto=matrix(nrow=length(ID), ncol=max(Frec_Card_number_of_tests))

# fecha=hora=minuto=matrix(nrow=length(ID), ncol=max(Frec_Card_number_of_tests))

for (ind in 1:length(ID)) {

  print(ind)
  aux=which(which(datos_ctes$Id_Paciente==ID[ind]) %in% which(datos_ctes$Etiqueta__form_cte_=="frecuenciac")==TRUE)

  aux_Frec_Card=datos_ctes$Valor__form_cte_[which(datos_ctes$Id_Paciente==ID[ind])][aux]

  Frec_Card_results[ind,1:length(aux_Frec_Card)]=aux_Frec_Card

  Frec_Card_fecha[ind,1:length(aux_Frec_Card)]=as.character(datos_ctes$Fecha__form_cte_[which(datos_ctes$Id_Paciente==ID[ind])][aux])
  Frec_Card_hora[ind,1:length(aux_Frec_Card)]=datos_ctes$hora[which(datos_ctes$Id_Paciente==ID[ind])][aux]
  Frec_Card_minuto[ind,1:length(aux_Frec_Card)]=datos_ctes$minuto[which(datos_ctes$Id_Paciente==ID[ind])][aux]

  # fecha[ind,1:length(aux_Frec_Card)]=order(Frec_Card_fecha[ind,],na.last = NA)
  # hora[ind,1:length(aux_Frec_Card)]=order(Frec_Card_hora[ind,],na.last = NA)
  # minuto[ind,1:length(aux_Frec_Card)]=order(Frec_Card_minuto[ind,],na.last = NA)



}

aux_col=ceiling(which.min(as.double(as.Date(Frec_Card_fecha)))/nrow(Frec_Card_fecha))
aux_row=which.min(as.double(as.Date(Frec_Card_fecha)))-nrow(Frec_Card_fecha)*(aux_col-1)


aux_date=which(Frec_Card_fecha==Frec_Card_fecha[aux_row,aux_col])
Frec_Card_hora[aux_date]
Frec_Card_minuto[aux_date]

# ceiling(5502/nrow(Frec_Card_fecha))
# 5502-nrow(Frec_Card_fecha)*(83-1)

## first test at 21:46 of 2020-03-10 and it is located in [8,83]

date_start=min(as.double(as.Date(Frec_Card_fecha)),na.rm=1)
time_start=min(Frec_Card_hora[aux_date])+(min(Frec_Card_minuto[aux_date])/60)

orden_Frec_Card=observation_points_Frec_Card=matrix(nrow = nrow(Frec_Card_fecha), ncol = ncol(Frec_Card_fecha))

for (i in 1:nrow(Frec_Card_fecha)) {

  date_diff=as.double(as.Date(Frec_Card_fecha[i,]))-date_start
  current_time=Frec_Card_hora[i,] + (Frec_Card_minuto[i,]/60)

  observation_points_Frec_Card[i,]=(24*date_diff)-time_start+current_time
  orden_Frec_Card[i,] = order(observation_points_Frec_Card[i,])
  # observation_points_Frec_Card[i,]=observation_points_Frec_Card[i,orden_Frec_Card[i,]]
}

Frec_Card_results[58,orden_Frec_Card[58,]]
Frec_Card_fecha[13,orden_Frec_Card[13,]]
Frec_Card_hora[17,orden_Frec_Card[17,]]

plot(Frec_Card_results[1,orden_Frec_Card[1,]], type="l", ylim=c(50,140), xlim=c(0,255))
lines(Frec_Card_results[58,orden_Frec_Card[58,]], col=2)
lines(Frec_Card_results[2,orden_Frec_Card[2,]], col=3)
# lines(Frec_Card_results[3,orden_Frec_Card[3,]], col=4)
lines(Frec_Card_results[4,orden_Frec_Card[4,]], col=5)

length(intersect(ID,intersect(id_muchas_ctes_Temp,id_muchas_ctes_Frec_Card)))

# Replace commas with decimal points
Temp_results <- gsub(",", ".", Temp_results)
Sat_capi_results <- gsub(",", ".", Sat_capi_results)
Frec_Card_results <- gsub(",", ".", Frec_Card_results)

Temperature=matrix(as.double(Temp_results), nrow=nrow(Temp_results))
Capilar=matrix(as.double(Sat_capi_results),nrow=nrow(Sat_capi_results))
Frec_Card=matrix(as.double(Frec_Card_results), nrow=nrow(Frec_Card_results))

# for (ind in 1:nrow(Temperature)) {
#
#   Temperature[ind,]=Temperature[ind,orden_Temp[ind,]]
#   Capilar[ind,]=Capilar[ind,orden_Sat_capi[ind,]]
#   Frec_Card[ind,]=Frec_Card[ind,orden_Frec_Card[ind,]]
# }

# all.equal(Temperature[1,1:65],as.double(Temp_results[1,orden_Temp[1,]][1:65]))

observation_points_temp_cap=matrix(nrow = nrow(observation_points_Temp), ncol= ncol(observation_points_Temp))

for (ind in 1:nrow(Temperature)) {

  aux=intersect(observation_points_Temp[ind,],observation_points_Sat_capi[ind,])
  aux=head(aux,-1)

  observation_points_temp_cap[ind,1:length(aux)]=sort(aux)
  aux_index=which(observation_points_temp_cap[ind,1:length(aux)] %in% observation_points_Temp[ind,])

  Temperature[ind,1:length(aux_index)]=Temperature[ind,aux_index][order(aux)]
  Temperature[ind,(length(aux_index)+1):ncol(Temperature)]=NA

  Capilar[ind,1:length(aux_index)]=Capilar[ind,aux_index][order(aux)]
  Capilar[ind,(length(aux_index)+1):ncol(Capilar)]=NA

}


M_temp=apply(X = Temperature, MARGIN = 1, FUN = function(x) sum(!is.na(x)) )
M_capilar=apply(X = Capilar, MARGIN = 1, FUN = function(x) sum(!is.na(x)) )
# M_Frec_Card=apply(X = Frec_Card, MARGIN = 1, FUN = function(x) sum(!is.na(x)) )

# all.equal(M_capilar,M_temp)

Temperature=Temperature[order(M_temp),]
Capilar=Capilar[order(M_capilar),]
# Frec_Card=Frec_Card[order(M_Frec_Card),]
#

M_temp=sort(M_temp)
M_capilar=sort(M_capilar)
# M_Frec_Card=sort(M_Frec_Card)
