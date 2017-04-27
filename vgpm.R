#Productivity using VGPM for southern Arabian Sea

#Csat is the chlorophyll pigment from satellite
Ctot<- function(Csat){if (Csat<1){38.0*(Csat)^0.425} else {40.2*(Csat)^0.507}}
#Pbopt is optimal rate of carbon fixation, where "Temp" is SST
pbopt<- function(Temp){1.2956 +2.749*(10^-1)* Temp+6.17*(10^-2)*(Temp^2)-2.05*(10^-2)*(Temp^3)+2.462*(10^-3)*(Temp^4)-1.348*(10^-4)*(Temp^5)+3.4132*(10^-6)*(Temp^6)-3.27*(10^-8)*(Temp^7)}
#Phyical depth of euphotic zone
Zeu<- function(Ctot){568.2*(Ctot)^-0.746}
#Diir is daily photoperiod
Dirr<- function(Dirr){Dirr}
#E0 is the PAR
E0<- function(E0){E0}
#PPeu is the total producticity using VGPM

PPeu<- function(Temp,Csat,Dirr,E0){
  pboptx<- pbopt(Temp)
  Ctotx<- Ctot(Csat)
  Zeux<- Zeu(Ctotx)
  Dirrx<- Dirr(Dirr)
  E0x<- E0(E0)
  final<- 0.66125*pboptx* (E0x/(E0x+4.1))*Csat*Zeux*Dirrx
  return(final)
  return(Zeux)
}
#Apply VGPM to a List
xPP<- lapply(August,function(x)x$x$PP<- PPeu(Temp = x$SST,Csat = x$Chl,Dirr = x$DL,E0 = x$PAR))

