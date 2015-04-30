#Tea-kettle technical advisory committee

#Establishing constant values
standby=25
active=1400
HLR_old_w_hr_F=.7097
HLR_new_w_hr_F=.5846938

T_air=60
bp=212
#should we vary the mass?
m_water=3.315
time_diff=1/12
heat_freq=30/time_diff

#setting up the prelims
obs=(600/time_diff)
time=1:obs
df=data.frame(time=time)

df$heat_up=df$time%%heat_freq
df$heat_up[which(df$heat_up!=1)]=0

df$T_water=NA
df$T_water[1]=73

df$T_water_new=NA
df$T_water_new[1]=73

df$Watt_input=NA
df$Watt_input_adj=NA
df$Watt_input[which(df$heat_up==1)]=(standby+active)*3.412

df$Watt_input_new=NA
df$Watt_input_adj_new=NA
df$Watt_input_new[which(df$heat_up==1)]=(standby+active)*3.412

df$Delta_T=NA
df$Delta_T_new=NA
  
df$Delta_T[1]=df$T_water[1]-T_air
df$Delta_T_new[1]=df$T_water_new[1]-T_air

df$Watt_input_adj[1]=df$Watt_input[1]-(HLR_old_w_hr_F*3.412)*df$Delta_T[1]
df$Watt_input_adj_new[1]=df$Watt_input_new[1]-(HLR_new_w_hr_F*3.412)*df$Delta_T_new[1]

for (i in 2:obs) {
  df$T_water[i]=df$T_water[i-1]+time_diff*df$Watt_input_adj[i-1]/(m_water*60) 
  df$T_water_new[i]=df$T_water_new[i-1]+time_diff*df$Watt_input_adj_new[i-1]/(m_water*60) 
  
  df$Delta_T[i]=df$T_water[i]-T_air
  df$Delta_T_new[i]=df$T_water_new[i]-T_air
  
  if (df$T_water[i]>bp) {df$T_water[i]=bp}
  if (df$T_water_new[i]>bp) {df$T_water_new[i]=bp}
  
  
  if (is.na(df$Watt_input[i])){
    if (df$T_water[i]<bp & df$T_water[i]>df$T_water[i-1]) {
      df$Watt_input[i]=(standby+active)*3.412
    }  else {
      df$Watt_input[i]=standby*3.412
    }
  }
  if (is.na(df$Watt_input_new[i])){
    if (df$T_water_new[i]<bp & df$T_water_new[i]>df$T_water_new[i-1]) {
      df$Watt_input_new[i]=(standby+active)*3.412
    }  else {
      df$Watt_input_new[i]=standby*3.412
    }
  }
  
  df$Watt_input_adj[i]=df$Watt_input[i]-(HLR_old_w_hr_F*3.412)*df$Delta_T[i]  
  df$Watt_input_adj_new[i]=df$Watt_input_new[i]-(HLR_new_w_hr_F*3.412)*df$Delta_T_new[i]  
  
}

summary(df$Watt_input)
summary(df$Watt_input_new)

plot(df$time,df$T_water,type="l", col="red",)
lines(df$time,df$T_water_new,type="l", col.main="blue",pch=22)

energy_savings=round((mean(df$Watt_input)-mean(df$Watt_input_new))*50*52/(3.412*1000))
print(paste0("Annual Energy Savings of: ",energy_savings," kWh!"))
percent_savings=100*round(((mean(df$Watt_input)-mean(df$Watt_input_new))*50*52/(3.412*1000))/mean(df$Watt_input),3)
print(paste0("That's ",percent_savings," Percent!"))

#  plot(df$time,df$Watt_input,type="l", col="red",)
#  lines(df$time,df$Watt_input_new,type="l", col.main="blue",pch=22)
hfhr=(70/time_diff)
strt=20/time_diff
plot(df$time[strt:hfhr],df$T_water[strt:hfhr],type="l", col="red",)
lines(df$time[strt:hfhr],df$T_water_new[strt:hfhr],type="l", col.main="blue",pch=22)


