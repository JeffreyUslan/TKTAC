#Tea-kettle technical advisory committee

source("tktac_kwh.R")
source("tktac_time.R")


reps=10
old=rep(NA,reps)
old_no_standby=rep(NA,reps)
improved=rep(NA,reps)
improved_no_standby=rep(NA,reps)
for (i in 1:reps) {
    old[i]=tktac_kwh(HLR_old_w_hr_F=.7097,obs_per_minute=12,lnorm_scale=2.5,water_thresh=140,draw=.75,prob_waste=.5,mod=5)
    old_no_standby[i]=tktac_kwh(standby=0,HLR_old_w_hr_F=.7097,obs_per_minute=12,lnorm_scale=2.5,water_thresh=140,draw=.75,prob_waste=.5,mod=5)
    improved[i]=tktac_kwh(HLR_old_w_hr_F=.5846938,obs_per_minute=12,lnorm_scale=2.5,water_thresh=140,draw=.75,prob_waste=.5,mod=5)
    improved_no_standby[i]=tktac_kwh(standby=0,HLR_old_w_hr_F=.5846938,obs_per_minute=12,lnorm_scale=2.5,water_thresh=140,draw=.75,prob_waste=.5,mod=5)
}

summary(old)
summary(old_no_standby)
summary(improved)
summary(improved_no_standby)

energy_savings=round((mean(old)-mean(improved)))
print(paste0("Annual Energy Savings of: ",energy_savings," kWh!"))
percent_savings=100*round(((mean(old)-mean(improved)))/mean(old),3)
print(paste0("That's ",percent_savings," Percent Saved!"))

dollar_savings=energy_savings*.1
print(paste0("We're saving a whole $",dollar_savings," per year!"))



energy_savings=round((mean(old)-mean(improved_no_standby)))
ecotope_sqft=2688
eui_delta=round(energy_savings*3.412/2688,3)
print(paste0("Bringing down our office EUI ",eui_delta," points!"))

scaling=439.6/326.5
nep=259.6*scaling

(439.6-nep)*3.412/ecotope_sqft

tktac_kwh(HLR_old_w_hr_F=.7097,obs_per_minute=2,hours=10,lnorm_scale=2.5,water_thresh=140,draw=.5,prob_waste=.3,mod=3)/(5*52)
tktac_kwh(HLR_old_w_hr_F=.5846938,obs_per_minute=2,hours=10,lnorm_scale=2.5,water_thresh=140,draw=.5,prob_waste=.3,mod=3)/(5*52)

x=seq(0,1,.01)
y=dlnorm(x*2.5)
# y=pbeta(x,1,1)
# Tair for standby lowest t_water 
plot(x,y)

  tktac_time(HLR_old_w_hr_F=.7097,standby=25,obs_per_minute=12,reheat_time_minutes=30)
  tktac_time(HLR_old_w_hr_F=.7097,standby=0,obs_per_minute=12,reheat_time_minutes=30)

  tktac_time(HLR_old_w_hr_F=.5846938,standby=25,obs_per_minute=12,reheat_time_minutes=30)
  tktac_time(HLR_old_w_hr_F=.5846938,standby=0,obs_per_minute=12,reheat_time_minutes=30)

  



