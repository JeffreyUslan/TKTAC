
#monte-carlo simulations
tktac_kwh<-function(standby=25,active=1400,HLR_old_w_hr_F,draw=.5,lnorm_scale=5,T_air=70,bp=212,water_thresh=120
                    ,hours=10,obs_per_minute=6,tap_water=73,kettle_mass_cap=3.315,prob_waste=.3,mod=3) {
  #setting up the prelims
  time_scale=1/obs_per_minute
  obs=(60*hours/time_scale)
  
  time=1:obs
  df=data.frame(time=time)
  
  df$m_water=NA
  df$m_water[1]=kettle_mass_cap
  
  
  df$T_water=NA
  check_night= sample(c(1,0),prob=c(prob_waste,1-prob_waste),1)
  if (check_night==1){
    print("Someone left the kettle on standby overnight :(")
    df$T_water[1]=tap_water+(standby/HLR_old_w_hr_F)
  } else {
    df$T_water[1]=tap_water  
  }
  
  df$btu_input=NA
  df$btu_input_adj=NA
  df$btu_input[1]=(standby+active)*3.412
  
  df$Delta_T=NA
  
  
  df$Delta_T[1]=df$T_water[1]-T_air
  
  
  df$btu_input_adj[1]=df$btu_input[1]-(HLR_old_w_hr_F*3.412)*df$Delta_T[1]
  
  draw_count=0
  for (i in 2:obs) {
    #   Draw 8 oz at a rate of log-normal distribution
    check1=dlnorm(i*lnorm_scale/obs)/(1/time_scale)/mod
    check= sample(c(1,0),prob=c(check1,1-check1),1)
    
    #draw if not currently heating and check is 1
    if (df$btu_input_adj[i-1]<0 & check==1) {    
      
      # some people reheat at any temperature
      check_waste= sample(c(1,0),prob=c(prob_waste,1-prob_waste),1)
      if (check_waste==1) {
        df$btu_input[i]=(standby+active)*3.412
        df$m_water[i]=df$m_water[i-1]    
      } else {
        df$m_water[i]=df$m_water[i-1]-draw        
        draw_count=draw_count+1
      }
      
      
    } else {
      df$m_water[i]=df$m_water[i-1]    
    }
    
    #   refill water when too low
    if (df$m_water[i]>draw) {
      df$T_water[i]=df$T_water[i-1]+time_scale*df$btu_input_adj[i-1]/(df$m_water[i]*60) 
    } else {
      df$m_water[i]=kettle_mass_cap
      df$T_water[i]=(df$m_water[i]*df$T_water[i-1]+(kettle_mass_cap-df$m_water[i])*tap_water)/kettle_mass_cap
      df$btu_input[i]=(standby+active)*3.412
    }
    # delta T 
    df$Delta_T[i]=df$T_water[i]-T_air
    
    # turn off blue light when boiling 
    if (df$T_water[i]>bp) {
      df$T_water[i]=bp
      df$m_water[i]=df$m_water[i-1]-draw
      draw_count=draw_count+1
      #     df$m_water[i]=df$m_water[i-1]-.5
    }
    # refill when water is too cool and someone wants water  
    if (is.na(df$btu_input[i])){
      if ((df$T_water[i]<bp & df$T_water[i]>df$T_water[i-1]) |  (df$T_water[i-1]<water_thresh & check==1))  {
        df$btu_input[i]=(standby+active)*3.412
        df$m_water[i]=kettle_mass_cap
        df$T_water[i]=(df$m_water[i]*df$T_water[i]+(kettle_mass_cap-df$m_water[i])*tap_water)/kettle_mass_cap
      }  else {
        df$btu_input[i]=standby*3.412
      }
    }
    
    df$btu_input_adj[i]=df$btu_input[i]-(HLR_old_w_hr_F*3.412)*df$Delta_T[i]  
  }
  print(paste0(draw_count, " Draws"))
  
  
  annual_kwh=round((mean(df$btu_input)*5*hours*52/(3.412*1000)))
  
  if (check_night==1){
    annual_kwh=annual_kwh+(24-hours)*standby*5*52/1000
  }
  
  
  strt=1
  df$hour=df$time*time_scale/60
  plot(df$hour[strt:obs],df$T_water[strt:obs],type="l", col="red",ann=FALSE)
  title(main="Tea Kettle Water Temperature Simulation",xlab="Hour of Workday", ylab="Water Temperature (F)")
  
  return(annual_kwh)
  
}


