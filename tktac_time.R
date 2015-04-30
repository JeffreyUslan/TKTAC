

tktac_time=function(standby=25,active=1400,HLR_old_w_hr_F,T_air=70,bp=212,hours=2,obs_per_minute=6,
                    tap_water=73,kettle_mass_cap=3.315,reheat_time_minutes=60){
  time_scale=1/obs_per_minute
  obs=(60*hours/time_scale)
  
  time=1:obs
  df=data.frame(time=time)
  
  df$m_water=NA
  df$m_water[1]=kettle_mass_cap
  
  
  df$T_water=NA
  df$T_water[1]=212  
  
  df$btu_input=NA
  df$btu_input_adj=NA
  #start in a state of cooling
  df$btu_input[1]=(standby)*3.412
  
  df$Delta_T=NA
  df$Delta_T[1]=df$T_water[1]-T_air
  
  df$btu_input_adj[1]=df$btu_input[1]-(HLR_old_w_hr_F*3.412)*df$Delta_T[1]
  reheat_time=0
  for (i in 2:obs) {
    df$m_water[i]=df$m_water[i-1]    
    df$T_water[i]=df$T_water[i-1]+time_scale*df$btu_input_adj[i-1]/(df$m_water[i]*60) 
    df$Delta_T[i]=df$T_water[i]-T_air
    
    # refill when water is too cool and someone wants water  
    
    if (is.na(df$btu_input[i])){
      if ((df$T_water[i]<bp & df$T_water[i]>df$T_water[i-1]))  {
        df$btu_input[i]=(25+active)*3.412
        df$m_water[i]=kettle_mass_cap
        df$T_water[i]=(df$m_water[i]*df$T_water[i]+(kettle_mass_cap-df$m_water[i])*tap_water)/kettle_mass_cap
      }  else {
        df$btu_input[i]=standby*3.412
      }
    }
    
    if (i>(reheat_time_minutes/time_scale)){
      
      if (reheat_time==0){
        this=df$T_water[i]
        print(paste0(this," F water before reboil")) 
      }
      
      df$btu_input[i]=(25+active)*3.412
      reheat_time=reheat_time+1
      
      if (df$T_water[i]>=bp){
        reheat_minutes=reheat_time*time_scale
        break
        
      }
    }
    
    df$btu_input_adj[i]=df$btu_input[i]-(HLR_old_w_hr_F*3.412)*df$Delta_T[i]  
  }  
  
  return(reheat_minutes)
  
  
}

