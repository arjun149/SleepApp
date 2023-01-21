# @Arcascope

  # Light processing parameters
  Alpha_0 <- 0.05
  P <- 1.5
  I_0 <- 9325.0
  Delta <- 0.0075
  G <- 33.75
  
  # Circadian model parameters
  Irecep <- 1 / 291.0
  Targc <- 0.9677
  Tau <- 23.84
  Beta1 <- -0.09318
  A1    <- 0.3855
  A2    <- 0.1977
  BetaL1<- -0.0026 
  BetaL2<- -0.957756
  Sigma <- 0.0400692
  K     <- 0.06458
  Gamma <- 0.024
  
  # Circadian model functions
  Bhat <- function (lightActivation, rawControl) {
    G * (1.0 - lightActivation) * alphaL(rawControl)
  }
  
  alphaL <- function(control) {
    Alpha_0 * pow(control, P) / (pow(control, P) + I_0)
  }
  
  phaseResponseCurve <- function(amplitude, phase) {
    amplitudePow3 <- amplitude*amplitude*amplitude
    amplitudePow4 <- amplitudePow3*amplitude
    amplitudePow8 <- amplitudePow4*amplitudePow4
    firstTerm <-  A1 * 0.5 * (amplitudePow3 + 1.0 / amplitude) * sin(phase + BetaL1)
    secondTerm <-  A2 * 0.5 * (1.0 + amplitudePow8) * sin(2.0 * phase + BetaL2)
    Sigma - firstTerm - secondTerm
  }
  
  amplitudeResponseCurve <- function(amplitude, phase){
    amplitudePow4 <- pow(amplitude, 4)
    amplitudePow8 <- pow(amplitudePow4, 2)
    firstTerm <- A1 * 0.5 * (1.0 - amplitudePow4) * cos(phase + BetaL1)
    secondTerm <-A2 * 0.5 * amplitude * (1.0 - amplitudePow8) * cos(2.0 * phase + BetaL2)
    firstTerm + secondTerm
  }
  
  pow <- function(a, p) {
    a ** p
  }
  
  sleepDrive <- function(R1b,phase) {
    R1b - 3.25 * cos(phase) # Original function 
    
    # Modified function from supplemental material of Skeldon et al. (May not be necessary)
    R1b - 3.25 * 0.5 * (1 + 0.8*sin(phase) - 0.47*cos(phase))  
  }
  
  # First, define the default light vector in light. 
  # TODO: This is where sunrise/sunset will go, to shape the onset and offset of light! 
  times <- seq(from=0,to=24,by=0.1)
  
  baselineLight <-30  # This is the minimum lux level when someone is awake, even if the sun is down
  L1 <- 700
  L2 <- 0
  
  # Very late sunrise, waking up at 8 seems to maximize sleep during school week
  S1<-10
  S2<-21
  
  # More typical sunrise/sunset, waking up at 7 seems to maximize sleep during school week
  S1<-7
  S2<-18
  
  c <- 6000.0
  
  # Uses the function from the 
  lights <- L2 + (L1 - L2)/2 * (tanh(c * (times - S1)) -  tanh(c * (times - S2)))
  
  socialFactor <- 25 # Arbitrary imposition of "earliest time teens will go to sleep"
  
#  plot(times, lights)
  
  # Create interpolation function for 
  lightApprox <- function(t) {
    approx(times, lights, t)
  }
  
  sleep.circ.model <- function (t, x, params) {
    
    ## Extract state variables
    R <- x[1]
    Psi <- x[2]
    A <- x[3]
    R1tot <- x[4]
    N <- x[5]
    
    R1b <- 0.5 * (A + R1tot + 4 - sqrt(pow(A + R1tot + 4,2.0) - 4 * A * R1tot))
    
    # TODO: Make this check robust to weird start times, like 10pm 
    inSchoolHours <- (t %% 24 > schoolStartLocalTimeInHours) && (t %% 24 <= schoolStartLocalTimeInHours + schoolDurationInHours)  # Determine if it's currently school hours
    isASchoolDay <- ((t %/% 24) %% 7) < 5  # Determine if it's a school day
    
    #  Get light exposure from the typical day
    light <- lightApprox(t %% 24)$y
    
    #  If you're in school, you're awake
    if (inSchoolHours && isASchoolDay) {
      isAwake <<- TRUE
      light <- schoolBrightnessInLux
    } else {
      
      # If it's free time, and you want to fall asleep, you can, as long as you're not up for social reasons
      if (sleepDrive(R1b, Psi) > Dsleep) {  
        if (t %% 24 > socialFactor || t %% 24 < 10) { # Forbid falling asleep before a certain time due to social reasons
          isAwake <<- FALSE
        }
      }
      # If it's free time, and you want to wake up, you can wake up
      if (sleepDrive(R1b, Psi) < Dwake) {   
        isAwake <<- TRUE
      }
    }
    
    if (isAwake) {
      ichi <- 1.0/18.18
      mu <- 869.5
      if (light < baselineLight) { # Set light to baselineLight if they're awake and it's dark outside
        light <- baselineLight
      }
    } else {
      ichi <- 1.0/7.0  # Originally 1.0/4.2
      mu <- 596.5 
      light <- 0  # Set light to 0 if they're asleep
    }
    
    # Update the lux value (if we haven't yet)
    if (allLux[1 + floor(t / dt)] == -1) {
      allLux[1 + floor(t / dt)] <<- light
    }
    
    # Process L
    alpha_L <- alphaL(light)
    dN <- 60.0 * (alpha_L * (1 - N) - Delta * N)
    BhatValue <- Bhat(N, light)
    
    # Circadian Model
    lightAmp <- BhatValue * amplitudeResponseCurve(R, Psi)
    lightPhase <- BhatValue * phaseResponseCurve(R, Psi)
    amplitudePow4 <- pow(R, 4)
    
    couplingTermAmp <- K * 0.5 * cos(Beta1) * R * (1.0 - amplitudePow4)
    dR <- (-1.0 * Gamma * R) + couplingTermAmp + lightAmp
    
    naturalOscillation <- 2.0 * pi / Tau
    couplingTermPhase <- K / 2.0 * sin(Beta1) * (1 + amplitudePow4)
    dPsi <- naturalOscillation + couplingTermPhase + lightPhase
    
    # Sleep Model
    dA <- ichi * (mu - A)
    dR1tot <- Irecep * (R1b - Targc * R1tot)
    # combine results into a single vector
    dxdt <- c(dR, dPsi, dA, dR1tot, dN)
    
    ## return result as a list!
    list(dxdt)
  }
  
  
  parms <- c()
  ic <- c(R=0.8240484019863923, Psi=2.4532409247791804, A=767.657, R1tot=584.24, N=0.78424752754260862)
  
  numberOfDays <- 49
  dt<- 0.1
  fullIntegrationWindow <- seq(from=0,to=numberOfDays * 24,by=dt)
  
  isAwake <- TRUE
  schoolStartLocalTimeInHours <- 6  # This is the start time for the school
  schoolDurationInHours <- 7  # This is the duration of the school day
  schoolBrightnessInLux <- 300  # This is the brightness of the school
  
  
  allSchoolStartOptions <- c(5, 6, 7, 8, 9, 10)
  sleepDurationSchool <- c()
  
  
  for (schoolStartLocalTimeInHours in allSchoolStartOptions) {
    allLux <<- -1 + numeric(numberOfDays * 24 / dt + 1)  # Sanity check vector to make sure we're getting the right lux
    
    out <- rk4(ic, fullIntegrationWindow, sleep.circ.model, parms)
    
    
    homeostatLastWeek <- tail(out[,4], 24 * 7 / dt)
    homeostatDiff <- diff(homeostatLastWeek)
   # cat(sprintf("Wake time for school: %f\n", schoolStartLocalTimeInHours))
    avgSleep <- (24 * length(homeostatDiff[homeostatDiff < 0])/length(homeostatDiff))
   # cat(sprintf("Average sleep on this schedule over a week: %f\n", avgSleep))
    
    homeostatLastWeekDiff <- head(homeostatDiff, 24 * 5 / dt)
    avgSleepSchool <- (24 * length(homeostatLastWeekDiff[homeostatLastWeekDiff < 0])/length(homeostatLastWeekDiff))
 #   cat(sprintf("Average sleep during school schedule over a week: %f\n", avgSleepSchool))
    sleepDurationSchool <- append(sleepDurationSchool, avgSleepSchool)
    homeostatFirstWeek <- head(out[,4], 24 * 7 / dt)
    homeostatToPrint <- homeostatLastWeek
  #  print("Sleep to wake transitions in week:")
    #print(((which(diff(sign(diff(homeostatToPrint))) != 0)+1) * dt) %% 24)
    
    
    
    # plot(head(allLux, 24 * 3 / dt))
    # par(new=TRUE)
    # plot(head(out[,4], 24 * 3 / dt),col="green")
   # 
   # plot(tail(allLux, 24* 7 / dt))
 #   par(new=TRUE)
#    plot(homeostatLastWeek, col="green")
    
    
  }
  
  return(sleepDurationSchool)
  
}

sample2 <- function(sunR, sunS) {
  Dwake <- 555.4
  Dsleep <- 572.7
  
  # Light processing parameters
  Alpha_0 <- 0.05
  P <- 1.5
  I_0 <- 9325.0
  Delta <- 0.0075
  G <- 33.75
  
  # Circadian model parameters
  Irecep <- 1 / 291.0
  Targc <- 0.9677
  Tau <- 23.84
  Beta1 <- -0.09318
  A1    <- 0.3855
  A2    <- 0.1977
  BetaL1<- -0.0026 
  BetaL2<- -0.957756
  Sigma <- 0.0400692
  K     <- 0.06458
  Gamma <- 0.024
  
  # Circadian model functions
  Bhat <- function (lightActivation, rawControl) {
    G * (1.0 - lightActivation) * alphaL(rawControl)
  }
  
  alphaL <- function(control) {
    Alpha_0 * pow(control, P) / (pow(control, P) + I_0)
  }
  
  phaseResponseCurve <- function(amplitude, phase) {
    amplitudePow3 <- amplitude*amplitude*amplitude
    amplitudePow4 <- amplitudePow3*amplitude
    amplitudePow8 <- amplitudePow4*amplitudePow4
    firstTerm <-  A1 * 0.5 * (amplitudePow3 + 1.0 / amplitude) * sin(phase + BetaL1)
    secondTerm <-  A2 * 0.5 * (1.0 + amplitudePow8) * sin(2.0 * phase + BetaL2)
    Sigma - firstTerm - secondTerm
  }
  
  amplitudeResponseCurve <- function(amplitude, phase){
    amplitudePow4 <- pow(amplitude, 4)
    amplitudePow8 <- pow(amplitudePow4, 2)
    firstTerm <- A1 * 0.5 * (1.0 - amplitudePow4) * cos(phase + BetaL1)
    secondTerm <-A2 * 0.5 * amplitude * (1.0 - amplitudePow8) * cos(2.0 * phase + BetaL2)
    firstTerm + secondTerm
  }
  
  pow <- function(a, p) {
    a ** p
  }
  
  sleepDrive <- function(R1b,phase) {
    R1b - 3.25 * cos(phase) # Original function 
    
    # Modified function from supplemental material of Skeldon et al. (May not be necessary)
    R1b - 3.25 * 0.5 * (1 + 0.8*sin(phase) - 0.47*cos(phase))  
  }
  
  # First, define the default light vector in light. 
  # TODO: This is where sunrise/sunset will go, to shape the onset and offset of light! 
  times <- seq(from=0,to=24,by=0.1)
  
  baselineLight <-30  # This is the minimum lux level when someone is awake, even if the sun is down
  L1 <- 700
  L2 <- 0
  
  # Very late sunrise, waking up at 8 seems to maximize sleep during school week
  S1<-10
  S2<-21
  
  # More typical sunrise/sunset, waking up at 7 seems to maximize sleep during school week
  S1<-7
  S2<-18
  
  c <- 6000.0
  
  # Uses the function from the 
  lights <- L2 + (L1 - L2)/2 * (tanh(c * (times - S1)) -  tanh(c * (times - S2)))
  
  socialFactor <- 25 # Arbitrary imposition of "earliest time teens will go to sleep"
  
  plot(times, lights)
  
  # Create interpolation function for 
  lightApprox <- function(t) {
    approx(times, lights, t)
  }
  
  sleep.circ.model <- function (t, x, params) {
    
    ## Extract state variables
    R <- x[1]
    Psi <- x[2]
    A <- x[3]
    R1tot <- x[4]
    N <- x[5]
    
    R1b <- 0.5 * (A + R1tot + 4 - sqrt(pow(A + R1tot + 4,2.0) - 4 * A * R1tot))
    
    # TODO: Make this check robust to weird start times, like 10pm 
    inSchoolHours <- (t %% 24 > schoolStartLocalTimeInHours) && (t %% 24 <= schoolStartLocalTimeInHours + schoolDurationInHours)  # Determine if it's currently school hours
    isASchoolDay <- ((t %/% 24) %% 7) < 5  # Determine if it's a school day
    
    #  Get light exposure from the typical day
    light <- lightApprox(t %% 24)$y
    
    #  If you're in school, you're awake
    if (inSchoolHours && isASchoolDay) {
      isAwake <<- TRUE
      light <- schoolBrightnessInLux
    } else {
      
      # If it's free time, and you want to fall asleep, you can, as long as you're not up for social reasons
      if (sleepDrive(R1b, Psi) > Dsleep) {  
        if (t %% 24 > socialFactor || t %% 24 < 10) { # Forbid falling asleep before a certain time due to social reasons
          isAwake <<- FALSE
        }
      }
      # If it's free time, and you want to wake up, you can wake up
      if (sleepDrive(R1b, Psi) < Dwake) {   
        isAwake <<- TRUE
      }
    }
    
    if (isAwake) {
      ichi <- 1.0/18.18
      mu <- 869.5
      if (light < baselineLight) { # Set light to baselineLight if they're awake and it's dark outside
        light <- baselineLight
      }
    } else {
      ichi <- 1.0/7.0  # Originally 1.0/4.2
      mu <- 596.5 
      light <- 0  # Set light to 0 if they're asleep
    }
    
    # Update the lux value (if we haven't yet)
    if (allLux[1 + floor(t / dt)] == -1) {
      allLux[1 + floor(t / dt)] <<- light
    }
    
    # Process L
    alpha_L <- alphaL(light)
    dN <- 60.0 * (alpha_L * (1 - N) - Delta * N)
    BhatValue <- Bhat(N, light)
    
    # Circadian Model
    lightAmp <- BhatValue * amplitudeResponseCurve(R, Psi)
    lightPhase <- BhatValue * phaseResponseCurve(R, Psi)
    amplitudePow4 <- pow(R, 4)
    
    couplingTermAmp <- K * 0.5 * cos(Beta1) * R * (1.0 - amplitudePow4)
    dR <- (-1.0 * Gamma * R) + couplingTermAmp + lightAmp
    
    naturalOscillation <- 2.0 * pi / Tau
    couplingTermPhase <- K / 2.0 * sin(Beta1) * (1 + amplitudePow4)
    dPsi <- naturalOscillation + couplingTermPhase + lightPhase
    
    # Sleep Model
    dA <- ichi * (mu - A)
    dR1tot <- Irecep * (R1b - Targc * R1tot)
    # combine results into a single vector
    dxdt <- c(dR, dPsi, dA, dR1tot, dN)
    
    ## return result as a list!
    list(dxdt)
  }
  
  
  parms <- c()
  ic <- c(R=0.8240484019863923, Psi=2.4532409247791804, A=767.657, R1tot=584.24, N=0.78424752754260862)
  
  numberOfDays <- 49
  dt<- 0.1
  fullIntegrationWindow <- seq(from=0,to=numberOfDays * 24,by=dt)
  
  isAwake <- TRUE
  schoolStartLocalTimeInHours <- 6  # This is the start time for the school
  schoolDurationInHours <- 7  # This is the duration of the school day
  schoolBrightnessInLux <- 300  # This is the brightness of the school
  
  
  allSchoolStartOptions <- c(5, 6, 7, 8, 9, 10)
  sleepDurationSchool <- c()
  
  
  for (schoolStartLocalTimeInHours in allSchoolStartOptions) {
    allLux <<- -1 + numeric(numberOfDays * 24 / dt + 1)  # Sanity check vector to make sure we're getting the right lux
    
    out <- rk4(ic, fullIntegrationWindow, sleep.circ.model, parms)
    
    
    homeostatLastWeek <- tail(out[,4], 24 * 7 / dt)
    homeostatDiff <- diff(homeostatLastWeek)
    # cat(sprintf("Wake time for school: %f\n", schoolStartLocalTimeInHours))
    avgSleep <- (24 * length(homeostatDiff[homeostatDiff < 0])/length(homeostatDiff))
    # cat(sprintf("Average sleep on this schedule over a week: %f\n", avgSleep))
    
    homeostatLastWeekDiff <- head(homeostatDiff, 24 * 5 / dt)
    avgSleepSchool <- (24 * length(homeostatLastWeekDiff[homeostatLastWeekDiff < 0])/length(homeostatLastWeekDiff))
    #   cat(sprintf("Average sleep during school schedule over a week: %f\n", avgSleepSchool))
    sleepDurationSchool <- append(sleepDurationSchool, avgSleepSchool)
    homeostatFirstWeek <- head(out[,4], 24 * 7 / dt)
    homeostatToPrint <- homeostatLastWeek
    #  print("Sleep to wake transitions in week:")
    #print(((which(diff(sign(diff(homeostatToPrint))) != 0)+1) * dt) %% 24)
    
    
    
    plot(head(allLux, 24 * 3 / dt))
    par(new=TRUE)
    plot(head(out[,4], 24 * 3 / dt),col="green")
    plot(tail(allLux, 24* 7 / dt))
    par(new=TRUE)
    plot(homeostatLastWeek, col="green")
    
    
  }
  
}

sample3 <- function(sunR, sunS) {
  Dwake <- 555.4
  Dsleep <- 572.7
  
  # Light processing parameters
  Alpha_0 <- 0.05
  P <- 1.5
  I_0 <- 9325.0
  Delta <- 0.0075
  G <- 33.75
  
  # Circadian model parameters
  Irecep <- 1 / 291.0
  Targc <- 0.9677
  Tau <- 23.84
  Beta1 <- -0.09318
  A1    <- 0.3855
  A2    <- 0.1977
  BetaL1<- -0.0026 
  BetaL2<- -0.957756
  Sigma <- 0.0400692
  K     <- 0.06458
  Gamma <- 0.024
  
  # Circadian model functions
  Bhat <- function (lightActivation, rawControl) {
    G * (1.0 - lightActivation) * alphaL(rawControl)
  }
  
  alphaL <- function(control) {
    Alpha_0 * pow(control, P) / (pow(control, P) + I_0)
  }
  
  phaseResponseCurve <- function(amplitude, phase) {
    amplitudePow3 <- amplitude*amplitude*amplitude
    amplitudePow4 <- amplitudePow3*amplitude
    amplitudePow8 <- amplitudePow4*amplitudePow4
    firstTerm <-  A1 * 0.5 * (amplitudePow3 + 1.0 / amplitude) * sin(phase + BetaL1)
    secondTerm <-  A2 * 0.5 * (1.0 + amplitudePow8) * sin(2.0 * phase + BetaL2)
    Sigma - firstTerm - secondTerm
  }
  
  amplitudeResponseCurve <- function(amplitude, phase){
    amplitudePow4 <- pow(amplitude, 4)
    amplitudePow8 <- pow(amplitudePow4, 2)
    firstTerm <- A1 * 0.5 * (1.0 - amplitudePow4) * cos(phase + BetaL1)
    secondTerm <-A2 * 0.5 * amplitude * (1.0 - amplitudePow8) * cos(2.0 * phase + BetaL2)
    firstTerm + secondTerm
  }
  
  pow <- function(a, p) {
    a ** p
  }
  
  sleepDrive <- function(R1b,phase) {
    R1b - 3.25 * cos(phase) # Original function 
    
    # Modified function from supplemental material of Skeldon et al. (May not be necessary)
    R1b - 3.25 * 0.5 * (1 + 0.8*sin(phase) - 0.47*cos(phase))  
  }
  
  # First, define the default light vector in light. 
  # TODO: This is where sunrise/sunset will go, to shape the onset and offset of light! 
  times <- seq(from=0,to=24,by=0.1)
  
  baselineLight <-30  # This is the minimum lux level when someone is awake, even if the sun is down
  L1 <- 700
  L2 <- 0
  
  # Very late sunrise, waking up at 8 seems to maximize sleep during school week
  S1<-10
  S2<-21
  
  # More typical sunrise/sunset, waking up at 7 seems to maximize sleep during school week
  S1<-7
  S2<-18
  
  c <- 6000.0
  
  # Uses the function from the 
  lights <- L2 + (L1 - L2)/2 * (tanh(c * (times - S1)) -  tanh(c * (times - S2)))
  
  socialFactor <- 25 # Arbitrary imposition of "earliest time teens will go to sleep"
  
  plot(times, lights)
}
