
#' Sutherland et al. Model to Estimate Impact of Ending the BCG School Scheme
#'
#' @param Data Data used by Sutherland et al. analysis. See \code{\link[AssessBCGPolicyChange]{sutherland_data}}.
#' @param incidence_rates A list of Incidence rates (per 100,000) for the BCG vaccinated and unvaccinated population.
#' See \code{\link[AssessBCGPolicyChange]{sutherland_incidence_rates}} for details.
#' @param Rates.Per The number of people by which to produce rates, defaults to 100,000 as in Sutherland et al.
#' @param Cohort.Length Numeric, the lengh of time spent in each cohort, defaults to 5 years as in Sutherland et al.
#' @param Data.start Numeric, the year the first data point originates, defaults to 1969 as in Sutherland et al.
#' @param Annual.TB.Decrease.Yearly Matrix of average yearly decreases in TB with each column representing an age group (15-19, 20-24, 25-29).
#' Row names must be the year, starting from \code{Data.start}. Defaults to using  \code{\link[AssessBCGPolicyChange]{sutherland_TB_decrease}}.
#' @param Percentage.Year.One Numeric, the percentage of a generation that occur in the first year.
#' Fitted via, \code{\link[AssessBCGPolicyChange]{fit_model_to_data}}.
#' @param trans_params A funciton for estimating the transmission parameters (expected total secondary notifications,
#' the size of the first generation, and the average interval between a primary case and all secondary cases). Defailts to
#' \code{\link[AssessBCGPolicyChange]{sutherland_trans_params}}.
#' @param Sym.Lag Numeric, the generation time between infection and symtoms. Must be smaller or equal to the \code{Cohort.Length}.
#' Defaults to \code{\link[AssessBCGPolicyChange]{sutherland_gen_time}}.
#' @return A list of tables reproducing the results presented in Sutherland et al. The final table estimates the total number of additional
#' cases from ending the shcheme. In order to provide a complete estimate each 5 year estimate has been multiplied by the cohort length.
#' @export
#' @importFrom matrixStats colProds
#' @examples
#'
#' sutherland_model(Percentage.Year.One = 0.764)
#'
sutherland_model <- function(Data = sutherland_data, incidence_rates = sutherland_incidence_rates, Rates.Per = 100000,
                             Cohort.Length = 5, Data.start = 1969,
                             Annual.TB.Decrease.Yearly = TB_decrease_as_matrix(sutherland_TB_decrease),
                             Percentage.Year.One = NULL, trans_params = sutherland_trans_params(), Sym.Lag = 2){

  ## Define variables
  Cohort <- Data[["Cohort"]]
  Cohort.Ranges <- Data[["Cohort.Ranges"]]
  sim_year <- as.numeric(rownames(Annual.TB.Decrease.Yearly))
  Projected.Cohort <- (max(sim_year) - min(sim_year) + 1) / Cohort.Length
  trans_params <- sutherland_trans_params(Annual.TB.Decrease.Yearly, Sym.Lag = Sym.Lag)
  Expected.Total.Sec.Note <- trans_params$Expected.Total.Sec.Note
  Size.First.Gen <- trans_params$Size.First.Gen
  Avg.Int.All.Sec <- trans_params$Avg.Int.All.Sec

  Data <- c(Data, incidence_rates)

  if (Sym.Lag > Cohort.Length) {
    stop("Sym.Lag must be smaller than Cohort.Length")
  }
  ################################## Simulate Risk of developing TB per for Projected years split by age and vaccination
  Years.Data.Ava <- length(Cohort)

  Est.Vac.Risk <- Est.Unvac.Risk <- Est.Tub.Risk <- Est.Inelg.Risk <- matrix(NA,Projected.Cohort, 4)
  colnames(Est.Vac.Risk) <- colnames(Est.Unvac.Risk) <- colnames(Est.Inelg.Risk) <- colnames(Est.Tub.Risk) <- c('Aged 13 in', Cohort.Ranges)

  for (i in 1:Projected.Cohort){
    Year.Vect <- ((i - 2) * Cohort.Length + 2):((i - 1) * Cohort.Length+1)
    if (i <= Years.Data.Ava){
      if (sum(Data[['Vaccinated']][i,] == 0) == 0){

        Est.Vac.Risk[i,] <- c(Data.start + (i - 1)*Cohort.Length, Data[['Vaccinated']][i,])
        Est.Unvac.Risk[i,] <- c(Data.start + (i - 1)*Cohort.Length, Data[['Unvaccinated']][i,])
        Est.Tub.Risk[i,] <- c(Data.start + (i - 1)*Cohort.Length, Data[['Tuberculin']][i,])
        Est.Inelg.Risk[i,] <- c(Data.start + (i - 1)*Cohort.Length, Data[['Inelgible']][i,])
      }else if (sum(Data[['Vaccinated']][i,] == 0) == 1){

        Est.Vac.Risk[i,] <- c(Data.start+(i-1)*Cohort.Length, Data[['Vaccinated']][i,1:2], Data[['Vaccinated']][i-1,3]*prod((1-Annual.TB.Decrease.Yearly[Year.Vect,3])))
        Est.Unvac.Risk[i,] <- c(Data.start+(i-1)*Cohort.Length, Data[['Unvaccinated']][i,1:2], Data[['Unvaccinated']][i-1,3]*prod((1-Annual.TB.Decrease.Yearly[Year.Vect,3])))
        Est.Tub.Risk[i,] <- c(Data.start+(i-1)*Cohort.Length, Data[['Tuberculin']][i,1:2], Data[['Tuberculin']][i-1,3]*prod((1-Annual.TB.Decrease.Yearly[Year.Vect,3])))
        Est.Inelg.Risk[i,] <- c(Data.start+(i-1)*Cohort.Length, Data[['Inelgible']][i,1:2], Data[['Inelgible']][i-1,3]*prod((1-Annual.TB.Decrease.Yearly[Year.Vect,3])))


      }else{

        Est.Vac.Risk[i,] <- c(Data.start+(i-1)*Cohort.Length, Data[['Vaccinated']][i,1],Data[['Vaccinated']][i-1,2]*prod((1-Annual.TB.Decrease.Yearly[Year.Vect,2]))
                              ,Est.Vac.Risk[i-1,4]*prod((1-Annual.TB.Decrease.Yearly[Year.Vect,3])))
        Est.Unvac.Risk[i,] <- c(Data.start+(i-1)*Cohort.Length, Data[['Unvaccinated']][i,1],Data[['Unvaccinated']][i-1,2]*prod((1-Annual.TB.Decrease.Yearly[Year.Vect,2]))
                                , Est.Unvac.Risk[i-1,4]*prod((1-Annual.TB.Decrease.Yearly[Year.Vect,3])))
        Est.Tub.Risk[i,] <- c(Data.start+(i-1)*Cohort.Length, Data[['Tuberculin']][i,1],Data[['Tuberculin']][i-1,2]*prod((1-Annual.TB.Decrease.Yearly[Year.Vect,2]))
                              ,Est.Tub.Risk[i-1,4]*prod((1-Annual.TB.Decrease.Yearly[Year.Vect,3])))
        Est.Inelg.Risk[i,] <- c(Data.start+(i-1)*Cohort.Length, Data[['Inelgible']][i,1],Data[['Inelgible']][i-1,2]*prod((1-Annual.TB.Decrease.Yearly[Year.Vect,2]))
                                , Est.Inelg.Risk[i-1,4]*prod((1-Annual.TB.Decrease.Yearly[Year.Vect,3])))
      }

    }else{

      Est.Vac.Risk[i,] <- c(Data.start+(i-1)*Cohort.Length,  Est.Vac.Risk[i-1,2:4]*colProds((1-Annual.TB.Decrease.Yearly[Year.Vect,])))
      Est.Unvac.Risk[i,] <- c(Data.start+(i-1)*Cohort.Length,  Est.Unvac.Risk[i-1,2:4]*colProds((1-Annual.TB.Decrease.Yearly[Year.Vect,])))
      Est.Tub.Risk[i,] <- c(Data.start+(i-1)*Cohort.Length,  Est.Tub.Risk[i-1,2:4]*colProds((1-Annual.TB.Decrease.Yearly[Year.Vect,])))
      Est.Inelg.Risk[i,] <- c(Data.start+(i-1)*Cohort.Length,  Est.Inelg.Risk[i-1,2:4]*colProds((1-Annual.TB.Decrease.Yearly[Year.Vect,])))


    }


  }

    ########################################### Simulate Risk of developing TB per projected years (Sutherland Table 2)
    Est.TB.Risk<- Est.TB.Risk.Round <- matrix(NA,Projected.Cohort, 3)
    colnames(Est.TB.Risk) <-colnames(Est.TB.Risk.Round) <- c('Aged 13 in', 'Unvaccinated', 'Vaccinated')

    Est.TB.Risk[,1] <- Est.Vac.Risk[,1]
    Est.TB.Risk[,2] <-Rates.Per/(Cohort.Length*rowSums(Est.Unvac.Risk[,2:4]))
    Est.TB.Risk[,3] <-Rates.Per/(Cohort.Length*rowSums(Est.Vac.Risk[,2:4]))

    Est.TB.Risk.Round[,1] <- Est.TB.Risk[,1]
    Est.TB.Risk.Round[,2:3] <- c(signif(Est.TB.Risk[,2], digits = 2) ,signif(Est.TB.Risk[,3], digits = 2))


    ########################################### Simluation of Notifications prevented between 15-30

    #################### Breakdown by age
    Est.Note.Prev.Age <- Est.Unvac.Risk - Est.Vac.Risk
    Est.Note.Prev.Age[,1] <- Est.Unvac.Risk[,1]

    #################### Summary of total notifications prevented
    Est.Note.Prev <- matrix(NA, Projected.Cohort,3)
    Est.Note.Prev[,1] <- Est.Note.Prev.Age[,1]
    Est.Note.Prev[,2] <- rowSums(Cohort.Length * Est.Note.Prev.Age[,2:4])
    Est.Note.Prev[,3] <- Rates.Per/Est.Note.Prev[,2]

    colnames(Est.Note.Prev) <- c('Vac at age 13','Notifications prevented in 15 years', 'Vacs to prevent one notification in 15 years')

    #################### Round to accuracy presented in Sutherland (Table 3)
    Est.Note.Prev.Round <- Est.Note.Prev
    Est.Note.Prev.Round[,2:3] <- c(round(Est.Note.Prev[,2], digits = 0), signif(Est.Note.Prev[,3], digits = 2))

    ################### ###################Simulation of TB notifications prevented by the schools BCG scheme (Sutherland Table 4)

    ################### Project the total number of BCG vaccinations needed for the entire projection.
    Proj.No.BCG.Vac <- sapply(1:nrow(Est.Note.Prev.Age), function(i){
      if (i <=length(Data[['BCG.vaccinations']])){
        x <- unname(Data[['BCG.vaccinations']][i])
      }else{
        x <- unname(tail(Data[['BCG.vaccinations']],n=1))
      }
      return(x)})

    ################## Project the total number of negavtive unvac
    Proj.No.BCG.Unvac <- sapply(1:nrow(Est.Note.Prev.Age), function(i){  # projection of people that are negative unvaccinated

      if (i <=length(Data[['BCG.Unvaccinated']])){
        x <- unname(Data[['BCG.Unvaccinated']][i])
      }else{
        x <- unname(tail(Data[['BCG.Unvaccinated']],n=1))
      }
      return(x)})

    ################# Project the total number of Tuberculin Positive
    Proj.No.Tub.Pos  <-  sapply(1:nrow(Est.Note.Prev.Age), function(i){ # projection of people that are tuberculin postive
      if (i <=length(Data[['BCG.Tuberculin']])){
        x <- unname(Data[['BCG.Tuberculin']][i])
      }else{
        x <- unname(tail(Data[['BCG.Tuberculin']],n=1))
      }
      return(x)})

    ################ Project the total number of inelgible people
    Proj.No.Inelg <-sapply(1:nrow(Est.Note.Prev.Age), function(i){  # projection of people that are inelgible for the programme
      if (i <=length(Data[['BCG.Inelgible']])){
        x <- unname(Data[['BCG.Inelgible']][i])
      }else{
        x <- unname(tail(Data[['BCG.Inelgible']],n=1))
      }
      return(x)})
    ################### Project notifcations prevented in each age group per year


    Note.Prev.Year <- Est.Note.Prev.Age[,2:4]*Proj.No.BCG.Vac/Rates.Per
    rownames(Note.Prev.Year) <- as.character(Est.Note.Prev.Age[,1])
    Round.Note.Prev.Year <- sapply(1:ncol(Note.Prev.Year), function(x){round(Note.Prev.Year[,x],digits=0)})
    colnames(Round.Note.Prev.Year) <- colnames(Note.Prev.Year)

    ########################################## Total notifcations prevented in year period -Table 4

    Total.Note.Year <- sapply(1:(nrow(Note.Prev.Year)-2), function(i){
      M <- Note.Prev.Year[i,3]+Note.Prev.Year[i + 1,2]+Note.Prev.Year[i + 2,1]
    })
    names(Total.Note.Year) <- as.character(tail(Est.Note.Prev.Age[,1],n=(Projected.Cohort-2))+4)

    Round.Total.Note.Year <- round(Total.Note.Year, digits = 0)

    ######################################### Primary additional notifications if the scheme ends at end of cohort year:


    ################## Preallocations and set up

    Primary.Additional.Note <-Round.Primary.Additional.Note <- matrix(0,Projected.Cohort , Projected.Cohort)
    colnames(Primary.Additional.Note) <- colnames(Round.Primary.Additional.Note) <- as.character(tail(Est.Note.Prev.Age[,1],n=(Projected.Cohort))+4)
    rownames(Primary.Additional.Note) <- rownames(Round.Primary.Additional.Note) <- as.character(tail(Est.Note.Prev.Age[,1],n=(Projected.Cohort))+2)

    ################## Total notifcations from primary infections


    for (j in 1:nrow(Note.Prev.Year)){
      Primary.Additional.Note[j,]  <- c(rep(0,j - 1) ,sapply(j:(nrow(Note.Prev.Year)), function(i){
        if (i == j){
          M <- 0
        } else if (i == (j + 1)){
          M <- Note.Prev.Year[j + 1,1]
        }else if (i==(j+2)){
          M <- Note.Prev.Year[j + 2,1]+Note.Prev.Year[j + 1,2]
        }else{
          M <- Note.Prev.Year[i,1] + Note.Prev.Year[i - 1,2] + Note.Prev.Year[i - 2,3]

        }
      }))
    }


    Round.Primary.Additional.Note <- round(Primary.Additional.Note, digits=0)


    ######################################## Secondary additional notifications resulting from stopping scheme
    index <- colnames(Round.Primary.Additional.Note)
    x <- t(t(Round.Primary.Additional.Note) * Size.First.Gen[index] * Expected.Total.Sec.Note[index] * Percentage.Year.One )
    y <- t(t(Primary.Additional.Note)*Expected.Total.Sec.Note[index]*(1-Size.First.Gen[index]*Percentage.Year.One)*
             (1-rowMeans(Annual.TB.Decrease.Yearly[index, ]))^(Cohort.Length-Avg.Int.All.Sec[index]))
    z <- x[, 2:(ncol(x))] + y[, 1:(ncol(y)-1)]
    p <- round( z[c('1986', '1991', '1996'), c('1993','1998','2003','2008', '2013')], digits=0)



    #########################################
    Secondary.Additional.Note <- z
    Round.Secondary.Additional.Note <- sapply(1:ncol(Secondary.Additional.Note), function(i){ round(Secondary.Additional.Note[,i], digits=0)})
    colnames(Round.Secondary.Additional.Note) <- colnames(Secondary.Additional.Note)

    Total.Secondary.Note.All.Time <- rowSums(Round.Secondary.Additional.Note)

    Sutherland.Secondary.Note.All.Time <- c(204, 110, 49)
    names(Sutherland.Secondary.Note.All.Time) <- c('1986', '1991', '1996') # values taked directly from sutherland anaylsis





  ########################################################## Total notifications if scheme continues


  Length <-1

  Total.Note.Subpop = function(Est.Risk, Proj.Pop, Length){




    Note.Subpop <- Est.Risk[,2:4]*Proj.Pop/Rates.Per



    Total.Note.Subpop <- sapply(1:(nrow(Note.Subpop)-2*Length), function(i){

      M <- Note.Subpop[i,3]+Note.Subpop[i+Length,2]+Note.Subpop[i+2*Length,1]

    })
    names(Total.Note.Subpop) <- as.character(tail(Est.Note.Prev.Age[,1],n=(Projected.Cohort-2)*Length)+4)

    Round.Total.Note.Subpop <- round(Total.Note.Subpop, digits=0)
    return(Round.Total.Note.Subpop)
  }




  ################### Notifications from BCG vaccinated group

  Round.Total.Note.Year.Vac.Rec <- Total.Note.Subpop(Est.Vac.Risk,Proj.No.BCG.Vac, Length)

  ################## Notifcations from Unvaccinated Group

  Round.Total.Note.Year.Vac.Unrec <- Total.Note.Subpop(Est.Unvac.Risk,Proj.No.BCG.Unvac, Length)

  ################## Total notifcations from Tuberculin positive group

  Round.Total.Note.Year.Tub.Pos <- Total.Note.Subpop(Est.Tub.Risk,Proj.No.Tub.Pos, Length)

  ################## Total notifcations from otherwise ineligible for the scheme

  Round.Total.Note.Year.Inelg <- Total.Note.Subpop(Est.Inelg.Risk,Proj.No.Inelg, Length)



  ################### Total Notifications if scheme continues

  Total.Note.Year.Full <- Round.Total.Note.Year.Vac.Rec + Round.Total.Note.Year.Vac.Unrec + Round.Total.Note.Year.Tub.Pos + Round.Total.Note.Year.Inelg
  names(Total.Note.Year.Full) <- names(Round.Total.Note.Year.Vac.Rec)

  dummy.Total.Note.Year.Full <- t(sapply(1:nrow(Round.Secondary.Additional.Note), function(i){ M <- Total.Note.Year.Full}))

  Stan.Year <- names(Total.Note.Year.Full)[1:(length(names(Total.Note.Year.Full)) + (1 - Length))]
  Total.Effects <- Round.Primary.Additional.Note[,Stan.Year] + Round.Secondary.Additional.Note[,Stan.Year] + dummy.Total.Note.Year.Full[,Stan.Year]



  #################### Total additional notifications from ending the scheme in selected years

  total_additional_notifications <- Round.Primary.Additional.Note[,Stan.Year] + Round.Secondary.Additional.Note[,Stan.Year]

  total_add_nots_all_time <- rowSums(total_additional_notifications) * Cohort.Length
  ######################################## Store output into list format

  Output <- list(Est.TB.Risk.Round, Est.Note.Prev.Round, Round.Note.Prev.Year, Round.Total.Note.Year,
                Total.Note.Year.Full, Round.Primary.Additional.Note, Round.Secondary.Additional.Note,p, Total.Secondary.Note.All.Time,
                Total.Effects, total_additional_notifications, total_add_nots_all_time)
  names(Output) <- c('Table 2 - Estimated risk of developing notified TB', 'Table 3 - Estimated of no. of TB notifications prevented', 'Table 4 - Estimated no. of TB notifications prevented by Schools BCG scheme'
                     , 'Table 4 - Total notifcations prevented each year', 'Table 5 - Total notifications if the scheme continues', 'Table 5 - Primary additional notifications',
                     'Table 5 - Secondary additional notifications','Table 5 - Secondary additional notifcations reduced to Sutherland for clarity',
                     'Table 5 - Total secondary notifications all time', 'Table 5 - Total Effects of ending the schools BCG scheme',
                     "Total additional notifications", "Total additional notifications all time")

  return(Output)
}


