##' Parameters that are common for several functions can be described
##' her and be inherited by @inheritParams.
##'
##' @title Dummy function for parameter description
##' @param AllFarmsData A list with information on all Norwegian farm.
##'        May be for historical period, future period or combined. It contains the following elements
##' \itemize{
##'  \item MedicineInfo - A data frame with information on different type of treatments,
##'        with one row per treatment. The column names are
##'        medicine, product.name, feed, delay, duration.const, temp.dependent,
##'        CH.effect, PA.effect, A.effect and egg.effect.
##'  \item locno - A vector of length n.farms with the farm id's written as character strings.
##'  \item coord - An (n.farms x 2) matrix with cooordinates for the n.farms farms. One row per farm
##'        with farm id numbers as row names. Column lat with latitude degrees North,
##'        written as decimal number, and column lon with longitudinal degrees East.
##'  \item S - An (n.weeks x n.farms) matrix with with 1 if a farm is active (i.e. have fish that week),
##'        with one row per week an one column per farm.
##'        The rownames are of the form "yyyyww" and the column names are the farm id's.
##'        Prediction ahead in time are simply 1 (active) for farms that were active in the last
##'        week in the historical data and 0 (non-active) for farms that were not active the last week.
##'  \item no - An (n.weeks x n.farms) matrix with number of fish at the farm
##'        measured in millions of fish, with one row per week an one column per farm.
##'        The rownames are of the form "yyyyww" and the column names are the farm id's.
##         The number of fish should ideally be the number of fish at a farm
##'        the week in question, but this is probably (per February 2018 unavailable),
##'        and can be replaced by the maximum number of fish allowed. When predicted
##'        ahead in time, the last numbeer is carried forward.
##'  \item AfPerFish - An (n.weeks x n.farms) matrix of weekly abundance of adult female lice 
##'        at the farms, with one row per week an one column per farm.
##'        The rownames are of the form "yyyyww" and the column names are the farm id's.
##         For historical data, the lice abundance are typically downloaded from Barentswatch,
##         and missing values are interpolated bettween farms in within farms over time.
##'        Predictions are based on a prediction model that takes into account seasonality,
##'        and interaction (infection between farms) between farms are ignored.
##'  \item N.Af.ext An (n.weeks x n.farms) matrix of weekly weighted number of adult female lice 
##'        at the the neighbouring farms for each single farm, where the weights depend on
##'        the seaway distances to the neighbouring farms.
##'        This is preliminary infection pressure in the population model for lice
##'        in the LiceModel package.
##'  \item Af.ext - An (n.weeks x n.farms) matrix of weekly weighted weekly abundance of adult female lice 
##'        at the the neighbouring farms for each single farm, where the weights depend on
##'          the seaway distances to the neighbouring farms.
##'        This is used in the population model for lice in the LiceModel package.
##'  \item temp - An (n.weeks x n.farms) matrix of weekly seawater temperatures at the farms
##'        measured in degrees Celcius, with one row per week an one column per farm.
##'        The rownames are of the form "yyyyww" and the column names are the farm id's.
##'        The number of weeks may differ from that in other weekly matrices.
##'        For historical data, the temperatures are typically downloaded from Barentswatch,
##'        and missing values are interpolated bettween farms in within farms over time.
##'        Predictions are based on a prediction model that takes into account seasonality.
##'  \item salinity - An (n.days x n.farms) matrix of daily salinity (in psu) at the farms,
##'        with one row per day an one column per farm.
##'        The rownames are of the form "yyyymmdd" and the column names are the farm id's.
##'        For historical data, the salinyty are typically downloaded from met.no and are
##'        based on their hydrological model,
##'        and potensial missing values (these are few) are interpolated between farms in within farms over time.
##'        Predictions are based on a prediction model that takes into account seasonality.
##'  \item Dist - An (n.farms x n.farms) matrix of pairwise seaway distances (in km) between all farms.
##'        The farm id's are the row and column names. Distances more than 100 km are set to 10000.
##'  \item Dist.ind - An (n.farms x n.farms) matrix corresponding to Dist,
##'        with elements=1 for distances <= 100 km, and 0 for distances > 100 km.
##' }
##' @param FullData A list background data for a single farm. It can be on cage level with n.merd cages
##'        or be aggregated to farm level with n.merds=1. The list elements are
##' \itemize{
##'  \item divinfo - A list with various information. The list elements are
##'  \itemize{
##'   \item loc.no - Unique farm identification number
##'   \item loc.name - Farm name. NEED ALSO GENERATION?
##'   \item stage.list - Vector of length n.stages with character codes for various lice stages
##'   \item n.stages - Number of stages = length of stage.list
##'   \item merd.names - Character vector of length n.merds with names of cages
##'   \item n.merds - Number of cages
##'   \item time.names - Character vector of length n.times with dates of the form "yyyymmdd"
##'   \item n.times - Number of days
##'   \item first.date.with.fish - Character string indicating first day with fish,
##'         with dates of the form "yyyymmdd". May contain dates outside time.names.
##'   \item last.date.with.fish - Character string indicating last day with fish in the data,
##'         usually the last data point, with dates of the form "yyyymmdd". May contain dates outside time.names.
##'   \item med.names.not.in.MedicineInfo - Character vector with possible treatment names
##'         that are unknown, i.e. not in the MedicineInfo matrix with known treatments. Usually NULL.
##'   \item med.names - Character vector of length n.med with names of the n.med different
##'         treatments used for this farm in these data
##'   \item n.med - Number of different treatments used for this farm
##'   \item MedicineInfo - A matrix with one row per "known" treatment,
##'         incuding non-medical treatments, with various information in the columns
##'   \item movement.from - A list with information on movements from cages.
##'         Redundant with w described elsewhere. May contain dates outside time.names.
##'   \item movement.to - A list with information on movements to cages.
##'         Redundant with w described elsewhere. May contain dates outside time.names.
##'  } 
##'  \item calendar - (n.times x 5) matrix with  calendar information,
##'        where n.times is the number of days, with year, month, day in month, day in week
##'        and week in year. The rownames are of the form "yyyymmdd".
##'  \item lice - A list of length n.merds, one element for each cage. Each element is a (nobs x 4) matrix
##'        with lice counts, with one row per observation and with columns nfish=number of fish investigated,
##'        OM=number of other mobiles (pre-adults + adult males, bevegelige), CH=number of chalimi (fastsittende),
##'        Af=number of adult females. Number of lice per fish is given by dividing by nfish.
##'        The rownames are of the form "yyyymmdd".
##'  \item vekt - (n.times x n.merds) matrix with average fish weight per cage given in kg.
##'        One row per day with rownames "yyyymmdd" and one column per cage. 
##'  \item antall - (n.times x n.merds) matrix with number of fish in each cage.
##'        One row per day with rownames "yyyymmdd" and one column per cage. 
##'  \item biom - (n.times x n.merds) matrix with biomass per cage given in kg.
##'        One row per day with rownames "yyyymmdd" and one column per cage.
##'  \item behand - (n.times x n.merds x n.med) array with with 1 indicating treatment
##'        and 0 indicating no treatments. The third dimensions are the n.med treatments
##'        used for this farm and the names of the treatments are the names of this dimension.  
##'  \item cleaner.fish.stocked - A list with two elements Rognkjeks and Leppefisk. Ech of these elements contain a
##'  \itemize{
##'   \item Rognkjeks - a (n.times x n.merds) matrix with number of cleaner fish of type Rognkjeks stocked.
##'   \item Leppefisk - a (n.times x n.merds) matrix with number of cleaner fish of type Leppefisk (wrasse) stocked.
##'  }
##'  \item w - (n.times x n.merds x n.merds+1) array with proportion of fish moved
##'        between cages or removed from the farm at the end of the day. For each day
##'        there is a (n.merds x n.merds+1) matrix where element (i,j) contains the
##'        proportions of fish in cage i that is moved to cage j, element (i,i) contains
##'        the proportion of fish that remains in cage i and element (i,n.merds+1) contains
##'        the proportion of fish in cage i that are removed from the farm.
##'  \item S - (n.times x n.merds) matrix with 1 if there is fish in the cage (antall>0) and 0 if not.
##'  \item temp - Vector of length n.times with seawater temperatures measured in degrees Celsius.
##'        May have been imputed from other farms, or may have been predicted ahead in time.
##'  \item salinity - Vector of length n.times with seawater temperatures measured in degrees Celsius.
##'        Per February 2018 based on a hydrodynamical model from met.no.
##'        May have been imputed from other farms, or may have been predicted ahead in time.
##'  \item smittepress - Vector of length n.times with daily preliminary infection pressure.
##'        This is either a weighted sum of the number of adult females at neighbouring farms, or a prediction ahead in time. 
##'  \item lusetetthetNabo - Vector of length n.times with a daily weighted number of of adult females per fish at neighbouring farms.
##'  \item covariates - A list with various covariates that includes lice age,
##'        where age varies from 0 to max.age, where max.age=100 per January 2018. The list elements are
##'  \itemize{
##'   \item Tmean - (n.times x (max.age+1)) matrix with average temperatures the lice have experienced
##'   \item treatment - Either NULL if no treatments, or a list with n.merds elements, one per cage.##'                     Each cage element contains a list with the different treatments 
##'                     used at the farm. Each treatment element contains
##'   \itemize{
##'    \item - "NR" if there is no treatments of this type for this cage,
##'            and a list with single treatments if there is at least one. The list element for a single treatment contains
##'    \itemize{
##'     \item - a list with with three elements, for the stages CH, PA og A. Each stage-element contains
##'            "NR" if the treatment type has no effect for this stage and a matrix if it has effect. This matrix is
##'                a (n.times x (max.age+1)) matrix with 0/1 values or decimal values between 0 and 1., Here >=0 means effect.
##'    }
##'   }
##'   \item reprod - a (n.times x (max.age+1)) matrix with reproduction factors depending on time (temperature) and lice-age.
##'  }
##' }
##' @param FullData.historical A FullData object for historical data
##' @param FullData.future A FullData object for future data.
##'        The first day is the last day of the historical data.
##' @param AllData A list with two elements of same type, one named
##'        AllData.merd.level at cage level and another named
##'        AllData.merd.level at farm level. These elements are
##' \itemize{ 
##'  \item a list with data for a single farm on cage level, with elements 
##'  \itemize{
##'   \item FullData - A FullData object
##'   \item pred.summary - A list with n.merds elements, one for each cage
##'         (or aggregated to farm level) with summary statistics for estimated or predicted lice levels. Each element is
##'   \itemize{
##'    \item a list with three elements, one for each of the counting groups
##'          CH (sessile, fastsittende), OM (other mobiles, bevegelige utenom voksne hunnlus)
##'          and Af (adult females, voksne hunnlus). Each element is
##'          an (n.times x n.stat) matrix with with summary statistics per day, with columns:
##'    \itemize{
##'     \item Expectation or best guess is in the column with name \code{mean}.
##'     \item Lower and upper limits in confidence or prediction intervals with width
##'           0.xx are named low0.xx og upp0.xx, e.g. \emph{low.0.95} and upp.0.95 for a 95\% interval.
##'           Per January 2018 these are calculated for for 95\%, 90\% og 80\% intervals.
##'     \item Probability for the underlying lice level being above a threshold x.x or x at the given day are
##'           named Pgtx.x or Pgtx, e.g. Pgt0.2 for P(lice per fish > 0.2).
##'     \item Cumulative probability to be over a threshold, i.e. the probability tro the lice
##'           level being above a threshold x-x or x at least once in the period from day 2 to day t are
##'           named cumPgtx.x. Day 1 is not included, since it is the last day in the estimation period if
##'           the current object is for predictions. OBS: En bitte liten feil må rettes opp i cumPgtx.xx.
##'    }
##'   }
##'   \item count.summary - a list of same format as pred.summary, but including count
##'         uncertainty for the average of lice counts on x fish,
##'         where x per January 2018 is 20 fish per cage. The real lice counts can be
##'         compared to the confidence limits, but the real unsertainty will differ
##'         if the real counts are based on less or more than 20 fish per cage.
##'         When aggegated over all farms, it is assumed that all cages are counted on
##'         the same day, which may not be the case for the real count data.
##'   \item cleaner.fish.summary - a list of similar format as pred.summary,
##'         but for cleaner fish ratio, i.e. number of cleaner fish divided by the number of fish.
##'         The count groups in pred.summeray is replaced by cleaner fish groups,
##'         which are Rognkjeks, Leppefisk and RensefiskTot (total cleaner fish ratio).
##'  }
##' }
##' @param AllData.historical An AllData object for historical data
##' @param AllData.future An AllData object for future data
##' @param est.res The output object from the estimateLice function.
##' @param pred.res The output object from the predictLice function.
##' @param sim.per.sim Integer, number of simulation per MCMC sample for computing confidence limits for lice counts
##' @param intervals Real vector, width of confidence intervals
##' @param limits Real vector, threshold for computing P(x>threshold)
##' @param no.fish.counted Integer, the assumed number of fish counted per cage when constructing confidence intervals for counts
##' @param adjust.factor An adjusting factor for to increase the uncertainty for quantities averages over cages, to accounts for correlation between cages
##' @param StartEndDate A data frame with three columns:with start time and end time for a fish generation
##' \itemize{
##'  \item Generation - Integer, year of stocking
##'  \item FromDate - Date object of format "yyyy-mm-dd", generated by as.Date(startDate,origin="1970-01-01")
##'  \item ToDate - Date object of format "yyyy-mm-dd", generated by as.Date(toDate,origin="1970-01-01")
##' }
##' @param forecast.horizon.days Integer, forecast horizon in days
##' @param forecast.horizon.weeks Integer, forecast horizon in weeks
##' @param nsim Integer, number of simulation used in prediction
##' @param no.days.back Integer, number of days in data period before first fish are stocked
##' @param max.stage.life Integer, maximum age of lice in days
##' @param ProductionDataFromCsv A list with data frames named
##'        Inventory, Treatment, Count, Movement, Cleanerfish and Environment.
##'        These elements are
##' \itemize{ 
##'  \item Inventory - Data frame with one row per day and with at least the following columns:
##'  \itemize{
##'   \item SiteName - Character string with farm name
##'   \item SiteNr - Integer with farm id
##'   \item Cage - Integer or character string with cage name (must contain a unique number) or number
##'   \item Generation - Integer, year of stocking
##'   \item SpeciesName - Character string with species, currently not used
##'   \item SpeciesNS9400ID - Integer, spesies code
##'   \item Date - Character string with format "yyyy-mm-dd"
##'   \item CloseCount - Integer, number of fish at the end of the day
##'   \item CloseBiomass - Integer or real, biomass in kg
##'   }
##'  \item Treatment - Data frame with one row per day with treatment and with at least the following columns:
##'  \itemize{
##'   \item SiteName - Character string with farm name
##'   \item SiteNr - Integer with farm id
##'   \item Cage - Integer or character string with cage name (must contain a unique number) or number
##'   \item Date - Character string with format "yyyy-mm-dd"
##'   \item TreatmentProduct - Character string with treatment type.
##'         Legal types are those listed in the MedicineInfo object, including "Ideal".
##'         The types are read from the first part of the character string,
##'         and some treatment types may be written in more than one legal way.
##'         If at least one treatment is of type "Ideal", three extra columns with names
##'         CHmortality, PAmortality and Amortality are needed.
##'   \item CHmortality - Integer between 0 an 100,
##'         treatment mortality in % for treatment of type "Ideal" for stage CH (chalimus)
##'   \item PAmortality - Integer between 0 an 100,
##'         treatment mortality in % for treatment of type "Ideal" for stage PA (pre-adult)
##'   \item Amortality - Integer between 0 an 100,
##'         treatment mortality in % for treatment of type "Ideal" for stage A (adult).
##'  }
##'  \item Count - Data frame with lice counts with one row per day/cage/fish
##'                and with at least the following columns:
##'  \itemize{
##'   \item SiteName - Character string with farm name
##'   \item SiteNr - Integer with farm id
##'   \item Cage - Integer or character string with cage name (must contain a unique number) or number
##'   \item Date - Character string with format "yyyy-mm-dd"
##'   \item FishNr - Integer with fish number, unique per cage/date.
##'         Format may later be changed to allow for lice counts aggregated over the numbe of fish. 
##'   \item Lice..Mobile. - Integer, number of other (than adult females) mobile lice. Column name may be changed.
##'   \item Lice..Female.Ovigorous. - Integer, number of adult female lice. Column name may be changed.
##'   \item Lice..Chalimus. - Integer, number of chalimi. Column name may be changed.
##'  }
##'  \item Movement - Data frame with movements of fish with with at least the following columns:
##'  \itemize{
##'   \item TransactionDate - Character string with format "yyyy-mm-dd"
##'   \item FromSite - Character string with farm name
##'   \item FromSiteNr - Not used
##'   \item FromCages - Name or number of cage where fish are moved from 
##'   \item FromProductNr - Not used
##'   \item FromProject - Not used
##'   \item ToSite - Not used
##'   \item ToSiteNr - Not used
##'   \item ToCage - Name or number of cage where fish are moved to
##'   \item ProductID - Not used
##'   \item ToProject - Not used
##'   \item MovementType -  Character string with movement type, only rows with "Internal Movement" are used
##'   \item FishCount - Integer, number of fish moved
##'   \item FishBiomassKg - Real, biomass of fish moved, in kg
##'  }
##'  \item Cleanerfish - Data frame with cleaner fish stocked with at least the following columns:
##'  \itemize{
##'   \item SiteDescriptionName - Character string with farm name
##'   \item SiteNr - Not used
##'   \item Cage - Name or number of cage where cleaner fish are stocked 
##'   \item SpeciesName - Character string, species type. Legal names 
##'   \item SpeciesNS9400ID - Integer, species code
##'   \item Date - Character string with format "yyyy-mm-dd"
##'   \item PondingCount - Integer, number of cleaner fish stocked
##'  }
##'  \item Environment - Data frame with information on ienvironment variables,
##'        currently seawater temperature and salinity.
##'        It has at least the following columns:
##'  \itemize{
##'   \item SiteName - Character string with farm name
##'   \item Date - Character string with format "yyyy-mm-dd"
##'   \item X.Sea.Temperature..Medium.. - Real, seawater temerature in degrees Celcius
##'   \item X.Salinity..Medium.. - Real, salinity in psu
##'  }
##' }
##' @param Treatment.future - Data frame with one row per day with future
##'        treatments and with at least the following columns:
##' \itemize{
##'  \item SiteName - Character string with farm name
##'  \item SiteNr - Integer with farm id
##'  \item Cage - Integer or character string with cage name (must contain a unique number) or number
##'  \item Date - Character string with format "yyyy-mm-dd"
##'  \item TreatmentProduct - Character string with treatment type.
##'        Legal types are those listed in the MedicineInfo object, including "Ideal".
##'        The types are read from the first part of the character string,
##'        and some treatment types may be written in more than one legal way.
##'        If at least one treatment is of type "Ideal", three extra columns with names
##'        CHmortality, PAmortality and Amortality are needed.
##'  \item CHmortality - Integer between 0 an 100,
##'        treatment mortality in % for treatment of type "Ideal" for stage CH (chalimus)
##'  \item PAmortality - Integer between 0 an 100,
##'        treatment mortality in % for treatment of type "Ideal" for stage PA (pre-adult)
##'  \item Amortality - Integer between 0 an 100,
##'        treatment mortality in % for treatment of type "Ideal" for stage A (adult).
##' }
##' @return NULL
##' @author Magne Aldrin
##' @export
dummy.for.documentation<-function(AllFarmsData,
                                  FullData,
                                  FullData.historical,
                                  FullData.future,
                                  AllData,
                                  AllData.historical,
                                  AllData.future,
                                  est.res,
                                  pred.res,
                                  sim.per.sim,
                                  intervals,
                                  limits,
                                  no.fish.counted,
                                  adjust.factor,
                                  StartEndDate,
                                  forecast.horizon.days,
                                  forecast.horizon.weeks,
                                  nsim,
                                  no.days.back,
                                  max.stage.life,
                                  ProductionDataFromCsv,
                                  Treatment.future
                                  ) {
	
  return(NULL)
}
