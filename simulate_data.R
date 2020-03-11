###################################################################################
## Simulate extra data for preliminary results to show Peru to convince them to  ##
## send me data                                                                  ##
###################################################################################

## Goal here will be to assume that the observed count is the mean for that place, which will be wrong some
 ## portion of the time, but w/e

## Simulate as a negative binomial where the mean is the observed count and the variance is greater than the count
 ## Just keep size = 1 which produces a variance that is approximately the square root of the mean

if (sim_data == TRUE) {
## This feels a bit rediculous, but stack 20 copies of dengue cases on top of one another
 ## add a column for year
 ## and then perturb the number of cases in each place
  
## Try fewer years to get the model running
dengue_cases.clone <- rbind(
  dengue_cases, dengue_cases, dengue_cases, dengue_cases, dengue_cases
, dengue_cases, dengue_cases, dengue_cases, dengue_cases, dengue_cases
# , dengue_cases, dengue_cases, dengue_cases, dengue_cases, dengue_cases
# , dengue_cases, dengue_cases, dengue_cases, dengue_cases, dengue_cases
  )

dengue_cases.clone       <- transform(dengue_cases.clone
  , year = rep(seq(1, n_years), each = nrow(dengue_cases)))

## Try much less variation to get the model running
dengue_cases.clone$cases <- rnbinom(nrow(dengue_cases.clone), size = 20, mu = dengue_cases.clone$cases + 1)

dengue_cases             <- dengue_cases.clone

} else {
  
## Just add a column for year so that the code downstream doesn't have to change
dengue_cases <- transform(dengue_cases, year = 1)
  
}
