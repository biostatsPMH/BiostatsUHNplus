#-------------------------------------------------------------------------------------
# Data Definition
data(enrollment, package='BiostatsUHNplus')
data(demography, package='BiostatsUHNplus')
data(ineligibility, package='BiostatsUHNplus')
data(ae, package='BiostatsUHNplus')

clinT <- plyr::join_all(list(enrollment, demography, ineligibility, ae), 
                        by = "Subject", type = "full");
clinT$AE_SEV_GD <- as.numeric(clinT$AE_SEV_GD);
clinT$Drug_1_Attribution <- "Unrelated";
clinT$Drug_1_Attribution[clinT$CTC_AE_ATTR_SCALE %in% c("Definite", "Probable", "Possible")] <- "Related";
clinT$Drug_2_Attribution <- "Unrelated";
clinT$Drug_2_Attribution[clinT$CTC_AE_ATTR_SCALE_1 %in% c("Definite", "Probable", "Possible")] <- "Related";

#-------------------------------------------------------------------------------------

test_that("covsum nested calculates correctly with no maincov", {
  output = covsum_nested(data=clinT,
                  id = c("ae_detail", "Subject", "COHORT"),
                  covs=c("AE_SEV_GD", "ENROL_DATE_INT"),
                  markup=F)
  expect_equal(names(output), c("Covariate",'Full Sample (n=234)'))
  expect_equal(output$Covariate, c("AE SEV GD","Mean (sd)","Median (Min,Max)","ENROL DATE INT","Mean (sd)","Median (Min,Max)"))
  expect_equal(output$'Full Sample (n=234)', c("","1.8 (0.8)","1.5 (1.0, 5.0)","","2017-01-07 (301.8 days)","2016-09-14 (2016-01-18, 2018-05-16)"))
})

test_that("covsum nested calculates correctly with maincov", {
  output = covsum_nested(data=clinT,
                  id = c("ae_detail", "Subject", "COHORT"),
                  maincov="Drug_1_Attribution",
                  covs=c("AE_SEV_GD", "ENROL_DATE_INT"),
                  markup=F)
  expect_equal(names(output) ,c("Covariate","Full Sample (n=234)","Related (n=49)","Unrelated (n=198)","p-value","Effect Size","StatTest","Nested p-value") )
  expect_equal(output$Covariate, c("AE SEV GD","Mean (sd)","Median (Min,Max)","ENROL DATE INT","Mean (sd)","Median (Min,Max)"))
  expect_equal(output[,3],c("","2.0 (0.9)","2 (1, 4)","","2017-01-31 (322.7 days)","2017-02-07 (2016-01-18, 2018-05-16)"))
})

test_that("covsum nested includes missing correctly when presenting row percentages", {
  output = covsum_nested(data=clinT,
                  id = c("ae_detail", "Subject", "COHORT"),
                  maincov='Drug_1_Attribution',
                  covs=c("AE_SEV_GD", "ENROL_DATE_INT", "Drug_2_Attribution"),
                  include_missing=TRUE,pvalue=FALSE,effSize=FALSE,percentage='row',digits.cat=3)
  expect_equal(names(output) ,c("Covariate","Full Sample (n=234)","Related (n=49)","Unrelated (n=198)") )
  expect_equal(output[,2],c("","1.8 (0.8)","1.5 (1.0, 5.0)","","2017-01-07 (301.8 days)","2016-09-14 (2016-01-18, 2018-05-16)","","37","197"))
  expect_equal(output[,4],c("","1.7 (0.8)","1.5 (1.0, 5.0)","","2016-12-28 (294.3 days)","2016-09-14 (2016-01-18, 2018-05-16)","","11 (28.205)","187 (89.904)"))
})