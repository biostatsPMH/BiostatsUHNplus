% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caterpillar_plot.R
\name{caterpillar_plot}
\alias{caterpillar_plot}
\title{Caterpillar plot. Useful for plotting random effects from hierarchical models,
such as MCMCglmm::MCMCglmm() object, that have binary outcome.}
\usage{
caterpillar_plot(
  subjID,
  subjLabel = NULL,
  remove.text.subjID = FALSE,
  mcmcglmm_object,
  orig_dataset,
  binaryOutcomeVar,
  prob = NULL,
  title = NULL,
  no.title = FALSE,
  subtitle = NULL,
  ncol = NULL,
  fonts = NULL,
  columnTextWidth = NULL,
  break.label.summary = FALSE
)
}
\arguments{
\item{subjID}{key identifier field for participant ID in data sets}

\item{subjLabel}{text label field in dataset to replace key identifier field for
participant ID with in plot (if provided)}

\item{remove.text.subjID}{boolean indicating if non-numeric text should be
removed from subjID in plot label. Note that this can only be used if there
are non-duplicate participant IDs when non-numeric text is removed. Default
is FALSE (if provided)}

\item{mcmcglmm_object}{MCMCglmm model output}

\item{orig_dataset}{data frame supplied to MCMCglmm function}

\item{binaryOutcomeVar}{name of binary variable (0,1) that denotes outcome
in MCMCglmm model}

\item{prob}{probability for highest posterior density interval, similar to a
confidence interval. Default is 0.95 (if provided)}

\item{title}{title of the plot. Overrides default title (if provided)}

\item{no.title}{boolean that denotes if title should be outputted in plot. Default
is TRUE (if provided)}

\item{subtitle}{subtitle of the plot. Overrides default subtitle (if provided)}

\item{ncol}{number of columns in plot. Default is 2 (if provided)}

\item{fonts}{character text that denotes font for title, subtitle, category labels,
x-axis plot labels (if provided)}

\item{columnTextWidth}{numeric that denotes character width for label text before
breaking to start new line. Default is 20 characters (if provided)}

\item{break.label.summary}{boolean to indicate if new line should start in label
before (n, event) summary. Default is FALSE}
}
\value{
ggplot object of caterpillar plot
}
\description{
Caterpillar plot. Useful for plotting random effects from hierarchical models,
such as MCMCglmm::MCMCglmm() object, that have binary outcome.
}
\examples{
data("ae");

ae$G3Plus <- 0;
ae$G3Plus[ae$AE_SEV_GD \%in\% c("3", "4", "5")] <- 1;
ae$Drug_1_Attribution <- 0;
ae$Drug_1_Attribution[ae$CTC_AE_ATTR_SCALE \%in\% c("Definite", "Probable", "Possible")] <- 1;
ae$Drug_2_Attribution <- 0;
ae$Drug_2_Attribution[ae$CTC_AE_ATTR_SCALE_1 \%in\% c("Definite", "Probable", "Possible")] <- 1;

prior2RE <- list(R = list(V = diag(1), fix = 1),
  G=list(G1=list(V=1, nu=0.02), G2=list(V=1, nu=0.02)));
  
model1 <- MCMCglmm::MCMCglmm(G3Plus ~ Drug_1_Attribution + Drug_2_Attribution, 
  random=~Subject + ae_category, family="categorical", data=ae, saveX=TRUE, 
  verbose=FALSE, burnin=2000, nitt=10000, thin=10, pr=TRUE, prior=prior2RE);
  
p <- caterpillar_plot(subjID = "Subject",
  mcmcglmm_object = model1,
  prob = 0.99,
  orig_dataset = ae,
  binaryOutcomeVar = "G3Plus")
  
p <- caterpillar_plot(subjID = "ae_category",
  mcmcglmm_object = model1,
  prob = 0.95,
  orig_dataset = ae,
  remove.text.subjID = FALSE,
  ncol = 4,
  binaryOutcomeVar = "G3Plus",
  subtitle = "System organ class (n, event)",
  title = "Odds Ratio for G3+ Severity with 95\% Highest Posterior Density Interval",
  fonts = c("Arial", "Arial", "Arial", "Arial"),
  break.label.summary = TRUE)
}
\keyword{plot}
