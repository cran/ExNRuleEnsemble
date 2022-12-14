\name{ILPD}
\alias{ILPD}
\docType{data}
\title{Indian Liver Patient Dataset}
\description{
This data set contains 416 liver patient records and 167 non liver patient records.The data set was collected from north east of Andhra Pradesh, India. The class label divides the patients into 2 groups i.e. liver patient or not (1, 0). This data set contains 441 male patient records and 142 female patient records.
}
\usage{data("ILPD")}
\format{  A data frame with 582 observations on the following 11 variables.
  \describe{
    \item{\code{v1}}{Age of the patient. Any patient whose age exceeded 89 is listed as being of age "90".}
    \item{\code{v2}}{Gender of the patients (male = 1, female = 0).}
    \item{\code{v3}}{Total Bilirubin.}
    \item{\code{v4}}{Direct Bilirubin.}
    \item{\code{v5}}{Alkphos Alkaline Phosphatase.}
    \item{\code{v6}}{Sgpt Alanine Aminotransferase.}
    \item{\code{v7}}{Sgot Aspartate Aminotransferase.}
    \item{\code{v8}}{Total Proteins.}
    \item{\code{v9}}{Albumin.}
    \item{\code{v10}}{A/G Ratio Albumin and Globulin Ratio.}
    \item{\code{Class}}{Liver patient or not (1, 0).}
  }
}

\source{
https://www.openml.org/search?type=data&sort=runs&id=1480&status=active
}
\references{
1. Bendi Venkata Ramana, Prof. M. S. Prasad Babu and Prof. N. B. Venkateswarlu, A Critical Comparative Study of Liver Patients from USA and INDIA: An Exploratory Analysis., International Journal of Computer Science Issues, ISSN:1694-0784, May 2012.

2. Bendi Venkata Ramana, Prof. M. S. Prasad Babu and Prof. N. B. Venkateswarlu, A Critical Study of Selected Classification Algorithms for Liver Disease Diagnosis, International Journal of Database Management Systems (IJDMS), Vol.3, No.2, ISSN : 0975-5705, PP 101-114, May 2011.
}
\examples{
data(ILPD)
str(ILPD)
}
\keyword{ILPD}
