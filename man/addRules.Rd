% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validRules.R
\name{addRules}
\alias{addRules}
\title{Add Rules to a Validation Rules List}
\usage{
addRules(x, y, ruleset = "default")
}
\arguments{
\item{x}{Named List or vector object to add to}

\item{y}{Named List or vector to append to x}

\item{ruleset}{Character Name of rule set to add the rules to
in x. If not present, it will be created.}
}
\description{
Generic to add items to a S3 list or vector
}
\examples{
# Add rule to default 
rulesObj <- validRules()
rulesObj <- rulesObj \%>\% 
    addRules(list('newrule' = function(x) x =='B'), ruleset = 'newRuleSet')
    
str(rulesObj)

}
\seealso{
Other validation: 
\code{\link{editRules}()},
\code{\link{makeValidData}()},
\code{\link{validRules}()},
\code{\link{validateColumn}()},
\code{\link{validateDF}()}
}
\concept{validation}
