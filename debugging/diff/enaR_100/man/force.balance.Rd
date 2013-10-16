\name{force.balance}
\alias{force.balance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Repeated, sequential application the balance function.
}
\description{
This function repeatedly balances a model, sequentially with the output
being passed back to the balance function, until it is  within tolerance
or the maximum number of iterations is reached. 
}
\usage{
force.balance(x='network object',tol=5,max.itr=10,method='AVG2')
}

\arguments{
  \item{x}{
    A network object.
  }
  \item{tol}{
    Percent error tolerance for difference between inputs and outputs.
  }
  \item{max.itr}{
    Maximum number iterations.
  }
  \item{method}{
    The balancing method to use, see balance.
  }
}
\details{

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a balanced network model.
}
\references{
%% ~put references to the literature/web site here ~
Allesina, S., Bondavalli, C., 2003.Steady state of ecosystem flow
networks: a comparison between balancing procedures.Ecological Modelling 165(2-3):231-239.
}
\author{
%%  ~~who you are~~
Matthew K. Lau (mkl48@nau.edu)
Stuart R. Borrett (borretts@uncw.edu)
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{balance}}
}
\examples{

}
