\name{GEOtop_animateMAPS}
\alias{GEOtop_animateMAPS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GEOtop output map animation
}
\description{
GEOtop output map animation. Using GEOtop_ReadPlotRst() to visualize GEOtop output maps in pdf format and ImageMagick to animate images.
}
\usage{
GEOtop_animateMAPS(path, mapkey, layers, soil_files, variable, limits, legend, 
                               lowcol="#f7fbff", highcol="#08306b", delay_value=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{path into GEOtop simulation}
  \item{mapkey}{keyword name; see package \emph{geotopbricks}, function \emph{get.geotop.inpts.keyword.value}, argument \emph{keyword}}
  \item{layers}{default = NULL, for variables without layering e.g. snow water eqivalent; integer vector defining layers which should be processed for data with layerinf, e.g. soil moisture}
  \item{soil_files}{boolean, TRUE: soil files are provided as GEOtop input. FALSE: soil is parameterized in the geotop.inpts file}
  \item{variable}{character, variable characterization}
  \item{map}{numeric, vector setting y-limits for plot}
  \item{legend}{character, legend characterization, e.g. "vol\%" for soil moisture data}
  \item{lowcol}{minimum colour for visualization, default = "#f7fbff"}
  \item{highcol}{maximum colour for visualization, default = "#08306b"; default is creating a blue       colour}
  \item{delay_value}{default = NULL, argument for function convert (ImageMagick), display the next image after pausing}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
.gif animation file in folder "path/gif"
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Johannes Brenner, \email{Johannes.Brenner@eurac.edu}
}
\note{
for further information on ImageMagick see \url{http://www.imagemagick.org/}, for information on how to install on LINUX (UHREL, Ubuntu) and CentOS see \url{http://tecadmin.net/install-imagemagick-on-linux/}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
