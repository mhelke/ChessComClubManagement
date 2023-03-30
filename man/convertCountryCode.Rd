\name{convertCountryCode}
\alias{convertCountryCode}
\title{Convert the Country Code from User Stats}
\usage{
convertCountryCode(countryEndpoint)
}
\description{
Provided for convenience because the other functions will not fetch the country code by default. This helper function can be used if you want the country a user is from. Returns the name of the country.
}
\examples{
convertCountryCode(countryEndpoint = "https://api.chess.com/pub/country/XX")
}
