# ChessComClubManagement

## Overview

ChessComClubManagement is a convenient package that can fetch useful information on your club to help you better manage members. All you need to know is the club ID and soon you'll have all the useful stats you need for effective data processing. Member information includes:
  * Username and link to their profile
  * Date user joined the club
  * Date user joined the site
  * Date user was last online
  * Country
  * Daily standard and 960 rating
  * Overall average time per move
  * Overall timeout percentage
  * Total matches entered for the club
  * Total timeouts in matches for the club
  * Total wins in matches for the club
  * Activity status within the club

## How to install

#### Option 1: Download the release

1. Download the latest package release
2. Download the latest Rtools stable release version here if not previously installed: https://cran.r-project.org/bin/windows/Rtools/
3. In the terminal, run `R CMD INSTALL -build -I [path to downloaded asset]`
4. From the R console, run `install.packages("[path to downloaded asset]", repors=NULL, type="source")`

#### Option 2: Install from GitHub

1. Download the latest Rtools stable release version here if not previously installed: https://cran.r-project.org/bin/windows/Rtools/
2. Install the package `devtools` from CRAN: `install.packages('devtools')`
3. Run the following command from the R console: `devtools::install_github('mhelke/ChessComClubManagement')`

## Author

Matthew Helke

* Contact: [matthewhelke52@gmail.com](mailto:matthewhelke52@gmail.com)
* Github: [mhelke](https://github.com/mhelke)
* LinkedIn: [matthew-helke](https://www.linkedin.com/in/matthew-helke)

## Contributing

Issues, and feature requests are welcome!
Please add your request to the [issues page](https://github.com/mhelke/ChessComClubManagement/issues)

## License

Copyright (c) 2023 Matthew Helke

This project is [MIT licensed](https://github.com/mhelke/ChessComClubManagement/blob/master/LICENSE.md)
