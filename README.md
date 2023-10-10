# Delphi Flu Hospitalization Production Forecaster

Submitted to the [2023-2024 FluSight repo](https://github.com/cdcepi/FluSight-forecast-hub).

## Usage

After getting Python setup, you can use the CLI utility.

```sh
# Setup, activate venv, and install dependencies
make install-py-deps
source venv/bin/activate

# Run the forecasts and make an evaluation notebook
python run.py forecast

# Run the forecasts on a given date
FORECAST_DUE_DATE="2023-10-11" python run.py forecast

# See more CLI commands
python run.py --help
```

## Directory Layout

-   `run.R` - the main R script for the forecaster
-   `run.py` - a CLI utility for running the forecaster and submitting to the
    FluSight repo
-   `Makefile` - a Makefile for installing dependencies
-   `R/` - importable R functions for the forecaster
-   `scripts/` - other pipeline scripts

## Installation

The following instructions were tested on Ubuntu 20.04 with R 4.2.2.
After cloning this repo and `cd`ing into it, run the following commands:

```sh
# Set your Github PAT as an env var in your profile
# https://docs.github.com/en/enterprise-server@3.9/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens
# (needed because some of Delphi's dependencies are large and will overload the unauthenticated Github API)
echo "GITHUB_PAT=<your token>" | tee -a ~/.zprofile

# Setup Python3
make install-py-deps
source venv/bin/activate

# Install the latest R if you don't have it: https://cloud.r-project.org/bin/linux/ubuntu/

# Install R dependencies
make install-r-deps
```

### Installing Gurobi

```sh
# Get a Gurobi license https://www.gurobi.com/downloads/free-academic-license/
# Download Gurobi
wget https://packages.gurobi.com/9.5/gurobi9.5.2_linux64.tar.gz
tar xzvf gurobi9.5.2_linux64.tar.gz
sudo mv gurobi952 /opt/
# grbgetkey requires a campus network or full VPN
/opt/gurobi952/linux64/bin/grbgetkey <your key hash>
# It will ask you to enter a path to a license file, I recommend ~/.gurobi/gurobi.lic

# Add variables to your bash profile
echo 'export GUROBI_HOME="/opt/gurobi952"' | tee -a ~/.zprofile
echo 'export GRB_LICENSE_FILE="${HOME}/.gurobi/gurobi.lic"' | tee -a ~/.zprofile

# Add Gurobi libraries to your R shared libs
echo 'R_GUROBI_LIBRARY_PATH="${GUROBI_HOME}/linux64/lib/"' | sudo tee -a /etc/R/ldpaths
echo 'R_LD_LIBRARY_PATH="${R_LD_LIBRARY_PATH}:${R_GUROBI_LIBRARY_PATH}"' | sudo tee -a /etc/R/ldpaths

# Install Gurobi R
Rscript -e 'renv::install("/opt/gurobi952/linux64/R/gurobi_9.5-2_R_4.2.0.tar.gz")'
# Test the installation
Rscript -e 'library(gurobi)'
```
