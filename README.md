# Delphi Flu Hospitalization Production Forecaster

Submitted to the [Flusight repo](https://github.com/cmu-delphi/Flusight-forecast-data).

The definition for the main current forecaster is in `train_model.R`.

## Usage

You can use the CLI utility in the `code/` directory.

```sh
# Run the forecasts and make an evaluation notebook (using exploration cache)
python forecaster.py forecast postprocess notebook

# Run the forecasts on a given date (make sure it's a Tuesday)
FORECAST_DATE="2023-02-28" python forecaster.py forecast postprocess notebook

# Use the production cache
FLU_CACHE="production" python forecaster.py forecast postprocess notebook

# See the CLI help message
python forecaster.py --help
```

## Installation

The following instructions were tested on Ubuntu 20.04 with R 4.2.2.
After cloning this repo and `cd`ing into it, run the following commands:

```sh
# Set your Github PAT as an env var in your profile
# https://docs.github.com/en/enterprise-server@3.9/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens
# (needed because some of Delphi's dependencies are large and will overload the unauthenticated Github API)
echo "GITHUB_PAT=<your token>" | tee -a ~/.zprofile

# Setup Python3
cd code
make install-system-deps
make install-py-deps

# Install the latest R if you don't have it
# https://cloud.r-project.org/bin/linux/ubuntu/
make install-r

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
