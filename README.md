# flu-hosp-forecast

Flu hospitalization forecaster for the Delphi team.
Submitted to the [Flusight repo](https://github.com/cmu-delphi/Flusight-forecast-data).

## Usage

You can use the CLI utility in the `code/` directory.

```sh
# Setup Python.
cd code
python3 -m venv venv
pip install -r requirements.txt
source venv/bin/activate

# Run the forecasts and make an evaluation notebook (using exploration cache)
python forecaster.py forecast postprocess notebook

# Run the forecasts on a given date (make sure it's a Tuesday)
FORECAST_DATE="2023-02-28" python forecaster.py forecast postprocess notebook

# Use the production cache
FLU_CACHE="production" python forecaster.py forecast postprocess notebook

# See the CLI help message
python forecaster.py --help
```

# Installation

## Installing System Dependencies

The following instructions were tested on Ubuntu 20.04, with R 4.2.2.

```sh
# Install package dependencies
sudo apt-get install libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    gfortran \
    libblas-dev \
    liblapack-dev \
    libcairo2-dev \
    libglpk-dev \
    libxt-dev \
    pandoc \
    python3 \
    python-is-python3 \
    libharfbuzz-dev \
    libfribidi-dev \
    libbz2-dev

# Install R 4.2.2 https://cloud.r-project.org/bin/linux/ubuntu/

#### Install Gurobi (skip below, because this repo no longer requires this)

# Get a Gurobi license https://www.gurobi.com/downloads/free-academic-license/
# Download Gurobi
wget https://packages.gurobi.com/9.5/gurobi9.5.2_linux64.tar.gz
tar xzvf gurobi9.5.2_linux64.tar.gz
sudo mv gurobi952 /opt/
# Will require a campus network or full VPN.
/opt/gurobi952/linux64/bin/grbgetkey <your key hash>
# It will ask you to enter a path to a license file, I recommend ~/.gurobi/gurobi.lic

# Add variables to your bash profile
echo 'export GUROBI_HOME="/opt/gurobi952"' | tee -a ~/.zprofile
echo 'export GRB_LICENSE_FILE="${HOME}/.gurobi/gurobi.lic"' | tee -a ~/.zprofile

# Add Gurobi libraries to your R shared libs
echo 'R_GUROBI_LIBRARY_PATH="${GUROBI_HOME}/linux64/lib/"' | sudo tee -a /etc/R/ldpaths
echo 'R_LD_LIBRARY_PATH="${R_LD_LIBRARY_PATH}:${R_GUROBI_LIBRARY_PATH}"' | sudo tee -a /etc/R/ldpaths

# Install Gurobi R
Rscript -e 'install.packages("/opt/gurobi952/linux64/R/gurobi_9.5-2_R_4.2.0.tar.gz", repos=NULL)'
# Test the installation
Rscript -e 'library(gurobi)'

#### End Install Gurobi

# Clone this repo
git clone https://github.com/cmu-delphi/hospitalization-forecaster
cd hospitalization-forecaster

# Set your Github PAT as an env var in your profile
# https://docs.github.com/en/enterprise-server@3.5/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token
# (needed because some of Delphi's dependencies are large and will overload the unauthenticated Github API)
echo "GITHUB_PAT=<your token>" | tee -a ~/.zprofile

# Setup a Python virtual env
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt

# Install R dependencies (takes a long time, optionally see r2u instructions below)
python forecaster.py install
```
