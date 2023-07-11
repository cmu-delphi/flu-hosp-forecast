.PHONY: venv-py
venv-py:
	@python -m venv venv

.PHONY: install-py
install-py: venv-py
	@venv/bin/pip install wheel && venv/bin/pip install -r requirements.txt

.PHONY: install-system-dependencies
install-system-dependencies:
	@sudo apt-get update && apt-get install -y \
		libproj-dev \
		gfortran \
		libblas-dev \
		liblapack-dev \
		libcairo2-dev \
		libglpk-dev \
		libxt-dev \
		pandoc \
		python3 \
		libharfbuzz-dev \
		libfribidi-dev \
		libbz2-dev \
		libcurl4-openssl-dev \
		libssl-dev \
		libxml2-dev \
		libudunits2-dev \
		libgdal-dev \
		libgeos-dev

.PHONY: install-r
install-r:
	@# Install R 4.2.2 from https://cloud.r-project.org/bin/linux/ubuntu/
	@# update indices
	@sudo apt update -qq
	@# install two helper packages we need
	@sudo apt install --no-install-recommends software-properties-common dirmngr
	@# add the signing key (by Michael Rutter) for these repos
	@# To verify key, run gpg --show-keys /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc 
	@# Fingerprint: E298A3A825C0D65DFD57CBB651716619E084DAB9
	@wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
	@# add the R 4.0 repo from CRAN -- adjust 'focal' to 'groovy' or 'bionic' as needed
	@sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"

.PHONY: install-renv	
install-renv: 
	@Rscript -e "install.packages(c('pak', 'renv'))"

.PHONY: install-r-packages
install-r-packages: install-renv
	@Rscript -e "renv::restore()"
