venv-py: venv
	@python -m venv venv

.PHONY: install-py-deps
install-py-deps: venv-py
	@venv/bin/pip install wheel && venv/bin/pip install -r requirements.txt

.PHONY: install-system-deps
install-system-deps:
	-@sudo apt-get update; sudo apt-get install -y \
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
		libgeos-dev \
		apt-file && sudo apt-file update

install-r-deps: renv
	@Rscript -e "install.packages(c('pak', 'renv', 'rspm'))"
	@Rscript -e "rspm::enable(); renv::restore()"
