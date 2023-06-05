FROM ubuntu:22.04

ARG DEBIAN_FRONTEND=noninteractive
RUN apt update

# Use New Zealand mirrors
RUN sed -i 's/archive/nz.archive/' /etc/apt/sources.list

RUN apt update

# Set timezone to Auckland
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get install -y locales tzdata git
RUN locale-gen en_NZ.UTF-8
RUN dpkg-reconfigure locales
RUN echo "Pacific/Auckland" > /etc/timezone
RUN dpkg-reconfigure -f noninteractive tzdata
ENV LANG en_NZ.UTF-8
ENV LANGUAGE en_NZ:en

# Create user 'user' to create a home directory
RUN useradd user
RUN mkdir -p /home/user/
RUN chown -R user:user /home/user
ENV HOME /home/user

# Install apt packages
RUN apt update
RUN apt install -y curl \
    wget \
    software-properties-common \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    software-properties-common \
    dirmngr

## Install R
# update indices
RUN apt update -qq
# add the signing key (by Michael Rutter) for these repos
# To verify key, run gpg --show-keys /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc 
# Fingerprint: E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
# add the R 4.0 repo from CRAN -- adjust 'focal' to 'groovy' or 'bionic' as needed
RUN add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"

# Copy the renv.lock file
COPY renv /code/renv

RUN apt install -y r-base

# Install renv and set the custom repository
RUN Rscript -e "install.packages('renv', repos = 'https://cloud.r-project.org/')"

# Add a non-root user
RUN useradd -m myuser
USER myuser

# Set RENV_PATHS_CACHE and RENV_PATHS_USER to directories within /code
ENV RENV_PATHS_CACHE /code/.renv-cache
ENV RENV_PATHS_USER /code/.renv-user
ENV HOME /code
