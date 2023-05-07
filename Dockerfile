FROM r-base:4.3.0

ARG DEBIAN_FRONTEND=noninteractive
RUN apt update

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
    libfontconfig1-dev

# Copy the renv.lock file
COPY renv /code

# Install renv and set the custom repository
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org/')"

WORKDIR /code
RUN R -e "renv::restore(lockfile='renv/profiles/model/renv.lock')"

# Add a non-root user
RUN useradd -m myuser
USER myuser
