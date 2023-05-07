FROM r-base:4.2.3

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
COPY renv /code/renv

# Install renv and set the custom repository
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org/')"

# Add a non-root user
RUN useradd -m myuser
USER myuser

# Set RENV_PATHS_CACHE and RENV_PATHS_USER to directories within /code
ENV RENV_PATHS_CACHE /code/.renv-cache
ENV RENV_PATHS_USER /code/.renv-user
ENV HOME /code
