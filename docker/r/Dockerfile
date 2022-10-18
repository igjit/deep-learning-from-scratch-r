FROM rocker/tidyverse:4.2.1

RUN apt update && apt install -y \
    libxt6 \
 && rm -rf /var/lib/apt/lists/*

RUN install2.r --error \
    dslabs \
 && rm -rf /tmp/downloaded_packages/
