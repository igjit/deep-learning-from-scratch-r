FROM rocker/tidyverse:4.2.1

RUN apt update && apt install -y \
    libxt6 \
    python3-pip \
 && rm -rf /var/lib/apt/lists/*

RUN pip3 install numpy

RUN install2.r --error \
    dslabs \
    reticulate \
 && rm -rf /tmp/downloaded_packages/
