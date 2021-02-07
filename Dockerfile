FROM rocker/tidyverse

RUN apt update
RUN apt -y upgrade
RUN apt install -y python3-pip
RUN mkdir /home/main

COPY requirements.txt /tmp/pip-tmp/
RUN pip3 --disable-pip-version-check --no-cache-dir install -r /tmp/pip-tmp/requirements.txt \
    && rm -rf /tmp/pip-tmp
COPY requirements.r /tmp/r-tmp/
RUN Rscript /tmp/r-tmp/requirements.r && rm -rf /tmp/r-tmp

# RUN apt-get -y install systemd

COPY main.R /home/main/
COPY main.py /home/main/


WORKDIR /home/main

# CMD [ "python3", "main.py" ]


# # [Optional] Uncomment this section to install additional OS packages.
# RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
#    && apt-get -y install --no-install-recommends r-base-dev python3-pip


