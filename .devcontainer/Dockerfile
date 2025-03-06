FROM python:3.12-slim AS build

# Stel de werkdirectory in
WORKDIR /code

# Kopieer alle files naar de werkdirectory
COPY build/ /code/build/
COPY doc/ /code/doc/
COPY extern/ /code/extern/
COPY release/ /code/release/
COPY src/ /code/src/
COPY test/ /code/test/
COPY .gitattributes /code/.gitattributes
COPY .gitignore /code/.gitignore

# RUN pip install -r /code/requirements.txt
RUN apt-get -y update
RUN apt-get -yq install --no-install-recommends git curl wget gpg make cmake gfortran libgomp1
# RUN wget -qO - https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | gpg --dearmor | tee /usr/share/keyrings/oneapi-archive-keyring.gpg > /dev/null
# RUN echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" | tee /etc/apt/sources.list.d/oneAPI.list
# RUN apt-get -y update
# RUN apt-get -y install intel-oneapi-runtime-compilers intel-oneapi-runtime-fortran
RUN apt-get clean -q
RUN cd /code/build && cmake ../src/core
RUN cd /code/build && make
