FROM debian:bookworm-slim

# Set the working directory
WORKDIR /code

# Install all dependencies in a single layer to reduce image size
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    git \
    make \
    cmake \
    gfortran \
    libgomp1 \
    tzdata \
    && rm -rf /var/lib/apt/lists/* \
    && apt-get clean

# Copy source files
COPY doc/ /code/doc/
COPY extern/ /code/extern/
COPY release/ /code/release/
COPY src/ /code/src/
COPY test/ /code/test/
COPY .gitattributes /code/.gitattributes
COPY .gitignore /code/.gitignore

# Create build directory
RUN mkdir -p /code/build
