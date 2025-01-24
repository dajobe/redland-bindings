FROM debian:testing

RUN apt-get update && apt-get install -y \
    autoconf \
    libtool \
    python-dev \
    librdf0-dev \
    librdf0 \
    libraptor2-dev \
    librasqal3-dev \
    librasqal3 \
    swig3.0 \
    git \
    make \
    vim \
    r-recommended
RUN R --no-save -e 'options(repos = c(CRAN = "http://cran.rstudio.com")); install.packages(c("devtools", "testthat", "roxygen2", "knitr"))' 
RUN cd /usr/bin && ln -s swig3.0 swig && cd /
RUN git clone https://github.com/ropensci/redland-bindings.git
RUN cd /redland-bindings && \
    ./autogen.sh --with-R && \
    make
RUN echo "NEXT STEPS:" && \
    echo "Build & run the container interactively and make should work:" && \
    echo "$ docker build -t redland-bindings ." && \
    echo "$ docker run --rm -it redland-bindings /bin/bash" && \
    echo "cd /redland-bindings/R && make check-local"
