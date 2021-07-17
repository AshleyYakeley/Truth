FROM commercialhaskell/stackage:lts17
RUN echo 2021-05-31
ENV DEBIAN_FRONTEND noninteractive
ENV PATH "/usr/local/bin:${PATH}"
RUN wget -qO - https://packages.confluent.io/deb/5.2/archive.key | apt-key add -
RUN apt-get update
RUN apt-get purge -y msodbcsql17
RUN apt-get upgrade -y
RUN apt-get install -y lintian
RUN pip3 install --upgrade pip
RUN pip3 install mkdocs markdown-include Pygments pymdown-extensions
RUN R -e "install.packages(c('shiny','ggplot2','plotly','optparse','shinyWidgets'),dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN npm --global install vsce
RUN apt-get install -y imagemagick
#RUN echo "stack ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers
