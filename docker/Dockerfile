FROM fpco/stack-build:lts-16.15
ENV DEBIAN_FRONTEND noninteractive
RUN echo 2020-09-19
RUN apt-get update
RUN apt-get purge -y msodbcsql17
RUN apt-get upgrade -y
RUN apt-get install -y lintian
RUN pip3 install --upgrade pip
RUN pip3 install mkdocs markdown-include Pygments
RUN R -e "install.packages(c('shiny','ggplot2','plotly','optparse','shinyWidgets'),dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN npm --global install vsce
#RUN echo "stack ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers
