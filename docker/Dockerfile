FROM fpco/stack-build-small:lts-19.5
RUN echo 2022-04-28
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y sudo curl gnome-platform-devel libgirepository1.0-dev lintian python3-pip imagemagick moreutils
RUN pip3 install --upgrade pip
RUN pip3 install mkdocs markdown-include Pygments pymdown-extensions scour
#RUN apt-get install -y r-base-core
#RUN R -e "install.packages(c('shiny','ggplot2','plotly','optparse','shinyWidgets'),dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN curl -fsSL https://deb.nodesource.com/setup_16.x | sudo -E bash -
RUN apt-get install -y nodejs
RUN npm --global install npm
RUN npm --global install vsce
#RUN echo "stack ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers
