FROM fpco/stack-build-small:lts-20.17
RUN echo 2023-04-05 > /container.date
ENV DEBIAN_FRONTEND noninteractive
COPY sources.list /etc/apt/sources.list
RUN apt-get update
RUN apt-get purge -y --allow-remove-essential login
RUN apt-get full-upgrade -y
RUN apt autoremove --purge -y
RUN apt-get install -y login sudo curl m4 gnome-platform-devel libgtk-3-dev libgirepository1.0-dev lintian python3-pip imagemagick moreutils
RUN pip3 install --upgrade pip
RUN pip3 install yq mkdocs markdown-include Pygments scour
RUN mkdir /extra
WORKDIR /extra
RUN git clone -n https://github.com/AshleyYakeley/mkdocs-plugin-commonmark.git
RUN cd mkdocs-plugin-commonmark && git fetch && git checkout 1033d5dfc0223fd499e73adb1aa1be7f99269b4a
RUN pip3 install ./mkdocs-plugin-commonmark
#RUN apt-get install -y r-base-core
#RUN R -e "install.packages(c('shiny','ggplot2','plotly','optparse','shinyWidgets'),dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN apt-get install -y npm
RUN npm --global install npm
RUN npm --global install @vscode/vsce
#RUN echo "stack ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers
