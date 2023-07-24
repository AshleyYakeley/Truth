FROM fpco/stack-build-small:lts-21.3
RUN echo 2023-07-22 > /container.date
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update
RUN apt-get full-upgrade -y
RUN apt autoremove --purge -y
RUN apt-get install -y sudo git curl m4 libgmp-dev gnome-platform-devel libgtk-3-dev libgirepository1.0-dev python3-pip imagemagick moreutils
RUN wget https://github.com/mikefarah/yq/releases/latest/download/yq_linux_amd64 -O /usr/local/bin/yq
RUN chmod +x /usr/local/bin/yq
RUN pip3 install --upgrade pip
RUN pip3 install mkdocs markdown-include Pygments scour
RUN mkdir /extra
WORKDIR /extra
RUN curl -O http://http.us.debian.org/debian/pool/main/l/lintian/lintian_2.116.3_all.deb
RUN apt install -y /extra/lintian_2.116.3_all.deb
RUN git clone -n https://github.com/AshleyYakeley/mkdocs-plugin-commonmark.git
RUN cd mkdocs-plugin-commonmark && git fetch && git checkout 1033d5dfc0223fd499e73adb1aa1be7f99269b4a
RUN pip3 install ./mkdocs-plugin-commonmark
#RUN apt-get install -y r-base-core
#RUN R -e "install.packages(c('shiny','ggplot2','plotly','optparse','shinyWidgets'),dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
RUN apt-get install -y nodejs
RUN npm --global install @vscode/vsce
RUN echo "stack ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers
