FROM fpco/stack-build-small:lts-22.14
RUN echo 2024-03-28 > /container.date

# Debian upgrade
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update
RUN apt-get full-upgrade -y
RUN apt autoremove --purge -y

# various APT stuff
RUN apt-get install -y sudo git curl m4 libgmp-dev gnome-platform-devel libgtk-3-dev libgirepository1.0-dev python3-pip python3-venv imagemagick moreutils

# yq
RUN wget https://github.com/mikefarah/yq/releases/latest/download/yq_linux_amd64 -O /usr/local/bin/yq
RUN chmod +x /usr/local/bin/yq

# various Python stuff
RUN pip3 install --upgrade pip
RUN pip3 install mkdocs==1.4.3 markdown-include Pygments scour build
RUN mkdir /extra
WORKDIR /extra

# lintian
RUN curl -O http://http.us.debian.org/debian/pool/main/l/lintian/lintian_2.116.3_all.deb
RUN apt install -y /extra/lintian_2.116.3_all.deb

# mkdocs-plugin-commonmark
RUN git clone -n https://github.com/AshleyYakeley/mkdocs-plugin-commonmark.git
RUN cd mkdocs-plugin-commonmark && git fetch && git checkout 1033d5dfc0223fd499e73adb1aa1be7f99269b4a
RUN pip3 install ./mkdocs-plugin-commonmark

# R
#RUN apt-get install -y r-base-core
#RUN R -e "install.packages(c('shiny','ggplot2','plotly','optparse','shinyWidgets'),dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Node.js & VScode
RUN curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | sudo gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg
RUN echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_21.x nodistro main" | sudo tee /etc/apt/sources.list.d/nodesource.list
RUN apt-get update
RUN apt-get install -y nodejs
RUN npm --global install @vscode/vsce

# make sudo work
RUN echo "stack ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers
