FROM fpco/stack-build-small:lts-22.21
RUN echo 2024-09-08 > /container.date

# Debian upgrade
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update
RUN apt-get full-upgrade -y
RUN apt autoremove --purge -y

# various APT stuff
RUN apt-get install -y sudo git curl m4 imagemagick librsvg2-bin moreutils python3-pip python3-venv lintian
RUN apt-get install -y \
    libgmp-dev gnome-platform-devel libgtk-3-dev libgirepository1.0-dev libjavascriptcoregtk-4.0-dev libsoup-gnome2.4-dev libwebkit2gtk-4.0-dev

# yq
RUN wget https://github.com/mikefarah/yq/releases/latest/download/yq_linux_amd64 -O /usr/local/bin/yq
RUN chmod +x /usr/local/bin/yq

# various Python stuff
RUN pip3 install --upgrade pip
RUN pip3 install sphinx myst-parser sphinx_rtd_theme Pygments scour build
RUN mkdir /extra
WORKDIR /extra

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
