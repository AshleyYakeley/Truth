ARG RESOLVER="lts-24.6"

FROM ubuntu:jammy
# FROM fpco/stack-build-small:${RESOLVER}
RUN echo 2025-08-29 > /container.date

# Debian upgrade
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update
RUN apt-get full-upgrade -y
RUN apt autoremove --purge -y

# workdir
RUN mkdir /extra
WORKDIR /extra

# entrypoint & cmd
RUN apt-get install -y pid1
ENTRYPOINT ["/usr/bin/pid1"]
CMD ["bash"]

# basics
RUN apt-get install -y sudo curl

# environment
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# make sudo work
RUN echo "root ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
RUN echo "ubuntu ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

# stack
RUN curl -sSL https://get.haskellstack.org/ | sh
ARG RESOLVER
RUN stack setup --resolver ${RESOLVER}

# GNOME stuff
RUN apt-get install -y \
    pkg-config build-essential \
    libsoup2.4-dev \
    gobject-introspection \
    libgirepository1.0-dev \
    libglib2.0-dev               gir1.2-glib-2.0 \
    libgdk-pixbuf-2.0-dev        gir1.2-gdkpixbuf-2.0 \
    libpango1.0-dev              gir1.2-pango-1.0 \
    libcairo2-dev                gir1.2-cairo-1.0 \
    libgtk-3-dev                 gir1.2-gtk-3.0 \
    libjavascriptcoregtk-4.0-dev gir1.2-javascriptcoregtk-4.0 \
    libwebkit2gtk-4.0-dev        gir1.2-webkit2-4.0

# various
RUN apt-get install -y git m4 imagemagick librsvg2-bin moreutils lintian

# Python stuff
RUN apt-get install -y \
    python3 \
    python3-venv \
    python3-dev

# Node.js & VScode
RUN curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | sudo gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg
RUN echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_21.x nodistro main" | sudo tee /etc/apt/sources.list.d/nodesource.list
RUN apt-get update
RUN apt-get install -y nodejs
RUN npm --global install @vscode/vsce

# yq
RUN curl -fsSLo /usr/local/bin/yq https://github.com/mikefarah/yq/releases/download/v4.47.1/yq_linux_amd64
RUN chmod +x /usr/local/bin/yq
