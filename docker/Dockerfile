FROM fpco/stack-build:lts-15.5
RUN pip3 install --upgrade pip
RUN pip3 install mkdocs
RUN pip3 install markdown-include
RUN pip3 install Pygments
RUN R -e "install.packages('shiny',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('optparse',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets',dependencies=TRUE, repos='http://cran.rstudio.com/')"
#RUN echo "stack ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers
