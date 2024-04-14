# Project options
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information
project = "Pinafore"
copyright = "2024, Ashley Yakeley"
author = "Ashley Yakeley"
release = "0.5"

# Sphinx options
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration
extensions = ["myst_parser","sphinx_rtd_theme"]
templates_path = ["_templates"]
exclude_patterns = []

# HTML options
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output
html_theme = "sphinx_rtd_theme"
html_static_path = ["_static"]
html_css_files = ["extra.css"]
html_show_sourcelink = False

# MyST options
myst_heading_anchors = 3
