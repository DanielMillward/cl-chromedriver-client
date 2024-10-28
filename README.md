# cl-chromedriver-client

A simple way to interface with Google's [ChromeDriver](https://developer.chrome.com/docs/chromedriver). 

## Installation

This project isn't on any package repository (mostly because I haven't bothered to look up how to do it). For right now, clone this into your ~/quicklisp/local-projects/ directory (assuming you have quicklisp installed).

Cl-chromedriver-client assumes you have ChromeDriver already installed. If you're on Windows, it also assumes you've added the path to your chromedriver executable to the PATH variable.

Cl-chromedriver-client also assumes you've installed Chrome. For Windows, going through the normal Chrome installation process should be enough to get it working. For Linux, make sure your chrome binary is installed at `usr/bin/google-chrome`. For more information, see [this StackOverflow answer](https://stackoverflow.com/a/49795348).


## Usage

