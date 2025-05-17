# Contributing

We love pull requests from everyone. By participating in this project, you
agree to abide by our [code of conduct](CONDUCT.md).

## Getting Started

* Make sure you have a [GitHub account](https://github.com/signup/free). If you are not familiar with git and GitHub, take a look at <http://happygitwithr.com/> to get started.
* [Submit a post for your issue](https://github.com/rmk118/morphmat/issues), assuming one does not already exist.
  * Clearly describe your issue, including steps to reproduce when it is a bug, or some justification for a proposed improvement.
* [Fork](https://github.com/rmk118/morphmat/fork) the repository on GitHub or in your shell terminal to make a copy of the repository on your account.
    
## Making changes

* Before you make a Pull Request, make sure you have discussed your proposed change in an issue post and that the team support your proposed change.
* We recommend that you create a Git branch for each pull request (PR).
* Edit the files, save often, and make commits of logical units, where each commit indicates one concept
* Follow the Tidyverse [style guide](https://style.tidyverse.org/).
* Make sure you write [good commit messages](http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html).
* We use testthat. Contributions with test cases included are easier to accept.
* For user-facing changes, add a bullet to the top of NEWS.md below the current development version header describing the changes made followed by your GitHub username, and links to relevant issue(s)/PR(s).
* Run _all_ the tests using `devtools::check()` to assure nothing else was accidentally broken.
* If you need help or unsure about anything, post an update to [your issue](https://github.com/rmk118/morphmat/issues/).

## Submitting your changes

Push to your fork and [submit a pull request](https://github.com/rmk118/morphmat/compare/).

At this point you're waiting on us. We like to at least comment on pull requests within a few days (and, typically, one business day). We may suggest some changes or improvements or alternatives.

Some things you can do that will increase the chance that your pull request is accepted:

* Engage in discussion on [your issue](https://github.com/rmk118/morphmat/issues/).
* Be familiar with the background literature cited in the [README](README.Rmd)
* Write tests that pass `devtools::check()`.
* Follow the [Tidyverse style guide](https://style.tidyverse.org/).

## Acknowledgements

This contributing guide was modified from the CONTRIBUTING.md file from the rrtools package (https://github.com/benmarwick/rrtools/blob/master/CONTRIBUTING.md), an excellent collection of tools for writing reproducible reports and articles in R.

