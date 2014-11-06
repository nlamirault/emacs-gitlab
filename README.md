# emacs-gitlab

[![License GPL 3][badge-license]][LICENSE]
[![travis][badge-travis]][travis]
[![Melpa Status](http://melpa.milkbox.net/packages/gitlab-badge.svg)](http://melpa.milkbox.net/#/gitlab)
[![MELPA Stable](http://stable.melpa.org/packages/gitlab-badge.svg)](http://stable.melpa.org/#/gitlab)

`emacs-gitlab` provides :
* a REST client to the [Gitlab][] API
* a [Helm][] interface

## Installation

The recommended way to install ``emacs-gitlab`` is via [MELPA][]:

    M-x package-install emacs-gitlab

or [Cask][]:

	(depends-on "emacs-gitlab")


## Usage

* Setup your Gitlab configurations :

        $ (setq gitlab-host "http://mygitlab.com"
                gitlab-username "foo"
                gitlab-password "bar")

* Show user's projects with helm interface:

        $ M-x helm-gitlab-projects

* Show user's issues with helm interface:

        $ M-x helm-gitlab-issues


## Development

### Cask

``emacs-gitlab`` use [Cask][] for dependencies
management. Install it and retrieve dependencies :

    $ curl -fsSkL https://raw.github.com/cask/cask/master/go | python
    $ export PATH="$HOME/.cask/bin:$PATH"
    $ cask


### Tests

* Edit ``test/gitlab-test-settings.el``, and setup your Gitlab informations:

        (require 'gitlab)
        (setq gitlab-host "http://gitlab.foo.com"
              gitlab-username "foo"
              gitlab-password "bar")
        (provide 'gitlab-test-settings)


* Launch unit tests :

        $ make clean test


## Support / Contribute

See [here](CONTRIBUTING.md)



## Changelog

A changelog is available [here](ChangeLog.md).


## License

See [LICENSE](LICENSE).


## Contact

Nicolas Lamirault <nicolas.lamirault@gmail.com>

[emacs-gitlab]: https://github.com/nlamirault/emacs-gitlab
[badge-license]: https://img.shields.io/badge/license-GPL_2-green.svg?style=flat
[LICENSE]: https://github.com/nlamirault/emacs-gitlab/blob/master/LICENSE
[travis]: https://travis-ci.org/nlamirault/emacs-gitlab
[badge-travis]: http://img.shields.io/travis/nlamirault/emacs-gitlab.svg?style=flat
[GNU Emacs]: https://www.gnu.org/software/emacs/
[MELPA]: http://melpa.milkbox.net/
[Cask]: http://cask.github.io/
[Issue tracker]: https://github.com/nlamirault/emacs-gitlab/issues

[Gitlab]: https://www.gitlab.com/
[Helm]: https://github.com/emacs-helm/helm
