# emacs-gitlab

[![License GPL 3][badge-license]][LICENSE]
[![Coverage Status](https://coveralls.io/repos/nlamirault/emacs-gitlab/badge.png)](https://coveralls.io/r/nlamirault/emacs-gitlab)

Master :
* [![MELPA Stable](https://stable.melpa.org/packages/gitlab-badge.svg)](https://stable.melpa.org/#/gitlab)
* [![Circle CI](https://circleci.com/gh/nlamirault/emacs-gitlab/tree/master.svg?style=svg)](https://circleci.com/gh/nlamirault/emacs-gitlab/tree/master)

Develop:
* [![Melpa Status](https://melpa.org/packages/gitlab-badge.svg)](https://melpa.org/#/gitlab)
* [![Circle CI](https://circleci.com/gh/nlamirault/emacs-gitlab/tree/develop.svg?style=svg)](https://circleci.com/gh/nlamirault/emacs-gitlab/tree/develop)

Package `gitlab` provides a REST client to the [Gitlab][] API.

## Installation

### Installation via package.el

`package.el` is the built-in package manager in Emacs.

emacs-gitlab is available on the two major community maintained repositories -
[MELPA STABLE](melpa-stable.milkbox.net), [MELPA](http://melpa.milkbox.net).

You can install `gitlab` with the following commnad:

<kbd>M-x package-install [RET] gitlab [RET]</kbd>

or by adding this bit of Emacs Lisp code to your Emacs initialization file
(`.emacs` or `init.el`):

```el
(unless (package-installed-p 'gitlab)
  (package-install 'gitlab))
```

If the installation doesn't work try refreshing the package list:

<kbd>M-x package-refresh-contents [RET]</kbd>

Keep in mind that MELPA packages are built automatically from
the `master` branch, meaning bugs might creep in there from time to
time. Never-the-less, installing from MELPA is the recommended way of
obtaining emacs-gitlab, as the `master` branch is normally quite stable and
"stable" (tagged) builds are released somewhat infrequently.

With the most recent builds of Emacs, you can pin emacs-gitlab to always
use MELPA Stable by adding this to your Emacs initialization:

```el
(add-to-list 'package-pinned-packages '(gitlab . "melpa-stable") t)
```

### Via el-get

[el-get](https://github.com/dimitri/el-get) is another popular package manager for Emacs. If you're an el-get
user just do <kbd>M-x el-get-install [RET] gitlab [RET]</kbd>.

### Manual

You can install emacs-gitlab manually by placing it on your `load-path` and
`require` ing it. Many people favour the folder `~/.emacs.d/vendor`.

```el
(add-to-list 'load-path "~/.emacs.d/vendor/")
(require 'gitlab)
```

## Usage

* Setup your Gitlab configurations :

        $ (setq gitlab-host "https://gitlab.com"
                gitlab-token-id "foo")

* Show user's projects with helm interface:

        $ M-x helm-gitlab-projects

[projects](var/emacs-gitlab-0.3-helm-projects.png)


* Show user's issues with helm interface:

        $ M-x helm-gitlab-issues

* Open current issue:

        $ M-x gitlab-open-issue
        $ o

* Close current issue:

        $ M-x gitlab-close-issue
        $ c

## Development

### Cask

``gitlab`` use [Cask][] for dependencies management.
Install it and retrieve dependencies :

    $ curl -fsSkL https://raw.github.com/cask/cask/master/go | python
    $ export PATH="$HOME/.cask/bin:$PATH"
    $ cask


### Tests

* Setup your Gitlab informations :

        $ cat $HOME/.emacs-gitlab.rc
        #!/bin/bash
        export GITLAB_HOST="https://gitlab.com"
        export GITLAB_TOKEN_ID="yourtokenid"
        export GITLAB_PROJECT_ID=111222
        export GITLAB_PROJECT_NAME="myproject"
        export GITLAB_PROJECT_DESCRIPTION="a project description"
        export GITLAB_ISSUE_ID=145645
        export GITLAB_ISSUE_TITLE="the issue title"

* Launch unit tests :

        $ . $HOME/.emacs-gitlab.rc
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
[badge-drone]: https://drone.io/github.com/nlamirault/emacs-gitlab/status.png
[drone]: https://drone.io/github.com/nlamirault/emacs-gitlab/latest

[GNU Emacs]: https://www.gnu.org/software/emacs/
[MELPA]: https://melpa.org
[Cask]: http://cask.github.io/
[Issue tracker]: https://github.com/nlamirault/emacs-gitlab/issues

[Gitlab]: https://www.gitlab.com/
[Helm]: https://github.com/emacs-helm/helm
