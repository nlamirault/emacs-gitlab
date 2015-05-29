# emacs-gitlab

[![License GPL 3][badge-license]][LICENSE]
[![travis][badge-travis]][travis]
[![drone][badge-drone]][drone]
[![Melpa Status](http://melpa.milkbox.net/packages/gitlab-badge.svg)](http://melpa.milkbox.net/#/gitlab)
[![MELPA Stable](http://stable.melpa.org/packages/gitlab-badge.svg)](http://stable.melpa.org/#/gitlab)
[![Coverage Status](https://coveralls.io/repos/nlamirault/emacs-gitlab/badge.png)](https://coveralls.io/r/nlamirault/emacs-gitlab)


Emacs package `gitlab` provides :
* a REST client to the [Gitlab][] API
* a [Helm][] interface

## Installation

The recommended way to install ``gitlab`` is via [MELPA][]:

    M-x package-install gitlab

or [Cask][]:

	(depends-on "gitlab")


## Usage

* Setup your Gitlab configurations :

        $ (setq gitlab-host "http://mygitlab.com"
                gitlab-username "foo"
                gitlab-password "bar"
                gitlab-token-id "xxxxxxxxxxxx")

To generate private ``gitlab-token-id`` run:

```
$ curl http://yourgitlabhost/api/v3/session/ --data-urlencode 'login=yourUserName' --data-urlencode 'password=yourPassword'
```


* Show user's projects with helm interface:

        $ M-x helm-gitlab-projects

[projects](var/emacs-gitlab-0.3-helm-projects.png)


* Show user's issues with helm interface:

        $ M-x helm-gitlab-issues

[issues](var/emacs-gitlab-0.3-helm-issues.png)


## Development

### Cask

``emacs-gitlab`` use [Cask][] for dependencies
management. Install it and retrieve dependencies :

    $ curl -fsSkL https://raw.github.com/cask/cask/master/go | python
    $ export PATH="$HOME/.cask/bin:$PATH"
    $ cask


### Tests

* Setup your Gitlab informations :

        $ export GITLAB_HOST="http://gitlab.foo.com"
        $ export GITLAB_USERNAME="foo"
        $ export GITLAB_PASSWORD="bar"
        $ export GITLAB_TOKEN_ID="xxxxxxxxxxxxxx"
        $ export GITLAB_PROJECT_ID="111222"
        $ export GITLAB_PROJECT_NAME="myproject"
        $ export GITLAB_PROJECT_DESCRIPTION="a project description"
        $ export GITLAB_ISSUE_ID="1"
        $ export GITLAB_ISSUE_TITLE="the issue title"

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
[badge-drone]: https://drone.io/github.com/nlamirault/emacs-gitlab/status.png
[drone]: https://drone.io/github.com/nlamirault/emacs-gitlab/latest
[GNU Emacs]: https://www.gnu.org/software/emacs/
[MELPA]: http://melpa.milkbox.net/
[Cask]: http://cask.github.io/
[Issue tracker]: https://github.com/nlamirault/emacs-gitlab/issues

[Gitlab]: https://www.gitlab.com/
[Helm]: https://github.com/emacs-helm/helm
