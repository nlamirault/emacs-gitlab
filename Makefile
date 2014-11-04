# Copyright (C) 2014 Nicolas Lamirault <nicolas.lamirault@gmail.com>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


EMACS = emacs
EMACSFLAGS = -L .
CASK = cask
VAGRANT = vagrant

ELS = $(wildcard *.el)
OBJECTS = $(ELS:.el=.elc)

VERSION=$(shell \
        grep Version gitlab.el \
        |awk -F':' '{print $$2}' \
	|sed -e "s/[^0-9.]//g")

PACKAGE_NAME = emacs-gitlab-$(VERSION)

NO_COLOR=\033[0m
OK_COLOR=\033[32;01m
ERROR_COLOR=\033[31;01m
WARN_COLOR=\033[33;01m

all: help

help:
	@echo -e "$(OK_COLOR)==== emacs-gitlab [$(VERSION)]====$(NO_COLOR)"
	@echo -e "$(WARN_COLOR)- init$(NO_COLOR)  : initialize development environment"
	@echo -e "$(WARN_COLOR)- build$(NO_COLOR) : build project"
	@echo -e "$(WARN_COLOR)- test$(NO_COLOR)  : launch unit tests"
	@echo -e "$(WARN_COLOR)- clean$(NO_COLOR) : cleanup"

init:
	@echo -e "$(OK_COLOR)[emacs-gitlab] Initialize environment$(NO_COLOR)"
	@$(CASK) --dev install

elpa:
	@echo -e "$(OK_COLOR)[emacs-gitlab] Build$(NO_COLOR)"
	@$(CASK) install
	@$(CASK) update
	@touch $@

.PHONY: build
build : elpa $(OBJECTS)

test: build
	@echo -e "$(OK_COLOR)[emacs-gitlab] Unit tests$(NO_COLOR)"
	$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-l test/run-tests

package:
	@echo -e "$(OK_COLOR)[emacs-gitlab] Packaging$(NO_COLOR)"
	$(CASK) package

.PHONY: ci
ci : build
	@${CASK} exec ert-runner --no-win < /dev/tty

.PHONY: clean
clean :
	@echo -e "$(OK_COLOR)[emacs-gitlab] Cleanup$(NO_COLOR)"
	@rm -fr $(OBJECTS) elpa

reset : clean
	@rm -rf .cask # Clean packages installed for development

%.elc : %.el
	@$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-f batch-byte-compile $<
