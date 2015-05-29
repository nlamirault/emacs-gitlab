# Copyright (C) 2014, 2015 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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

APP = gitlab

EMACS = emacs
EMACSFLAGS = -L .
CASK = cask

VERSION=$(shell \
        grep Version gitlab.el \
        |awk -F':' '{print $$2}' \
	|sed -e "s/[^0-9.]//g")

PACKAGE_FOLDER=$(APP)-$(VERSION)
ARCHIVE=$(PACKAGE_FOLDER).tar

ELS = $(wildcard *.el)
OBJECTS = $(ELS:.el=.elc)

NO_COLOR=\033[0m
OK_COLOR=\033[32;01m
ERROR_COLOR=\033[31;01m
WARN_COLOR=\033[33;01m

all: help

help:
	@echo -e "$(OK_COLOR)==== $(APP) [$(VERSION)]====$(NO_COLOR)"
	@echo -e "$(WARN_COLOR)- init$(NO_COLOR)    : initialize development environment"
	@echo -e "$(WARN_COLOR)- build$(NO_COLOR)   : build project"
	@echo -e "$(WARN_COLOR)- test$(NO_COLOR)    : launch unit tests"
	@echo -e "$(WARN_COLOR)- clean$(NO_COLOR)   : cleanup"
	@echo -e "$(WARN_COLOR)- package$(NO_COLOR) : packaging"

init:
	@echo -e "$(OK_COLOR)[$(APP)] Initialize environment$(NO_COLOR)"
	@$(CASK) --dev install

elpa:
	@echo -e "$(OK_COLOR)[$(APP)] Build$(NO_COLOR)"
	@$(CASK) install
	@$(CASK) update
	@touch $@

.PHONY: build
build : elpa $(OBJECTS)

test: clean build
	@echo -e "$(OK_COLOR)[$(APP)] Unit tests$(NO_COLOR)"
	@$(CASK) exec ert-runner

.PHONY: clean
clean :
	@echo -e "$(OK_COLOR)[$(APP)] Cleanup$(NO_COLOR)"
	@rm -fr $(APP)-pkg.el $(OBJECTS) elpa $(ARCHIVE).gz

reset : clean
	@rm -rf .cask

pkg-file:
	@$(CASK) pkg-file

pkg-el: pkg-file
	@$(CASK) package

package: clean pkg-el
	@echo -e "$(OK_COLOR)[$(APP)] Packaging$(NO_COLOR)"
	cp dist/$(ARCHIVE) .
	gzip $(ARCHIVE)
	rm -fr dist

%.elc : %.el
	@$(CASK) exec $(EMACS) --no-site-file --no-site-lisp --batch \
		$(EMACSFLAGS) \
		-f batch-byte-compile $<
