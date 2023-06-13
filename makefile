REPO_NAME := $(shell basename `git rev-parse --show-toplevel` | tr '[:upper:]' '[:lower:]')
GIT_TAG ?= $(shell git log --oneline | head -n1 | awk '{print $$1}')
DOCKER_REGISTRY := mathematiguy
IMAGE := $(DOCKER_REGISTRY)/$(REPO_NAME)
HAS_DOCKER ?= $(shell which docker)
UID ?= $(shell id -g)
GID ?= $(shell id -u)
RUN ?= $(if $(HAS_DOCKER), docker run $(DOCKER_ARGS) -it --rm -v $$(pwd):/code -w /code -u $(UID):$(GID) $(IMAGE))
DOCKER_ARGS ?=

.PHONY: docker docker-push docker-pull enter enter-root

install: renv/profiles/buildsite/renv/library/R-4.2/x86_64-pc-linux-gnu renv/profiles/model/renv/library/R-4.2/x86_64-pc-linux-gnu

renv/profiles/%/renv/library/R-4.2/x86_64-pc-linux-gnu:
	$(RUN) Rscript -e "renv::restore(lockfile='renv/profiles/$*/renv.lock')"

.cmdstan/cmdstan-2.32.2:
	$(RUN) Rscript -e "cmdstanr::install_cmdstan()"

buildsite: $(shell ls *.qmd | sed 's/\.qmd/.html/g')

%.html: %.qmd
	$(RUN) quarto render $<

run: renv/profiles/buildsite/renv/library/R-4.2/x86_64-pc-linux-gnu \
	renv/profiles/model/renv/library/R-4.2/x86_64-pc-linux-gnu \
	.cmdstan/cmdstan-2.32.2
	$(RUN) Rscript run.R

r_shell: DOCKER_ARGS= -dit --rm -e DISPLAY=$$DISPLAY -v /tmp/.X11-unix:/tmp/.X11-unix:ro --name="rdev"
r_shell:
	$(RUN) R

docker:
	docker build $(DOCKER_ARGS) --tag $(IMAGE):$(GIT_TAG) .
	docker tag $(IMAGE):$(GIT_TAG) $(IMAGE):latest

docker-push:
	docker push $(IMAGE):$(GIT_TAG)
	docker push $(IMAGE):latest

docker-pull:
	docker pull $(IMAGE):$(GIT_TAG)
	docker tag $(IMAGE):$(GIT_TAG) $(IMAGE):latest

enter: DOCKER_ARGS=-it
enter:
	$(RUN) bash

enter-root: DOCKER_ARGS=-it
enter-root: UID=root
enter-root: GID=root
enter-root:
	$(RUN) bash
