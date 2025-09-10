# Makefile for update and build Emacs, and sync/rebuild Doom

# Make Debug
# MAKEFLAGS += --debug
# Recipe Debug
# .SHELLFLAGS = -e -c -x

# Directories
CURRENT_PATH := $(lastword $(MAKEFILE_LIST))
EMACS_GIT_REPO := https://github.com/emacs-mirror/emacs
EMACS_SRC_DIR ?= $(shell find $(HOME)/github -maxdepth 2 -type d -name emacs)
EMACS_BRANCH ?= master
ARCH=$(shell uname -a | awk '{print $$1}')
EMACS_BUILD_DIR ?= $(abspath $(HOME)/.local)
DOOM_CONFIG_DIR ?= $(abspath $(HOME)/.config/doom)
DOOM_EMACS_DIR ?= $(abspath $(HOME)/.config/emacs)
DOOM_BIN_PATH ?= $(abspath $(DOOM_EMACS_DIR)/bin/doom)
DOOM_EMACS_BUILD_PATH ?= $(abspath $(DOOM_EMACS_DIR)/build)

# Commands
CLANG := $(shell command -v clang)
GMAKE := $(shell command -v gmake)
MAKE := $(shell command -v make)
BASH := $(shell command -v bash)
BEAR := $(shell command -v bear)
HOSTNAME := $(shell hostname)

# Compiler and options
export SHELL := $(BASH)
export MAKE := $(if $(GMAKE),$(GMAKE),$(MAKE))
export BEAR := $(BEAR)
export CC := $(if $(filter ubuntu2204,$(HOSTNAME)),gcc-12,$(CLANG))
export CFLAGS := $(if $(filter arch-nuc,$(HOSTNAME)), -ggdb3 -Og, -O3)
export LDFLAGS := $(if $(filter arch-nuc,$(HOSTNAME)), -ggdb3, )

# Linux add -lgccjit
LDFLAGS += $(if $(filter Linux,$(ARCH)), -lgccjit,)

# Emacs configuration options
EMACS_BUILD_OPTIONS := \
	--with-native-compilation \
	--without-compress-install \
	--disable-silent-rules \
	--with-mailutils \
	--with-sound=yes \
	--with-pdumper=yes \
	--with-gpm \
	--with-gnutls=ifavailable \
	--with-zlib \
	--with-modules \
	--with-threads \
	--with-file-notification=yes \
	--with-libgmp \
	--with-included-regex \
	--with-tree-sitter=ifavailable \
	--with-mps

CFLAGS += -I$(HOME)/.local/include
LDFLAGS += -L$(HOME)/.local/lib -lmps

EMACS_BUILD_OPTIONS += \
	--with-xpm \
	--with-jpeg \
	--with-tiff=ifavailable \
	--with-gif \
	--with-png \
	--with-rsvg \
	--with-lcms2 \
	--with-cairo \
	--with-xml2 \
	--with-xft \
	--with-harfbuzz \
	--with-libotf \
	--with-xaw3d

EMACS_BUILD_OPTIONS += $(if $(filter Linux,$(ARCH)),--with-dbus \
    --with-x-toolkit=no \
    --with-gconf \
    --with-gsettings)

ifneq (,$(filter arch-nuc,$(HOSTNAME)))
EMACS_BUILD_OPTIONS += --with-pgtk
endif

EMACS_BUILD_OPTIONS += $(if $(filter Darwin,$(ARCH)),--with-ns)

all: check_repo_update emacs

# Update source repository
define update_emacs_repo
	$(shell echo -e "GET https://github.com HTTP/1.0\n\n" | nc github.com 80 > /dev/null 2>&1 || exit 1)
	$(eval _init_commit_id:=$(shell git -C $(EMACS_SRC_DIR) checkout $(EMACS_BRANCH) && git -C $(EMACS_SRC_DIR) rev-parse HEAD))
	$(eval _update_commit_id:=$(shell cd $(EMACS_SRC_DIR) && git fetch origin -q && git rev-parse origin/$(EMACS_BRANCH)))
	$(if $(filter $(_init_commit_id),$(_update_commit_id)),\
		,$(eval $(shell git -C $(EMACS_SRC_DIR) rebase origin/$(EMACS_BRANCH) -q))$(_update_commit_id))
endef

# Trigger but maybe not update emacs_updaated
check_repo_update:
	$(eval _src_update=$(strip $(update_emacs_repo)))
	$(eval $(if $(_src_update),$(shell echo $$(date "+%Y-%m-%d %H:%M:%S") emacs repo update [$(_src_update)] on branch [$(EMACS_BRANCH)]>> $(DOOM_EMACS_BUILD_PATH)/emacs_updated),))

.PHONY: check_repo_update
$(DOOM_EMACS_DIR)/emacs_updated: check_repo_update

EMACS_KEY_FILES:= \
	$(EMACS_SRC_DIR)/compile_commands.json \
	$(EMACS_BIN_PATH)
$(EMACS_KEY_FILES): $(DOOM_EMACS_DIR)/emacs_updated
	cd $(EMACS_SRC_DIR) && ./autogen.sh
	cd $(EMACS_SRC_DIR) && ./configure $(EMACS_BUILD_OPTIONS) --prefix=$(EMACS_BUILD_DIR)
	$(BEAR) --output $(EMACS_SRC_DIR)/compile_commands.json -- $(MAKE) -C $(EMACS_SRC_DIR) -j$(shell nproc)
#	$(MAKE) install -C $(EMACS_SRC_DIR)

# ifneq (,$(filter Darwin,$(ARCH)))

# 	ln -sf  $(EMACS_SRC_DIR)/nextstep/Emacs.app $(HOME)/Applications

# else ifneq (,$(filter Linux,$(ARCH)))

# 	if [ ! -f $(HOME)/.config/systemd/user/emacs.service ]; then \
# 		ln -s $(HOME)/.local/lib/systemd/user/emacs.service -T $(HOME)/.config/systemd/user/emacs.service; \
# 	fi

# endif

emacs: $(EMACS_KEY_FILES)


clean-emacs:
	$(foreach file,$(EMACS_KEY_FILES), $(shell rm -rf $(file)))


# Build doom if only build emacs
DOOM_EXISTS := $(shell [ -d $(DOOM_EMACS_DIR) ] && [ -d $(DOOM_CONFIG_DIR) ] && [ -e $(EMACS_BIN_PATH) ] && echo "yes")

ifeq ($(DOOM_EXISTS),yes)
EMACS_BIN_PATH := $(shell which emacs)
EMACS_BIN_PATH ?= $(if $(filter Darwin,$(ARCH)),$(shell readlink -f $(HOME)/Applications/Emacs.app/Contents/MacOS/Emacs),$(EMACS_BUILD_DIR)/bin/emacs)
DOOM_BUILD_FILE := $(DOOM_EMACS_BUILD_PATH)/doom_rebuild
$(DOOM_BUILD_FILE): $(EMACS_KEY_FILES)
	env EMACS=$(EMACS_BIN_PATH) EMACSDIR=$(DOOM_EMACS_DIR) DOOMDIR=$(DOOM_CONFIG_DIR) $(DOOM_BIN_PATH) sync --rebuild
	@echo $$(date "+%Y-%m-%d %H:%M:%S") doom rebuild >> $@

doom: $(DOOM_BUILD_FILE)
endif # DOOM_EXITS

# TODO: save log to /tmp for review
