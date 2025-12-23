# Guile Scheme LLM Integration Toolkit Makefile
# FreeBSD and Linux compatible

.PHONY: all install install-guile-deps install-guile-json test clean help check-system

# Detect operating system
UNAME_S := $(shell uname -s)

# Platform-specific defaults
ifeq ($(UNAME_S),FreeBSD)
    # FreeBSD uses guile3 for Guile 3.x, bash is in /usr/local/bin
    SHELL := /usr/local/bin/bash
    GUILE ?= guile3
    MAKE_CMD := gmake
    PKG_INSTALL := pkg install -y
else ifeq ($(UNAME_S),Darwin)
    # macOS
    SHELL := $(shell which bash)
    GUILE ?= guile
    MAKE_CMD := make
    PKG_INSTALL := brew install
else
    # Linux and others
    SHELL := $(shell which bash)
    GUILE ?= guile
    MAKE_CMD := make
    PKG_INSTALL := apt-get install -y
endif

# Configuration
PROJECT_NAME := scheme-llm-toolkit
PROJECT_ROOT := $(shell pwd)
PREFIX ?= /usr/local
GUILE_SITE_DIR ?= $(shell $(GUILE) -c "(display (%site-dir))" 2>/dev/null || echo "/usr/local/share/guile/site/3.0")
GUILE_SITE_CCACHE_DIR ?= $(shell $(GUILE) -c "(display (%site-ccache-dir))" 2>/dev/null || echo "/usr/local/lib/guile/3.0/site-ccache")

# Project directories
SRC_DIR := src
TEST_DIR := experiments
CONFIG_DIR := config
DOCS_DIR := docs

# Source files
SCHEME_SOURCES := $(shell find $(SRC_DIR) -name "*.scm" 2>/dev/null || echo "")
TEST_SOURCES := $(shell find $(TEST_DIR) -name "*.scm" 2>/dev/null || echo "")

# Default target
all: help

# Check system configuration
check-system:
	@echo "System Detection"
	@echo "================"
	@echo "  OS:           $(UNAME_S)"
	@echo "  Shell:        $(SHELL)"
	@echo "  Guile:        $(GUILE)"
	@echo "  Guile path:   $(shell which $(GUILE) 2>/dev/null || echo 'NOT FOUND')"
	@echo "  Make:         $(MAKE_CMD)"
	@echo "  Site dir:     $(GUILE_SITE_DIR)"
	@echo ""
	@# Check Guile version
	@if command -v $(GUILE) >/dev/null 2>&1; then \
		echo "Guile version: $$($(GUILE) --version | head -1)"; \
		MAJOR=$$($(GUILE) -c "(display (major-version))"); \
		if [ "$$MAJOR" -lt 3 ]; then \
			echo "WARNING: Guile 3.x required, found $$MAJOR.x"; \
			echo "On FreeBSD: pkg install guile3"; \
		fi \
	else \
		echo "ERROR: Guile not found"; \
		echo "On FreeBSD: pkg install guile3"; \
		echo "On Debian/Ubuntu: apt install guile-3.0"; \
		echo "On macOS: brew install guile"; \
	fi

# Development environment setup
.PHONY: dev-env
dev-env:
	@echo "Starting development environment with tmux and Emacs..."
	@if tmux has-session -t $(PROJECT_NAME) 2>/dev/null; then \
		echo "Tmux session '$(PROJECT_NAME)' already exists. Attaching..."; \
		tmux attach-session -t $(PROJECT_NAME); \
	else \
		echo "Creating new tmux session '$(PROJECT_NAME)'..."; \
		tmux new-session -d -s $(PROJECT_NAME) -c $(PROJECT_ROOT) "emacs -nw -Q -l $(PROJECT_ROOT)/$(PROJECT_NAME).el"; \
		echo "Session created. TTY: $$(tmux list-panes -t $(PROJECT_NAME) -F '#{pane_tty}')"; \
		echo "To attach: tmux attach-session -t $(PROJECT_NAME)"; \
		tmux attach-session -t $(PROJECT_NAME); \
	fi

# Get tmux session TTY
.PHONY: dev-tty
dev-tty:
	@if tmux has-session -t $(PROJECT_NAME) 2>/dev/null; then \
		echo "TTY for $(PROJECT_NAME) session:"; \
		tmux list-panes -t $(PROJECT_NAME) -F "#{pane_tty}"; \
	else \
		echo "No tmux session '$(PROJECT_NAME)' found. Run '$(MAKE_CMD) dev-env' first."; \
	fi

# Help target
help:
	@echo "Guile Scheme LLM Integration Toolkit"
	@echo "===================================="
	@echo ""
	@echo "Detected: $(UNAME_S) (use '$(MAKE_CMD)' to run)"
	@echo ""
	@echo "Available targets:"
	@echo "  $(MAKE_CMD) check-system       - Show system configuration"
	@echo "  $(MAKE_CMD) dev-env            - Start tmux/Emacs development environment"
	@echo "  $(MAKE_CMD) dev-tty            - Get TTY of running dev environment"
	@echo "  $(MAKE_CMD) install-guile-json - Install guile-json (FreeBSD: from source)"
	@echo "  $(MAKE_CMD) install-guile-deps - Install Guile dependencies"
	@echo "  $(MAKE_CMD) test               - Run all tests"
	@echo "  $(MAKE_CMD) install            - Install system-wide"
	@echo "  $(MAKE_CMD) clean              - Clean build artifacts"
	@echo "  $(MAKE_CMD) help               - Show this help message"
	@echo ""
	@echo "Configuration:"
	@echo "  PREFIX=$(PREFIX)"
	@echo "  GUILE=$(GUILE)"
	@echo "  GUILE_SITE_DIR=$(GUILE_SITE_DIR)"
ifeq ($(UNAME_S),FreeBSD)
	@echo ""
	@echo "FreeBSD Quick Start:"
	@echo "  pkg install guile3 curl bash"
	@echo "  $(MAKE_CMD) install-guile-json  # Build guile-json from source"
endif

# Install guile-json from source (required on FreeBSD)
install-guile-json:
ifeq ($(UNAME_S),FreeBSD)
	@echo "Installing guile-json from source..."
	@./scripts/install-guile-json-freebsd.sh
else
	@echo "On $(UNAME_S), use your package manager:"
	@echo "  Debian/Ubuntu: apt install guile-json"
	@echo "  macOS: brew install guile-json"
	@echo "  Guix: guix install guile-json"
endif

# Install Guile dependencies
install-guile-deps:
	@echo "Installing Guile dependencies..."
	@# Check for Guile
	@if ! command -v $(GUILE) >/dev/null 2>&1; then \
		echo "Error: $(GUILE) not found."; \
		if [ "$(UNAME_S)" = "FreeBSD" ]; then \
			echo "Install with: pkg install guile3"; \
		elif [ "$(UNAME_S)" = "Darwin" ]; then \
			echo "Install with: brew install guile"; \
		else \
			echo "Install with: apt install guile-3.0"; \
		fi; \
		exit 1; \
	fi
	@echo "Guile found: $(shell $(GUILE) --version | head -n1)"
	@# Install guile-json if not present
	@if ! $(GUILE) -c "(use-modules (json))" 2>/dev/null; then \
		echo "guile-json not found. Installing..."; \
		if [ "$(UNAME_S)" = "FreeBSD" ]; then \
			echo "Run: pkg install guile3-json"; \
		elif command -v guix >/dev/null 2>&1; then \
			guix install guile-json; \
		elif command -v apt-get >/dev/null 2>&1; then \
			sudo apt-get install -y guile-json; \
		elif command -v brew >/dev/null 2>&1; then \
			brew install guile-json; \
		else \
			echo "Please install guile-json manually for your system."; \
		fi \
	else \
		echo "guile-json is already installed."; \
	fi
	@# Check for curl/wget
	@echo "Checking for curl/wget..."
	@if ! command -v curl >/dev/null 2>&1 && ! command -v wget >/dev/null 2>&1; then \
		echo "Warning: Neither curl nor wget found."; \
		if [ "$(UNAME_S)" = "FreeBSD" ]; then \
			echo "Run: pkg install curl"; \
		fi \
	else \
		echo "HTTP client found."; \
	fi
	@echo "Dependencies check complete."

# Run tests
test:
	@echo "Running tests..."
	@echo "Using: $(GUILE)"
	@# First check dependencies
	@echo "Checking dependencies..."
	@if [ -f $(TEST_DIR)/000-deps-check/check.scm ]; then \
		$(GUILE) $(TEST_DIR)/000-deps-check/check.scm || exit 1; \
	fi
	@# Run JSON tests
	@echo ""
	@echo "Running JSON parser tests..."
	@if [ -f $(TEST_DIR)/001-json-test/test-json.scm ]; then \
		$(GUILE) -L $(SRC_DIR) $(TEST_DIR)/001-json-test/test-json.scm || exit 1; \
	fi
	@# Run Ollama tests if available
	@echo ""
	@echo "Running Ollama integration tests..."
	@if [ -f $(TEST_DIR)/002-ollama-test/check-ollama.scm ]; then \
		if $(GUILE) $(TEST_DIR)/002-ollama-test/check-ollama.scm 2>/dev/null; then \
			$(GUILE) -L $(SRC_DIR) $(TEST_DIR)/002-ollama-test/test-ollama.scm || exit 1; \
		else \
			echo "Skipping Ollama tests (Ollama not running)"; \
		fi \
	fi
	@# Run provider tests
	@echo ""
	@echo "Running provider abstraction tests..."
	@if [ -f $(TEST_DIR)/003-provider-test/test-provider.scm ]; then \
		$(GUILE) -L $(SRC_DIR) $(TEST_DIR)/003-provider-test/test-provider.scm || exit 1; \
	fi
	@# Run contract tests
	@echo ""
	@echo "Running contract tests..."
	@if [ -f $(TEST_DIR)/008-contracts/test-contracts.scm ]; then \
		$(GUILE) -L $(SRC_DIR) $(TEST_DIR)/008-contracts/test-contracts.scm || exit 1; \
	fi
	@echo ""
	@echo "All tests passed!"

# Install system-wide
install: install-guile-deps
	@echo "Installing Scheme LLM Toolkit..."
	@# Create directories
	@mkdir -p $(DESTDIR)$(GUILE_SITE_DIR)/llm
	@# Copy source files
	@if [ -n "$(SCHEME_SOURCES)" ]; then \
		echo "Installing Scheme modules..."; \
		for dir in core providers utils; do \
			if [ -d $(SRC_DIR)/llm/$$dir ]; then \
				mkdir -p $(DESTDIR)$(GUILE_SITE_DIR)/llm/$$dir; \
				cp -v $(SRC_DIR)/llm/$$dir/*.scm $(DESTDIR)$(GUILE_SITE_DIR)/llm/$$dir/ 2>/dev/null || true; \
			fi \
		done \
	else \
		echo "Warning: No source files found to install."; \
	fi
	@# Create config directory in user home
	@echo "Setting up configuration..."
	@mkdir -p ~/.config/scheme-llm-toolkit
	@if [ ! -f ~/.config/scheme-llm-toolkit/providers.scm ] && [ -f $(CONFIG_DIR)/providers.example.scm ]; then \
		cp $(CONFIG_DIR)/providers.example.scm ~/.config/scheme-llm-toolkit/providers.scm; \
		echo "Created example configuration at ~/.config/scheme-llm-toolkit/providers.scm"; \
		echo "Please edit this file with your API keys."; \
	fi
	@echo "Installation complete!"
	@echo ""
	@echo "To use the toolkit, add the following to your Guile code:"
	@echo "  (use-modules (llm core provider) (llm providers ollama))"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@find . -name "*.go" -delete 2>/dev/null || true
	@find . -name "*~" -delete 2>/dev/null || true
	@echo "Clean complete."

.DEFAULT_GOAL := help
