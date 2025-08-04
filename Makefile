# Guile Scheme LLM Integration Toolkit Makefile
SHELL := /bin/bash
.PHONY: all install install-guile-deps test clean help

# Configuration
PREFIX ?= /usr/local
GUILE ?= guile3
GUILE_SITE_DIR ?= $(shell $(GUILE) -c "(display (%site-dir))")
GUILE_SITE_CCACHE_DIR ?= $(shell $(GUILE) -c "(display (%site-ccache-dir))")

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

# Help target
help:
	@echo "Guile Scheme LLM Integration Toolkit"
	@echo "===================================="
	@echo ""
	@echo "Available targets:"
	@echo "  make install-guile-deps  - Install Guile dependencies"
	@echo "  make test               - Run all tests"
	@echo "  make install            - Install system-wide"
	@echo "  make clean              - Clean build artifacts"
	@echo "  make help               - Show this help message"
	@echo ""
	@echo "Configuration:"
	@echo "  PREFIX=$(PREFIX)"
	@echo "  GUILE_SITE_DIR=$(GUILE_SITE_DIR)"

# Install Guile dependencies
install-guile-deps:
	@echo "Installing Guile dependencies..."
	@# Check for Guile
	@if ! command -v $(GUILE) >/dev/null 2>&1; then \
		echo "Error: Guile not found. Please install Guile 3.0 or later."; \
		exit 1; \
	fi
	@echo "Guile found: $(shell $(GUILE) --version | head -n1)"
	@# Install guile-json if not present
	@if ! $(GUILE) -c "(use-modules (json))" 2>/dev/null; then \
		echo "Installing guile-json..."; \
		if command -v guix >/dev/null 2>&1; then \
			guix install guile-json; \
		elif command -v apt-get >/dev/null 2>&1; then \
			sudo apt-get install -y guile-json; \
		elif command -v brew >/dev/null 2>&1; then \
			brew install guile-json; \
		else \
			echo "Warning: Could not install guile-json automatically."; \
			echo "Please install it manually for your system."; \
		fi \
	else \
		echo "guile-json is already installed."; \
	fi
	@# Check for other potential dependencies
	@echo "Checking for curl/wget for HTTP requests..."
	@if ! command -v curl >/dev/null 2>&1 && ! command -v wget >/dev/null 2>&1; then \
		echo "Warning: Neither curl nor wget found. HTTP functionality may be limited."; \
	fi
	@echo "Dependencies check complete."

# Run tests
test:
	@echo "Running tests..."
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
	@echo "  (use-modules (llm core) (llm providers ollama))"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@find . -name "*.go" -delete 2>/dev/null || true
	@find . -name "*~" -delete 2>/dev/null || true
	@echo "Clean complete."

.DEFAULT_GOAL := help