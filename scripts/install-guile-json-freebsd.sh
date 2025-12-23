#!/bin/sh
# install-guile-json-freebsd.sh - Install guile-json on FreeBSD
#
# guile-json is not available as a FreeBSD package, so we build from source.

set -e

GUILE_JSON_VERSION="4.7.3"
GUILE_JSON_URL="https://download.savannah.gnu.org/releases/guile-json/guile-json-${GUILE_JSON_VERSION}.tar.gz"

echo "Installing guile-json ${GUILE_JSON_VERSION} on FreeBSD..."
echo ""

# Check for required tools
for cmd in guile3 curl tar gmake; do
    if ! command -v "$cmd" >/dev/null 2>&1; then
        echo "Error: $cmd not found. Install with: pkg install $cmd"
        exit 1
    fi
done

# Create temp directory
TMPDIR=$(mktemp -d)
cd "$TMPDIR"

echo "Downloading guile-json..."
curl -L -o guile-json.tar.gz "$GUILE_JSON_URL"

echo "Extracting..."
tar xzf guile-json.tar.gz
cd "guile-json-${GUILE_JSON_VERSION}"

echo "Configuring..."
# FreeBSD needs explicit paths
./configure \
    --prefix=/usr/local \
    GUILE=/usr/local/bin/guile3 \
    PKG_CONFIG_PATH=/usr/local/libdata/pkgconfig

echo "Building..."
gmake

echo "Installing (requires root)..."
sudo gmake install

# Clean up
cd /
rm -rf "$TMPDIR"

echo ""
echo "Testing installation..."
if guile3 -c "(use-modules (json)) (display 'ok)" 2>/dev/null; then
    echo "SUCCESS: guile-json installed correctly!"
else
    echo "WARNING: Installation completed but module not found."
    echo "You may need to set GUILE_LOAD_PATH:"
    echo "  export GUILE_LOAD_PATH=/usr/local/share/guile/site/3.0"
fi
