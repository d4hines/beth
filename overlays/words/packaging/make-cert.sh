#!/usr/bin/env bash
# Create a stable self-signed code-signing certificate in your login keychain.
#
# Why: an ad-hoc signature changes every build, so macOS treats each build as a
# new app and drops the Accessibility grant. Signing every build with this one
# fixed certificate gives the app a stable identity, so you grant Accessibility
# once and it sticks across rebuilds.
#
# This is a personal, self-signed dev certificate — it is NOT for distribution.
# You'll be asked for your login password once (to trust it for code signing).
set -euo pipefail

NAME="Word Counter Dev"
KEYCHAIN="$HOME/Library/Keychains/login.keychain-db"

if security find-identity -v -p codesigning 2>/dev/null | grep -q "$NAME"; then
  echo "✓ Certificate '$NAME' already exists and is valid for code signing."
  exit 0
fi

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

# Use macOS's built-in LibreSSL for consistent PKCS#12 output.
OPENSSL=/usr/bin/openssl
# macOS `security import` fails MAC verification on EMPTY-password PKCS#12 files,
# so use a throwaway passphrase for the temp .p12 (it's deleted right after).
P12PASS="word-counter-dev"

cat > "$TMP/cert.cnf" <<EOF
[req]
distinguished_name = dn
x509_extensions = v3
prompt = no
[dn]
CN = $NAME
[v3]
basicConstraints = critical,CA:false
keyUsage = critical,digitalSignature
extendedKeyUsage = critical,codeSigning
EOF

echo "==> Generating key + self-signed certificate"
"$OPENSSL" req -x509 -newkey rsa:2048 -nodes -days 3650 \
  -keyout "$TMP/key.pem" -out "$TMP/cert.pem" -config "$TMP/cert.cnf"

"$OPENSSL" pkcs12 -export -inkey "$TMP/key.pem" -in "$TMP/cert.pem" \
  -out "$TMP/id.p12" -passout "pass:$P12PASS" -name "$NAME"

echo "==> Importing into login keychain"
security import "$TMP/id.p12" -k "$KEYCHAIN" -P "$P12PASS" -T /usr/bin/codesign -A

echo "==> Trusting it for code signing (you may be prompted for your password)"
security add-trusted-cert -r trustRoot -p codeSign -k "$KEYCHAIN" "$TMP/cert.pem"

echo
echo "✓ Done. Verify with:  security find-identity -v -p codesigning"
echo "  Then rebuild:       ./packaging/build-app.sh"
