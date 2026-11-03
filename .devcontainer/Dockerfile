FROM haskell:9.14.1-bookworm

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        python3-matplotlib python3-numpy python3-scipy python-is-python3 sudo git less ca-certificates curl xz-utils \
    && rm -rf /var/lib/apt/lists/*

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID
RUN if id -u $USER_UID >/dev/null 2>&1; then \
        existing=$(id -un $USER_UID); \
        usermod -l $USERNAME -d /home/$USERNAME -m $existing && \
        groupmod -n $USERNAME $(id -gn $USER_UID); \
    else \
        groupadd --gid $USER_GID $USERNAME && \
        useradd --uid $USER_UID --gid $USER_GID --shell /bin/bash --create-home $USERNAME; \
    fi \
    && echo "$USERNAME ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME \
    && mkdir -p /workspaces \
    && chown $USER_UID:$USER_GID /workspaces


# GHCup 0.2.6.2 is the newest release (github.com/haskell/ghcup-hs/releases, 31 Oct 2026). It runs as root, so the download is
# checked before it runs. The sha256 values are the ones in the release's SHA256SUMS, whose signature was verified with gpg
# on 31 Oct 2026: signed by Julian Ospald, primary key 7D1E8AFD1D4A16D71FADA2F2CCC85C0E40C06A8C, one of the keys listed on
# https://www.haskell.org/ghcup/install/manual/. Take new values from a signature-verified SHA256SUMS when the version changes.
RUN machine=$(uname -m) \
    && case "$machine" in \
        x86_64) sha256=9ed5da5449b48043a0d17e767c05d2ef585e25a639bb934329496c6d2fad9cf8 ;; \
        aarch64) sha256=65a5f05120288ee4f1a81d28825374b6af317456a351a586adfce90c6dc29e3b ;; \
        *) echo "no GHCup checksum for $machine" >&2; exit 1 ;; \
    esac \
    && curl -fL https://downloads.haskell.org/~ghcup/0.2.6.2/$machine-linux-ghcup-0.2.6.2 -o /tmp/ghcup \
    && echo "$sha256  /tmp/ghcup" | sha256sum -c - \
    && chmod +x /tmp/ghcup \
    && /tmp/ghcup install cabal 3.18.2.0 \
    && install /root/.ghcup/bin/cabal-3.18.2.0 /usr/local/bin/cabal \
    && rm /tmp/ghcup

ENV PATH=/usr/local/bin:$PATH
# Login shells reset PATH from /etc/profile, which would drop the image's GHC; keep the image's PATH for them too.
RUN echo "export PATH=\"$PATH\"" > /etc/profile.d/lazyppl-path.sh

COPY .devcontainer/banner.sh /etc/lazyppl-banner.sh
RUN echo '. /etc/lazyppl-banner.sh' >> /etc/bash.bashrc
USER $USERNAME
WORKDIR /home/$USERNAME
RUN cabal update && mkdir -p /tmp/lazyppl-deps
COPY --chown=vscode:vscode lazyppl.cabal cabal.project LICENSE README.md ChangeLog.md /tmp/lazyppl-deps/
COPY --chown=vscode:vscode singlesite/lazyppl-singlesite.cabal singlesite/LICENSE singlesite/ChangeLog.md /tmp/lazyppl-deps/singlesite/
COPY --chown=vscode:vscode examples/lazyppl-examples.cabal examples/LICENSE /tmp/lazyppl-deps/examples/
RUN mkdir -p /tmp/lazyppl-deps/src /tmp/lazyppl-deps/test /tmp/lazyppl-deps/singlesite/src /tmp/lazyppl-deps/singlesite/test \
        /tmp/lazyppl-deps/examples/models /tmp/lazyppl-deps/examples/native /tmp/lazyppl-deps/examples/test \
    && cd /tmp/lazyppl-deps && cabal build all --only-dependencies -j
