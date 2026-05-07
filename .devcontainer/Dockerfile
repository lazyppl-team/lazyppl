FROM haskell:9.6.7-bullseye

# System libraries: LAPACK/GSL for hmatrix (Gaussian process), Python+matplotlib
# for the plotting demos.
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        libblas-dev \
        liblapack-dev \
        libgsl-dev \
        libgmp-dev \
        libtinfo-dev \
        python3 \
        python3-pip \
        sudo \
        git \
        less \
        ca-certificates \
        curl \
    && rm -rf /var/lib/apt/lists/*

RUN pip3 install --no-cache-dir matplotlib numpy scipy

# Non-root user. If UID 1000 is already taken by a future base-image change,
# rename that user instead of failing.
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

USER $USERNAME
WORKDIR /home/$USERNAME

# Tell stack to use the system GHC rather than installing its own.
RUN stack config set system-ghc --global true \
    && stack config set install-ghc --global false

# Pre-compile lazyppl at the SAME path Codespaces will bind-mount the user's
# source (/workspaces/lazyppl). This makes the absolute paths baked into
# Paths_lazyppl.hs identical at image-build time and at codespace runtime, so
# the postCreate cp of .stack-work hits stack's cache cleanly with no relink.
RUN git clone --depth 1 -b master https://github.com/lazyppl-team/lazyppl /workspaces/lazyppl \
    && cd /workspaces/lazyppl \
    && for attempt in 1 2 3 4 5; do \
         if stack build; then break; fi; \
         echo "stack build attempt $attempt failed, retrying in 30s..."; \
         sleep 30; \
       done \
    && [ -d .stack-work ] \
    && mv /workspaces/lazyppl/.stack-work /home/$USERNAME/lazyppl-stack-work-cache

# Print a welcome hint on each new interactive shell.
COPY --chown=$USER_UID:$USER_GID .devcontainer/banner.sh /home/$USERNAME/.lazyppl-banner.sh
RUN echo '. ~/.lazyppl-banner.sh' >> /home/$USERNAME/.bashrc
