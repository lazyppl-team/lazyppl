FROM haskell:9.6.7

# System libraries needed by hmatrix (LAPACK), the Gaussian process module,
# and the matplotlib Haskell wrapper used by the demos.
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        libblas-dev \
        liblapack-dev \
        libgsl-dev \
        libgmp-dev \
        libtinfo-dev \
        python3 \
        python3-pip \
        python3-tk \
        sudo \
        git \
        less \
        ca-certificates \
        curl \
    && rm -rf /var/lib/apt/lists/*

# Python deps for the matplotlib Haskell wrapper used by the demos.
RUN pip3 install --no-cache-dir \
        matplotlib \
        numpy \
        scipy

# Codespaces expects a non-root user with sudo.
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID
RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID --shell /bin/bash --create-home $USERNAME \
    && echo "$USERNAME ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME

USER $USERNAME
WORKDIR /home/$USERNAME

# Configure stack as the vscode user (so it writes to /home/vscode/.stack,
# not /root/.stack) to reuse the system GHC instead of installing its own.
RUN stack config set system-ghc --global true \
    && stack config set install-ghc --global false
