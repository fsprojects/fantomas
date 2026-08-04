FROM mcr.microsoft.com/dotnet/sdk:10.0.103

# Avoid warnings by switching to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# This Dockerfile adds a non-root user with sudo access. Use the "remoteUser"
# property in devcontainer.json to use it. On Linux, the container user's GID/UIDs
# will be updated to match your local UID/GID (when using the dockerFile property).
# See https://aka.ms/vscode-remote/containers/non-root-user for details.
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Configure apt and install packages
RUN apt-get update \
    && apt-get -y install --no-install-recommends apt-utils dialog 2>&1 \
    #
    # Verify git, process tools, lsb-release (common in install instructions for CLIs) installed
    && apt-get -y install git iproute2 procps lsb-release \
    #
    # Create a non-root user to use if preferred - see https://aka.ms/vscode-remote/containers/non-root-user.
    # Create group if it doesn't exist (handle case where GID already exists)
    && if ! getent group $USERNAME > /dev/null 2>&1; then \
        groupadd --gid $USER_GID $USERNAME 2>/dev/null || \
        (EXISTING_GROUP=$(getent group $USER_GID | cut -d: -f1) && groupmod -n $USERNAME $EXISTING_GROUP); \
    fi \
    # Create user if it doesn't exist (handle case where UID already exists)
    && if ! getent passwd $USERNAME > /dev/null 2>&1; then \
        useradd -s /bin/bash --uid $USER_UID --gid $USER_GID -m $USERNAME 2>/dev/null || \
        (EXISTING_USER=$(getent passwd $USER_UID | cut -d: -f1) && usermod -l $USERNAME -d /home/$USERNAME -m $EXISTING_USER); \
    fi \
    # [Optional] Add sudo support for the non-root user
    && apt-get install -y sudo \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME\
    && chmod 0440 /etc/sudoers.d/$USERNAME \
    #
    # Clean up
    && apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/*

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialog