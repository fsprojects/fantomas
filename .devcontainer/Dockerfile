FROM mcr.microsoft.com/dotnet/sdk:8.0.100-rc.2

# Avoid warnings by switching to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# Configure apt and install packages
RUN apt-get update \
    && apt-get -y install --no-install-recommends apt-utils dialog 2>&1 \
    # Verify bash, git, process tools, lsb-release (common in install instructions for CLIs) installed
    && apt-get -y install bash git openssh-client less iproute2 procps lsb-release \
    && apt-get -y install gnupg \
    # Clean up
    && apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/*

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialog