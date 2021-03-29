FROM ubuntu:20.04

# Build using `docker build` pointing to directory where Dockerfile is,
# and use `-t xfind:0.1` to specify tag in format `name:tag`
# $ docker build --squash -t xfind:0.1.0 .
# $ docker build --squash -t xfind .

ENV DEBIAN_FRONTEND=noninteractive \
    TERM=xterm-256color


# Set ubuntu version for various urls
ARG UBUNTU_VERSION=20.04


# https://www.techrepublic.com/article/how-to-create-your-own-docker-image/
## Install core stuff, C++ (cmake, boost, rapidjson) and swift dependencies (https://swift.org/getting-started/);
## time is installed for scripts/benchmark.py
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        apt-transport-https \
        binutils \
        build-essential \
        ca-certificates \
        clang \
        cmake \
        curl \
        git \
        gnupg2 \
        libboost-all-dev \
        libc6-dev \
        libcurl4 \
        libedit2 \
        libicu-dev \
        libncurses5 \
        libpython2.7-dev \
        libsqlite3-0 \
        libtinfo5 \
        libxml2 \
        libz3-dev \
        pkg-config \
        rapidjson-dev \
        software-properties-common \
        time \
        tzdata \
        unzip \
        vim \
        wget \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*


# ## Install dart stuff
# RUN sh -c 'wget -qO- https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -' \
#     && sh -c 'wget -qO- https://storage.googleapis.com/download.dartlang.org/linux/debian/dart_stable.list > /etc/apt/sources.list.d/dart_stable.list' \
#     && apt-get install -y --no-install-recommends \
#         dart
ARG DART_VERSION=2.12.2
ENV DART_PATH=/usr/local/dart-sdk
ENV PATH=$PATH:${DART_PATH}/bin
RUN wget --progress=dot:giga https://storage.googleapis.com/dart-archive/channels/stable/release/${DART_VERSION}/sdk/dartsdk-linux-x64-release.zip \
    && unzip dartsdk-linux-x64-release.zip -d /usr/local \
    && rm dartsdk-linux-x64-release.zip


## Install dotnet stuff
# (from https://docs.microsoft.com/en-us/dotnet/core/install/linux-ubuntu)
ENV DOTNET_PATH=/usr/share/dotnet
RUN wget --progress=dot:giga https://packages.microsoft.com/config/ubuntu/${UBUNTU_VERSION}/packages-microsoft-prod.deb \
    && dpkg -i packages-microsoft-prod.deb \
    && rm packages-microsoft-prod.deb \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        apt-transport-https \
    &&  apt-get update \
    && apt-get install -y --no-install-recommends \
        dotnet-sdk-5.0 \
        powershell \
    && rm -rf /var/lib/apt/lists/*


## Install go stuff
ARG GO_VERSION=1.16.2
ENV GO_PATH=/usr/local/go
ENV PATH=$PATH:${GO_PATH}/bin
RUN rm -rf /usr/local/go \
    && wget --progress=dot:giga https://dl.google.com/go/go${GO_VERSION}.linux-amd64.tar.gz \
    && tar -C /usr/local -xzf go${GO_VERSION}.linux-amd64.tar.gz \
    && rm go${GO_VERSION}.linux-amd64.tar.gz


# Install haskell stuff
# NOTE: had to install netbase too, see https://github.com/snoyberg/http-client/issues/292#issuecomment-461454932
# NOTE 2: Disabling Haskell support for now, need to work through some recent build issues
# RUN apt-get update \
#     && apt-get install -y --no-install-recommends \
#         ghc \
#         haskell-stack \
#         netbase \
#     && rm -rf /var/lib/apt/lists/* \
#     && stack upgrade --binary-only


## Install java stuff
ENV JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        openjdk-11-jdk \
        maven \
    && rm -rf /var/lib/apt/lists/*


## Install clojure stuff
RUN wget --progress=dot:giga https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -P /usr/local/bin \
    && chmod a+x /usr/local/bin/lein \
    && lein


## Install kotlin stuff
ARG KOTLIN_VERSION=1.4.31
ARG GRADLE_VERSION=6.8.3
ENV KOTLIN_PATH=/usr/local/kotlinc \
    GRADLE_PATH=/usr/local/gradle-${GRADLE_VERSION}
ENV PATH=$PATH:${KOTLIN_PATH}/bin:${GRADLE_PATH}/bin
RUN wget --progress=dot:giga https://github.com/JetBrains/kotlin/releases/download/v${KOTLIN_VERSION}/kotlin-compiler-${KOTLIN_VERSION}.zip \
    && unzip kotlin-compiler-${KOTLIN_VERSION}.zip -d /usr/local \
    && rm kotlin-compiler-${KOTLIN_VERSION}.zip \
    && wget --progress=dot:giga https://services.gradle.org/distributions/gradle-${GRADLE_VERSION}-bin.zip \
    && unzip gradle-${GRADLE_VERSION}-bin.zip -d /usr/local \
    && rm gradle-${GRADLE_VERSION}-bin.zip


## Install node stuff
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        nodejs \
        npm \
    && rm -rf /var/lib/apt/lists/*

# NOTE: I'm disabling the OCaml stuff for now. When I first built an OCaml version, after
#       some initial struggles I started to form a deep appreciation of the language. As it
#       is now, I've become pretty disillusioned with the current state of things. I've been
#       unable to build on OSX for some time ("dune" doesn't compile, etc.). I renewed my
#       hope when building the docker image, I thought this would be the chance to get the
#       Ocaml version back up and running. The problem that I'm seeing is that the Jane Street
#       version that I depended on appears to have had some substantial changes, and so I'm
#       finding it difficult to find the fixes to code that previously built without any
#       problems. I realize that I'm going to need to do another deep dive to get this version
#       updated with the latest changes, etc., but for now I'm going to step away from this
#       and focus on other TODOs and other projects.
# ## Install ocaml stuff
# RUN apt-get update \
#     && apt-get install -y --no-install-recommends \
#         m4 \
#         ocaml \
#     && rm -rf /var/lib/apt/lists/*


# ## Install opam
# ## --disable-sandboxing bypasses need for bubblewrap to be installed
# ## installing core installs corebuild
# ARG OPAM_VERSION=2.0.8
# RUN wget https://github.com/ocaml/opam/releases/download/${OPAM_VERSION}/opam-${OPAM_VERSION}-x86_64-linux -o opam \
#     && chmod u+x opam \
#     && mv opam /usr/local/bin/ \
#     && opam init -y --disable-sandboxing \
#     && eval $(opam env) \
#     && opam update \
#     && opam install -y \
#         base \
#         core \
#         ocaml-lsp-server \
#         ounit \
#         re2 \
#         utop \
#         xml-light \
#         yojson \
#     && echo '#use "topfind";;' > ~/.ocamlinit \
#     && echo '#thread;;' >> ~/.ocamlinit \
#     && echo '#require "core.top";;' >> ~/.ocamlinit


## Install php stuff
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        php-cli \
        php-curl \
        php-xml \
        php-mbstring \
    && rm -rf /var/lib/apt/lists/* \
    && php -r "copy('https://getcomposer.org/installer', 'composer-setup.php');" \
    # && hash=$(curl -sS https://composer.github.io/installer.sig) \
    # && php -r "if (hash_file('sha384', 'composer-setup.php') === $hash) { echo 'Installer verified'; } else { echo 'Installer corrupt'; unlink('composer-setup.php'); } echo PHP_EOL;" \
    && php composer-setup.php \
    && php -r "unlink('composer-setup.php');" \
    && mv /composer.phar /usr/local/bin/composer


## Install perl stuff
# This seems to be needed in order for en_US.utf-8 to be a valid locale
# from https://hub.docker.com/_/ubuntu
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        locales \
    && rm -rf /var/lib/apt/lists/* \
    && localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8
ENV LANG=en_US.utf8
ENV LANGUAGE=$LANG \
    LC_ALL=$LANG


## Install python stuff
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        python3 \
        python3-venv \
        python3-dev \
        python3-pip \
    && rm -rf /var/lib/apt/lists/*


## Install ruby stuff
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ruby-full \
    && rm -rf /var/lib/apt/lists/*


## Install rust stuff
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y \
    && echo 'source $HOME/.cargo/env' >> $HOME/.profile \
    && /bin/bash -c "source $HOME/.cargo/env"


## Install scala/sbt stuff
ARG SBT_VERSION=1.4.9
ENV SBT_PATH=/usr/local/sbt
ENV PATH=$PATH:${SBT_PATH}/bin
RUN wget --progress=dot:giga https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.zip \
    && unzip sbt-${SBT_VERSION}.zip -d /usr/local \
    && rm sbt-${SBT_VERSION}.zip


## Install swift stuff
ARG SWIFT_VERSION=5.3.3
# not sure how to dynamically derive this from UBUNTU_VERSION, but needed for swift URL
ARG UBUNTU_VERSION_ABBREV=2004
ENV SWIFT_PATH=/usr/local/swift-${SWIFT_VERSION}-RELEASE-ubuntu${UBUNTU_VERSION}
ENV PATH=$PATH:${SWIFT_PATH}/usr/bin
RUN wget --progress=dot:giga https://swift.org/builds/swift-${SWIFT_VERSION}-release/ubuntu${UBUNTU_VERSION_ABBREV}/swift-${SWIFT_VERSION}-RELEASE/swift-${SWIFT_VERSION}-RELEASE-ubuntu${UBUNTU_VERSION}.tar.gz \
    && tar -C /usr/local -xzf swift-${SWIFT_VERSION}-RELEASE-ubuntu${UBUNTU_VERSION}.tar.gz \
    && rm swift-${SWIFT_VERSION}-RELEASE-ubuntu${UBUNTU_VERSION}.tar.gz


## Set up XFIND_PATH and set as workdir
ENV XFIND_PATH=/workspaces/xfind
ENV PATH=$PATH:${XFIND_PATH}/bin
RUN mkdir -p /workspaces/xfind
WORKDIR /workspaces/xfind
