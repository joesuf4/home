FROM ubuntu:latest
USER root
ENV TZ=America/NewYork
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update -qq && apt install -y -qq locales curl ca-certificates net-tools zip make unzip git software-properties-common jq zlib1g-dev libsqlite3-dev python3-pip yamllint pylint tidy clang-tidy apache2-dev libapr1-dev libaprutil1-dev libapache2-mod-perl2 libapache2-mod-apreq2 libapache2-request-perl
RUN cd /tmp && curl -L https://github.com/koalaman/shellcheck/releases/download/v0.8.0/shellcheck-v0.8.0.linux.x86_64.tar.xz | tar -xJf - && mv shellcheck-v0.8.0/shellcheck /usr/local/bin
RUN curl https://raw.githubusercontent.com/terraform-linters/tflint/master/install_linux.sh | bash
RUN curl -L https://github.com/mvdan/sh/releases/download/v3.5.1/shfmt_v3.5.1_linux_amd64 -o /usr/local/bin/shfmt && chmod +x /usr/local/bin/shfmt
RUN pip3 install flake8
RUN git clone https://github.com/joesuf4/jinjalint /tmp/jinjalint && (cd /tmp/jinjalint && python3 setup.py install)
RUN git clone https://github.com/asdf-vm/asdf.git /root/.asdf
ENV PATH="/root/bin:$PATH:/root/.dotnet/tools"
RUN bash -c '. ~/.asdf/asdf.sh; mkdir -p /root/bin; echo -e "#!/bin/bash\nexec /usr/bin/curl -k \$@" > /root/bin/curl; chmod +x /root/bin/curl; for pkg in dotnet-core golangci-lint nodejs; do asdf plugin-add $pkg; v="$(asdf list-all $pkg | tail -n 1)"; asdf install $pkg $v; echo $pkg $v >>~/.tool-versions; done'
RUN bash -c '. ~/.asdf/asdf.sh; . ~/.asdf/plugins/dotnet-core/set-dotnet-home.bash; dotnet tool install -g dotnet-format --version "7.0.*" --add-source https://pkgs.dev.azure.com/dnceng/public/_packaging/dotnet-tools/nuget/v3/index.json'
RUN bash -c '. ~/.asdf/asdf.sh; npm config set strict-ssl false && npm install -g eslint typescript navigator jsdom jquery && npm install -g --save-dev @typescript-eslint/parser @typescript-eslint/eslint-plugin'
RUN cpan -f install sealed Algorithm::Diff LCS::BV LCS::XS B::Lint IO::Select URI Term::ReadKey
ENV NODE_PATH="./node_modules:/root/.asdf/installs/nodejs/$(awk '$1 == "nodejs" {print $2}' /root/.tool-versions)/lib/node_modules"
ENV USER=root
ENV TERM=xterm
ENV LANG=en_US.UTF-8
ENTRYPOINT bash -c "cd /src && grep '[)]\$' linter.rc | (echo; awk '{print \$1}') | cut -d')' -f1 |  xargs -P$(nproc) -d'\n' -i bash -c '. ~/.asdf/asdf.sh; git diff --name-only $(git show-branch --merge-base HEAD)~1 | LINTER={} bash linter.sh'"
RUN git config --global --add safe.directory /src
RUN localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8
