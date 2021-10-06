FROM ubuntu:latest
USER root
ENV TZ=America/NewYork
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update -qq && apt install -y -qq curl ca-certificates net-tools zip make unzip git software-properties-common jq nodejs npm zlib1g-dev libsqlite3-dev python3-pip yamllint pylint tidy clang-tidy
RUN cd /tmp && curl -L https://github.com/koalaman/shellcheck/releases/download/v0.7.2/shellcheck-v0.7.2.linux.x86_64.tar.xz | tar -xJf - && mv shellcheck-v0.7.2/shellcheck /usr/local/bin
RUN curl https://raw.githubusercontent.com/terraform-linters/tflint/master/install_linux.sh | bash
RUN curl -L https://github.com/mvdan/sh/releases/download/v3.3.0/shfmt_v3.3.0_linux_amd64 -o /usr/local/bin/shfmt && chmod +x /usr/local/bin/shfmt
RUN npm config set strict-ssl false && npm install -g eslint
RUN pip3 install flake8
RUN git clone https://github.com/joesuf4/jinjalint /tmp/jinjalint && (cd /tmp/jinjalint && python3 setup.py install)
RUN git clone https://github.com/asdf-vm/asdf.git /root/.asdf
ENV PATH="$PATH:/root/.dotnet/tools"
RUN bash -c '. ~/.asdf/asdf.sh; for pkg in dotnet-core golangci-lint; do asdf plugin-add $pkg; v="$(asdf list-all $pkg | tail -n 1)"; asdf install $pkg $v; echo $pkg $v >>~/.tool-versions; done'
RUN bash -c '. ~/.asdf/asdf.sh; . ~/.asdf/plugins/dotnet-core/set-dotnet-home.bash; dotnet tool install -g dotnet-format --version "7.0.*" --add-source https://pkgs.dev.azure.com/dnceng/public/_packaging/dotnet-tools/nuget/v3/index.json || :'
RUN cpan -f install B::Lint
ENTRYPOINT cd /src && grep '[)]$' linter.rc | (echo; awk '{print $1}') | cut -d')' -f1 |  xargs -P$(nproc) -d'\n' -i sh -c 'git diff --name-only $(git show-branch --merge-base HEAD)~1 | LINTER={} bash linter.sh'
