FROM ubuntu:20.04
USER root
ENV TZ=America/NewYork
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update -qq && apt install -y -qq curl ca-certificates net-tools zip make unzip git software-properties-common jq nodejs npm zlib1g-dev libsqlite3-dev python3-pip yamllint pylint tidy clang-tidy
RUN cd /tmp && curl -L https://github.com/koalaman/shellcheck/releases/download/v0.7.2/shellcheck-v0.7.2.linux.x86_64.tar.xz | tar -xJf - && mv shellcheck-v0.7.2/shellcheck /usr/local/bin
RUN curl https://raw.githubusercontent.com/terraform-linters/tflint/master/install_linux.sh | bash
RUN curl -L https://github.com/mvdan/sh/releases/download/v3.3.0/shfmt_v3.3.0_linux_amd64 -o /usr/local/bin/shfmt && chmod +x /usr/local/bin/shfmt
RUN cpan -f install B::Lint
RUN npm config set strict-ssl false && npm install -g eslint
RUN pip3 install flake8
RUN git clone https://github.com/joesuf4/jinjalint /tmp/jinjalint && (cd /tmp/jinjalint && python3 setup.py install)
ENTRYPOINT cd /src && grep '[)]$' linter.rc | (echo; awk '{print $1}') | cut -d')' -f1 |  xargs -P$(nproc) -d'\n' -i sh -c 'git diff --name-only $(git show-branch --merge-base HEAD)~1 | LINTER={} bash linter.sh'
