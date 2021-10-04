# for details, see https://blackstone.jira.com/wiki/spaces/ENG/pages/2509178295/Code+Linting+in+GitLab+Repos
FROM artifactory.blackstone.com/docker/jenkins/inbound-agent:4.7-1-jdk11
USER root
ENV TZ=America/NewYork
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update -qq && apt install -y -qq curl ca-certificates net-tools zip make unzip git software-properties-common jq nodejs npm zlib1g-dev libsqlite3-dev python3-pip yamllint pylint tidy
RUN cd /tmp && curl -L https://github.com/koalaman/shellcheck/releases/download/v0.7.2/shellcheck-v0.7.2.linux.x86_64.tar.xz | tar -xJf - && mv shellcheck-v0.7.2/shellcheck /usr/local/bin
RUN curl https://artifactory.blackstone.com/artifactory/misc/platform/certs/blackstone-certs.zip -o /tmp/JenkinsBuildCerts.zip && \
    unzip /tmp/JenkinsBuildCerts.zip -d /usr/local/share/ca-certificates && update-ca-certificates
RUN curl https://raw.githubusercontent.com/terraform-linters/tflint/master/install_linux.sh | bash
RUN bash -c 'cp <(curl -L https://github.com/mvdan/sh/releases/download/v3.3.0/shfmt_v3.3.0_linux_amd64) /usr/local/bin/shfmt && chmod +x /usr/local/bin/shfmt'
ENV NODE_VERSION=12.6.0
ENV PATH="/root/.nvm/versions/node/v${NODE_VERSION}/bin/:${PATH}"
# Sadly this by can't be forced in to the one step, as the CURL has to happen then the ENV set then the rest of the install or it throws some wacky errors :(
RUN curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.34.0/install.sh | bash
ENV NVM_DIR=/root/.nvm
RUN . "$NVM_DIR/nvm.sh" && nvm install ${NODE_VERSION} && \
    . "$NVM_DIR/nvm.sh" && nvm use v${NODE_VERSION} && \
    . "$NVM_DIR/nvm.sh" && nvm alias default v${NODE_VERSION}
RUN npm config set strict-ssl false && npm install -g eslint
RUN pip3 install flake8
RUN git clone https://github.com/joesuf4/jinjalint /tmp/jinjalint && (cd /tmp/jinjalint && python3 setup.py install)
ENTRYPOINT cd /src && grep '[)]$' linter.rc | (echo; awk '{print $1}') | cut -d')' -f1 |  xargs -P$(nproc) -d'\n' -i sh -c 'git diff --name-only $(git show-branch --merge-base HEAD)~1 | LINTER={} bash linter.sh'
