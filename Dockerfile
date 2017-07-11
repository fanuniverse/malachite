FROM fedora:25

RUN dnf -y update && dnf install -y libicu-devel && dnf clean all

RUN mkdir /app
WORKDIR /app

COPY .stack-work/install/x86_64-linux/lts-8.20/8.0.2/bin/malachite /app

EXPOSE 8080
USER nobody
ENV PORT 8080

CMD ./malachite
