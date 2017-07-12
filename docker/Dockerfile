FROM debian:stretch-slim

RUN apt-get update \
 && apt-get install -y netbase ca-certificates libgmp10 \
 && rm -rfv /var/lib/apt/lists/*

ADD usr-lib.tar /usr/lib/x86_64-linux-gnu

COPY malachite .

USER nobody
ENV PORT 8080
EXPOSE 8080

CMD ./malachite
