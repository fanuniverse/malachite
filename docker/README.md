# Docker

This is a guide to building the Docker image locally.
You can pull the latest image from
[littlebobbytables/malachite](https://hub.docker.com/r/littlebobbytables/malachite/).

## Prerequisites

[fpco/stack-build](https://hub.docker.com/r/fpco/stack-build/) image
(get it by running `stack docker pull`).

You also need to build the executable (`stack build`) and copy it to
this directory. The subsequent commands assume that your current
working directory is the location of this file, too.

## Building the image

```
docker run --rm -it -v "$(pwd)":/base fpco/stack-build
```

Inside the container:

```
cd /base
make
exit
```

Finally:

```
docker build -t malachite .
```

(Optionally, to make sure it works):

```
docker run -p 8080:8080 -e CAMO_HOST=example.com -e CAMO_KEY=0 malachite
```
