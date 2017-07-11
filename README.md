# Malachite

An image scraping service, currently supporting
DeviantART and Tumblr links.

## API

The service has a single endpoint, `GET /`, which accepts
the target link as the `url` parameter.

The service can respond with either of the following:

| Status | Description | Body |
| --- | --- | --- |
| 200 | the service was able to fetch and scrape the given URL | a JSON object containing the scraped metadata |
| 404 | the service was unable to fetch or scrape the given URL | (empty) |
| 400 | no URL was given | (empty) |

## Docker

A sample Dockerfile is provided. The size of the final image is, however, rather excessive, and a rewrite is planned.

## Development

### Requirements

* [stack](https://github.com/commercialhaskell/stack)
* `libicu-devel` (fast-tagsoup)

### Building

```
stack setup # install GHC
stack build
```

### Testing

```
stack test
```
