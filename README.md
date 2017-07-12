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

Pull from [littlebobbytables/malachite](https://hub.docker.com/r/littlebobbytables/malachite/).

## Development

### Requirements

* Docker
* [stack](https://github.com/commercialhaskell/stack)

### Building

```
stack docker pull
stack build
```

### Testing

```
stack test
```
