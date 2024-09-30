ARG GLEAM_VERSION=v1.5.1

FROM ghcr.io/gleam-lang/gleam:${GLEAM_VERSION}-erlang-alpine AS builder

WORKDIR /build

COPY ./client /build/client
COPY ./server /build/server
COPY ./common /build/common

RUN mkdir -p /build/server/priv/static/images

COPY ./client/priv/static/images /build/server/priv/static/images

RUN apk update

RUN apk add libbsd-dev build-base inotify-tools

# Compile frontend
RUN cd /build/client \
  && gleam clean \
  && gleam deps download \
  && gleam run -m lustre/dev build --outdir=build/server/priv/static --minify

# Compile the project
RUN cd /build/server \
  && gleam export erlang-shipment \
  && mv build/erlang-shipment /app \
  && rm -r /build

# Run the server
WORKDIR /app
ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["run"]
