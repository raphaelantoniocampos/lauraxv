FROM ghcr.io/gleam-lang/gleam:v1.4.1-node-alpine

WORKDIR /build

COPY . /build


RUN apk update && \
    apk add --no-cache erlang erlang-dev rebar3

RUN mkdir -p /app

# Compile the project
RUN cd /build \
  && gleam clean \
  && gleam deps download \
  && gleam run -m lustre/dev build app --minify \
  && cp index.html /app/index.html \
  && cp -r priv /app/priv

# EXPOSE 1234

# Run the server
WORKDIR /app
# RUN echo "main()" >> /app/client/client.mjs
CMD ["node", "/app/priv/static/client.min.mjs"]
