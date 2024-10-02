echo "pub fn get_api_url() { \"$1\" }" > ./src/env.gleam
echo "export function get_route() { return window.location.pathname; }" > ./src/ffi.mjs
