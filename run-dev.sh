echo "pub fn get_api_url() { \"http://localhost:8080\" }" > ./client/src/env.gleam 

cd ./client \
  && gleam run -m lustre/dev build app --outdir=../server/priv/static --minify 

cd ..

cd ./server \
  && gleam run
