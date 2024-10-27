# mv ./db/db.sqlite3 ./db/db.sqlite3.temp
# fly sftp get /mnt/sqlite3/db.sqlite3 ./db/db.sqlite3

echo "pub fn get_api_url() { \"http://localhost:8080\" }" > ./client/src/env.gleam 

cd ./client \
  && gleam run -m lustre/dev build app --outdir=../server/priv/static --minify 

cd ..
rm -r ./server/build/dev/erlang/server/priv
mv ./server/priv ./server/build/dev/erlang/server/

mkdir ./server/build/dev/erlang/server/priv/static/images
cp -r ./client/priv/static/images/* ./server/build/dev/erlang/server/priv/static/images/
mkdir ./server/build/dev/erlang/server/priv/static/stylesheets
cp -r ./client/src/stylesheets/* ./server/build/dev/erlang/server/priv/static/stylesheets/


cd ./server \
  && gleam run

cd ..
