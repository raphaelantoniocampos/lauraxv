# mv ./db/db.sqlite3 ./db/db.sqlite3.temp
# fly sftp get /mnt/sqlite3/db.sqlite3 ./db/db.sqlite3

echo "pub fn get_api_url() { \"http://localhost:8080\" }" > ./client/src/env.gleam 

cd ./client \
  && gleam run -m lustre/dev build app --outdir=../server/priv/static --minify 

cp priv/static/mansory.css ../server/priv/static/

cd ..
rm -r ./server/build/dev/erlang/server/priv
mv ./server/priv ./server/build/dev/erlang/server/

mkdir ./server/build/dev/erlang/server/priv/static/images
cp -r ./client/priv/static/images/* ./server/build/dev/erlang/server/priv/static/images/


cd ./server \
  && gleam run

cd ..
