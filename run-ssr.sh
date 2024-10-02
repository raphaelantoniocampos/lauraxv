cd ./client
gleam run -m lustre/dev build --outdir=../server/priv/static
cd ../server
cp -r ./server/priv/static/images/ ./server/build/dev/erlang/server/priv/static/images/*

mjs="./server/priv/static/client.mjs"

export="
export function main2() { let _pipe = application(init4, update, view); return start2(_pipe, "#app", void 0);}
// build/.lustre/entry.mjs
main2();
"

head -n -7 "$mjs" > temp.txt

echo "$export" >> temp.txt

mv temp.txt "$arquivo"

DATABASE_PATH="file:db.sqlite3?mode=rw" GLEAM_ENV="development" PORT="8083" SECRET_KEY_BASE="m6cGj3W7ve7vsrfcA4mXaTZq6g3sw97mLm2JmvVkEIL77gYnL807rV9vBBbaaCAMGrRFM5JocMui0fu-Jw64pw" gleam run

