cd ./client
echo "pub fn get_api_url() { \"http://localhost:8083\" }" > ./src/env.gleam
gleam run -m lustre/dev build --outdir=../server/priv/static --minify
cd ../server
DATABASE_PATH="file:db.sqlite3?mode=rw" GLEAM_ENV="development" PORT="8083" SECRET_KEY_BASE="m6cGj3W7ve7vsrfcA4mXaTZq6g3sw97mLm2JmvVkEIL77gYnL807rV9vBBbaaCAMGrRFM5JocMui0fu-Jw64pw" gleam run


