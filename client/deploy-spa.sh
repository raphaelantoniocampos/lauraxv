gleam run -m lustre/dev build app --minify
mkdir -p dist
cp index.html dist/index.html
cp -r priv dist/priv
npm install wrangler --save-dev
npx wrangler pages deploy dist --project-name lauraxv-14dez
