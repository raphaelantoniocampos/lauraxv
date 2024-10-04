fly app open
mv ./db/db.sqlite3 ./db/db.sqlite3.temp
fly sftp get /db/db.sqlite3 ./db/db.sqlite3
fly deploy
rm ./db/db.sqlite3.temp
