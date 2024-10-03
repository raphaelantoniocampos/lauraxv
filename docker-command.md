docker run -d -p 8083:8083 -e DATABASE_PATH="file:/app/db/db.sqlite3?mode=rw" -e SECRET_KEY_BASE="pu5wrezu1yv84v04l6gwlfxcc4a9f8mrr2i5mfz0r3kijvq86c2ccqodziplivv4" -e GLEAM_ENV="development" -e PORT="8083" -v /home/raphaelac/repositories/lauraxv/server/db:/app/db --name lauraxv lauraxv:latest

docker run -d \
  -p 8083:8083 \
  -e DATABASE_PATH="file:/app/db/db.sqlite3?mode=rw" \
  -e SECRET_KEY_BASE="pu5wrezu1yv84v04l6gwlfxcc4a9f8mrr2i5mfz0r3kijvq86c2ccqodziplivv4" \
  -e GLEAM_ENV="development" \
  -e PORT="8083" \
  -v /home/raphaelac/repositories/lauraxv/server/db:/app/db \
  --name lauraxv \
  lauraxv:latest

FlyV1 fm2_lJPECAAAAAAAB7PVxBAebIQjHb6fZsy+qbcWdURQwrVodHRwczovL2FwaS5mbHkuaW8vdjGYAJLOAAzAUB8Lk7lodHRwczovL2FwaS5mbHkuaW8vYWFhL3YxxDyrdSSb+vXIYZNAx02amK9+x7kt7w/BQU8V+kx2K2pqEFOcGkSMAKnmTeBikceF22otc/gWAdei6kAc+MbETk+DSCwjkA+0t6ldDoBCbK8mw6skRo9U+o1USsVcDk9Of4F0AJmAUQXuVGzjuDq2CWSXqHQxCktJ5ue+F/T2oi2vNBm3L6uyQPmftruAeBCRgapsYXVyYXh2LWRiAwWRgaxsaXRlZnMtY2xvdWQDxCBkN+yC/mmbbfA+e1b6XQvaCHEFmYWgnQDey+a6JaBo2g==,fm2_lJPETk+DSCwjkA+0t6ldDoBCbK8mw6skRo9U+o1USsVcDk9Of4F0AJmAUQXuVGzjuDq2CWSXqHQxCktJ5ue+F/T2oi2vNBm3L6uyQPmftruAeMQQVNc18rhlN0Yl1UZlfWg4msO5aHR0cHM6Ly9hcGkuZmx5LmlvL2FhYS92MZgEks5m/rJIzwAAAAEi9tBmF84ADF2PCpHOAAxdjwzEEOMYvu3h4GY4IBj4AcZClWbEIKhJiXbltCrkuamzECVO1/Cj+yXI/eZaJLgv2V293Rnz
