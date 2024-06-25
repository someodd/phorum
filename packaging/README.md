This directory is for packaging and Phorum configuration.

I use `./package.sh` to build the package (I call it from the project root).

Includes `systemd` files which you can copy yourself:

```
cp ./etc/phorum.{service,socket} /etc/systemd/system/
systemctl daemon-reload
systemctl enable phorum.socket
systemctl start  phorum.socket
systemctl start  phorum.service # optional, started by the socket automatically if needed
```