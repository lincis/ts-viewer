chmod 600 deploy_key
  rsync -Pav -e "ssh -o StrictHostKeyChecking=no -i deploy_rsa" --exclude-from=./rsync-exclude.txt ./ts-viewver $DEPLOY_HOST:~/shiny/appdir/putnu31 --delete