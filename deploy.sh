chmod 600 deploy_rsa
  rsync -Pav -e "ssh -o StrictHostKeyChecking=no -i deploy_rsa" --exclude-from=./rsync-exclude.txt ./ts-viewer $DEPLOY_HOST:~/shiny/appdir/putnu31 --delete