PROJECT=$1

rm -r $PROJECT/_build
cp -f ./rebel $PROJECT/node_modules/.bin/rebel
cd $PROJECT && npm start
