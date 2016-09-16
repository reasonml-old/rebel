PROJECT=$1

rm -rf $PROJECT/_build && \
cp -f _build/src/rebel $PROJECT/node_modules/.bin/rebel && \
cd $PROJECT && npm start
