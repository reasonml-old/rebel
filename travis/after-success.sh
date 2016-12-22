if [ "$TRAVIS_BRANCH" = "master" ]; then
  if [ "$TRAVIS_PULL_REQUEST" = false ]; then

    cd _build

    git config --global user.email "travis@travis-ci.org"

    git config --global user.name "Travis"

    git clone https://github.com/vramana/rebel-bin.git --depth=1

    cd rebel-bin

    git rm -rf .

    git checkout --orphan "bin-$TRAVIS_COMMIT-$TRAVIS_OS_NAME"

    mkdir _build

    cp -r ../src _build

    cp -r ../../src .

    git add .

    git commit -m "Travis $TRAVIS_BUILD_NUMBER"

    git push "https://${GH_TOKEN}@${GH_REF}" "bin-$TRAVIS_COMMIT-$TRAVIS_OS_NAME" > /dev/null 2>&1

  fi
fi
