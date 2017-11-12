function linux_before_install()
{
    cp -r packages/travis /tmp/
    cd ..
    tar --exclude=.git \
        -c -z -f /tmp/travis/increment.tar.gz increment
    docker build --tag increment /tmp/travis/
}

function linux_script()
{
    umask og+w
    mkdir upload
    docker run -i -t -v$(pwd)/upload:/upload --user=max increment \
           /bin/bash -c \
           'rpmbuild -bb /src/increment.spec --define "_rpmdir /upload"'

}

${TRAVIS_OS_NAME}_$1
