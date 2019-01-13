FROM fpco/stack-build:latest

WORKDIR /home/app

COPY . /home/app
COPY ./root-bp.sh /root/.bash_profile

RUN stack install ghcid

RUN stack build

CMD ["ghcid", "--command=\"stack --allow-different-user ghci\""]
