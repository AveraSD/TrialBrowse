# ubuntu image

FROM ubuntu:20.04

#install mongo db community edition
RUN apt-get update && apt-get install -y gnupg curl
	
RUN curl -fsSL https://www.mongodb.org/static/pgp/server-7.0.asc | \
   sudo gpg -o /usr/share/keyrings/mongodb-server-7.0.gpg \
   --dearmor
RUN echo "deb [ arch=amd64,arm64 signed-by=/usr/share/keyrings/mongodb-server-7.0.gpg ] https://repo.mongodb.org/apt/ubuntu focal/mongodb-org/7.0 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-7.0.list
RUN apt-get update && apt-get install -y mongodb-org && \
	apt-get clean && \
	apt-get purge && \
	rm -rf /var/lib/apt/lists/* 
	


#mongo db data directory
RUN mkdir -p /data/trials

# init directory
RUN mkdir -p /docker-entrypoint-initdb.d

#copy initialization script to init directory
COPY ../db_init.sh /docker-entrypoint-initdb.d/

#expose port
EXPOSE 27017

# start mongo db
CMD ["mongod"]

