# Complice Xmobar integration as a web server
# To run:
# docker run --name complice -d --restart always -e COMPLICE_TOKEN -it -p 7000:7000 complice
# where COMPLICE_TOKEN is the complice api token base64 encoded
FROM node
RUN wget "https://github.com/sharkdp/pastel/releases/download/v0.8.1/pastel_0.8.1_amd64.deb"
RUN dpkg -i pastel_0.8.1_amd64.deb
COPY . .
RUN npm install
CMD node index.js
