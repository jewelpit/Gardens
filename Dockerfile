# syntax=docker/dockerfile:1
FROM mcr.microsoft.com/dotnet/sdk:6.0

ENV DEBIAN_FRONTEND=noninteractive
RUN apt update && apt upgrade -y && apt install -y npm

WORKDIR /source

COPY client/package.json client/package-lock.json ./client/
RUN cd client && npm install

COPY *.fsproj .
RUN dotnet restore

COPY . .
ENTRYPOINT dotnet run --no-launch-profile
