  
# https://hub.docker.com/_/microsoft-dotnet-core
FROM mcr.microsoft.com/dotnet/core/sdk:3.1 AS build
WORKDIR /source

# copy csproj and restore as distinct layers
COPY vptech-challenge.fsproj ./
RUN dotnet restore

# copy everything else and build app
COPY . ./
RUN dotnet publish vptech-challenge.fsproj -c release -o /app --no-restore

# final stage/image
FROM mcr.microsoft.com/dotnet/core/aspnet:3.1
WORKDIR /app
RUN useradd -ms /bin/bash admin
COPY --from=build /app ./
RUN chown -R admin:admin /app
RUN chmod 777 /app
EXPOSE 5000/tcp
ENTRYPOINT ["dotnet", "vptech-challenge.dll"]