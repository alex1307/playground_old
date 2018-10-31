FROM anapsix/alpine-java:8_jdk-dcevm_unlimited
MAINTAINER alexander.todorov@ayagasha.com

WORKDIR /app
# Externalize source jar name
COPY build/docker/playground.jar ./java-app.jar

EXPOSE 8080

# Expose Environment Variables for Application Configuration
# ENV SPRING_APPLICATION_JSON='{"spring": {"cloud": {"config": {"server": \
#    {"git": {"uri": "/var/lib/spring-cloud/config-repo", "clone-on-start": true}}}}}}'

ENTRYPOINT ["java"]
CMD ["-jar", "/app/java-app.jar"]
