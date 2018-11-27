FROM java:openjdk-8

VOLUME /app
ADD *.jar /app.jar
ENV JAVA_OPTS=""

EXPOSE 8080

ENTRYPOINT [ "sh", "-c", "java $JAVA_OPTS -jar /app.jar" ]