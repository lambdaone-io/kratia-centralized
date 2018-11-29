FROM java:openjdk-8

VOLUME /app
ADD *.jar /app/app.jar

EXPOSE 8080

ENTRYPOINT [ "sh", "-c", "java -jar /app/app.jar" ]