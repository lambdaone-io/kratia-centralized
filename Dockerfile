FROM kolov/java8

VOLUME /app
ADD *.jar /app.jar
ENV JAVA_OPTS=""
ENV SERVER_PORT=9000

EXPOSE 9000

ENTRYPOINT [ "sh", "-c", "java $JAVA_OPTS -jar /app.jar" ]