package com.ayagasha.playground.infrastructure;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@SpringBootApplication
@ComponentScan(basePackages = {
        "com.ayagasha.playground.infrastructure"})
@Configuration
@Slf4j
public class InfrastructureApp extends SpringApplication {

    public static void main(String[] args) {
        SpringApplication.run(InfrastructureApp.class, args);
        log.info("Infrastructure App is started successfully.");
        log.debug("DEBUG message");
    }


}

