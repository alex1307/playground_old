package com.ayagasha.playground.devops.controller;


import com.ayagasha.playground.devops.dto.Hello;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.request.ServletWebRequest;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

@Slf4j
@RestController
@RequestMapping("/playground")
public class Controller {

    private static final String HEADER_FORWARDED = "X-FORWARDED-FOR";
    private static Long counter = 1L;

    public Controller() {
        log.info("Hello controller has been created at {}",
                LocalDateTime
                        .now()
                        .format(DateTimeFormatter.ISO_LOCAL_DATE_TIME));
    }

    @GetMapping("/")
    public ResponseEntity<Hello> hello(final ServletWebRequest request) {
        final String host = request.getRequest().getRemoteHost();
        final String ip = request.getRequest().getRemoteAddr();
        return ResponseEntity.ok(Hello.builder()
                .message("Hello")
                .host(host)
                .ip(ip)
                .counter(counter++)
                .build());
    }

}
