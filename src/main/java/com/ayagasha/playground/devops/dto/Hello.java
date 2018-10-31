package com.ayagasha.playground.devops.dto;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class Hello {

    private String message;
    private String host;
    private String ip;
    private Long counter;

}
