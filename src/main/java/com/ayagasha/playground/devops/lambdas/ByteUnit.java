package com.ayagasha.playground.devops.lambdas;

import java.math.BigDecimal;

public enum ByteUnit {

    BYTE(1L),
    KILOBYTE(BigDecimal.valueOf(2).pow(10).longValue()),
    MEGABYTE(BigDecimal.valueOf(2).pow(20).longValue()),
    GIGABYTE(BigDecimal.valueOf(2).pow(30).longValue());

    private Long unit;

    private ByteUnit(Long unit) {
        this.unit = unit;
    }

    public Long getUnit() {
        return unit;
    }
}
