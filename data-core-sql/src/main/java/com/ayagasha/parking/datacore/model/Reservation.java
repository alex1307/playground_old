package com.ayagasha.parking.datacore.model;

import lombok.Builder;
import lombok.Data;
import lombok.experimental.Tolerate;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;

@Data
@Builder
public class Reservation {

    @Id
    private static final String SEQ_CATEGORY = "SEQ_RESERVATION";

    @Id
    @Column("ID")
    private Long id;
    @Column("NAME")
    private String name;

    @Tolerate
    public Reservation() {
    }
}


