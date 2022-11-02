package com.ayagasha.playground.datacore.repository;

import com.ayagasha.playground.datacore.model.Reservation;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;

public interface ReservationRepository extends ReactiveCrudRepository<Reservation, Long> {

    @Query("select * from reservation where name = $1")
    Flux<Reservation> findByName(String name);
}
